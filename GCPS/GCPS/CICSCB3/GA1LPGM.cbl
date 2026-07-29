00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID.     GA1LPGM.                                         GA1LPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV003
00004  AUTHOR.         SANDRA BUCH.                                     GA1LPGM 
00005  DATE-WRITTEN.   02/14/85.                                        GA1LPGM 
00006  DATE-COMPILED.                                                   GA1LPGM 
00007      SKIP3                                                        GA1LPGM 
00008 ******************************************************************GA1LPGM 
00009 *   GA1LPGM     ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM  GA1LPGM 
00010 *                              CONGENITAL DEFECT            GA1L  GA1LPGM 
00011 *                                                                 GA1LPGM 
00012 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1LPGM 
00013 *   CURRENTLY ON THE ALL LEVEL TABULAR RECORD.                    GA1LPGM 
00014 *                                                                 GA1LPGM 
00015 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1LPGM 
00016 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN DECIDE IF   GA1LPGM 
00017 *   ANY ENTRIES WILL BE DELETED.  THE SCREEN ENTRY WILL BE        GA1LPGM 
00018 *   VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD WILL BE   GA1LPGM 
00019 *   MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK INTO THE    GA1LPGM 
00020 *   RECORD BEFORE UPDATING THE RECORD.                            GA1LPGM 
00021 *                                                                 GA1LPGM 
00022 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID:#ACON)  GA1LPGM 
00023 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1LPGM 
00024 *   XCTL TO TRANS GA2L OR PROGRAM GA2LPGM.  THIS PROGRAM WILL     GA1LPGM 
00025 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1LPGM 
00026 *   TABLE.                                                        GA1LPGM 
00027 *                                                                 GA1LPGM 
00028 *   PF7/PF19  PAGE BACKWARD.                                      GA1LPGM 
00029 *   PF8/PF20  PAGE FORWARD.                                       GA1LPGM 
00030 *   PF10/PF22 PAGE TO BOTTOM.                                     GA1LPGM 
00031 *   PF11/PF23 PAGE TO TOP.                                        GA1LPGM 
00032 *                                                                 GA1LPGM 
00033 *   FUNC CODE: GA1L                                               GA1LPGM 
00034 *   MAPSET:    GA1LSETC <<<< REDEFINED BY USER DEFINED MAP >>>>   GA1LPGM 
00035 *   FILES:     GCPSWORK                                           GA1LPGM 
00036 *                                                                 GA1LPGM 
00037 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00038 *                                                                 GA1LPGM 
00039 *    TAILORING INSTRUCTIONS:                                      GA1LPGM 
00040 *                                                                 GA1LPGM 
00041 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1LPGM 
00042 *                                                                 GA1LPGM 
00043 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1L/     GA1LPGM 
00044 *              SCREEN PAGE NUMBER                 /009I/001L/     GA1LPGM 
00045 *              ADD PROGRAM FUNCTION CODE          /GA9I/GA2L/     GA1LPGM 
00046 *              BENEFIT PROVISION TABULAR ID       /#PPF/#ACON/    GA1LPGM 
00047 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GAI/       GA1LPGM 
00048 *                                                                 GA1LPGM 
00049 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1LPGM 
00050 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1LPGM 
00051 *     OCCURANCES.                                                 GA1LPGM 
00052 *                                                                 GA1LPGM 
00053 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1LPGM 
00054 *     THAT MUST BE CHANGED.                                       GA1LPGM 
00055 *                                                                 GA1LPGM 
00056 ******************************************************************GA1LPGM 
00057 *                                                                *GA1LPGM 
00058 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA1LPGM 
00059 *       *-*         U P D A T E   H I S T O R Y         *-*      *GA1LPGM 
00060 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA1LPGM 
00061 *                                                                *GA1LPGM 
00062 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GA1LPGM 
00063 *                                                                *GA1LPGM 
00064 *    XXXX    01/16/86  ENW  FIELD CHANGED:             FROM:  TO:*GA1LPGM 
00065 *                           GC-GCCONTR-MAX-REC-LEN      483  563 *GA1LPGM 
00066 *                           GC-GCCONTR-VARY-LEN          12   10 *GA1LPGM 
00067 *                           WS-CONTRACT-MAX-OCCURS      450  520 *GA1LPGM 
00068 *                                                                *GA1LPGM 
00069 *    D136/   08/25/86  AMJ  1. ADD SUPPORT FOR PF7,8,10 AND 11.  *GA1LPGM 
00070 *    D137                   2. ADD XXXXXXXXX ID SELECTION FIELD. *GA1LPGM 
00071 *                           3. ADD LOCATION COUNTERS (I.E 1 TO 36*GA1LPGM 
00072 *                              OF 54 XXXXXXXXXX DISPLAYED) TO    *GA1LPGM 
00073 *                              SCREEN.                           *GA1LPGM 
00074 *                           4. USE USER-DEFINED LOGICAL MAP FOR  *GA1LPGM 
00075 *                              SCREEN.  THIS REPLACES THE PARTIAL*GA1LPGM 
00076 *                              USE OF BMS MAP AND USER-DEFINED.  *GA1LPGM 
00077 *                                                                *GA1LPGM 
00078 *                                                                *GA1LPGM 
00079 *    D0120   01/30/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT   *GA1LPGM 
00080 *                           EXECUTED FROM TRANSACTION GTM1:      *GA1LPGM 
00081 *                           1. PF1/PF13 - CONSTRUCT COMMAREA AS  *GA1LPGM 
00082 *                              IF GC4A HAD CALLED, XCTL TO ADD   *GA1LPGM 
00083 *                              SCREEN PROGRAM.                   *GA1LPGM 
00084 *                           2. PF3/PF15 - CONSTRUCT COMMAREA AS  *GA1LPGM 
00085 *                              IF GC4A HAD CALLED, XCTL TO       *GA1LPGM 
00086 *                              GTM1PGM.                          *GA1LPGM 
00087 *                                                                *GA1LPGM 
00088 *    D116     8/17/87  FRY    CAPTURE OPERATOR-ID WHEN A 'C3',   *GA1LPGM 
00089 *                             'C5', OR 'G3' RECORD IS UPDATED.   *GA1LPGM 
00090 *                                                                *GA1LPGM 
00091 *                                                                *GA1LPGM 
00092 *  11161  11/17/90  ENW   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA1LPGM 
00093 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA1LPGM 
00094 *                         ROUTINES. REMOVED HARD CODED LENGTHS.  *GA1LPGM 
00095 *                                                                *GA1LPGM 
00096 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD      *GA1LPGM 
00097 *                           FROM ONE POSITION TO TWO POSITIONS.  *GA1LPGM 
00098 *                                                                *GA1LPGM 
00099 *  D12009 09/27/91  GDM   CONVERT TO COBOL II                    *GA1LPGM 
00100 *                                                                *GA1LPGM 
00101 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA1LPGM 
00102 *                                                                *GA1LPGM 
00103 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO*GA1LPGM 
00104 *                             7 BYTES. CHANGE # OF OCCURS TO 396 *GA1LPGM 
00105 *                             ON #ACON TABULAR.                  *GA1LPGM 
00105 *                                                                *GA1LPGM 
00105 * ICD-10   07/06/11     BA   EXPAND MAP-SELECT FIELD FROM 6 TO 7.*GA1LPGM 
00106 ******************************************************************GA1LPGM 
00107  ENVIRONMENT DIVISION.                                            GA1LPGM 
00108      EJECT                                                        GA1LPGM 
00109  DATA DIVISION.                                                   GA1LPGM 
00110  WORKING-STORAGE SECTION.                                         GA1LPGM 
00111                                                                   GA1LPGM 
00112  01  WS-MISC.                                                     GA1LPGM 
00113      05  WS-BEGIN                PIC X(24)  VALUE                 GA1LPGM 
00114      '***GA1LPGM WS BEGINS***'.                                   GA1LPGM 
00115      05  WS-PARA-ID              PIC X(4) VALUE 'XXXX'.           GA1LPGM 
00116      05  WS-ABEND-CODE           PIC X(4) VALUE 'XXXX'.           GA1LPGM 
00117      05  WS-SELECT-FROM          PIC S9(4) COMP SYNC VALUE +0.    GA1LPGM 
00118      05  WS-SELECT-TO            PIC S9(4) COMP SYNC VALUE +0.    GA1LPGM 
00119      05  WS-SELECT-OF            PIC S9(4) COMP SYNC VALUE +0.    GA1LPGM 
00120      05  WS-SELECT-FROM-MASK     PIC ZZZ9.                        GA1LPGM 
00121      05  WS-SELECT-FROM-MASK-RDF REDEFINES                        GA1LPGM 
00122              WS-SELECT-FROM-MASK.                                 GA1LPGM 
00123          10  FILLER              PIC X.                           GA1LPGM 
00124          10  WS-SELECT-FROM-TRUNC                                 GA1LPGM 
00125                                  PIC XXX.                         GA1LPGM 
00126      05  WS-SELECT-TO-MASK       PIC ZZZ9.                        GA1LPGM 
00127      05  WS-SELECT-TO-MASK-RDF REDEFINES                          GA1LPGM 
00128              WS-SELECT-TO-MASK.                                   GA1LPGM 
00129          10  FILLER              PIC X.                           GA1LPGM 
00130          10  WS-SELECT-TO-TRUNC  PIC XXX.                         GA1LPGM 
00131      05  WS-SELECT-OF-MASK       PIC ZZZ9.                        GA1LPGM 
00132      05  WS-SELECT-OF-MASK-RDF REDEFINES                          GA1LPGM 
00133              WS-SELECT-OF-MASK.                                   GA1LPGM 
00134          10  FILLER              PIC X.                           GA1LPGM 
00135          10  WS-SELECT-OF-TRUNC  PIC XXX.                         GA1LPGM 
00136      05  WS-GAI-INDEX            PIC S9(4) COMP SYNC VALUE +0.    GA1LPGM 
00137                                                                   GA1LPGM 
00138 ** MAP COBOL SCREEN DSECTS **                                     GA1LPGM 
00139  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1LPGM 
00140      '***  I/O MAPAREA ***'.                                      GA1LPGM 
00141  COPY GA1LSETC.                                                   GA1LPGM 
00142 /*****************************************************************GA1LPGM 
00143 ******************************************************************GA1LPGM 
00144 ******************************************************************GA1LPGM 
00145 **                                                              **GA1LPGM 
00146 **     THIS IS A USER-DEFINED LOGICAL MAP.  ANY CHANGES TO      **GA1LPGM 
00147 **     MAPSET GA1LSETC AFFECTING ITS LENGTH MUST BE TAKEN       **GA1LPGM 
00148 **     INTO ACCOUNT HERE.                                       **GA1LPGM 
00149 **                                                              **GA1LPGM 
00150 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA1LPGM 
00151 **                                                              **GA1LPGM 
00152 **                                            AMJ 8/27/86       **GA1LPGM 
00153 **                                                              **GA1LPGM 
00154 ******************************************************************GA1LPGM 
00155 ******************************************************************GA1LPGM 
00156 ******************************************************************GA1LPGM 
00157                                                                   GA1LPGM 
00158  01  MAP-USER-DEFINED REDEFINES GA1LI01I.                         GA1LPGM 
00159                                                                   GA1LPGM 
00160      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA1LPGM 
00161                                                                   GA1LPGM 
00162      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA1LPGM 
00163      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA1LPGM 
00164      05  MAP-FUNCTION-CODE                PIC X(04).              GA1LPGM 
00165                                                                   GA1LPGM 
00166      05  MAP-MAIN-TITLE-LEN               PIC S9(4) COMP SYNC.    GA1LPGM 
00167      05  MAP-MAIN-TITLE-ATTR              PIC X.                  GA1LPGM 
00168      05  MAP-MAIN-TITLE                   PIC X(40).              GA1LPGM 
00169                                                                   GA1LPGM 
00170      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA1LPGM 
00171      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA1LPGM 
00172      05  MAP-SCREEN-ID                    PIC X(06).              GA1LPGM 
00173                                                                   GA1LPGM 
00174      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA1LPGM 
00175      05  MAP-ID-LINE-ATTR                 PIC X.                  GA1LPGM 
00176      05  MAP-ID-LINE                      PIC X(79).              GA1LPGM 
00177                                                                   GA1LPGM 
00178      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA1LPGM 
00179          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA1LPGM 
00180          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA1LPGM 
00181          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA1LPGM 
00182          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA1LPGM 
00183          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA1LPGM 
00184          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA1LPGM 
00185          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA1LPGM 
00186          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA1LPGM 
00187          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA1LPGM 
00188          10  FILLER                           PIC X(18).          GA1LPGM 
00189      05  CONTRACT-ID-LINE REDEFINES MAP-ID-LINE.                  GA1LPGM 
00190          10  CONTRACT-ID-HEADING              PIC X(14).          GA1LPGM 
00191          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA1LPGM 
00192          10  CONTRACT-GROUP-NO                PIC X(6).           GA1LPGM 
00193          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA1LPGM 
00194          10  CONTRACT-SECTION-NO              PIC X(4).           GA1LPGM 
00195          10  CONTRACT-LOB-HEADING             PIC X(6).           GA1LPGM 
00196          10  CONTRACT-LOB                     PIC X.              GA1LPGM 
00197          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA1LPGM 
00198          10  CONTRACT-PROV-CTL                PIC XX.             GA1LPGM 
00199          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA1LPGM 
00200          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA1LPGM 
00201          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA1LPGM 
00202          10  CONTRACT-EFF-DATE                PIC X(6).           GA1LPGM 
00203          10  FILLER                           PIC X(09).          GA1LPGM 
00204      05  BENEFIT-PROVISION-ID-LINE REDEFINES MAP-ID-LINE.         GA1LPGM 
00205          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA1LPGM 
00206          10  BEN-PROV-GROUP-NO                PIC X(6).           GA1LPGM 
00207          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA1LPGM 
00208          10  BEN-PROV-SECTION-NO              PIC X(4).           GA1LPGM 
00209          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA1LPGM 
00210          10  BEN-PROV-LOB                     PIC X.              GA1LPGM 
00211          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA1LPGM 
00212          10  BEN-PROV-PROV-CTL                PIC XX.             GA1LPGM 
00213          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA1LPGM 
00214          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA1LPGM 
00215          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA1LPGM 
00216          10  BEN-PROV-EFF-DATE                PIC X(6).           GA1LPGM 
00217          10  BEN-PROV-ID-HEADING              PIC X(8).           GA1LPGM 
00218          10  BEN-PROV-ID-NO                   PIC X(6).           GA1LPGM 
00219          10  FILLER                           PIC X(09).          GA1LPGM 
00220                                                                   GA1LPGM 
00221      05  MAP-TABULAR-ID-LEN               PIC S9(4) COMP SYNC.    GA1LPGM 
00222      05  MAP-TABULAR-ID-ATTR              PIC X.                  GA1LPGM 
00223      05  MAP-TABULAR-ID                   PIC X(06).              GA1LPGM 
00224                                                                   GA1LPGM 
00225      05  MAP-TABULAR-SLOT-LEN             PIC S9(4) COMP SYNC.    GA1LPGM 
00226      05  MAP-TABULAR-SLOT-ATTR            PIC X.                  GA1LPGM 
00227      05  MAP-TABULAR-SLOT                 PIC X(07).              GA1LPGM 
00228                                                                   GA1LPGM 
00229      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA1LPGM 
00230      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA1LPGM 
00231      05  MAP-FROM-MENU-ID                 PIC X(04).              GA1LPGM 
00232                                                                   GA1LPGM 
00233      05  MAP-SELECT-TXT1-LEN              PIC S9(4) COMP SYNC.    GA1LPGM 
00234      05  MAP-SELECT-TXT1-ATTR             PIC X.                  GA1LPGM 
00235      05  MAP-SELECT-TXT1                  PIC X(07).              GA1LPGM 
00236                                                                   GA1LPGM 
00237      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA1LPGM 
00238      05  MAP-SELECT-ATTR                  PIC X.                  GA1LPGM 
00239      05  MAP-SELECT                       PIC X(07).              GA1LPGM 
00240                                                                   GA1LPGM 
00241      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA1LPGM 
00242      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA1LPGM 
00243      05  MAP-SELECT-FROM                  PIC X(03).              GA1LPGM 
00244                                                                   GA1LPGM 
00245      05  MAP-SELECT-TXT2-LEN              PIC S9(4) COMP SYNC.    GA1LPGM 
00246      05  MAP-SELECT-TXT2-ATTR             PIC X.                  GA1LPGM 
00247      05  MAP-SELECT-TXT2                  PIC X(02).              GA1LPGM 
00248                                                                   GA1LPGM 
00249      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA1LPGM 
00250      05  MAP-SELECT-TO-ATTR               PIC X.                  GA1LPGM 
00251      05  MAP-SELECT-TO                    PIC X(03).              GA1LPGM 
00252                                                                   GA1LPGM 
00253      05  MAP-SELECT-TXT3-LEN              PIC S9(4) COMP SYNC.    GA1LPGM 
00254      05  MAP-SELECT-TXT3-ATTR             PIC X.                  GA1LPGM 
00255      05  MAP-SELECT-TXT3                  PIC X(02).              GA1LPGM 
00256                                                                   GA1LPGM 
00257      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA1LPGM 
00258      05  MAP-SELECT-OF-ATTR               PIC X.                  GA1LPGM 
00259      05  MAP-SELECT-OF                    PIC X(03).              GA1LPGM 
00260                                                                   GA1LPGM 
00261      05  MAP-SELECT-TXT4-LEN              PIC S9(4) COMP SYNC.    GA1LPGM 
00262      05  MAP-SELECT-TXT4-ATTR             PIC X.                  GA1LPGM 
00263      05  MAP-SELECT-TXT4                  PIC X(30).              GA1LPGM 
00264                                                                   GA1LPGM 
00265 **+**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1LPGM 
00266 **  CHANGE OCCURS COUNT AND LENGTH TO MATCH BMS MAP               GA1LPGM 
00267 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1LPGM 
00268      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA1LPGM 
00269          INDEXED BY MAP-IDX1.                                     GA1LPGM 
00270        10  MAP-PROCEDURE-ARGUMENT-COL  OCCURS 2 TIMES             GA1LPGM 
00271            INDEXED BY  MAP-IDX2.                                  GA1LPGM 
00272          15  MAP-ACTION-CODE-LEN          PIC S9(4) COMP SYNC.    GA1LPGM 
00273          15  MAP-ACTION-CODE-ATTR         PIC X.                  GA1LPGM 
00274          15  MAP-ACTION-CODE              PIC X.                  GA1LPGM 
00275          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA1LPGM 
00276          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA1LPGM 
00277          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA1LPGM 
00278          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA1LPGM 
00279          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA1LPGM 
00280          15  MAP-CODE-FUNCTION            PIC X(3).               GA1LPGM 
00281 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1LPGM 
00282 *------ AFTER OCCURS                                              GA1LPGM 
00283      05  MAP-SELECT-TXT5-LEN              PIC S9(4) COMP SYNC.    GA1LPGM 
00284      05  MAP-SELECT-TXT5-ATTR             PIC X.                  GA1LPGM 
00285      05  MAP-SELECT-TXT5                  PIC X(79).              GA1LPGM 
00286                                                                   GA1LPGM 
00287      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA1LPGM 
00288      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA1LPGM 
00289      05  MAP-ERROR-MESSAGE                PIC X(79).              GA1LPGM 
00290      SKIP3                                                        GA1LPGM 
00291 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00292  01  WS-MAP-OCCURS-COUNTERS.                                      GA1LPGM 
00293 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00294 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1LPGM 
00295 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00296      05  WS-MAP-ROW              PIC S9(3)  COMP-3  VALUE +15.    GA1LPGM 
00297      05  WS-MAP-COL              PIC S9(3)  COMP-3  VALUE +2.     GA1LPGM 
00298 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00299      EJECT                                                        GA1LPGM 
00300 ** ALTERNATIVE WORKFILE KEYS **                                   GA1LPGM 
00301  01  FILLER                      PIC X(32)  VALUE                 GA1LPGM 
00302      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1LPGM 
00303  01  WS-ALT-WORKFILE-KEYS.                                        GA1LPGM 
00304  COPY GCWRKKEY.                                                   GA1LPGM 
00305      EJECT                                                        GA1LPGM 
00306 ** HARDCOPY WORK AREA **                                          GA1LPGM 
00307 *01  WS-HARDCOPY-COMMAREA.                                        GA1LPGM 
00308 *COPY PRNCOBOL.                                                   GA1LPGM 
00309                                                                   GA1LPGM 
00310      EJECT                                                        GA1LPGM 
00311 ** WORKFIELDS, AND SWITCHES **                                    GA1LPGM 
00312  01  WS-WORK-FIELDS.                                              GA1LPGM 
00313      05  WS-HEX-00                     PIC X.                     GA1LPGM 
00314      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1LPGM 
00315 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00316 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00317 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00318      05  WS-SAVED-FIELDS.                                         GA1LPGM 
00319        10  WS-SAVED-PROCED-ARGUMENT    PIC X(7).                  GA1LPGM 
00320        10  WS-SAVED-CODE-FUNCTION      PIC X(3).                  GA1LPGM 
00321 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00322  01  WS-SWITCHES.                                                 GA1LPGM 
00323      05  WS-ERROR-SW                   PIC X.                     GA1LPGM 
00324                                                                   GA1LPGM 
00325 ** TITLE LINES **                                                 GA1LPGM 
00326  01  WS-TITLE-LINES.                                              GA1LPGM 
00327      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(40)  VALUE    GA1LPGM 
00328          ' GROUP SPECIFIC CONDITIONAL PROCEDURES  '.              GA1LPGM 
00329      05  CONTRACT-TITLE-LINE                  PIC X(40)  VALUE    GA1LPGM 
00330          '    CONTRACT CONDITIONAL PROCEDURES     '.              GA1LPGM 
00331      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(40)  VALUE    GA1LPGM 
00332          'BENEFIT PROVISION CONDITIONAL PROCEDURES'.              GA1LPGM 
00333                                                                   GA1LPGM 
00334      EJECT                                                        GA1LPGM 
00335 ** ATTRIBUTES **                                                  GA1LPGM 
00336  COPY DFHBMSCA.                                                   GA1LPGM 
00337      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1LPGM 
00338      EJECT                                                        GA1LPGM 
00339 ** ATTENTION IDENTIFIERS **                                       GA1LPGM 
00340  COPY DFHAID.                                                     GA1LPGM 
00341      EJECT                                                        GA1LPGM 
00342 ** RECORD LENGTHS **                                              GA1LPGM 
00343  01  WS-RECORD-LENGTHS.                                           GA1LPGM 
00344     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA1LPGM 
00345     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA1LPGM 
00346     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1LPGM 
00347     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1LPGM 
00348 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA1LPGM 
00349 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA1LPGM 
00350 *   05 GC-GCGRPSPC-FIXED-LEN          PIC S9(5) COMP-3 VALUE +410.GA1LPGM 
00351 *   05 GC-GCGRPSPC-VARY-LEN           PIC S9(5) COMP-3 VALUE +10. GA1LPGM 
00352 *   05 GC-GCGRPSPC-VARY-MAX-OCUR      PIC S9(5) COMP-3 VALUE +30. GA1LPGM 
00353 *   05 GC-GCCONTR-FIXED-LEN           PIC S9(5) COMP-3 VALUE +563.GA1LPGM 
00354 *   05 GC-GCCONTR-VARY-LEN            PIC S9(5) COMP-3 VALUE +10. GA1LPGM 
00355 *   05 GC-GCCONTR-VARY-MAX-OCUR       PIC S9(5) COMP-3 VALUE +520.GA1LPGM 
00356 *   05 GC-GCBENPRV-FIXED-LEN          PIC S9(5) COMP-3 VALUE +245.GA1LPGM 
00357 *   05 GC-GCBENPRV-VARY-LEN           PIC S9(5) COMP-3 VALUE +10. GA1LPGM 
00358 *   05 GC-GCBENPRV-VARY-MAX-OCUR      PIC S9(5) COMP-3 VALUE +15. GA1LPGM 
00359 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00360 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00361 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00362 *   05 GC-GCTABULR-ACON-FIXED-LEN     PIC S9(5) COMP-3 VALUE +40. GA1LPGM 
00363 *   05 GC-GCTABULR-ACON-VARY-LEN      PIC S9(5) COMP-3 VALUE +9.  GA1LPGM 
00364 *   05 GC-GCTABULR-ACON-VARY-MAX-OCUR PIC S9(5) COMP-3 VALUE +440.GA1LPGM 
00365 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00366 /                                                                 GA1LPGM 
00367  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1LPGM 
00368  01  COMMAREA-POINTER-AREA.                                       GA1LPGM 
00369      05  COMMAREA-PNTR-COMP           PIC S9(8)  COMP.            GA1LPGM 
00370      05  COMMAREA-PNTR                REDEFINES                   GA1LPGM 
00371          COMMAREA-PNTR-COMP           USAGE IS POINTER.           GA1LPGM 
00372 /                                                                 GA1LPGM 
00373  01  WS-GCPS-LENGTHS.                                             GA1LPGM 
00374      COPY GCCDRLEN.                                               GA1LPGM 
00375                                                                   GA1LPGM 
00376  01  WS-END                      PIC X(16)  VALUE                 GA1LPGM 
00377      '*** W/S ENDS ***'.                                          GA1LPGM 
00378      EJECT                                                        GA1LPGM 
00379  LINKAGE SECTION.                                                 GA1LPGM 
00380  01  DFHCOMMAREA.                                                 GA1LPGM 
00381      COPY G2ALCKEC.                                               GA1LPGM 
00382 *    05  INCOMING-COMMAREA-PNTR-COMP  PIC S9(8)  COMP.            GA1LPGM 
00383 *    05  INCOMING-COMMAREA-PNTR       REDEFINES                   GA1LPGM 
00384 *        INCOMING-COMMAREA-PNTR-COMP  USAGE IS POINTER.           GA1LPGM 
00385 *                                                                 GA1LPGM 
00386 *01  BLL-CELLS.                                                   GA1LPGM 
00387 *    02  FILLER                  PIC S9(8)  COMP.                 GA1LPGM 
00388 *    02  COMMAREA-PNTR           PIC S9(8)  COMP.                 GA1LPGM 
00389 *    02  ALL-LEVEL-TAB-PNTR      PIC S9(8)  COMP.                 GA1LPGM 
00390 *    02  ALL-LEVEL-TAB-PNTR2     PIC S9(8)  COMP.                 GA1LPGM 
00391 *    02  COPY-AREA-PNTR          PIC S9(8)  COMP.                 GA1LPGM 
00392 *    02  GRP-SPEC-PNTR           PIC S9(8)  COMP.                 GA1LPGM 
00393 *    02  CONTRACT-PNTR           PIC S9(8)  COMP.                 GA1LPGM 
00394 *    02  CONTRACT-PNTR2          PIC S9(8)  COMP.                 GA1LPGM 
00395 *    02  BEN-PROV-PNTR           PIC S9(8)  COMP.                 GA1LPGM 
00396 *                                                                 GA1LPGM 
00397 *01  GCA-COMMAREA.                                                GA1LPGM 
00398 *COPY G2ALCKEC.                                                   GA1LPGM 
00399      EJECT                                                        GA1LPGM 
00400  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA1LPGM 
00401  COPY GCIOPRM1.                                                   GA1LPGM 
00402      EJECT                                                        GA1LPGM 
00403  COPY GCWRKDCC.                                                   GA1LPGM 
00404      SKIP3                                                        GA1LPGM 
00405      SKIP3                                                        GA1LPGM 
00406      SKIP3                                                        GA1LPGM 
00407  COPY GCTACONC.                                                   GA1LPGM 
00408      EJECT                                                        GA1LPGM 
00409 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00410 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00411 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00412  01  COPY-OF-TABLE-AREA.                                          GA1LPGM 
00413      05  COPY-OF-TABLE   OCCURS 396 TIMES   INDEXED BY  COPY-IDX. GA1LPGM 
00414        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA1LPGM 
00415        10  COPY-CODE-FUNCTION          PIC X(3).                  GA1LPGM 
00416 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00417      EJECT                                                        GA1LPGM 
00418  01  IO-PARM-GRP-SPEC-RECORD.                                     GA1LPGM 
00419  COPY GCIOPRM2.                                                   GA1LPGM 
00420      EJECT                                                        GA1LPGM 
00421  COPY GCWRKDC2.                                                   GA1LPGM 
00422      EJECT                                                        GA1LPGM 
00423  COPY GCGROUPC.                                                   GA1LPGM 
00424      EJECT                                                        GA1LPGM 
00425                                                                   GA1LPGM 
00426  01  IO-PARM-CONTRACT-RECORD.                                     GA1LPGM 
00427  COPY GCIOPRM3.                                                   GA1LPGM 
00428      EJECT                                                        GA1LPGM 
00429  COPY GCWRKDC3.                                                   GA1LPGM 
00430      EJECT                                                        GA1LPGM 
00431  COPY GCCONTRC.                                                   GA1LPGM 
00432      EJECT                                                        GA1LPGM 
00433                                                                   GA1LPGM 
00434  01  IO-PARM-BEN-PROV-RECORD.                                     GA1LPGM 
00435  COPY GCIOPRM4.                                                   GA1LPGM 
00436      EJECT                                                        GA1LPGM 
00437  COPY GCWRKDC4.                                                   GA1LPGM 
00438      EJECT                                                        GA1LPGM 
00439  COPY GCBENPVC.                                                   GA1LPGM 
00440      EJECT                                                        GA1LPGM 
00441  PROCEDURE DIVISION.                                              GA1LPGM 
00442                                                                   GA1LPGM 
00443 ******************************************************************GA1LPGM 
00444 **                H O U S E K E E P I N G                         GA1LPGM 
00445 **                                                                GA1LPGM 
00446 ** DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM. GA1LPGM 
00447 **                                                                GA1LPGM 
00448 ******************************************************************GA1LPGM 
00449  0000-HOUSEKEEPING SECTION.                                       GA1LPGM 
00450                                                                   GA1LPGM 
00451      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1LPGM 
00452      IF EIBAID  =  DFHCLEAR                                       GA1LPGM 
00453          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1LPGM 
00454                         ERASE                                     GA1LPGM 
00455          END-EXEC                                                 GA1LPGM 
00456          EXEC CICS RETURN                                         GA1LPGM 
00457          END-EXEC.                                                GA1LPGM 
00458                                                                   GA1LPGM 
00459      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1LPGM 
00460         END-EXEC.                                                 GA1LPGM 
00461      EJECT                                                        GA1LPGM 
00462 ******************************************************************GA1LPGM 
00463 **                     M A I N L I N E                            GA1LPGM 
00464 **                                                                GA1LPGM 
00465 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1LPGM 
00466 **  TAKEN BY THE OPERATOR.                                        GA1LPGM 
00467 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1LPGM 
00468 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO DETERMINE  GA1LPGM 
00469 **     WHICH ENTRIES, IF ANY, THEY MIGHT WANT TO DELETE.          GA1LPGM 
00470 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1LPGM 
00471 **     KEY PF12 OR PF24.                                          GA1LPGM 
00472 **  3. RECEIVE THE SCREEN.                                        GA1LPGM 
00473 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1LPGM 
00474 **     MENU.                                                      GA1LPGM 
00475 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1LPGM 
00476 **     LOGIC.                                                     GA1LPGM 
00477 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1LPGM 
00478 **     (RETURN) TO THE ADD PROGRAM (GA2LPGM).                     GA1LPGM 
00479 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1LPGM 
00480 **     (RETURN) TO THE PREVIOUS MENU.                             GA1LPGM 
00481 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1LPGM 
00482 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1LPGM 
00483 **                                                                GA1LPGM 
00484 ******************************************************************GA1LPGM 
00485  1000-MAIN-LINE SECTION.                                          GA1LPGM 
00486                                                                   GA1LPGM 
00487      MOVE '1000'  TO  WS-PARA-ID.                                 GA1LPGM 
00488      IF EIBTRNID  NOT =  'GA1L'                                   GA1LPGM 
00489         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1LPGM 
00490         GO TO 1099-RETURN.                                        GA1LPGM 
00491                                                                   GA1LPGM 
00492      EXEC CICS RECEIVE   MAP('GA1LI01') MAPSET('GA1LSET')         GA1LPGM 
00493         INTO(GA1LI01I) END-EXEC.                                  GA1LPGM 
00494                                                                   GA1LPGM 
00495      IF MAP-SCREEN-ID NOT = '001L00'                              GA1LPGM 
00496         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1LPGM 
00497                                                                   GA1LPGM 
00498      IF EIBAID  =  DFHENTER                                       GA1LPGM 
00499            OR DFHPF7 OR DFHPF8 OR DFHPF10 OR DFHPF11              GA1LPGM 
00500            OR DFHPF19 OR DFHPF20 OR DFHPF22 OR DFHPF23            GA1LPGM 
00501         PERFORM 2000-DELETE-PROCESSING                            GA1LPGM 
00502         GO TO 1099-RETURN.                                        GA1LPGM 
00503                                                                   GA1LPGM 
00504      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1LPGM 
00505         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1LPGM 
00506                                                                   GA1LPGM 
00507      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1LPGM 
00508         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1LPGM 
00509                                                                   GA1LPGM 
00510      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1LPGM 
00511      MOVE -1  TO  MAP-SELECT-LEN.                                 GA1LPGM 
00512      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1LPGM 
00513 -    'THIS PROGRAM ***'                                           GA1LPGM 
00514      TO  MAP-ERROR-MESSAGE.                                       GA1LPGM 
00515      EXEC CICS SEND   MAP('GA1LI01') MAPSET('GA1LSET') DATAONLY   GA1LPGM 
00516         FROM(GA1LI01O) CURSOR END-EXEC.                           GA1LPGM 
00517                                                                   GA1LPGM 
00518  1099-RETURN.                                                     GA1LPGM 
00519 ***  EXEC CICS RETURN   END-EXEC.                                 GA1LPGM 
00520      EXEC CICS RETURN TRANSID ('GA1L')                            GA1LPGM 
00521                COMMAREA (DFHCOMMAREA)                             GA1LPGM 
00522      END-EXEC.                                                    GA1LPGM 
00523                                                                   GA1LPGM 
00524      GOBACK.                                                      GA1LPGM 
00525      EJECT                                                        GA1LPGM 
00526 ******************************************************************GA1LPGM 
00527 **              D E L E T E   P R O C E S S I N G                 GA1LPGM 
00528 **                                                                GA1LPGM 
00529 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1LPGM 
00530 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1LPGM 
00531 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1LPGM 
00532 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1LPGM 
00533 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1LPGM 
00534 **    BACK INTO THE RECORD THAT WE READ.)                         GA1LPGM 
00535 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1LPGM 
00536 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1LPGM 
00537 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1LPGM 
00538 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1LPGM 
00539 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1LPGM 
00540 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1LPGM 
00541 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1LPGM 
00542 **    ENTRY.                                                      GA1LPGM 
00543 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1LPGM 
00544 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1LPGM 
00545 **    RECORD.                                                     GA1LPGM 
00546 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1LPGM 
00547 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1LPGM 
00548 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1LPGM 
00549 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1LPGM 
00550 **    ARE BYPASSED; WE READ THE ALL LEVEL TABULAR RECORD:         GA1LPGM 
00551 **     A. IF ENTER WAS KEYED - SAVE THE NEXT ENTRY TO BE DISPLAYEDGA1LPGM 
00552 **        PERFORM THE ROUTINE TO BUILD THE DISPLAY, AND SEND THE  GA1LPGM 
00553 **        SCREEN TO THE OPERATOR.                                 GA1LPGM 
00554 **     B. IF PF7/PF19  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1LPGM 
00555 **        PREVIOUS PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO   GA1LPGM 
00556 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1LPGM 
00557 **     C. IF PF8/PF20  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1LPGM 
00558 **        NEXT PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1LPGM 
00559 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1LPGM 
00560 **     D. IF PF10/PF22  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1LPGM 
00561 **        LAST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1LPGM 
00562 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1LPGM 
00563 **     E. IF PF11/PF23  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1LPGM 
00564 **        FIRST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILDGA1LPGM 
00565 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1LPGM 
00566 **                                                                GA1LPGM 
00567 **                                                                GA1LPGM 
00568 ******************************************************************GA1LPGM 
00569  2000-DELETE-PROCESSING SECTION.                                  GA1LPGM 
00570                                                                   GA1LPGM 
00571      MOVE '2000'  TO  WS-PARA-ID.                                 GA1LPGM 
00572      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1LPGM 
00573      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1LPGM 
00574      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1LPGM 
00575                                                                   GA1LPGM 
00576      MOVE '2010'  TO  WS-PARA-ID.                                 GA1LPGM 
00577  2010-VALIDATE-ACT-CODE.                                          GA1LPGM 
00578      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D'               GA1LPGM 
00579         ADD 1  TO  WS-DELETE-COUNT.                               GA1LPGM 
00580      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D' OR            GA1LPGM 
00581         =  SPACE OR  =  LOW-VALUES                                GA1LPGM 
00582         MOVE DFHBMUNF  TO                                         GA1LPGM 
00583            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1LPGM 
00584         MOVE DFHBMASF  TO                                         GA1LPGM 
00585 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00586 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00587 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00588            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1LPGM 
00589            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1LPGM 
00590      ELSE                                                         GA1LPGM 
00591         MOVE DFHBMUBF  TO                                         GA1LPGM 
00592            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1LPGM 
00593         MOVE DFHBMABF  TO                                         GA1LPGM 
00594            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1LPGM 
00595            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1LPGM 
00596 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00597         IF WS-ERROR-SW  NOT =  'Y'                                GA1LPGM 
00598            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1LPGM 
00599            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1LPGM 
00600                                                                   GA1LPGM 
00601      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1LPGM 
00602         SET MAP-IDX1  UP BY  1                                    GA1LPGM 
00603      ELSE                                                         GA1LPGM 
00604         IF MAP-IDX2  <  WS-MAP-COL                                GA1LPGM 
00605            SET MAP-IDX1  TO  1                                    GA1LPGM 
00606            SET MAP-IDX2  UP BY  1                                 GA1LPGM 
00607         ELSE                                                      GA1LPGM 
00608            GO TO 2020-DONE-VALIDATE-A-C.                          GA1LPGM 
00609 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00610 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00611 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00612      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)               GA1LPGM 
00613             NOT =  LOW-VALUES  AND                                GA1LPGM 
00614         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2)                    GA1LPGM 
00615             NOT =  LOW-VALUES                                     GA1LPGM 
00616 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00617         GO TO 2010-VALIDATE-ACT-CODE.                             GA1LPGM 
00618                                                                   GA1LPGM 
00619  2020-DONE-VALIDATE-A-C.                                          GA1LPGM 
00620      MOVE '2020'  TO  WS-PARA-ID.                                 GA1LPGM 
00621      SET MAP-IDX1  TO  1.                                         GA1LPGM 
00622      IF WS-ERROR-SW  =  'Y'                                       GA1LPGM 
00623         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1LPGM 
00624            MAP-ERROR-MESSAGE                                      GA1LPGM 
00625         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE,                   GA1LPGM 
00626            MAP-MAIN-TITLE,                                        GA1LPGM 
00627            MAP-SCREEN-ID,                                         GA1LPGM 
00628            MAP-TABULAR-ID,                                        GA1LPGM 
00629            MAP-TABULAR-SLOT,                                      GA1LPGM 
00630            MAP-ID-LINE,                                           GA1LPGM 
00631            MAP-FROM-MENU-ID                                       GA1LPGM 
00632 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00633 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1LPGM 
00634 **  ADD ITS MAP FIELD NAME HERE.                                  GA1LPGM 
00635 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00636         MOVE '2100'  TO  WS-PARA-ID                               GA1LPGM 
00637         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1LPGM 
00638            VARYING MAP-IDX2 FROM  1  BY  1                        GA1LPGM 
00639                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1LPGM 
00640              AFTER MAP-IDX1 FROM  1  BY  1                        GA1LPGM 
00641                             UNTIL MAP-IDX1  > WS-MAP-ROW          GA1LPGM 
00642         EXEC CICS SEND   MAP('GA1LI01') MAPSET('GA1LSET') DATAONLYGA1LPGM 
00643            FROM(GA1LI01O) CURSOR END-EXEC                         GA1LPGM 
00644         GO TO 2099-EXIT.                                          GA1LPGM 
00645                                                                   GA1LPGM 
00646      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1LPGM 
00647              GC-GCIOPARM-LEN +                                    GA1LPGM 
00648              GC-WORKFILE-KEY-LEN +                                GA1LPGM 
00649              GC-GCTABULR-ACON-FIXED-LEN +                         GA1LPGM 
00650           (GC-GCTABULR-ACON-VARY-MAX-OCUR *                       GA1LPGM 
00651           GC-GCTABULR-ACON-VARY-LEN).                             GA1LPGM 
00652                                                                   GA1LPGM 
00653 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA1LPGM 
00654      EXEC CICS GETMAIN                                            GA1LPGM 
00655         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA1LPGM 
00656         INITIMG(WS-HEX-00)                                        GA1LPGM 
00657         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1LPGM 
00658 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1LPGM 
00659 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1LPGM 
00660                                                                   GA1LPGM 
00661      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA1LPGM 
00662         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1LPGM 
00663         MOVE 'G'                  TO GCIO-WRK-STATUS-CODE         GA1LPGM 
00664         MOVE 'G3'                 TO GCIO-WRK-RECORD-TYPE         GA1LPGM 
00665         MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE           GA1LPGM 
00666         MOVE GCA-GROUP-NO-1-3     TO GCIO-WRK-GROUP-NO-1-3        GA1LPGM 
00667         MOVE GRP-SPEC-GROUP-NO    TO GCIO-WRK-GROUP-NO            GA1LPGM 
00668         MOVE GCA-SEC-NO-1         TO GCIO-WRK-SEC-NO-1            GA1LPGM 
00669         MOVE GRP-SPEC-SECTION-NO  TO GCIO-WRK-SECTION-NO          GA1LPGM 
00670         MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE            GA1LPGM 
00671         MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS,        GA1LPGM 
00672                                      GCIO-WRK-PROVIDER-CONTROL    GA1LPGM 
00673         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1LPGM 
00674         MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN           GA1LPGM 
00675         MOVE MAP-TABULAR-ID       TO GCIO-WRK-PROVISION-ID        GA1LPGM 
00676         MOVE MAP-TABULAR-SLOT     TO GCIO-WRK-PROVISION-SLOT-NO   GA1LPGM 
00677         MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID    GA1LPGM 
00678         MOVE ZEROES               TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA1LPGM 
00679                                                                   GA1LPGM 
00680      IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA1LPGM 
00681         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1LPGM 
00682         MOVE 'C'                  TO GCIO-WRK-STATUS-CODE         GA1LPGM 
00683         MOVE 'C3'                 TO GCIO-WRK-RECORD-TYPE         GA1LPGM 
00684         MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE           GA1LPGM 
00685         MOVE GCA-GROUP-NO-1-3     TO GCIO-WRK-GROUP-NO-1-3        GA1LPGM 
00686         MOVE CONTRACT-GROUP-NO    TO GCIO-WRK-GROUP-NO            GA1LPGM 
00687         MOVE GCA-SEC-NO-1         TO GCIO-WRK-SEC-NO-1            GA1LPGM 
00688         MOVE CONTRACT-SECTION-NO  TO GCIO-WRK-SECTION-NO          GA1LPGM 
00689         MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE            GA1LPGM 
00690         MOVE CONTRACT-LOB         TO GCIO-WRK-LINE-OF-BUS         GA1LPGM 
00691         MOVE CONTRACT-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL    GA1LPGM 
00692         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1LPGM 
00693         MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN           GA1LPGM 
00694         MOVE MAP-TABULAR-ID       TO GCIO-WRK-PROVISION-ID        GA1LPGM 
00695         MOVE MAP-TABULAR-SLOT     TO GCIO-WRK-PROVISION-SLOT-NO   GA1LPGM 
00696         MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID    GA1LPGM 
00697         MOVE ZEROES               TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA1LPGM 
00698                                                                   GA1LPGM 
00699      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA1LPGM 
00700         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1LPGM 
00701         MOVE 'C'                  TO GCIO-WRK-STATUS-CODE         GA1LPGM 
00702         MOVE 'C5'                 TO GCIO-WRK-RECORD-TYPE         GA1LPGM 
00703         MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE           GA1LPGM 
00704         MOVE GCA-GROUP-NO-1-3     TO GCIO-WRK-GROUP-NO-1-3        GA1LPGM 
00705         MOVE BEN-PROV-GROUP-NO    TO GCIO-WRK-GROUP-NO            GA1LPGM 
00706         MOVE GCA-SEC-NO-1         TO GCIO-WRK-SEC-NO-1            GA1LPGM 
00707         MOVE BEN-PROV-SECTION-NO  TO GCIO-WRK-SECTION-NO          GA1LPGM 
00708         MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE            GA1LPGM 
00709         MOVE BEN-PROV-LOB         TO GCIO-WRK-LINE-OF-BUS         GA1LPGM 
00710         MOVE BEN-PROV-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL    GA1LPGM 
00711         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1LPGM 
00712         MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN           GA1LPGM 
00713         MOVE BEN-PROV-ID-NO       TO GCIO-WRK-PROVISION-ID        GA1LPGM 
00714         MOVE 9999999              TO GCIO-WRK-PROVISION-SLOT-NO   GA1LPGM 
00715         MOVE MAP-TABULAR-ID       TO GCIO-WRK-TAB-PROVISION-ID    GA1LPGM 
00716         MOVE MAP-TABULAR-SLOT     TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA1LPGM 
00717                                                                   GA1LPGM 
00718 *     MOVE  WS-Y  TO  WS-YY.                                      GA1LPGM 
00719 *     IF WS-M  >  2                                               GA1LPGM 
00720 *        DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                 GA1LPGM 
00721 *           REMAINDER  WS-REMAINDER                               GA1LPGM 
00722 *     ELSE                                                        GA1LPGM 
00723 *        MOVE 1  TO  WS-REMAINDER.                                GA1LPGM 
00724 *    SET WS-M-IDX  TO  WS-M.                                      GA1LPGM 
00725 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1LPGM 
00726 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1LPGM 
00727 *    IF WS-REMAINDER  =  ZERO                                     GA1LPGM 
00728 *       ADD 1  TO  WS-DDD.                                        GA1LPGM 
00729 *                                                                 GA1LPGM 
00730 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1LPGM 
00731 *                                                                 GA1LPGM 
00732      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA1LPGM 
00733      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1LPGM 
00734      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1LPGM 
00735                                                                   GA1LPGM 
00736      IF WS-DELETE-COUNT  =  ZERO                                  GA1LPGM 
00737         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1LPGM 
00738                                                                   GA1LPGM 
00739      IF EIBAID = DFHPF7  OR DFHPF19 OR                            GA1LPGM 
00740                  DFHPF8  OR DFHPF20 OR                            GA1LPGM 
00741                  DFHPF10 OR DFHPF22 OR                            GA1LPGM 
00742                  DFHPF11 OR DFHPF23                               GA1LPGM 
00743      THEN                                                         GA1LPGM 
00744          MOVE '*** ACTION CODE ENTRY INVALID WHEN PAGING ***'     GA1LPGM 
00745            TO MAP-ERROR-MESSAGE                                   GA1LPGM 
00746          MOVE -1 TO MAP-SELECT-LEN                                GA1LPGM 
00747          MOVE LOW-VALUES TO MAP-FUNCTION-CODE                     GA1LPGM 
00748                             MAP-SCREEN-ID                         GA1LPGM 
00749 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00750 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1LPGM 
00751 **  ADD ITS MAP FIELD NAME HERE.                                  GA1LPGM 
00752 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00753          MOVE '2100' TO WS-PARA-ID                                GA1LPGM 
00754          PERFORM 2100-DONT-RETRANSMIT-FIELDS                      GA1LPGM 
00755             VARYING MAP-IDX2 FROM 1 BY 1                          GA1LPGM 
00756                              UNTIL MAP-IDX2 > WS-MAP-COL          GA1LPGM 
00757               AFTER MAP-IDX1 FROM 1 BY 1                          GA1LPGM 
00758                              UNTIL MAP-IDX1 > WS-MAP-ROW          GA1LPGM 
00759          EXEC CICS SEND MAP('GA1LI01')                            GA1LPGM 
00760                         MAPSET('GA1LSET')                         GA1LPGM 
00761                         DATAONLY                                  GA1LPGM 
00762                         FROM(GA1LI01O)                            GA1LPGM 
00763                         CURSOR                                    GA1LPGM 
00764                         END-EXEC                                  GA1LPGM 
00765          GO TO 2099-EXIT                                          GA1LPGM 
00766      ELSE                                                         GA1LPGM 
00767          NEXT SENTENCE.                                           GA1LPGM 
00768                                                                   GA1LPGM 
00769 ******************************************************************GA1LPGM 
00770 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1LPGM 
00771 *                                                                 GA1LPGM 
00772 ******************************************************************GA1LPGM 
00773                                                                   GA1LPGM 
00774      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1LPGM 
00775      TO   GAI-ENTRY-COUNT.                                        GA1LPGM 
00776                                                                   GA1LPGM 
00777      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1LPGM 
00778                                                                   GA1LPGM 
00779      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1LPGM 
00780         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1LPGM 
00781         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1LPGM 
00782                                                                   GA1LPGM 
00783      IF  NOT GCIO-GOOD-RETURN                                     GA1LPGM 
00784         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1LPGM 
00785 -    'CONTACT SYSTEMS AREA ***'                                   GA1LPGM 
00786      TO  MAP-ERROR-MESSAGE                                        GA1LPGM 
00787         MOVE '1LF1'  TO  WS-ABEND-CODE                            GA1LPGM 
00788         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1LPGM 
00789                                                                   GA1LPGM 
00790      COMPUTE  WS-COPY-LENGTH  =                                   GA1LPGM 
00791            GAI-ENTRY-COUNT  *  GC-GCTABULR-ACON-VARY-LEN.         GA1LPGM 
00792                                                                   GA1LPGM 
00793 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA1LPGM 
00794      EXEC CICS GETMAIN                                            GA1LPGM 
00795         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA1LPGM 
00796         LENGTH(WS-COPY-LENGTH)                                    GA1LPGM 
00797         INITIMG(WS-HEX-00) END-EXEC.                              GA1LPGM 
00798 ***  SERVICE RELOAD COPY-OF-TABLE-AREA.                           GA1LPGM 
00799                                                                   GA1LPGM 
00800      MOVE GAI-ENTRY-COUNT  TO  GAI-ENTRY-COUNT.                   GA1LPGM 
00801      SET COPY-IDX, GAI-INDEX  TO  1.                              GA1LPGM 
00802                                                                   GA1LPGM 
00803      MOVE '2030'  TO  WS-PARA-ID.                                 GA1LPGM 
00804  2030-MAKE-A-COPY-OF-RECORD.                                      GA1LPGM 
00805      IF GAI-INDEX  NOT >  GAI-ENTRY-COUNT                         GA1LPGM 
00806         MOVE GAI-ENTRY (GAI-INDEX)  TO  COPY-OF-TABLE (COPY-IDX)  GA1LPGM 
00807         SET COPY-IDX, GAI-INDEX  UP BY 1                          GA1LPGM 
00808         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1LPGM 
00809                                                                   GA1LPGM 
00810      SET MAP-IDX1, MAP-IDX2, COPY-IDX, GAI-INDEX  TO  1.          GA1LPGM 
00811      MOVE '2040'  TO  WS-PARA-ID.                                 GA1LPGM 
00812  2040-DELETE-MARKED-ENTRIES.                                      GA1LPGM 
00813 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00814 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00815 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00816      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1LPGM 
00817             AND                                                   GA1LPGM 
00818         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1LPGM 
00819         GO TO 2060-SAVE-REST-OF-COPY.                             GA1LPGM 
00820                                                                   GA1LPGM 
00821      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) >             GA1LPGM 
00822         COPY-PROCEDURE-ARGUMENT (COPY-IDX)                        GA1LPGM 
00823         GO TO 2050-SAVE-COPIED-ENTRY                              GA1LPGM 
00824      ELSE                                                         GA1LPGM 
00825         IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) <          GA1LPGM 
00826            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA1LPGM 
00827            MOVE '1LL1'  TO  WS-ABEND-CODE                         GA1LPGM 
00828            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1LPGM 
00829 -    'RM SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                 GA1LPGM 
00830            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1LPGM 
00831                                                                   GA1LPGM 
00832 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00833                                                                   GA1LPGM 
00834      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1LPGM 
00835         IF MAP-IDX1  <  WS-MAP-ROW                                GA1LPGM 
00836            SET MAP-IDX1  UP BY  1                                 GA1LPGM 
00837            GO TO 2050-SAVE-COPIED-ENTRY                           GA1LPGM 
00838         ELSE                                                      GA1LPGM 
00839            IF MAP-IDX2  <  WS-MAP-COL                             GA1LPGM 
00840               SET MAP-IDX1  TO  1                                 GA1LPGM 
00841               SET MAP-IDX2  UP BY  1                              GA1LPGM 
00842               GO TO 2050-SAVE-COPIED-ENTRY                        GA1LPGM 
00843            ELSE                                                   GA1LPGM 
00844               GO TO 2060-SAVE-REST-OF-COPY.                       GA1LPGM 
00845                                                                   GA1LPGM 
00846      SET COPY-IDX  UP BY  1.                                      GA1LPGM 
00847      IF COPY-IDX  NOT <  GAI-ENTRY-COUNT                          GA1LPGM 
00848         MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAI-ENTRY (GAI-INDEX)  GA1LPGM 
00849         SET  GAI-ENTRY-COUNT  TO  GAI-INDEX                       GA1LPGM 
00850         MOVE GAI-ENTRY-COUNT  TO  GAI-ENTRY-COUNT                 GA1LPGM 
00851         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1LPGM 
00852                                                                   GA1LPGM 
00853      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1LPGM 
00854         SET MAP-IDX1  UP BY  1                                    GA1LPGM 
00855         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1LPGM 
00856                                                                   GA1LPGM 
00857      IF MAP-IDX2  <  WS-MAP-COL                                   GA1LPGM 
00858         SET MAP-IDX1  TO  1                                       GA1LPGM 
00859         SET MAP-IDX2  UP BY  1                                    GA1LPGM 
00860         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1LPGM 
00861      ELSE                                                         GA1LPGM 
00862         GO TO 2060-SAVE-REST-OF-COPY.                             GA1LPGM 
00863                                                                   GA1LPGM 
00864  2050-SAVE-COPIED-ENTRY.                                          GA1LPGM 
00865      MOVE '2050'  TO  WS-PARA-ID.                                 GA1LPGM 
00866      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAI-ENTRY (GAI-INDEX).    GA1LPGM 
00867                                                                   GA1LPGM 
00868      SET GAI-INDEX  UP BY  1.                                     GA1LPGM 
00869      IF COPY-IDX  <  GAI-ENTRY-COUNT                              GA1LPGM 
00870         SET COPY-IDX  UP BY  1                                    GA1LPGM 
00871         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1LPGM 
00872      ELSE                                                         GA1LPGM 
00873 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1LPGM 
00874 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1LPGM 
00875 ***      THE TABLE OF ENTRIES.                                    GA1LPGM 
00876         MOVE '1LL2'  TO  WS-ABEND-CODE                            GA1LPGM 
00877         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1LPGM 
00878 -    'SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                    GA1LPGM 
00879         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1LPGM 
00880                                                                   GA1LPGM 
00881  2060-SAVE-REST-OF-COPY.                                          GA1LPGM 
00882      MOVE '2060'  TO  WS-PARA-ID.                                 GA1LPGM 
00883      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAI-ENTRY (GAI-INDEX).    GA1LPGM 
00884                                                                   GA1LPGM 
00885      SET GAI-INDEX  UP BY  1.                                     GA1LPGM 
00886      IF COPY-IDX  <  GAI-ENTRY-COUNT                              GA1LPGM 
00887         SET COPY-IDX  UP BY  1                                    GA1LPGM 
00888         GO TO 2060-SAVE-REST-OF-COPY.                             GA1LPGM 
00889                                                                   GA1LPGM 
00890      SET GAI-INDEX  DOWN BY  1.                                   GA1LPGM 
00891      SET GAI-ENTRY-COUNT  TO  GAI-INDEX.                          GA1LPGM 
00892      MOVE GAI-ENTRY-COUNT  TO  GAI-ENTRY-COUNT.                   GA1LPGM 
00893                                                                   GA1LPGM 
00894  2070-UPDATE-MODIFIED-REC.                                        GA1LPGM 
00895      MOVE '2070'  TO  WS-PARA-ID.                                 GA1LPGM 
00896                                                                   GA1LPGM 
00897 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1LPGM 
00898                                                                   GA1LPGM 
00899      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1LPGM 
00900      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1LPGM 
00901                                                                   GA1LPGM 
00902      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1LPGM 
00903              GC-WORKFILE-KEY-LEN        +                         GA1LPGM 
00904              GC-GCTABULR-ACON-FIXED-LEN +                         GA1LPGM 
00905             (GAI-ENTRY-COUNT  *  GC-GCTABULR-ACON-VARY-LEN).      GA1LPGM 
00906                                                                   GA1LPGM 
00907      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1LPGM 
00908              GC-GCIOPARM-LEN      +                               GA1LPGM 
00909              GCIO-RECORD-LENGTH.                                  GA1LPGM 
00910                                                                   GA1LPGM 
00911      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1LPGM 
00912         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1LPGM 
00913         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1LPGM 
00914                                                                   GA1LPGM 
00915      IF GCIO-GOOD-RETURN                                          GA1LPGM 
00916         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1LPGM 
00917      MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASE CGA1LPGM 
00918 -    'ONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.            GA1LPGM 
00919      MOVE '1LF2'  TO  WS-ABEND-CODE.                              GA1LPGM 
00920      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1LPGM 
00921                                                                   GA1LPGM 
00922  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1LPGM 
00923      MOVE  '2080'  TO  WS-PARA-ID.                                GA1LPGM 
00924      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1LPGM 
00925      TO   GAI-ENTRY-COUNT.                                        GA1LPGM 
00926      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1LPGM 
00927      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1LPGM 
00928         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1LPGM 
00929         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1LPGM 
00930                                                                   GA1LPGM 
00931      IF GCIO-GOOD-RETURN                                          GA1LPGM 
00932         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1LPGM 
00933      MOVE '1LF3'  TO  WS-ABEND-CODE.                              GA1LPGM 
00934      MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE CONGA1LPGM 
00935 -    'TACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.              GA1LPGM 
00936      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1LPGM 
00937                                                                   GA1LPGM 
00938  2090-BUILD-NEXT-DISPLAY.                                         GA1LPGM 
00939      MOVE  '2090'  TO  WS-PARA-ID.                                GA1LPGM 
00940      SET MAP-IDX1  TO  WS-MAP-ROW.                                GA1LPGM 
00941      SET MAP-IDX2  TO  WS-MAP-COL.                                GA1LPGM 
00942      SET GAI-INDEX  TO  1.                                        GA1LPGM 
00943 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00944 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
00945 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00946      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1LPGM 
00947             AND                                                   GA1LPGM 
00948         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1LPGM 
00949         MOVE GAI-ENTRY (GAI-INDEX)  TO  WS-SAVED-FIELDS           GA1LPGM 
00950      ELSE                                                         GA1LPGM 
00951         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) TO       GA1LPGM 
00952            WS-SAVED-PROCED-ARGUMENT                               GA1LPGM 
00953         MOVE MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) TO            GA1LPGM 
00954            WS-SAVED-CODE-FUNCTION.                                GA1LPGM 
00955 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
00956                                                                   GA1LPGM 
00957                                                                   GA1LPGM 
00958      SET GAI-INDEX TO 1.                                          GA1LPGM 
00959                                                                   GA1LPGM 
00960      IF  MAP-SELECT-LEN > 0 AND                                   GA1LPGM 
00961          MAP-SELECT     > SPACES                                  GA1LPGM 
00962          MOVE MAP-SELECT TO WS-SAVED-PROCED-ARGUMENT.             GA1LPGM 
00963                                                                   GA1LPGM 
00964      IF  EIBAID = DFHPF7 OR DFHPF19 OR                            GA1LPGM 
00965                   DFHPF8 OR DFHPF20 OR                            GA1LPGM 
00966                   DFHPF10 OR DFHPF22 OR                           GA1LPGM 
00967                   DFHPF11 OR DFHPF23                              GA1LPGM 
00968          MOVE SPACES TO MAP-SELECT                                GA1LPGM 
00969      ELSE                                                         GA1LPGM 
00970          GO TO 2090-FILL-THE-SCREEN.                              GA1LPGM 
00971                                                                   GA1LPGM 
00972      IF  EIBAID = DFHPF7 OR DFHPF19                               GA1LPGM 
00973          GO TO 2090-PAGE-BACKWARD.                                GA1LPGM 
00974      IF  EIBAID = DFHPF8 OR DFHPF20                               GA1LPGM 
00975          GO TO 2090-PAGE-FORWARD.                                 GA1LPGM 
00976      IF  EIBAID = DFHPF10 OR DFHPF22                              GA1LPGM 
00977          GO TO 2090-PAGE-TO-BOTTOM.                               GA1LPGM 
00978      IF  EIBAID = DFHPF11 OR DFHPF23                              GA1LPGM 
00979          GO TO 2090-PAGE-TO-TOP.                                  GA1LPGM 
00980                                                                   GA1LPGM 
00981  2090-PAGE-BACKWARD.                                              GA1LPGM 
00982                                                                   GA1LPGM 
00983      MOVE MAP-PROCEDURE-ARGUMENT(1 1) TO                          GA1LPGM 
00984          WS-SAVED-PROCED-ARGUMENT.                                GA1LPGM 
00985      MOVE MAP-CODE-FUNCTION(1 1)  TO  WS-SAVED-CODE-FUNCTION.     GA1LPGM 
00986                                                                   GA1LPGM 
00987      SEARCH GAI-ENTRY                                             GA1LPGM 
00988          AT END                                                   GA1LPGM 
00989              MOVE GAI-ENTRY(1)  TO  WS-SAVED-FIELDS               GA1LPGM 
00990              GO TO 2090-FILL-THE-SCREEN                           GA1LPGM 
00991          WHEN WS-SAVED-FIELDS  =  GAI-ENTRY(GAI-INDEX)            GA1LPGM 
00992              SET WS-GAI-INDEX TO GAI-INDEX.                       GA1LPGM 
00993                                                                   GA1LPGM 
00994      COMPUTE WS-GAI-INDEX =                                       GA1LPGM 
00995          WS-GAI-INDEX - (WS-MAP-ROW * WS-MAP-COL) + 1.            GA1LPGM 
00996                                                                   GA1LPGM 
00997      IF  WS-GAI-INDEX < +0                                        GA1LPGM 
00998          MOVE GAI-ENTRY(1)  TO  WS-SAVED-FIELDS                   GA1LPGM 
00999      ELSE                                                         GA1LPGM 
01000          SET  GAI-INDEX TO WS-GAI-INDEX                           GA1LPGM 
01001          MOVE GAI-ENTRY(GAI-INDEX)  TO  WS-SAVED-FIELDS.          GA1LPGM 
01002                                                                   GA1LPGM 
01003      GO TO 2090-FILL-THE-SCREEN.                                  GA1LPGM 
01004                                                                   GA1LPGM 
01005  2090-PAGE-FORWARD.                                               GA1LPGM 
01006                                                                   GA1LPGM 
01007      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)               GA1LPGM 
01008              = LOW-VALUES                                         GA1LPGM 
01009         MOVE GAI-ENTRY(GAI-INDEX)  TO  WS-SAVED-FIELDS            GA1LPGM 
01010      ELSE                                                         GA1LPGM 
01011         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)  TO      GA1LPGM 
01012                                          WS-SAVED-PROCED-ARGUMENT GA1LPGM 
01013         MOVE MAP-CODE-FUNCTION(MAP-IDX1, MAP-IDX2)  TO            GA1LPGM 
01014                                            WS-SAVED-CODE-FUNCTION.GA1LPGM 
01015                                                                   GA1LPGM 
01016      GO TO 2090-FILL-THE-SCREEN.                                  GA1LPGM 
01017                                                                   GA1LPGM 
01018  2090-PAGE-TO-BOTTOM.                                             GA1LPGM 
01019                                                                   GA1LPGM 
01020      COMPUTE WS-GAI-INDEX =                                       GA1LPGM 
01021          GAI-ENTRY-COUNT - (WS-MAP-ROW * WS-MAP-COL).             GA1LPGM 
01022                                                                   GA1LPGM 
01023      IF WS-GAI-INDEX < +0                                         GA1LPGM 
01024         MOVE GAI-ENTRY(1)  TO  WS-SAVED-FIELDS                    GA1LPGM 
01025      ELSE                                                         GA1LPGM 
01026         SET  GAI-INDEX TO WS-GAI-INDEX                            GA1LPGM 
01027         MOVE GAI-ENTRY(GAI-INDEX)  TO  WS-SAVED-FIELDS.           GA1LPGM 
01028                                                                   GA1LPGM 
01029      GO TO 2090-FILL-THE-SCREEN.                                  GA1LPGM 
01030                                                                   GA1LPGM 
01031  2090-PAGE-TO-TOP.                                                GA1LPGM 
01032                                                                   GA1LPGM 
01033      SET  GAI-INDEX TO 1.                                         GA1LPGM 
01034      MOVE GAI-ENTRY(GAI-INDEX)  TO  WS-SAVED-FIELDS.              GA1LPGM 
01035                                                                   GA1LPGM 
01036      GO TO 2090-FILL-THE-SCREEN.                                  GA1LPGM 
01037                                                                   GA1LPGM 
01038  2090-FILL-THE-SCREEN.                                            GA1LPGM 
01039                                                                   GA1LPGM 
01040      PERFORM 4500-FILL-THE-SCREEN.                                GA1LPGM 
01041      EXEC CICS SEND MAP ('GA1LI01')                               GA1LPGM 
01042          MAPSET ('GA1LSET')                                       GA1LPGM 
01043          ERASE                                                    GA1LPGM 
01044          FROM (GA1LI01O)                                          GA1LPGM 
01045          END-EXEC.                                                GA1LPGM 
01046                                                                   GA1LPGM 
01047  2099-EXIT.   EXIT.                                               GA1LPGM 
01048      EJECT                                                        GA1LPGM 
01049  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1LPGM 
01050 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01051 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
01052 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01053      MOVE LOW-VALUES  TO                                          GA1LPGM 
01054         MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),                     GA1LPGM 
01055         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1LPGM 
01056         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1LPGM 
01057 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01058                                                                   GA1LPGM 
01059  2199-EXIT.   EXIT.                                               GA1LPGM 
01060      EJECT                                                        GA1LPGM 
01061 ******************************************************************GA1LPGM 
01062 **          X C T L   T O   A D D   S C R E E N                   GA1LPGM 
01063 **                                                                GA1LPGM 
01064 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1LPGM 
01065 ** ADDING ENTRIES.  WE READ THE ALL LEVEL TABULAR & PASS THE      GA1LPGM 
01066 ** ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL TABULAR  GA1LPGM 
01067 ** RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE PROGRAM GA1LPGM 
01068 ** ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE            GA1LPGM 
01069 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1LPGM 
01070 ******************************************************************GA1LPGM 
01071  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1LPGM 
01072      MOVE '3000'  TO  WS-PARA-ID.                                 GA1LPGM 
01073                                                                   GA1LPGM 
01074 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA1LPGM 
01075 *    EXEC CICS GETMAIN                                            GA1LPGM 
01076 *       SET(ADDRESS OF GCA-COMMAREA)                              GA1LPGM 
01077 *       INITIMG(WS-HEX-00)                                        GA1LPGM 
01078 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA1LPGM 
01079 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1LPGM 
01080 *                                                                 GA1LPGM 
01081 *    IF  MAP-FROM-MENU-ID = 'GS3A'                                GA1LPGM 
01082 *       MOVE MAP-ID-LINE TO GROUP-SPECIFIC-ID-LINE                GA1LPGM 
01083 *       MOVE GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                    GA1LPGM 
01084 *       MOVE GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO                GA1LPGM 
01085 *       MOVE GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1LPGM 
01086 *       MOVE GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1LPGM 
01087 *       MOVE SPACES  TO  GCA-L-O-B,                               GA1LPGM 
01088 *                        GCA-PROV-CTL,                            GA1LPGM 
01089 *                        GCA-BEN-PROV-ID.                         GA1LPGM 
01090 *                                                                 GA1LPGM 
01091 *    IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA1LPGM 
01092 *       MOVE MAP-ID-LINE TO CONTRACT-ID-LINE                      GA1LPGM 
01093 *       MOVE CONTRACT-GROUP-NO  TO  GCA-GRP-NO                    GA1LPGM 
01094 *       MOVE CONTRACT-SECTION-NO  TO  GCA-SECTN-NO                GA1LPGM 
01095 *       MOVE CONTRACT-LOB  TO  GCA-L-O-B                          GA1LPGM 
01096 *       MOVE CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                  GA1LPGM 
01097 *       MOVE CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1LPGM 
01098 *       MOVE CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1LPGM 
01099 *       MOVE SPACES  TO  GCA-BEN-PROV-ID.                         GA1LPGM 
01100 *                                                                 GA1LPGM 
01101 *    IF  MAP-FROM-MENU-ID = 'GC8A'                                GA1LPGM 
01102 *       MOVE MAP-ID-LINE TO BENEFIT-PROVISION-ID-LINE             GA1LPGM 
01103 *       MOVE BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                    GA1LPGM 
01104 *       MOVE BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO                GA1LPGM 
01105 *       MOVE BEN-PROV-LOB  TO  GCA-L-O-B                          GA1LPGM 
01106 *       MOVE BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                  GA1LPGM 
01107 *       MOVE BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1LPGM 
01108 *       MOVE BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1LPGM 
01109 *       MOVE BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                 GA1LPGM 
01110 *                                                                 GA1LPGM 
01111 *    MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID.                 GA1LPGM 
01112 *    MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT.             GA1LPGM 
01113 *    MOVE SPACES  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE,                GA1LPGM 
01114 *                     GCA-INTERNAL-TAB-ID,                        GA1LPGM 
01115 *                     GCA-INTERNAL-TAB-SLOT,                      GA1LPGM 
01116 *                     GCA-OCCURS-ENTRY-COUNTER,                   GA1LPGM 
01117 *                     GCA-ADD-DEL-IND.                            GA1LPGM 
01118 *    MOVE MAP-FROM-MENU-ID TO GCA-FROM-MENU-ID.                   GA1LPGM 
01119 *    MOVE ZEROES  TO  GCA-EFF-DT.                                 GA1LPGM 
01120 *                                                                 GA1LPGM 
01121 *    SET COMMAREA-PNTR TO ADDRESS                                 GA1LPGM 
01122 *    OF GCA-COMMAREA.                                             GA1LPGM 
01123 *                                                                 GA1LPGM 
01124 *    EXEC CICS XCTL  PROGRAM('GA2LPGM') COMMAREA(COMMAREA-PNTR)   GA1LPGM 
01125 *       LENGTH(4) END-EXEC.                                       GA1LPGM 
01126 *                                                                 GA1LPGM 
01127      EXEC CICS XCTL  PROGRAM('GA2LPGM')                           GA1LPGM 
01128                      COMMAREA(DFHCOMMAREA)                        GA1LPGM 
01129                      LENGTH (LENGTH OF DFHCOMMAREA)               GA1LPGM 
01130      END-EXEC.                                                    GA1LPGM 
01131                                                                   GA1LPGM 
01132  3099-EXIT.   EXIT.                                               GA1LPGM 
01133      EJECT                                                        GA1LPGM 
01134 ***************************************************************** GA1LPGM 
01135 **          D I S P L A Y   F I R S T   S C R E E N               GA1LPGM 
01136 **                                                                GA1LPGM 
01137 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1LPGM 
01138 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1LPGM 
01139 ** ALL LEVEL TABULAR RECORD & PASS US THE RECORD (PRECEEDED BY I/OGA1LPGM 
01140 ** PARMS AND WORKFILE KEY).  WE WILL THEN USE THAT RECORD TO BUILDGA1LPGM 
01141 ** THE SCREEN IMAGE.                                              GA1LPGM 
01142 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1LPGM 
01143 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1LPGM 
01144 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1LPGM 
01145 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1LPGM 
01146 ** DISPLAYED, THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,  GA1LPGM 
01147 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1LPGM 
01148 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1LPGM 
01149 ******************************************************************GA1LPGM 
01150  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1LPGM 
01151      MOVE '4000'  TO  WS-PARA-ID.                                 GA1LPGM 
01152                                                                   GA1LPGM 
01153 **+**++ MOVE LOW-VALUES TO SCREEN BEFORE PROCESSING ++++++++++++  GA1LPGM 
01154      MOVE LOW-VALUES TO GA1LI01I.                                 GA1LPGM 
01155                                                                   GA1LPGM 
01156      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1LPGM 
01157         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1LPGM 
01158            TO MAP-ERROR-MESSAGE                                   GA1LPGM 
01159         MOVE '1LC1'  TO  WS-ABEND-CODE                            GA1LPGM 
01160         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1LPGM 
01161                                                                   GA1LPGM 
01162 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA1LPGM 
01163 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1LPGM 
01164 ***  MOVE GCA-RECORD-POINTER  TO  ALL-LEVEL-TAB-PNTR.             GA1LPGM 
01165 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1LPGM 
01166 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1LPGM 
01167                                                                   GA1LPGM 
01168 *    SET ADDRESS OF GCA-COMMAREA                                  GA1LPGM 
01169 *    TO  INCOMING-COMMAREA-PNTR.                                  GA1LPGM 
01170                                                                   GA1LPGM 
01171      SET ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD                    GA1LPGM 
01172      TO  GCA-RECORD-POINTER.                                      GA1LPGM 
01173                                                                   GA1LPGM 
01174      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-TABULAR-ID.               GA1LPGM 
01175      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-TABULAR-SLOT.           GA1LPGM 
01176      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA1LPGM 
01177 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01178 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1LPGM 
01179 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1LPGM 
01180 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01181 *                                                                 GA1LPGM 
01182 *    MOVE WRK-EFF-DATE  TO  WS-YYDDD.                             GA1LPGM 
01183 *                                                                 GA1LPGM 
01184 *    SET WS-M-IDX  TO  1.                                         GA1LPGM 
01185 *    MOVE WS-YY  TO  WS-Y.                                        GA1LPGM 
01186 *    DIVIDE  WS-YY  BY  4  GIVING  WS-QUOTIENT                    GA1LPGM 
01187 *       REMAINDER  WS-REMAINDER.                                  GA1LPGM 
01188 *                                                                 GA1LPGM 
01189 *    MOVE '4010'  TO  WS-PARA-ID.                                 GA1LPGM 
01190 *4010-DETERMINE-DATE.                                             GA1LPGM 
01191 *    IF  WS-REMAINDER  =  ZERO  AND  WS-M-IDX  >  2               GA1LPGM 
01192 *       COMPUTE  WS-MONTH-TABLE (WS-M-IDX)  =                     GA1LPGM 
01193 *          WS-MONTH-TABLE (WS-M-IDX)  +  1.                       GA1LPGM 
01194 *    IF  WS-MONTH-TABLE (WS-M-IDX)  =  WS-DDD  OR  >  WS-DDD      GA1LPGM 
01195 *       SET WS-M-IDX  DOWN BY  1                                  GA1LPGM 
01196 *       SET WS-M  TO  WS-M-IDX                                    GA1LPGM 
01197 *       COMPUTE  WS-D  =  WS-DDD  -  WS-MONTH-TABLE (WS-M-IDX)    GA1LPGM 
01198 *    ELSE                                                         GA1LPGM 
01199 *       IF  WS-M-IDX  <  13                                       GA1LPGM 
01200 *          SET WS-M-IDX  UP BY  1                                 GA1LPGM 
01201 *          GO TO  4010-DETERMINE-DATE                             GA1LPGM 
01202 *       ELSE                                                      GA1LPGM 
01203 *          MOVE '*** INVALID EFFECTIVE DATE DISCOVERED, WE CAN NOTGA1LPGM 
01204 *-   ' PROCESS THIS REQUEST ***'  TO  MAP-ERROR-MESSAGE           GA1LPGM 
01205 *          MOVE '1LC2'  TO  WS-ABEND-CODE                         GA1LPGM 
01206 *          PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1LPGM 
01207 *                                                                 GA1LPGM 
01208 *    MOVE WS-MDY  TO  GCA-EFFECTIVE-DATE.                         GA1LPGM 
01209                                                                   GA1LPGM 
01210      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1LPGM 
01211         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-MAIN-TITLE        GA1LPGM 
01212         MOVE 'GROUP SPECIFIC ID = '  TO GRP-SPEC-ID-HEADING       GA1LPGM 
01213         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA1LPGM 
01214         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA1LPGM 
01215         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1LPGM 
01216         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA1LPGM 
01217         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1LPGM 
01218         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1LPGM 
01219         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1LPGM 
01220         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1LPGM 
01221                                                                   GA1LPGM 
01222      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1LPGM 
01223         MOVE CONTRACT-TITLE-LINE  TO  MAP-MAIN-TITLE              GA1LPGM 
01224         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1LPGM 
01225         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA1LPGM 
01226         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA1LPGM 
01227         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1LPGM 
01228         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA1LPGM 
01229         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1LPGM 
01230         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1LPGM 
01231         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1LPGM 
01232         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1LPGM 
01233         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1LPGM 
01234         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1LPGM 
01235         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1LPGM 
01236         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1LPGM 
01237                                                                   GA1LPGM 
01238      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1LPGM 
01239         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-MAIN-TITLE     GA1LPGM 
01240         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA1LPGM 
01241         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA1LPGM 
01242         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA1LPGM 
01243         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA1LPGM 
01244         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA1LPGM 
01245         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1LPGM 
01246         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA1LPGM 
01247         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1LPGM 
01248         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA1LPGM 
01249         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1LPGM 
01250         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA1LPGM 
01251         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1LPGM 
01252         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA1LPGM 
01253         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1LPGM 
01254                                                                   GA1LPGM 
01255      SET GAI-INDEX  TO  1.                                        GA1LPGM 
01256      MOVE GAI-ENTRY (GAI-INDEX)  TO  WS-SAVED-FIELDS.             GA1LPGM 
01257                                                                   GA1LPGM 
01258      PERFORM 4500-FILL-THE-SCREEN.                                GA1LPGM 
01259      EXEC CICS SEND   MAP('GA1LI01') MAPSET('GA1LSET') ERASE      GA1LPGM 
01260         FROM(GA1LI01O) END-EXEC.                                  GA1LPGM 
01261                                                                   GA1LPGM 
01262  4099-EXIT.   EXIT.                                               GA1LPGM 
01263      EJECT                                                        GA1LPGM 
01264 ***************************************************************** GA1LPGM 
01265 **             F I L L   T H E   S C R E E N                      GA1LPGM 
01266 **                                                                GA1LPGM 
01267 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1LPGM 
01268 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1LPGM 
01269 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1LPGM 
01270 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1LPGM 
01271 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1LPGM 
01272 ******************************************************************GA1LPGM 
01273  4500-FILL-THE-SCREEN SECTION.                                    GA1LPGM 
01274                                                                   GA1LPGM 
01275      MOVE '4500'  TO  WS-PARA-ID.                                 GA1LPGM 
01276      MOVE  GAI-ENTRY-COUNT  TO  GAI-ENTRY-COUNT.                  GA1LPGM 
01277      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1LPGM 
01278                                                                   GA1LPGM 
01279      COMPUTE WS-SELECT-OF = GAI-ENTRY-COUNT - 1.                  GA1LPGM 
01280      MOVE WS-SELECT-OF TO WS-SELECT-OF-MASK.                      GA1LPGM 
01281      MOVE WS-SELECT-OF-TRUNC TO MAP-SELECT-FROM                   GA1LPGM 
01282                                 MAP-SELECT-TO                     GA1LPGM 
01283                                 MAP-SELECT-OF.                    GA1LPGM 
01284                                                                   GA1LPGM 
01285      IF GAI-ENTRY-COUNT  NOT >  1                                 GA1LPGM 
01286         MOVE '4530'  TO  WS-PARA-ID                               GA1LPGM 
01287         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1LPGM 
01288                                                                   GA1LPGM 
01289      SET GAI-INDEX  TO  1.                                        GA1LPGM 
01290      MOVE '4510'  TO  WS-PARA-ID.                                 GA1LPGM 
01291  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1LPGM 
01292 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01293 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
01294 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01295      IF GAI-ENTRY (GAI-INDEX)  <  WS-SAVED-FIELDS                 GA1LPGM 
01296 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01297         SET GAI-INDEX  UP BY  1                                   GA1LPGM 
01298         IF  GAI-INDEX  <  GAI-ENTRY-COUNT                         GA1LPGM 
01299            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1LPGM 
01300         ELSE                                                      GA1LPGM 
01301            SET GAI-INDEX  TO  1.                                  GA1LPGM 
01302                                                                   GA1LPGM 
01303      SET WS-SELECT-FROM TO GAI-INDEX.                             GA1LPGM 
01304      MOVE WS-SELECT-FROM TO WS-SELECT-FROM-MASK.                  GA1LPGM 
01305      MOVE WS-SELECT-FROM-TRUNC TO MAP-SELECT-FROM.                GA1LPGM 
01306                                                                   GA1LPGM 
01307      MOVE '4520'  TO  WS-PARA-ID.                                 GA1LPGM 
01308  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1LPGM 
01309      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1LPGM 
01310      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1LPGM 
01311 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01312 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
01313 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01314      MOVE GAI-PROCEDURE-ARGUMENT (GAI-INDEX) TO                   GA1LPGM 
01315         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2).              GA1LPGM 
01316      MOVE GAI-COMBINATION-CODE-FUNCTION (GAI-INDEX) TO            GA1LPGM 
01317         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1LPGM 
01318 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01319                                                                   GA1LPGM 
01320      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1LPGM 
01321         SET  MAP-IDX1  UP BY  1                                   GA1LPGM 
01322      ELSE                                                         GA1LPGM 
01323         IF MAP-IDX2  <  WS-MAP-COL                                GA1LPGM 
01324            SET  MAP-IDX1  TO  1                                   GA1LPGM 
01325            SET  MAP-IDX2  UP BY  1                                GA1LPGM 
01326         ELSE                                                      GA1LPGM 
01327            SET WS-SELECT-TO TO GAI-INDEX                          GA1LPGM 
01328            MOVE WS-SELECT-TO TO WS-SELECT-TO-MASK                 GA1LPGM 
01329            MOVE WS-SELECT-TO-TRUNC TO MAP-SELECT-TO               GA1LPGM 
01330            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1LPGM 
01331                                                                   GA1LPGM 
01332      IF GAI-INDEX  <  (GAI-ENTRY-COUNT - 1 )                      GA1LPGM 
01333         SET  GAI-INDEX  UP BY  1                                  GA1LPGM 
01334         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1LPGM 
01335                                                                   GA1LPGM 
01336      SET WS-SELECT-TO TO GAI-INDEX.                               GA1LPGM 
01337      MOVE WS-SELECT-TO TO WS-SELECT-TO-MASK.                      GA1LPGM 
01338      MOVE WS-SELECT-TO-TRUNC TO MAP-SELECT-TO.                    GA1LPGM 
01339                                                                   GA1LPGM 
01340      MOVE '4530'  TO  WS-PARA-ID.                                 GA1LPGM 
01341  4530-FILL-REST-WITH-NULLS.                                       GA1LPGM 
01342      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1LPGM 
01343 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01344 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1LPGM 
01345 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01346      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1LPGM 
01347         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1LPGM 
01348         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1LPGM 
01349 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1LPGM 
01350      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1LPGM 
01351         SET  MAP-IDX1  UP BY  1                                   GA1LPGM 
01352         GO TO 4530-FILL-REST-WITH-NULLS                           GA1LPGM 
01353      ELSE                                                         GA1LPGM 
01354         IF MAP-IDX2  <  WS-MAP-COL                                GA1LPGM 
01355            SET  MAP-IDX1  TO  1                                   GA1LPGM 
01356            SET  MAP-IDX2  UP BY  1                                GA1LPGM 
01357            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1LPGM 
01358                                                                   GA1LPGM 
01359      MOVE '4540'  TO  WS-PARA-ID.                                 GA1LPGM 
01360  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1LPGM 
01361      IF GAI-ENTRY-COUNT  =  1                                     GA1LPGM 
01362         MOVE '*** NO ENTRIES TO DELETE ***'                       GA1LPGM 
01363         TO  MAP-ERROR-MESSAGE                                     GA1LPGM 
01364         GO TO 4599-EXIT.                                          GA1LPGM 
01365                                                                   GA1LPGM 
01366      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1LPGM 
01367         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'                 GA1LPGM 
01368         TO  MAP-ERROR-MESSAGE.                                    GA1LPGM 
01369                                                                   GA1LPGM 
01370  4599-EXIT.     EXIT.                                             GA1LPGM 
01371      EJECT                                                        GA1LPGM 
01372 ***************************************************************** GA1LPGM 
01373 **        X C T L   T O   P R E V I O U S   M E N U               GA1LPGM 
01374 **                                                                GA1LPGM 
01375 **  THE OPERATOR WANTS TO RETURN TO THE MENU THIS PROGRAM         GA1LPGM 
01376 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND     GA1LPGM 
01377 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA1LPGM 
01378 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM MENU  GA1LPGM 
01379 ** ID' FIELD).                                                    GA1LPGM 
01380 ******************************************************************GA1LPGM 
01381  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1LPGM 
01382      MOVE '5000'  TO  WS-PARA-ID.                                 GA1LPGM 
01383                                                                   GA1LPGM 
01384      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA1LPGM 
01385         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA1LPGM 
01386                                                                   GA1LPGM 
01387      IF  MAP-FROM-MENU-ID = 'GC4A'                                GA1LPGM 
01388         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA1LPGM 
01389                                                                   GA1LPGM 
01390      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA1LPGM 
01391         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA1LPGM 
01392                                                                   GA1LPGM 
01393      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA1LPGM 
01394         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA1LPGM 
01395                                                                   GA1LPGM 
01396                                                                   GA1LPGM 
01397  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA1LPGM 
01398      MOVE '5010'  TO  WS-PARA-ID.                                 GA1LPGM 
01399                                                                   GA1LPGM 
01400      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1LPGM 
01401          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1LPGM 
01402                 GC-GCGRPSPC-FIXED-LEN      +                      GA1LPGM 
01403         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1LPGM 
01404                                                                   GA1LPGM 
01405 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA1LPGM 
01406      EXEC CICS GETMAIN                                            GA1LPGM 
01407         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA1LPGM 
01408         INITIMG(WS-HEX-00)                                        GA1LPGM 
01409         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01410 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA1LPGM 
01411                                                                   GA1LPGM 
01412      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1LPGM 
01413                                                                   GA1LPGM 
01414      MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           GA1LPGM 
01415      MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           GA1LPGM 
01416      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE              GA1LPGM 
01417      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1LPGM 
01418      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA1LPGM 
01419      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1LPGM 
01420      MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS,           GA1LPGM 
01421                                   GCIO-WRK-PROVIDER-CONTROL.      GA1LPGM 
01422      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1LPGM 
01423      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1LPGM 
01424                                                                   GA1LPGM 
01425 **   MOVE  WS-Y  TO  WS-YY.                                       GA1LPGM 
01426 **   IF WS-M  >  2                                                GA1LPGM 
01427 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1LPGM 
01428 **         REMAINDER  WS-REMAINDER                                GA1LPGM 
01429 **   ELSE                                                         GA1LPGM 
01430 **      MOVE 1  TO  WS-REMAINDER.                                 GA1LPGM 
01431 **   SET WS-M-IDX  TO  WS-M.                                      GA1LPGM 
01432 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1LPGM 
01433 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1LPGM 
01434 **   IF WS-REMAINDER  =  ZERO                                     GA1LPGM 
01435 **      ADD 1  TO  WS-DDD.                                        GA1LPGM 
01436 **                                                                GA1LPGM 
01437 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1LPGM 
01438      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA1LPGM 
01439      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1LPGM 
01440                       GCIO-WRK-TAB-PROVISION-ID.                  GA1LPGM 
01441      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1LPGM 
01442                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1LPGM 
01443      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1LPGM 
01444                                                                   GA1LPGM 
01445      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA1LPGM 
01446      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA1LPGM 
01447                                                                   GA1LPGM 
01448      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1LPGM 
01449                                                                   GA1LPGM 
01450      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1LPGM 
01451         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA1LPGM 
01452         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01453                                                                   GA1LPGM 
01454      IF  NOT GCIO2-GOOD-RETURN                                    GA1LPGM 
01455         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1LPGM 
01456 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1LPGM 
01457         MOVE '1LF4'  TO  WS-ABEND-CODE                            GA1LPGM 
01458         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1LPGM 
01459                                                                   GA1LPGM 
01460      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1LPGM 
01461          GC-WORKFILE-KEY-LEN        +                             GA1LPGM 
01462                 GC-GCGRPSPC-FIXED-LEN      +                      GA1LPGM 
01463         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1LPGM 
01464                                                                   GA1LPGM 
01465      EXEC CICS XCTL  PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)   GA1LPGM 
01466         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01467                                                                   GA1LPGM 
01468      GO  TO  5099-EXIT.                                           GA1LPGM 
01469                                                                   GA1LPGM 
01470  5020-XCTL-TO-CONTRACT-MENU.                                      GA1LPGM 
01471      MOVE '5020'  TO  WS-PARA-ID.                                 GA1LPGM 
01472                                                                   GA1LPGM 
01473      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1LPGM 
01474          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1LPGM 
01475                 GC-GCCONTR-MAX-REC-LEN.                           GA1LPGM 
01476                                                                   GA1LPGM 
01477 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA1LPGM 
01478      EXEC CICS GETMAIN                                            GA1LPGM 
01479         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA1LPGM 
01480         INITIMG(WS-HEX-00)                                        GA1LPGM 
01481         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01482                                                                   GA1LPGM 
01483 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA1LPGM 
01484 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA1LPGM 
01485                                                                   GA1LPGM 
01486      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1LPGM 
01487                                                                   GA1LPGM 
01488      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1LPGM 
01489      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1LPGM 
01490      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1LPGM 
01491      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1LPGM 
01492      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA1LPGM 
01493      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1LPGM 
01494      MOVE CONTRACT-LOB         TO GCIO-WRK-LINE-OF-BUS.           GA1LPGM 
01495      MOVE CONTRACT-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL.      GA1LPGM 
01496      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1LPGM 
01497      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1LPGM 
01498                                                                   GA1LPGM 
01499 **   MOVE  WS-Y  TO  WS-YY.                                       GA1LPGM 
01500 **   IF WS-M  >  2                                                GA1LPGM 
01501 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1LPGM 
01502 **         REMAINDER  WS-REMAINDER                                GA1LPGM 
01503 **   ELSE                                                         GA1LPGM 
01504 **      MOVE 1  TO  WS-REMAINDER.                                 GA1LPGM 
01505 **   SET WS-M-IDX  TO  WS-M.                                      GA1LPGM 
01506 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1LPGM 
01507 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1LPGM 
01508 **   IF WS-REMAINDER  =  ZERO                                     GA1LPGM 
01509 **      ADD 1  TO  WS-DDD.                                        GA1LPGM 
01510 **                                                                GA1LPGM 
01511 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1LPGM 
01512      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA1LPGM 
01513      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1LPGM 
01514                       GCIO-WRK-TAB-PROVISION-ID.                  GA1LPGM 
01515      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1LPGM 
01516                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1LPGM 
01517      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA1LPGM 
01518                                                                   GA1LPGM 
01519      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA1LPGM 
01520      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA1LPGM 
01521                                                                   GA1LPGM 
01522      MOVE  'RD '  TO  GCIO3-FILE-ACCESS-CODE.                     GA1LPGM 
01523                                                                   GA1LPGM 
01524      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1LPGM 
01525         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA1LPGM 
01526         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01527                                                                   GA1LPGM 
01528      IF  NOT GCIO3-GOOD-RETURN                                    GA1LPGM 
01529         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1LPGM 
01530 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1LPGM 
01531         MOVE '1LF5'  TO  WS-ABEND-CODE                            GA1LPGM 
01532         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1LPGM 
01533                                                                   GA1LPGM 
01534      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1LPGM 
01535          GC-WORKFILE-KEY-LEN        +                             GA1LPGM 
01536                 GC-GCCONTR-MAX-REC-LEN.                           GA1LPGM 
01537                                                                   GA1LPGM 
01538      EXEC CICS XCTL  PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)   GA1LPGM 
01539         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01540                                                                   GA1LPGM 
01541      GO  TO  5099-EXIT.                                           GA1LPGM 
01542                                                                   GA1LPGM 
01543  5030-XCTL-TO-BEN-PROV-MENU.                                      GA1LPGM 
01544      MOVE '5030'  TO  WS-PARA-ID.                                 GA1LPGM 
01545                                                                   GA1LPGM 
01546      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1LPGM 
01547          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1LPGM 
01548                 GC-GCBENPRV-FIXED-LEN      +                      GA1LPGM 
01549         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1LPGM 
01550                                                                   GA1LPGM 
01551 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA1LPGM 
01552      EXEC CICS GETMAIN                                            GA1LPGM 
01553         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA1LPGM 
01554         INITIMG(WS-HEX-00)                                        GA1LPGM 
01555         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01556 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA1LPGM 
01557                                                                   GA1LPGM 
01558      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1LPGM 
01559                                                                   GA1LPGM 
01560      MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           GA1LPGM 
01561      MOVE 'C4'                 TO GCIO-WRK-RECORD-TYPE.           GA1LPGM 
01562      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1LPGM 
01563      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1LPGM 
01564      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA1LPGM 
01565      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1LPGM 
01566      MOVE BEN-PROV-LOB         TO GCIO-WRK-LINE-OF-BUS.           GA1LPGM 
01567      MOVE BEN-PROV-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL.      GA1LPGM 
01568      MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1LPGM 
01569      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1LPGM 
01570      MOVE BEN-PROV-ID-NO       TO GCIO-WRK-PROVISION-ID.          GA1LPGM 
01571                                                                   GA1LPGM 
01572 **   MOVE  WS-Y  TO  WS-YY.                                       GA1LPGM 
01573 **   IF WS-M  >  2                                                GA1LPGM 
01574 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1LPGM 
01575 **         REMAINDER  WS-REMAINDER                                GA1LPGM 
01576 **   ELSE                                                         GA1LPGM 
01577 **      MOVE 1  TO  WS-REMAINDER.                                 GA1LPGM 
01578 **   SET WS-M-IDX  TO  WS-M.                                      GA1LPGM 
01579 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1LPGM 
01580 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1LPGM 
01581 **   IF WS-REMAINDER  =  ZERO                                     GA1LPGM 
01582 **      ADD 1  TO  WS-DDD.                                        GA1LPGM 
01583 **                                                                GA1LPGM 
01584 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1LPGM 
01585      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA1LPGM 
01586      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA1LPGM 
01587      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA1LPGM 
01588      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1LPGM 
01589      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA1LPGM 
01590                                                                   GA1LPGM 
01591      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA1LPGM 
01592      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA1LPGM 
01593                                                                   GA1LPGM 
01594      MOVE  'RD '  TO  GCIO4-FILE-ACCESS-CODE.                     GA1LPGM 
01595                                                                   GA1LPGM 
01596      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1LPGM 
01597         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA1LPGM 
01598         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01599                                                                   GA1LPGM 
01600      IF  NOT GCIO4-GOOD-RETURN                                    GA1LPGM 
01601         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1LPGM 
01602 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1LPGM 
01603         MOVE '1LF6'  TO  WS-ABEND-CODE                            GA1LPGM 
01604         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1LPGM 
01605                                                                   GA1LPGM 
01606      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1LPGM 
01607          GC-WORKFILE-KEY-LEN        +                             GA1LPGM 
01608                 GC-GCBENPRV-FIXED-LEN      +                      GA1LPGM 
01609         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1LPGM 
01610                                                                   GA1LPGM 
01611      EXEC CICS XCTL  PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)   GA1LPGM 
01612         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1LPGM 
01613                                                                   GA1LPGM 
01614      GO  TO  5099-EXIT.                                           GA1LPGM 
01615                                                                   GA1LPGM 
01616                                                                   GA1LPGM 
01617  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA1LPGM 
01618      MOVE '5040'  TO  WS-PARA-ID.                                 GA1LPGM 
01619                                                                   GA1LPGM 
01620      EXEC CICS XCTL                                               GA1LPGM 
01621                PROGRAM('GTM1PGM')                                 GA1LPGM 
01622                END-EXEC.                                          GA1LPGM 
01623                                                                   GA1LPGM 
01624      GO  TO  5099-EXIT.                                           GA1LPGM 
01625                                                                   GA1LPGM 
01626  5099-EXIT.                                                       GA1LPGM 
01627      EXIT.                                                        GA1LPGM 
01628      EJECT                                                        GA1LPGM 
01629 ***************************************************************** GA1LPGM 
01630 **           X C T L   T O   M A I N   M E N U                    GA1LPGM 
01631 **                                                                GA1LPGM 
01632 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1LPGM 
01633 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1LPGM 
01634 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1LPGM 
01635 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOME OF AGA1LPGM 
01636 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1LPGM 
01637 ** AND PROGRESS DOWN.                                             GA1LPGM 
01638 ******************************************************************GA1LPGM 
01639  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1LPGM 
01640      MOVE '6000'  TO  WS-PARA-ID.                                 GA1LPGM 
01641      MOVE '1LP1'  TO  WS-ABEND-CODE.                              GA1LPGM 
01642                                                                   GA1LPGM 
01643      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1LPGM 
01644                                                                   GA1LPGM 
01645  6099-EXIT.     EXIT.                                             GA1LPGM 
01646      EJECT                                                        GA1LPGM 
01647 ******************************************************************GA1LPGM 
01648  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1LPGM 
01649                                                                   GA1LPGM 
01650      SET MAP-IDX1  TO  7.                                         GA1LPGM 
01651      SET MAP-IDX2  TO  1.                                         GA1LPGM 
01652      MOVE -1  TO  MAP-SELECT-LEN.                                 GA1LPGM 
01653                                                                   GA1LPGM 
01654      EXEC CICS SEND   MAP('GA1LI01') MAPSET('GA1LSET') ERASE      GA1LPGM 
01655         FROM(GA1LI01O) WAIT END-EXEC.                             GA1LPGM 
01656                                                                   GA1LPGM 
01657      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1LPGM 
01658                                                                   GA1LPGM 
01659  9999-EXIT.     EXIT.                                             GA1LPGM 
