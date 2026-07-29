00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. GA1MPGM.                                             GA1MPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV003
00004  AUTHOR. SANDRA BUCH.                                             GA1MPGM 
00005  DATE-WRITTEN. 02/14/85.                                          GA1MPGM 
00006  DATE-COMPILED.                                                   GA1MPGM 
00007      SKIP3                                                        GA1MPGM 
00008 ******************************************************************GA1MPGM 
00009 *   GA1MPGM     ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM  GA1MPGM 
00010 *                              COSMETIC SURGICAL            GA1M  GA1MPGM 
00011 *                                                                 GA1MPGM 
00012 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1MPGM 
00013 *   CURRENTLY ON THE ALL LEVEL TABULAR RECORD.                    GA1MPGM 
00014 *                                                                 GA1MPGM 
00015 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1MPGM 
00016 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN DECIDE IF   GA1MPGM 
00017 *   ANY ENTRIES WILL BE DELETED.  THE SCREEN ENTRY WILL BE        GA1MPGM 
00018 *   VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD WILL BE   GA1MPGM 
00019 *   MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK INTO THE    GA1MPGM 
00020 *   RECORD BEFORE UPDATING THE RECORD.                            GA1MPGM 
00021 *                                                                 GA1MPGM 
00022 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID:#ACOS)  GA1MPGM 
00023 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1MPGM 
00024 *   XCTL TO TRANS GA2M OR PROGRAM GA2MPGM.  THIS PROGRAM WILL     GA1MPGM 
00025 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1MPGM 
00026 *   TABLE.                                                        GA1MPGM 
00027 *                                                                 GA1MPGM 
00028 *   PF7/PF19  PAGE BACKWARD.                                      GA1MPGM 
00029 *   PF8/PF20  PAGE FORWARD.                                       GA1MPGM 
00030 *   PF10/PF22 PAGE TO BOTTOM.                                     GA1MPGM 
00031 *   PF11/PF23 PAGE TO TOP.                                        GA1MPGM 
00032 *                                                                 GA1MPGM 
00033 *   FUNC CODE: GA1M                                               GA1MPGM 
00034 *   MAPSET:    GA1MSETC <<<< REDEFINED BY USER DEFINED MAP >>>>   GA1MPGM 
00035 *   FILES:     GCPSWORK                                           GA1MPGM 
00036 *                                                                 GA1MPGM 
00037 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00038 *                                                                 GA1MPGM 
00039 *    TAILORING INSTRUCTIONS:                                      GA1MPGM 
00040 *                                                                 GA1MPGM 
00041 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1MPGM 
00042 *                                                                 GA1MPGM 
00043 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1M/     GA1MPGM 
00044 *              SCREEN PAGE NUMBER                 /009I/001M/     GA1MPGM 
00045 *              ADD PROGRAM FUNCTION CODE          /GA9I/GA2M/     GA1MPGM 
00046 *              BENEFIT PROVISION TABULAR ID       /#PPF/#ACOS/    GA1MPGM 
00047 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GAJ/       GA1MPGM 
00048 *                                                                 GA1MPGM 
00049 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1MPGM 
00050 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1MPGM 
00051 *     OCCURANCES.                                                 GA1MPGM 
00052 *                                                                 GA1MPGM 
00053 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1MPGM 
00054 *     THAT MUST BE CHANGED.                                       GA1MPGM 
00055 *                                                                 GA1MPGM 
00056 ******************************************************************GA1MPGM 
00057 *                                                                *GA1MPGM 
00058 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA1MPGM 
00059 *       *-*         U P D A T E   H I S T O R Y         *-*      *GA1MPGM 
00060 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA1MPGM 
00061 *                                                                *GA1MPGM 
00062 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GA1MPGM 
00063 *                                                                *GA1MPGM 
00064 *            01/16/86  ENW  CHANGED:                   FROM:  TO:*GA1MPGM 
00065 *                           WS-CONTRACT-FIXED-PORTION   483  563 *GA1MPGM 
00066 *                           WS-CONTRACT-KEY-LENGTH       12   10 *GA1MPGM 
00067 *                           WS-CONTRACT-MAX-OCCURS      450  520 *GA1MPGM 
00068 *                                                                *GA1MPGM 
00069 *    D136/   08/27/86  AMJ  1. ADD SUPPORT FOR PF7,8,10 AND 11.  *GA1MPGM 
00070 *    D137                   2. ADD XXXXXXXXX ID SELECTION FIELD. *GA1MPGM 
00071 *                           3. ADD LOCATION COUNTERS (I.E 1 TO 36*GA1MPGM 
00072 *                              OF 54 XXXXXXXXXX DISPLAYED) TO    *GA1MPGM 
00073 *                              SCREEN.                           *GA1MPGM 
00074 *                           4. USE USER-DEFINED LOGICAL MAP FOR  *GA1MPGM 
00075 *                              SCREEN.  THIS REPLACES THE PARTIAL*GA1MPGM 
00076 *                              USE OF BMS MAP AND USER-DEFINED.  *GA1MPGM 
00077 *                                                                *GA1MPGM 
00078 *    D0120   01/30/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT   *GA1MPGM 
00079 *                           EXECUTED FROM TRANSACTION GTM1:      *GA1MPGM 
00080 *                           1. PF1/PF13 - CONSTRUCT COMMAREA AS  *GA1MPGM 
00081 *                              IF GC4A HAD CALLED, XCTL TO ADD   *GA1MPGM 
00082 *                              SCREEN PROGRAM.                   *GA1MPGM 
00083 *                           2. PF3/PF15 - CONSTRUCT COMMAREA AS  *GA1MPGM 
00084 *                              IF GC4A HAD CALLED, XCTL TO       *GA1MPGM 
00085 *                              GTM1PGM.                          *GA1MPGM 
00086 *                                                                *GA1MPGM 
00087 *    D116     8/17/87  FRY    CAPTURE OPERATOR-ID WHEN A 'C3',   *GA1MPGM 
00088 *                             'C5', OR 'G3' RECORD IS UPDATED.   *GA1MPGM 
00089 *                                                                *GA1MPGM 
00090 *  11161  11/17/90  PFH   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA1MPGM 
00091 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA1MPGM 
00092 *                         ROUTINES.                              *GA1MPGM 
00093 *                                                                *GA1MPGM 
00094 *                                                                *GA1MPGM 
00095 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD      *GA1MPGM 
00096 *                           FROM ONE POSITION TO TWO POSITIONS.  *GA1MPGM 
00097 *                                                                *GA1MPGM 
00098 *D12009 09/27/91  GDM   CONVERT TO COBOL II                      *GA1MPGM 
00099 *                                                                *GA1MPGM 
00100 *  14726/                                                         GA1MPGM 
00101 *  15057     10/24/97  AB   ADDED CODE TO SUPPORT THE YEAR        GA1MPGM 
00102 *                           2000 AND THE EXPANSION OF THE         GA1MPGM 
00103 *                           CONTRACT KEY TO SUPPORT THE TX        GA1MPGM 
00104 *                           MERGER.                               GA1MPGM 
00105 *                           CAHNGED POINTER TO COMMAREA TO        GA1MPGM 
00106 *                            ACTUAL USE OF COMMAREA               GA1MPGM 
00107 *                                                                 GA1MPGM 
00108 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION        GA1MPGM 
00109 *                                                                 GA1MPGM 
00110 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO GA1MPGM 
00111 *                             7 BYTES. CHANGE # OF OCCURS TO 396  GA1MPGM 
00112 *                             ON #ACOS TABULAR.                   GA1MPGM 
00112 *                                                                *GA1MPGM 
00112 * ICD-10   07/06/11     BA   EXPAND MAP-SELECT FIELD FROM 6 TO 7.*GA1MPGM 
00113 ******************************************************************GA1MPGM 
00114 ******************************************************************GA1MPGM 
00115 ******************************************************************GA1MPGM 
00116  ENVIRONMENT DIVISION.                                            GA1MPGM 
00117      EJECT                                                        GA1MPGM 
00118  DATA DIVISION.                                                   GA1MPGM 
00119  WORKING-STORAGE SECTION.                                         GA1MPGM 
00120                                                                   GA1MPGM 
00121  01  WS-MISC.                                                     GA1MPGM 
00122      05  WS-BEGIN                PIC X(24)  VALUE                 GA1MPGM 
00123      '***GA1MPGM WS BEGINS***'.                                   GA1MPGM 
00124      05  WS-PARA-ID              PIC X(4) VALUE 'XXXX'.           GA1MPGM 
00125      05  WS-ABEND-CODE           PIC X(4) VALUE 'XXXX'.           GA1MPGM 
00126      05  WS-SELECT-FROM          PIC S9(4) COMP SYNC VALUE +0.    GA1MPGM 
00127      05  WS-SELECT-TO            PIC S9(4) COMP SYNC VALUE +0.    GA1MPGM 
00128      05  WS-SELECT-OF            PIC S9(4) COMP SYNC VALUE +0.    GA1MPGM 
00129      05  WS-SELECT-FROM-MASK     PIC ZZZ9.                        GA1MPGM 
00130      05  WS-SELECT-FROM-MASK-RDF REDEFINES                        GA1MPGM 
00131              WS-SELECT-FROM-MASK.                                 GA1MPGM 
00132          10  FILLER              PIC X.                           GA1MPGM 
00133          10  WS-SELECT-FROM-TRUNC                                 GA1MPGM 
00134                                  PIC XXX.                         GA1MPGM 
00135      05  WS-SELECT-TO-MASK       PIC ZZZ9.                        GA1MPGM 
00136      05  WS-SELECT-TO-MASK-RDF REDEFINES                          GA1MPGM 
00137              WS-SELECT-TO-MASK.                                   GA1MPGM 
00138          10  FILLER              PIC X.                           GA1MPGM 
00139          10  WS-SELECT-TO-TRUNC  PIC XXX.                         GA1MPGM 
00140      05  WS-SELECT-OF-MASK       PIC ZZZ9.                        GA1MPGM 
00141      05  WS-SELECT-OF-MASK-RDF REDEFINES                          GA1MPGM 
00142              WS-SELECT-OF-MASK.                                   GA1MPGM 
00143          10  FILLER              PIC X.                           GA1MPGM 
00144          10  WS-SELECT-OF-TRUNC  PIC XXX.                         GA1MPGM 
00145      05  WS-GAJ-INDEX            PIC S9(4) COMP SYNC VALUE +0.    GA1MPGM 
00146                                                                   GA1MPGM 
00147 ** MAP COBOL SCREEN DSECTS **                                     GA1MPGM 
00148  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1MPGM 
00149      '***  I/O MAPAREA ***'.                                      GA1MPGM 
00150  COPY GA1MSETC.                                                   GA1MPGM 
00151 /*****************************************************************GA1MPGM 
00152 ******************************************************************GA1MPGM 
00153 ******************************************************************GA1MPGM 
00154 **                                                              **GA1MPGM 
00155 **     THIS IS A USER-DEFINED LOGICAL MAP.  ANY CHANGES TO      **GA1MPGM 
00156 **     MAPSET GA1MSETC AFFECTING ITS LENGTH MUST BE TAKEN       **GA1MPGM 
00157 **     INTO ACCOUNT HERE.                                       **GA1MPGM 
00158 **                                                              **GA1MPGM 
00159 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA1MPGM 
00160 **                                                              **GA1MPGM 
00161 **                                            AMJ 8/27/86       **GA1MPGM 
00162 **                                                              **GA1MPGM 
00163 ******************************************************************GA1MPGM 
00164 ******************************************************************GA1MPGM 
00165 ******************************************************************GA1MPGM 
00166                                                                   GA1MPGM 
00167  01  MAP-USER-DEFINED REDEFINES GA1MI01I.                         GA1MPGM 
00168                                                                   GA1MPGM 
00169      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA1MPGM 
00170                                                                   GA1MPGM 
00171      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA1MPGM 
00172      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA1MPGM 
00173      05  MAP-FUNCTION-CODE                PIC X(04).              GA1MPGM 
00174                                                                   GA1MPGM 
00175      05  MAP-MAIN-TITLE-LEN               PIC S9(4) COMP SYNC.    GA1MPGM 
00176      05  MAP-MAIN-TITLE-ATTR              PIC X.                  GA1MPGM 
00177      05  MAP-MAIN-TITLE                   PIC X(40).              GA1MPGM 
00178                                                                   GA1MPGM 
00179      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA1MPGM 
00180      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA1MPGM 
00181      05  MAP-SCREEN-ID                    PIC X(06).              GA1MPGM 
00182                                                                   GA1MPGM 
00183      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA1MPGM 
00184      05  MAP-ID-LINE-ATTR                 PIC X.                  GA1MPGM 
00185      05  MAP-ID-LINE                      PIC X(79).              GA1MPGM 
00186                                                                   GA1MPGM 
00187      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA1MPGM 
00188          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA1MPGM 
00189          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA1MPGM 
00190          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA1MPGM 
00191          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA1MPGM 
00192          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA1MPGM 
00193          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA1MPGM 
00194          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA1MPGM 
00195          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA1MPGM 
00196          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA1MPGM 
00197          10  FILLER                           PIC X(18).          GA1MPGM 
00198      05  CONTRACT-ID-LINE REDEFINES MAP-ID-LINE.                  GA1MPGM 
00199          10  CONTRACT-ID-HEADING              PIC X(14).          GA1MPGM 
00200          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA1MPGM 
00201          10  CONTRACT-GROUP-NO                PIC X(6).           GA1MPGM 
00202          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA1MPGM 
00203          10  CONTRACT-SECTION-NO              PIC X(4).           GA1MPGM 
00204          10  CONTRACT-LOB-HEADING             PIC X(6).           GA1MPGM 
00205          10  CONTRACT-LOB                     PIC X.              GA1MPGM 
00206          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA1MPGM 
00207          10  CONTRACT-PROV-CTL                PIC XX.             GA1MPGM 
00208          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA1MPGM 
00209          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA1MPGM 
00210          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA1MPGM 
00211          10  CONTRACT-EFF-DATE                PIC X(6).           GA1MPGM 
00212          10  FILLER                           PIC X(09).          GA1MPGM 
00213      05  BENEFIT-PROVISION-ID-LINE REDEFINES MAP-ID-LINE.         GA1MPGM 
00214          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA1MPGM 
00215          10  BEN-PROV-GROUP-NO                PIC X(6).           GA1MPGM 
00216          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA1MPGM 
00217          10  BEN-PROV-SECTION-NO              PIC X(4).           GA1MPGM 
00218          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA1MPGM 
00219          10  BEN-PROV-LOB                     PIC X.              GA1MPGM 
00220          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA1MPGM 
00221          10  BEN-PROV-PROV-CTL                PIC XX.             GA1MPGM 
00222          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA1MPGM 
00223          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA1MPGM 
00224          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA1MPGM 
00225          10  BEN-PROV-EFF-DATE                PIC X(6).           GA1MPGM 
00226          10  BEN-PROV-ID-HEADING              PIC X(8).           GA1MPGM 
00227          10  BEN-PROV-ID-NO                   PIC X(6).           GA1MPGM 
00228          10  FILLER                           PIC X(09).          GA1MPGM 
00229                                                                   GA1MPGM 
00230      05  MAP-TABULAR-ID-LEN               PIC S9(4) COMP SYNC.    GA1MPGM 
00231      05  MAP-TABULAR-ID-ATTR              PIC X.                  GA1MPGM 
00232      05  MAP-TABULAR-ID                   PIC X(06).              GA1MPGM 
00233                                                                   GA1MPGM 
00234      05  MAP-TABULAR-SLOT-LEN             PIC S9(4) COMP SYNC.    GA1MPGM 
00235      05  MAP-TABULAR-SLOT-ATTR            PIC X.                  GA1MPGM 
00236      05  MAP-TABULAR-SLOT                 PIC X(07).              GA1MPGM 
00237                                                                   GA1MPGM 
00238      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA1MPGM 
00239      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA1MPGM 
00240      05  MAP-FROM-MENU-ID                 PIC X(04).              GA1MPGM 
00241                                                                   GA1MPGM 
00242      05  MAP-SELECT-TXT1-LEN              PIC S9(4) COMP SYNC.    GA1MPGM 
00243      05  MAP-SELECT-TXT1-ATTR             PIC X.                  GA1MPGM 
00244      05  MAP-SELECT-TXT1                  PIC X(07).              GA1MPGM 
00245                                                                   GA1MPGM 
00246      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA1MPGM 
00247      05  MAP-SELECT-ATTR                  PIC X.                  GA1MPGM 
00248      05  MAP-SELECT                       PIC X(07).              GA1MPGM 
00249                                                                   GA1MPGM 
00250      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA1MPGM 
00251      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA1MPGM 
00252      05  MAP-SELECT-FROM                  PIC X(03).              GA1MPGM 
00253                                                                   GA1MPGM 
00254      05  MAP-SELECT-TXT2-LEN              PIC S9(4) COMP SYNC.    GA1MPGM 
00255      05  MAP-SELECT-TXT2-ATTR             PIC X.                  GA1MPGM 
00256      05  MAP-SELECT-TXT2                  PIC X(02).              GA1MPGM 
00257                                                                   GA1MPGM 
00258      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA1MPGM 
00259      05  MAP-SELECT-TO-ATTR               PIC X.                  GA1MPGM 
00260      05  MAP-SELECT-TO                    PIC X(03).              GA1MPGM 
00261                                                                   GA1MPGM 
00262      05  MAP-SELECT-TXT3-LEN              PIC S9(4) COMP SYNC.    GA1MPGM 
00263      05  MAP-SELECT-TXT3-ATTR             PIC X.                  GA1MPGM 
00264      05  MAP-SELECT-TXT3                  PIC X(02).              GA1MPGM 
00265                                                                   GA1MPGM 
00266      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA1MPGM 
00267      05  MAP-SELECT-OF-ATTR               PIC X.                  GA1MPGM 
00268      05  MAP-SELECT-OF                    PIC X(03).              GA1MPGM 
00269                                                                   GA1MPGM 
00270      05  MAP-SELECT-TXT4-LEN              PIC S9(4) COMP SYNC.    GA1MPGM 
00271      05  MAP-SELECT-TXT4-ATTR             PIC X.                  GA1MPGM 
00272      05  MAP-SELECT-TXT4                  PIC X(30).              GA1MPGM 
00273                                                                   GA1MPGM 
00274 **+**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1MPGM 
00275 **  CHANGE OCCURS COUNT AND LENGTH TO MATCH BMS MAP               GA1MPGM 
00276 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1MPGM 
00277                                                                   GA1MPGM 
00278      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA1MPGM 
00279          INDEXED BY MAP-IDX1.                                     GA1MPGM 
00280        10  MAP-PROCEDURE-ARGUMENT-COL  OCCURS 2 TIMES             GA1MPGM 
00281            INDEXED BY  MAP-IDX2.                                  GA1MPGM 
00282          15  MAP-ACTION-CODE-LEN          PIC S9(4) COMP SYNC.    GA1MPGM 
00283          15  MAP-ACTION-CODE-ATTR         PIC X.                  GA1MPGM 
00284          15  MAP-ACTION-CODE              PIC X.                  GA1MPGM 
00285          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA1MPGM 
00286          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA1MPGM 
00287          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA1MPGM 
00288          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA1MPGM 
00289          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA1MPGM 
00290          15  MAP-CODE-FUNCTION            PIC X(3).               GA1MPGM 
00291                                                                   GA1MPGM 
00292 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1MPGM 
00293 *------ AFTER OCCURS                                              GA1MPGM 
00294      05  MAP-SELECT-TXT5-LEN              PIC S9(4) COMP SYNC.    GA1MPGM 
00295      05  MAP-SELECT-TXT5-ATTR             PIC X.                  GA1MPGM 
00296      05  MAP-SELECT-TXT5                  PIC X(79).              GA1MPGM 
00297                                                                   GA1MPGM 
00298      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA1MPGM 
00299      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA1MPGM 
00300      05  MAP-ERROR-MESSAGE                PIC X(79).              GA1MPGM 
00301      SKIP3                                                        GA1MPGM 
00302 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00303  01  WS-MAP-OCCURS-COUNTERS.                                      GA1MPGM 
00304 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00305 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1MPGM 
00306 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00307      05  WS-MAP-ROW              PIC S9(3)  COMP-3  VALUE +15.    GA1MPGM 
00308      05  WS-MAP-COL              PIC S9(3)  COMP-3  VALUE +2.     GA1MPGM 
00309 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00310      EJECT                                                        GA1MPGM 
00311 ** ALTERNATIVE WORKFILE KEYS **                                   GA1MPGM 
00312  01  FILLER                      PIC X(32)  VALUE                 GA1MPGM 
00313      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1MPGM 
00314  01  WS-ALT-WORKFILE-KEYS.                                        GA1MPGM 
00315  COPY GCWRKKEY.                                                   GA1MPGM 
00316      EJECT                                                        GA1MPGM 
00317 ** HARDCOPY WORK AREA **                                          GA1MPGM 
00318 *01  WS-HARDCOPY-COMMAREA.                                        GA1MPGM 
00319 *COPY PRNCOBOL.                                                   GA1MPGM 
00320                                                                   GA1MPGM 
00321 ** DATE FORMATTING AREA **                                        GA1MPGM 
00322  01  WS-DATE-AREA.                                                GA1MPGM 
00323      05  WS-MDY.                                                  GA1MPGM 
00324        10  WS-M                  PIC 99.                          GA1MPGM 
00325        10  WS-D                  PIC 99.                          GA1MPGM 
00326        10  WS-Y                  PIC 99.                          GA1MPGM 
00327      05  WS-YYDDD                PIC 9(5).                        GA1MPGM 
00328      05  FILLER          REDEFINES   WS-YYDDD.                    GA1MPGM 
00329        10  WS-YY                 PIC 99.                          GA1MPGM 
00330        10  WS-DDD                PIC 999.                         GA1MPGM 
00331                                                                   GA1MPGM 
00332 ******************************************************            GA1MPGM 
00333 **    MONTH TABLE FOR DATE CONVERSION                             GA1MPGM 
00334 **    WILL BE GENERATED ONLY ONCE                                 GA1MPGM 
00335 ******************************************************            GA1MPGM 
00336  01   WS-JUL-GREG-DATE-CONV-TAB.                                  GA1MPGM 
00337      05  WS-MONTH-TABLE   OCCURS 13  INDEXED BY  WS-M-IDX         GA1MPGM 
00338          PIC 999 COMP-3.                                          GA1MPGM 
00339      EJECT                                                        GA1MPGM 
00340 ** WORKFIELDS, AND SWITCHES **                                    GA1MPGM 
00341  01  WS-WORK-FIELDS.                                              GA1MPGM 
00342      05  WS-HEX-00                     PIC X.                     GA1MPGM 
00343      05  WS-QUOTIENT                   PIC 999  COMP-3.           GA1MPGM 
00344      05  WS-REMAINDER                  PIC 999  COMP-3.           GA1MPGM 
00345      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1MPGM 
00346 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00347 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00348 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00349      05  WS-SAVED-FIELDS.                                         GA1MPGM 
00350        10  WS-SAVED-PROCED-ARGUMENT    PIC X(7).                  GA1MPGM 
00351        10  WS-SAVED-CODE-FUNCTION      PIC X(3).                  GA1MPGM 
00352 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00353  01  WS-SWITCHES.                                                 GA1MPGM 
00354      05  WS-ERROR-SW                   PIC X.                     GA1MPGM 
00355                                                                   GA1MPGM 
00356 ** TITLE LINES **                                                 GA1MPGM 
00357  01  WS-TITLE-LINES.                                              GA1MPGM 
00358      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(40)  VALUE    GA1MPGM 
00359          ' GROUP SPECIFIC CONDITIONAL PROCEDURES  '.              GA1MPGM 
00360      05  CONTRACT-TITLE-LINE                  PIC X(40)  VALUE    GA1MPGM 
00361          '    CONTRACT CONDITIONAL PROCEDURES     '.              GA1MPGM 
00362      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(40)  VALUE    GA1MPGM 
00363          'BENEFIT PROVISION CONDITIONAL PROCEDURES'.              GA1MPGM 
00364                                                                   GA1MPGM 
00365      EJECT                                                        GA1MPGM 
00366 ** ATTRIBUTES **                                                  GA1MPGM 
00367  COPY DFHBMSCA.                                                   GA1MPGM 
00368      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1MPGM 
00369      EJECT                                                        GA1MPGM 
00370 ** ATTENTION IDENTIFIERS **                                       GA1MPGM 
00371  COPY DFHAID.                                                     GA1MPGM 
00372      EJECT                                                        GA1MPGM 
00373 ** RECORD LENGTHS **                                              GA1MPGM 
00374  01  WS-RECORD-LENGTHS.                                           GA1MPGM 
00375     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA1MPGM 
00376     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA1MPGM 
00377     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1MPGM 
00378     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1MPGM 
00379     COPY GCCDRLEN.                                                GA1MPGM 
00380 **  05 WS-GCIO-PARM-LENGTH            PIC S9(5) COMP-3 VALUE +228.GA1MPGM 
00381 **  05 WS-WORK-RECORD-KEY-LENGTH      PIC S9(5) COMP-3 VALUE +64. GA1MPGM 
00382 **  05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA1MPGM 
00383 **  05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1MPGM 
00384 **  05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA1MPGM 
00385 **  05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA1MPGM 
00386 **  05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1MPGM 
00387 **  05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA1MPGM 
00388 **  05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA1MPGM 
00389 **  05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1MPGM 
00390 **  05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA1MPGM 
00391 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00392 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00393 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00394 **  05 WS-TABULAR-FIXED-PORTION       PIC S9(5) COMP-3 VALUE +40. GA1MPGM 
00395 **  05 WS-TABULAR-VARIABLE-PORTION    PIC S9(5) COMP-3 VALUE +9.  GA1MPGM 
00396 **  05 WS-TABULAR-MAX-OCCURS          PIC S9(5) COMP-3 VALUE +440.GA1MPGM 
00397 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00398 /                                                                 GA1MPGM 
00399  01  WS-ONE-LOW                       PIC X(01) VALUE LOW-VALUES. GA1MPGM 
00400                                                                   GA1MPGM 
00401  01  COMMAREA-POINTER-AREA.                                       GA1MPGM 
00402      05  COMMAREA-PNTR-COMP  PIC S9(8)  COMP.                     GA1MPGM 
00403      05  COMMAREA-PNTR       REDEFINES                            GA1MPGM 
00404          COMMAREA-PNTR-COMP  USAGE IS POINTER.                    GA1MPGM 
00405 /                                                                 GA1MPGM 
00406                                                                   GA1MPGM 
00407  01  WS-END                      PIC X(16)  VALUE                 GA1MPGM 
00408      '*** W/S ENDS ***'.                                          GA1MPGM 
00409      EJECT                                                        GA1MPGM 
00410  LINKAGE SECTION.                                                 GA1MPGM 
00411  01 DFHCOMMAREA.                                                  GA1MPGM 
00412     COPY G2ALCKEC.                                                GA1MPGM 
00413                                                                   GA1MPGM 
00414 *01  DFHCOMMAREA.                                                 GA1MPGM 
00415 *    05  INCOMING-COMMAREA-PNTR-COMP  PIC S9(8)  COMP.            GA1MPGM 
00416 *    05  INCOMING-COMMAREA-PNTR       REDEFINES                   GA1MPGM 
00417 *        INCOMING-COMMAREA-PNTR-COMP  USAGE IS POINTER.           GA1MPGM 
00418 *                                                                 GA1MPGM 
00419 *01  BLL-CELLS.                                                   GA1MPGM 
00420 *    02  FILLER                  PIC S9(8)  COMP.                 GA1MPGM 
00421 *    02  COMMAREA-PNTR           PIC S9(8)  COMP.                 GA1MPGM 
00422 *    02  ALL-LEVEL-TAB-PNTR      PIC S9(8)  COMP.                 GA1MPGM 
00423 *    02  ALL-LEVEL-TAB-PNTR2     PIC S9(8)  COMP.                 GA1MPGM 
00424 *    02  COPY-AREA-PNTR          PIC S9(8)  COMP.                 GA1MPGM 
00425 *    02  GRP-SPEC-PNTR           PIC S9(8)  COMP.                 GA1MPGM 
00426 *    02  CONTRACT-PNTR           PIC S9(8)  COMP.                 GA1MPGM 
00427 *    02  CONTRACT-PNTR2          PIC S9(8)  COMP.                 GA1MPGM 
00428 *    02  BEN-PROV-PNTR           PIC S9(8)  COMP.                 GA1MPGM 
00429 *                                                                 GA1MPGM 
00430 *01  GCA-COMMAREA.                                                GA1MPGM 
00431 *COPY G2ALCKEC.                                                   GA1MPGM 
00432      EJECT                                                        GA1MPGM 
00433  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA1MPGM 
00434  COPY GCIOPRM1.                                                   GA1MPGM 
00435      EJECT                                                        GA1MPGM 
00436  COPY GCWRKDCC.                                                   GA1MPGM 
00437      SKIP3                                                        GA1MPGM 
00438  COPY GCTACOSC.                                                   GA1MPGM 
00439      EJECT                                                        GA1MPGM 
00440 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00441 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00442 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00443  01  COPY-OF-TABLE-AREA.                                          GA1MPGM 
00444      05  COPY-OF-TABLE   OCCURS 396 TIMES   INDEXED BY  COPY-IDX. GA1MPGM 
00445        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA1MPGM 
00446        10  COPY-CODE-FUNCTION          PIC X(3).                  GA1MPGM 
00447 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00448      EJECT                                                        GA1MPGM 
00449  01  IO-PARM-GRP-SPEC-RECORD.                                     GA1MPGM 
00450  COPY GCIOPRM2.                                                   GA1MPGM 
00451      EJECT                                                        GA1MPGM 
00452  COPY GCWRKDC2.                                                   GA1MPGM 
00453      EJECT                                                        GA1MPGM 
00454  COPY GCGROUPC.                                                   GA1MPGM 
00455      EJECT                                                        GA1MPGM 
00456                                                                   GA1MPGM 
00457  01  IO-PARM-CONTRACT-RECORD.                                     GA1MPGM 
00458  COPY GCIOPRM3.                                                   GA1MPGM 
00459      EJECT                                                        GA1MPGM 
00460  COPY GCWRKDC3.                                                   GA1MPGM 
00461      EJECT                                                        GA1MPGM 
00462  COPY GCCONTRC.                                                   GA1MPGM 
00463      EJECT                                                        GA1MPGM 
00464                                                                   GA1MPGM 
00465  01  IO-PARM-BEN-PROV-RECORD.                                     GA1MPGM 
00466  COPY GCIOPRM4.                                                   GA1MPGM 
00467      EJECT                                                        GA1MPGM 
00468  COPY GCWRKDC4.                                                   GA1MPGM 
00469      EJECT                                                        GA1MPGM 
00470  COPY GCBENPVC.                                                   GA1MPGM 
00471      EJECT                                                        GA1MPGM 
00472  PROCEDURE DIVISION.                                              GA1MPGM 
00473                                                                   GA1MPGM 
00474 ******************************************************************GA1MPGM 
00475 **                H O U S E K E E P I N G                         GA1MPGM 
00476 **                                                                GA1MPGM 
00477 ** DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM. GA1MPGM 
00478 **                                                                GA1MPGM 
00479 ******************************************************************GA1MPGM 
00480  0000-HOUSEKEEPING SECTION.                                       GA1MPGM 
00481                                                                   GA1MPGM 
00482      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1MPGM 
00483                                                                   GA1MPGM 
00484      IF EIBAID  =  DFHCLEAR                                       GA1MPGM 
00485          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1MPGM 
00486                         ERASE                                     GA1MPGM 
00487          END-EXEC                                                 GA1MPGM 
00488          EXEC CICS RETURN                                         GA1MPGM 
00489          END-EXEC.                                                GA1MPGM 
00490                                                                   GA1MPGM 
00491      EXEC CICS HANDLE CONDITION                                   GA1MPGM 
00492         MAPFAIL(6000-XCTL-TO-MAIN-MENU)                           GA1MPGM 
00493         END-EXEC.                                                 GA1MPGM 
00494      EJECT                                                        GA1MPGM 
00495 ******************************************************************GA1MPGM 
00496 **                     M A I N L I N E                            GA1MPGM 
00497 **                                                                GA1MPGM 
00498 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1MPGM 
00499 **  TAKEN BY THE OPERATOR.                                        GA1MPGM 
00500 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1MPGM 
00501 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO DETERMINE  GA1MPGM 
00502 **     WHICH ENTRIES, IF ANY, THEY MIGHT WANT TO DELETE.          GA1MPGM 
00503 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1MPGM 
00504 **     KEY PF12 OR PF24.                                          GA1MPGM 
00505 **  3. RECEIVE THE SCREEN.                                        GA1MPGM 
00506 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1MPGM 
00507 **     MENU.                                                      GA1MPGM 
00508 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1MPGM 
00509 **     LOGIC.                                                     GA1MPGM 
00510 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1MPGM 
00511 **     (RETURN) TO THE ADD PROGRAM (GA2MPGM).                     GA1MPGM 
00512 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1MPGM 
00513 **     (RETURN) TO THE PREVIOUS MENU.                             GA1MPGM 
00514 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1MPGM 
00515 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1MPGM 
00516 **                                                                GA1MPGM 
00517 ******************************************************************GA1MPGM 
00518  1000-MAIN-LINE SECTION.                                          GA1MPGM 
00519                                                                   GA1MPGM 
00520      MOVE '1000'  TO  WS-PARA-ID.                                 GA1MPGM 
00521      IF EIBTRNID  NOT =  'GA1M'                                   GA1MPGM 
00522         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1MPGM 
00523         GO TO 1099-RETURN.                                        GA1MPGM 
00524                                                                   GA1MPGM 
00525 ******************************************************************GA1MPGM 
00526 **   IF EIBAID  =  DFHPF12 OR  =  DFHPF24                       **GA1MPGM 
00527 **      PERFORM 7000-PRINT-HARDCOPY                             **GA1MPGM 
00528 **      GO TO 1099-RETURN.                                      **GA1MPGM 
00529 ******************************************************************GA1MPGM 
00530                                                                   GA1MPGM 
00531      EXEC CICS RECEIVE   MAP('GA1MI01') MAPSET('GA1MSET')         GA1MPGM 
00532         INTO(GA1MI01I) END-EXEC.                                  GA1MPGM 
00533                                                                   GA1MPGM 
00534      IF MAP-SCREEN-ID NOT = '001M00'                              GA1MPGM 
00535         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1MPGM 
00536                                                                   GA1MPGM 
00537      IF EIBAID  =  DFHENTER                                       GA1MPGM 
00538            OR DFHPF7 OR DFHPF8 OR DFHPF10 OR DFHPF11              GA1MPGM 
00539            OR DFHPF19 OR DFHPF20 OR DFHPF22 OR DFHPF23            GA1MPGM 
00540         PERFORM 2000-DELETE-PROCESSING                            GA1MPGM 
00541         GO TO 1099-RETURN.                                        GA1MPGM 
00542                                                                   GA1MPGM 
00543      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1MPGM 
00544         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1MPGM 
00545                                                                   GA1MPGM 
00546      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1MPGM 
00547         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1MPGM 
00548                                                                   GA1MPGM 
00549      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1MPGM 
00550      MOVE -1  TO  MAP-SELECT-LEN.                                 GA1MPGM 
00551      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1MPGM 
00552 -    'THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                   GA1MPGM 
00553      EXEC CICS SEND   MAP('GA1MI01') MAPSET('GA1MSET') DATAONLY   GA1MPGM 
00554         FROM(GA1MI01O) CURSOR END-EXEC.                           GA1MPGM 
00555                                                                   GA1MPGM 
00556  1099-RETURN.                                                     GA1MPGM 
00557 *    EXEC CICS RETURN   END-EXEC.                                 GA1MPGM 
00558      EXEC CICS RETURN TRANSID('GA1M')                             GA1MPGM 
00559                COMMAREA(DFHCOMMAREA)                              GA1MPGM 
00560      END-EXEC.                                                    GA1MPGM 
00561                                                                   GA1MPGM 
00562      GOBACK.                                                      GA1MPGM 
00563      EJECT                                                        GA1MPGM 
00564 ******************************************************************GA1MPGM 
00565 **              D E L E T E   P R O C E S S I N G                 GA1MPGM 
00566 **                                                                GA1MPGM 
00567 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1MPGM 
00568 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1MPGM 
00569 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1MPGM 
00570 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1MPGM 
00571 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1MPGM 
00572 **    BACK INTO THE RECORD THAT WE READ.)                         GA1MPGM 
00573 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1MPGM 
00574 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1MPGM 
00575 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1MPGM 
00576 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1MPGM 
00577 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1MPGM 
00578 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1MPGM 
00579 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1MPGM 
00580 **    ENTRY.                                                      GA1MPGM 
00581 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1MPGM 
00582 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1MPGM 
00583 **    RECORD.                                                     GA1MPGM 
00584 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1MPGM 
00585 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1MPGM 
00586 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1MPGM 
00587 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1MPGM 
00588 **    ARE BYPASSED; WE READ THE ALL LEVEL TABULAR RECORD:         GA1MPGM 
00589 **     A. IF ENTER WAS KEYED - SAVE THE NEXT ENTRY TO BE DISPLAYEDGA1MPGM 
00590 **        PERFORM THE ROUTINE TO BUILD THE DISPLAY, AND SEND THE  GA1MPGM 
00591 **        SCREEN TO THE OPERATOR.                                 GA1MPGM 
00592 **     B. IF PF7/PF19  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1MPGM 
00593 **        PREVIOUS PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO   GA1MPGM 
00594 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1MPGM 
00595 **     C. IF PF8/PF20  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1MPGM 
00596 **        NEXT PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1MPGM 
00597 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1MPGM 
00598 **     D. IF PF10/PF22  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1MPGM 
00599 **        LAST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1MPGM 
00600 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1MPGM 
00601 **     E. IF PF11/PF23  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1MPGM 
00602 **        FIRST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILDGA1MPGM 
00603 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1MPGM 
00604 **                                                                GA1MPGM 
00605 **                                                                GA1MPGM 
00606 ******************************************************************GA1MPGM 
00607  2000-DELETE-PROCESSING SECTION.                                  GA1MPGM 
00608                                                                   GA1MPGM 
00609      MOVE '2000'  TO  WS-PARA-ID.                                 GA1MPGM 
00610      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1MPGM 
00611      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1MPGM 
00612      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1MPGM 
00613                                                                   GA1MPGM 
00614      MOVE '2010'  TO  WS-PARA-ID.                                 GA1MPGM 
00615  2010-VALIDATE-ACT-CODE.                                          GA1MPGM 
00616      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D'               GA1MPGM 
00617         ADD 1  TO  WS-DELETE-COUNT.                               GA1MPGM 
00618      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D' OR            GA1MPGM 
00619         =  SPACE OR  =  LOW-VALUES                                GA1MPGM 
00620         MOVE DFHBMUNF  TO                                         GA1MPGM 
00621            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1MPGM 
00622         MOVE DFHBMASF  TO                                         GA1MPGM 
00623 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00624 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00625 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00626            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1MPGM 
00627            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1MPGM 
00628      ELSE                                                         GA1MPGM 
00629         MOVE DFHBMUBF  TO                                         GA1MPGM 
00630            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1MPGM 
00631         MOVE DFHBMABF  TO                                         GA1MPGM 
00632            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1MPGM 
00633            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1MPGM 
00634 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00635         IF WS-ERROR-SW  NOT =  'Y'                                GA1MPGM 
00636            MOVE -1  TO                                            GA1MPGM 
00637                MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)           GA1MPGM 
00638            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1MPGM 
00639                                                                   GA1MPGM 
00640      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1MPGM 
00641         SET MAP-IDX1  UP BY  1                                    GA1MPGM 
00642      ELSE                                                         GA1MPGM 
00643         IF MAP-IDX2  <  WS-MAP-COL                                GA1MPGM 
00644            SET MAP-IDX1  TO  1                                    GA1MPGM 
00645            SET MAP-IDX2  UP BY  1                                 GA1MPGM 
00646         ELSE                                                      GA1MPGM 
00647            GO TO 2020-DONE-VALIDATE-A-C.                          GA1MPGM 
00648 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00649 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00650 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00651      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)               GA1MPGM 
00652             NOT =  LOW-VALUES  AND                                GA1MPGM 
00653         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2)                    GA1MPGM 
00654             NOT =  LOW-VALUES                                     GA1MPGM 
00655 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00656         GO TO 2010-VALIDATE-ACT-CODE.                             GA1MPGM 
00657                                                                   GA1MPGM 
00658  2020-DONE-VALIDATE-A-C.                                          GA1MPGM 
00659      MOVE '2020'  TO  WS-PARA-ID.                                 GA1MPGM 
00660      SET MAP-IDX1  TO  1.                                         GA1MPGM 
00661      IF WS-ERROR-SW  =  'Y'                                       GA1MPGM 
00662         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1MPGM 
00663            MAP-ERROR-MESSAGE                                      GA1MPGM 
00664         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE,                   GA1MPGM 
00665            MAP-MAIN-TITLE,                                        GA1MPGM 
00666            MAP-SCREEN-ID,                                         GA1MPGM 
00667            MAP-TABULAR-ID,                                        GA1MPGM 
00668            MAP-TABULAR-SLOT,                                      GA1MPGM 
00669            MAP-ID-LINE,                                           GA1MPGM 
00670            MAP-FROM-MENU-ID                                       GA1MPGM 
00671 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00672 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1MPGM 
00673 **  ADD ITS MAP FIELD NAME HERE.                                  GA1MPGM 
00674 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00675         MOVE '2100'  TO  WS-PARA-ID                               GA1MPGM 
00676         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1MPGM 
00677            VARYING MAP-IDX2 FROM  1  BY  1                        GA1MPGM 
00678                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1MPGM 
00679              AFTER MAP-IDX1 FROM  1  BY  1                        GA1MPGM 
00680                             UNTIL MAP-IDX1  > WS-MAP-ROW          GA1MPGM 
00681         EXEC CICS SEND   MAP('GA1MI01') MAPSET('GA1MSET') DATAONLYGA1MPGM 
00682            FROM(GA1MI01O) CURSOR END-EXEC                         GA1MPGM 
00683         GO TO 2099-EXIT.                                          GA1MPGM 
00684                                                                   GA1MPGM 
00685      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1MPGM 
00686              GC-GCIOPARM-LEN +                                    GA1MPGM 
00687              GC-WORKFILE-KEY-LEN +                                GA1MPGM 
00688              GC-GCTABULR-ACDR-FIXED-LEN +                         GA1MPGM 
00689           (GC-GCTABULR-ACOS-VARY-MAX-OCUR *                       GA1MPGM 
00690                GC-GCTABULR-ACOS-VARY-LEN).                        GA1MPGM 
00691                                                                   GA1MPGM 
00692 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA1MPGM 
00693      EXEC CICS GETMAIN                                            GA1MPGM 
00694         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA1MPGM 
00695         INITIMG(WS-HEX-00)                                        GA1MPGM 
00696         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1MPGM 
00697 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1MPGM 
00698 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1MPGM 
00699                                                                   GA1MPGM 
00700      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA1MPGM 
00701         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1MPGM 
00702         MOVE 'G'   TO  GCIO-WRK-STATUS-CODE                       GA1MPGM 
00703         MOVE 'G3'  TO  GCIO-WRK-RECORD-TYPE                       GA1MPGM 
00704         MOVE GCA-PLAN-CODE      TO  GCIO-WRK-PLAN-CODE            GA1MPGM 
00705         MOVE GCA-GROUP-NO-1-3   TO  GCIO-WRK-GROUP-NO-1-3         GA1MPGM 
00706         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA1MPGM 
00707         MOVE GCA-SEC-NO-1       TO  GCIO-WRK-SEC-NO-1             GA1MPGM 
00708         MOVE GRP-SPEC-SECTION-NO TO  GCIO-WRK-SECTION-NO          GA1MPGM 
00709         MOVE GCA-PKG-CODE       TO  GCIO-WRK-PKG-CODE             GA1MPGM 
00710         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1MPGM 
00711                          GCIO-WRK-PROVIDER-CONTROL                GA1MPGM 
00712         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1MPGM 
00713         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1MPGM 
00714         MOVE MAP-TABULAR-ID TO GCIO-WRK-PROVISION-ID              GA1MPGM 
00715         MOVE MAP-TABULAR-SLOT TO GCIO-WRK-PROVISION-SLOT-NO       GA1MPGM 
00716         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA1MPGM 
00717         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1MPGM 
00718                                                                   GA1MPGM 
00719      IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA1MPGM 
00720         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1MPGM 
00721         MOVE 'C'   TO  GCIO-WRK-STATUS-CODE                       GA1MPGM 
00722         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE                       GA1MPGM 
00723         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1MPGM 
00724         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1MPGM 
00725         MOVE CONTRACT-GROUP-NO    TO  GCIO-WRK-GROUP-NO           GA1MPGM 
00726         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1MPGM 
00727         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1MPGM 
00728         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1MPGM 
00729         MOVE CONTRACT-LOB         TO  GCIO-WRK-LINE-OF-BUS        GA1MPGM 
00730         MOVE CONTRACT-PROV-CTL    TO  GCIO-WRK-PROVIDER-CONTROL   GA1MPGM 
00731         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1MPGM 
00732         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1MPGM 
00733         MOVE MAP-TABULAR-ID TO GCIO-WRK-PROVISION-ID              GA1MPGM 
00734         MOVE MAP-TABULAR-SLOT TO GCIO-WRK-PROVISION-SLOT-NO       GA1MPGM 
00735         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA1MPGM 
00736         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1MPGM 
00737                                                                   GA1MPGM 
00738      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA1MPGM 
00739         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1MPGM 
00740         MOVE 'C'   TO  GCIO-WRK-STATUS-CODE                       GA1MPGM 
00741         MOVE 'C5'  TO  GCIO-WRK-RECORD-TYPE                       GA1MPGM 
00742         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1MPGM 
00743         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1MPGM 
00744         MOVE BEN-PROV-GROUP-NO    TO  GCIO-WRK-GROUP-NO           GA1MPGM 
00745         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1MPGM 
00746         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1MPGM 
00747         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1MPGM 
00748         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1MPGM 
00749         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1MPGM 
00750         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1MPGM 
00751         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1MPGM 
00752         MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID            GA1MPGM 
00753         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1MPGM 
00754         MOVE MAP-TABULAR-ID TO GCIO-WRK-TAB-PROVISION-ID          GA1MPGM 
00755         MOVE MAP-TABULAR-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.       GA1MPGM 
00756                                                                   GA1MPGM 
00757 *    MOVE  WS-Y  TO  WS-YY.                                       GA1MPGM 
00758 *    IF WS-M  >  2                                                GA1MPGM 
00759 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1MPGM 
00760 *          REMAINDER  WS-REMAINDER                                GA1MPGM 
00761 *    ELSE                                                         GA1MPGM 
00762 *       MOVE 1  TO  WS-REMAINDER.                                 GA1MPGM 
00763 *    SET WS-M-IDX  TO  WS-M.                                      GA1MPGM 
00764 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1MPGM 
00765 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1MPGM 
00766 *    IF WS-REMAINDER  =  ZERO                                     GA1MPGM 
00767 *       ADD 1  TO  WS-DDD.                                        GA1MPGM 
00768                                                                   GA1MPGM 
00769 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1MPGM 
00770      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA1MPGM 
00771      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1MPGM 
00772      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1MPGM 
00773                                                                   GA1MPGM 
00774      IF WS-DELETE-COUNT  =  ZERO                                  GA1MPGM 
00775         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1MPGM 
00776                                                                   GA1MPGM 
00777      IF EIBAID = DFHPF7  OR DFHPF19 OR                            GA1MPGM 
00778                  DFHPF8  OR DFHPF20 OR                            GA1MPGM 
00779                  DFHPF10 OR DFHPF22 OR                            GA1MPGM 
00780                  DFHPF11 OR DFHPF23                               GA1MPGM 
00781      THEN                                                         GA1MPGM 
00782          MOVE '*** ACTION CODE ENTRY INVALID WHEN PAGING ***'     GA1MPGM 
00783            TO MAP-ERROR-MESSAGE                                   GA1MPGM 
00784          MOVE -1 TO MAP-SELECT-LEN                                GA1MPGM 
00785          MOVE LOW-VALUES TO MAP-FUNCTION-CODE                     GA1MPGM 
00786                             MAP-SCREEN-ID                         GA1MPGM 
00787 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00788 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1MPGM 
00789 **  ADD ITS MAP FIELD NAME HERE.                                  GA1MPGM 
00790 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00791          MOVE '2100' TO WS-PARA-ID                                GA1MPGM 
00792          PERFORM 2100-DONT-RETRANSMIT-FIELDS                      GA1MPGM 
00793             VARYING MAP-IDX2 FROM 1 BY 1                          GA1MPGM 
00794                              UNTIL MAP-IDX2 > WS-MAP-COL          GA1MPGM 
00795               AFTER MAP-IDX1 FROM 1 BY 1                          GA1MPGM 
00796                              UNTIL MAP-IDX1 > WS-MAP-ROW          GA1MPGM 
00797          EXEC CICS SEND MAP('GA1MI01')                            GA1MPGM 
00798                         MAPSET('GA1MSET')                         GA1MPGM 
00799                         DATAONLY                                  GA1MPGM 
00800                         FROM(GA1MI01O)                            GA1MPGM 
00801                         CURSOR                                    GA1MPGM 
00802                         END-EXEC                                  GA1MPGM 
00803          GO TO 2099-EXIT                                          GA1MPGM 
00804      ELSE                                                         GA1MPGM 
00805          NEXT SENTENCE.                                           GA1MPGM 
00806                                                                   GA1MPGM 
00807 ******************************************************************GA1MPGM 
00808 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1MPGM 
00809 *                                                                 GA1MPGM 
00810 ******************************************************************GA1MPGM 
00811      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1MPGM 
00812      TO   GAJ-ENTRY-COUNT.                                        GA1MPGM 
00813                                                                   GA1MPGM 
00814      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1MPGM 
00815                                                                   GA1MPGM 
00816      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1MPGM 
00817         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1MPGM 
00818         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1MPGM 
00819                                                                   GA1MPGM 
00820      IF  NOT GCIO-GOOD-RETURN                                     GA1MPGM 
00821         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1MPGM 
00822 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1MPGM 
00823         MOVE '1MF1'  TO  WS-ABEND-CODE                            GA1MPGM 
00824         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1MPGM 
00825                                                                   GA1MPGM 
00826      COMPUTE  WS-COPY-LENGTH  =                                   GA1MPGM 
00827            GAJ-ENTRY-COUNT  *  GC-GCTABULR-ACOS-VARY-LEN.         GA1MPGM 
00828                                                                   GA1MPGM 
00829 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA1MPGM 
00830      EXEC CICS GETMAIN                                            GA1MPGM 
00831         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA1MPGM 
00832         LENGTH(WS-COPY-LENGTH)                                    GA1MPGM 
00833         INITIMG(WS-HEX-00) END-EXEC.                              GA1MPGM 
00834 ***  SERVICE RELOAD COPY-OF-TABLE-AREA.                           GA1MPGM 
00835                                                                   GA1MPGM 
00836      MOVE GAJ-ENTRY-COUNT  TO  GAJ-ENTRY-COUNT.                   GA1MPGM 
00837      SET COPY-IDX, GAJ-INDEX  TO  1.                              GA1MPGM 
00838                                                                   GA1MPGM 
00839      MOVE '2030'  TO  WS-PARA-ID.                                 GA1MPGM 
00840  2030-MAKE-A-COPY-OF-RECORD.                                      GA1MPGM 
00841      IF GAJ-INDEX  NOT >  GAJ-ENTRY-COUNT                         GA1MPGM 
00842         MOVE GAJ-ENTRY (GAJ-INDEX)  TO  COPY-OF-TABLE (COPY-IDX)  GA1MPGM 
00843         SET COPY-IDX, GAJ-INDEX  UP BY 1                          GA1MPGM 
00844         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1MPGM 
00845                                                                   GA1MPGM 
00846      SET MAP-IDX1, MAP-IDX2, COPY-IDX, GAJ-INDEX  TO  1.          GA1MPGM 
00847      MOVE '2040'  TO  WS-PARA-ID.                                 GA1MPGM 
00848  2040-DELETE-MARKED-ENTRIES.                                      GA1MPGM 
00849 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00850 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00851 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00852      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1MPGM 
00853             AND                                                   GA1MPGM 
00854         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1MPGM 
00855         GO TO 2060-SAVE-REST-OF-COPY.                             GA1MPGM 
00856                                                                   GA1MPGM 
00857      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) >             GA1MPGM 
00858         COPY-PROCEDURE-ARGUMENT (COPY-IDX)                        GA1MPGM 
00859         GO TO 2050-SAVE-COPIED-ENTRY                              GA1MPGM 
00860      ELSE                                                         GA1MPGM 
00861         IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) <          GA1MPGM 
00862            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA1MPGM 
00863            MOVE '1ML1'  TO  WS-ABEND-CODE                         GA1MPGM 
00864            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1MPGM 
00865 -    'RM SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                 GA1MPGM 
00866            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1MPGM 
00867                                                                   GA1MPGM 
00868 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00869                                                                   GA1MPGM 
00870      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1MPGM 
00871         IF MAP-IDX1  <  WS-MAP-ROW                                GA1MPGM 
00872            SET MAP-IDX1  UP BY  1                                 GA1MPGM 
00873            GO TO 2050-SAVE-COPIED-ENTRY                           GA1MPGM 
00874         ELSE                                                      GA1MPGM 
00875            IF MAP-IDX2  <  WS-MAP-COL                             GA1MPGM 
00876               SET MAP-IDX1  TO  1                                 GA1MPGM 
00877               SET MAP-IDX2  UP BY  1                              GA1MPGM 
00878               GO TO 2050-SAVE-COPIED-ENTRY                        GA1MPGM 
00879            ELSE                                                   GA1MPGM 
00880               GO TO 2060-SAVE-REST-OF-COPY.                       GA1MPGM 
00881                                                                   GA1MPGM 
00882      SET COPY-IDX  UP BY  1.                                      GA1MPGM 
00883      IF COPY-IDX  NOT <  GAJ-ENTRY-COUNT                          GA1MPGM 
00884         MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAJ-ENTRY (GAJ-INDEX)  GA1MPGM 
00885         SET  GAJ-ENTRY-COUNT  TO  GAJ-INDEX                       GA1MPGM 
00886         MOVE GAJ-ENTRY-COUNT  TO  GAJ-ENTRY-COUNT                 GA1MPGM 
00887         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1MPGM 
00888                                                                   GA1MPGM 
00889      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1MPGM 
00890         SET MAP-IDX1  UP BY  1                                    GA1MPGM 
00891         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1MPGM 
00892                                                                   GA1MPGM 
00893      IF MAP-IDX2  <  WS-MAP-COL                                   GA1MPGM 
00894         SET MAP-IDX1  TO  1                                       GA1MPGM 
00895         SET MAP-IDX2  UP BY  1                                    GA1MPGM 
00896         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1MPGM 
00897      ELSE                                                         GA1MPGM 
00898         GO TO 2060-SAVE-REST-OF-COPY.                             GA1MPGM 
00899                                                                   GA1MPGM 
00900  2050-SAVE-COPIED-ENTRY.                                          GA1MPGM 
00901      MOVE '2050'  TO  WS-PARA-ID.                                 GA1MPGM 
00902      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAJ-ENTRY (GAJ-INDEX).    GA1MPGM 
00903                                                                   GA1MPGM 
00904      SET GAJ-INDEX  UP BY  1.                                     GA1MPGM 
00905      IF COPY-IDX  <  GAJ-ENTRY-COUNT                              GA1MPGM 
00906         SET COPY-IDX  UP BY  1                                    GA1MPGM 
00907         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1MPGM 
00908      ELSE                                                         GA1MPGM 
00909 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1MPGM 
00910 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1MPGM 
00911 ***      THE TABLE OF ENTRIES.                                    GA1MPGM 
00912         MOVE '1ML2'  TO  WS-ABEND-CODE                            GA1MPGM 
00913         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1MPGM 
00914 -    'SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                    GA1MPGM 
00915         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1MPGM 
00916                                                                   GA1MPGM 
00917  2060-SAVE-REST-OF-COPY.                                          GA1MPGM 
00918      MOVE '2060'  TO  WS-PARA-ID.                                 GA1MPGM 
00919      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAJ-ENTRY (GAJ-INDEX).    GA1MPGM 
00920                                                                   GA1MPGM 
00921      SET GAJ-INDEX  UP BY  1.                                     GA1MPGM 
00922      IF COPY-IDX  <  GAJ-ENTRY-COUNT                              GA1MPGM 
00923         SET COPY-IDX  UP BY  1                                    GA1MPGM 
00924         GO TO 2060-SAVE-REST-OF-COPY.                             GA1MPGM 
00925                                                                   GA1MPGM 
00926      SET GAJ-INDEX  DOWN BY  1.                                   GA1MPGM 
00927      SET GAJ-ENTRY-COUNT  TO  GAJ-INDEX.                          GA1MPGM 
00928      MOVE GAJ-ENTRY-COUNT  TO  GAJ-ENTRY-COUNT.                   GA1MPGM 
00929                                                                   GA1MPGM 
00930  2070-UPDATE-MODIFIED-REC.                                        GA1MPGM 
00931      MOVE '2070'  TO  WS-PARA-ID.                                 GA1MPGM 
00932                                                                   GA1MPGM 
00933 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1MPGM 
00934                                                                   GA1MPGM 
00935      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1MPGM 
00936      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1MPGM 
00937                                                                   GA1MPGM 
00938      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1MPGM 
00939              GC-WORKFILE-KEY-LEN        +                         GA1MPGM 
00940              GC-GCTABULR-ACOS-FIXED-LEN +                         GA1MPGM 
00941             (GAJ-ENTRY-COUNT  *  GC-GCTABULR-ACOS-VARY-LEN).      GA1MPGM 
00942                                                                   GA1MPGM 
00943      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1MPGM 
00944              GC-GCIOPARM-LEN      +                               GA1MPGM 
00945              GCIO-RECORD-LENGTH.                                  GA1MPGM 
00946                                                                   GA1MPGM 
00947      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1MPGM 
00948         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1MPGM 
00949         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1MPGM 
00950                                                                   GA1MPGM 
00951      IF GCIO-GOOD-RETURN                                          GA1MPGM 
00952         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1MPGM 
00953      MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASE CGA1MPGM 
00954 -    'ONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.            GA1MPGM 
00955      MOVE '1MF2'  TO  WS-ABEND-CODE.                              GA1MPGM 
00956      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1MPGM 
00957                                                                   GA1MPGM 
00958  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1MPGM 
00959      MOVE  '2080'  TO  WS-PARA-ID.                                GA1MPGM 
00960      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1MPGM 
00961      TO   GAJ-ENTRY-COUNT.                                        GA1MPGM 
00962      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1MPGM 
00963      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1MPGM 
00964         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1MPGM 
00965         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1MPGM 
00966                                                                   GA1MPGM 
00967      IF GCIO-GOOD-RETURN                                          GA1MPGM 
00968         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1MPGM 
00969      MOVE '1MF3'  TO  WS-ABEND-CODE.                              GA1MPGM 
00970      MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE CONGA1MPGM 
00971 -    'TACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.              GA1MPGM 
00972      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1MPGM 
00973                                                                   GA1MPGM 
00974  2090-BUILD-NEXT-DISPLAY.                                         GA1MPGM 
00975      MOVE  '2090'  TO  WS-PARA-ID.                                GA1MPGM 
00976      SET MAP-IDX1  TO  WS-MAP-ROW.                                GA1MPGM 
00977      SET MAP-IDX2  TO  WS-MAP-COL.                                GA1MPGM 
00978      SET GAJ-INDEX  TO  1.                                        GA1MPGM 
00979 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00980 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
00981 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00982      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1MPGM 
00983             AND                                                   GA1MPGM 
00984         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1MPGM 
00985         MOVE GAJ-ENTRY (GAJ-INDEX)  TO  WS-SAVED-FIELDS           GA1MPGM 
00986      ELSE                                                         GA1MPGM 
00987         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) TO       GA1MPGM 
00988            WS-SAVED-PROCED-ARGUMENT                               GA1MPGM 
00989         MOVE MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) TO            GA1MPGM 
00990            WS-SAVED-CODE-FUNCTION.                                GA1MPGM 
00991 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
00992                                                                   GA1MPGM 
00993                                                                   GA1MPGM 
00994      SET GAJ-INDEX TO 1.                                          GA1MPGM 
00995                                                                   GA1MPGM 
00996      IF  MAP-SELECT-LEN > 0 AND                                   GA1MPGM 
00997          MAP-SELECT     > SPACES                                  GA1MPGM 
00998          MOVE MAP-SELECT TO WS-SAVED-PROCED-ARGUMENT.             GA1MPGM 
00999                                                                   GA1MPGM 
01000      IF  EIBAID = DFHPF7 OR DFHPF19 OR                            GA1MPGM 
01001                   DFHPF8 OR DFHPF20 OR                            GA1MPGM 
01002                   DFHPF10 OR DFHPF22 OR                           GA1MPGM 
01003                   DFHPF11 OR DFHPF23                              GA1MPGM 
01004          MOVE SPACES TO MAP-SELECT                                GA1MPGM 
01005      ELSE                                                         GA1MPGM 
01006          GO TO 2090-FILL-THE-SCREEN.                              GA1MPGM 
01007                                                                   GA1MPGM 
01008      IF  EIBAID = DFHPF7 OR DFHPF19                               GA1MPGM 
01009          GO TO 2090-PAGE-BACKWARD.                                GA1MPGM 
01010      IF  EIBAID = DFHPF8 OR DFHPF20                               GA1MPGM 
01011          GO TO 2090-PAGE-FORWARD.                                 GA1MPGM 
01012      IF  EIBAID = DFHPF10 OR DFHPF22                              GA1MPGM 
01013          GO TO 2090-PAGE-TO-BOTTOM.                               GA1MPGM 
01014      IF  EIBAID = DFHPF11 OR DFHPF23                              GA1MPGM 
01015          GO TO 2090-PAGE-TO-TOP.                                  GA1MPGM 
01016                                                                   GA1MPGM 
01017  2090-PAGE-BACKWARD.                                              GA1MPGM 
01018                                                                   GA1MPGM 
01019      MOVE MAP-PROCEDURE-ARGUMENT(1 1)  TO                         GA1MPGM 
01020                                        WS-SAVED-PROCED-ARGUMENT.  GA1MPGM 
01021      MOVE MAP-CODE-FUNCTION(1 1)  TO  WS-SAVED-CODE-FUNCTION.     GA1MPGM 
01022                                                                   GA1MPGM 
01023      SEARCH GAJ-ENTRY                                             GA1MPGM 
01024          AT END                                                   GA1MPGM 
01025              MOVE GAJ-ENTRY(1)  TO  WS-SAVED-FIELDS               GA1MPGM 
01026              GO TO 2090-FILL-THE-SCREEN                           GA1MPGM 
01027          WHEN WS-SAVED-FIELDS  =  GAJ-ENTRY(GAJ-INDEX)            GA1MPGM 
01028              SET WS-GAJ-INDEX TO GAJ-INDEX.                       GA1MPGM 
01029                                                                   GA1MPGM 
01030      COMPUTE WS-GAJ-INDEX =                                       GA1MPGM 
01031          WS-GAJ-INDEX - (WS-MAP-ROW * WS-MAP-COL) + 1.            GA1MPGM 
01032                                                                   GA1MPGM 
01033      IF  WS-GAJ-INDEX < +0                                        GA1MPGM 
01034          MOVE GAJ-ENTRY(1)  TO  WS-SAVED-FIELDS                   GA1MPGM 
01035      ELSE                                                         GA1MPGM 
01036          SET  GAJ-INDEX TO WS-GAJ-INDEX                           GA1MPGM 
01037          MOVE GAJ-ENTRY(GAJ-INDEX)  TO  WS-SAVED-FIELDS.          GA1MPGM 
01038                                                                   GA1MPGM 
01039      GO TO 2090-FILL-THE-SCREEN.                                  GA1MPGM 
01040                                                                   GA1MPGM 
01041  2090-PAGE-FORWARD.                                               GA1MPGM 
01042                                                                   GA1MPGM 
01043      IF  MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)              GA1MPGM 
01044                                                      =  LOW-VALUESGA1MPGM 
01045          MOVE GAJ-ENTRY(GAJ-INDEX)  TO  WS-SAVED-FIELDS           GA1MPGM 
01046      ELSE                                                         GA1MPGM 
01047          MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)  TO     GA1MPGM 
01048                                           WS-SAVED-PROCED-ARGUMENTGA1MPGM 
01049          MOVE MAP-CODE-FUNCTION(MAP-IDX1, MAP-IDX2)  TO           GA1MPGM 
01050                                            WS-SAVED-CODE-FUNCTION.GA1MPGM 
01051                                                                   GA1MPGM 
01052      GO TO 2090-FILL-THE-SCREEN.                                  GA1MPGM 
01053                                                                   GA1MPGM 
01054  2090-PAGE-TO-BOTTOM.                                             GA1MPGM 
01055                                                                   GA1MPGM 
01056      COMPUTE WS-GAJ-INDEX =                                       GA1MPGM 
01057          GAJ-ENTRY-COUNT - (WS-MAP-ROW * WS-MAP-COL).             GA1MPGM 
01058                                                                   GA1MPGM 
01059      IF WS-GAJ-INDEX < +0                                         GA1MPGM 
01060         MOVE GAJ-ENTRY(1)  TO  WS-SAVED-FIELDS                    GA1MPGM 
01061      ELSE                                                         GA1MPGM 
01062         SET GAJ-INDEX  TO  WS-GAJ-INDEX                           GA1MPGM 
01063         MOVE GAJ-ENTRY(GAJ-INDEX)  TO  WS-SAVED-FIELDS.           GA1MPGM 
01064                                                                   GA1MPGM 
01065      GO TO 2090-FILL-THE-SCREEN.                                  GA1MPGM 
01066                                                                   GA1MPGM 
01067  2090-PAGE-TO-TOP.                                                GA1MPGM 
01068                                                                   GA1MPGM 
01069      SET  GAJ-INDEX TO 1.                                         GA1MPGM 
01070      MOVE GAJ-ENTRY(GAJ-INDEX)  TO  WS-SAVED-FIELDS.              GA1MPGM 
01071                                                                   GA1MPGM 
01072      GO TO 2090-FILL-THE-SCREEN.                                  GA1MPGM 
01073                                                                   GA1MPGM 
01074  2090-FILL-THE-SCREEN.                                            GA1MPGM 
01075                                                                   GA1MPGM 
01076      PERFORM 4500-FILL-THE-SCREEN.                                GA1MPGM 
01077      EXEC CICS SEND MAP ('GA1MI01')                               GA1MPGM 
01078          MAPSET ('GA1MSET')                                       GA1MPGM 
01079          ERASE                                                    GA1MPGM 
01080          FROM (GA1MI01O)                                          GA1MPGM 
01081          END-EXEC.                                                GA1MPGM 
01082                                                                   GA1MPGM 
01083  2099-EXIT.   EXIT.                                               GA1MPGM 
01084      EJECT                                                        GA1MPGM 
01085  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1MPGM 
01086 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01087 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
01088 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01089      MOVE LOW-VALUES  TO                                          GA1MPGM 
01090         MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),                     GA1MPGM 
01091         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1MPGM 
01092         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1MPGM 
01093 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01094                                                                   GA1MPGM 
01095  2199-EXIT.   EXIT.                                               GA1MPGM 
01096      EJECT                                                        GA1MPGM 
01097 ******************************************************************GA1MPGM 
01098 **          X C T L   T O   A D D   S C R E E N                   GA1MPGM 
01099 **                                                                GA1MPGM 
01100 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1MPGM 
01101 ** ADDING ENTRIES.  WE READ THE ALL LEVEL TABULAR & PASS THE      GA1MPGM 
01102 ** ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL TABULAR  GA1MPGM 
01103 ** RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE PROGRAM GA1MPGM 
01104 ** ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE            GA1MPGM 
01105 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1MPGM 
01106 ******************************************************************GA1MPGM 
01107  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1MPGM 
01108      MOVE '3000'  TO  WS-PARA-ID.                                 GA1MPGM 
01109                                                                   GA1MPGM 
01110 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA1MPGM 
01111 *    EXEC CICS GETMAIN                                            GA1MPGM 
01112 *       SET(ADDRESS OF GCA-COMMAREA)                              GA1MPGM 
01113 *       INITIMG(WS-HEX-00)                                        GA1MPGM 
01114 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA1MPGM 
01115 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1MPGM 
01116                                                                   GA1MPGM 
01117 *    IF  MAP-FROM-MENU-ID = 'GS3A'                                GA1MPGM 
01118 *       MOVE MAP-ID-LINE TO GROUP-SPECIFIC-ID-LINE                GA1MPGM 
01119 *       MOVE GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                    GA1MPGM 
01120 *       MOVE GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO                GA1MPGM 
01121 *       MOVE GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1MPGM 
01122 *       MOVE GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1MPGM 
01123 *       MOVE SPACES  TO  GCA-L-O-B,                               GA1MPGM 
01124 *                        GCA-PROV-CTL,                            GA1MPGM 
01125 *                        GCA-BEN-PROV-ID.                         GA1MPGM 
01126 *                                                                 GA1MPGM 
01127 *    IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA1MPGM 
01128 *       MOVE MAP-ID-LINE TO CONTRACT-ID-LINE                      GA1MPGM 
01129 *       MOVE CONTRACT-GROUP-NO  TO  GCA-GRP-NO                    GA1MPGM 
01130 *       MOVE CONTRACT-SECTION-NO  TO  GCA-SECTN-NO                GA1MPGM 
01131 *       MOVE CONTRACT-LOB  TO  GCA-L-O-B                          GA1MPGM 
01132 *       MOVE CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                  GA1MPGM 
01133 *       MOVE CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1MPGM 
01134 *       MOVE CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1MPGM 
01135 *       MOVE SPACES  TO  GCA-BEN-PROV-ID.                         GA1MPGM 
01136 *                                                                 GA1MPGM 
01137 *    IF  MAP-FROM-MENU-ID = 'GC8A'                                GA1MPGM 
01138 *       MOVE MAP-ID-LINE TO BENEFIT-PROVISION-ID-LINE             GA1MPGM 
01139 *       MOVE BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                    GA1MPGM 
01140 *       MOVE BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO                GA1MPGM 
01141 *       MOVE BEN-PROV-LOB  TO  GCA-L-O-B                          GA1MPGM 
01142 *       MOVE BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                  GA1MPGM 
01143 *       MOVE BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1MPGM 
01144 *       MOVE BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1MPGM 
01145 *       MOVE BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                 GA1MPGM 
01146 *                                                                 GA1MPGM 
01147 *    MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID.                 GA1MPGM 
01148 *    MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT.             GA1MPGM 
01149 *    MOVE SPACES  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE,                GA1MPGM 
01150 *                     GCA-INTERNAL-TAB-ID,                        GA1MPGM 
01151 *                     GCA-INTERNAL-TAB-SLOT,                      GA1MPGM 
01152 *                     GCA-OCCURS-ENTRY-COUNTER,                   GA1MPGM 
01153 *                     GCA-ADD-DEL-IND.                            GA1MPGM 
01154 *    MOVE MAP-FROM-MENU-ID TO GCA-FROM-MENU-ID.                   GA1MPGM 
01155 *    MOVE ZEROES  TO  GCA-EFF-DT.                                 GA1MPGM 
01156 *                                                                 GA1MPGM 
01157 *    SET COMMAREA-PNTR TO ADDRESS                                 GA1MPGM 
01158 *    OF  GCA-COMMAREA.                                            GA1MPGM 
01159 *                                                                 GA1MPGM 
01160 *    EXEC CICS XCTL  PROGRAM('GA2MPGM') COMMAREA(COMMAREA-PNTR)   GA1MPGM 
01161 *       LENGTH(4) END-EXEC.                                       GA1MPGM 
01162      EXEC CICS XCTL  PROGRAM('GA2MPGM')                           GA1MPGM 
01163                      COMMAREA(DFHCOMMAREA)                        GA1MPGM 
01164                      LENGTH (LENGTH OF DFHCOMMAREA)               GA1MPGM 
01165      END-EXEC.                                                    GA1MPGM 
01166                                                                   GA1MPGM 
01167  3099-EXIT.   EXIT.                                               GA1MPGM 
01168      EJECT                                                        GA1MPGM 
01169 ***************************************************************** GA1MPGM 
01170 **          D I S P L A Y   F I R S T   S C R E E N               GA1MPGM 
01171 **                                                                GA1MPGM 
01172 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1MPGM 
01173 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1MPGM 
01174 ** ALL LEVEL TABULAR RECORD & PASS US THE RECORD (PRECEEDED BY I/OGA1MPGM 
01175 ** PARMS AND WORKFILE KEY).  WE WILL THEN USE THAT RECORD TO BUILDGA1MPGM 
01176 ** THE SCREEN IMAGE.                                              GA1MPGM 
01177 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1MPGM 
01178 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1MPGM 
01179 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1MPGM 
01180 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1MPGM 
01181 ** DISPLAYED, THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,  GA1MPGM 
01182 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1MPGM 
01183 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1MPGM 
01184 ******************************************************************GA1MPGM 
01185  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1MPGM 
01186      MOVE '4000'  TO  WS-PARA-ID.                                 GA1MPGM 
01187                                                                   GA1MPGM 
01188 ***  MOVE LOW-VALUES TO SCREEN                                    GA1MPGM 
01189                                                                   GA1MPGM 
01190 ***  MOVE LOW-VALUES TO GA1MI01I.                                 GA1MPGM 
01191                                                                   GA1MPGM 
01192      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1MPGM 
01193         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1MPGM 
01194            TO MAP-ERROR-MESSAGE                                   GA1MPGM 
01195         MOVE '1MC1'  TO  WS-ABEND-CODE                            GA1MPGM 
01196         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1MPGM 
01197                                                                   GA1MPGM 
01198 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA1MPGM 
01199 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1MPGM 
01200 ***  MOVE GCA-RECORD-POINTER  TO  ALL-LEVEL-TAB-PNTR.             GA1MPGM 
01201 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1MPGM 
01202 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1MPGM 
01203                                                                   GA1MPGM 
01204 *    SET ADDRESS OF GCA-COMMAREA                                  GA1MPGM 
01205 *    TO  INCOMING-COMMAREA-PNTR                                   GA1MPGM 
01206                                                                   GA1MPGM 
01207      SET ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD                    GA1MPGM 
01208      TO  GCA-RECORD-POINTER.                                      GA1MPGM 
01209                                                                   GA1MPGM 
01210      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-TABULAR-ID.               GA1MPGM 
01211      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-TABULAR-SLOT.           GA1MPGM 
01212      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA1MPGM 
01213 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01214 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1MPGM 
01215 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1MPGM 
01216 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01217                                                                   GA1MPGM 
01218 *    MOVE WRK-EFF-DATE  TO  WS-YYDDD.                             GA1MPGM 
01219                                                                   GA1MPGM 
01220 *    SET WS-M-IDX  TO  1.                                         GA1MPGM 
01221 *    MOVE WS-YY  TO  WS-Y.                                        GA1MPGM 
01222 *    DIVIDE  WS-YY  BY  4  GIVING  WS-QUOTIENT                    GA1MPGM 
01223 *       REMAINDER  WS-REMAINDER.                                  GA1MPGM 
01224                                                                   GA1MPGM 
01225 *    MOVE '4010'  TO  WS-PARA-ID.                                 GA1MPGM 
01226 *4010-DETERMINE-DATE.                                             GA1MPGM 
01227 *    IF  WS-REMAINDER  =  ZERO  AND  WS-M-IDX  >  2               GA1MPGM 
01228 *       COMPUTE  WS-MONTH-TABLE (WS-M-IDX)  =                     GA1MPGM 
01229 *          WS-MONTH-TABLE (WS-M-IDX)  +  1.                       GA1MPGM 
01230 *    IF  WS-MONTH-TABLE (WS-M-IDX)  =  WS-DDD  OR  >  WS-DDD      GA1MPGM 
01231 *       SET WS-M-IDX  DOWN BY  1                                  GA1MPGM 
01232 *       SET WS-M  TO  WS-M-IDX                                    GA1MPGM 
01233 *       COMPUTE  WS-D  =  WS-DDD  -  WS-MONTH-TABLE (WS-M-IDX)    GA1MPGM 
01234 *    ELSE                                                         GA1MPGM 
01235 *       IF  WS-M-IDX  <  13                                       GA1MPGM 
01236 *          SET WS-M-IDX  UP BY  1                                 GA1MPGM 
01237 *          GO TO  4010-DETERMINE-DATE                             GA1MPGM 
01238 *       ELSE                                                      GA1MPGM 
01239 *          MOVE '*** INVALID EFFECTIVE DATE DISCOVERED, WE CAN NOTGA1MPGM 
01240 *    ' PROCESS THIS REQUEST ***'  TO  MAP-ERROR-MESSAGE           GA1MPGM 
01241 *          MOVE '1MC2'  TO  WS-ABEND-CODE                         GA1MPGM 
01242 *          PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1MPGM 
01243                                                                   GA1MPGM 
01244 *    MOVE WS-MDY  TO  GCA-EFFECTIVE-DATE.                         GA1MPGM 
01245                                                                   GA1MPGM 
01246      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1MPGM 
01247         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-MAIN-TITLE        GA1MPGM 
01248         MOVE 'GROUP SPECIFIC ID = '  TO GRP-SPEC-ID-HEADING       GA1MPGM 
01249         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA1MPGM 
01250         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA1MPGM 
01251         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1MPGM 
01252         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA1MPGM 
01253         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1MPGM 
01254         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1MPGM 
01255         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1MPGM 
01256         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1MPGM 
01257                                                                   GA1MPGM 
01258      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1MPGM 
01259         MOVE CONTRACT-TITLE-LINE  TO  MAP-MAIN-TITLE              GA1MPGM 
01260         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1MPGM 
01261         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA1MPGM 
01262         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA1MPGM 
01263         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1MPGM 
01264         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA1MPGM 
01265         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1MPGM 
01266         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1MPGM 
01267         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1MPGM 
01268         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1MPGM 
01269         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1MPGM 
01270         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1MPGM 
01271         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1MPGM 
01272         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1MPGM 
01273                                                                   GA1MPGM 
01274      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1MPGM 
01275         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-MAIN-TITLE     GA1MPGM 
01276         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA1MPGM 
01277         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA1MPGM 
01278         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA1MPGM 
01279         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA1MPGM 
01280         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA1MPGM 
01281         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1MPGM 
01282         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA1MPGM 
01283         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1MPGM 
01284         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA1MPGM 
01285         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1MPGM 
01286         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA1MPGM 
01287         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1MPGM 
01288         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA1MPGM 
01289         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1MPGM 
01290                                                                   GA1MPGM 
01291      SET GAJ-INDEX  TO  1.                                        GA1MPGM 
01292      MOVE GAJ-ENTRY (GAJ-INDEX)  TO  WS-SAVED-FIELDS.             GA1MPGM 
01293                                                                   GA1MPGM 
01294      PERFORM 4500-FILL-THE-SCREEN.                                GA1MPGM 
01295      EXEC CICS SEND   MAP('GA1MI01') MAPSET('GA1MSET') ERASE      GA1MPGM 
01296         FROM(GA1MI01O) END-EXEC.                                  GA1MPGM 
01297                                                                   GA1MPGM 
01298  4099-EXIT.   EXIT.                                               GA1MPGM 
01299      EJECT                                                        GA1MPGM 
01300 ***************************************************************** GA1MPGM 
01301 **             F I L L   T H E   S C R E E N                      GA1MPGM 
01302 **                                                                GA1MPGM 
01303 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1MPGM 
01304 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1MPGM 
01305 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1MPGM 
01306 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1MPGM 
01307 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1MPGM 
01308 ******************************************************************GA1MPGM 
01309  4500-FILL-THE-SCREEN SECTION.                                    GA1MPGM 
01310                                                                   GA1MPGM 
01311      MOVE '4500'  TO  WS-PARA-ID.                                 GA1MPGM 
01312      MOVE  GAJ-ENTRY-COUNT  TO  GAJ-ENTRY-COUNT.                  GA1MPGM 
01313      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1MPGM 
01314                                                                   GA1MPGM 
01315      COMPUTE WS-SELECT-OF = GAJ-ENTRY-COUNT - 1.                  GA1MPGM 
01316      MOVE WS-SELECT-OF TO WS-SELECT-OF-MASK.                      GA1MPGM 
01317      MOVE WS-SELECT-OF-TRUNC TO MAP-SELECT-FROM                   GA1MPGM 
01318                                 MAP-SELECT-TO                     GA1MPGM 
01319                                 MAP-SELECT-OF.                    GA1MPGM 
01320                                                                   GA1MPGM 
01321      IF GAJ-ENTRY-COUNT  NOT >  1                                 GA1MPGM 
01322         MOVE '4530'  TO  WS-PARA-ID                               GA1MPGM 
01323         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1MPGM 
01324                                                                   GA1MPGM 
01325      SET GAJ-INDEX  TO  1.                                        GA1MPGM 
01326      MOVE '4510'  TO  WS-PARA-ID.                                 GA1MPGM 
01327  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1MPGM 
01328 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01329 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
01330 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01331      IF GAJ-ENTRY(GAJ-INDEX)  <  WS-SAVED-FIELDS                  GA1MPGM 
01332 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01333         SET GAJ-INDEX  UP BY  1                                   GA1MPGM 
01334         IF GAJ-INDEX  <  GAJ-ENTRY-COUNT                          GA1MPGM 
01335            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1MPGM 
01336         ELSE                                                      GA1MPGM 
01337            SET GAJ-INDEX  TO  1.                                  GA1MPGM 
01338                                                                   GA1MPGM 
01339      SET WS-SELECT-FROM TO GAJ-INDEX.                             GA1MPGM 
01340      MOVE WS-SELECT-FROM TO WS-SELECT-FROM-MASK.                  GA1MPGM 
01341      MOVE WS-SELECT-FROM-TRUNC TO MAP-SELECT-FROM.                GA1MPGM 
01342                                                                   GA1MPGM 
01343      MOVE '4520'  TO  WS-PARA-ID.                                 GA1MPGM 
01344  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1MPGM 
01345      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1MPGM 
01346      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1MPGM 
01347 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01348 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
01349 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01350      MOVE GAJ-PROCEDURE-ARGUMENT (GAJ-INDEX) TO                   GA1MPGM 
01351         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2).              GA1MPGM 
01352      MOVE GAJ-COMBINATION-CODE-FUNCTION (GAJ-INDEX) TO            GA1MPGM 
01353         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1MPGM 
01354 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01355                                                                   GA1MPGM 
01356      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1MPGM 
01357         SET  MAP-IDX1  UP BY  1                                   GA1MPGM 
01358      ELSE                                                         GA1MPGM 
01359         IF MAP-IDX2  <  WS-MAP-COL                                GA1MPGM 
01360            SET  MAP-IDX1  TO  1                                   GA1MPGM 
01361            SET  MAP-IDX2  UP BY  1                                GA1MPGM 
01362         ELSE                                                      GA1MPGM 
01363            SET WS-SELECT-TO TO GAJ-INDEX                          GA1MPGM 
01364            MOVE WS-SELECT-TO TO WS-SELECT-TO-MASK                 GA1MPGM 
01365            MOVE WS-SELECT-TO-TRUNC TO MAP-SELECT-TO               GA1MPGM 
01366            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1MPGM 
01367                                                                   GA1MPGM 
01368      IF GAJ-INDEX  <  (GAJ-ENTRY-COUNT - 1 )                      GA1MPGM 
01369         SET  GAJ-INDEX  UP BY  1                                  GA1MPGM 
01370         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1MPGM 
01371                                                                   GA1MPGM 
01372      SET WS-SELECT-TO TO GAJ-INDEX.                               GA1MPGM 
01373      MOVE WS-SELECT-TO TO WS-SELECT-TO-MASK.                      GA1MPGM 
01374      MOVE WS-SELECT-TO-TRUNC TO MAP-SELECT-TO.                    GA1MPGM 
01375                                                                   GA1MPGM 
01376      MOVE '4530'  TO  WS-PARA-ID.                                 GA1MPGM 
01377  4530-FILL-REST-WITH-NULLS.                                       GA1MPGM 
01378      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1MPGM 
01379 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01380 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1MPGM 
01381 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01382      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1MPGM 
01383         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1MPGM 
01384         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1MPGM 
01385 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1MPGM 
01386      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1MPGM 
01387         SET  MAP-IDX1  UP BY  1                                   GA1MPGM 
01388         GO TO 4530-FILL-REST-WITH-NULLS                           GA1MPGM 
01389      ELSE                                                         GA1MPGM 
01390         IF MAP-IDX2  <  WS-MAP-COL                                GA1MPGM 
01391            SET  MAP-IDX1  TO  1                                   GA1MPGM 
01392            SET  MAP-IDX2  UP BY  1                                GA1MPGM 
01393            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1MPGM 
01394                                                                   GA1MPGM 
01395      MOVE '4540'  TO  WS-PARA-ID.                                 GA1MPGM 
01396  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1MPGM 
01397      IF GAJ-ENTRY-COUNT  =  1                                     GA1MPGM 
01398         MOVE '*** NO ENTRIES TO DELETE ***'                       GA1MPGM 
01399         TO  MAP-ERROR-MESSAGE                                     GA1MPGM 
01400         GO TO 4599-EXIT.                                          GA1MPGM 
01401                                                                   GA1MPGM 
01402      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1MPGM 
01403         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'                 GA1MPGM 
01404         TO  MAP-ERROR-MESSAGE.                                    GA1MPGM 
01405                                                                   GA1MPGM 
01406  4599-EXIT.     EXIT.                                             GA1MPGM 
01407      EJECT                                                        GA1MPGM 
01408 ***************************************************************** GA1MPGM 
01409 **        X C T L   T O   P R E V I O U S   M E N U               GA1MPGM 
01410 **                                                                GA1MPGM 
01411 **  THE OPERATOR WANTS TO RETURN TO THE MENU THIS PROGRAM         GA1MPGM 
01412 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND     GA1MPGM 
01413 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA1MPGM 
01414 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM MENU  GA1MPGM 
01415 ** ID' FIELD).                                                    GA1MPGM 
01416 ******************************************************************GA1MPGM 
01417  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1MPGM 
01418      MOVE '5000'  TO  WS-PARA-ID.                                 GA1MPGM 
01419                                                                   GA1MPGM 
01420      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA1MPGM 
01421         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA1MPGM 
01422                                                                   GA1MPGM 
01423      IF  MAP-FROM-MENU-ID = 'GC4A'                                GA1MPGM 
01424         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA1MPGM 
01425                                                                   GA1MPGM 
01426      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA1MPGM 
01427         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA1MPGM 
01428                                                                   GA1MPGM 
01429      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA1MPGM 
01430         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA1MPGM 
01431                                                                   GA1MPGM 
01432                                                                   GA1MPGM 
01433  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA1MPGM 
01434      MOVE '5010'  TO  WS-PARA-ID.                                 GA1MPGM 
01435                                                                   GA1MPGM 
01436      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1MPGM 
01437          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1MPGM 
01438                 GC-GCGRPSPC-FIXED-LEN      +                      GA1MPGM 
01439         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1MPGM 
01440                                                                   GA1MPGM 
01441 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA1MPGM 
01442      EXEC CICS GETMAIN                                            GA1MPGM 
01443         SET(IO-PARM-GRP-SPEC-RECORD)                              GA1MPGM 
01444         INITIMG(WS-HEX-00)                                        GA1MPGM 
01445         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01446 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA1MPGM 
01447                                                                   GA1MPGM 
01448      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1MPGM 
01449                                                                   GA1MPGM 
01450      MOVE 'G'   TO  GCIO-WRK-STATUS-CODE.                         GA1MPGM 
01451      MOVE 'G2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1MPGM 
01452      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1MPGM 
01453      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1MPGM 
01454      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1MPGM 
01455      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1MPGM 
01456      MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                       GA1MPGM 
01457                       GCIO-WRK-PROVIDER-CONTROL.                  GA1MPGM 
01458      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1MPGM 
01459      MOVE GCA-EFFDT-CEN   TO GCIO-WRK-EFFDT-CEN.                  GA1MPGM 
01460                                                                   GA1MPGM 
01461 *    MOVE  WS-Y  TO  WS-YY.                                       GA1MPGM 
01462 *    IF WS-M  >  2                                                GA1MPGM 
01463 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1MPGM 
01464 *          REMAINDER  WS-REMAINDER                                GA1MPGM 
01465 *    ELSE                                                         GA1MPGM 
01466 *       MOVE 1  TO  WS-REMAINDER.                                 GA1MPGM 
01467 *    SET WS-M-IDX  TO  WS-M.                                      GA1MPGM 
01468 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1MPGM 
01469 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1MPGM 
01470 *    IF WS-REMAINDER  =  ZERO                                     GA1MPGM 
01471 *       ADD 1  TO  WS-DDD.                                        GA1MPGM 
01472 *                                                                 GA1MPGM 
01473 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1MPGM 
01474      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA1MPGM 
01475      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1MPGM 
01476                       GCIO-WRK-TAB-PROVISION-ID.                  GA1MPGM 
01477      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1MPGM 
01478                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1MPGM 
01479      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1MPGM 
01480                                                                   GA1MPGM 
01481      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA1MPGM 
01482      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA1MPGM 
01483                                                                   GA1MPGM 
01484      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1MPGM 
01485                                                                   GA1MPGM 
01486      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1MPGM 
01487         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA1MPGM 
01488         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01489                                                                   GA1MPGM 
01490      IF  NOT GCIO2-GOOD-RETURN                                    GA1MPGM 
01491         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1MPGM 
01492 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1MPGM 
01493         MOVE '1MF4'  TO  WS-ABEND-CODE                            GA1MPGM 
01494         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1MPGM 
01495                                                                   GA1MPGM 
01496      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1MPGM 
01497          GC-WORKFILE-KEY-LEN        +                             GA1MPGM 
01498                 GC-GCGRPSPC-FIXED-LEN      +                      GA1MPGM 
01499         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1MPGM 
01500                                                                   GA1MPGM 
01501      EXEC CICS XCTL  PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)   GA1MPGM 
01502         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01503                                                                   GA1MPGM 
01504      GO  TO  5099-EXIT.                                           GA1MPGM 
01505                                                                   GA1MPGM 
01506  5020-XCTL-TO-CONTRACT-MENU.                                      GA1MPGM 
01507      MOVE '5020'  TO  WS-PARA-ID.                                 GA1MPGM 
01508                                                                   GA1MPGM 
01509      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1MPGM 
01510          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1MPGM 
01511                 GC-GCCONTR-FIXED-LEN       +                      GA1MPGM 
01512         (GC-GCCONTR-VARY-LEN     *  GC-GCCONTR-VARY-MAX-OCUR).    GA1MPGM 
01513                                                                   GA1MPGM 
01514 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA1MPGM 
01515      EXEC CICS GETMAIN                                            GA1MPGM 
01516         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA1MPGM 
01517         INITIMG(WS-HEX-00)                                        GA1MPGM 
01518         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01519 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA1MPGM 
01520 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA1MPGM 
01521                                                                   GA1MPGM 
01522      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1MPGM 
01523                                                                   GA1MPGM 
01524      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1MPGM 
01525      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1MPGM 
01526      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1MPGM 
01527      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1MPGM 
01528      MOVE GCA-SECTION-NUM TO  GCIO-WRK-SECTION-NUM.               GA1MPGM 
01529      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1MPGM 
01530      MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA1MPGM 
01531      MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA1MPGM 
01532      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1MPGM 
01533      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA1MPGM 
01534                                                                   GA1MPGM 
01535 *    MOVE  WS-Y  TO  WS-YY.                                       GA1MPGM 
01536 *    IF WS-M  >  2                                                GA1MPGM 
01537 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1MPGM 
01538 *          REMAINDER  WS-REMAINDER                                GA1MPGM 
01539 *    ELSE                                                         GA1MPGM 
01540 *       MOVE 1  TO  WS-REMAINDER.                                 GA1MPGM 
01541 *    SET WS-M-IDX  TO  WS-M.                                      GA1MPGM 
01542 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1MPGM 
01543 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1MPGM 
01544 *    IF WS-REMAINDER  =  ZERO                                     GA1MPGM 
01545 *       ADD 1  TO  WS-DDD.                                        GA1MPGM 
01546 *                                                                 GA1MPGM 
01547 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1MPGM 
01548      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA1MPGM 
01549      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1MPGM 
01550                       GCIO-WRK-TAB-PROVISION-ID.                  GA1MPGM 
01551      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1MPGM 
01552                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1MPGM 
01553      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA1MPGM 
01554                                                                   GA1MPGM 
01555      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA1MPGM 
01556      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA1MPGM 
01557                                                                   GA1MPGM 
01558      MOVE  'RD '  TO  GCIO3-FILE-ACCESS-CODE.                     GA1MPGM 
01559                                                                   GA1MPGM 
01560      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1MPGM 
01561         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA1MPGM 
01562         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01563                                                                   GA1MPGM 
01564      IF  NOT GCIO3-GOOD-RETURN                                    GA1MPGM 
01565         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1MPGM 
01566 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1MPGM 
01567         MOVE '1MF5'  TO  WS-ABEND-CODE                            GA1MPGM 
01568         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1MPGM 
01569                                                                   GA1MPGM 
01570      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1MPGM 
01571          GC-WORKFILE-KEY-LEN        +                             GA1MPGM 
01572                 GC-GCCONTR-FIXED-LEN       +                      GA1MPGM 
01573         (GC-GCCONTR-VARY-LEN     *  GC-GCCONTR-VARY-MAX-OCUR).    GA1MPGM 
01574                                                                   GA1MPGM 
01575      EXEC CICS XCTL  PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)   GA1MPGM 
01576         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01577                                                                   GA1MPGM 
01578      GO  TO  5099-EXIT.                                           GA1MPGM 
01579                                                                   GA1MPGM 
01580  5030-XCTL-TO-BEN-PROV-MENU.                                      GA1MPGM 
01581      MOVE '5030'  TO  WS-PARA-ID.                                 GA1MPGM 
01582                                                                   GA1MPGM 
01583      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1MPGM 
01584          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1MPGM 
01585                 GC-GCBENPRV-FIXED-LEN      +                      GA1MPGM 
01586         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1MPGM 
01587                                                                   GA1MPGM 
01588 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA1MPGM 
01589      EXEC CICS GETMAIN                                            GA1MPGM 
01590         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA1MPGM 
01591         INITIMG(WS-HEX-00)                                        GA1MPGM 
01592         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01593 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA1MPGM 
01594                                                                   GA1MPGM 
01595      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1MPGM 
01596                                                                   GA1MPGM 
01597      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1MPGM 
01598      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA1MPGM 
01599      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1MPGM 
01600      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1MPGM 
01601      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1MPGM 
01602      MOVE GCA-PKG-CODE  TO  GCIO-WRK-PKG-CODE.                    GA1MPGM 
01603      MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA1MPGM 
01604      MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA1MPGM 
01605      MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1MPGM 
01606      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA1MPGM 
01607      MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID.              GA1MPGM 
01608                                                                   GA1MPGM 
01609 *    MOVE  WS-Y  TO  WS-YY.                                       GA1MPGM 
01610 *    IF WS-M  >  2                                                GA1MPGM 
01611 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1MPGM 
01612 *          REMAINDER  WS-REMAINDER                                GA1MPGM 
01613 *    ELSE                                                         GA1MPGM 
01614 *       MOVE 1  TO  WS-REMAINDER.                                 GA1MPGM 
01615 *    SET WS-M-IDX  TO  WS-M.                                      GA1MPGM 
01616 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1MPGM 
01617 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1MPGM 
01618 *    IF WS-REMAINDER  =  ZERO                                     GA1MPGM 
01619 *       ADD 1  TO  WS-DDD.                                        GA1MPGM 
01620 *                                                                 GA1MPGM 
01621 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1MPGM 
01622      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA1MPGM 
01623      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA1MPGM 
01624      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA1MPGM 
01625      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1MPGM 
01626      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA1MPGM 
01627                                                                   GA1MPGM 
01628      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA1MPGM 
01629      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA1MPGM 
01630                                                                   GA1MPGM 
01631      MOVE  'RD '  TO  GCIO4-FILE-ACCESS-CODE.                     GA1MPGM 
01632                                                                   GA1MPGM 
01633      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1MPGM 
01634         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA1MPGM 
01635         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01636                                                                   GA1MPGM 
01637      IF  NOT GCIO4-GOOD-RETURN                                    GA1MPGM 
01638         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1MPGM 
01639 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1MPGM 
01640         MOVE '1MF6'  TO  WS-ABEND-CODE                            GA1MPGM 
01641         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1MPGM 
01642                                                                   GA1MPGM 
01643      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1MPGM 
01644          GC-WORKFILE-KEY-LEN        +                             GA1MPGM 
01645                 GC-GCBENPRV-FIXED-LEN      +                      GA1MPGM 
01646         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1MPGM 
01647                                                                   GA1MPGM 
01648      EXEC CICS XCTL  PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)   GA1MPGM 
01649         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1MPGM 
01650                                                                   GA1MPGM 
01651      GO  TO  5099-EXIT.                                           GA1MPGM 
01652                                                                   GA1MPGM 
01653                                                                   GA1MPGM 
01654                                                                   GA1MPGM 
01655  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA1MPGM 
01656      MOVE '5040'  TO  WS-PARA-ID.                                 GA1MPGM 
01657                                                                   GA1MPGM 
01658      EXEC CICS XCTL                                               GA1MPGM 
01659                PROGRAM('GTM1PGM')                                 GA1MPGM 
01660                END-EXEC.                                          GA1MPGM 
01661                                                                   GA1MPGM 
01662      GO  TO  5099-EXIT.                                           GA1MPGM 
01663                                                                   GA1MPGM 
01664  5099-EXIT.                                                       GA1MPGM 
01665      EXIT.                                                        GA1MPGM 
01666      EJECT                                                        GA1MPGM 
01667 ***************************************************************** GA1MPGM 
01668 **           X C T L   T O   M A I N   M E N U                    GA1MPGM 
01669 **                                                                GA1MPGM 
01670 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1MPGM 
01671 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1MPGM 
01672 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1MPGM 
01673 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOME OF AGA1MPGM 
01674 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1MPGM 
01675 ** AND PROGRESS DOWN.                                             GA1MPGM 
01676 ******************************************************************GA1MPGM 
01677  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1MPGM 
01678      MOVE '6000'  TO  WS-PARA-ID.                                 GA1MPGM 
01679      MOVE '1MP1'  TO  WS-ABEND-CODE.                              GA1MPGM 
01680                                                                   GA1MPGM 
01681      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1MPGM 
01682                                                                   GA1MPGM 
01683  6099-EXIT.     EXIT.                                             GA1MPGM 
01684      EJECT                                                        GA1MPGM 
01685 /**************************************************************** GA1MPGM 
01686 **              P R I N T   H A R D C O P Y                       GA1MPGM 
01687 **                                                                GA1MPGM 
01688 **   THE OPERATOR HAS KEYED THE PF12 OF PF24 KEY INDICATING THEY  GA1MPGM 
01689 ** WANT A HARDCOPY IMAGE OF THE CURRENT SCREEN.  WE LINK TO THE   GA1MPGM 
01690 ** 'CSCRTCPY' PROGRAM WHICH WILL PRINT THE SCREEN, AND RETURN US AGA1MPGM 
01691 ** RETURN CODE INDICATING SUCCESS OR THE TYPE OF ERROR.           GA1MPGM 
01692 ******************************************************************GA1MPGM 
01693 *7000-PRINT-HARDCOPY SECTION.                                     GA1MPGM 
01694 **   MOVE '7000'  TO  WS-PARA-ID.                                 GA1MPGM 
01695 **                                                                GA1MPGM 
01696 **   MOVE 'CSCRTCPY'  TO  PRINT-PROGRAM-ID.                       GA1MPGM 
01697 **   MOVE 'PRN'  TO  PRINT-REQUEST-TYPE.                          GA1MPGM 
01698 **   SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1MPGM 
01699 **   MOVE -1  TO  MAP-SELECT-LEN.                                 GA1MPGM 
01700 **   EXEC CICS LINK   PROGRAM('CSCRTCPY')                         GA1MPGM 
01701 **      LENGTH(PRINT-COMMAREA-LEN)                                GA1MPGM 
01702 **      COMMAREA(CSCRTCPY-COMMAREA-DEFINITION) END-EXEC.          GA1MPGM 
01703 **                                                                GA1MPGM 
01704 **   IF PRINT-OK                                                  GA1MPGM 
01705 **      MOVE '    *** HARDCOPY REQUEST COMPLETED ***'             GA1MPGM 
01706 **      TO  MAP-ERROR-MESSAGE                                     GA1MPGM 
01707 **      GO TO 7010-SEND-SCREEN.                                   GA1MPGM 
01708 **                                                                GA1MPGM 
01709 **   IF PRINT-OUT-OF-SERVICE                                      GA1MPGM 
01710 **      MOVE '                *** OUT OF SERVICE ***'             GA1MPGM 
01711 **      TO  MAP-ERROR-MESSAGE                                     GA1MPGM 
01712 **      GO TO 7010-SEND-SCREEN.                                   GA1MPGM 
01713 **                                                                GA1MPGM 
01714 **   IF PRINT-NO-TEMP-STORAGE                                     GA1MPGM 
01715 **      MOVE '               *** NO TEMP STORAGE ***'             GA1MPGM 
01716 **      TO  MAP-ERROR-MESSAGE                                     GA1MPGM 
01717 **      GO TO 7010-SEND-SCREEN.                                   GA1MPGM 
01718 **                                                                GA1MPGM 
01719 **   IF PRINT-NO-PRINTER                                          GA1MPGM 
01720 **      MOVE '           *** NO PRINTER ATTACHED ***'             GA1MPGM 
01721 **      TO  MAP-ERROR-MESSAGE                                     GA1MPGM 
01722 **      GO TO 7010-SEND-SCREEN.                                   GA1MPGM 
01723 **                                                                GA1MPGM 
01724 **   MOVE '                  *** TS INT CTL ERR ***'              GA1MPGM 
01725 **      TO  MAP-ERROR-MESSAGE.                                    GA1MPGM 
01726 **                                                                GA1MPGM 
01727 *7010-SEND-SCREEN.                                                GA1MPGM 
01728 **   MOVE '7010'  TO  WS-PARA-ID.                                 GA1MPGM 
01729 **                                                                GA1MPGM 
01730 **   EXEC CICS SEND   MAP('GA1MI01') MAPSET('GA1MSET') DATAONLY   GA1MPGM 
01731 **      FROM(GA1MI01O) CURSOR END-EXEC.                           GA1MPGM 
01732 **                                                                GA1MPGM 
01733 *7099-EXIT.     EXIT.                                             GA1MPGM 
01734 **   SKIP3                                                        GA1MPGM 
01735 ******************************************************************GA1MPGM 
01736  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1MPGM 
01737                                                                   GA1MPGM 
01738      SET MAP-IDX1  TO  7.                                         GA1MPGM 
01739      SET MAP-IDX2  TO  1.                                         GA1MPGM 
01740      MOVE -1  TO  MAP-SELECT-LEN.                                 GA1MPGM 
01741                                                                   GA1MPGM 
01742      EXEC CICS SEND   MAP('GA1MI01') MAPSET('GA1MSET') ERASE      GA1MPGM 
01743         FROM(GA1MI01O) WAIT END-EXEC.                             GA1MPGM 
01744                                                                   GA1MPGM 
01745      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1MPGM 
01746                                                                   GA1MPGM 
01747  9999-EXIT.     EXIT.                                             GA1MPGM 
