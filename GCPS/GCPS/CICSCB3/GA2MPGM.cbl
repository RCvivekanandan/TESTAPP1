00001  IDENTIFICATION DIVISION.                                         01/12/06
00002  PROGRAM-ID. GA2MPGM.                                             GA2MPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV004
00004  AUTHOR. SANDRA BUCH.                                             GA2MPGM 
00005  DATE-WRITTEN. 02/14/85.                                          GA2MPGM 
00006  DATE-COMPILED.                                                   GA2MPGM 
00007      SKIP3                                                        GA2MPGM 
00008 ******************************************************************GA2MPGM 
00009 *   GA2MPGM   ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM    GA2MPGM 
00010 *                          COSMETIC SURGICAL            GA2M      GA2MPGM 
00011 *                                                                 GA2MPGM 
00012 *     THIS PROGRAM WILL ADD ENTRIES TO THE DENTAL INPATIENT       GA2MPGM 
00013 *   PROCEDURE ARGUMENT/CODE FUNCTION OF ALL LEVEL TABULAR RECORD. GA2MPGM 
00014 *                                                                 GA2MPGM 
00015 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2MPGM 
00016 *   TO ADD ENTRIES TO THIS PARTICULAR ALL LEVEL TABULAR RECORD.   GA2MPGM 
00017 *   THE PROGRAM READS THE ENTRIES, & VALIDATES THE FORMAT OF EACH GA2MPGM 
00018 *   FIELD IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN   GA2MPGM 
00019 *   ERROR).  IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES GA2MPGM 
00020 *   IN ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER GA2MPGM 
00021 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2MPGM 
00022 *   ENTRIES FOR THIS ALL LEVEL TABULAR RECORD.                    GA2MPGM 
00023 *                                                                 GA2MPGM 
00024 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID:#ACOS) GA2MPGM 
00025 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2MPGM 
00026 *   XCTL TO TRANS GA1M OR PROGRAM GA1MPGM.  THIS PROGRAM WILL     GA2MPGM 
00027 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2MPGM 
00028 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2MPGM 
00029 *   CODE.                                                         GA2MPGM 
00030 *                                                                 GA2MPGM 
00031 *   PF7/PF19  PAGE BACKWARD.                                      GA2MPGM 
00032 *   PF8/PF20  PAGE FORWARD.                                       GA2MPGM 
00033 *   PF10/PF22 PAGE TO BOTTOM.                                     GA2MPGM 
00034 *   PF11/PF23 PAGE TO TOP.                                        GA2MPGM 
00035 *                                                                 GA2MPGM 
00036 *   FUNC CODE: GA2M                                               GA2MPGM 
00037 *   MAPSET:    GA2MSETC <<<< REDEFINED BY USER DEFINED MAP >>>>   GA2MPGM 
00038 *   FILES:     GCPSWORK                                           GA2MPGM 
00039 *                                                                 GA2MPGM 
00040 ******************************************************************GA2MPGM 
00041 *                                                                *GA2MPGM 
00042 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA2MPGM 
00043 *       *-*         U P D A T E   H I S T O R Y         *-*      *GA2MPGM 
00044 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA2MPGM 
00045 *                                                                *GA2MPGM 
00046 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GA2MPGM 
00047 *                                                                *GA2MPGM 
00048 *            01/21/86  ENW  CHANGED WS CONTRACT LENGTHS AND      *GA2MPGM 
00049 *                           REMOVED HANDLE CONDITIONS EXCEPT     *GA2MPGM 
00050 *                           FOR MAPFAIL.                         *GA2MPGM 
00051 *                                                                *GA2MPGM 
00052 *            03/20/86  NJS  ADDED CODE TO CHECK FOR A RETURN     *GA2MPGM 
00053 *                           CODE OF '20' FROM GCVIOPGM.  THIS    *GA2MPGM 
00054 *                           IMPLIES THE EDIT TABLE WAS EMPTY     *GA2MPGM 
00055 *                           AND THAT FIELD VALIDATION COULD      *GA2MPGM 
00056 *                           NOT BE PERFORMED.  PF4/PF16 CAN BE   *GA2MPGM 
00057 *                           USED TO ACCEPT THE DATA AS SHOWN     *GA2MPGM 
00058 *                           ON THE SCREEN AND CONTINUE           *GA2MPGM 
00059 *                           PROCESSING.                          *GA2MPGM 
00060 *                                                                *GA2MPGM 
00061 *    EL500   06/18/86  DES  FIXED PF4/16 CODE TO ACCEPT EMPTY    *GA2MPGM 
00062 *                           VALIDATION TABLE CONDITION ONLY,     *GA2MPGM 
00063 *                           ALL OTHER ERRORS STILL MUST BE FIXED *GA2MPGM 
00064 *                                                                *GA2MPGM 
00065 *    D136/   08/27/86  AMJ  1. ADD SUPPORT FOR PF7,8,10 AND 11.  *GA2MPGM 
00066 *    D137                   2. ADD XXXXXXXXX ID SELECTION FIELD. *GA2MPGM 
00067 *                           3. ADD LOCATION COUNTERS (I.E 1 TO 36*GA2MPGM 
00068 *                              OF 54 XXXXXXXXXX DISPLAYED) TO    *GA2MPGM 
00069 *                              SCREEN.                           *GA2MPGM 
00070 *                           4. USE USER-DEFINED LOGICAL MAP FOR  *GA2MPGM 
00071 *                              SCREEN.  THIS REPLACES THE PARTIAL*GA2MPGM 
00072 *                              USE OF BMS MAP AND USER-DEFINED.  *GA2MPGM 
00073 *                                                                *GA2MPGM 
00074 *    D0120   01/30/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT   *GA2MPGM 
00075 *                           EXECUTED FROM TRANSACTION GTM1:      *GA2MPGM 
00076 *                           1. PF1/PF13 - CONSTRUCT COMMAREA AS  *GA2MPGM 
00077 *                              IF GC4A HAD CALLED, XCTL TO ADD   *GA2MPGM 
00078 *                              SCREEN PROGRAM.                   *GA2MPGM 
00079 *                           2. PF3/PF15 - CONSTRUCT COMMAREA AS  *GA2MPGM 
00080 *                              IF GC4A HAD CALLED, XCTL TO       *GA2MPGM 
00081 *                              GTM1PGM.                          *GA2MPGM 
00082 *                                                                *GA2MPGM 
00083 *    D116     8/17/87  FRY    CAPTURE OPERATOR-ID WHEN A 'C3',   *GA2MPGM 
00084 *                             'C5', OR 'G3' RECORD IS UPDATED.   *GA2MPGM 
00085 *                                                               * GA2MPGM 
00086 * D1013 10/07/87  FCG  REMOVE LINK TO CSEXECIO AND REPLACE WITH * GA2MPGM 
00087 *                      LINK TO GCPPDIO. REPLACED LOGIC CODE     * GA2MPGM 
00088 *                      TO PROCESS WITH NEW INTERFACE PROGRAM.   * GA2MPGM 
00089 *                                                                *GA2MPGM 
00090 *                                                                *GA2MPGM 
00091 *  11161  10/24/90  ENW   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA2MPGM 
00092 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA2MPGM 
00093 *                         ROUTINES. REMOVED HARDCODED LENGTHS.   *GA2MPGM 
00094 *                                                               * GA2MPGM 
00095 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD     * GA2MPGM 
00096 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2MPGM 
00097 *                                                               * GA2MPGM 
00098 *D12009 09/27/91  GDM     CONVERT TO COBOL II                   * GA2MPGM 
00099 *                                                               * GA2MPGM 
00100 *  14726/ 10/24/97  AB    ADDED CODE TO SUPPORT THE YEAR 2000    *GA2MPGM 
00101 *  15057                  AND THE EXPANSION OF THE GROUP SPECIFIC*GA2MPGM 
00102 *                         AND CONTRACT KEY TO SUPPORT THE TEXAS  *GA2MPGM 
00103 *                         MERGER.                                *GA2MPGM 
00104 *                                                               * GA2MPGM 
00105 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA2MPGM 
00106 *                                                                *GA2MPGM 
00107 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO*GA2MPGM 
00108 *                             7 BYTES. CHANGE # OF OCCURS TO 396 *GA2MPGM 
00109 *                             ON #ACOS TABULAR.                  *GA2MPGM 
00110 *                                                               * GA2MPGM 
00111 *            10-19-04   NB    RECOMPILE NEW GCPPDIOC COPYBOOK    *GA2MPGM 
00112 *                                                                *GA2MPGM 
      *ICD-10  07/06/11  BA      EXPAND PROCED-CODE FROM 5 TO 7 BYTES. *        
00113 *                          CHANGE LOGIC FOR ICD-10 REQUIREMENTS. *GA2MPGM 
00114 ******************************************************************GA2MPGM 
00115 ******************************************************************GA2MPGM 
00116      SKIP3                                                        GA2MPGM 
00117  ENVIRONMENT DIVISION.                                            GA2MPGM 
00118 /                                                                 GA2MPGM 
00119  DATA DIVISION.                                                   GA2MPGM 
00120  WORKING-STORAGE SECTION.                                         GA2MPGM 
00121                                                                   GA2MPGM 
00122  01  WS-MISC.                                                     GA2MPGM 
00123      05  WS-ONE-LOW                 PIC X VALUE LOW-VALUES.       GA2MPGM 
00124      05  WS-BEGIN                PIC X(24)  VALUE                 GA2MPGM 
00125      '***GA2MPGM WS BEGINS***'.                                   GA2MPGM 
00126      05  WS-PARA-ID              PIC X(4) VALUE 'XXXX'.           GA2MPGM 
00127      05  WS-ABEND-CODE           PIC X(4) VALUE 'XXXX'.           GA2MPGM 
00128      05  WS-SELECT-FROM          PIC S9(4) COMP SYNC VALUE +0.    GA2MPGM 
00129      05  WS-SELECT-TO            PIC S9(4) COMP SYNC VALUE +0.    GA2MPGM 
00130      05  WS-SELECT-OF            PIC S9(4) COMP SYNC VALUE +0.    GA2MPGM 
00131      05  WS-SELECT-FROM-MASK     PIC ZZZ9.                        GA2MPGM 
00132      05  WS-SELECT-FROM-MASK-RDF REDEFINES                        GA2MPGM 
00133              WS-SELECT-FROM-MASK.                                 GA2MPGM 
00134          10  FILLER              PIC X.                           GA2MPGM 
00135          10  WS-SELECT-FROM-TRUNC                                 GA2MPGM 
00136                                  PIC XXX.                         GA2MPGM 
00137      05  WS-SELECT-TO-MASK       PIC ZZZ9.                        GA2MPGM 
00138      05  WS-SELECT-TO-MASK-RDF REDEFINES                          GA2MPGM 
00139              WS-SELECT-TO-MASK.                                   GA2MPGM 
00140          10  FILLER              PIC X.                           GA2MPGM 
00141          10  WS-SELECT-TO-TRUNC  PIC XXX.                         GA2MPGM 
00142      05  WS-SELECT-OF-MASK       PIC ZZZ9.                        GA2MPGM 
00143      05  WS-SELECT-OF-MASK-RDF REDEFINES                          GA2MPGM 
00144              WS-SELECT-OF-MASK.                                   GA2MPGM 
00145          10  FILLER              PIC X.                           GA2MPGM 
00146          10  WS-SELECT-OF-TRUNC  PIC XXX.                         GA2MPGM 
00147      05  WS-GAJ-INDEX            PIC S9(4) COMP SYNC VALUE +0.    GA2MPGM 
00148                                                                   GA2MPGM 
00149                                                                   GA2MPGM 
00150                                                                   GA2MPGM 
00151 ******************************************************************GA2MPGM 
00152 ** THE FIELDS LISTED BELOW ARE USED WHEN CALLING THE PROCEDURE  **GA2MPGM 
00153 ** OR DIAGNOSIS HCSC FILES.                                     **GA2MPGM 
00154 ******************************************************************GA2MPGM 
00155  01  PROCED-KEY.                                                  GA2MPGM 
00156 *** ICD-10 START                                                  GA2MPGM 
           03  SYSTEM-INDICATOR        PIC X(1) VALUE SPACE.                    
00157      03  PROCED-CODE             PIC X(7).                        GA2MPGM 
00158      03  PROCEDR-DIGIT REDEFINES PROCED-CODE                      GA2MPGM 
00159              OCCURS 7 TIMES      PIC X(1).                        GA2MPGM 
00160 *** ICD-10 END                                                    GA2MPGM 
00160                                                                   GA2MPGM 
00161  01  WS-PRO-HAF-COMM-LEN         PIC S9(4)  COMP  VALUE +344.     GA2MPGM 
00162                                                                   GA2MPGM 
00163 ** MAP COBOL SCREEN DSECTS **                                     GA2MPGM 
00164  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2MPGM 
00165      '***  I/O MAPAREA ***'.                                      GA2MPGM 
00166  COPY GA2MSETC.                                                   GA2MPGM 
00167 /*****************************************************************GA2MPGM 
00168 ******************************************************************GA2MPGM 
00169 ******************************************************************GA2MPGM 
00170 **                                                              **GA2MPGM 
00171 **     THIS IS A USER-DEFINED LOGICAL MAP.  ANY CHANGES TO      **GA2MPGM 
00172 **     MAPSET GA2MSETC AFFECTING ITS LENGTH MUST BE TAKEN       **GA2MPGM 
00173 **     INTO ACCOUNT HERE.                                       **GA2MPGM 
00174 **                                                              **GA2MPGM 
00175 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA2MPGM 
00176 **                                                              **GA2MPGM 
00177 **                                            AMJ 8/27/86       **GA2MPGM 
00178 **                                                              **GA2MPGM 
00179 ******************************************************************GA2MPGM 
00180 ******************************************************************GA2MPGM 
00181 ******************************************************************GA2MPGM 
00182                                                                   GA2MPGM 
00183  01  MAP-USER-DEFINED REDEFINES GA2MI01I.                         GA2MPGM 
00184                                                                   GA2MPGM 
00185      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA2MPGM 
00186                                                                   GA2MPGM 
00187      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA2MPGM 
00188      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA2MPGM 
00189      05  MAP-FUNCTION-CODE                PIC X(04).              GA2MPGM 
00190                                                                   GA2MPGM 
00191      05  MAP-MAIN-TITLE-LEN               PIC S9(4) COMP SYNC.    GA2MPGM 
00192      05  MAP-MAIN-TITLE-ATTR              PIC X.                  GA2MPGM 
00193      05  MAP-MAIN-TITLE                   PIC X(43).              GA2MPGM 
00194                                                                   GA2MPGM 
00195      05  MAP-ADD-INQ-LEN                  PIC S9(4) COMP SYNC.    GA2MPGM 
00196      05  MAP-ADD-INQ-ATTR                 PIC X.                  GA2MPGM 
00197      05  MAP-ADD-INQ                      PIC X(03).              GA2MPGM 
00198                                                                   GA2MPGM 
00199      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA2MPGM 
00200      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA2MPGM 
00201      05  MAP-SCREEN-ID                    PIC X(06).              GA2MPGM 
00202                                                                   GA2MPGM 
00203      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA2MPGM 
00204      05  MAP-ID-LINE-ATTR                 PIC X.                  GA2MPGM 
00205      05  MAP-ID-LINE                      PIC X(79).              GA2MPGM 
00206                                                                   GA2MPGM 
00207      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA2MPGM 
00208          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA2MPGM 
00209          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA2MPGM 
00210          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA2MPGM 
00211          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA2MPGM 
00212          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA2MPGM 
00213          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA2MPGM 
00214          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA2MPGM 
00215          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA2MPGM 
00216          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA2MPGM 
00217          10  FILLER                           PIC X(18).          GA2MPGM 
00218      05  CONTRACT-ID-LINE REDEFINES MAP-ID-LINE.                  GA2MPGM 
00219          10  CONTRACT-ID-HEADING              PIC X(14).          GA2MPGM 
00220          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA2MPGM 
00221          10  CONTRACT-GROUP-NO                PIC X(6).           GA2MPGM 
00222          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA2MPGM 
00223          10  CONTRACT-SECTION-NO              PIC X(4).           GA2MPGM 
00224          10  CONTRACT-LOB-HEADING             PIC X(6).           GA2MPGM 
00225          10  CONTRACT-LOB                     PIC X.              GA2MPGM 
00226          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA2MPGM 
00227          10  CONTRACT-PROV-CTL                PIC XX.             GA2MPGM 
00228          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA2MPGM 
00229          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA2MPGM 
00230          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA2MPGM 
00231          10  CONTRACT-EFF-DATE                PIC X(6).           GA2MPGM 
00232          10  FILLER                           PIC X(09).          GA2MPGM 
00233      05  BENEFIT-PROVISION-ID-LINE REDEFINES MAP-ID-LINE.         GA2MPGM 
00234          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA2MPGM 
00235          10  BEN-PROV-GROUP-NO                PIC X(6).           GA2MPGM 
00236          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA2MPGM 
00237          10  BEN-PROV-SECTION-NO              PIC X(4).           GA2MPGM 
00238          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA2MPGM 
00239          10  BEN-PROV-LOB                     PIC X.              GA2MPGM 
00240          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA2MPGM 
00241          10  BEN-PROV-PROV-CTL                PIC XX.             GA2MPGM 
00242          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA2MPGM 
00243          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA2MPGM 
00244          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA2MPGM 
00245          10  BEN-PROV-EFF-DATE                PIC X(6).           GA2MPGM 
00246          10  BEN-PROV-ID-HEADING              PIC X(8).           GA2MPGM 
00247          10  BEN-PROV-ID-NO                   PIC X(6).           GA2MPGM 
00248          10  FILLER                           PIC X(09).          GA2MPGM 
00249                                                                   GA2MPGM 
00250      05  MAP-TABULAR-ID-LEN               PIC S9(4) COMP SYNC.    GA2MPGM 
00251      05  MAP-TABULAR-ID-ATTR              PIC X.                  GA2MPGM 
00252      05  MAP-TABULAR-ID                   PIC X(06).              GA2MPGM 
00253                                                                   GA2MPGM 
00254      05  MAP-TABULAR-SLOT-LEN             PIC S9(4) COMP SYNC.    GA2MPGM 
00255      05  MAP-TABULAR-SLOT-ATTR            PIC X.                  GA2MPGM 
00256      05  MAP-TABULAR-SLOT                 PIC X(07).              GA2MPGM 
00257                                                                   GA2MPGM 
00258      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA2MPGM 
00259      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA2MPGM 
00260      05  MAP-FROM-MENU-ID                 PIC X(04).              GA2MPGM 
00261                                                                   GA2MPGM 
00262      05  MAP-SELECT-TXT1-LEN              PIC S9(4) COMP SYNC.    GA2MPGM 
00263      05  MAP-SELECT-TXT1-ATTR             PIC X.                  GA2MPGM 
00264      05  MAP-SELECT-TXT1                  PIC X(07).              GA2MPGM 
00265                                                                   GA2MPGM 
00266      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA2MPGM 
00267      05  MAP-SELECT-ATTR                  PIC X.                  GA2MPGM 
00268      05  MAP-SELECT                       PIC X(07).              GA2MPGM 
00269                                                                   GA2MPGM 
00270      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA2MPGM 
00271      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA2MPGM 
00272      05  MAP-SELECT-FROM                  PIC X(03).              GA2MPGM 
00273                                                                   GA2MPGM 
00274      05  MAP-SELECT-TXT2-LEN              PIC S9(4) COMP SYNC.    GA2MPGM 
00275      05  MAP-SELECT-TXT2-ATTR             PIC X.                  GA2MPGM 
00276      05  MAP-SELECT-TXT2                  PIC X(02).              GA2MPGM 
00277                                                                   GA2MPGM 
00278      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA2MPGM 
00279      05  MAP-SELECT-TO-ATTR               PIC X.                  GA2MPGM 
00280      05  MAP-SELECT-TO                    PIC X(03).              GA2MPGM 
00281                                                                   GA2MPGM 
00282      05  MAP-SELECT-TXT3-LEN              PIC S9(4) COMP SYNC.    GA2MPGM 
00283      05  MAP-SELECT-TXT3-ATTR             PIC X.                  GA2MPGM 
00284      05  MAP-SELECT-TXT3                  PIC X(02).              GA2MPGM 
00285                                                                   GA2MPGM 
00286      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA2MPGM 
00287      05  MAP-SELECT-OF-ATTR               PIC X.                  GA2MPGM 
00288      05  MAP-SELECT-OF                    PIC X(03).              GA2MPGM 
00289                                                                   GA2MPGM 
00290      05  MAP-SELECT-TXT4-LEN              PIC S9(4) COMP SYNC.    GA2MPGM 
00291      05  MAP-SELECT-TXT4-ATTR             PIC X.                  GA2MPGM 
00292      05  MAP-SELECT-TXT4                  PIC X(30).              GA2MPGM 
00293                                                                   GA2MPGM 
00294 **+**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2MPGM 
00295 **  CHANGE OCCURS COUNT AND LENGTH TO MATCH BMS MAP               GA2MPGM 
00296 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2MPGM 
00297                                                                   GA2MPGM 
00298      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA2MPGM 
00299          INDEXED BY MAP-IDX.                                      GA2MPGM 
00300          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA2MPGM 
00301          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA2MPGM 
00302          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA2MPGM 
00303          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA2MPGM 
00304          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA2MPGM 
00305          15  MAP-CODE-FUNCTION            PIC X(3).               GA2MPGM 
00306                                                                   GA2MPGM 
00307 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2MPGM 
00308 *------ AFTER OCCURS                                              GA2MPGM 
00309      05  MAP-SELECT-TXT5-LEN              PIC S9(4) COMP SYNC.    GA2MPGM 
00310      05  MAP-SELECT-TXT5-ATTR             PIC X.                  GA2MPGM 
00311      05  MAP-SELECT-TXT5                  PIC X(79).              GA2MPGM 
00312                                                                   GA2MPGM 
00313      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA2MPGM 
00314      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA2MPGM 
00315      05  MAP-ERROR-MESSAGE                PIC X(79).              GA2MPGM 
00316      SKIP3                                                        GA2MPGM 
00317  01  FILLER.                                                      GA2MPGM 
00318 ****************************************************************  GA2MPGM 
00319 **   THIS FIELD DESCRIBES THE NUMBER OF OCCURS FOR THE MAP.       GA2MPGM 
00320 ****************************************************************  GA2MPGM 
00321      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +15.  GA2MPGM 
00322 /                                                                 GA2MPGM 
00323 ** ALTERNATIVE WORKFILE KEYS **                                   GA2MPGM 
00324  01  FILLER                      PIC X(32)  VALUE                 GA2MPGM 
00325      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2MPGM 
00326  01  WS-ALT-WORKFILE-KEYS.                                        GA2MPGM 
00327  COPY GCWRKKEY.                                                   GA2MPGM 
00328 /                                                                 GA2MPGM 
00329 ** DATE FORMATTING AREA **                                        GA2MPGM 
00330  01  FILLER                      PIC X(28)  VALUE                 GA2MPGM 
00331      '*** DATE FORMATTING AREA ***'.                              GA2MPGM 
00332  01  WS-DATE-AREA.                                                GA2MPGM 
00333      05  WS-MDY.                                                  GA2MPGM 
00334        10  WS-M                  PIC 99.                          GA2MPGM 
00335        10  WS-D                  PIC 99.                          GA2MPGM 
00336        10  WS-Y                  PIC 99.                          GA2MPGM 
00337      05  WS-YYDDD                PIC 9(5).                        GA2MPGM 
00338      05  FILLER          REDEFINES   WS-YYDDD.                    GA2MPGM 
00339        10  WS-YY                     PIC 99.                      GA2MPGM 
00340        10  WS-DDD                    PIC 999.                     GA2MPGM 
00341                                                                   GA2MPGM 
00342 ******************************************************            GA2MPGM 
00343 **    MONTH TABLE FOR DATE CONVERSION                             GA2MPGM 
00344 **    WILL BE GENERATED ONLY ONCE                                 GA2MPGM 
00345 ******************************************************            GA2MPGM 
00346  01   WS-JUL-GREG-DATE-CONV-TAB.                                  GA2MPGM 
00347      05  WS-MONTH-TABLE   OCCURS 13  INDEXED BY WS-M-IDX          GA2MPGM 
00348          PIC S999 COMP-3.                                         GA2MPGM 
00349                                                                   GA2MPGM 
00350 ** WORKFIELDS **                                                  GA2MPGM 
00351  01  FILLER                           PIC X(16)                   GA2MPGM 
00352              VALUE  '** WORKFIELDS **'.                           GA2MPGM 
00353  01  WS-WORK-FIELDS.                                              GA2MPGM 
00354      05  WS-HEX-00                    PIC X.                      GA2MPGM 
00355      05  WS-QUOTIENT                  PIC 999  COMP-3.            GA2MPGM 
00356      05  WS-REMAINDER                 PIC 999  COMP-3.            GA2MPGM 
00357      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2MPGM 
00358 ***  05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2MPGM 
00359 ***    VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2MPGM 
00360                                                                   GA2MPGM 
00361  01  WS-TEST-AREA                     PIC X(07).                  GA2MPGM 
00362  01  WS-TEST-DATA REDEFINES WS-TEST-AREA.                         GA2MPGM 
00363      05  WS-TEST-DETAIL OCCURS 7 TIMES PIC X(01).                 GA2MPGM 
00364          88  WS-NON-SPECIAL-CHARACTERS VALUE                      GA2MPGM 
00365                                        SPACE                      GA2MPGM 
00366                                        '0' THRU '9'               GA2MPGM 
00367                                        'A' THRU 'I'               GA2MPGM 
00368                                        'J' THRU 'R'               GA2MPGM 
00369                                        'S' THRU 'Z'.              GA2MPGM 
00370                                                                   GA2MPGM 
00371 ****************************************************************  GA2MPGM 
00372 ** THIS GROUP IS A SAVE AREA FOR ONE OCCURENCE OF ENTRY IN TABULARGA2MPGM 
00373 ** RECORD USED IN MINIPULATING THE TABLE.                         GA2MPGM 
00374 ****************************************************************  GA2MPGM 
00375      05  WS-SAVED-FIELDS.                                         GA2MPGM 
00376        10  WS-SAVED-PROCEDURE-ARGUMENT   PIC X(7).                GA2MPGM 
00377        10  WS-SAVED-CODE-FUNCTION        PIC X(3).                GA2MPGM 
00378                                                                   GA2MPGM 
00379      05  WS-SORTED-TAB     OCCURS 16 TIMES INDEXED BY             GA2MPGM 
00380          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2MPGM 
00381        10  WS-PROCEDURE-ARGUMENT      PIC X(7).                   GA2MPGM 
00382        10  WS-CODE-FUNCTION           PIC X(3).                   GA2MPGM 
00383                                                                   GA2MPGM 
00384 *** SWITCHES ***                                                  GA2MPGM 
00385  01  FILLER                           PIC X(14)                   GA2MPGM 
00386              VALUE  '** SWITCHES **'.                             GA2MPGM 
00387  01  WS-SWITCHES.                                                 GA2MPGM 
00388      05  WS-ERROR-SW                  PIC X.                      GA2MPGM 
00389                                                                   GA2MPGM 
00390 ** TITLE LINES **                                                 GA2MPGM 
00391  01  WS-TITLE-LINES.                                              GA2MPGM 
00392      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(43)  VALUE    GA2MPGM 
00393          '   GROUP SPECIFIC CONDITIONAL PROCEDURES   '.           GA2MPGM 
00394      05  CONTRACT-TITLE-LINE                  PIC X(43)  VALUE    GA2MPGM 
00395          '      CONTRACT CONDITIONAL PROCEDURES      '.           GA2MPGM 
00396      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(43)  VALUE    GA2MPGM 
00397          '  BENEFIT PROVISION CONDITIONAL PROCEDURES '.           GA2MPGM 
00398                                                                   GA2MPGM 
00399 /                                                                 GA2MPGM 
00400 *** RECORD LENGTHS ***                                            GA2MPGM 
00401  01  FILLER                           PIC X(20)                   GA2MPGM 
00402              VALUE  '** RECORD LENGTHS **'.                       GA2MPGM 
00403  01  WS-RECORD-LENGTHS.                                           GA2MPGM 
00404     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA2MPGM 
00405     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA2MPGM 
00406     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2MPGM 
00407     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2MPGM 
00408     05 GCVI-COMMAREA-LEN              PIC S9(4) COMP VALUE +19.   GA2MPGM 
00409 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA2MPGM 
00410 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA2MPGM 
00411 *   05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA2MPGM 
00412 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2MPGM 
00413 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA2MPGM 
00414 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA2MPGM 
00415 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2MPGM 
00416 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA2MPGM 
00417 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA2MPGM 
00418 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2MPGM 
00419 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA2MPGM 
00420 ****************************************************************  GA2MPGM 
00421 **   THESE FIELDS DESCRIBE THE TABULAR RECORD.                    GA2MPGM 
00422 ********ACOS****************************************************  GA2MPGM 
00423 *   05 GC-GCTABULR-ACOS-FIXED-LEN     PIC S9(5) COMP-3 VALUE +40. GA2MPGM 
00424 *   05 GC-GCTABULR-ACOS-VARY-LEN      PIC S9(5) COMP-3 VALUE +9.  GA2MPGM 
00425 *   05 GC-GCTABULR-ACOS-VARY-MAX-OCUR PIC S9(5) COMP-3 VALUE +440.GA2MPGM 
00426 /                                                                 GA2MPGM 
00427  COPY COBXIO.                                                     GA2MPGM 
00428 /                                                                 GA2MPGM 
00429 ** ATTRIBUTES **                                                  GA2MPGM 
00430  COPY DFHBMSCA.                                                   GA2MPGM 
00431      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2MPGM 
00432 /                                                                 GA2MPGM 
00433 ** ATTENTION IDENTIFIERS **                                       GA2MPGM 
00434  COPY DFHAID.                                                     GA2MPGM 
00435 /                                                                 GA2MPGM 
00436  01  COMMAREA-POINTER-AREA.                                       GA2MPGM 
00437      05  COMMAREA-PNTR-COMP PIC S9(8) COMP.                       GA2MPGM 
00438      05  COMMAREA-PNTR      REDEFINES                             GA2MPGM 
00439          COMMAREA-PNTR-COMP USAGE IS POINTER.                     GA2MPGM 
00440 /                                                                 GA2MPGM 
00441  01  GCVIOPGMS-PARM.                                              GA2MPGM 
00442  COPY GCVINTRC.                                                   GA2MPGM 
00443      SKIP3                                                        GA2MPGM 
00444  01  WS-GCPS-LENGTHS.                                             GA2MPGM 
00445       COPY GCCDRLEN.                                              GA2MPGM 
00446      SKIP3                                                        GA2MPGM 
00447  01  WS-END                          PIC X(16)  VALUE             GA2MPGM 
00448      '*** W/S ENDS ***'.                                          GA2MPGM 
00449 /                                                                 GA2MPGM 
00450  LINKAGE SECTION.                                                 GA2MPGM 
00451 /                                                                 GA2MPGM 
00452  01  DFHCOMMAREA.                                                 GA2MPGM 
00453  COPY G2ALCKEC.                                                   GA2MPGM 
00454                                                                   GA2MPGM 
00455 *01  DFHCOMMAREA.                                                 GA2MPGM 
00456 *    05  INCOMING-COMMAREA-PNTR-COMP PIC S9(8) COMP.              GA2MPGM 
00457 *    05  INCOMING-COMMAREA-PNTR      REDEFINES                    GA2MPGM 
00458 *        INCOMING-COMMAREA-PNTR-COMP USAGE IS POINTER.            GA2MPGM 
00459 *                                                                 GA2MPGM 
00460 *01  BLL-CELLS.                                                   GA2MPGM 
00461 *    02  FILLER                      PIC S9(8)  COMP.             GA2MPGM 
00462 *    02  COMMAREA-PNTR               PIC S9(8)  COMP.             GA2MPGM 
00463 *    02  ALL-LEVEL-TAB-PNTR          PIC S9(8)  COMP.             GA2MPGM 
00464 *    02  ALL-LEVEL-TAB-PNTR2         PIC S9(8)  COMP.             GA2MPGM 
00465 *    02  COPY-AREA-PNTR              PIC S9(8)  COMP.             GA2MPGM 
00466 *    02  GRP-SPEC-PNTR               PIC S9(8)  COMP.             GA2MPGM 
00467 *    02  CONTRACT-PNTR               PIC S9(8)  COMP.             GA2MPGM 
00468 *    02  CONTRACT-PNTR2              PIC S9(8)  COMP.             GA2MPGM 
00469 *    02  BEN-PROV-PNTR               PIC S9(8)  COMP.             GA2MPGM 
00470 *    02  GCPPDIO-BLL-PNTR            PIC S9(8)  COMP.             GA2MPGM 
00471 *                                                                 GA2MPGM 
00472 *01  GCA-COMMAREA.                                                GA2MPGM 
00473 *COPY G2ALCKEC.                                                   GA2MPGM 
00474 /                                                                 GA2MPGM 
00475 ** I/O PARM, WORKFILE KEY, AND CONTRACT TABULAR RECORD **         GA2MPGM 
00476  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA2MPGM 
00477  COPY GCIOPRM1.                                                   GA2MPGM 
00478 /                                                                 GA2MPGM 
00479  COPY GCWRKDCC.                                                   GA2MPGM 
00480 /                                                                 GA2MPGM 
00481  COPY GCTACOSC.                                                   GA2MPGM 
00482 /                                                                 GA2MPGM 
00483 ****************************************************************  GA2MPGM 
00484 ** THIS SAVE AREA IS USED TO CONTAIN THE TABLE FROM THE TABULAR   GA2MPGM 
00485 ** RECORD; PRIOR TO MERGING INTO THE RECORD BY THE SORT PROCESS   GA2MPGM 
00486 ** WITH NEW ENTRIES FROM THE SCREEN.                              GA2MPGM 
00487 ****************************************************************  GA2MPGM 
00488  01  COPY-OF-TABLE-AREA.                                          GA2MPGM 
00489      05  COPY-OF-TABLE    OCCURS 396 TIMES    INDEXED BY          GA2MPGM 
00490            COPY-IDX.                                              GA2MPGM 
00491        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA2MPGM 
00492        10  COPY-CODE-FUNCTION          PIC X(3).                  GA2MPGM 
00493 /                                                                 GA2MPGM 
00494 ** IO PARM, WITH WORKFILE KEY, AND RECORDS **                     GA2MPGM 
00495  01  IO-PARM-GRP-SPEC-RECORD.                                     GA2MPGM 
00496  COPY GCIOPRM2.                                                   GA2MPGM 
00497 /                                                                 GA2MPGM 
00498  COPY GCWRKDC2.                                                   GA2MPGM 
00499 /                                                                 GA2MPGM 
00500  COPY GCGROUPC.                                                   GA2MPGM 
00501 /                                                                 GA2MPGM 
00502                                                                   GA2MPGM 
00503  01  IO-PARM-CONTRACT-RECORD.                                     GA2MPGM 
00504  COPY GCIOPRM3.                                                   GA2MPGM 
00505 /                                                                 GA2MPGM 
00506  COPY GCWRKDC3.                                                   GA2MPGM 
00507 /                                                                 GA2MPGM 
00508  COPY GCCONTRC.                                                   GA2MPGM 
00509 /                                                                 GA2MPGM 
00510                                                                   GA2MPGM 
00511  01  IO-PARM-BEN-PROV-RECORD.                                     GA2MPGM 
00512  COPY GCIOPRM4.                                                   GA2MPGM 
00513 /                                                                 GA2MPGM 
00514  COPY GCWRKDC4.                                                   GA2MPGM 
00515 /                                                                 GA2MPGM 
00516  COPY GCBENPVC.                                                   GA2MPGM 
00517 /                                                                 GA2MPGM 
00518 ** IO PARM AREA **                                                GA2MPGM 
00519  01  GCPPDIO-PARM-AREA.                                           GA2MPGM 
00520  COPY GCPPDIOC.                                                   GA2MPGM 
00521                                                                   GA2MPGM 
00522 /                                                                 GA2MPGM 
00523  PROCEDURE DIVISION.                                              GA2MPGM 
00524                                                                   GA2MPGM 
00525 ******************************************************************GA2MPGM 
00526 **                H O U S E K E E P I N G                         GA2MPGM 
00527 **                                                                GA2MPGM 
00528 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2MPGM 
00529 **                                                                GA2MPGM 
00530 ******************************************************************GA2MPGM 
00531  0000-HOUSEKEEPING SECTION.                                       GA2MPGM 
00532      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2MPGM 
00533                                                                   GA2MPGM 
00534      IF EIBAID  =  DFHCLEAR                                       GA2MPGM 
00535          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2MPGM 
00536                         ERASE                                     GA2MPGM 
00537          END-EXEC                                                 GA2MPGM 
00538          EXEC CICS RETURN                                         GA2MPGM 
00539          END-EXEC.                                                GA2MPGM 
00540                                                                   GA2MPGM 
00541      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2MPGM 
00542                                   END-EXEC.                       GA2MPGM 
00543 /                                                                 GA2MPGM 
00544 ******************************************************************GA2MPGM 
00545 **                     M A I N L I N E                            GA2MPGM 
00546 **                                                                GA2MPGM 
00547 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2MPGM 
00548 **  TAKEN BY THE OPERATOR.                                        GA2MPGM 
00549 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2MPGM 
00550 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2MPGM 
00551 **     ADDITIONS FROM.                                            GA2MPGM 
00552 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2MPGM 
00553 **     KEY PF12 OR PF24.                                          GA2MPGM 
00554 **  3. RECEIVE THE SCREEN.                                        GA2MPGM 
00555 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2MPGM 
00556 **     MENU.                                                      GA2MPGM 
00557 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2MPGM 
00558 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2MPGM 
00559 **     (RETURN) TO THE DELETE PROGRAM (GA1MPGM).                  GA2MPGM 
00560 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2MPGM 
00561 **     (RETURN) TO THE PREVIOUS MENU.                             GA2MPGM 
00562 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN EXECUTE  GA2MPGM 
00563 **     THE NORMAL ADD LOGIC, EXCEPT BYPASS THE EMPTY VALIDATION   GA2MPGM 
00564 **     TABLE CONDITION FOR THE COMBINATION CODE FIELD.            GA2MPGM 
00565 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2MPGM 
00566 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2MPGM 
00567 **                                                                GA2MPGM 
00568 ******************************************************************GA2MPGM 
00569  1000-MAIN-LINE SECTION.                                          GA2MPGM 
00570                                                                   GA2MPGM 
00571      MOVE '1000'  TO  WS-PARA-ID.                                 GA2MPGM 
00572      IF EIBTRNID  NOT =  'GA2M'                                   GA2MPGM 
00573         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2MPGM 
00574         GO TO 1099-RETURN.                                        GA2MPGM 
00575                                                                   GA2MPGM 
00576      EXEC CICS RECEIVE   MAP('GA2MI01') MAPSET('GA2MSET')         GA2MPGM 
00577         INTO(GA2MI01I) END-EXEC.                                  GA2MPGM 
00578                                                                   GA2MPGM 
00579      IF MAP-SCREEN-ID NOT = '002M00'                              GA2MPGM 
00580         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2MPGM 
00581                                                                   GA2MPGM 
00582      IF EIBAID  =  DFHENTER                                       GA2MPGM 
00583         PERFORM 2000-ADD-PROCESSING                               GA2MPGM 
00584         GO TO 1099-RETURN.                                        GA2MPGM 
00585                                                                   GA2MPGM 
00586      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2MPGM 
00587         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2MPGM 
00588                                                                   GA2MPGM 
00589      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2MPGM 
00590         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2MPGM 
00591                                                                   GA2MPGM 
00592      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2MPGM 
00593         PERFORM 2000-ADD-PROCESSING                               GA2MPGM 
00594         GO TO 1099-RETURN.                                        GA2MPGM 
00595                                                                   GA2MPGM 
00596      SET MAP-IDX  TO  1.                                          GA2MPGM 
00597      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX).           GA2MPGM 
00598      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2MPGM 
00599 -    ' THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                  GA2MPGM 
00600      EXEC CICS SEND   MAP('GA2MI01') MAPSET('GA2MSET') DATAONLY   GA2MPGM 
00601         FROM(GA2MI01O) CURSOR END-EXEC.                           GA2MPGM 
00602                                                                   GA2MPGM 
00603  1099-RETURN.                                                     GA2MPGM 
00604      EXEC CICS RETURN TRANSID('GA2M')                             GA2MPGM 
00605                COMMAREA(DFHCOMMAREA)                              GA2MPGM 
00606      END-EXEC.                                                    GA2MPGM 
00607                                                                   GA2MPGM 
00608      GOBACK.                                                      GA2MPGM 
00609 /                                                                 GA2MPGM 
00610 ******************************************************************GA2MPGM 
00611 **               A D D   P R O C E S S I N G                      GA2MPGM 
00612 **                                                                GA2MPGM 
00613 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2MPGM 
00614 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2MPGM.            GA2MPGM 
00615 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2MPGM 
00616 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2MPGM 
00617 **     SKIP TO THE NEXT LINE.                                     GA2MPGM 
00618 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2MPGM 
00619 **     WITH SPECIAL CHARACTERS, AND NUMERIC FIELDS ARE TESTED     GA2MPGM 
00620 **     WITH THE NUMERIC CLASS TEST.  THE OPERATOR MUST ENTER SOME GA2MPGM 
00621 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2MPGM 
00622 **     DATA.                                                      GA2MPGM 
00623 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2MPGM 
00624 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2MPGM 
00625 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2MPGM 
00626 **     ORDER, FIELD BY FIELD.                                     GA2MPGM 
00627 **  6. THE ALL LEVEL TABULAR RECORD IS READ, AND A COPY OF THE    GA2MPGM 
00628 **     TABLE IS MADE.                                             GA2MPGM 
00629 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2MPGM 
00630 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2MPGM 
00631 **     BACK INTO THE RECORD.                                      GA2MPGM 
00632 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2MPGM 
00633 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2MPGM 
00634 **                                                                GA2MPGM 
00635 ******************************************************************GA2MPGM 
00636  2000-ADD-PROCESSING SECTION.                                     GA2MPGM 
00637                                                                   GA2MPGM 
00638      MOVE '2000'  TO  WS-PARA-ID.                                 GA2MPGM 
00639      MOVE 'N'  TO  WS-ERROR-SW.                                   GA2MPGM 
00640      MOVE 'Y'  TO  GCVI-TABLE-SW.                                 GA2MPGM 
00641      MOVE ZERO  TO  WS-ADD-COUNT.                                 GA2MPGM 
00642      SET MAP-IDX  TO  1.                                          GA2MPGM 
00643                                                                   GA2MPGM 
00644      MOVE '2005'  TO  WS-PARA-ID.                                 GA2MPGM 
00645  2005-RESET-ALL-ATTRIBUTES.                                       GA2MPGM 
00646      MOVE DFHBMUNF TO MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)       GA2MPGM 
00647                       MAP-CODE-FUNCTION-ATTR (MAP-IDX).           GA2MPGM 
00648      IF MAP-IDX   <  WS-MAP-ROW                                   GA2MPGM 
00649         SET MAP-IDX   UP BY  1                                    GA2MPGM 
00650         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2MPGM 
00651                                                                   GA2MPGM 
00652      SET MAP-IDX  TO  1.                                          GA2MPGM 
00653      MOVE '2010'  TO  WS-PARA-ID.                                 GA2MPGM 
00654  2010-VALIDATE-ADD-ENTRIES.                                       GA2MPGM 
00655      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX) = ZERO  AND          GA2MPGM 
00656         MAP-CODE-FUNCTION-LEN (MAP-IDX) = ZERO                    GA2MPGM 
00657                                                                   GA2MPGM 
00658         IF MAP-IDX   <  WS-MAP-ROW                                GA2MPGM 
00659            SET MAP-IDX   UP BY  1                                 GA2MPGM 
00660            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2MPGM 
00661         ELSE                                                      GA2MPGM 
00662            GO TO 2020-CHECK-FOR-ERRORS.                           GA2MPGM 
00663                                                                   GA2MPGM 
00664      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)  =  ZERO             GA2MPGM 
00665         MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  GA2MPGM 
00666         MOVE '???????'  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)      GA2MPGM 
00667         IF WS-ERROR-SW  NOT  =  'Y'                               GA2MPGM 
00668            MOVE 'Y'  TO  WS-ERROR-SW                              GA2MPGM 
00669            MOVE -1   TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)     GA2MPGM 
00670            MOVE ' *** PROCEDURE ARGUMENT IS INVALID ***'          GA2MPGM 
00671               TO MAP-ERROR-MESSAGE                                GA2MPGM 
00672         ELSE                                                      GA2MPGM 
00673            NEXT SENTENCE                                          GA2MPGM 
00674      ELSE                                                         GA2MPGM 
00675         PERFORM 2015-EDIT-PROCEDURE-ARGUMENT                      GA2MPGM 
00676            THRU 2015-EXIT.                                        GA2MPGM 
00677                                                                   GA2MPGM 
00678      IF  MAP-CODE-FUNCTION-LEN (MAP-IDX)  =  ZERO                 GA2MPGM 
00679         MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)       GA2MPGM 
00680         MOVE '???'  TO  MAP-CODE-FUNCTION (MAP-IDX)               GA2MPGM 
00681         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2MPGM 
00682            MOVE 'Y'  TO  WS-ERROR-SW                              GA2MPGM 
00683            MOVE -1   TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)          GA2MPGM 
00684            MOVE ' *** CODE FUNCTION IS INVALID ***'               GA2MPGM 
00685            TO  MAP-ERROR-MESSAGE                                  GA2MPGM 
00686         ELSE                                                      GA2MPGM 
00687            NEXT SENTENCE                                          GA2MPGM 
00688      ELSE                                                         GA2MPGM 
00689         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2MPGM 
00690            WS-TEST-AREA                                           GA2MPGM 
00691 ***     MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2MPGM 
00692 ***        WS-SAVED-CODE-FUNCTION                                 GA2MPGM 
00693 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION FROM QUOTES TO '\
00694 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION FROM                    GA2MPGM 
00695 ***        WS-NON-SPECIAL-CHARACTERS  TO  QUOTES                  GA2MPGM 
00696 ***     IF  WS-SAVED-CODE-FUNCTION NOT = QUOTES                   GA2MPGM 
00697         IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                GA2MPGM 
00698                 WS-NON-SPECIAL-CHARACTERS  (2) AND                GA2MPGM 
00699                 WS-NON-SPECIAL-CHARACTERS  (3) AND                GA2MPGM 
00700                 WS-NON-SPECIAL-CHARACTERS  (4) AND                GA2MPGM 
00701                 WS-NON-SPECIAL-CHARACTERS  (5) AND                GA2MPGM 
00702                 WS-NON-SPECIAL-CHARACTERS  (6) AND                GA2MPGM 
00703                 WS-NON-SPECIAL-CHARACTERS  (7)                    GA2MPGM 
00704            MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)    GA2MPGM 
00705            IF  WS-ERROR-SW  NOT =  'Y'                            GA2MPGM 
00706               MOVE 'Y'  TO  WS-ERROR-SW                           GA2MPGM 
00707               MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)        GA2MPGM 
00708               MOVE '        *** CODE FUNCTION IS INVALID ***'     GA2MPGM 
00709                  TO  MAP-ERROR-MESSAGE.                           GA2MPGM 
00710                                                                   GA2MPGM 
00711      IF MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  NOT =  DFHBMUBF    GA2MPGM 
00712                        AND                                        GA2MPGM 
00713         MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2MPGM 
00714         ADD 1  TO  WS-ADD-COUNT                                   GA2MPGM 
00715         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2MPGM 
00716         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                 GA2MPGM 
00717                              WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)  GA2MPGM 
00718         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2MPGM 
00719                                  WS-CODE-FUNCTION (WS-SORT-IDX).  GA2MPGM 
00720                                                                   GA2MPGM 
00721      IF MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2MPGM 
00722         MOVE 'MULT02' TO  GCVI-FIELDS-KEY-ID                      GA2MPGM 
00723         MOVE ZEROES   TO  GCVI-RETURN-CODE                        GA2MPGM 
00724         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO  GCVI-VALUE-LEN-3    GA2MPGM 
00725         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2MPGM 
00726                        COMMAREA(GCVIOPGMS-PARM)                   GA2MPGM 
00727                        LENGTH(GCVI-COMMAREA-LEN) END-EXEC         GA2MPGM 
00728         IF GCVI-VALUE-NOT-FOUND                                   GA2MPGM 
00729            IF WS-ERROR-SW  NOT =  'Y'                             GA2MPGM 
00730               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2MPGM 
00731               MOVE -1    TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)      GA2MPGM 
00732               MOVE '*** COMBINATION CODE INVALID ***'             GA2MPGM 
00733               TO  MAP-ERROR-MESSAGE                               GA2MPGM 
00734               MOVE 'Y'  TO  WS-ERROR-SW                           GA2MPGM 
00735            ELSE                                                   GA2MPGM 
00736               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2MPGM 
00737         ELSE                                                      GA2MPGM 
00738            IF GCVI-VALUE-NOT-LOADED                               GA2MPGM 
00739               IF EIBAID  =  DFHPF4 OR  =  DFHPF16                 GA2MPGM 
00740                  NEXT SENTENCE                                    GA2MPGM 
00741               ELSE                                                GA2MPGM 
00742                 MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR(MAP-IDX)GA2MPGM 
00743                  IF WS-ERROR-SW  NOT =  'Y'                       GA2MPGM 
00744                     MOVE 'Y'  TO  WS-ERROR-SW                     GA2MPGM 
00745                     MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)  GA2MPGM 
00746               MOVE 'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS PGA2MPGM 
00747 -                       'F4/PF16 TO CONTINUE'                     GA2MPGM 
00748                     TO  MAP-ERROR-MESSAGE.                        GA2MPGM 
00749                                                                   GA2MPGM 
00750      IF MAP-IDX   <  WS-MAP-ROW                                   GA2MPGM 
00751         SET MAP-IDX   UP BY  1                                    GA2MPGM 
00752         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2MPGM 
00753 /                                                                 GA2MPGM 
00754 ****************************************************************  GA2MPGM 
00755 ** THIS GENERIC ROUTINE WILL EDIT THE PROCEDURE CODE. FIELDS  **  GA2MPGM 
00756 ** REQUIRING PROBABLE 'TAILORING' APPEAR AS -->NAME.          **  GA2MPGM 
00757 **                                                            **  GA2MPGM 
00758 ** REQUIRED WORKING-STORAGE:                                  **  GA2MPGM 
00759 **   1. 01  PROCED-KEY.                                       **  GA2MPGM 
00760 **          03  SYSTEM-INDICATOR          PIC X(1).           **  GA2MPGM 
00761 **          03  PROCED-CODE               PIC X(7).           **  GA2MPGM 
00762 **              04  PROCEDR-DIGIT REDEFINES PROCED-CODE       **  GA2MPGM 
00763 **                  OCCURS 7 TIMES        PIC X(1).           **  GA2MPGM 
00764 **                                                            **  GA2MPGM 
00765 **   2. 01  WS-PRO-HAF-COMM-LEN   PIC S9(4) COMP VALUE +344.  **  GA2MPGM 
00766 **                                                            **  GA2MPGM 
00767 **   3. COPY COBXIO.                                          **  GA2MPGM 
00768 ****************************************************************  GA2MPGM 
00769                                                                   GA2MPGM 
00770  2015-EDIT-PROCEDURE-ARGUMENT.                                    GA2MPGM 
00771                                                                   GA2MPGM 
00772 ***  MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2MPGM 
00773 ***      WS-SAVED-PROCEDURE-ARGUMENT.                             GA2MPGM 
00774                                                                   GA2MPGM 
00775      MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2MPGM 
00776          WS-TEST-AREA.                                            GA2MPGM 
00777                                                                   GA2MPGM 
00778 ***  TRANSFORM  WS-SAVED-PROCEDURE-ARGUMENT FROM QUOTES TO '\
00779 ***  TRANSFORM  WS-SAVED-PROCEDURE-ARGUMENT FROM                  GA2MPGM 
00780 ***      WS-NON-SPECIAL-CHARACTERS  TO  QUOTES.                   GA2MPGM 
00781 ***  IF WS-SAVED-PROCEDURE-ARGUMENT  NOT =  QUOTES                GA2MPGM 
00782                                                                   GA2MPGM 
00783      IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                   GA2MPGM 
00784              WS-NON-SPECIAL-CHARACTERS  (2) AND                   GA2MPGM 
00785              WS-NON-SPECIAL-CHARACTERS  (3) AND                   GA2MPGM 
00786              WS-NON-SPECIAL-CHARACTERS  (4) AND                   GA2MPGM 
00787              WS-NON-SPECIAL-CHARACTERS  (5) AND                   GA2MPGM 
00788              WS-NON-SPECIAL-CHARACTERS  (6) AND                   GA2MPGM 
00789              WS-NON-SPECIAL-CHARACTERS  (7)                       GA2MPGM 
00790          MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX) GA2MPGM 
00791         IF WS-ERROR-SW  NOT =  'Y'                                GA2MPGM 
00792            MOVE 'Y'  TO  WS-ERROR-SW                              GA2MPGM 
00793            MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)      GA2MPGM 
00794            MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'   GA2MPGM 
00795                TO  MAP-ERROR-MESSAGE                              GA2MPGM 
00796            GO TO 2015-EXIT                                        GA2MPGM 
00797          ELSE                                                     GA2MPGM 
00798            GO TO 2015-EXIT.                                       GA2MPGM 
00799                                                                   GA2MPGM 
00801      EXEC CICS GETMAIN                                            GA2MPGM 
00802         SET(ADDRESS OF GCPPDIO-PARM-AREA)                         GA2MPGM 
00803         INITIMG(WS-HEX-00)                                        GA2MPGM 
00804         LENGTH(GCPPDIO-CA-LEN)                                    GA2MPGM 
00804      END-EXEC.                                                    GA2MPGM 
00805                                                                   GA2MPGM 
00806 ***  SERVICE RELOAD GCPPDIO-PARM-AREA.                            GA2MPGM 
00807                                                                   GA2MPGM 
           MOVE MAP-PROCEDURE-ARGUMENT(MAP-IDX) TO PROCED-CODE.                 
                                                                                
00799 *** ICD-10 START                                                  GA2MPGM 
           IF WS-TEST-AREA (1:3) = 'BIT'                                        
              MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX)           
                                                                                
              IF WS-ERROR-SW  NOT =  'Y'                                        
                 MOVE 'Y' TO  WS-ERROR-SW                                       
                 MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)               
                 MOVE                                                           
           ' *** REQUESTED PCG BIT IS NOT ALLOWED FOR THIS TABULAR ***'         
                                                  TO  MAP-ERROR-MESSAGE         
              END-IF                                                            
                                                                                
              GO TO 2015-EXIT.                                                  
      *** ICD-10 END                                                            
                                                                                
           MOVE 'PRCDR03 '  TO  GCPPDIO-REQUEST-TYPE                            
                                                                                
           IF PROCEDR-DIGIT(5)  =  SPACE                                        
              MOVE  'H'  TO  SYSTEM-INDICATOR                                   
           ELSE                                                                 
      *** ICD-10 START                                                          
              IF PROCEDR-DIGIT(7)  =  SPACE                                     
                 MOVE  'C'  TO  SYSTEM-INDICATOR                                
              ELSE                                                              
00799            MOVE  'Z'  TO  SYSTEM-INDICATOR                        GA2MPGM 
              END-IF                                                            
           END-IF                                                               
      *** ICD-10 END                                                            
      *    END-IF                                                               
00817                                                                   GA2MPGM 
           MOVE PROCED-KEY     TO  GCPPDIO-SERVICE-CODE-AREA.                   
                                                                                
00818      EXEC CICS LINK PROGRAM('GCPPDIO')                            GA2MPGM 
00819           COMMAREA(GCPPDIO-PARM-AREA)                             GA2MPGM 
00820           LENGTH(GCPPDIO-CA-LEN)                                  GA2MPGM 
00821      END-EXEC.                                                    GA2MPGM 
00822                                                                   GA2MPGM 
00823      IF GCPPDIO-SUCCESSFUL                                        GA2MPGM 
00824          GO TO 2015-EXIT.                                         GA2MPGM 
00825                                                                   GA2MPGM 
00826      IF NOT GCPPDIO-REC-NOT-FOUND                                 GA2MPGM 
00827          MOVE 'DEW1'  TO  WS-ABEND-CODE                           GA2MPGM 
00828          MOVE GCPPDIO-RETURN-MESSAGE TO MAP-ERROR-MESSAGE         GA2MPGM 
00829          PERFORM 9999-ERROR-MSG-THEN-ABEND.                       GA2MPGM 
00830                                                                   GA2MPGM 
00831      MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX).     GA2MPGM 
00832                                                                   GA2MPGM 
00833      IF WS-ERROR-SW  NOT =  'Y'                                   GA2MPGM 
00834          MOVE 'Y' TO  WS-ERROR-SW                                 GA2MPGM 
00835          MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)         GA2MPGM 
00836          MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'     GA2MPGM 
00837            TO  MAP-ERROR-MESSAGE.                                 GA2MPGM 
00838                                                                   GA2MPGM 
00839  2015-EXIT. EXIT.                                                 GA2MPGM 
00840 /                                                                 GA2MPGM 
00841  2020-CHECK-FOR-ERRORS.                                           GA2MPGM 
00842      MOVE '2020'  TO  WS-PARA-ID.                                 GA2MPGM 
00843      IF WS-ERROR-SW  =  'Y' OR GCVI-TABLE-SW = 'N'                GA2MPGM 
00844         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE,                   GA2MPGM 
00845            MAP-MAIN-TITLE,                                        GA2MPGM 
00846            MAP-SCREEN-ID,                                         GA2MPGM 
00847            MAP-TABULAR-ID,                                        GA2MPGM 
00848            MAP-TABULAR-SLOT,                                      GA2MPGM 
00849            MAP-FROM-MENU-ID,                                      GA2MPGM 
00850            MAP-ID-LINE                                            GA2MPGM 
00851         MOVE '2100'  TO  WS-PARA-ID                               GA2MPGM 
00852         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2MPGM 
00853            VARYING MAP-IDX  FROM  1  BY  1                        GA2MPGM 
00854               UNTIL MAP-IDX  >  WS-MAP-ROW                        GA2MPGM 
00855                                                                   GA2MPGM 
00856         EXEC CICS SEND   MAP('GA2MI01') MAPSET('GA2MSET')         GA2MPGM 
00857            DATAONLY FROM(GA2MI01O) CURSOR END-EXEC                GA2MPGM 
00858         GO TO 2099-EXIT.                                          GA2MPGM 
00859                                                                   GA2MPGM 
00860      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2MPGM 
00861         MOVE '                  *** ADD ENTRY NOT FOUND ***'      GA2MPGM 
00862            TO  MAP-ERROR-MESSAGE                                  GA2MPGM 
00863         SET MAP-IDX  TO  1                                        GA2MPGM 
00864         MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)         GA2MPGM 
00865         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE,                   GA2MPGM 
00866         MAP-MAIN-TITLE,                                           GA2MPGM 
00867         MAP-SCREEN-ID,                                            GA2MPGM 
00868         MAP-TABULAR-ID,                                           GA2MPGM 
00869         MAP-TABULAR-SLOT,                                         GA2MPGM 
00870         MAP-FROM-MENU-ID,                                         GA2MPGM 
00871         MAP-ID-LINE                                               GA2MPGM 
00872         EXEC CICS SEND   MAP('GA2MI01') MAPSET('GA2MSET')         GA2MPGM 
00873            DATAONLY FROM(GA2MI01O) CURSOR END-EXEC                GA2MPGM 
00874         GO TO 2099-EXIT.                                          GA2MPGM 
00875                                                                   GA2MPGM 
00876  2025-CONTINUE-UPDATE.                                            GA2MPGM 
00877      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2MPGM 
00878               GC-GCIOPARM-LEN + GC-WORKFILE-KEY-LEN +             GA2MPGM 
00879                         GC-GCTABULR-ACOS-FIXED-LEN +              GA2MPGM 
00880           (GC-GCTABULR-ACOS-VARY-LEN *                            GA2MPGM 
00881           GC-GCTABULR-ACOS-VARY-MAX-OCUR).                        GA2MPGM 
00882                                                                   GA2MPGM 
00883 ***  EXEC CICS GETMAIN SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00) GA2MPGM 
00884      EXEC CICS GETMAIN                                            GA2MPGM 
00885         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2MPGM 
00886         INITIMG(WS-HEX-00)                                        GA2MPGM 
00887         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2MPGM 
00888 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2MPGM 
00889 ***  ADD ALL-LEVEL-TAB-PNTR, 4096  GIVING  ALL-LEVEL-TAB-PNTR2.   GA2MPGM 
00890                                                                   GA2MPGM 
00891      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA2MPGM 
00892         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2MPGM 
00893         MOVE  'G'   TO  GCIO-WRK-STATUS-CODE                      GA2MPGM 
00894         MOVE  'G3'  TO  GCIO-WRK-RECORD-TYPE                      GA2MPGM 
00895         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2MPGM 
00896         MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2MPGM 
00897         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA2MPGM 
00898         MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2MPGM 
00899         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA2MPGM 
00900         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2MPGM 
00901         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2MPGM 
00902                          GCIO-WRK-PROVIDER-CONTROL                GA2MPGM 
00903         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2MPGM 
00904         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2MPGM 
00905         MOVE MAP-TABULAR-ID TO GCIO-WRK-PROVISION-ID              GA2MPGM 
00906         MOVE MAP-TABULAR-SLOT TO GCIO-WRK-PROVISION-SLOT-NO       GA2MPGM 
00907         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2MPGM 
00908         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2MPGM 
00909                                                                   GA2MPGM 
00910      IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA2MPGM 
00911         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2MPGM 
00912         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2MPGM 
00913         MOVE  'C3'  TO  GCIO-WRK-RECORD-TYPE                      GA2MPGM 
00914         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2MPGM 
00915         MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2MPGM 
00916         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA2MPGM 
00917         MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2MPGM 
00918         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA2MPGM 
00919         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2MPGM 
00920         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2MPGM 
00921         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2MPGM 
00922         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2MPGM 
00923         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2MPGM 
00924         MOVE MAP-TABULAR-ID TO GCIO-WRK-PROVISION-ID              GA2MPGM 
00925         MOVE MAP-TABULAR-SLOT TO GCIO-WRK-PROVISION-SLOT-NO       GA2MPGM 
00926         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2MPGM 
00927         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2MPGM 
00928                                                                   GA2MPGM 
00929      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA2MPGM 
00930         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2MPGM 
00931         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2MPGM 
00932         MOVE  'C5'  TO  GCIO-WRK-RECORD-TYPE                      GA2MPGM 
00933         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2MPGM 
00934         MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2MPGM 
00935         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA2MPGM 
00936         MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2MPGM 
00937         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA2MPGM 
00938         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2MPGM 
00939         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2MPGM 
00940         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2MPGM 
00941         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2MPGM 
00942         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2MPGM 
00943         MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID            GA2MPGM 
00944         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2MPGM 
00945         MOVE MAP-TABULAR-ID TO GCIO-WRK-TAB-PROVISION-ID          GA2MPGM 
00946         MOVE MAP-TABULAR-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.       GA2MPGM 
00947                                                                   GA2MPGM 
00948 *    MOVE  WS-Y  TO  WS-YY.                                       GA2MPGM 
00949 *    IF WS-M  >  2                                                GA2MPGM 
00950 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA2MPGM 
00951 *          REMAINDER  WS-REMAINDER                                GA2MPGM 
00952 *    ELSE                                                         GA2MPGM 
00953 *       MOVE 1  TO  WS-REMAINDER.                                 GA2MPGM 
00954 *    SET WS-M-IDX  TO  WS-M.                                      GA2MPGM 
00955 *    MOVE WS-MONTH-TABLE (WS-M)  TO  WS-DDD.                      GA2MPGM 
00956 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2MPGM 
00957 *    IF WS-REMAINDER  =  ZERO                                     GA2MPGM 
00958 *       ADD 1  TO  WS-DDD.                                        GA2MPGM 
00959 *                                                                 GA2MPGM 
00960 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2MPGM 
00961      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA2MPGM 
00962      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2MPGM 
00963      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2MPGM 
00964                                                                   GA2MPGM 
00965      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2MPGM 
00966      TO   GAJ-ENTRY-COUNT.                                        GA2MPGM 
00967                                                                   GA2MPGM 
00968      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2MPGM 
00969                                                                   GA2MPGM 
00970      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2MPGM 
00971         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2MPGM 
00972         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2MPGM 
00973                                                                   GA2MPGM 
00974      IF  NOT GCIO-GOOD-RETURN                                     GA2MPGM 
00975         MOVE '*** ERROR READING ALL LEVEL TABULAR.  CONTACT SYSTEMGA2MPGM 
00976 -    'S AREA ***'  TO  MAP-ERROR-MESSAGE                          GA2MPGM 
00977         MOVE '2MF1'  TO  WS-ABEND-CODE                            GA2MPGM 
00978         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
00979                                                                   GA2MPGM 
00980      IF  WS-ADD-COUNT  NOT >  ZERO                                GA2MPGM 
00981         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2MPGM 
00982                                                                   GA2MPGM 
00983      SET WS-SORT-IDX  TO  1.                                      GA2MPGM 
00984      SET WS-SORT-IDX2  TO  2.                                     GA2MPGM 
00985      MOVE '2030'  TO  WS-PARA-ID.                                 GA2MPGM 
00986                                                                   GA2MPGM 
00987  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2MPGM 
00988      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2MPGM 
00989         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2MPGM 
00990                                                                   GA2MPGM 
00991      IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <                     GA2MPGM 
00992         WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                      GA2MPGM 
00993         SET WS-SORT-IDX2  UP BY  1                                GA2MPGM 
00994         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2MPGM 
00995      ELSE                                                         GA2MPGM 
00996         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2MPGM 
00997            WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                   GA2MPGM 
00998            MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                    GA2MPGM 
00999               WS-SAVED-FIELDS                                     GA2MPGM 
01000            MOVE WS-SORTED-TAB (WS-SORT-IDX2) TO                   GA2MPGM 
01001               WS-SORTED-TAB (WS-SORT-IDX)                         GA2MPGM 
01002            MOVE WS-SAVED-FIELDS TO                                GA2MPGM 
01003               WS-SORTED-TAB (WS-SORT-IDX2)                        GA2MPGM 
01004            SET WS-SORT-IDX2  UP BY  1                             GA2MPGM 
01005            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2MPGM 
01006                                                                   GA2MPGM 
01007      IF WS-CODE-FUNCTION (WS-SORT-IDX) <                          GA2MPGM 
01008         WS-CODE-FUNCTION (WS-SORT-IDX2)                           GA2MPGM 
01009         SET WS-SORT-IDX2  UP BY  1                                GA2MPGM 
01010         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2MPGM 
01011      ELSE                                                         GA2MPGM 
01012         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2MPGM 
01013            WS-CODE-FUNCTION (WS-SORT-IDX2)                        GA2MPGM 
01014            MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                    GA2MPGM 
01015               WS-SAVED-FIELDS                                     GA2MPGM 
01016            MOVE WS-SORTED-TAB (WS-SORT-IDX2) TO                   GA2MPGM 
01017               WS-SORTED-TAB (WS-SORT-IDX)                         GA2MPGM 
01018            MOVE WS-SAVED-FIELDS TO                                GA2MPGM 
01019               WS-SORTED-TAB (WS-SORT-IDX2)                        GA2MPGM 
01020            SET WS-SORT-IDX2  UP BY  1                             GA2MPGM 
01021            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2MPGM 
01022                                                                   GA2MPGM 
01023      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2MPGM 
01024      MOVE WS-SORTED-TAB (WS-SORT-IDX3)                            GA2MPGM 
01025        TO WS-SORTED-TAB (WS-SORT-IDX2).                           GA2MPGM 
01026      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2MPGM 
01027      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2MPGM 
01028                                                                   GA2MPGM 
01029  2040-ARE-WE-DONE-WITH-SORT.                                      GA2MPGM 
01030      MOVE '2040'  TO  WS-PARA-ID.                                 GA2MPGM 
01031      SET WS-SORT-IDX  UP BY  1.                                   GA2MPGM 
01032      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2MPGM 
01033         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2MPGM 
01034         SET WS-SORT-IDX2  UP BY  1                                GA2MPGM 
01035         MOVE '2030'  TO  WS-PARA-ID                               GA2MPGM 
01036         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2MPGM 
01037                                                                   GA2MPGM 
01038      SET WS-ADD-COUNT  TO  WS-SORT-IDX.                           GA2MPGM 
01039 *    MOVE HIGH-VALUES  TO  WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)    GA2MPGM 
01040 ***********************    WS-CODE-FUNCTION (WS-SORT-IDX).        GA2MPGM 
01041      MOVE HIGH-VALUES      TO  WS-SORTED-TAB (WS-SORT-IDX).       GA2MPGM 
01042      MOVE GAJ-ENTRY-COUNT  TO  GAJ-ENTRY-COUNT.                   GA2MPGM 
01043                                                                   GA2MPGM 
01044      COMPUTE  WS-COPY-LENGTH  =                                   GA2MPGM 
01045           GAJ-ENTRY-COUNT  *  GC-GCTABULR-ACOS-VARY-LEN.          GA2MPGM 
01046                                                                   GA2MPGM 
01047 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA2MPGM 
01048      EXEC CICS GETMAIN                                            GA2MPGM 
01049         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA2MPGM 
01050         LENGTH(WS-COPY-LENGTH)                                    GA2MPGM 
01051         INITIMG(WS-HEX-00) END-EXEC.                              GA2MPGM 
01052 ***  SERVICE RELOAD  COPY-OF-TABLE-AREA.                          GA2MPGM 
01053                                                                   GA2MPGM 
01054      SET COPY-IDX,  GAJ-INDEX  TO 1.                              GA2MPGM 
01055                                                                   GA2MPGM 
01056      MOVE '2050'  TO  WS-PARA-ID.                                 GA2MPGM 
01057  2050-MAKE-A-COPY-OF-RECORD.                                      GA2MPGM 
01058      IF GAJ-INDEX  NOT >  GAJ-ENTRY-COUNT                         GA2MPGM 
01059         MOVE GAJ-ENTRY (GAJ-INDEX)  TO                            GA2MPGM 
01060            COPY-OF-TABLE (COPY-IDX)                               GA2MPGM 
01061         SET COPY-IDX, GAJ-INDEX  UP BY  1                         GA2MPGM 
01062         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2MPGM 
01063                                                                   GA2MPGM 
01064      IF WS-ADD-COUNT  +  GAJ-ENTRY-COUNT  >                       GA2MPGM 
01065         GC-GCTABULR-ACOS-VARY-MAX-OCUR                            GA2MPGM 
01066         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2MPGM 
01067 -    'EASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE       GA2MPGM 
01068         MOVE '2ML1'  TO  WS-ABEND-CODE                            GA2MPGM 
01069         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01070                                                                   GA2MPGM 
01071      SET WS-SORT-IDX, COPY-IDX, GAJ-INDEX  TO  1.                 GA2MPGM 
01072      MOVE '2060'  TO  WS-PARA-ID.                                 GA2MPGM 
01073  2060-MERGE-IN-NEW-ENTRIES.                                       GA2MPGM 
01074      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2MPGM 
01075         SET GAJ-INDEX  DOWN BY  1                                 GA2MPGM 
01076         SET GAJ-ENTRY-COUNT  TO  GAJ-INDEX                        GA2MPGM 
01077         MOVE GAJ-ENTRY-COUNT  TO  GAJ-ENTRY-COUNT                 GA2MPGM 
01078         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2MPGM 
01079                                                                   GA2MPGM 
01080      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2MPGM 
01081             =  HIGH-VALUES  AND                                   GA2MPGM 
01082         COPY-OF-TABLE (COPY-IDX)  NOT  =  HIGH-VALUES             GA2MPGM 
01083         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2MPGM 
01084                                                                   GA2MPGM 
01085      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2MPGM 
01086             NOT  =  HIGH-VALUES  AND                              GA2MPGM 
01087         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2MPGM 
01088         GO TO 2080-INSERT-NEW-ENTRY.                              GA2MPGM 
01089                                                                   GA2MPGM 
01090      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2MPGM 
01091             =  HIGH-VALUES  AND                                   GA2MPGM 
01092         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2MPGM 
01093         NEXT SENTENCE                                             GA2MPGM 
01094      ELSE                                                         GA2MPGM 
01095         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2MPGM 
01096            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA2MPGM 
01097            GO TO 2070-SAVE-COPIED-ENTRY                           GA2MPGM 
01098         ELSE                                                      GA2MPGM 
01099            IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <               GA2MPGM 
01100               COPY-PROCEDURE-ARGUMENT (COPY-IDX)                  GA2MPGM 
01101               GO TO 2080-INSERT-NEW-ENTRY.                        GA2MPGM 
01102                                                                   GA2MPGM 
01103      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2MPGM 
01104             =  HIGH-VALUES  AND                                   GA2MPGM 
01105         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2MPGM 
01106         NEXT SENTENCE                                             GA2MPGM 
01107      ELSE                                                         GA2MPGM 
01108         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2MPGM 
01109            COPY-CODE-FUNCTION (COPY-IDX)                          GA2MPGM 
01110            GO TO 2070-SAVE-COPIED-ENTRY                           GA2MPGM 
01111         ELSE                                                      GA2MPGM 
01112            IF WS-CODE-FUNCTION (WS-SORT-IDX) <                    GA2MPGM 
01113               COPY-CODE-FUNCTION (COPY-IDX)                       GA2MPGM 
01114               GO TO 2080-INSERT-NEW-ENTRY.                        GA2MPGM 
01115                                                                   GA2MPGM 
01116 ****************************************************************  GA2MPGM 
01117 **   AT THIS POINT THE NEW ENTRY'S THREE FIELDS MUST BE EQUAL TO  GA2MPGM 
01118 **   THE OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING  GA2MPGM 
01119 **   THE INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE   GA2MPGM 
01120 **   ENTRY FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.  GA2MPGM 
01121 ****************************************************************  GA2MPGM 
01122                                                                   GA2MPGM 
01123      SET WS-SORT-IDX  UP BY  1.                                   GA2MPGM 
01124                                                                   GA2MPGM 
01125  2070-SAVE-COPIED-ENTRY.                                          GA2MPGM 
01126      MOVE '2070'  TO  WS-PARA-ID.                                 GA2MPGM 
01127      MOVE COPY-OF-TABLE (COPY-IDX)  TO                            GA2MPGM 
01128         GAJ-ENTRY (GAJ-INDEX).                                    GA2MPGM 
01129                                                                   GA2MPGM 
01130      IF COPY-IDX  NOT >  GAJ-ENTRY-COUNT                          GA2MPGM 
01131         SET COPY-IDX  UP BY  1                                    GA2MPGM 
01132         SET GAJ-INDEX  UP BY  1                                   GA2MPGM 
01133         MOVE '2060'  TO  WS-PARA-ID                               GA2MPGM 
01134         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2MPGM 
01135      ELSE                                                         GA2MPGM 
01136         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2MPGM 
01137 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2MPGM 
01138         MOVE '2ML2'  TO  WS-ABEND-CODE                            GA2MPGM 
01139         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01140                                                                   GA2MPGM 
01141  2080-INSERT-NEW-ENTRY.                                           GA2MPGM 
01142      MOVE '2080'  TO  WS-PARA-ID.                                 GA2MPGM 
01143                                                                   GA2MPGM 
01144      MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                          GA2MPGM 
01145         GAJ-ENTRY (GAJ-INDEX).                                    GA2MPGM 
01146                                                                   GA2MPGM 
01147      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2MPGM 
01148         SET WS-SORT-IDX  UP BY  1                                 GA2MPGM 
01149         SET GAJ-INDEX  UP BY  1                                   GA2MPGM 
01150         MOVE '2060'  TO  WS-PARA-ID                               GA2MPGM 
01151         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2MPGM 
01152      ELSE                                                         GA2MPGM 
01153         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2MPGM 
01154 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2MPGM 
01155         MOVE '2ML3'  TO  WS-ABEND-CODE                            GA2MPGM 
01156         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01157                                                                   GA2MPGM 
01158  2090-UPDATE-ALL-LVL-TAB-REC.                                     GA2MPGM 
01159      MOVE '2090'  TO  WS-PARA-ID.                                 GA2MPGM 
01160                                                                   GA2MPGM 
01161 *--SET INDICATOR TO CAPTURE OPERATOR-ID.                          GA2MPGM 
01162                                                                   GA2MPGM 
01163      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2MPGM 
01164      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2MPGM 
01165                                                                   GA2MPGM 
01166      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN        +GA2MPGM 
01167                     GC-GCTABULR-ACOS-FIXED-LEN +                  GA2MPGM 
01168              (GAJ-ENTRY-COUNT  *  GC-GCTABULR-ACOS-VARY-LEN).     GA2MPGM 
01169                                                                   GA2MPGM 
01170      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2MPGM 
01171            GC-GCIOPARM-LEN      +  GCIO-RECORD-LENGTH.            GA2MPGM 
01172                                                                   GA2MPGM 
01173      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2MPGM 
01174         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2MPGM 
01175         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2MPGM 
01176                                                                   GA2MPGM 
01177      IF NOT GCIO-GOOD-RETURN                                      GA2MPGM 
01178         MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASGA2MPGM 
01179 -    'E CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE          GA2MPGM 
01180         MOVE '2MF2'  TO  WS-ABEND-CODE                            GA2MPGM 
01181         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01182                                                                   GA2MPGM 
01183      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2MPGM 
01184         VARYING MAP-IDX  FROM 1  BY  1                            GA2MPGM 
01185            UNTIL  MAP-IDX  >  WS-MAP-ROW.                         GA2MPGM 
01186                                                                   GA2MPGM 
01187 ******************************************************************GA2MPGM 
01188 ** THE SAME BMS MAP IS BEING USED FOR THE ADD SCREEN AND FOR    **GA2MPGM 
01189 ** THE INQUIRY SCREEN.  THE ADD SCREEN IS INPUT-ONLY AND        **GA2MPGM 
01190 ** PFKEY PAGING IS AN OUTPUT FUNCTION, SO ALL MESSAGES AND      **GA2MPGM 
01191 ** FIELDS HAVING TO DO WITH PAGING ARE SUPPRESSED ON THE        **GA2MPGM 
01192 ** ADD SCREEN, AND THE CURSOR IS SET TO THE FIRST INPUT FIELD.  **GA2MPGM 
01193 ******************************************************************GA2MPGM 
01194      MOVE DFHBMASD TO MAP-SELECT-TXT1-ATTR                        GA2MPGM 
01195                       MAP-SELECT-ATTR                             GA2MPGM 
01196                       MAP-SELECT-FROM-ATTR                        GA2MPGM 
01197                       MAP-SELECT-TXT2-ATTR                        GA2MPGM 
01198                       MAP-SELECT-TO-ATTR                          GA2MPGM 
01199                       MAP-SELECT-TXT3-ATTR                        GA2MPGM 
01200                       MAP-SELECT-OF-ATTR                          GA2MPGM 
01201                       MAP-SELECT-TXT4-ATTR                        GA2MPGM 
01202                       MAP-SELECT-TXT5-ATTR.                       GA2MPGM 
01203                                                                   GA2MPGM 
01204      MOVE -1 TO MAP-PROCEDURE-ARGUMENT-LEN (1).                   GA2MPGM 
01205                                                                   GA2MPGM 
01206      EXEC CICS SEND   MAP('GA2MI01') MAPSET('GA2MSET') ERASE      GA2MPGM 
01207         FROM(GA2MI01O) CURSOR END-EXEC.                           GA2MPGM 
01208                                                                   GA2MPGM 
01209  2099-EXIT.   EXIT.                                               GA2MPGM 
01210 /                                                                 GA2MPGM 
01211 ******************************************************************GA2MPGM 
01212 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2MPGM 
01213 **                                                                GA2MPGM 
01214 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2MPGM 
01215 **  ALREADY ON THE OPERATORS SCREEN.                              GA2MPGM 
01216 **                                                                GA2MPGM 
01217 ******************************************************************GA2MPGM 
01218  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2MPGM 
01219                                                                   GA2MPGM 
01220      MOVE LOW-VALUES  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)        GA2MPGM 
01221                           MAP-CODE-FUNCTION (MAP-IDX).            GA2MPGM 
01222                                                                   GA2MPGM 
01223  2199-EXIT.   EXIT.                                               GA2MPGM 
01224 /                                                                 GA2MPGM 
01225 ******************************************************************GA2MPGM 
01226 **          X C T L   T O   D E L   S C R E E N                   GA2MPGM 
01227 **                                                                GA2MPGM 
01228 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2MPGM 
01229 ** DELETING ENTRIES.  WE READ THE ALL LEVEL TABULAR RECORD & PASS GA2MPGM 
01230 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, & ALL LEVEL TABULARGA2MPGM 
01231 ** RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU THE      GA2MPGM 
01232 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA2MPGM 
01233 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2MPGM 
01234 ******************************************************************GA2MPGM 
01235  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2MPGM 
01236      MOVE '3000'  TO  WS-PARA-ID.                                 GA2MPGM 
01237                                                                   GA2MPGM 
01238      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2MPGM 
01239            GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +   GA2MPGM 
01240            GC-GCTABULR-ACOS-FIXED-LEN +                           GA2MPGM 
01241           (GC-GCTABULR-ACOS-VARY-LEN    *                         GA2MPGM 
01242           GC-GCTABULR-ACOS-VARY-MAX-OCUR).                        GA2MPGM 
01243                                                                   GA2MPGM 
01244 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA2MPGM 
01245      EXEC CICS GETMAIN                                            GA2MPGM 
01246         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2MPGM 
01247         INITIMG(WS-HEX-00)                                        GA2MPGM 
01248         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2MPGM 
01249 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2MPGM 
01250 ***  ADD ALL-LEVEL-TAB-PNTR, 4096 GIVING  ALL-LEVEL-TAB-PNTR2.    GA2MPGM 
01251                                                                   GA2MPGM 
01252      MOVE LOW-VALUES  TO  GCIO-WORKFILE-KEY.                      GA2MPGM 
01253                                                                   GA2MPGM 
01254 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA2MPGM 
01255 *    EXEC CICS GETMAIN                                            GA2MPGM 
01256 *       SET(ADDRESS OF GCA-COMMAREA)                              GA2MPGM 
01257 *       INITIMG(WS-HEX-00)                                        GA2MPGM 
01258 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA2MPGM 
01259 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2MPGM 
01260                                                                   GA2MPGM 
01261      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA2MPGM 
01262         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2MPGM 
01263         MOVE  'G'   TO  GCIO-WRK-STATUS-CODE                      GA2MPGM 
01264         MOVE  'G3'  TO  GCIO-WRK-RECORD-TYPE                      GA2MPGM 
01265         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA2MPGM 
01266         MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM          GA2MPGM 
01267         MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM        GA2MPGM 
01268         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA2MPGM 
01269         MOVE SPACES  TO  GCA-L-O-B                                GA2MPGM 
01270                          GCA-PROV-CTL                             GA2MPGM 
01271                          GCA-BEN-PROV-ID                          GA2MPGM 
01272                          GCIO-WRK-LINE-OF-BUS                     GA2MPGM 
01273                          GCIO-WRK-PROVIDER-CONTROL                GA2MPGM 
01274         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2MPGM 
01275         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2MPGM 
01276         MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID               GA2MPGM 
01277                            GCIO-WRK-PROVISION-ID                  GA2MPGM 
01278         MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT           GA2MPGM 
01279                            GCIO-WRK-PROVISION-SLOT-NO             GA2MPGM 
01280         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2MPGM 
01281         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2MPGM 
01282                                                                   GA2MPGM 
01283      IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA2MPGM 
01284         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2MPGM 
01285         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2MPGM 
01286         MOVE  'C3'  TO  GCIO-WRK-RECORD-TYPE                      GA2MPGM 
01287         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2MPGM 
01288         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2MPGM 
01289         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2MPGM 
01290         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2MPGM 
01291         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2MPGM 
01292         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2MPGM 
01293         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2MPGM 
01294         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2MPGM 
01295         MOVE SPACES                TO GCA-BEN-PROV-ID             GA2MPGM 
01296         MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID               GA2MPGM 
01297                            GCIO-WRK-PROVISION-ID                  GA2MPGM 
01298         MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT           GA2MPGM 
01299                            GCIO-WRK-PROVISION-SLOT-NO             GA2MPGM 
01300         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2MPGM 
01301         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2MPGM 
01302                                                                   GA2MPGM 
01303      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA2MPGM 
01304         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2MPGM 
01305         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2MPGM 
01306         MOVE  'C5'  TO  GCIO-WRK-RECORD-TYPE                      GA2MPGM 
01307         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2MPGM 
01308         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2MPGM 
01309         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2MPGM 
01310         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2MPGM 
01311         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2MPGM 
01312         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2MPGM 
01313         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2MPGM 
01314         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2MPGM 
01315         MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID       GA2MPGM 
01316         MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID               GA2MPGM 
01317                            GCIO-WRK-TAB-PROVISION-ID              GA2MPGM 
01318         MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT           GA2MPGM 
01319                            GCIO-WRK-TAB-PROV-SLOT-NO              GA2MPGM 
01320         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.             GA2MPGM 
01321 *                                                                 GA2MPGM 
01322 *    MOVE WS-Y  TO  WS-YY.                                        GA2MPGM 
01323 *    IF  WS-M  >  2                                               GA2MPGM 
01324 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2MPGM 
01325 *          REMAINDER  WS-REMAINDER                                GA2MPGM 
01326 *    ELSE                                                         GA2MPGM 
01327 *       MOVE 1  TO  WS-REMAINDER.                                 GA2MPGM 
01328 *    SET WS-M-IDX  TO  WS-M.                                      GA2MPGM 
01329 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2MPGM 
01330 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2MPGM 
01331 *    IF  WS-REMAINDER  =  ZERO                                    GA2MPGM 
01332 *       ADD 1  TO  WS-DDD.                                        GA2MPGM 
01333 *                                                                 GA2MPGM 
01334 *    MOVE  WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE                  GA2MPGM 
01335 *                        GCA-EFF-DT.                              GA2MPGM 
01336      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA2MPGM 
01337      MOVE  SPACES  TO  GCA-INTERNAL-TAB-ID,                       GA2MPGM 
01338                        GCA-INTERNAL-TAB-SLOT,                     GA2MPGM 
01339                        GCA-ADD-DEL-IND,                           GA2MPGM 
01340                        GCA-ALL-LEVEL-TAB-FUNC-CODE,               GA2MPGM 
01341                        GCA-OCCURS-ENTRY-COUNTER.                  GA2MPGM 
01342      MOVE  MAP-FROM-MENU-ID TO GCA-FROM-MENU-ID.                  GA2MPGM 
01343      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2MPGM 
01344 ***  MOVE  ALL-LEVEL-TAB-PNTR TO GCA-RECORD-POINTER.              GA2MPGM 
01345                                                                   GA2MPGM 
01346      SET GCA-RECORD-POINTER TO ADDRESS                            GA2MPGM 
01347      OF  IO-PARM-ALL-LVL-TAB-RECORD.                              GA2MPGM 
01348                                                                   GA2MPGM 
01349      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2MPGM 
01350      TO   GAJ-ENTRY-COUNT.                                        GA2MPGM 
01351                                                                   GA2MPGM 
01352      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2MPGM 
01353                                                                   GA2MPGM 
01354      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2MPGM 
01355         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2MPGM 
01356         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2MPGM 
01357                                                                   GA2MPGM 
01358      IF  NOT GCIO-GOOD-RETURN                                     GA2MPGM 
01359         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2MPGM 
01360 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2MPGM 
01361         MOVE '2MF3'  TO  WS-ABEND-CODE                            GA2MPGM 
01362         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01363                                                                   GA2MPGM 
01364 *    SET COMMAREA-PNTR TO ADDRESS                                 GA2MPGM 
01365 *    OF  GCA-COMMAREA.                                            GA2MPGM 
01366 *                                                                 GA2MPGM 
01367 *    EXEC CICS XCTL  PROGRAM('GA1MPGM') COMMAREA(COMMAREA-PNTR)   GA2MPGM 
01368 *       LENGTH(4)  END-EXEC.                                      GA2MPGM 
01369                                                                   GA2MPGM 
01370      EXEC CICS XCTL  PROGRAM('GA1MPGM')                           GA2MPGM 
01371                      COMMAREA(DFHCOMMAREA)                        GA2MPGM 
01372                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2MPGM 
01373      END-EXEC.                                                    GA2MPGM 
01374                                                                   GA2MPGM 
01375  3099-EXIT.   EXIT.                                               GA2MPGM 
01376 /                                                                 GA2MPGM 
01377 ***************************************************************** GA2MPGM 
01378 **          D I S P L A Y   F I R S T   S C R E E N               GA2MPGM 
01379 **                                                                GA2MPGM 
01380 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE DELETE GA2MPGM 
01381 ** PROGRAM, THAT PROGRAM WILL PASS THE ADDRESS OF A PARAMETER LISTGA2MPGM 
01382 ** CONTAINING THE FIELDS FROM THE HEADER OF THE SCREEN.           GA2MPGM 
01383 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2MPGM 
01384 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2MPGM 
01385 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2MPGM 
01386 ******************************************************************GA2MPGM 
01387  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2MPGM 
01388      MOVE '4000'  TO  WS-PARA-ID.                                 GA2MPGM 
01389                                                                   GA2MPGM 
01390 ***  MOVE LOW VALUES TO SCREEN                                    GA2MPGM 
01391                                                                   GA2MPGM 
01392      MOVE LOW-VALUES TO GA2MI01I.                                 GA2MPGM 
01393                                                                   GA2MPGM 
01394      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2MPGM 
01395         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2MPGM 
01396            TO MAP-ERROR-MESSAGE                                   GA2MPGM 
01397         MOVE '2MC1'  TO  WS-ABEND-CODE                            GA2MPGM 
01398         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01399                                                                   GA2MPGM 
01400 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA2MPGM 
01401 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2MPGM 
01402                                                                   GA2MPGM 
01403 *    SET ADDRESS OF GCA-COMMAREA                                  GA2MPGM 
01404 *    TO  INCOMING-COMMAREA-PNTR.                                  GA2MPGM 
01405                                                                   GA2MPGM 
01406      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-TABULAR-ID.               GA2MPGM 
01407      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-TABULAR-SLOT.           GA2MPGM 
01408      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA2MPGM 
01409                                                                   GA2MPGM 
01410      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2MPGM 
01411         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-MAIN-TITLE        GA2MPGM 
01412         MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2MPGM 
01413         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA2MPGM 
01414         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA2MPGM 
01415         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2MPGM 
01416         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA2MPGM 
01417         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2MPGM 
01418         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2MPGM 
01419         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2MPGM 
01420         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2MPGM 
01421                                                                   GA2MPGM 
01422      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2MPGM 
01423         MOVE CONTRACT-TITLE-LINE  TO  MAP-MAIN-TITLE              GA2MPGM 
01424         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2MPGM 
01425         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA2MPGM 
01426         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA2MPGM 
01427         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2MPGM 
01428         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA2MPGM 
01429         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2MPGM 
01430         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2MPGM 
01431         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2MPGM 
01432         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2MPGM 
01433         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2MPGM 
01434         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2MPGM 
01435         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2MPGM 
01436         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2MPGM 
01437                                                                   GA2MPGM 
01438      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2MPGM 
01439         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-MAIN-TITLE     GA2MPGM 
01440         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA2MPGM 
01441         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA2MPGM 
01442         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA2MPGM 
01443         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA2MPGM 
01444         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA2MPGM 
01445         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2MPGM 
01446         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA2MPGM 
01447         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2MPGM 
01448         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA2MPGM 
01449         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2MPGM 
01450         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA2MPGM 
01451         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2MPGM 
01452         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA2MPGM 
01453         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2MPGM 
01454                                                                   GA2MPGM 
01455 ******************************************************************GA2MPGM 
01456 ** THE SAME BMS MAP IS BEING USED FOR THE ADD SCREEN AND FOR    **GA2MPGM 
01457 ** THE INQUIRY SCREEN.  THE ADD SCREEN IS INPUT-ONLY AND        **GA2MPGM 
01458 ** PFKEY PAGING IS AN OUTPUT FUNCTION, SO ALL MESSAGES AND      **GA2MPGM 
01459 ** FIELDS HAVING TO DO WITH PAGING ARE SUPPRESSED ON THE        **GA2MPGM 
01460 ** ADD SCREEN, AND THE CURSOR IS SET TO THE FIRST INPUT FIELD.  **GA2MPGM 
01461 ******************************************************************GA2MPGM 
01462      MOVE DFHBMASD TO MAP-SELECT-TXT1-ATTR                        GA2MPGM 
01463                       MAP-SELECT-ATTR                             GA2MPGM 
01464                       MAP-SELECT-FROM-ATTR                        GA2MPGM 
01465                       MAP-SELECT-TXT2-ATTR                        GA2MPGM 
01466                       MAP-SELECT-TO-ATTR                          GA2MPGM 
01467                       MAP-SELECT-TXT3-ATTR                        GA2MPGM 
01468                       MAP-SELECT-OF-ATTR                          GA2MPGM 
01469                       MAP-SELECT-TXT4-ATTR                        GA2MPGM 
01470                       MAP-SELECT-TXT5-ATTR.                       GA2MPGM 
01471                                                                   GA2MPGM 
01472      MOVE -1 TO MAP-PROCEDURE-ARGUMENT-LEN (1).                   GA2MPGM 
01473                                                                   GA2MPGM 
01474      EXEC CICS SEND   MAP('GA2MI01') MAPSET('GA2MSET') ERASE      GA2MPGM 
01475         FROM(GA2MI01O) CURSOR END-EXEC.                           GA2MPGM 
01476                                                                   GA2MPGM 
01477  4099-EXIT.   EXIT.                                               GA2MPGM 
01478 /                                                                 GA2MPGM 
01479 ***************************************************************** GA2MPGM 
01480 **        X C T L   T O   P R E V I O U S   M E N U               GA2MPGM 
01481 **                                                                GA2MPGM 
01482 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2MPGM 
01483 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD &       GA2MPGM 
01484 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA2MPGM 
01485 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM       GA2MPGM 
01486 ** MENU ID' FIELD).                                               GA2MPGM 
01487 ******************************************************************GA2MPGM 
01488  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2MPGM 
01489      MOVE '5000'  TO  WS-PARA-ID.                                 GA2MPGM 
01490                                                                   GA2MPGM 
01491      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA2MPGM 
01492         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA2MPGM 
01493                                                                   GA2MPGM 
01494      IF  MAP-FROM-MENU-ID = 'GC4A'                                GA2MPGM 
01495         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA2MPGM 
01496                                                                   GA2MPGM 
01497      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA2MPGM 
01498         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA2MPGM 
01499                                                                   GA2MPGM 
01500      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA2MPGM 
01501         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA2MPGM 
01502                                                                   GA2MPGM 
01503                                                                   GA2MPGM 
01504                                                                   GA2MPGM 
01505  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA2MPGM 
01506      MOVE '5010'  TO  WS-PARA-ID.                                 GA2MPGM 
01507                                                                   GA2MPGM 
01508      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2MPGM 
01509          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2MPGM 
01510                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2MPGM 
01511                                                                   GA2MPGM 
01512 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA2MPGM 
01513      EXEC CICS GETMAIN                                            GA2MPGM 
01514         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA2MPGM 
01515         INITIMG(WS-HEX-00)                                        GA2MPGM 
01516         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01517 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA2MPGM 
01518                                                                   GA2MPGM 
01519      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2MPGM 
01520                                                                   GA2MPGM 
01521      MOVE 'G'   TO  GCIO-WRK-STATUS-CODE.                         GA2MPGM 
01522      MOVE 'G2'  TO  GCIO-WRK-RECORD-TYPE.                         GA2MPGM 
01523      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2MPGM 
01524      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2MPGM 
01525      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GA2MPGM 
01526      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2MPGM 
01527      MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                       GA2MPGM 
01528                       GCIO-WRK-PROVIDER-CONTROL.                  GA2MPGM 
01529      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2MPGM 
01530      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2MPGM 
01531                                                                   GA2MPGM 
01532 *    MOVE WS-Y  TO  WS-YY.                                        GA2MPGM 
01533 *    IF  WS-M  >  2                                               GA2MPGM 
01534 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2MPGM 
01535 *          REMAINDER  WS-REMAINDER                                GA2MPGM 
01536 *    ELSE                                                         GA2MPGM 
01537 *       MOVE 1  TO  WS-REMAINDER.                                 GA2MPGM 
01538 *    SET WS-M-IDX  TO  WS-M.                                      GA2MPGM 
01539 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2MPGM 
01540 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2MPGM 
01541 *    IF  WS-REMAINDER  =  ZERO                                    GA2MPGM 
01542 *       ADD 1  TO  WS-DDD.                                        GA2MPGM 
01543                                                                   GA2MPGM 
01544 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2MPGM 
01545      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA2MPGM 
01546      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2MPGM 
01547                       GCIO-WRK-TAB-PROVISION-ID.                  GA2MPGM 
01548      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2MPGM 
01549                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2MPGM 
01550      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2MPGM 
01551                                                                   GA2MPGM 
01552      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA2MPGM 
01553      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA2MPGM 
01554                                                                   GA2MPGM 
01555      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      GA2MPGM 
01556                                                                   GA2MPGM 
01557      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2MPGM 
01558         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA2MPGM 
01559         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01560                                                                   GA2MPGM 
01561      IF  NOT GCIO2-GOOD-RETURN                                    GA2MPGM 
01562         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2MPGM 
01563 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2MPGM 
01564         MOVE '2MF4'  TO  WS-ABEND-CODE                            GA2MPGM 
01565         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01566                                                                   GA2MPGM 
01567      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2MPGM 
01568          GC-WORKFILE-KEY-LEN        +                             GA2MPGM 
01569                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2MPGM 
01570                                                                   GA2MPGM 
01571      EXEC CICS XCTL PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)    GA2MPGM 
01572         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01573                                                                   GA2MPGM 
01574      GO TO 5099-EXIT.                                             GA2MPGM 
01575                                                                   GA2MPGM 
01576  5020-XCTL-TO-CONTRACT-MENU.                                      GA2MPGM 
01577      MOVE '5020'  TO  WS-PARA-ID.                                 GA2MPGM 
01578                                                                   GA2MPGM 
01579      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2MPGM 
01580          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2MPGM 
01581                 GC-GCCONTR-MAX-REC-LEN.                           GA2MPGM 
01582                                                                   GA2MPGM 
01583 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA2MPGM 
01584      EXEC CICS GETMAIN                                            GA2MPGM 
01585         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA2MPGM 
01586         INITIMG(WS-HEX-00)                                        GA2MPGM 
01587         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01588 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA2MPGM 
01589 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA2MPGM 
01590                                                                   GA2MPGM 
01591      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2MPGM 
01592                                                                   GA2MPGM 
01593      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2MPGM 
01594      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA2MPGM 
01595      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2MPGM 
01596      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GA2MPGM 
01597      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GA2MPGM 
01598      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2MPGM 
01599      MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2MPGM 
01600      MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2MPGM 
01601      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2MPGM 
01602 *    MOVE CONTRACT-EFF-DATE  TO  WS-MDY.                          GA2MPGM 
01603      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2MPGM 
01604                                                                   GA2MPGM 
01605 *    MOVE WS-Y  TO  WS-YY.                                        GA2MPGM 
01606 *    IF  WS-M  >  2                                               GA2MPGM 
01607 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2MPGM 
01608 *          REMAINDER  WS-REMAINDER                                GA2MPGM 
01609 *    ELSE                                                         GA2MPGM 
01610 *       MOVE 1  TO  WS-REMAINDER.                                 GA2MPGM 
01611 *    SET WS-M-IDX  TO  WS-M.                                      GA2MPGM 
01612 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2MPGM 
01613 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2MPGM 
01614 *    IF  WS-REMAINDER  =  ZERO                                    GA2MPGM 
01615 *       ADD 1  TO  WS-DDD.                                        GA2MPGM 
01616                                                                   GA2MPGM 
01617 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2MPGM 
01618      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA2MPGM 
01619      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2MPGM 
01620                       GCIO-WRK-TAB-PROVISION-ID.                  GA2MPGM 
01621      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2MPGM 
01622                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2MPGM 
01623      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA2MPGM 
01624                                                                   GA2MPGM 
01625      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA2MPGM 
01626      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA2MPGM 
01627                                                                   GA2MPGM 
01628      MOVE 'RD '  TO  GCIO3-FILE-ACCESS-CODE.                      GA2MPGM 
01629                                                                   GA2MPGM 
01630      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2MPGM 
01631         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA2MPGM 
01632         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01633                                                                   GA2MPGM 
01634      IF  NOT GCIO3-GOOD-RETURN                                    GA2MPGM 
01635         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2MPGM 
01636 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2MPGM 
01637         MOVE '2MF5'  TO  WS-ABEND-CODE                            GA2MPGM 
01638         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01639                                                                   GA2MPGM 
01640      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2MPGM 
01641          GC-WORKFILE-KEY-LEN        +                             GA2MPGM 
01642                 GC-GCCONTR-MAX-REC-LEN.                           GA2MPGM 
01643                                                                   GA2MPGM 
01644      EXEC CICS XCTL PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)    GA2MPGM 
01645         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01646                                                                   GA2MPGM 
01647      GO TO 5099-EXIT.                                             GA2MPGM 
01648                                                                   GA2MPGM 
01649  5030-XCTL-TO-BEN-PROV-MENU.                                      GA2MPGM 
01650      MOVE '5030'  TO  WS-PARA-ID.                                 GA2MPGM 
01651                                                                   GA2MPGM 
01652      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2MPGM 
01653          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2MPGM 
01654                 GC-GCBENPRV-MAX-REC-LEN.                          GA2MPGM 
01655                                                                   GA2MPGM 
01656 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA2MPGM 
01657      EXEC CICS GETMAIN                                            GA2MPGM 
01658         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA2MPGM 
01659         INITIMG(WS-HEX-00)                                        GA2MPGM 
01660         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01661 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA2MPGM 
01662                                                                   GA2MPGM 
01663      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2MPGM 
01664                                                                   GA2MPGM 
01665      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2MPGM 
01666      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA2MPGM 
01667      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2MPGM 
01668      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GA2MPGM 
01669      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GA2MPGM 
01670      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2MPGM 
01671      MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2MPGM 
01672      MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2MPGM 
01673      MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2MPGM 
01674      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2MPGM 
01675      MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID.              GA2MPGM 
01676                                                                   GA2MPGM 
01677 *    MOVE WS-Y  TO  WS-YY.                                        GA2MPGM 
01678 *    IF  WS-M  >  2                                               GA2MPGM 
01679 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2MPGM 
01680 *          REMAINDER  WS-REMAINDER                                GA2MPGM 
01681 *    ELSE                                                         GA2MPGM 
01682 *       MOVE 1  TO  WS-REMAINDER.                                 GA2MPGM 
01683 *    SET WS-M-IDX  TO  WS-M.                                      GA2MPGM 
01684 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2MPGM 
01685 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2MPGM 
01686 *    IF  WS-REMAINDER  =  ZERO                                    GA2MPGM 
01687 *       ADD 1  TO  WS-DDD.                                        GA2MPGM 
01688                                                                   GA2MPGM 
01689 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2MPGM 
01690      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA2MPGM 
01691      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA2MPGM 
01692      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA2MPGM 
01693      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2MPGM 
01694      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA2MPGM 
01695                                                                   GA2MPGM 
01696      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA2MPGM 
01697      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA2MPGM 
01698                                                                   GA2MPGM 
01699      MOVE 'RD '  TO  GCIO4-FILE-ACCESS-CODE.                      GA2MPGM 
01700                                                                   GA2MPGM 
01701      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2MPGM 
01702         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA2MPGM 
01703         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01704                                                                   GA2MPGM 
01705      IF  NOT GCIO4-GOOD-RETURN                                    GA2MPGM 
01706         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2MPGM 
01707 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2MPGM 
01708         MOVE '2MF6'  TO  WS-ABEND-CODE                            GA2MPGM 
01709         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2MPGM 
01710                                                                   GA2MPGM 
01711      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2MPGM 
01712          GC-WORKFILE-KEY-LEN        +                             GA2MPGM 
01713                 GC-GCBENPRV-MAX-REC-LEN.                          GA2MPGM 
01714                                                                   GA2MPGM 
01715      EXEC CICS XCTL PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)    GA2MPGM 
01716         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2MPGM 
01717                                                                   GA2MPGM 
01718      GO TO 5099-EXIT.                                             GA2MPGM 
01719                                                                   GA2MPGM 
01720                                                                   GA2MPGM 
01721  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA2MPGM 
01722      MOVE '5040'  TO  WS-PARA-ID.                                 GA2MPGM 
01723                                                                   GA2MPGM 
01724      EXEC CICS XCTL                                               GA2MPGM 
01725                PROGRAM('GTM1PGM')                                 GA2MPGM 
01726                END-EXEC.                                          GA2MPGM 
01727                                                                   GA2MPGM 
01728      GO  TO  5099-EXIT.                                           GA2MPGM 
01729                                                                   GA2MPGM 
01730  5099-EXIT.                                                       GA2MPGM 
01731      EXIT.                                                        GA2MPGM 
01732 /                                                                 GA2MPGM 
01733 ***************************************************************** GA2MPGM 
01734 **           X C T L   T O   M A I N   M E N U                    GA2MPGM 
01735 **                                                                GA2MPGM 
01736 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2MPGM 
01737 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2MPGM 
01738 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2MPGM 
01739 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2MPGM 
01740 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2MPGM 
01741 ** AND PROGRESS DOWN.                                             GA2MPGM 
01742 ******************************************************************GA2MPGM 
01743  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2MPGM 
01744      MOVE '6000'  TO  WS-PARA-ID.                                 GA2MPGM 
01745      MOVE '2MP1'  TO  WS-ABEND-CODE.                              GA2MPGM 
01746                                                                   GA2MPGM 
01747      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2MPGM 
01748                                                                   GA2MPGM 
01749  6099-EXIT.     EXIT.                                             GA2MPGM 
01750 /                                                                 GA2MPGM 
01751 /     E R R O R   M E S S A G E   T H E N   A B E N D             GA2MPGM 
01752 ******************************************************************GA2MPGM 
01753  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2MPGM 
01754                                                                   GA2MPGM 
01755      SET MAP-IDX   TO  7.                                         GA2MPGM 
01756      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX).           GA2MPGM 
01757      EXEC CICS SEND   MAP('GA2MI01') MAPSET('GA2MSET') ERASE      GA2MPGM 
01758         FROM(GA2MI01O) CURSOR WAIT END-EXEC.                      GA2MPGM 
01759                                                                   GA2MPGM 
01760      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2MPGM 
01761                                                                   GA2MPGM 
01762  9999-EXIT.     EXIT.                                             GA2MPGM 
