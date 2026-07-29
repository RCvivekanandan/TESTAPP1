00001  IDENTIFICATION DIVISION.                                         01/12/06
00002  PROGRAM-ID.   GA2LPGM.                                           GA2LPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV004
00004  AUTHOR.       SANDRA BUCH.                                       GA2LPGM 
00005  DATE-WRITTEN. 02/14/85.                                          GA2LPGM 
00006  DATE-COMPILED.                                                   GA2LPGM 
00007      SKIP3                                                        GA2LPGM 
00008 ******************************************************************GA2LPGM 
00009 *                                                                *GA2LPGM 
00010 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA2LPGM 
00011 *       *-*         U P D A T E   H I S T O R Y         *-*      *GA2LPGM 
00012 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *GA2LPGM 
00013 *                                                                *GA2LPGM 
00014 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*GA2LPGM 
00015 *                                                                *GA2LPGM 
00016 *            01/21/86  ENW  CHANGED WS CONTRACT LENGTHS AND      *GA2LPGM 
00017 *                           REMOVED ALL HANDLE CONDITIONS        *GA2LPGM 
00018 *                           EXCEPT FOR MAPFAIL.                  *GA2LPGM 
00019 *                                                                *GA2LPGM 
00020 *            03/20/86  NJS  ADDED CODE TO CHECK FOR A RETURN     *GA2LPGM 
00021 *                           CODE OF '20' FROM GCVIOPGM.  THIS    *GA2LPGM 
00022 *                           IMPLIES THAT THE EDIT TABLE WAS      *GA2LPGM 
00023 *                           EMPTY AND FIELD VALIDATION COULD     *GA2LPGM 
00024 *                           NOT BE PERFORMED.  PF4/PF16 MAY      *GA2LPGM 
00025 *                           BE USED TO ACCEPT THE DATA AS        *GA2LPGM 
00026 *                           SHOWN AND CONTINUE PROCESSING.       *GA2LPGM 
00027 *                                                                *GA2LPGM 
00028 *    EL500   06/18/86  DES  FIXED PF4/16 CODE TO ACCEPT EMPTY    *GA2LPGM 
00029 *                           VALIDATION TABLE CONDITION ONLY,     *GA2LPGM 
00030 *                           ALL OTHER ERRORS STILL MUST BE FIXED *GA2LPGM 
00031 *                                                                *GA2LPGM 
00032 *    D136/   08/25/86  AMJ  USE USER-DEFINED LOGICAL MAP FOR     *GA2LPGM 
00033 *    D137                   SCREEN.  THIS REPLACES THE PARTIAL   *GA2LPGM 
00034 *                           USE OF BMS MAP AND USER-DEFINED.     *GA2LPGM 
00035 *                                                                *GA2LPGM 
00036 *    D0120   01/30/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT   *GA2LPGM 
00037 *                           EXECUTED FROM TRANSACTION GTM1:      *GA2LPGM 
00038 *                           1. PF1/PF13 - CONSTRUCT COMMAREA AS  *GA2LPGM 
00039 *                              IF GC4A HAD CALLED, XCTL TO ADD   *GA2LPGM 
00040 *                              SCREEN PROGRAM.                   *GA2LPGM 
00041 *                           2. PF3/PF15 - CONSTRUCT COMMAREA AS  *GA2LPGM 
00042 *                              IF GC4A HAD CALLED, XCTL TO       *GA2LPGM 
00043 *                              GTM1PGM.                          *GA2LPGM 
00044 *                                                                *GA2LPGM 
00045 *    D116     8/17/87  FRY   CAPTURE OPERATOR-ID WHEN A 'C3',    *GA2LPGM 
00046 *                            'C5', OR 'G3' RECORD IS UPDATED.    *GA2LPGM 
00047 *                                                               * GA2LPGM 
00048 * D1013 10/07/87  FCG  REMOVE LINK TO CSEXECIO AND REPLACE WITH * GA2LPGM 
00049 *                      LINK TO GCPPDIO. REPLACED LOGIC CODE     * GA2LPGM 
00050 *                      TO PROCESS WITH NEW INTERFACE PROGRAM.   * GA2LPGM 
00051 *                                                                *GA2LPGM 
00052 *                                                                *GA2LPGM 
00053 *  11161  10/24/90  ENW   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA2LPGM 
00054 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA2LPGM 
00055 *                         ROUTINES. REMOVED HARD CODED LENGTHS   *GA2LPGM 
00056 *                                                                *GA2LPGM 
00057 *                                                                *GA2LPGM 
00058 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD      *GA2LPGM 
00059 *                           FROM ONE POSITION TO TWO POSITIONS.  *GA2LPGM 
00060 *                                                                *GA2LPGM 
00061 * D12009  09/27/91  GDM   CONVERT TO COBOL II                    *GA2LPGM 
00062 *                                                                *GA2LPGM 
00063 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA2LPGM 
00064 *                                                                *GA2LPGM 
00065 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO*GA2LPGM 
00066 *                             7 BYTES. CHANGE # OF OCCURS TO 396 *GA2LPGM 
00067 *                             ON #ADIP TABULAR.                  *GA2LPGM 
00068 *                                                                *GA2LPGM 
00069 *            10-19-04   NB    RECOMPILE NEW GCPPDIOC COPYBOOK    *GA2LPGM 
00070 *                                                                *GA2LPGM 
00070 *ICD-10  07/06/11  BA  EXPAND MAP-SELECT FIELD FROM 6 TO 7 BYTES.*GA2LPGM 
      *                      EXPAND PROCED-CODE FROM 5 TO 7 BYTES.     *        
      *                      CHANGE LOGIC FOR ICD-10 REQUIREMENTS.     *        
00071 ******************************************************************GA2LPGM 
00072 ******************************************************************GA2LPGM 
00073 ******************************************************************GA2LPGM 
00074 *   GA2LPGM   ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM    GA2LPGM 
00075 *                          CONGENITAL DEFECT            GA2L      GA2LPGM 
00076 *                                                                 GA2LPGM 
00077 *     THIS PROGRAM WILL ADD ENTRIES TO THE DENTAL INPATIENT       GA2LPGM 
00078 *   PROCEDURE ARGUMENT/CODE FUNCTION OF ALL LEVEL TABULAR RECORD. GA2LPGM 
00079 *                                                                 GA2LPGM 
00080 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2LPGM 
00081 *   TO ADD ENTRIES TO THIS PARTICULAR ALL LEVEL TABULAR RECORD.   GA2LPGM 
00082 *   THE PROGRAM READS THE ENTRIES, & VALIDATES THE FORMAT OF EACH GA2LPGM 
00083 *   FIELD IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN   GA2LPGM 
00084 *   ERROR).  IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES GA2LPGM 
00085 *   IN ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER GA2LPGM 
00086 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2LPGM 
00087 *   ENTRIES FOR THIS ALL LEVEL TABULAR RECORD.                    GA2LPGM 
00088 *                                                                 GA2LPGM 
00089 *   TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID:#ACON)  GA2LPGM 
00090 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2LPGM 
00091 *   XCTL TO TRANS GA1L OR PROGRAM GA1LPGM.  THIS PROGRAM WILL     GA2LPGM 
00092 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2LPGM 
00093 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2LPGM 
00094 *   CODE.                                                         GA2LPGM 
00095 *                                                                 GA2LPGM 
00096 *   PF7/PF19  PAGE BACKWARD.                                      GA2LPGM 
00097 *   PF8/PF20  PAGE FORWARD.                                       GA2LPGM 
00098 *   PF10/PF22 PAGE TO BOTTOM.                                     GA2LPGM 
00099 *   PF11/PF23 PAGE TO TOP.                                        GA2LPGM 
00100 *                                                                 GA2LPGM 
00101 *   FUNC CODE: GA2L                                               GA2LPGM 
00102 *   MAPSET:    GA2LSETC <<<< REDEFINED BY USER DEFINED MAP >>>>   GA2LPGM 
00103 *   FILES:     GCPSWORK                                           GA2LPGM 
00104 *                                                                 GA2LPGM 
00105 ******************************************************************GA2LPGM 
00106      SKIP3                                                        GA2LPGM 
00107  ENVIRONMENT DIVISION.                                            GA2LPGM 
00108 /                                                                 GA2LPGM 
00109  DATA DIVISION.                                                   GA2LPGM 
00110  WORKING-STORAGE SECTION.                                         GA2LPGM 
00111  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2LPGM 
00112      '***GA2LPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2LPGM 
00113  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2LPGM 
00114                                                                   GA2LPGM 
00115  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2LPGM 
00116                                                                   GA2LPGM 
00117 ******************************************************************GA2LPGM 
00118 ** THE FIELDS LISTED BELOW ARE USED WHEN CALLING THE PROCEDURE  **GA2LPGM 
00119 ** OR DIAGNOSIS HCSC FILES.                                     **GA2LPGM 
00120 ******************************************************************GA2LPGM 
00121  01  PROCED-KEY.                                                  GA2LPGM 
00122      03  SYSTEM-INDICATOR        PIC X(1) VALUE SPACE.            GA2LPGM 
00123 *** ICD-10 START                                                  GA2LPGM 
00124      03  PROCED-CODE             PIC X(7).                        GA2LPGM 
00125      03  PROCEDR-DIGIT REDEFINES PROCED-CODE                      GA2LPGM 
00126              OCCURS 7 TIMES      PIC X(1).                        GA2LPGM 
00126 *** ICD-10 END                                                    GA2LPGM 
00126                                                                   GA2LPGM 
00127  01  WS-PRO-HAF-COMM-LEN         PIC S9(4)  COMP  VALUE +344.     GA2LPGM 
00128                                                                   GA2LPGM 
00129 ** MAP COBOL SCREEN DSECTS **                                     GA2LPGM 
00130  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2LPGM 
00131      '***  I/O MAPAREA ***'.                                      GA2LPGM 
00132  COPY GA2LSETC.                                                   GA2LPGM 
00133 /*****************************************************************GA2LPGM 
00134 ******************************************************************GA2LPGM 
00135 ******************************************************************GA2LPGM 
00136 **                                                              **GA2LPGM 
00137 **     THIS IS A USER-DEFINED LOGICAL MAP.  ANY CHANGES TO      **GA2LPGM 
00138 **     MAPSET GA2LSETC AFFECTING ITS LENGTH MUST BE TAKEN       **GA2LPGM 
00139 **     INTO ACCOUNT HERE.                                       **GA2LPGM 
00140 **                                                              **GA2LPGM 
00141 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA2LPGM 
00142 **                                                              **GA2LPGM 
00143 **                                            AMJ 8/26/86       **GA2LPGM 
00144 **                                                              **GA2LPGM 
00145 ******************************************************************GA2LPGM 
00146 ******************************************************************GA2LPGM 
00147 ******************************************************************GA2LPGM 
00148                                                                   GA2LPGM 
00149  01  MAP-USER-DEFINED REDEFINES GA2LI01I.                         GA2LPGM 
00150                                                                   GA2LPGM 
00151      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA2LPGM 
00152                                                                   GA2LPGM 
00153      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA2LPGM 
00154      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA2LPGM 
00155      05  MAP-FUNCTION-CODE                PIC X(04).              GA2LPGM 
00156                                                                   GA2LPGM 
00157      05  MAP-MAIN-TITLE-LEN               PIC S9(4) COMP SYNC.    GA2LPGM 
00158      05  MAP-MAIN-TITLE-ATTR              PIC X.                  GA2LPGM 
00159      05  MAP-MAIN-TITLE                   PIC X(43).              GA2LPGM 
00160                                                                   GA2LPGM 
00161      05  MAP-ADD-INQ-LEN                  PIC S9(4) COMP SYNC.    GA2LPGM 
00162      05  MAP-ADD-INQ-ATTR                 PIC X.                  GA2LPGM 
00163      05  MAP-ADD-INQ                      PIC X(03).              GA2LPGM 
00164                                                                   GA2LPGM 
00165      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA2LPGM 
00166      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA2LPGM 
00167      05  MAP-SCREEN-ID                    PIC X(06).              GA2LPGM 
00168                                                                   GA2LPGM 
00169      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA2LPGM 
00170      05  MAP-ID-LINE-ATTR                 PIC X.                  GA2LPGM 
00171      05  MAP-ID-LINE                      PIC X(79).              GA2LPGM 
00172                                                                   GA2LPGM 
00173      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA2LPGM 
00174          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA2LPGM 
00175          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA2LPGM 
00176          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA2LPGM 
00177          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA2LPGM 
00178          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA2LPGM 
00179          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA2LPGM 
00180          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA2LPGM 
00181          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA2LPGM 
00182          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA2LPGM 
00183          10  FILLER                           PIC X(18).          GA2LPGM 
00184      05  CONTRACT-ID-LINE REDEFINES MAP-ID-LINE.                  GA2LPGM 
00185          10  CONTRACT-ID-HEADING              PIC X(14).          GA2LPGM 
00186          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA2LPGM 
00187          10  CONTRACT-GROUP-NO                PIC X(6).           GA2LPGM 
00188          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA2LPGM 
00189          10  CONTRACT-SECTION-NO              PIC X(4).           GA2LPGM 
00190          10  CONTRACT-LOB-HEADING             PIC X(6).           GA2LPGM 
00191          10  CONTRACT-LOB                     PIC X.              GA2LPGM 
00192          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA2LPGM 
00193          10  CONTRACT-PROV-CTL                PIC XX.             GA2LPGM 
00194          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA2LPGM 
00195          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA2LPGM 
00196          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA2LPGM 
00197          10  CONTRACT-EFF-DATE                PIC X(6).           GA2LPGM 
00198          10  FILLER                           PIC X(09).          GA2LPGM 
00199      05  BENEFIT-PROVISION-ID-LINE REDEFINES MAP-ID-LINE.         GA2LPGM 
00200          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA2LPGM 
00201          10  BEN-PROV-GROUP-NO                PIC X(6).           GA2LPGM 
00202          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA2LPGM 
00203          10  BEN-PROV-SECTION-NO              PIC X(4).           GA2LPGM 
00204          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA2LPGM 
00205          10  BEN-PROV-LOB                     PIC X.              GA2LPGM 
00206          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA2LPGM 
00207          10  BEN-PROV-PROV-CTL                PIC XX.             GA2LPGM 
00208          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA2LPGM 
00209          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA2LPGM 
00210          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA2LPGM 
00211          10  BEN-PROV-EFF-DATE                PIC X(6).           GA2LPGM 
00212          10  BEN-PROV-ID-HEADING              PIC X(8).           GA2LPGM 
00213          10  BEN-PROV-ID-NO                   PIC X(6).           GA2LPGM 
00214          10  FILLER                           PIC X(09).          GA2LPGM 
00215                                                                   GA2LPGM 
00216      05  MAP-TABULAR-ID-LEN               PIC S9(4) COMP SYNC.    GA2LPGM 
00217      05  MAP-TABULAR-ID-ATTR              PIC X.                  GA2LPGM 
00218      05  MAP-TABULAR-ID                   PIC X(06).              GA2LPGM 
00219                                                                   GA2LPGM 
00220      05  MAP-TABULAR-SLOT-LEN             PIC S9(4) COMP SYNC.    GA2LPGM 
00221      05  MAP-TABULAR-SLOT-ATTR            PIC X.                  GA2LPGM 
00222      05  MAP-TABULAR-SLOT                 PIC X(07).              GA2LPGM 
00223                                                                   GA2LPGM 
00224      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA2LPGM 
00225      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA2LPGM 
00226      05  MAP-FROM-MENU-ID                 PIC X(04).              GA2LPGM 
00227                                                                   GA2LPGM 
00228      05  MAP-SELECT-TXT1-LEN              PIC S9(4) COMP SYNC.    GA2LPGM 
00229      05  MAP-SELECT-TXT1-ATTR             PIC X.                  GA2LPGM 
00230      05  MAP-SELECT-TXT1                  PIC X(07).              GA2LPGM 
00231                                                                   GA2LPGM 
00232      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA2LPGM 
00233      05  MAP-SELECT-ATTR                  PIC X.                  GA2LPGM 
00234      05  MAP-SELECT                       PIC X(07).              GA2LPGM 
00235                                                                   GA2LPGM 
00236      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA2LPGM 
00237      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA2LPGM 
00238      05  MAP-SELECT-FROM                  PIC X(03).              GA2LPGM 
00239                                                                   GA2LPGM 
00240      05  MAP-SELECT-TXT2-LEN              PIC S9(4) COMP SYNC.    GA2LPGM 
00241      05  MAP-SELECT-TXT2-ATTR             PIC X.                  GA2LPGM 
00242      05  MAP-SELECT-TXT2                  PIC X(02).              GA2LPGM 
00243                                                                   GA2LPGM 
00244      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA2LPGM 
00245      05  MAP-SELECT-TO-ATTR               PIC X.                  GA2LPGM 
00246      05  MAP-SELECT-TO                    PIC X(03).              GA2LPGM 
00247                                                                   GA2LPGM 
00248      05  MAP-SELECT-TXT3-LEN              PIC S9(4) COMP SYNC.    GA2LPGM 
00249      05  MAP-SELECT-TXT3-ATTR             PIC X.                  GA2LPGM 
00250      05  MAP-SELECT-TXT3                  PIC X(02).              GA2LPGM 
00251                                                                   GA2LPGM 
00252      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA2LPGM 
00253      05  MAP-SELECT-OF-ATTR               PIC X.                  GA2LPGM 
00254      05  MAP-SELECT-OF                    PIC X(03).              GA2LPGM 
00255                                                                   GA2LPGM 
00256      05  MAP-SELECT-TXT4-LEN              PIC S9(4) COMP SYNC.    GA2LPGM 
00257      05  MAP-SELECT-TXT4-ATTR             PIC X.                  GA2LPGM 
00258      05  MAP-SELECT-TXT4                  PIC X(30).              GA2LPGM 
00259                                                                   GA2LPGM 
00260 **+**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2LPGM 
00261 **  CHANGE OCCURS COUNT AND LENGTH TO MATCH BMS MAP               GA2LPGM 
00262 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2LPGM 
00263      05  MAP-PROCEDURE-ARGUMENT-ROW OCCURS 15 TIMES               GA2LPGM 
00264          INDEXED BY MAP-IDX.                                      GA2LPGM 
00265          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA2LPGM 
00266          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA2LPGM 
00267          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA2LPGM 
00268          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA2LPGM 
00269          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA2LPGM 
00270          15  MAP-CODE-FUNCTION            PIC X(3).               GA2LPGM 
00271 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2LPGM 
00272 *------ AFTER OCCURS                                              GA2LPGM 
00273      05  MAP-SELECT-TXT5-LEN              PIC S9(4) COMP SYNC.    GA2LPGM 
00274      05  MAP-SELECT-TXT5-ATTR             PIC X.                  GA2LPGM 
00275      05  MAP-SELECT-TXT5                  PIC X(79).              GA2LPGM 
00276                                                                   GA2LPGM 
00277      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA2LPGM 
00278      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA2LPGM 
00279      05  MAP-ERROR-MESSAGE                PIC X(79).              GA2LPGM 
00280      SKIP3                                                        GA2LPGM 
00281  01  FILLER.                                                      GA2LPGM 
00282 ****************************************************************  GA2LPGM 
00283 **   THIS FIELD DESCRIBES THE NUMBER OF OCCURS FOR THE MAP.       GA2LPGM 
00284 ****************************************************************  GA2LPGM 
00285      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +15.  GA2LPGM 
00286 /                                                                 GA2LPGM 
00287 ** ALTERNATIVE WORKFILE KEYS **                                   GA2LPGM 
00288  01  FILLER                      PIC X(32)  VALUE                 GA2LPGM 
00289      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2LPGM 
00290  01  WS-ALT-WORKFILE-KEYS.                                        GA2LPGM 
00291  COPY GCWRKKEY.                                                   GA2LPGM 
00292 /                                                                 GA2LPGM 
00293 ** HARDCOPY WORK AREA **                                          GA2LPGM 
00294  01  FILLER                      PIC X(26)  VALUE                 GA2LPGM 
00295      '*** HARDCOPY WORK AREA ***'.                                GA2LPGM 
00296 *01  WS-HARDCOPY-COMMAREA.                                        GA2LPGM 
00297 *COPY PRNCOBOL.                                                   GA2LPGM 
00298                                                                   GA2LPGM 
00299 ** WORKFIELDS **                                                  GA2LPGM 
00300  01  FILLER                           PIC X(16)                   GA2LPGM 
00301              VALUE  '** WORKFIELDS **'.                           GA2LPGM 
00302  01  WS-WORK-FIELDS.                                              GA2LPGM 
00303      05  WS-HEX-00                    PIC X.                      GA2LPGM 
00304      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2LPGM 
00305 ***  05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2LPGM 
00306 ***    VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2LPGM 
00307                                                                   GA2LPGM 
00308  01  WS-TEST-AREA                     PIC X(7).                   GA2LPGM 
00309  01  WS-TEST-DATA REDEFINES WS-TEST-AREA.                         GA2LPGM 
00310      05  WS-TEST-DETAIL OCCURS 7 TIMES PIC X(01).                 GA2LPGM 
00311          88  WS-NON-SPECIAL-CHARACTERS VALUE                      GA2LPGM 
00312                                        SPACE                      GA2LPGM 
00313                                        '0' THRU '9'               GA2LPGM 
00314                                        'A' THRU 'I'               GA2LPGM 
00315                                        'J' THRU 'R'               GA2LPGM 
00316                                        'S' THRU 'Z'.              GA2LPGM 
00317                                                                   GA2LPGM 
00318 ******************************************************************GA2LPGM 
00319 ** THESE FIELDS DECRIBE ONE OCCURRENCE OF AN ENTRY IN THE TABULAR GA2LPGM 
00320 ** RECORD.  THEY ARE USED TO SAVE ONE OCCURRENCE DURING PROCESSINGGA2LPGM 
00321 ******************************************************************GA2LPGM 
00322      05  WS-SAVED-FIELDS.                                         GA2LPGM 
00323        10  WS-SAVED-PROCEDURE-ARGUMENT   PIC X(7).                GA2LPGM 
00324        10  WS-SAVED-CODE-FUNCTION        PIC X(3).                GA2LPGM 
00325                                                                   GA2LPGM 
00326      05  WS-SORTED-TAB     OCCURS 16 TIMES INDEXED BY             GA2LPGM 
00327          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2LPGM 
00328        10  WS-PROCEDURE-ARGUMENT      PIC X(7).                   GA2LPGM 
00329        10  WS-CODE-FUNCTION           PIC X(3).                   GA2LPGM 
00330                                                                   GA2LPGM 
00331 *** SWITCHES ***                                                  GA2LPGM 
00332  01  FILLER                           PIC X(14)                   GA2LPGM 
00333              VALUE  '** SWITCHES **'.                             GA2LPGM 
00334  01  WS-SWITCHES.                                                 GA2LPGM 
00335      05  WS-ERROR-SW                  PIC X.                      GA2LPGM 
00336                                                                   GA2LPGM 
00337 ** TITLE LINES **                                                 GA2LPGM 
00338  01  WS-TITLE-LINES.                                              GA2LPGM 
00339      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(43)  VALUE    GA2LPGM 
00340          '   GROUP SPECIFIC CONDITIONAL PROCEDURES   '.           GA2LPGM 
00341      05  CONTRACT-TITLE-LINE                  PIC X(43)  VALUE    GA2LPGM 
00342          '      CONTRACT CONDITIONAL PROCEDURES      '.           GA2LPGM 
00343      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(43)  VALUE    GA2LPGM 
00344          '  BENEFIT PROVISION CONDITIONAL PROCEDURES '.           GA2LPGM 
00345                                                                   GA2LPGM 
00346 /                                                                 GA2LPGM 
00347 *** RECORD LENGTHS ***                                            GA2LPGM 
00348  01  FILLER                           PIC X(20)                   GA2LPGM 
00349              VALUE  '** RECORD LENGTHS **'.                       GA2LPGM 
00350  01  WS-RECORD-LENGTHS.                                           GA2LPGM 
00351     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA2LPGM 
00352     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA2LPGM 
00353     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2LPGM 
00354     05 GCVI-COMMAREA-LEN              PIC S9(4)  COMP  VALUE +19. GA2LPGM 
00355     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2LPGM 
00356 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA2LPGM 
00357 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA2LPGM 
00358 *   05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA2LPGM 
00359 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2LPGM 
00360 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA2LPGM 
00361 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA2LPGM 
00362 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2LPGM 
00363 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA2LPGM 
00364 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA2LPGM 
00365 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2LPGM 
00366 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA2LPGM 
00367 ******************************************************************GA2LPGM 
00368 **   THESE FIELDS ARE USED TO DESCRIBE THE TABULAR RECORD.        GA2LPGM 
00369 ******************************************************************GA2LPGM 
00370 *   05 GC-GCTABULR-ACON-FIXED-LEN     PIC S9(5) COMP-3 VALUE +40. GA2LPGM 
00371 *   05 GC-GCTABULR-ACON-VARY-LEN      PIC S9(5) COMP-3 VALUE +9.  GA2LPGM 
00372 *   05 GC-GCTABULR-ACON-VARY-MAX-OCUR PIC S9(5) COMP-3 VALUE +440.GA2LPGM 
00373 /                                                                 GA2LPGM 
00374  COPY COBXIO.                                                     GA2LPGM 
00375 /                                                                 GA2LPGM 
00376 ** ATTRIBUTES **                                                  GA2LPGM 
00377  COPY DFHBMSCA.                                                   GA2LPGM 
00378      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2LPGM 
00379 /                                                                 GA2LPGM 
00380 ** ATTENTION IDENTIFIERS **                                       GA2LPGM 
00381  COPY DFHAID.                                                     GA2LPGM 
00382 /                                                                 GA2LPGM 
00383  01  GCVIOPGMS-PARM.                                              GA2LPGM 
00384  COPY GCVINTRC.                                                   GA2LPGM 
00385                                                                   GA2LPGM 
00386  01  WS-GCPS-LENGTHS.                                             GA2LPGM 
00387      COPY GCCDRLEN.                                               GA2LPGM 
00388                                                                   GA2LPGM 
00389 /                                                                 GA2LPGM 
00390  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2LPGM 
00391  01  COMMAREA-POINTER-AREA.                                       GA2LPGM 
00392      05  COMMAREA-PNTR-COMP  PIC S9(8) COMP.                      GA2LPGM 
00393      05  COMMAREA-PNTR       REDEFINES                            GA2LPGM 
00394          COMMAREA-PNTR-COMP  USAGE IS POINTER.                    GA2LPGM 
00395 /                                                                 GA2LPGM 
00396  01  WS-END                          PIC X(16)  VALUE             GA2LPGM 
00397      '*** W/S ENDS ***'.                                          GA2LPGM 
00398 /                                                                 GA2LPGM 
00399  LINKAGE SECTION.                                                 GA2LPGM 
00400 /                                                                 GA2LPGM 
00401  01  DFHCOMMAREA.                                                 GA2LPGM 
00402  COPY G2ALCKEC.                                                   GA2LPGM 
00403 *    05  INCOMING-COMMAREA-PNTR-COMP  PIC S9(8) COMP.             GA2LPGM 
00404 *    05  INCOMING-COMMAREA-PNTR       REDEFINES                   GA2LPGM 
00405 *        INCOMING-COMMAREA-PNTR-COMP  USAGE IS POINTER.           GA2LPGM 
00406 *                                                                 GA2LPGM 
00407 *01  BLL-CELLS.                                                   GA2LPGM 
00408 *    02  FILLER                      PIC S9(8)  COMP.             GA2LPGM 
00409 *    02  COMMAREA-PNTR               PIC S9(8)  COMP.             GA2LPGM 
00410 *    02  ALL-LEVEL-TAB-PNTR          PIC S9(8)  COMP.             GA2LPGM 
00411 *    02  ALL-LEVEL-TAB-PNTR2         PIC S9(8)  COMP.             GA2LPGM 
00412 *    02  COPY-AREA-PNTR              PIC S9(8)  COMP.             GA2LPGM 
00413 *    02  GRP-SPEC-PNTR               PIC S9(8)  COMP.             GA2LPGM 
00414 *    02  CONTRACT-PNTR               PIC S9(8)  COMP.             GA2LPGM 
00415 *    02  CONTRACT-PNTR2              PIC S9(8)  COMP.             GA2LPGM 
00416 *    02  BEN-PROV-PNTR               PIC S9(8)  COMP.             GA2LPGM 
00417 *    02  GCPPDIO-BLL-PNTR            PIC S9(8)  COMP.             GA2LPGM 
00418 *                                                                 GA2LPGM 
00419 *01  GCA-COMMAREA.                                                GA2LPGM 
00420 *COPY G2ALCKEC.                                                   GA2LPGM 
00421 /                                                                 GA2LPGM 
00422 ** I/O PARM, WORKFILE KEY, AND CONTRACT TABULAR RECORD **         GA2LPGM 
00423  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA2LPGM 
00424  COPY GCIOPRM1.                                                   GA2LPGM 
00425 /                                                                 GA2LPGM 
00426  COPY GCWRKDCC.                                                   GA2LPGM 
00427 /                                                                 GA2LPGM 
00428  COPY GCTACONC.                                                   GA2LPGM 
00429 /                                                                 GA2LPGM 
00430 ***************************************************************** GA2LPGM 
00431 ** THIS IS A SAVE AREA FOR THE TABLE FROM THE TABULAR RECORD      GA2LPGM 
00432 ** THESE ENTRIES ARE MERGED BACK INTO THE TABULAR RECORD DURING   GA2LPGM 
00433 ** THE SORT.                                                      GA2LPGM 
00434 ***************************************************************** GA2LPGM 
00435  01  COPY-OF-TABLE-AREA.                                          GA2LPGM 
00436      05  COPY-OF-TABLE    OCCURS 396 TIMES    INDEXED BY          GA2LPGM 
00437            COPY-IDX.                                              GA2LPGM 
00438        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA2LPGM 
00439        10  COPY-CODE-FUNCTION          PIC X(3).                  GA2LPGM 
00440 /                                                                 GA2LPGM 
00441 ** IO PARM, WITH WORKFILE KEY, AND RECORDS **                     GA2LPGM 
00442  01  IO-PARM-GRP-SPEC-RECORD.                                     GA2LPGM 
00443  COPY GCIOPRM2.                                                   GA2LPGM 
00444 /                                                                 GA2LPGM 
00445  COPY GCWRKDC2.                                                   GA2LPGM 
00446 /                                                                 GA2LPGM 
00447  COPY GCGROUPC.                                                   GA2LPGM 
00448 /                                                                 GA2LPGM 
00449                                                                   GA2LPGM 
00450  01  IO-PARM-CONTRACT-RECORD.                                     GA2LPGM 
00451  COPY GCIOPRM3.                                                   GA2LPGM 
00452 /                                                                 GA2LPGM 
00453  COPY GCWRKDC3.                                                   GA2LPGM 
00454 /                                                                 GA2LPGM 
00455  COPY GCCONTRC.                                                   GA2LPGM 
00456 /                                                                 GA2LPGM 
00457                                                                   GA2LPGM 
00458  01  IO-PARM-BEN-PROV-RECORD.                                     GA2LPGM 
00459  COPY GCIOPRM4.                                                   GA2LPGM 
00460 /                                                                 GA2LPGM 
00461  COPY GCWRKDC4.                                                   GA2LPGM 
00462 /                                                                 GA2LPGM 
00463  COPY GCBENPVC.                                                   GA2LPGM 
00464 /                                                                 GA2LPGM 
00465 ** IO PARM AREA **                                                GA2LPGM 
00466  01  GCPPDIO-PARM-AREA.                                           GA2LPGM 
00467  COPY GCPPDIOC.                                                   GA2LPGM 
00468 /                                                                 GA2LPGM 
00469                                                                   GA2LPGM 
00470  PROCEDURE DIVISION.                                              GA2LPGM 
00471                                                                   GA2LPGM 
00472 ******************************************************************GA2LPGM 
00473 **                H O U S E K E E P I N G                         GA2LPGM 
00474 **                                                                GA2LPGM 
00475 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2LPGM 
00476 **                                                                GA2LPGM 
00477 ******************************************************************GA2LPGM 
00478  0000-HOUSEKEEPING SECTION.                                       GA2LPGM 
00479                                                                   GA2LPGM 
00480      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2LPGM 
00481      IF EIBAID  =  DFHCLEAR                                       GA2LPGM 
00482          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2LPGM 
00483                         ERASE                                     GA2LPGM 
00484          END-EXEC                                                 GA2LPGM 
00485          EXEC CICS RETURN                                         GA2LPGM 
00486          END-EXEC.                                                GA2LPGM 
00487                                                                   GA2LPGM 
00488      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2LPGM 
00489                                   END-EXEC.                       GA2LPGM 
00490 /                                                                 GA2LPGM 
00491 ******************************************************************GA2LPGM 
00492 **                     M A I N L I N E                            GA2LPGM 
00493 **                                                                GA2LPGM 
00494 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2LPGM 
00495 **  TAKEN BY THE OPERATOR.                                        GA2LPGM 
00496 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2LPGM 
00497 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2LPGM 
00498 **     ADDITIONS FROM.                                            GA2LPGM 
00499 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2LPGM 
00500 **     KEY PF12 OR PF24.                                          GA2LPGM 
00501 **  3. RECEIVE THE SCREEN.                                        GA2LPGM 
00502 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2LPGM 
00503 **     MENU.                                                      GA2LPGM 
00504 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2LPGM 
00505 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2LPGM 
00506 **     (RETURN) TO THE DELETE PROGRAM (GA1LPGM).                  GA2LPGM 
00507 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2LPGM 
00508 **     (RETURN) TO THE PREVIOUS MENU.                             GA2LPGM 
00509 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN EXECUTE  GA2LPGM 
00510 **     THE NORMAL ADD LOGIC, EXCEPT BYPASS EMPTY VALIDATION TABLE GA2LPGM 
00511 **     CONDITION FOR THE COMBINATION CODE FIELD.                  GA2LPGM 
00512 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2LPGM 
00513 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2LPGM 
00514 **                                                                GA2LPGM 
00515 ******************************************************************GA2LPGM 
00516  1000-MAIN-LINE SECTION.                                          GA2LPGM 
00517                                                                   GA2LPGM 
00518      MOVE '1000'  TO  WS-PARA-ID.                                 GA2LPGM 
00519      IF EIBTRNID  NOT =  'GA2L'                                   GA2LPGM 
00520         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2LPGM 
00521         GO TO 1099-RETURN.                                        GA2LPGM 
00522                                                                   GA2LPGM 
00523      EXEC CICS RECEIVE   MAP('GA2LI01') MAPSET('GA2LSET')         GA2LPGM 
00524         INTO(GA2LI01I) END-EXEC.                                  GA2LPGM 
00525                                                                   GA2LPGM 
00526      IF MAP-SCREEN-ID NOT = '002L00'                              GA2LPGM 
00527         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2LPGM 
00528                                                                   GA2LPGM 
00529      IF EIBAID  =  DFHENTER                                       GA2LPGM 
00530         PERFORM 2000-ADD-PROCESSING                               GA2LPGM 
00531         GO TO 1099-RETURN.                                        GA2LPGM 
00532                                                                   GA2LPGM 
00533      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2LPGM 
00534         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2LPGM 
00535                                                                   GA2LPGM 
00536      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2LPGM 
00537         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2LPGM 
00538                                                                   GA2LPGM 
00539      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2LPGM 
00540         PERFORM 2000-ADD-PROCESSING                               GA2LPGM 
00541         GO TO 1099-RETURN.                                        GA2LPGM 
00542                                                                   GA2LPGM 
00543      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (1).                 GA2LPGM 
00544      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2LPGM 
00545 -    ' THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                  GA2LPGM 
00546      EXEC CICS SEND   MAP('GA2LI01') MAPSET('GA2LSET') DATAONLY   GA2LPGM 
00547         FROM(GA2LI01O) CURSOR END-EXEC.                           GA2LPGM 
00548                                                                   GA2LPGM 
00549  1099-RETURN.                                                     GA2LPGM 
00550 *    EXEC CICS RETURN   END-EXEC.                                 GA2LPGM 
00551      EXEC CICS RETURN TRANSID ('GA2L')                            GA2LPGM 
00552                COMMAREA (DFHCOMMAREA)                             GA2LPGM 
00553                END-EXEC.                                          GA2LPGM 
00554      GOBACK.                                                      GA2LPGM 
00555 /                                                                 GA2LPGM 
00556 ******************************************************************GA2LPGM 
00557 **               A D D   P R O C E S S I N G                      GA2LPGM 
00558 **                                                                GA2LPGM 
00559 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2LPGM 
00560 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2LPGM.            GA2LPGM 
00561 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2LPGM 
00562 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2LPGM 
00563 **     SKIP TO THE NEXT LINE.                                     GA2LPGM 
00564 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2LPGM 
00565 **     WITH SPECIAL CHARACTERS, AND NUMERIC FIELDS ARE TESTED     GA2LPGM 
00566 **     WITH THE NUMERIC CLASS TEST.  THE OPERATOR MUST ENTER SOME GA2LPGM 
00567 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2LPGM 
00568 **     DATA.                                                      GA2LPGM 
00569 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2LPGM 
00570 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2LPGM 
00571 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2LPGM 
00572 **     ORDER, FIELD BY FIELD.                                     GA2LPGM 
00573 **  6. THE ALL LEVEL TABULAR RECORD IS READ, AND A COPY OF THE    GA2LPGM 
00574 **     TABLE IS MADE.                                             GA2LPGM 
00575 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2LPGM 
00576 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2LPGM 
00577 **     BACK INTO THE RECORD.                                      GA2LPGM 
00578 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2LPGM 
00579 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2LPGM 
00580 **                                                                GA2LPGM 
00581 ******************************************************************GA2LPGM 
00582  2000-ADD-PROCESSING SECTION.                                     GA2LPGM 
00583                                                                   GA2LPGM 
00584      MOVE '2000'  TO  WS-PARA-ID.                                 GA2LPGM 
00585      MOVE 'N'  TO  WS-ERROR-SW.                                   GA2LPGM 
00586      MOVE 'Y'  TO  GCVI-TABLE-SW.                                 GA2LPGM 
00587      MOVE ZERO  TO  WS-ADD-COUNT.                                 GA2LPGM 
00588      SET MAP-IDX  TO  1.                                          GA2LPGM 
00589                                                                   GA2LPGM 
00590      MOVE '2005'  TO  WS-PARA-ID.                                 GA2LPGM 
00591  2005-RESET-ALL-ATTRIBUTES.                                       GA2LPGM 
00592      MOVE DFHBMUNF TO MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)       GA2LPGM 
00593                       MAP-CODE-FUNCTION-ATTR (MAP-IDX).           GA2LPGM 
00594      IF MAP-IDX   <  WS-MAP-ROW                                   GA2LPGM 
00595         SET MAP-IDX   UP BY  1                                    GA2LPGM 
00596         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2LPGM 
00597                                                                   GA2LPGM 
00598      SET MAP-IDX  TO  1.                                          GA2LPGM 
00599      MOVE '2010'  TO  WS-PARA-ID.                                 GA2LPGM 
00600  2010-VALIDATE-ADD-ENTRIES.                                       GA2LPGM 
00601      MOVE '2010'              TO WS-PARA-ID.                      GA2LPGM 
00602                                                                   GA2LPGM 
00603      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX) = ZERO  AND          GA2LPGM 
00604         MAP-CODE-FUNCTION-LEN (MAP-IDX) = ZERO                    GA2LPGM 
00605                                                                   GA2LPGM 
00606         IF MAP-IDX   <  WS-MAP-ROW                                GA2LPGM 
00607            SET MAP-IDX   UP BY  1                                 GA2LPGM 
00608            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2LPGM 
00609         ELSE                                                      GA2LPGM 
00610            GO TO 2020-CHECK-FOR-ERRORS.                           GA2LPGM 
00611                                                                   GA2LPGM 
00612      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)  =  ZERO             GA2LPGM 
00613         MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  GA2LPGM 
00614         MOVE '???????'  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)      GA2LPGM 
00615         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2LPGM 
00616            MOVE 'Y'  TO  WS-ERROR-SW                              GA2LPGM 
00617            MOVE -1   TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)     GA2LPGM 
00618            MOVE ' *** PROCEDURE ARGUMENT IS INVALID ***'  TO      GA2LPGM 
00619               MAP-ERROR-MESSAGE                                   GA2LPGM 
00620         ELSE                                                      GA2LPGM 
00621            NEXT SENTENCE                                          GA2LPGM 
00622      ELSE                                                         GA2LPGM 
00623         PERFORM 2015-EDIT-PROCEDURE-ARGUMENT                      GA2LPGM 
00624            THRU 2015-EXIT.                                        GA2LPGM 
00625                                                                   GA2LPGM 
00626      IF  MAP-CODE-FUNCTION-LEN (MAP-IDX)  =  ZERO                 GA2LPGM 
00627         MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)       GA2LPGM 
00628         MOVE '???'  TO  MAP-CODE-FUNCTION (MAP-IDX)               GA2LPGM 
00629         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2LPGM 
00630            MOVE 'Y'  TO  WS-ERROR-SW                              GA2LPGM 
00631            MOVE -1   TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)          GA2LPGM 
00632            MOVE ' *** CODE FUNCTION IS INVALID ***'  TO           GA2LPGM 
00633               MAP-ERROR-MESSAGE                                   GA2LPGM 
00634         ELSE                                                      GA2LPGM 
00635            NEXT SENTENCE                                          GA2LPGM 
00636      ELSE                                                         GA2LPGM 
00637         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2LPGM 
00638            WS-TEST-AREA                                           GA2LPGM 
00639 ***     MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2LPGM 
00640 ***        WS-SAVED-CODE-FUNCTION                                 GA2LPGM 
00641 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION FROM QUOTES TO '\
00642 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION FROM                    GA2LPGM 
00643 ***        WS-NON-SPECIAL-CHARACTERS  TO  QUOTES                  GA2LPGM 
00644 ***     INSPECT WS-SAVED-CODE-FUNCTION REPLACING ALL              GA2LPGM 
00645 ***        QUOTES BY '\
00646 ***     INSPECT WS-SAVED-CODE-FUNCTION REPLACING ALL              GA2LPGM 
00647 ***        WS-NON-SPECIAL-CHARACTERS  BY  QUOTES                  GA2LPGM 
00648 ***     IF  WS-SAVED-CODE-FUNCTION NOT = QUOTES                   GA2LPGM 
00649         IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                GA2LPGM 
00650                 WS-NON-SPECIAL-CHARACTERS  (2) AND                GA2LPGM 
00651                 WS-NON-SPECIAL-CHARACTERS  (3) AND                GA2LPGM 
00652                 WS-NON-SPECIAL-CHARACTERS  (4) AND                GA2LPGM 
00653                 WS-NON-SPECIAL-CHARACTERS  (5) AND                GA2LPGM 
00654                 WS-NON-SPECIAL-CHARACTERS  (6) AND                GA2LPGM 
00655                 WS-NON-SPECIAL-CHARACTERS  (7)                    GA2LPGM 
00656            MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)    GA2LPGM 
00657            IF  WS-ERROR-SW  NOT =  'Y'                            GA2LPGM 
00658               MOVE 'Y'  TO  WS-ERROR-SW                           GA2LPGM 
00659               MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)        GA2LPGM 
00660               MOVE '        *** CODE FUNCTION IS INVALID ***'     GA2LPGM 
00661                  TO  MAP-ERROR-MESSAGE.                           GA2LPGM 
00662                                                                   GA2LPGM 
00663      IF MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX)  NOT =  DFHBMUBF     GA2LPGM 
00664                        AND                                        GA2LPGM 
00665         MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2LPGM 
00666         ADD 1  TO  WS-ADD-COUNT                                   GA2LPGM 
00667         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2LPGM 
00668         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                 GA2LPGM 
00669                              WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)  GA2LPGM 
00670         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2LPGM 
00671                                   WS-CODE-FUNCTION (WS-SORT-IDX). GA2LPGM 
00672                                                                   GA2LPGM 
00673      IF MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2LPGM 
00674         MOVE 'MULT02'  TO  GCVI-FIELDS-KEY-ID                     GA2LPGM 
00675         MOVE  ZEROES   TO  GCVI-RETURN-CODE                       GA2LPGM 
00676         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO  GCVI-VALUE-LEN-3    GA2LPGM 
00677         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2LPGM 
00678                        COMMAREA(GCVIOPGMS-PARM)                   GA2LPGM 
00679                        LENGTH(GCVI-COMMAREA-LEN) END-EXEC         GA2LPGM 
00680         IF GCVI-VALUE-NOT-FOUND                                   GA2LPGM 
00681            IF WS-ERROR-SW  NOT =  'Y'                             GA2LPGM 
00682               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2LPGM 
00683               MOVE -1       TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)   GA2LPGM 
00684               MOVE '*** COMBINATION CODE INVALID ***'             GA2LPGM 
00685               TO MAP-ERROR-MESSAGE                                GA2LPGM 
00686               MOVE 'Y'  TO  WS-ERROR-SW                           GA2LPGM 
00687            ELSE                                                   GA2LPGM 
00688               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2LPGM 
00689         ELSE                                                      GA2LPGM 
00690            IF GCVI-VALUE-NOT-LOADED                               GA2LPGM 
00691               IF EIBAID  =  DFHPF4 OR  =  DFHPF16                 GA2LPGM 
00692                  NEXT SENTENCE                                    GA2LPGM 
00693               ELSE                                                GA2LPGM 
00694                 MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR(MAP-IDX)GA2LPGM 
00695                  IF WS-ERROR-SW  NOT =  'Y'                       GA2LPGM 
00696                     MOVE 'Y'  TO  WS-ERROR-SW                     GA2LPGM 
00697                     MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)  GA2LPGM 
00698               MOVE 'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS PGA2LPGM 
00699 -                     'F4/PF16 TO CONTINUE'                       GA2LPGM 
00700                       TO  MAP-ERROR-MESSAGE.                      GA2LPGM 
00701                                                                   GA2LPGM 
00702      IF MAP-IDX   <  WS-MAP-ROW                                   GA2LPGM 
00703         SET MAP-IDX   UP BY  1                                    GA2LPGM 
00704         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2LPGM 
00705 /                                                                 GA2LPGM 
00706 ****************************************************************  GA2LPGM 
00707 ** THIS GENERIC ROUTINE WILL EDIT THE PROCEDURE CODE. FIELDS  **  GA2LPGM 
00708 ** REQUIRING PROBABLE 'TAILORING' APPEAR AS -->NAME.          **  GA2LPGM 
00709 **                                                            **  GA2LPGM 
00710 ** REQUIRED WORKING-STORAGE:                                  **  GA2LPGM 
00711 **   1. 01  PROCED-KEY.                                       **  GA2LPGM 
00712 **          03  SYSTEM-INDICATOR          PIC X(1).           **  GA2LPGM 
00713 **          03  PROCED-CODE               PIC X(7).           **  GA2LPGM 
00714 **              04  PROCEDR-DIGIT REDEFINES PROCED-CODE       **  GA2LPGM 
00715 **                  OCCURS 7 TIMES        PIC X(1).           **  GA2LPGM 
00716 **                                                            **  GA2LPGM 
00717 **   2. 01  WS-PRO-HAF-COMM-LEN   PIC S9(4) COMP VALUE +344.  **  GA2LPGM 
00718 **                                                            **  GA2LPGM 
00719 **   3. COPY COBXIO.                                          **  GA2LPGM 
00720 ****************************************************************  GA2LPGM 
00721                                                                   GA2LPGM 
00722  2015-EDIT-PROCEDURE-ARGUMENT.                                    GA2LPGM 
00723                                                                   GA2LPGM 
00724 ***  MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2LPGM 
00725 ***      WS-SAVED-PROCEDURE-ARGUMENT.                             GA2LPGM 
00726                                                                   GA2LPGM 
00727      MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2LPGM 
00728          WS-TEST-AREA.                                            GA2LPGM 
00729                                                                   GA2LPGM 
00730 ***  TRANSFORM  WS-SAVED-PROCEDURE-ARGUMENT FROM QUOTES TO '\
00731 ***  TRANSFORM  WS-SAVED-PROCEDURE-ARGUMENT FROM                  GA2LPGM 
00732 ***      WS-NON-SPECIAL-CHARACTERS  TO  QUOTES.                   GA2LPGM 
00733 ***  INSPECT WS-SAVED-PROCEDURE-ARGUMENT REPLACING ALL            GA2LPGM 
00734 ***      QUOTES BY '\
00735 ***  INSPECT WS-SAVED-PROCEDURE-ARGUMENT REPLACING ALL            GA2LPGM 
00736 ***      WS-NON-SPECIAL-CHARACTERS  BY  QUOTES.                   GA2LPGM 
00737 ***  IF  WS-SAVED-PROCEDURE-ARGUMENT NOT = QUOTES                 GA2LPGM 
00738                                                                   GA2LPGM 
00739      IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                   GA2LPGM 
00740              WS-NON-SPECIAL-CHARACTERS  (2) AND                   GA2LPGM 
00741              WS-NON-SPECIAL-CHARACTERS  (3) AND                   GA2LPGM 
00742              WS-NON-SPECIAL-CHARACTERS  (4) AND                   GA2LPGM 
00743              WS-NON-SPECIAL-CHARACTERS  (5) AND                   GA2LPGM 
00744              WS-NON-SPECIAL-CHARACTERS  (6) AND                   GA2LPGM 
00745              WS-NON-SPECIAL-CHARACTERS  (7)                       GA2LPGM 
00746          MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX) GA2LPGM 
00747          IF  WS-ERROR-SW  NOT =  'Y'                              GA2LPGM 
00748            MOVE 'Y' TO  WS-ERROR-SW                               GA2LPGM 
00749            MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)      GA2LPGM 
00750            MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'   GA2LPGM 
00751                  TO  MAP-ERROR-MESSAGE                            GA2LPGM 
00752            GO TO 2015-EXIT                                        GA2LPGM 
00753          ELSE                                                     GA2LPGM 
00754            GO TO 2015-EXIT.                                       GA2LPGM 
00755                                                                   GA2LPGM 
00757      EXEC CICS GETMAIN                                            GA2LPGM 
00758         SET(ADDRESS OF GCPPDIO-PARM-AREA)                         GA2LPGM 
00759         INITIMG(WS-HEX-00)                                        GA2LPGM 
00760         LENGTH(GCPPDIO-CA-LEN)                                    GA2LPGM 
00760      END-EXEC.                                                    GA2LPGM 
00761                                                                   GA2LPGM 
00762 ***  SERVICE RELOAD GCPPDIO-PARM-AREA.                            GA2LPGM 
00763                                                                   GA2LPGM 
           MOVE MAP-PROCEDURE-ARGUMENT(MAP-IDX) TO PROCED-CODE.                 
                                                                                
00755 *** ICD-10 START                                                  GA2LPGM 
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
              MOVE  'H'     TO  SYSTEM-INDICATOR                                
           ELSE                                                                 
      *** ICD-10 START                                                          
              IF PROCEDR-DIGIT(7)  =  SPACE                                     
                 MOVE  'C'  TO  SYSTEM-INDICATOR                                
              ELSE                                                              
                 MOVE  'Z'  TO  SYSTEM-INDICATOR                                
              END-IF                                                            
           END-IF                                                               
      *** ICD-10 END                                                            
      *    END-IF                                                               
                                                                                
           MOVE PROCED-KEY     TO  GCPPDIO-SERVICE-CODE-AREA.                   
00773                                                                   GA2LPGM 
00774      EXEC CICS LINK PROGRAM('GCPPDIO')                            GA2LPGM 
00775           COMMAREA(GCPPDIO-PARM-AREA)                             GA2LPGM 
00776           LENGTH(GCPPDIO-CA-LEN)                                  GA2LPGM 
00777      END-EXEC.                                                    GA2LPGM 
00778                                                                   GA2LPGM 
00779      IF GCPPDIO-SUCCESSFUL                                        GA2LPGM 
00780          GO TO 2015-EXIT.                                         GA2LPGM 
00781                                                                   GA2LPGM 
00782      IF NOT GCPPDIO-REC-NOT-FOUND                                 GA2LPGM 
00783          MOVE 'DEW1'  TO  WS-ABEND-CODE                           GA2LPGM 
00784          MOVE GCPPDIO-RETURN-MESSAGE TO MAP-ERROR-MESSAGE         GA2LPGM 
00785          PERFORM 9999-ERROR-MSG-THEN-ABEND.                       GA2LPGM 
00786                                                                   GA2LPGM 
00787      MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX).     GA2LPGM 
00788                                                                   GA2LPGM 
00789      IF WS-ERROR-SW  NOT =  'Y'                                   GA2LPGM 
00790          MOVE 'Y' TO  WS-ERROR-SW                                 GA2LPGM 
00791          MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)         GA2LPGM 
00792          MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'     GA2LPGM 
00793            TO  MAP-ERROR-MESSAGE.                                 GA2LPGM 
00794                                                                   GA2LPGM 
00795  2015-EXIT. EXIT.                                                 GA2LPGM 
00796 /                                                                 GA2LPGM 
00797  2020-CHECK-FOR-ERRORS.                                           GA2LPGM 
00798      MOVE '2020'  TO  WS-PARA-ID.                                 GA2LPGM 
00799      IF WS-ERROR-SW  =  'Y' OR GCVI-TABLE-SW = 'N'                GA2LPGM 
00800         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE,                   GA2LPGM 
00801            MAP-MAIN-TITLE,                                        GA2LPGM 
00802            MAP-SCREEN-ID,                                         GA2LPGM 
00803            MAP-TABULAR-ID,                                        GA2LPGM 
00804            MAP-TABULAR-SLOT,                                      GA2LPGM 
00805            FRMNUIDO,                                              GA2LPGM 
00806            MAP-ID-LINE                                            GA2LPGM 
00807         MOVE '2100'  TO  WS-PARA-ID                               GA2LPGM 
00808         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2LPGM 
00809            VARYING MAP-IDX  FROM  1  BY  1                        GA2LPGM 
00810               UNTIL MAP-IDX  >  WS-MAP-ROW                        GA2LPGM 
00811                                                                   GA2LPGM 
00812         EXEC CICS SEND   MAP('GA2LI01') MAPSET('GA2LSET')         GA2LPGM 
00813            DATAONLY FROM(GA2LI01O) CURSOR END-EXEC                GA2LPGM 
00814         GO TO 2099-EXIT.                                          GA2LPGM 
00815                                                                   GA2LPGM 
00816      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2LPGM 
00817         MOVE '                  *** ADD ENTRY NOT FOUND ***'      GA2LPGM 
00818            TO  MAP-ERROR-MESSAGE                                  GA2LPGM 
00819         SET MAP-IDX  TO  1                                        GA2LPGM 
00820         MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)         GA2LPGM 
00821         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE,                   GA2LPGM 
00822            MAP-MAIN-TITLE,                                        GA2LPGM 
00823            MAP-SCREEN-ID,                                         GA2LPGM 
00824            MAP-TABULAR-ID,                                        GA2LPGM 
00825            MAP-TABULAR-SLOT,                                      GA2LPGM 
00826            FRMNUIDO,                                              GA2LPGM 
00827            MAP-ID-LINE                                            GA2LPGM 
00828         EXEC CICS SEND   MAP('GA2LI01') MAPSET('GA2LSET')         GA2LPGM 
00829            DATAONLY FROM(GA2LI01O) CURSOR END-EXEC                GA2LPGM 
00830         GO TO 2099-EXIT.                                          GA2LPGM 
00831                                                                   GA2LPGM 
00832  2025-CONTINUE-UPDATE.                                            GA2LPGM 
00833      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2LPGM 
00834               GC-GCIOPARM-LEN + GC-WORKFILE-KEY-LEN +             GA2LPGM 
00835                         GC-GCTABULR-ACON-FIXED-LEN +              GA2LPGM 
00836           (GC-GCTABULR-ACON-VARY-LEN *                            GA2LPGM 
00837           GC-GCTABULR-ACON-VARY-MAX-OCUR).                        GA2LPGM 
00838                                                                   GA2LPGM 
00839 ***  EXEC CICS GETMAIN SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00) GA2LPGM 
00840      EXEC CICS GETMAIN                                            GA2LPGM 
00841         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2LPGM 
00842         INITIMG(WS-HEX-00)                                        GA2LPGM 
00843         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2LPGM 
00844 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2LPGM 
00845 ***  ADD ALL-LEVEL-TAB-PNTR, 4096  GIVING  ALL-LEVEL-TAB-PNTR2.   GA2LPGM 
00846                                                                   GA2LPGM 
00847      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA2LPGM 
00848         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2LPGM 
00849         MOVE  'G'                 TO GCIO-WRK-STATUS-CODE         GA2LPGM 
00850         MOVE  'G3'                TO GCIO-WRK-RECORD-TYPE         GA2LPGM 
00851         MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE           GA2LPGM 
00852         MOVE GCA-GROUP-NO-1-3     TO GCIO-WRK-GROUP-NO-1-3        GA2LPGM 
00853         MOVE GRP-SPEC-GROUP-NO    TO GCIO-WRK-GROUP-NO            GA2LPGM 
00854         MOVE GCA-SEC-NO-1         TO GCIO-WRK-SEC-NO-1            GA2LPGM 
00855         MOVE GRP-SPEC-SECTION-NO  TO GCIO-WRK-SECTION-NO          GA2LPGM 
00856         MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE            GA2LPGM 
00857         MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS         GA2LPGM 
00858                                      GCIO-WRK-PROVIDER-CONTROL    GA2LPGM 
00859         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2LPGM 
00860         MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN           GA2LPGM 
00861 *  MOVE GRP-SPEC-EFF-DATE  TO  WS-MDY                             GA2LPGM 
00862         MOVE MAP-TABULAR-ID       TO GCIO-WRK-PROVISION-ID        GA2LPGM 
00863         MOVE MAP-TABULAR-SLOT     TO GCIO-WRK-PROVISION-SLOT-NO   GA2LPGM 
00864         MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID    GA2LPGM 
00865         MOVE ZEROES               TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA2LPGM 
00866                                                                   GA2LPGM 
00867      IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA2LPGM 
00868         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2LPGM 
00869         MOVE  'C'                 TO GCIO-WRK-STATUS-CODE         GA2LPGM 
00870         MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE         GA2LPGM 
00871         MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE           GA2LPGM 
00872         MOVE GCA-GROUP-NO-1-3     TO GCIO-WRK-GROUP-NO-1-3        GA2LPGM 
00873         MOVE CONTRACT-GROUP-NO    TO GCIO-WRK-GROUP-NO            GA2LPGM 
00874         MOVE GCA-SEC-NO-1         TO GCIO-WRK-SEC-NO-1            GA2LPGM 
00875         MOVE CONTRACT-SECTION-NO  TO GCIO-WRK-SECTION-NO          GA2LPGM 
00876         MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE            GA2LPGM 
00877         MOVE CONTRACT-LOB         TO GCIO-WRK-LINE-OF-BUS         GA2LPGM 
00878         MOVE CONTRACT-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL    GA2LPGM 
00879         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2LPGM 
00880         MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN           GA2LPGM 
00881 *  MOVE CONTRACT-EFF-DATE  TO  WS-MDY                             GA2LPGM 
00882         MOVE MAP-TABULAR-ID       TO GCIO-WRK-PROVISION-ID        GA2LPGM 
00883         MOVE MAP-TABULAR-SLOT     TO GCIO-WRK-PROVISION-SLOT-NO   GA2LPGM 
00884         MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID    GA2LPGM 
00885         MOVE ZEROES               TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA2LPGM 
00886                                                                   GA2LPGM 
00887      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA2LPGM 
00888         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2LPGM 
00889         MOVE  'C'                 TO GCIO-WRK-STATUS-CODE         GA2LPGM 
00890         MOVE  'C5'                TO GCIO-WRK-RECORD-TYPE         GA2LPGM 
00891         MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE           GA2LPGM 
00892         MOVE GCA-GROUP-NO-1-3     TO GCIO-WRK-GROUP-NO-1-3        GA2LPGM 
00893         MOVE BEN-PROV-GROUP-NO    TO GCIO-WRK-GROUP-NO            GA2LPGM 
00894         MOVE GCA-SEC-NO-1         TO GCIO-WRK-SEC-NO-1            GA2LPGM 
00895         MOVE BEN-PROV-SECTION-NO  TO GCIO-WRK-SECTION-NO          GA2LPGM 
00896         MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE            GA2LPGM 
00897         MOVE BEN-PROV-LOB         TO GCIO-WRK-LINE-OF-BUS         GA2LPGM 
00898         MOVE BEN-PROV-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL    GA2LPGM 
00899         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2LPGM 
00900         MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN           GA2LPGM 
00901 *   MOVE BEN-PROV-EFF-DATE  TO  WS-MDY                            GA2LPGM 
00902         MOVE BEN-PROV-ID-NO       TO GCIO-WRK-PROVISION-ID        GA2LPGM 
00903         MOVE 9999999              TO GCIO-WRK-PROVISION-SLOT-NO   GA2LPGM 
00904         MOVE MAP-TABULAR-ID       TO GCIO-WRK-TAB-PROVISION-ID    GA2LPGM 
00905         MOVE MAP-TABULAR-SLOT     TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA2LPGM 
00906                                                                   GA2LPGM 
00907 *    MOVE  WS-Y  TO  WS-YY.                                       GA2LPGM 
00908 *    IF WS-M  >  2                                                GA2LPGM 
00909 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA2LPGM 
00910 *          REMAINDER  WS-REMAINDER                                GA2LPGM 
00911 *    ELSE                                                         GA2LPGM 
00912 *       MOVE 1  TO  WS-REMAINDER.                                 GA2LPGM 
00913 *    SET WS-M-IDX  TO  WS-M.                                      GA2LPGM 
00914 *    MOVE WS-MONTH-TABLE (WS-M)  TO  WS-DDD.                      GA2LPGM 
00915 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2LPGM 
00916 *    IF WS-REMAINDER  =  ZERO                                     GA2LPGM 
00917 *       ADD 1  TO  WS-DDD.                                        GA2LPGM 
00918 *                                                                 GA2LPGM 
00919 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2LPGM 
00920      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA2LPGM 
00921      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2LPGM 
00922      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2LPGM 
00923                                                                   GA2LPGM 
00924      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2LPGM 
00925      TO   GAI-ENTRY-COUNT.                                        GA2LPGM 
00926                                                                   GA2LPGM 
00927      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2LPGM 
00928                                                                   GA2LPGM 
00929      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2LPGM 
00930         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2LPGM 
00931         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2LPGM 
00932                                                                   GA2LPGM 
00933      IF  NOT GCIO-GOOD-RETURN                                     GA2LPGM 
00934         MOVE '*** ERROR READING ALL LEVEL TABULAR.  CONTACT SYSTEMGA2LPGM 
00935 -    'S AREA ***'  TO  MAP-ERROR-MESSAGE                          GA2LPGM 
00936         MOVE '2LF1'  TO  WS-ABEND-CODE                            GA2LPGM 
00937         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
00938                                                                   GA2LPGM 
00939      IF  WS-ADD-COUNT  NOT >  ZERO                                GA2LPGM 
00940         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2LPGM 
00941                                                                   GA2LPGM 
00942      SET WS-SORT-IDX  TO  1.                                      GA2LPGM 
00943      SET WS-SORT-IDX2  TO  2.                                     GA2LPGM 
00944      MOVE '2030'  TO  WS-PARA-ID.                                 GA2LPGM 
00945                                                                   GA2LPGM 
00946  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2LPGM 
00947      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2LPGM 
00948         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2LPGM 
00949                                                                   GA2LPGM 
00950      IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <                     GA2LPGM 
00951         WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                      GA2LPGM 
00952         SET WS-SORT-IDX2  UP BY  1                                GA2LPGM 
00953         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2LPGM 
00954      ELSE                                                         GA2LPGM 
00955         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2LPGM 
00956            WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                   GA2LPGM 
00957            MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                    GA2LPGM 
00958               WS-SAVED-FIELDS                                     GA2LPGM 
00959            MOVE WS-SORTED-TAB (WS-SORT-IDX2) TO                   GA2LPGM 
00960               WS-SORTED-TAB (WS-SORT-IDX)                         GA2LPGM 
00961            MOVE WS-SAVED-FIELDS TO                                GA2LPGM 
00962               WS-SORTED-TAB (WS-SORT-IDX2)                        GA2LPGM 
00963            SET WS-SORT-IDX2  UP BY  1                             GA2LPGM 
00964            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2LPGM 
00965                                                                   GA2LPGM 
00966      IF WS-CODE-FUNCTION (WS-SORT-IDX) <                          GA2LPGM 
00967         WS-CODE-FUNCTION (WS-SORT-IDX2)                           GA2LPGM 
00968         SET WS-SORT-IDX2  UP BY  1                                GA2LPGM 
00969         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2LPGM 
00970      ELSE                                                         GA2LPGM 
00971         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2LPGM 
00972            WS-CODE-FUNCTION (WS-SORT-IDX2)                        GA2LPGM 
00973            MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                    GA2LPGM 
00974               WS-SAVED-FIELDS                                     GA2LPGM 
00975            MOVE WS-SORTED-TAB (WS-SORT-IDX2) TO                   GA2LPGM 
00976               WS-SORTED-TAB (WS-SORT-IDX)                         GA2LPGM 
00977            MOVE WS-SAVED-FIELDS TO                                GA2LPGM 
00978               WS-SORTED-TAB (WS-SORT-IDX2)                        GA2LPGM 
00979            SET WS-SORT-IDX2  UP BY  1                             GA2LPGM 
00980            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2LPGM 
00981                                                                   GA2LPGM 
00982      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2LPGM 
00983      MOVE WS-SORTED-TAB (WS-SORT-IDX3)                            GA2LPGM 
00984        TO WS-SORTED-TAB (WS-SORT-IDX2).                           GA2LPGM 
00985      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2LPGM 
00986      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2LPGM 
00987                                                                   GA2LPGM 
00988  2040-ARE-WE-DONE-WITH-SORT.                                      GA2LPGM 
00989      MOVE '2040'  TO  WS-PARA-ID.                                 GA2LPGM 
00990      SET WS-SORT-IDX  UP BY  1.                                   GA2LPGM 
00991      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2LPGM 
00992         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2LPGM 
00993         SET WS-SORT-IDX2  UP BY  1                                GA2LPGM 
00994         MOVE '2030'  TO  WS-PARA-ID                               GA2LPGM 
00995         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2LPGM 
00996                                                                   GA2LPGM 
00997      SET WS-ADD-COUNT  TO  WS-SORT-IDX.                           GA2LPGM 
00998      MOVE HIGH-VALUES  TO  WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)    GA2LPGM 
00999                            WS-CODE-FUNCTION (WS-SORT-IDX).        GA2LPGM 
01000      MOVE GAI-ENTRY-COUNT  TO  GAI-ENTRY-COUNT.                   GA2LPGM 
01001                                                                   GA2LPGM 
01002      COMPUTE  WS-COPY-LENGTH  =                                   GA2LPGM 
01003           GAI-ENTRY-COUNT  *  GC-GCTABULR-ACON-VARY-LEN.          GA2LPGM 
01004                                                                   GA2LPGM 
01005 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA2LPGM 
01006      EXEC CICS GETMAIN                                            GA2LPGM 
01007         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA2LPGM 
01008         LENGTH(WS-COPY-LENGTH)                                    GA2LPGM 
01009         INITIMG(WS-HEX-00) END-EXEC.                              GA2LPGM 
01010 ***  SERVICE RELOAD  COPY-OF-TABLE-AREA.                          GA2LPGM 
01011                                                                   GA2LPGM 
01012      SET COPY-IDX,  GAI-INDEX  TO 1.                              GA2LPGM 
01013                                                                   GA2LPGM 
01014      MOVE '2050'  TO  WS-PARA-ID.                                 GA2LPGM 
01015  2050-MAKE-A-COPY-OF-RECORD.                                      GA2LPGM 
01016      IF GAI-INDEX  NOT >  GAI-ENTRY-COUNT                         GA2LPGM 
01017         MOVE GAI-ENTRY (GAI-INDEX)  TO                            GA2LPGM 
01018            COPY-OF-TABLE (COPY-IDX)                               GA2LPGM 
01019         SET COPY-IDX, GAI-INDEX  UP BY  1                         GA2LPGM 
01020         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2LPGM 
01021                                                                   GA2LPGM 
01022      IF WS-ADD-COUNT  +  GAI-ENTRY-COUNT  >                       GA2LPGM 
01023         GC-GCTABULR-ACON-VARY-MAX-OCUR                            GA2LPGM 
01024         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2LPGM 
01025 -    'EASE CONTACT SYSTEMS AREA ***'                              GA2LPGM 
01026             TO  MAP-ERROR-MESSAGE                                 GA2LPGM 
01027         MOVE '2LL1'  TO  WS-ABEND-CODE                            GA2LPGM 
01028         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01029                                                                   GA2LPGM 
01030      SET WS-SORT-IDX, COPY-IDX, GAI-INDEX  TO  1.                 GA2LPGM 
01031      MOVE '2060'  TO  WS-PARA-ID.                                 GA2LPGM 
01032  2060-MERGE-IN-NEW-ENTRIES.                                       GA2LPGM 
01033      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2LPGM 
01034         SET GAI-INDEX  DOWN BY  1                                 GA2LPGM 
01035         SET GAI-ENTRY-COUNT  TO  GAI-INDEX                        GA2LPGM 
01036         MOVE GAI-ENTRY-COUNT  TO  GAI-ENTRY-COUNT                 GA2LPGM 
01037         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2LPGM 
01038                                                                   GA2LPGM 
01039      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2LPGM 
01040             =  HIGH-VALUES  AND                                   GA2LPGM 
01041         COPY-OF-TABLE (COPY-IDX)  NOT  =  HIGH-VALUES             GA2LPGM 
01042         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2LPGM 
01043                                                                   GA2LPGM 
01044      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2LPGM 
01045             NOT  =  HIGH-VALUES  AND                              GA2LPGM 
01046         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2LPGM 
01047         GO TO 2080-INSERT-NEW-ENTRY.                              GA2LPGM 
01048                                                                   GA2LPGM 
01049      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2LPGM 
01050             =  HIGH-VALUES  AND                                   GA2LPGM 
01051         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2LPGM 
01052         NEXT SENTENCE                                             GA2LPGM 
01053      ELSE                                                         GA2LPGM 
01054         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2LPGM 
01055            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA2LPGM 
01056            GO TO 2070-SAVE-COPIED-ENTRY                           GA2LPGM 
01057         ELSE                                                      GA2LPGM 
01058            IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <               GA2LPGM 
01059               COPY-PROCEDURE-ARGUMENT (COPY-IDX)                  GA2LPGM 
01060               GO TO 2080-INSERT-NEW-ENTRY.                        GA2LPGM 
01061                                                                   GA2LPGM 
01062      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2LPGM 
01063             =  HIGH-VALUES  AND                                   GA2LPGM 
01064         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2LPGM 
01065         NEXT SENTENCE                                             GA2LPGM 
01066      ELSE                                                         GA2LPGM 
01067         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2LPGM 
01068            COPY-CODE-FUNCTION (COPY-IDX)                          GA2LPGM 
01069            GO TO 2070-SAVE-COPIED-ENTRY                           GA2LPGM 
01070         ELSE                                                      GA2LPGM 
01071            IF WS-CODE-FUNCTION (WS-SORT-IDX) <                    GA2LPGM 
01072               COPY-CODE-FUNCTION (COPY-IDX)                       GA2LPGM 
01073               GO TO 2080-INSERT-NEW-ENTRY.                        GA2LPGM 
01074                                                                   GA2LPGM 
01075 ****************************************************************  GA2LPGM 
01076 **   AT THIS POINT THE NEW ENTRY'S THREE FIELDS MUST BE EQUAL TO  GA2LPGM 
01077 **   THE OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING  GA2LPGM 
01078 **   THE INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE   GA2LPGM 
01079 **   ENTRY FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.  GA2LPGM 
01080 ****************************************************************  GA2LPGM 
01081                                                                   GA2LPGM 
01082      SET WS-SORT-IDX  UP BY  1.                                   GA2LPGM 
01083                                                                   GA2LPGM 
01084  2070-SAVE-COPIED-ENTRY.                                          GA2LPGM 
01085      MOVE '2070'  TO  WS-PARA-ID.                                 GA2LPGM 
01086      MOVE COPY-OF-TABLE (COPY-IDX)  TO                            GA2LPGM 
01087         GAI-ENTRY (GAI-INDEX).                                    GA2LPGM 
01088                                                                   GA2LPGM 
01089      IF COPY-IDX  NOT >  GAI-ENTRY-COUNT                          GA2LPGM 
01090         SET COPY-IDX  UP BY  1                                    GA2LPGM 
01091         SET GAI-INDEX  UP BY  1                                   GA2LPGM 
01092         MOVE '2060'  TO  WS-PARA-ID                               GA2LPGM 
01093         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2LPGM 
01094      ELSE                                                         GA2LPGM 
01095         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2LPGM 
01096 -    'PLEASE CONTACT SYSTEMS AREA ***'                            GA2LPGM 
01097         TO  MAP-ERROR-MESSAGE                                     GA2LPGM 
01098         MOVE '2LL2'  TO  WS-ABEND-CODE                            GA2LPGM 
01099         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01100                                                                   GA2LPGM 
01101  2080-INSERT-NEW-ENTRY.                                           GA2LPGM 
01102      MOVE '2080'  TO  WS-PARA-ID.                                 GA2LPGM 
01103      MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                          GA2LPGM 
01104         GAI-ENTRY (GAI-INDEX).                                    GA2LPGM 
01105                                                                   GA2LPGM 
01106      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2LPGM 
01107         SET WS-SORT-IDX  UP BY  1                                 GA2LPGM 
01108         SET GAI-INDEX  UP BY  1                                   GA2LPGM 
01109         MOVE '2060'  TO  WS-PARA-ID                               GA2LPGM 
01110         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2LPGM 
01111      ELSE                                                         GA2LPGM 
01112         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2LPGM 
01113 -    'PLEASE CONTACT SYSTEMS AREA ***'                            GA2LPGM 
01114         TO  MAP-ERROR-MESSAGE                                     GA2LPGM 
01115         MOVE '2LL3'  TO  WS-ABEND-CODE                            GA2LPGM 
01116         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01117                                                                   GA2LPGM 
01118  2090-UPDATE-ALL-LVL-TAB-REC.                                     GA2LPGM 
01119      MOVE '2090'  TO  WS-PARA-ID.                                 GA2LPGM 
01120                                                                   GA2LPGM 
01121 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2LPGM 
01122                                                                   GA2LPGM 
01123      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2LPGM 
01124      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2LPGM 
01125                                                                   GA2LPGM 
01126      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN        +GA2LPGM 
01127                     GC-GCTABULR-ACON-FIXED-LEN +                  GA2LPGM 
01128              (GAI-ENTRY-COUNT  *                                  GA2LPGM 
01129              GC-GCTABULR-ACON-VARY-LEN).                          GA2LPGM 
01130                                                                   GA2LPGM 
01131      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2LPGM 
01132            GC-GCIOPARM-LEN      +  GCIO-RECORD-LENGTH.            GA2LPGM 
01133                                                                   GA2LPGM 
01134      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2LPGM 
01135         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2LPGM 
01136         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2LPGM 
01137                                                                   GA2LPGM 
01138      IF NOT GCIO-GOOD-RETURN                                      GA2LPGM 
01139         MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASGA2LPGM 
01140 -    'E CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE          GA2LPGM 
01141         MOVE '2LF2'  TO  WS-ABEND-CODE                            GA2LPGM 
01142         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01143                                                                   GA2LPGM 
01144      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2LPGM 
01145         VARYING MAP-IDX  FROM 1  BY  1                            GA2LPGM 
01146            UNTIL  MAP-IDX  >  WS-MAP-ROW.                         GA2LPGM 
01147                                                                   GA2LPGM 
01148 ******************************************************************GA2LPGM 
01149 ** THE SAME BMS MAP IS BEING USED FOR THE ADD SCREEN AND FOR    **GA2LPGM 
01150 ** THE INQUIRY SCREEN.  THE ADD SCREEN IS INPUT-ONLY AND        **GA2LPGM 
01151 ** PFKEY PAGING IS AN OUTPUT FUNCTION, SO ALL MESSAGES AND      **GA2LPGM 
01152 ** FIELDS HAVING TO DO WITH PAGING ARE SUPPRESSED ON THE        **GA2LPGM 
01153 ** ADD SCREEN, AND THE CURSOR IS SET TO THE FIRST INPUT FIELD.  **GA2LPGM 
01154 ******************************************************************GA2LPGM 
01155      MOVE DFHBMASD TO MAP-SELECT-TXT1-ATTR                        GA2LPGM 
01156                       MAP-SELECT-ATTR                             GA2LPGM 
01157                       MAP-SELECT-FROM-ATTR                        GA2LPGM 
01158                       MAP-SELECT-TXT2-ATTR                        GA2LPGM 
01159                       MAP-SELECT-TO-ATTR                          GA2LPGM 
01160                       MAP-SELECT-TXT3-ATTR                        GA2LPGM 
01161                       MAP-SELECT-OF-ATTR                          GA2LPGM 
01162                       MAP-SELECT-TXT4-ATTR                        GA2LPGM 
01163                       MAP-SELECT-TXT5-ATTR.                       GA2LPGM 
01164      MOVE -1 TO MAP-PROCEDURE-ARGUMENT-LEN (1).                   GA2LPGM 
01165                                                                   GA2LPGM 
01166      EXEC CICS SEND   MAP('GA2LI01') MAPSET('GA2LSET') ERASE      GA2LPGM 
01167         FROM(GA2LI01O) CURSOR END-EXEC.                           GA2LPGM 
01168                                                                   GA2LPGM 
01169  2099-EXIT.   EXIT.                                               GA2LPGM 
01170 /                                                                 GA2LPGM 
01171 ******************************************************************GA2LPGM 
01172 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2LPGM 
01173 **                                                                GA2LPGM 
01174 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2LPGM 
01175 **  ALREADY ON THE OPERATORS SCREEN.                              GA2LPGM 
01176 **                                                                GA2LPGM 
01177 ******************************************************************GA2LPGM 
01178  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2LPGM 
01179                                                                   GA2LPGM 
01180      MOVE LOW-VALUES  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)        GA2LPGM 
01181                           MAP-CODE-FUNCTION (MAP-IDX).            GA2LPGM 
01182                                                                   GA2LPGM 
01183  2199-EXIT.   EXIT.                                               GA2LPGM 
01184 /                                                                 GA2LPGM 
01185 ******************************************************************GA2LPGM 
01186 **          X C T L   T O   D E L   S C R E E N                   GA2LPGM 
01187 **                                                                GA2LPGM 
01188 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2LPGM 
01189 ** DELETING ENTRIES.  WE READ THE ALL LEVEL TABULAR RECORD & PASS GA2LPGM 
01190 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, & ALL LEVEL TABULARGA2LPGM 
01191 ** RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU THE      GA2LPGM 
01192 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA2LPGM 
01193 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2LPGM 
01194 ******************************************************************GA2LPGM 
01195  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2LPGM 
01196      MOVE '3000'  TO  WS-PARA-ID.                                 GA2LPGM 
01197                                                                   GA2LPGM 
01198      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2LPGM 
01199            GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +   GA2LPGM 
01200            GC-GCTABULR-ACON-FIXED-LEN +                           GA2LPGM 
01201           (GC-GCTABULR-ACON-VARY-LEN    *                         GA2LPGM 
01202           GC-GCTABULR-ACON-VARY-MAX-OCUR).                        GA2LPGM 
01203                                                                   GA2LPGM 
01204 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA2LPGM 
01205      EXEC CICS GETMAIN                                            GA2LPGM 
01206         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2LPGM 
01207         INITIMG(WS-HEX-00)                                        GA2LPGM 
01208         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2LPGM 
01209 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2LPGM 
01210 ***  ADD ALL-LEVEL-TAB-PNTR, 4096 GIVING  ALL-LEVEL-TAB-PNTR2.    GA2LPGM 
01211                                                                   GA2LPGM 
01212      MOVE LOW-VALUES  TO  GCIO-WORKFILE-KEY.                      GA2LPGM 
01213                                                                   GA2LPGM 
01214 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA2LPGM 
01215 *    EXEC CICS GETMAIN                                            GA2LPGM 
01216 *       SET(ADDRESS OF GCA-COMMAREA)                              GA2LPGM 
01217 *       INITIMG(WS-HEX-00)                                        GA2LPGM 
01218 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA2LPGM 
01219 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2LPGM 
01220                                                                   GA2LPGM 
01221      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA2LPGM 
01222         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2LPGM 
01223         MOVE  'G'   TO  GCIO-WRK-STATUS-CODE                      GA2LPGM 
01224         MOVE  'G3'  TO  GCIO-WRK-RECORD-TYPE                      GA2LPGM 
01225         MOVE MAP-ID-LINE TO GROUP-SPECIFIC-ID-LINE                GA2LPGM 
01226         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA2LPGM 
01227         MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM          GA2LPGM 
01228         MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM        GA2LPGM 
01229         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA2LPGM 
01230         MOVE SPACES  TO  GCA-L-O-B                                GA2LPGM 
01231                          GCA-PROV-CTL                             GA2LPGM 
01232                          GCA-BEN-PROV-ID                          GA2LPGM 
01233                          GCIO-WRK-LINE-OF-BUS                     GA2LPGM 
01234                          GCIO-WRK-PROVIDER-CONTROL                GA2LPGM 
01235        MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL GA2LPGM 
01236        MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN           GA2LPGM 
01237        MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID                GA2LPGM 
01238                            GCIO-WRK-PROVISION-ID                  GA2LPGM 
01239         MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT           GA2LPGM 
01240                            GCIO-WRK-PROVISION-SLOT-NO             GA2LPGM 
01241         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2LPGM 
01242         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2LPGM 
01243                                                                   GA2LPGM 
01244      IF  MAP-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA2LPGM 
01245         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2LPGM 
01246         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2LPGM 
01247         MOVE  'C3'  TO  GCIO-WRK-RECORD-TYPE                      GA2LPGM 
01248         MOVE MAP-ID-LINE TO CONTRACT-ID-LINE                      GA2LPGM 
01249         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2LPGM 
01250         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2LPGM 
01251         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2LPGM 
01252         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2LPGM 
01253         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2LPGM 
01254         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2LPGM 
01255         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2LPGM 
01256         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2LPGM 
01257         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2LPGM 
01258         MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID               GA2LPGM 
01259                            GCIO-WRK-PROVISION-ID                  GA2LPGM 
01260         MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT           GA2LPGM 
01261                            GCIO-WRK-PROVISION-SLOT-NO             GA2LPGM 
01262         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2LPGM 
01263         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2LPGM 
01264                                                                   GA2LPGM 
01265      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA2LPGM 
01266         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2LPGM 
01267         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2LPGM 
01268         MOVE  'C5'  TO  GCIO-WRK-RECORD-TYPE                      GA2LPGM 
01269         MOVE MAP-ID-LINE TO BENEFIT-PROVISION-ID-LINE             GA2LPGM 
01270         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2LPGM 
01271         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2LPGM 
01272         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2LPGM 
01273         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2LPGM 
01274         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2LPGM 
01275         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2LPGM 
01276         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2LPGM 
01277         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2LPGM 
01278         MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID       GA2LPGM 
01279         MOVE MAP-TABULAR-ID TO GCA-ALL-LEVEL-TAB-ID               GA2LPGM 
01280                            GCIO-WRK-TAB-PROVISION-ID              GA2LPGM 
01281         MOVE MAP-TABULAR-SLOT TO GCA-ALL-LEVEL-TAB-SLOT           GA2LPGM 
01282                            GCIO-WRK-TAB-PROV-SLOT-NO              GA2LPGM 
01283         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.             GA2LPGM 
01284                                                                   GA2LPGM 
01285 *    MOVE WS-Y  TO  WS-YY.                                        GA2LPGM 
01286 *    IF  WS-M  >  2                                               GA2LPGM 
01287 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2LPGM 
01288 *          REMAINDER  WS-REMAINDER                                GA2LPGM 
01289 *    ELSE                                                         GA2LPGM 
01290 *       MOVE 1  TO  WS-REMAINDER.                                 GA2LPGM 
01291 *    SET WS-M-IDX  TO  WS-M.                                      GA2LPGM 
01292 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2LPGM 
01293 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2LPGM 
01294 *   IF  WS-REMAINDER  =  ZERO                                     GA2LPGM 
01295 *       ADD 1  TO  WS-DDD.                                        GA2LPGM 
01296 *                                                                 GA2LPGM 
01297 *    MOVE  WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE                  GA2LPGM 
01298 *                        GCA-EFF-DT.                              GA2LPGM 
01299      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA2LPGM 
01300      MOVE  SPACES  TO  GCA-INTERNAL-TAB-ID,                       GA2LPGM 
01301                        GCA-INTERNAL-TAB-SLOT,                     GA2LPGM 
01302                        GCA-ADD-DEL-IND,                           GA2LPGM 
01303                        GCA-ALL-LEVEL-TAB-FUNC-CODE,               GA2LPGM 
01304                        GCA-OCCURS-ENTRY-COUNTER.                  GA2LPGM 
01305      MOVE  MAP-FROM-MENU-ID TO GCA-FROM-MENU-ID.                  GA2LPGM 
01306      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2LPGM 
01307 ***  MOVE  ALL-LEVEL-TAB-PNTR TO GCA-RECORD-POINTER.              GA2LPGM 
01308                                                                   GA2LPGM 
01309      SET GCA-RECORD-POINTER TO ADDRESS                            GA2LPGM 
01310      OF  IO-PARM-ALL-LVL-TAB-RECORD.                              GA2LPGM 
01311                                                                   GA2LPGM 
01312      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2LPGM 
01313      TO   GAI-ENTRY-COUNT.                                        GA2LPGM 
01314                                                                   GA2LPGM 
01315      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2LPGM 
01316                                                                   GA2LPGM 
01317      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2LPGM 
01318         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2LPGM 
01319         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2LPGM 
01320                                                                   GA2LPGM 
01321      IF  NOT GCIO-GOOD-RETURN                                     GA2LPGM 
01322         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2LPGM 
01323 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2LPGM 
01324         MOVE '2LF3'  TO  WS-ABEND-CODE                            GA2LPGM 
01325         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01326                                                                   GA2LPGM 
01327 *    SET COMMAREA-PNTR TO ADDRESS                                 GA2LPGM 
01328 *    OF  GCA-COMMAREA.                                            GA2LPGM 
01329 *                                                                 GA2LPGM 
01330 *    EXEC CICS XCTL  PROGRAM('GA1LPGM') COMMAREA(COMMAREA-PNTR)   GA2LPGM 
01331 *       LENGTH(4)  END-EXEC.                                      GA2LPGM 
01332                                                                   GA2LPGM 
01333      EXEC CICS XCTL  PROGRAM('GA1LPGM')                           GA2LPGM 
01334                      COMMAREA(DFHCOMMAREA)                        GA2LPGM 
01335                      LENGTH(LENGTH OF DFHCOMMAREA)                GA2LPGM 
01336      END-EXEC.                                                    GA2LPGM 
01337  3099-EXIT.   EXIT.                                               GA2LPGM 
01338 /                                                                 GA2LPGM 
01339 ***************************************************************** GA2LPGM 
01340 **          D I S P L A Y   F I R S T   S C R E E N               GA2LPGM 
01341 **                                                                GA2LPGM 
01342 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE DELETE GA2LPGM 
01343 ** PROGRAM, THAT PROGRAM WILL PASS THE ADDRESS OF A PARAMETER LISTGA2LPGM 
01344 ** CONTAINING THE FIELDS FROM THE HEADER OF THE SCREEN.           GA2LPGM 
01345 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2LPGM 
01346 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2LPGM 
01347 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2LPGM 
01348 ******************************************************************GA2LPGM 
01349  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2LPGM 
01350      MOVE '4000'  TO  WS-PARA-ID.                                 GA2LPGM 
01351                                                                   GA2LPGM 
01352 ***  MOVE LOW-VALUES TO SCREEN                                    GA2LPGM 
01353                                                                   GA2LPGM 
01354      MOVE LOW-VALUES TO GA2LI01I.                                 GA2LPGM 
01355                                                                   GA2LPGM 
01356      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2LPGM 
01357         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2LPGM 
01358            TO MAP-ERROR-MESSAGE                                   GA2LPGM 
01359         MOVE '2LC1'  TO  WS-ABEND-CODE                            GA2LPGM 
01360         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01361                                                                   GA2LPGM 
01362 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA2LPGM 
01363 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2LPGM 
01364                                                                   GA2LPGM 
01365 *    SET ADDRESS OF GCA-COMMAREA                                  GA2LPGM 
01366 *    TO  INCOMING-COMMAREA-PNTR.                                  GA2LPGM 
01367                                                                   GA2LPGM 
01368      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-TABULAR-ID.               GA2LPGM 
01369      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-TABULAR-SLOT.           GA2LPGM 
01370      MOVE GCA-FROM-MENU-ID TO FRMNUIDO.                           GA2LPGM 
01371                                                                   GA2LPGM 
01372 ******************************************************************GA2LPGM 
01373 ** THE SAME BMS MAP IS BEING USED FOR THE ADD SCREEN AND FOR    **GA2LPGM 
01374 ** THE INQUIRY SCREEN.  THE ADD SCREEN IS INPUT-ONLY AND        **GA2LPGM 
01375 ** PFKEY PAGING IS AN OUTPUT FUNCTION, SO ALL MESSAGES AND      **GA2LPGM 
01376 ** FIELDS HAVING TO DO WITH PAGING ARE SUPPRESSED ON THE        **GA2LPGM 
01377 ** ADD SCREEN, AND THE CURSOR IS SET TO THE FIRST INPUT FIELD.  **GA2LPGM 
01378 ******************************************************************GA2LPGM 
01379      MOVE DFHBMASD TO MAP-SELECT-TXT1-ATTR                        GA2LPGM 
01380                       MAP-SELECT-ATTR                             GA2LPGM 
01381                       MAP-SELECT-FROM-ATTR                        GA2LPGM 
01382                       MAP-SELECT-TXT2-ATTR                        GA2LPGM 
01383                       MAP-SELECT-TO-ATTR                          GA2LPGM 
01384                       MAP-SELECT-TXT3-ATTR                        GA2LPGM 
01385                       MAP-SELECT-OF-ATTR                          GA2LPGM 
01386                       MAP-SELECT-TXT4-ATTR                        GA2LPGM 
01387                       MAP-SELECT-TXT5-ATTR.                       GA2LPGM 
01388      MOVE -1 TO MAP-PROCEDURE-ARGUMENT-LEN (1).                   GA2LPGM 
01389                                                                   GA2LPGM 
01390      IF  GCA-FROM-MENU-ID = 'GS3A'                                GA2LPGM 
01391         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-MAIN-TITLE        GA2LPGM 
01392         MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2LPGM 
01393         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA2LPGM 
01394         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA2LPGM 
01395         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2LPGM 
01396         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA2LPGM 
01397         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2LPGM 
01398         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2LPGM 
01399         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2LPGM 
01400         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2LPGM 
01401                                                                   GA2LPGM 
01402      IF  GCA-FROM-MENU-ID = 'GC4A' OR 'GTM1'                      GA2LPGM 
01403         MOVE CONTRACT-TITLE-LINE  TO  MAP-MAIN-TITLE              GA2LPGM 
01404         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2LPGM 
01405         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA2LPGM 
01406         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA2LPGM 
01407         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2LPGM 
01408         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA2LPGM 
01409         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2LPGM 
01410         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2LPGM 
01411         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2LPGM 
01412         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2LPGM 
01413         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2LPGM 
01414         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2LPGM 
01415         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2LPGM 
01416         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2LPGM 
01417                                                                   GA2LPGM 
01418      IF  GCA-FROM-MENU-ID = 'GC8A'                                GA2LPGM 
01419         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-MAIN-TITLE     GA2LPGM 
01420         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA2LPGM 
01421         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA2LPGM 
01422         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA2LPGM 
01423         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA2LPGM 
01424         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA2LPGM 
01425         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2LPGM 
01426         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA2LPGM 
01427         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2LPGM 
01428         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA2LPGM 
01429         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2LPGM 
01430         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA2LPGM 
01431         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2LPGM 
01432         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA2LPGM 
01433         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2LPGM 
01434                                                                   GA2LPGM 
01435      EXEC CICS SEND   MAP('GA2LI01') MAPSET('GA2LSET') ERASE      GA2LPGM 
01436         FROM(GA2LI01O) CURSOR END-EXEC.                           GA2LPGM 
01437                                                                   GA2LPGM 
01438  4099-EXIT.   EXIT.                                               GA2LPGM 
01439 /                                                                 GA2LPGM 
01440 ***************************************************************** GA2LPGM 
01441 **        X C T L   T O   P R E V I O U S   M E N U               GA2LPGM 
01442 **                                                                GA2LPGM 
01443 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2LPGM 
01444 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD &       GA2LPGM 
01445 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA2LPGM 
01446 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM       GA2LPGM 
01447 ** MENU ID' FIELD).                                               GA2LPGM 
01448 ******************************************************************GA2LPGM 
01449  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2LPGM 
01450      MOVE '5000'  TO  WS-PARA-ID.                                 GA2LPGM 
01451                                                                   GA2LPGM 
01452      IF  MAP-FROM-MENU-ID = 'GS3A'                                GA2LPGM 
01453         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA2LPGM 
01454                                                                   GA2LPGM 
01455      IF  MAP-FROM-MENU-ID = 'GC4A'                                GA2LPGM 
01456         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA2LPGM 
01457                                                                   GA2LPGM 
01458      IF  MAP-FROM-MENU-ID = 'GC8A'                                GA2LPGM 
01459         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA2LPGM 
01460                                                                   GA2LPGM 
01461      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA2LPGM 
01462         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA2LPGM 
01463                                                                   GA2LPGM 
01464                                                                   GA2LPGM 
01465                                                                   GA2LPGM 
01466                                                                   GA2LPGM 
01467  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA2LPGM 
01468      MOVE '5010'  TO  WS-PARA-ID.                                 GA2LPGM 
01469                                                                   GA2LPGM 
01470      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2LPGM 
01471          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2LPGM 
01472                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2LPGM 
01473                                                                   GA2LPGM 
01474 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA2LPGM 
01475      EXEC CICS GETMAIN                                            GA2LPGM 
01476         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA2LPGM 
01477         INITIMG(WS-HEX-00)                                        GA2LPGM 
01478         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01479 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA2LPGM 
01480                                                                   GA2LPGM 
01481      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2LPGM 
01482                                                                   GA2LPGM 
01483      MOVE 'G'                   TO GCIO-WRK-STATUS-CODE.          GA2LPGM 
01484      MOVE 'G2'                  TO GCIO-WRK-RECORD-TYPE.          GA2LPGM 
01485      MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            GA2LPGM 
01486      MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            GA2LPGM 
01487      MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM             GA2LPGM 
01488 *    MOVE GRP-SPEC-GROUP-NO     TO GCIO-WRK-GROUP-NO.             GA2LPGM 
01489      MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM.          GA2LPGM 
01490 *    MOVE GRP-SPEC-SECTION-NO   TO GCIO-WRK-SECTION-NO.           GA2LPGM 
01491      MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             GA2LPGM 
01492      MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS,          GA2LPGM 
01493                                    GCIO-WRK-PROVIDER-CONTROL.     GA2LPGM 
01494      MOVE GRP-SPEC-FAM-REL-LVL  TO GCIO-WRK-FAMILY-RELATION-LVL.  GA2LPGM 
01495      MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            GA2LPGM 
01496 *    MOVE GRP-SPEC-EFF-DATE  TO  WS-MDY.                          GA2LPGM 
01497                                                                   GA2LPGM 
01498 *     MOVE WS-Y  TO  WS-YY.                                       GA2LPGM 
01499 *     IF  WS-M  >  2                                              GA2LPGM 
01500 *        DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                   GA2LPGM 
01501 *           REMAINDER  WS-REMAINDER                               GA2LPGM 
01502 *     ELSE                                                        GA2LPGM 
01503 *        MOVE 1  TO  WS-REMAINDER.                                GA2LPGM 
01504 *     SET WS-M-IDX  TO  WS-M.                                     GA2LPGM 
01505 *     MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                 GA2LPGM 
01506 *     COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                         GA2LPGM 
01507 *     IF  WS-REMAINDER  =  ZERO                                   GA2LPGM 
01508 *        ADD 1  TO  WS-DDD.                                       GA2LPGM 
01509 *                                                                 GA2LPGM 
01510 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2LPGM 
01511      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA2LPGM 
01512      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2LPGM 
01513                       GCIO-WRK-TAB-PROVISION-ID.                  GA2LPGM 
01514      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2LPGM 
01515                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2LPGM 
01516      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2LPGM 
01517                                                                   GA2LPGM 
01518      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA2LPGM 
01519      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA2LPGM 
01520                                                                   GA2LPGM 
01521      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      GA2LPGM 
01522                                                                   GA2LPGM 
01523      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2LPGM 
01524         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA2LPGM 
01525         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01526                                                                   GA2LPGM 
01527      IF  NOT GCIO2-GOOD-RETURN                                    GA2LPGM 
01528         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2LPGM 
01529 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2LPGM 
01530         MOVE '2LF4'  TO  WS-ABEND-CODE                            GA2LPGM 
01531         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01532                                                                   GA2LPGM 
01533      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2LPGM 
01534          GC-WORKFILE-KEY-LEN        +                             GA2LPGM 
01535                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2LPGM 
01536                                                                   GA2LPGM 
01537      EXEC CICS XCTL PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)    GA2LPGM 
01538         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01539                                                                   GA2LPGM 
01540      GO TO 5099-EXIT.                                             GA2LPGM 
01541                                                                   GA2LPGM 
01542  5020-XCTL-TO-CONTRACT-MENU.                                      GA2LPGM 
01543      MOVE '5020'  TO  WS-PARA-ID.                                 GA2LPGM 
01544                                                                   GA2LPGM 
01545      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2LPGM 
01546          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2LPGM 
01547                 GC-GCCONTR-MAX-REC-LEN.                           GA2LPGM 
01548                                                                   GA2LPGM 
01549 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA2LPGM 
01550      EXEC CICS GETMAIN                                            GA2LPGM 
01551         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA2LPGM 
01552         INITIMG(WS-HEX-00)                                        GA2LPGM 
01553         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01554 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA2LPGM 
01555 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA2LPGM 
01556                                                                   GA2LPGM 
01557      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2LPGM 
01558                                                                   GA2LPGM 
01559      MOVE 'C'               TO GCIO-WRK-STATUS-CODE.              GA2LPGM 
01560      MOVE 'C2'              TO GCIO-WRK-RECORD-TYPE.              GA2LPGM 
01561      MOVE GCA-PLAN-CODE     TO GCIO-WRK-PLAN-CODE.                GA2LPGM 
01562      MOVE GCA-GROUP-NUM     TO GCIO-WRK-GROUP-NUM.                GA2LPGM 
01563      MOVE GCA-SECTION-NUM   TO GCIO-WRK-SECTION-NUM               GA2LPGM 
01564      MOVE GCA-PKG-CODE      TO GCIO-WRK-PKG-CODE.                 GA2LPGM 
01565      MOVE GCA-L-O-B         TO GCIO-WRK-LINE-OF-BUS.              GA2LPGM 
01566      MOVE GCA-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL.         GA2LPGM 
01567      MOVE GCA-FAM-REL-LVL   TO GCIO-WRK-FAMILY-RELATION-LVL.      GA2LPGM 
01568      MOVE GCA-EFFDT-CEN     TO GCIO-WRK-EFFDT-CEN.                GA2LPGM 
01569                                                                   GA2LPGM 
01570 *    MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NO.               GA2LPGM 
01571 *    MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NO.           GA2LPGM 
01572 *    MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2LPGM 
01573 *    MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2LPGM 
01574 *    MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2LPGM 
01575 *    MOVE CONTRACT-EFF-DATE  TO  WS-MDY.                          GA2LPGM 
01576                                                                   GA2LPGM 
01577 *    MOVE WS-Y  TO  WS-YY.                                        GA2LPGM 
01578 *    IF  WS-M  >  2                                               GA2LPGM 
01579 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2LPGM 
01580 *          REMAINDER  WS-REMAINDER                                GA2LPGM 
01581 *    ELSE                                                         GA2LPGM 
01582 *       MOVE 1  TO  WS-REMAINDER.                                 GA2LPGM 
01583 *    SET WS-M-IDX  TO  WS-M.                                      GA2LPGM 
01584 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2LPGM 
01585 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2LPGM 
01586 *    IF  WS-REMAINDER  =  ZERO                                    GA2LPGM 
01587 *       ADD 1  TO  WS-DDD.                                        GA2LPGM 
01588 *                                                                 GA2LPGM 
01589 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2LPGM 
01590      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA2LPGM 
01591      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2LPGM 
01592                       GCIO-WRK-TAB-PROVISION-ID.                  GA2LPGM 
01593      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2LPGM 
01594                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2LPGM 
01595      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA2LPGM 
01596                                                                   GA2LPGM 
01597      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA2LPGM 
01598      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA2LPGM 
01599                                                                   GA2LPGM 
01600      MOVE 'RD '  TO  GCIO3-FILE-ACCESS-CODE.                      GA2LPGM 
01601                                                                   GA2LPGM 
01602      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2LPGM 
01603         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA2LPGM 
01604         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01605                                                                   GA2LPGM 
01606      IF  NOT GCIO3-GOOD-RETURN                                    GA2LPGM 
01607         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2LPGM 
01608 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2LPGM 
01609         MOVE '2LF5'  TO  WS-ABEND-CODE                            GA2LPGM 
01610         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01611                                                                   GA2LPGM 
01612      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2LPGM 
01613          GC-WORKFILE-KEY-LEN        +                             GA2LPGM 
01614                 GC-GCCONTR-MAX-REC-LEN.                           GA2LPGM 
01615                                                                   GA2LPGM 
01616      EXEC CICS XCTL PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)    GA2LPGM 
01617         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01618                                                                   GA2LPGM 
01619      GO TO 5099-EXIT.                                             GA2LPGM 
01620                                                                   GA2LPGM 
01621  5030-XCTL-TO-BEN-PROV-MENU.                                      GA2LPGM 
01622      MOVE '5030'  TO  WS-PARA-ID.                                 GA2LPGM 
01623                                                                   GA2LPGM 
01624      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2LPGM 
01625          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2LPGM 
01626                 GC-GCBENPRV-MAX-REC-LEN.                          GA2LPGM 
01627                                                                   GA2LPGM 
01628 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA2LPGM 
01629      EXEC CICS GETMAIN                                            GA2LPGM 
01630         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA2LPGM 
01631         INITIMG(WS-HEX-00)                                        GA2LPGM 
01632         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01633 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA2LPGM 
01634                                                                   GA2LPGM 
01635      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2LPGM 
01636                                                                   GA2LPGM 
01637      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2LPGM 
01638      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA2LPGM 
01639      MOVE GCA-PLAN-CODE     TO GCIO-WRK-PLAN-CODE.                GA2LPGM 
01640      MOVE GCA-GROUP-NUM     TO GCIO-WRK-GROUP-NUM.                GA2LPGM 
01641      MOVE GCA-SECTION-NUM   TO GCIO-WRK-SECTION-NUM               GA2LPGM 
01642      MOVE GCA-PKG-CODE      TO GCIO-WRK-PKG-CODE.                 GA2LPGM 
01643      MOVE GCA-L-O-B         TO GCIO-WRK-LINE-OF-BUS.              GA2LPGM 
01644      MOVE GCA-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL.         GA2LPGM 
01645      MOVE GCA-FAM-REL-LVL   TO GCIO-WRK-FAMILY-RELATION-LVL.      GA2LPGM 
01646      MOVE GCA-EFFDT-CEN     TO GCIO-WRK-EFFDT-CEN.                GA2LPGM 
01647      MOVE GCA-BEN-PROV-ID   TO GCIO-WRK-PROVISION-ID.             GA2LPGM 
01648 *                                                                 GA2LPGM 
01649 *    MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NO.               GA2LPGM 
01650 *    MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NO.           GA2LPGM 
01651 *    MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2LPGM 
01652 *    MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2LPGM 
01653 *    MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2LPGM 
01654 *    MOVE BEN-PROV-EFF-DATE  TO  WS-MDY.                          GA2LPGM 
01655 *    MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID.              GA2LPGM 
01656 *                                                                 GA2LPGM 
01657 *    MOVE WS-Y  TO  WS-YY.                                        GA2LPGM 
01658 *    IF  WS-M  >  2                                               GA2LPGM 
01659 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2LPGM 
01660 *          REMAINDER  WS-REMAINDER                                GA2LPGM 
01661 *    ELSE                                                         GA2LPGM 
01662 *       MOVE 1  TO  WS-REMAINDER.                                 GA2LPGM 
01663 *    SET WS-M-IDX  TO  WS-M.                                      GA2LPGM 
01664 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2LPGM 
01665 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2LPGM 
01666 *    IF  WS-REMAINDER  =  ZERO                                    GA2LPGM 
01667 *       ADD 1  TO  WS-DDD.                                        GA2LPGM 
01668 *                                                                 GA2LPGM 
01669 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2LPGM 
01670      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA2LPGM 
01671      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA2LPGM 
01672      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA2LPGM 
01673      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2LPGM 
01674      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA2LPGM 
01675                                                                   GA2LPGM 
01676      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA2LPGM 
01677      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA2LPGM 
01678                                                                   GA2LPGM 
01679      MOVE 'RD '  TO  GCIO4-FILE-ACCESS-CODE.                      GA2LPGM 
01680                                                                   GA2LPGM 
01681      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2LPGM 
01682         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA2LPGM 
01683         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01684                                                                   GA2LPGM 
01685      IF  NOT GCIO4-GOOD-RETURN                                    GA2LPGM 
01686         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2LPGM 
01687 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2LPGM 
01688         MOVE '2LF6'  TO  WS-ABEND-CODE                            GA2LPGM 
01689         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2LPGM 
01690                                                                   GA2LPGM 
01691      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2LPGM 
01692          GC-WORKFILE-KEY-LEN        +                             GA2LPGM 
01693                 GC-GCBENPRV-MAX-REC-LEN.                          GA2LPGM 
01694                                                                   GA2LPGM 
01695      EXEC CICS XCTL PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)    GA2LPGM 
01696         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2LPGM 
01697                                                                   GA2LPGM 
01698      GO TO 5099-EXIT.                                             GA2LPGM 
01699                                                                   GA2LPGM 
01700                                                                   GA2LPGM 
01701  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA2LPGM 
01702      MOVE '5040'  TO  WS-PARA-ID.                                 GA2LPGM 
01703                                                                   GA2LPGM 
01704      EXEC CICS XCTL                                               GA2LPGM 
01705                PROGRAM('GTM1PGM')                                 GA2LPGM 
01706                END-EXEC.                                          GA2LPGM 
01707                                                                   GA2LPGM 
01708      GO  TO  5099-EXIT.                                           GA2LPGM 
01709                                                                   GA2LPGM 
01710  5099-EXIT.                                                       GA2LPGM 
01711      EXIT.                                                        GA2LPGM 
01712 /                                                                 GA2LPGM 
01713 ***************************************************************** GA2LPGM 
01714 **           X C T L   T O   M A I N   M E N U                    GA2LPGM 
01715 **                                                                GA2LPGM 
01716 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2LPGM 
01717 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2LPGM 
01718 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2LPGM 
01719 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2LPGM 
01720 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2LPGM 
01721 ** AND PROGRESS DOWN.                                             GA2LPGM 
01722 ******************************************************************GA2LPGM 
01723  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2LPGM 
01724      MOVE '6000'  TO  WS-PARA-ID.                                 GA2LPGM 
01725      MOVE '2LP1'  TO  WS-ABEND-CODE.                              GA2LPGM 
01726                                                                   GA2LPGM 
01727      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2LPGM 
01728                                                                   GA2LPGM 
01729  6099-EXIT.     EXIT.                                             GA2LPGM 
01730 /                                                                 GA2LPGM 
01731 /   E R R O R   M E S S A G E   T H E N   A B E N D               GA2LPGM 
01732 ******************************************************************GA2LPGM 
01733  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2LPGM 
01734                                                                   GA2LPGM 
01735      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (7).                 GA2LPGM 
01736      EXEC CICS SEND   MAP('GA2LI01') MAPSET('GA2LSET') ERASE      GA2LPGM 
01737         FROM(GA2LI01O) CURSOR WAIT END-EXEC.                      GA2LPGM 
01738                                                                   GA2LPGM 
01739      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2LPGM 
01740                                                                   GA2LPGM 
01741  9999-EXIT.     EXIT.                                             GA2LPGM 
