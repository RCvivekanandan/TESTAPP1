00001  ID DIVISION.                                                     01/12/06
00002  PROGRAM-ID.     GA2JPGM.                                         GA2JPGM 
00003 *** THIS IS A COBOL II PROGRAM                                       LV004
00004  AUTHOR.         S BUCH.                                          GA2JPGM 
00005  DATE-WRITTEN.   02/07/85.                                        GA2JPGM 
00006  DATE-COMPILED.                                                   GA2JPGM 
00007      SKIP3                                                        GA2JPGM 
00008 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2JPGM 
00009 *** * * * * * * +-------------------------+ * * * * * * * * * * **GA2JPGM 
00010 *** * * * * * * |   U P D A T E   L O G   | * * * * * * * * * * **GA2JPGM 
00011 *** * * * * * * +-------------------------+ * * * * * * * * * * **GA2JPGM 
00012 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2JPGM 
00013 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2JPGM 
00014 *                                                                 GA2JPGM 
00015 *           01-21-86    ENW   CHANGED WS-CONTRACT LENGTHS AND     GA2JPGM 
00016 *                             REMOVED ALL HANDLE CONDITIONS EXCEPTGA2JPGM 
00017 *                             FOR MAPFAIL                         GA2JPGM 
00018 *                                                                 GA2JPGM 
00019 *   T529    03-19-86    MDD   ADDED CODE TO CHECK RETURN CODE FROMGA2JPGM 
00020 *                             GCVIOPGM FOR A VALUE OF '20', THIS  GA2JPGM 
00021 *                             MEANS THE EDIT TABLE IS EMPTY AND A GA2JPGM 
00022 *                             VALIDATION COULD NOT BE PERFORMED.  GA2JPGM 
00023 *                             PF4/16 CAN BE USED TO ACCEPT THE    GA2JPGM 
00024 *                             DATA AS SHOWN ON THE SCREEN AND TO  GA2JPGM 
00025 *                             CONTINUE PROCESSING.                GA2JPGM 
00026 *                                                                 GA2JPGM 
00027 *  EL500    06/18/86    DES   FIXED PF4/16 CODE TO ACCEPT EMPTY   GA2JPGM 
00028 *                             VALIDATION TABLE CONDITION ONLY,    GA2JPGM 
00029 *                             ALL OTHER ERRORS STILL MUST BE FIXEDGA2JPGM 
00030 *                                                                 GA2JPGM 
00031 *  D137     08/26/86    JLA   1. DARKEN  SELECT FIELD.            GA2JPGM 
00032 *                             2. DARKEN LOCATION COUNTERS.        GA2JPGM 
00033 *                             3. USE USER DEFINED LOGICAL MAP FOR GA2JPGM 
00034 *                                SCREEN. THIS REPLACES THE PARTIALGA2JPGM 
00035 *                                USE OF BMS MAP AND USER DEFINED. GA2JPGM 
00036 *                                                                 GA2JPGM 
00037 *  D0120    01/29/87    JLA  CHANGES FOR SINGLE TABULAR SUPPORT   GA2JPGM 
00038 *                            EXECUTED FROM TRANSACTION GTM1:      GA2JPGM 
00039 *                            1. PF1/PF13 - CONSTRUCT COMMAREA AS  GA2JPGM 
00040 *                               IF GC4A HAD CALLED, XCTL TO ADD   GA2JPGM 
00041 *                               SCREEN PROGRAM.                   GA2JPGM 
00042 *                            2. PF3/PF15 - CONSTRUCT COMMAREA AS  GA2JPGM 
00043 *                               IF GC4A HAD CALLED, XCTL TO       GA2JPGM 
00044 *                               GTM1PGM.                          GA2JPGM 
00045 *                                                                *GA2JPGM 
00046 *  D116      8/17/87    FRY    CAPTURE OPERATOR-ID WHEN A 'C3',  *GA2JPGM 
00047 *                              'C5', OR 'G3' RECORD IS UPDATED.  *GA2JPGM 
00048 *                                                                *GA2JPGM 
00049 *                                                                *GA2JPGM 
00050 * D1013 10/06/87  FCG  REMOVE LINK TO CSEXECIO  AND REPLACE WITH *GA2JPGM 
00051 *                      LINK TO GCPPDIO. REPLACED LOGIC CODE     * GA2JPGM 
00052 *                      TO PROCESS WITH NEW INTERFACE PROGRAM.   * GA2JPGM 
00053 *                                                               * GA2JPGM 
00054 *                                                                *GA2JPGM 
00055 *  11161  10/24/90  ENW   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA2JPGM 
00056 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA2JPGM 
00057 *                         ROUTINES. REPLACED HARD CODED LENGTHS. *GA2JPGM 
00058 *                                                               * GA2JPGM 
00059 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD     * GA2JPGM 
00060 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2JPGM 
00061 *                                                               * GA2JPGM 
00062 *D12009 09/27/91  GDM   CONVERT TO COBOL II                     * GA2JPGM 
00063 *                                                               * GA2JPGM 
00064 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION      * GA2JPGM 
00065 *                                                               * GA2JPGM 
00066 *   D365A   05/06/03    GTF EXPAND PROCEDURE ARGUMENT FROM 6 TO * GA2JPGM 
00067 *                           7 BYTES. CHANGE # OF OCCURS TO 396  * GA2JPGM 
00068 *                           ON #ADIP TABULAR.                   * GA2JPGM 
00068 *                                                                *GA2JPGM 
00068 *ICD-10  07/06/11  BA  EXPAND MAP-SELECT FIELD FROM 6 TO 7 BYTES.*GA2JPGM 
00068 *                      EXPAND PROCED-CODE FROM 5 TO 7 BYTES.     *GA2JPGM 
00068 *                      CHANGE LOGIC FOR ICD-10 REQUIREMENTS.     *GA2JPGM 
00069 ***************************************************************** GA2JPGM 
00070 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2JPGM 
00071 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2JPGM 
00072      SKIP3                                                        GA2JPGM 
00073      SKIP3                                                        GA2JPGM 
00074 ******************************************************************GA2JPGM 
00075 *   GA2JPGM   ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM    GA2JPGM 
00076 *                           DENTAL INPATIENT            GA2J      GA2JPGM 
00077 *                                                                 GA2JPGM 
00078 *     THIS PROGRAM WILL ADD ENTRIES TO THE DENTAL INPATIENT       GA2JPGM 
00079 *   PROCEDURE ARGUMENT/CODE FUNCTION OF ALL LEVEL TABULAR RECORD. GA2JPGM 
00080 *                                                                 GA2JPGM 
00081 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2JPGM 
00082 *   TO ADD ENTRIES TO THIS PARTICULAR ALL LEVEL TABULAR RECORD.   GA2JPGM 
00083 *   THE PROGRAM READS THE ENTRIES, & VALIDATES THE FORMAT OF EACH GA2JPGM 
00084 *   FIELD IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN   GA2JPGM 
00085 *   ERROR).  IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES GA2JPGM 
00086 *   IN ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER GA2JPGM 
00087 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2JPGM 
00088 *   ENTRIES FOR THIS ALL LEVEL TABULAR RECORD.                    GA2JPGM 
00089 *                                                                 GA2JPGM 
00090 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID:#ADIP) GA2JPGM 
00091 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2JPGM 
00092 *   XCTL TO TRANS GA1J OR PROGRAM GA1JPGM.  THIS PROGRAM WILL     GA2JPGM 
00093 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2JPGM 
00094 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2JPGM 
00095 *   CODE.                                                         GA2JPGM 
00096 *                                                                 GA2JPGM 
00097 *                                                                 GA2JPGM 
00098 *   FUNC CODE: GA2J                                               GA2JPGM 
00099 *   MAPSET:    GA2JSETC  <<<< REDEFINED BY USER DEFINED MAP >>>>  GA2JPGM 
00100 *   FILES:     GCPSWORK                                           GA2JPGM 
00101 ******************************************************************GA2JPGM 
00102      SKIP3                                                        GA2JPGM 
00103  ENVIRONMENT DIVISION.                                            GA2JPGM 
00104 /                                                                 GA2JPGM 
00105  DATA DIVISION.                                                   GA2JPGM 
00106  WORKING-STORAGE SECTION.                                         GA2JPGM 
00107  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2JPGM 
00108      '***GA2JPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2JPGM 
00109  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2JPGM 
00110                                                                   GA2JPGM 
00111  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2JPGM 
00112                                                                   GA2JPGM 
00113                                                                   GA2JPGM 
00114 ******************************************************************GA2JPGM 
00115 ** THE FIELDS LISTED BELOW ARE USED WHEN CALLING THE PROCEDURE  **GA2JPGM 
00116 ** OR DIAGNOSIS HCSC FILES.                                     **GA2JPGM 
00117 ******************************************************************GA2JPGM 
00118  01  PROCED-KEY.                                                  GA2JPGM 
00119      03  SYSTEM-INDICATOR        PIC X(1) VALUE SPACE.            GA2JPGM 
      *** ICD-10 START                                                          
00120      03  PROCED-CODE             PIC X(7).                        GA2JPGM 
00121      03  PROCEDR-DIGIT REDEFINES PROCED-CODE                      GA2JPGM 
00122              OCCURS 7 TIMES      PIC X(1).                        GA2JPGM 
      *** ICD-10 END                                                            
00123                                                                   GA2JPGM 
00124  01  WS-PRO-HAF-COMM-LEN         PIC S9(4)  COMP  VALUE +344.     GA2JPGM 
00125                                                                   GA2JPGM 
00126                                                                   GA2JPGM 
00127 ** MAP COBOL SCREEN DSECTS **                                     GA2JPGM 
00128  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2JPGM 
00129      '***  I/O MAPAREA ***'.                                      GA2JPGM 
00130  COPY GA2JSETC.                                                   GA2JPGM 
00131 /*****************************************************************GA2JPGM 
00132 ******************************************************************GA2JPGM 
00133 ******************************************************************GA2JPGM 
00134 **                                                              **GA2JPGM 
00135 **    THIS IS A USER DEFINED LOGICAL MAP.  ANY CHANGES TO       **GA2JPGM 
00136 **     MAPSET GA2JSETC AFFECTING IT\
00137 **     FOR HERE.                                                **GA2JPGM 
00138 **                                                              **GA2JPGM 
00139 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA2JPGM 
00140 **                                                              **GA2JPGM 
00141 **                                            JLA 8/26/86       **GA2JPGM 
00142 **                                                              **GA2JPGM 
00143 ******************************************************************GA2JPGM 
00144 ******************************************************************GA2JPGM 
00145 ******************************************************************GA2JPGM 
00146                                                                   GA2JPGM 
00147  01  MAP-USER-DEFINED     REDEFINES   GA2JI01I.                   GA2JPGM 
00148                                                                   GA2JPGM 
00149      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA2JPGM 
00150                                                                   GA2JPGM 
00151      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA2JPGM 
00152      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA2JPGM 
00153      05  MAP-FUNCTION-CODE                PIC X(04).              GA2JPGM 
00154                                                                   GA2JPGM 
00155      05  MAP-TITLE-LINE-LEN               PIC S9(4) COMP SYNC.    GA2JPGM 
00156      05  MAP-TITLE-LINE-ATTR              PIC X.                  GA2JPGM 
00157      05  MAP-TITLE-LINE                   PIC X(43).              GA2JPGM 
00158                                                                   GA2JPGM 
00159      05  MAP-ADD-INQUIRE-LEN              PIC S9(4) COMP SYNC.    GA2JPGM 
00160      05  MAP-ADD-INQUIRE-ATTR             PIC X.                  GA2JPGM 
00161      05  MAP-ADD-INQUIRE                  PIC X(03).              GA2JPGM 
00162                                                                   GA2JPGM 
00163      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA2JPGM 
00164      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA2JPGM 
00165      05  MAP-SCREEN-ID                    PIC X(06).              GA2JPGM 
00166                                                                   GA2JPGM 
00167      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA2JPGM 
00168      05  MAP-ID-LINE-ATTR                 PIC X.                  GA2JPGM 
00169      05  MAP-ID-LINE                      PIC X(79).              GA2JPGM 
00170      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA2JPGM 
00171          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA2JPGM 
00172          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA2JPGM 
00173          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA2JPGM 
00174          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA2JPGM 
00175          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA2JPGM 
00176          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA2JPGM 
00177          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA2JPGM 
00178          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA2JPGM 
00179          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA2JPGM 
00180          10  FILLER                           PIC X(18).          GA2JPGM 
00181      05  CONTRACT-ID-LINE  REDEFINES  MAP-ID-LINE.                GA2JPGM 
00182          10  CONTRACT-ID-HEADING              PIC X(14).          GA2JPGM 
00183          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA2JPGM 
00184          10  CONTRACT-GROUP-NO                PIC X(6).           GA2JPGM 
00185          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA2JPGM 
00186          10  CONTRACT-SECTION-NO              PIC X(4).           GA2JPGM 
00187          10  CONTRACT-LOB-HEADING             PIC X(6).           GA2JPGM 
00188          10  CONTRACT-LOB                     PIC X.              GA2JPGM 
00189          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA2JPGM 
00190          10  CONTRACT-PROV-CTL                PIC XX.             GA2JPGM 
00191          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA2JPGM 
00192          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA2JPGM 
00193          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA2JPGM 
00194          10  CONTRACT-EFF-DATE                PIC X(6).           GA2JPGM 
00195          10  FILLER                           PIC X(09).          GA2JPGM 
00196      05  BENEFIT-PROVISION-ID-LINE  REDEFINES  MAP-ID-LINE.       GA2JPGM 
00197          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA2JPGM 
00198          10  BEN-PROV-GROUP-NO                PIC X(6).           GA2JPGM 
00199          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA2JPGM 
00200          10  BEN-PROV-SECTION-NO              PIC X(4).           GA2JPGM 
00201          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA2JPGM 
00202          10  BEN-PROV-LOB                     PIC X.              GA2JPGM 
00203          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA2JPGM 
00204          10  BEN-PROV-PROV-CTL                PIC XX.             GA2JPGM 
00205          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA2JPGM 
00206          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA2JPGM 
00207          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA2JPGM 
00208          10  BEN-PROV-EFF-DATE                PIC X(6).           GA2JPGM 
00209          10  BEN-PROV-ID-HEADING              PIC X(8).           GA2JPGM 
00210          10  BEN-PROV-ID-NO                   PIC X(6).           GA2JPGM 
00211          10  FILLER                           PIC X(09).          GA2JPGM 
00212                                                                   GA2JPGM 
00213      05  MAP-ALL-LEVEL-TAB-ID-LEN         PIC S9(4) COMP SYNC.    GA2JPGM 
00214      05  MAP-ALL-LEVEL-TAB-ID-ATTR        PIC X.                  GA2JPGM 
00215      05  MAP-ALL-LEVEL-TAB-ID             PIC X(06).              GA2JPGM 
00216                                                                   GA2JPGM 
00217      05  MAP-ALL-LEVEL-TAB-SLOT-LEN       PIC S9(4) COMP SYNC.    GA2JPGM 
00218      05  MAP-ALL-LEVEL-TAB-SLOT-ATTR      PIC X.                  GA2JPGM 
00219      05  MAP-ALL-LEVEL-TAB-SLOT           PIC X(07).              GA2JPGM 
00220                                                                   GA2JPGM 
00221      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA2JPGM 
00222      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA2JPGM 
00223      05  MAP-FROM-MENU-ID                 PIC X(04).              GA2JPGM 
00224                                                                   GA2JPGM 
00225      05  MAP-SELECT-LABEL-LEN             PIC S9(4) COMP SYNC.    GA2JPGM 
00226      05  MAP-SELECT-LABEL-ATTR            PIC X.                  GA2JPGM 
00227      05  MAP-SELECT-LABEL                 PIC X(07).              GA2JPGM 
00228                                                                   GA2JPGM 
00229      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA2JPGM 
00230      05  MAP-SELECT-ATTR                  PIC X.                  GA2JPGM 
00231      05  MAP-SELECT                       PIC X(07).              GA2JPGM 
00232                                                                   GA2JPGM 
00233      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA2JPGM 
00234      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA2JPGM 
00235      05  MAP-SELECT-FROM                  PIC X(03).              GA2JPGM 
00236                                                                   GA2JPGM 
00237      05  MAP-SELECT-TO-LABEL-LEN          PIC S9(4) COMP SYNC.    GA2JPGM 
00238      05  MAP-SELECT-TO-LABEL-ATTR         PIC X.                  GA2JPGM 
00239      05  MAP-SELECT-TO-LABEL              PIC X(02).              GA2JPGM 
00240                                                                   GA2JPGM 
00241      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA2JPGM 
00242      05  MAP-SELECT-TO-ATTR               PIC X.                  GA2JPGM 
00243      05  MAP-SELECT-TO                    PIC X(03).              GA2JPGM 
00244                                                                   GA2JPGM 
00245      05  MAP-SELECT-OF-LABEL-LEN          PIC S9(4) COMP SYNC.    GA2JPGM 
00246      05  MAP-SELECT-OF-LABEL-ATTR         PIC X.                  GA2JPGM 
00247      05  MAP-SELECT-OF-LABEL              PIC X(02).              GA2JPGM 
00248                                                                   GA2JPGM 
00249      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA2JPGM 
00250      05  MAP-SELECT-OF-ATTR               PIC X.                  GA2JPGM 
00251      05  MAP-SELECT-OF                    PIC X(03).              GA2JPGM 
00252                                                                   GA2JPGM 
00253      05  MAP-SELECT-DISPLAY-LABEL-LEN     PIC S9(4) COMP SYNC.    GA2JPGM 
00254      05  MAP-SELECT-DISPLAY-LABEL-ATTR    PIC X.                  GA2JPGM 
00255      05  MAP-SELECT-DISPLAY-LABEL         PIC X(24).              GA2JPGM 
00256                                                                   GA2JPGM 
00257      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA2JPGM 
00258          INDEXED BY MAP-IDX.                                      GA2JPGM 
00259          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA2JPGM 
00260          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA2JPGM 
00261          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA2JPGM 
00262          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA2JPGM 
00263          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA2JPGM 
00264          15  MAP-CODE-FUNCTION            PIC X(3).               GA2JPGM 
00265                                                                   GA2JPGM 
00266      05  MAP-PAGING-LABEL-LEN             PIC S9(4) COMP SYNC.    GA2JPGM 
00267      05  MAP-PAGING-LABEL-ATTR            PIC X.                  GA2JPGM 
00268      05  MAP-PAGING-LABEL                 PIC X(79).              GA2JPGM 
00269                                                                   GA2JPGM 
00270      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA2JPGM 
00271      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA2JPGM 
00272      05  MAP-ERROR-MESSAGE                PIC X(79).              GA2JPGM 
00273      SKIP2                                                        GA2JPGM 
00274  01  FILLER.                                                      GA2JPGM 
00275 ****************************************************************  GA2JPGM 
00276 **   THIS FIELD DESCRIBES THE NUMBER OF OCCURS FROM MAP.          GA2JPGM 
00277 ****************************************************************  GA2JPGM 
00278      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +15.  GA2JPGM 
00279 /                                                                 GA2JPGM 
00280 ** ALTERNATIVE WORKFILE KEYS **                                   GA2JPGM 
00281  01  FILLER                      PIC X(32)  VALUE                 GA2JPGM 
00282      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2JPGM 
00283  01  WS-ALT-WORKFILE-KEYS.                                        GA2JPGM 
00284  COPY GCWRKKEY.                                                   GA2JPGM 
00285 /                                                                 GA2JPGM 
00286 ** HARDCOPY WORK AREA **                                          GA2JPGM 
00287  01  FILLER                      PIC X(26)  VALUE                 GA2JPGM 
00288      '*** HARDCOPY WORK AREA ***'.                                GA2JPGM 
00289 *01  WS-HARDCOPY-COMMAREA.                                        GA2JPGM 
00290 *COPY PRNCOBOL.                                                   GA2JPGM 
00291                                                                   GA2JPGM 
00292                                                                   GA2JPGM 
00293 ** WORKFIELDS **                                                  GA2JPGM 
00294  01  FILLER                           PIC X(16)                   GA2JPGM 
00295              VALUE  '** WORKFIELDS **'.                           GA2JPGM 
00296  01  WS-WORK-FIELDS.                                              GA2JPGM 
00297      05  WS-HEX-00                    PIC X.                      GA2JPGM 
00298      05  WS-QUOTIENT                  PIC 999  COMP-3.            GA2JPGM 
00299      05  WS-REMAINDER                 PIC 999  COMP-3.            GA2JPGM 
00300      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2JPGM 
00301 ***  05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2JPGM 
00302 ***    VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2JPGM 
00303                                                                   GA2JPGM 
00304  01  WS-TEST-AREA                     PIC X(7).                   GA2JPGM 
00305  01  WS-TEST-DATA REDEFINES WS-TEST-AREA.                         GA2JPGM 
00306      05  WS-TEST-DETAIL OCCURS 7 TIMES PIC X(1).                  GA2JPGM 
00307          88  WS-NON-SPECIAL-CHARACTERS VALUE                      GA2JPGM 
00308                                        SPACE                      GA2JPGM 
00309                                        '0' THRU '9'               GA2JPGM 
00310                                        'A' THRU 'I'               GA2JPGM 
00311                                        'J' THRU 'R'               GA2JPGM 
00312                                        'S' THRU 'Z'.              GA2JPGM 
00313                                                                   GA2JPGM 
00314 ****************************************************************  GA2JPGM 
00315 ** THIS GROUP MATCHES ONE OCCURENCE OF AN ENTRY IN THE TABULAR    GA2JPGM 
00316 ** RECORD.  IT IS A SAVE AREA FOR PROCESSING.                     GA2JPGM 
00317 ****************************************************************  GA2JPGM 
00318      05  WS-SAVED-FIELDS.                                         GA2JPGM 
00319        10  WS-SAVED-PROCEDURE-ARGUMENT   PIC X(7).                GA2JPGM 
00320        10  WS-SAVED-CODE-FUNCTION        PIC X(3).                GA2JPGM 
00321 ****************************************************************  GA2JPGM 
00322 ** THIS IS THE AREA IN WHICH THE SORTING OF NEW ENTRIES HAPPENS.  GA2JPGM 
00323 ** ONE MORE OCCURENCE IS PROVIDED THAN IS FOUND ON THE SCREEN,    GA2JPGM 
00324 ** THIS IS FOR THE TRAILER.                                       GA2JPGM 
00325 ****************************************************************  GA2JPGM 
00326      05  WS-SORTED-TAB     OCCURS 16 TIMES INDEXED BY             GA2JPGM 
00327          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2JPGM 
00328        10  WS-PROCEDURE-ARGUMENT      PIC X(7).                   GA2JPGM 
00329        10  WS-CODE-FUNCTION           PIC X(3).                   GA2JPGM 
00330                                                                   GA2JPGM 
00331 *** SWITCHES ***                                                  GA2JPGM 
00332  01  FILLER                           PIC X(14)                   GA2JPGM 
00333              VALUE  '** SWITCHES **'.                             GA2JPGM 
00334  01  WS-SWITCHES.                                                 GA2JPGM 
00335      05  WS-ERROR-SW                  PIC X.                      GA2JPGM 
00336                                                                   GA2JPGM 
00337 ** TITLE LINES **                                                 GA2JPGM 
00338  01  WS-TITLE-LINES.                                              GA2JPGM 
00339      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(43)  VALUE    GA2JPGM 
00340          '   GROUP SPECIFIC CONDITIONAL PROCEDURES   '.           GA2JPGM 
00341      05  CONTRACT-TITLE-LINE                  PIC X(43)  VALUE    GA2JPGM 
00342          '      CONTRACT CONDITIONAL PROCEDURES      '.           GA2JPGM 
00343      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(43)  VALUE    GA2JPGM 
00344          '  BENEFIT PROVISION CONDITIONAL PROCEDURES '.           GA2JPGM 
00345                                                                   GA2JPGM 
00346 /                                                                 GA2JPGM 
00347 *** RECORD LENGTHS ***                                            GA2JPGM 
00348  01  FILLER                           PIC X(20)                   GA2JPGM 
00349              VALUE  '** RECORD LENGTHS **'.                       GA2JPGM 
00350  01  WS-RECORD-LENGTHS.                                           GA2JPGM 
00351     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA2JPGM 
00352     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA2JPGM 
00353     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2JPGM 
00354     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2JPGM 
00355     05 GCVI-COMMAREA-LEN              PIC S9(4) COMP   VALUE +19. GA2JPGM 
00356 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA2JPGM 
00357 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA2JPGM 
00358 *   05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA2JPGM 
00359 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2JPGM 
00360 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA2JPGM 
00361 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA2JPGM 
00362 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2JPGM 
00363 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA2JPGM 
00364 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA2JPGM 
00365 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2JPGM 
00366 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA2JPGM 
00367 ****************************************************************  GA2JPGM 
00368 **   THESE FIELDS DESCRIBE THE TABULAR RECORD.                    GA2JPGM 
00369 ****************************************************************  GA2JPGM 
00370 *   05 GC-GCTABULR-ADIP-FIXED-LEN     PIC S9(5) COMP-3 VALUE +40. GA2JPGM 
00371 *   05 GC-GCTABULR-ADIP-VARY-LEN      PIC S9(5) COMP-3 VALUE +9.  GA2JPGM 
00372 *   05 GC-GCTABULR-ADIP-VARY-MAX-OCUR PIC S9(5) COMP-3 VALUE +440.GA2JPGM 
00373 /                                                                 GA2JPGM 
00374  COPY COBXIO.                                                     GA2JPGM 
00375 /                                                                 GA2JPGM 
00376 ** ATTRIBUTES **                                                  GA2JPGM 
00377  COPY DFHBMSCA.                                                   GA2JPGM 
00378      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2JPGM 
00379 /                                                                 GA2JPGM 
00380 ** ATTENTION IDENTIFIERS **                                       GA2JPGM 
00381  COPY DFHAID.                                                     GA2JPGM 
00382 /                                                                 GA2JPGM 
00383  01  GCVIOPGMS-PARM.                                              GA2JPGM 
00384  COPY GCVINTRC.                                                   GA2JPGM 
00385 /                                                                 GA2JPGM 
00386  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2JPGM 
00387 /                                                                 GA2JPGM 
00388  01  COMMAREA-POINTER-AREA.                                       GA2JPGM 
00389      05  COMMAREA-PNTR-COMP PIC S9(8) COMP.                       GA2JPGM 
00390      05  COMMAREA-PNTR      REDEFINES                             GA2JPGM 
00391          COMMAREA-PNTR-COMP USAGE IS POINTER.                     GA2JPGM 
00392 /                                                                 GA2JPGM 
00393  01  WS-GCPS-LENGTHS.                                             GA2JPGM 
00394      COPY GCCDRLEN.                                               GA2JPGM 
00395 /                                                                 GA2JPGM 
00396  01  WS-END                          PIC X(16)  VALUE             GA2JPGM 
00397      '*** W/S ENDS ***'.                                          GA2JPGM 
00398 /                                                                 GA2JPGM 
00399  LINKAGE SECTION.                                                 GA2JPGM 
00400 /                                                                 GA2JPGM 
00401  01  DFHCOMMAREA.                                                 GA2JPGM 
00402      COPY G2ALCKEC.                                               GA2JPGM 
00403 *    05  INCOMING-COMMAREA-PNTR-COMP PIC S9(8) COMP.              GA2JPGM 
00404 *    05  INCOMING-COMMAREA-PNTR      REDEFINES                    GA2JPGM 
00405 *        INCOMING-COMMAREA-PNTR-COMP USAGE IS POINTER.            GA2JPGM 
00406 *                                                                 GA2JPGM 
00407 *01  BLL-CELLS.                                                   GA2JPGM 
00408 *    02  FILLER                      PIC S9(8)  COMP.             GA2JPGM 
00409 *    02  COMMAREA-PNTR               PIC S9(8)  COMP.             GA2JPGM 
00410 *    02  ALL-LEVEL-TAB-PNTR          PIC S9(8)  COMP.             GA2JPGM 
00411 *    02  ALL-LEVEL-TAB-PNTR2         PIC S9(8)  COMP.             GA2JPGM 
00412 *    02  COPY-AREA-PNTR              PIC S9(8)  COMP.             GA2JPGM 
00413 *    02  GRP-SPEC-PNTR               PIC S9(8)  COMP.             GA2JPGM 
00414 *    02  CONTRACT-PNTR               PIC S9(8)  COMP.             GA2JPGM 
00415 *    02  CONTRACT-PNTR2              PIC S9(8)  COMP.             GA2JPGM 
00416 *    02  BEN-PROV-PNTR               PIC S9(8)  COMP.             GA2JPGM 
00417 *    02  GCPPDIO-BLL-PNTR            PIC S9(8)  COMP.             GA2JPGM 
00418 *                                                                 GA2JPGM 
00419 *01  GCA-COMMAREA.                                                GA2JPGM 
00420 *COPY G2ALCKEC.                                                   GA2JPGM 
00421 /                                                                 GA2JPGM 
00422 ** I/O PARM, WORKFILE KEY, AND CONTRACT TABULAR RECORD **         GA2JPGM 
00423  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA2JPGM 
00424  COPY GCIOPRM1.                                                   GA2JPGM 
00425 /                                                                 GA2JPGM 
00426  COPY GCWRKDCC.                                                   GA2JPGM 
00427 /                                                                 GA2JPGM 
00428  COPY GCTADIPC.                                                   GA2JPGM 
00429 /                                                                 GA2JPGM 
00430 ****************************************************************  GA2JPGM 
00431 ** THIS AREA HOLDS THE ENTRIES FROM THE TABLE FOR THE TABULAR     GA2JPGM 
00432 ** RECORD PRIOR TO BEING MERGED WITH SCREENS DATA BACK INTO THE   GA2JPGM 
00433 ** RECORD.                                                        GA2JPGM 
00434 ****************************************************************  GA2JPGM 
00435  01  COPY-OF-TABLE-AREA.                                          GA2JPGM 
00436      05  COPY-OF-TABLE    OCCURS 396 TIMES    INDEXED BY          GA2JPGM 
00437            COPY-IDX.                                              GA2JPGM 
00438        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA2JPGM 
00439        10  COPY-CODE-FUNCTION          PIC X(3).                  GA2JPGM 
00440 /                                                                 GA2JPGM 
00441 ** IO PARM, WITH WORKFILE KEY, AND RECORDS **                     GA2JPGM 
00442  01  IO-PARM-GRP-SPEC-RECORD.                                     GA2JPGM 
00443  COPY GCIOPRM2.                                                   GA2JPGM 
00444 /                                                                 GA2JPGM 
00445  COPY GCWRKDC2.                                                   GA2JPGM 
00446 /                                                                 GA2JPGM 
00447  COPY GCGROUPC.                                                   GA2JPGM 
00448 /                                                                 GA2JPGM 
00449                                                                   GA2JPGM 
00450  01  IO-PARM-CONTRACT-RECORD.                                     GA2JPGM 
00451  COPY GCIOPRM3.                                                   GA2JPGM 
00452 /                                                                 GA2JPGM 
00453  COPY GCWRKDC3.                                                   GA2JPGM 
00454 /                                                                 GA2JPGM 
00455  COPY GCCONTRC.                                                   GA2JPGM 
00456 /                                                                 GA2JPGM 
00457                                                                   GA2JPGM 
00458  01  IO-PARM-BEN-PROV-RECORD.                                     GA2JPGM 
00459  COPY GCIOPRM4.                                                   GA2JPGM 
00460 /                                                                 GA2JPGM 
00461  COPY GCWRKDC4.                                                   GA2JPGM 
00462 /                                                                 GA2JPGM 
00463  COPY GCBENPVC.                                                   GA2JPGM 
00464 /                                                                 GA2JPGM 
00465 ** IO PARM AREA **                                                GA2JPGM 
00466  01  GCPPDIO-PARM-AREA.                                           GA2JPGM 
00467  COPY GCPPDIOC.                                                   GA2JPGM 
00468                                                                   GA2JPGM 
00469  PROCEDURE DIVISION.                                              GA2JPGM 
00470                                                                   GA2JPGM 
00471 ******************************************************************GA2JPGM 
00472 **                H O U S E K E E P I N G                         GA2JPGM 
00473 **                                                                GA2JPGM 
00474 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2JPGM 
00475 **                                                                GA2JPGM 
00476 ******************************************************************GA2JPGM 
00477  0000-HOUSEKEEPING SECTION.                                       GA2JPGM 
00478                                                                   GA2JPGM 
00479      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2JPGM 
00480      IF EIBAID  =  DFHCLEAR                                       GA2JPGM 
00481          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2JPGM 
00482                         ERASE                                     GA2JPGM 
00483          END-EXEC                                                 GA2JPGM 
00484          EXEC CICS RETURN                                         GA2JPGM 
00485          END-EXEC.                                                GA2JPGM 
00486                                                                   GA2JPGM 
00487      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2JPGM 
00488                                   END-EXEC.                       GA2JPGM 
00489 /                                                                 GA2JPGM 
00490 ******************************************************************GA2JPGM 
00491 **                     M A I N L I N E                            GA2JPGM 
00492 **                                                                GA2JPGM 
00493 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2JPGM 
00494 **  TAKEN BY THE OPERATOR.                                        GA2JPGM 
00495 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2JPGM 
00496 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2JPGM 
00497 **     ADDITIONS FROM.                                            GA2JPGM 
00498 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2JPGM 
00499 **     KEY PF12 OR PF24.                                          GA2JPGM 
00500 **  3. RECEIVE THE SCREEN.                                        GA2JPGM 
00501 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2JPGM 
00502 **     MENU.                                                      GA2JPGM 
00503 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2JPGM 
00504 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2JPGM 
00505 **     (RETURN) TO THE DELETE PROGRAM (GA1JPGM).                  GA2JPGM 
00506 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2JPGM 
00507 **     (RETURN) TO THE PREVIOUS MENU.                             GA2JPGM 
00508 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2JPGM 
00509 **     NORMAL ADD PROCESSING, EXCEPT TO BYPASS AN EMPTY VALIDATIONGA2JPGM 
00510 **     TABLE CONDITION FOR THE COMBINATION CODE FIELD.            GA2JPGM 
00511 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2JPGM 
00512 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2JPGM 
00513 **                                                                GA2JPGM 
00514 ******************************************************************GA2JPGM 
00515  1000-MAIN-LINE SECTION.                                          GA2JPGM 
00516                                                                   GA2JPGM 
00517      MOVE '1000'  TO  WS-PARA-ID.                                 GA2JPGM 
00518      IF EIBTRNID  NOT =  'GA2J'                                   GA2JPGM 
00519         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2JPGM 
00520         GO TO 1099-RETURN.                                        GA2JPGM 
00521                                                                   GA2JPGM 
00522      EXEC CICS RECEIVE   MAP('GA2JI01') MAPSET('GA2JSET')         GA2JPGM 
00523         INTO(GA2JI01I) END-EXEC.                                  GA2JPGM 
00524                                                                   GA2JPGM 
00525      IF MAP-SCREEN-ID  NOT = '002J00'                             GA2JPGM 
00526         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2JPGM 
00527                                                                   GA2JPGM 
00528      IF EIBAID  =  DFHENTER                                       GA2JPGM 
00529         PERFORM 2000-ADD-PROCESSING                               GA2JPGM 
00530         GO TO 1099-RETURN.                                        GA2JPGM 
00531                                                                   GA2JPGM 
00532      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2JPGM 
00533         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2JPGM 
00534                                                                   GA2JPGM 
00535      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2JPGM 
00536         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2JPGM 
00537                                                                   GA2JPGM 
00538      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2JPGM 
00539         PERFORM 2000-ADD-PROCESSING                               GA2JPGM 
00540         GO TO 1099-RETURN.                                        GA2JPGM 
00541                                                                   GA2JPGM 
00542      SET MAP-IDX  TO  1.                                          GA2JPGM 
00543      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX).           GA2JPGM 
00544      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2JPGM 
00545 -    ' THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                  GA2JPGM 
00546      EXEC CICS SEND   MAP('GA2JI01') MAPSET('GA2JSET') DATAONLY   GA2JPGM 
00547         FROM(GA2JI01O) CURSOR END-EXEC.                           GA2JPGM 
00548                                                                   GA2JPGM 
00549  1099-RETURN.                                                     GA2JPGM 
00550 *    EXEC CICS RETURN   END-EXEC.                                 GA2JPGM 
00551      EXEC CICS RETURN TRANSID ('GA2J')                            GA2JPGM 
00552                COMMAREA (DFHCOMMAREA)                             GA2JPGM 
00553                END-EXEC.                                          GA2JPGM 
00554                                                                   GA2JPGM 
00555      GOBACK.                                                      GA2JPGM 
00556 /                                                                 GA2JPGM 
00557 ******************************************************************GA2JPGM 
00558 **               A D D   P R O C E S S I N G                      GA2JPGM 
00559 **                                                                GA2JPGM 
00560 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2JPGM 
00561 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2JPGM.            GA2JPGM 
00562 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2JPGM 
00563 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2JPGM 
00564 **     SKIP TO THE NEXT LINE.                                     GA2JPGM 
00565 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2JPGM 
00566 **     WITH SPECIAL CHARACTERS, AND NUMERIC FIELDS ARE TESTED     GA2JPGM 
00567 **     WITH THE NUMERIC CLASS TEST.  THE OPERATOR MUST ENTER SOME GA2JPGM 
00568 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2JPGM 
00569 **     DATA.                                                      GA2JPGM 
00570 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2JPGM 
00571 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2JPGM 
00572 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2JPGM 
00573 **     ORDER, FIELD BY FIELD.                                     GA2JPGM 
00574 **  6. THE ALL LEVEL TABULAR RECORD IS READ, AND A COPY OF THE    GA2JPGM 
00575 **     TABLE IS MADE.                                             GA2JPGM 
00576 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2JPGM 
00577 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2JPGM 
00578 **     BACK INTO THE RECORD.                                      GA2JPGM 
00579 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2JPGM 
00580 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2JPGM 
00581 **                                                                GA2JPGM 
00582 ******************************************************************GA2JPGM 
00583  2000-ADD-PROCESSING SECTION.                                     GA2JPGM 
00584                                                                   GA2JPGM 
00585      MOVE '2000'  TO  WS-PARA-ID.                                 GA2JPGM 
00586      MOVE 'N'     TO  WS-ERROR-SW.                                GA2JPGM 
00587      MOVE 'Y'     TO  GCVI-TABLE-SW.                              GA2JPGM 
00588      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2JPGM 
00589      SET MAP-IDX  TO  1.                                          GA2JPGM 
00590                                                                   GA2JPGM 
00591      MOVE '2005'  TO  WS-PARA-ID.                                 GA2JPGM 
00592  2005-RESET-ALL-ATTRIBUTES.                                       GA2JPGM 
00593      MOVE DFHBMUNF TO MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)       GA2JPGM 
00594                       MAP-CODE-FUNCTION-ATTR (MAP-IDX).           GA2JPGM 
00595      IF MAP-IDX   <  WS-MAP-ROW                                   GA2JPGM 
00596         SET MAP-IDX   UP BY  1                                    GA2JPGM 
00597         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2JPGM 
00598                                                                   GA2JPGM 
00599      SET MAP-IDX  TO  1.                                          GA2JPGM 
00600      MOVE '2010'  TO  WS-PARA-ID.                                 GA2JPGM 
00601  2010-VALIDATE-ADD-ENTRIES.                                       GA2JPGM 
00602      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX) = ZERO  AND          GA2JPGM 
00603         MAP-CODE-FUNCTION-LEN (MAP-IDX) = ZERO                    GA2JPGM 
00604                                                                   GA2JPGM 
00605         IF MAP-IDX   <  WS-MAP-ROW                                GA2JPGM 
00606            SET MAP-IDX   UP BY  1                                 GA2JPGM 
00607            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2JPGM 
00608         ELSE                                                      GA2JPGM 
00609            GO TO 2020-CHECK-FOR-ERRORS.                           GA2JPGM 
00610                                                                   GA2JPGM 
00611      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)  =  ZERO             GA2JPGM 
00612         MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  GA2JPGM 
00613         MOVE '???????'  TO MAP-PROCEDURE-ARGUMENT (MAP-IDX)       GA2JPGM 
00614         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2JPGM 
00615            MOVE 'Y'  TO  WS-ERROR-SW                              GA2JPGM 
00616            MOVE -1   TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)     GA2JPGM 
00617            MOVE ' *** PROCEDURE ARGUMENT IS INVALID ***'  TO      GA2JPGM 
00618                 MAP-ERROR-MESSAGE                                 GA2JPGM 
00619         ELSE                                                      GA2JPGM 
00620            NEXT SENTENCE                                          GA2JPGM 
00621      ELSE                                                         GA2JPGM 
00622         PERFORM 2015-EDIT-PROCEDURE-ARGUMENT                      GA2JPGM 
00623            THRU 2015-EXIT.                                        GA2JPGM 
00624                                                                   GA2JPGM 
00625      IF MAP-CODE-FUNCTION-LEN (MAP-IDX)  =  ZERO                  GA2JPGM 
00626         MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)       GA2JPGM 
00627         MOVE '???'  TO  MAP-CODE-FUNCTION (MAP-IDX)               GA2JPGM 
00628         IF WS-ERROR-SW  NOT  =  'Y'                               GA2JPGM 
00629            MOVE 'Y'  TO  WS-ERROR-SW                              GA2JPGM 
00630            MOVE -1   TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)          GA2JPGM 
00631            MOVE ' *** CODE FUNCTION IS INVALID ***'               GA2JPGM 
00632              TO  MAP-ERROR-MESSAGE                                GA2JPGM 
00633         ELSE                                                      GA2JPGM 
00634            NEXT SENTENCE                                          GA2JPGM 
00635      ELSE                                                         GA2JPGM 
00636         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2JPGM 
00637            WS-TEST-AREA                                           GA2JPGM 
00638 ***     MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2JPGM 
00639 ***                                       WS-SAVED-CODE-FUNCTION  GA2JPGM 
00640 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION  FROM  QUOTES  TO  '\
00641 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION  FROM                   GA2JPGM 
00642 ***                        WS-NON-SPECIAL-CHARACTERS  TO  QUOTES  GA2JPGM 
00643 ***     IF WS-SAVED-CODE-FUNCTION  NOT =  QUOTES                  GA2JPGM 
00644         IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                GA2JPGM 
00645                 WS-NON-SPECIAL-CHARACTERS  (2) AND                GA2JPGM 
00646                 WS-NON-SPECIAL-CHARACTERS  (3) AND                GA2JPGM 
00647                 WS-NON-SPECIAL-CHARACTERS  (4) AND                GA2JPGM 
00648                 WS-NON-SPECIAL-CHARACTERS  (5) AND                GA2JPGM 
00649                 WS-NON-SPECIAL-CHARACTERS  (6) AND                GA2JPGM 
00650                 WS-NON-SPECIAL-CHARACTERS  (7)                    GA2JPGM 
00651            MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)    GA2JPGM 
00652            IF WS-ERROR-SW  NOT =  'Y'                             GA2JPGM 
00653               MOVE 'Y'  TO  WS-ERROR-SW                           GA2JPGM 
00654               MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)        GA2JPGM 
00655               MOVE '        *** CODE FUNCTION IS INVALID ***'     GA2JPGM 
00656                 TO  MAP-ERROR-MESSAGE.                            GA2JPGM 
00657                                                                   GA2JPGM 
00658      IF MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  NOT =  DFHBMUBF    GA2JPGM 
00659                           AND                                     GA2JPGM 
00660         MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2JPGM 
00661         ADD 1  TO  WS-ADD-COUNT                                   GA2JPGM 
00662         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2JPGM 
00663         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                 GA2JPGM 
00664                              WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)  GA2JPGM 
00665         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2JPGM 
00666                                   WS-CODE-FUNCTION (WS-SORT-IDX). GA2JPGM 
00667                                                                   GA2JPGM 
00668      IF MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2JPGM 
00669         MOVE 'MULT02' TO  GCVI-FIELDS-KEY-ID                      GA2JPGM 
00670         MOVE ZEROES  TO  GCVI-RETURN-CODE                         GA2JPGM 
00671         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO  GCVI-VALUE-LEN-3    GA2JPGM 
00672         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2JPGM 
00673                        COMMAREA(GCVIOPGMS-PARM)                   GA2JPGM 
00674                        LENGTH(GCVI-COMMAREA-LEN) END-EXEC         GA2JPGM 
00675         IF GCVI-VALUE-NOT-FOUND                                   GA2JPGM 
00676            IF WS-ERROR-SW  NOT =  'Y'                             GA2JPGM 
00677               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2JPGM 
00678               MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)        GA2JPGM 
00679               MOVE '*** COMBINATION CODE INVALID ***'             GA2JPGM 
00680                 TO  MAP-ERROR-MESSAGE                             GA2JPGM 
00681               MOVE 'Y'  TO  WS-ERROR-SW                           GA2JPGM 
00682            ELSE                                                   GA2JPGM 
00683               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2JPGM 
00684         ELSE                                                      GA2JPGM 
00685            IF GCVI-VALUE-NOT-LOADED                               GA2JPGM 
00686               IF EIBAID  =  DFHPF4 OR  =  DFHPF16                 GA2JPGM 
00687                  NEXT SENTENCE                                    GA2JPGM 
00688               ELSE                                                GA2JPGM 
00689                  MOVE DFHBMUBF  TO                                GA2JPGM 
00690                                 MAP-CODE-FUNCTION-ATTR (MAP-IDX)  GA2JPGM 
00691                  IF WS-ERROR-SW  NOT =  'Y'                       GA2JPGM 
00692                     MOVE 'Y' TO  WS-ERROR-SW                      GA2JPGM 
00693                     MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)  GA2JPGM 
00694                     MOVE                                          GA2JPGM 
00695                    'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS PGA2JPGM 
00696 -                     'F4/PF16 TO CONTINUE'  TO MAP-ERROR-MESSAGE.GA2JPGM 
00697                                                                   GA2JPGM 
00698      IF MAP-IDX   <  WS-MAP-ROW                                   GA2JPGM 
00699         SET MAP-IDX   UP BY  1                                    GA2JPGM 
00700         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2JPGM 
00701 /                                                                 GA2JPGM 
00702 ****************************************************************  GA2JPGM 
00703 ** THIS GENERIC ROUTINE WILL EDIT THE PROCEDURE CODE. FIELDS  **  GA2JPGM 
00704 ** REQUIRING PROBABLE 'TAILORING' APPEAR AS -->NAME.          **  GA2JPGM 
00705 **                                                            **  GA2JPGM 
00706 ** REQUIRED WORKING-STORAGE:                                  **  GA2JPGM 
00707 **   1. 01  PROCED-KEY.                                       **  GA2JPGM 
00708 **          03  SYSTEM-INDICATOR          PIC X(1).           **  GA2JPGM 
00709 **          03  PROCED-CODE               PIC X(7).           **  GA2JPGM 
00710 **              04  PROCEDR-DIGIT REDEFINES PROCED-CODE       **  GA2JPGM 
00711 **                  OCCURS 7 TIMES        PIC X(1).           **  GA2JPGM 
00712 **                                                            **  GA2JPGM 
00713 **   2. 01  WS-PRO-HAF-COMM-LEN   PIC S9(4) COMP VALUE +344.  **  GA2JPGM 
00714 **                                                            **  GA2JPGM 
00715 **   3. COPY COBXIO.                                          **  GA2JPGM 
00716 ****************************************************************  GA2JPGM 
00717                                                                   GA2JPGM 
00718  2015-EDIT-PROCEDURE-ARGUMENT.                                    GA2JPGM 
00719                                                                   GA2JPGM 
00720 ***  MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2JPGM 
00721 ***                                  WS-SAVED-PROCEDURE-ARGUMENT. GA2JPGM 
00722                                                                   GA2JPGM 
00723      MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2JPGM 
00724          WS-TEST-AREA.                                            GA2JPGM 
00725                                                                   GA2JPGM 
00726 ***  TRANSFORM  WS-SAVED-PROCEDURE-ARGUMENT  FROM  QUOTES  TO '\
00727 ***  TRANSFORM  WS-SAVED-PROCEDURE-ARGUMENT  FROM                 GA2JPGM 
00728 ***                         WS-NON-SPECIAL-CHARACTERS  TO  QUOTES.GA2JPGM 
00729 ***  IF  WS-SAVED-PROCEDURE-ARGUMENT  NOT =  QUOTES               GA2JPGM 
00730                                                                   GA2JPGM 
00731      IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                   GA2JPGM 
00732              WS-NON-SPECIAL-CHARACTERS  (2) AND                   GA2JPGM 
00733              WS-NON-SPECIAL-CHARACTERS  (3) AND                   GA2JPGM 
00734              WS-NON-SPECIAL-CHARACTERS  (4) AND                   GA2JPGM 
00735              WS-NON-SPECIAL-CHARACTERS  (5) AND                   GA2JPGM 
00736              WS-NON-SPECIAL-CHARACTERS  (6) AND                   GA2JPGM 
00737              WS-NON-SPECIAL-CHARACTERS  (7)                       GA2JPGM 
00738          MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX) GA2JPGM 
00739          IF  WS-ERROR-SW  NOT =  'Y'                              GA2JPGM 
00740            MOVE 'Y'  TO  WS-ERROR-SW                              GA2JPGM 
00741            MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)      GA2JPGM 
00742            MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'   GA2JPGM 
00743              TO  MAP-ERROR-MESSAGE                                GA2JPGM 
00744            GO TO 2015-EXIT                                        GA2JPGM 
00745          ELSE                                                     GA2JPGM 
00746            GO TO 2015-EXIT.                                       GA2JPGM 
                                                                                
00749      EXEC CICS GETMAIN                                            GA2JPGM 
00750         SET(ADDRESS OF GCPPDIO-PARM-AREA)                         GA2JPGM 
00751         INITIMG(WS-HEX-00)                                        GA2JPGM 
00752         LENGTH(GCPPDIO-CA-LEN)                                    GA2JPGM 
00752      END-EXEC.                                                    GA2JPGM 
00753                                                                   GA2JPGM 
00754 ***  SERVICE RELOAD GCPPDIO-PARM-AREA.                            GA2JPGM 
                                                                                
           MOVE MAP-PROCEDURE-ARGUMENT(MAP-IDX) TO PROCED-CODE          GA2JPGM 
                                                                                
00747 *** ICD-10 START                                                  GA2JPGM 
           IF WS-TEST-AREA (1:3) = 'BIT'                                        
00780         MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX)   GA2JPGM 
00782         IF WS-ERROR-SW  NOT =  'Y'                                GA2JPGM 
00783            MOVE 'Y' TO  WS-ERROR-SW                               GA2JPGM 
00784            MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)       GA2JPGM 
00785            MOVE                                                   GA2JPGM 
00785      ' *** REQUESTED PCG BIT IS NOT ALLOWED FOR THIS TABULAR ***' GA2JPGM 
00786                                             TO  MAP-ERROR-MESSAGE GA2JPGM 
              END-IF                                                            
                                                                                
00772         GO TO 2015-EXIT.                                          GA2JPGM 
      *** ICD-10 END                                                            
                                                                                
00764      MOVE 'PRCDR03 '  TO  GCPPDIO-REQUEST-TYPE                    GA2JPGM 
                                                                                
00757      IF PROCEDR-DIGIT(5)  =  SPACE                                GA2JPGM 
00757         MOVE  'H'  TO  SYSTEM-INDICATOR                           GA2JPGM 
00757      ELSE                                                         GA2JPGM 
00757 *** ICD-10 START                                                  GA2JPGM 
00758         IF PROCEDR-DIGIT(7)  =  SPACE                             GA2JPGM 
00759            MOVE  'C'  TO  SYSTEM-INDICATOR                        GA2JPGM 
00760         ELSE                                                      GA2JPGM 
00761            MOVE  'Z'  TO  SYSTEM-INDICATOR                        GA2JPGM 
              END-IF                                                            
           END-IF                                                               
00757 *** ICD-10 END                                                    GA2JPGM 
      *    END-IF                                                               
                                                                                
00763      MOVE PROCED-KEY  TO  GCPPDIO-SERVICE-CODE-AREA.              GA2JPGM 
00765                                                                   GA2JPGM 
00766      EXEC CICS LINK PROGRAM('GCPPDIO')                            GA2JPGM 
00767           COMMAREA(GCPPDIO-PARM-AREA)                             GA2JPGM 
00768           LENGTH(GCPPDIO-CA-LEN)                                  GA2JPGM 
00769      END-EXEC.                                                    GA2JPGM 
00770                                                                   GA2JPGM 
00771      IF GCPPDIO-SUCCESSFUL                                        GA2JPGM 
00772          GO TO 2015-EXIT.                                         GA2JPGM 
00773                                                                   GA2JPGM 
00774      IF NOT GCPPDIO-REC-NOT-FOUND                                 GA2JPGM 
00775          MOVE 'DEW1'  TO  WS-ABEND-CODE                           GA2JPGM 
00776          MOVE GCPPDIO-RETURN-MESSAGE TO MAP-ERROR-MESSAGE         GA2JPGM 
00777          PERFORM 9999-ERROR-MSG-THEN-ABEND.                       GA2JPGM 
00778                                                                   GA2JPGM 
00780      MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX).     GA2JPGM 
00782      IF WS-ERROR-SW  NOT =  'Y'                                   GA2JPGM 
00783          MOVE 'Y' TO  WS-ERROR-SW                                 GA2JPGM 
00784          MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)         GA2JPGM 
00785          MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'     GA2JPGM 
00786            TO  MAP-ERROR-MESSAGE.                                 GA2JPGM 
00787                                                                   GA2JPGM 
00788  2015-EXIT. EXIT.                                                 GA2JPGM 
00789 /                                                                 GA2JPGM 
00790  2020-CHECK-FOR-ERRORS.                                           GA2JPGM 
00791      MOVE '2020'  TO  WS-PARA-ID.                                 GA2JPGM 
00792      IF WS-ERROR-SW  =  'Y' OR GCVI-TABLE-SW = 'N'                GA2JPGM 
00793         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA2JPGM 
00794                              MAP-TITLE-LINE                       GA2JPGM 
00795                              MAP-SCREEN-ID                        GA2JPGM 
00796                              MAP-ALL-LEVEL-TAB-ID                 GA2JPGM 
00797                              MAP-ALL-LEVEL-TAB-SLOT               GA2JPGM 
00798                              MAP-FROM-MENU-ID                     GA2JPGM 
00799                              MAP-ID-LINE                          GA2JPGM 
00800         MOVE '2100'  TO  WS-PARA-ID                               GA2JPGM 
00801         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2JPGM 
00802            VARYING MAP-IDX  FROM  1  BY  1                        GA2JPGM 
00803               UNTIL MAP-IDX  >  WS-MAP-ROW                        GA2JPGM 
00804                                                                   GA2JPGM 
00805         EXEC CICS SEND   MAP('GA2JI01') MAPSET('GA2JSET')         GA2JPGM 
00806            DATAONLY FROM(GA2JI01O) CURSOR END-EXEC                GA2JPGM 
00807         GO TO 2099-EXIT.                                          GA2JPGM 
00808                                                                   GA2JPGM 
00809      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2JPGM 
00810         MOVE '                  *** ADD ENTRY NOT FOUND ***'      GA2JPGM 
00811           TO  MAP-ERROR-MESSAGE                                   GA2JPGM 
00812         SET MAP-IDX  TO  1                                        GA2JPGM 
00813         MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)         GA2JPGM 
00814         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA2JPGM 
00815                              MAP-TITLE-LINE                       GA2JPGM 
00816                              MAP-SCREEN-ID                        GA2JPGM 
00817                              MAP-ALL-LEVEL-TAB-ID                 GA2JPGM 
00818                              MAP-ALL-LEVEL-TAB-SLOT               GA2JPGM 
00819                              MAP-FROM-MENU-ID                     GA2JPGM 
00820                              MAP-ID-LINE                          GA2JPGM 
00821         EXEC CICS SEND   MAP('GA2JI01') MAPSET('GA2JSET')         GA2JPGM 
00822            DATAONLY FROM(GA2JI01O) CURSOR END-EXEC                GA2JPGM 
00823         GO TO 2099-EXIT.                                          GA2JPGM 
00824                                                                   GA2JPGM 
00825  2025-CONTINUE-PROCESSING.                                        GA2JPGM 
00826                                                                   GA2JPGM 
00827      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2JPGM 
00828               GC-GCIOPARM-LEN + GC-WORKFILE-KEY-LEN +             GA2JPGM 
00829                         GC-GCTABULR-ADIP-FIXED-LEN +              GA2JPGM 
00830           (GC-GCTABULR-ADIP-VARY-LEN *                            GA2JPGM 
00831           GC-GCTABULR-ADIP-VARY-MAX-OCUR).                        GA2JPGM 
00832                                                                   GA2JPGM 
00833 ***  EXEC CICS GETMAIN SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00) GA2JPGM 
00834      EXEC CICS GETMAIN                                            GA2JPGM 
00835         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2JPGM 
00836         INITIMG(WS-HEX-00)                                        GA2JPGM 
00837         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2JPGM 
00838 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2JPGM 
00839 ***  ADD ALL-LEVEL-TAB-PNTR, 4096  GIVING  ALL-LEVEL-TAB-PNTR2.   GA2JPGM 
00840                                                                   GA2JPGM 
00841      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2JPGM 
00842         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2JPGM 
00843         MOVE  'G'                   TO GCIO-WRK-STATUS-CODE       GA2JPGM 
00844         MOVE  'G3'                  TO GCIO-WRK-RECORD-TYPE       GA2JPGM 
00845         MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE         GA2JPGM 
00846         MOVE GCA-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3      GA2JPGM 
00847         MOVE GRP-SPEC-GROUP-NO      TO GCIO-WRK-GROUP-NO          GA2JPGM 
00848         MOVE GCA-SEC-NO-1           TO GCIO-WRK-SEC-NO-1          GA2JPGM 
00849         MOVE GRP-SPEC-SECTION-NO    TO GCIO-WRK-SECTION-NO        GA2JPGM 
00850         MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE          GA2JPGM 
00851         MOVE SPACES                 TO GCIO-WRK-LINE-OF-BUS,      GA2JPGM 
00852                                      GCIO-WRK-PROVIDER-CONTROL    GA2JPGM 
00853         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2JPGM 
00854         MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN         GA2JPGM 
00855         MOVE MAP-ALL-LEVEL-TAB-ID   TO GCIO-WRK-PROVISION-ID      GA2JPGM 
00856         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA2JPGM 
00857         MOVE SPACES                 TO GCIO-WRK-TAB-PROVISION-ID  GA2JPGM 
00858         MOVE ZEROES                 TO GCIO-WRK-TAB-PROV-SLOT-NO. GA2JPGM 
00859                                                                   GA2JPGM 
00860      IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA2JPGM 
00861         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2JPGM 
00862         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2JPGM 
00863         MOVE  'C3'                  TO GCIO-WRK-RECORD-TYPE       GA2JPGM 
00864         MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE         GA2JPGM 
00865         MOVE GCA-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3      GA2JPGM 
00866         MOVE CONTRACT-GROUP-NO      TO GCIO-WRK-GROUP-NO          GA2JPGM 
00867         MOVE GCA-SEC-NO-1           TO GCIO-WRK-SEC-NO-1          GA2JPGM 
00868         MOVE CONTRACT-SECTION-NO    TO GCIO-WRK-SECTION-NO        GA2JPGM 
00869         MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE          GA2JPGM 
00870         MOVE CONTRACT-LOB           TO GCIO-WRK-LINE-OF-BUS       GA2JPGM 
00871         MOVE CONTRACT-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL  GA2JPGM 
00872         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2JPGM 
00873         MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN         GA2JPGM 
00874         MOVE MAP-ALL-LEVEL-TAB-ID   TO GCIO-WRK-PROVISION-ID      GA2JPGM 
00875         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA2JPGM 
00876         MOVE SPACES                 TO GCIO-WRK-TAB-PROVISION-ID  GA2JPGM 
00877         MOVE ZEROES                 TO GCIO-WRK-TAB-PROV-SLOT-NO. GA2JPGM 
00878                                                                   GA2JPGM 
00879      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2JPGM 
00880         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2JPGM 
00881         MOVE  'C'                   TO GCIO-WRK-STATUS-CODE       GA2JPGM 
00882         MOVE  'C5'                  TO GCIO-WRK-RECORD-TYPE       GA2JPGM 
00883         MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE         GA2JPGM 
00884         MOVE GCA-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3      GA2JPGM 
00885         MOVE BEN-PROV-GROUP-NO      TO GCIO-WRK-GROUP-NO          GA2JPGM 
00886         MOVE GCA-SEC-NO-1           TO GCIO-WRK-SEC-NO-1          GA2JPGM 
00887         MOVE BEN-PROV-SECTION-NO    TO GCIO-WRK-SECTION-NO        GA2JPGM 
00888         MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE          GA2JPGM 
00889         MOVE BEN-PROV-LOB           TO GCIO-WRK-LINE-OF-BUS       GA2JPGM 
00890         MOVE BEN-PROV-PROV-CTL      TO  GCIO-WRK-PROVIDER-CONTROL GA2JPGM 
00891         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2JPGM 
00892         MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN         GA2JPGM 
00893         MOVE BEN-PROV-ID-NO         TO GCIO-WRK-PROVISION-ID      GA2JPGM 
00894         MOVE 9999999                TO GCIO-WRK-PROVISION-SLOT-NO GA2JPGM 
00895         MOVE MAP-ALL-LEVEL-TAB-ID   TO GCIO-WRK-TAB-PROVISION-ID  GA2JPGM 
00896         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO. GA2JPGM 
00897                                                                   GA2JPGM 
00898      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA2JPGM 
00899      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2JPGM 
00900      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2JPGM 
00901                                                                   GA2JPGM 
00902      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2JPGM 
00903      TO   GAG-ENTRY-COUNT.                                        GA2JPGM 
00904                                                                   GA2JPGM 
00905      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2JPGM 
00906                                                                   GA2JPGM 
00907      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2JPGM 
00908         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2JPGM 
00909         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2JPGM 
00910                                                                   GA2JPGM 
00911      IF  NOT GCIO-GOOD-RETURN                                     GA2JPGM 
00912         MOVE '*** ERROR READING ALL LEVEL TABULAR.  CONTACT SYSTEMGA2JPGM 
00913 -    'S AREA ***'  TO  MAP-ERROR-MESSAGE                          GA2JPGM 
00914         MOVE '2JF1'  TO  WS-ABEND-CODE                            GA2JPGM 
00915         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
00916                                                                   GA2JPGM 
00917      IF  WS-ADD-COUNT  NOT >  ZERO                                GA2JPGM 
00918         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2JPGM 
00919                                                                   GA2JPGM 
00920      SET WS-SORT-IDX  TO  1.                                      GA2JPGM 
00921      SET WS-SORT-IDX2  TO  2.                                     GA2JPGM 
00922      MOVE '2030'  TO  WS-PARA-ID.                                 GA2JPGM 
00923                                                                   GA2JPGM 
00924  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2JPGM 
00925      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2JPGM 
00926         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2JPGM 
00927                                                                   GA2JPGM 
00928      IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <                     GA2JPGM 
00929         WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                      GA2JPGM 
00930         SET WS-SORT-IDX2  UP BY  1                                GA2JPGM 
00931         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2JPGM 
00932      ELSE                                                         GA2JPGM 
00933         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2JPGM 
00934            WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                   GA2JPGM 
00935            MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                    GA2JPGM 
00936               WS-SAVED-FIELDS                                     GA2JPGM 
00937            MOVE WS-SORTED-TAB (WS-SORT-IDX2) TO                   GA2JPGM 
00938               WS-SORTED-TAB (WS-SORT-IDX)                         GA2JPGM 
00939            MOVE WS-SAVED-FIELDS TO                                GA2JPGM 
00940               WS-SORTED-TAB (WS-SORT-IDX2)                        GA2JPGM 
00941            SET WS-SORT-IDX2  UP BY  1                             GA2JPGM 
00942            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2JPGM 
00943                                                                   GA2JPGM 
00944      IF WS-CODE-FUNCTION (WS-SORT-IDX) <                          GA2JPGM 
00945         WS-CODE-FUNCTION (WS-SORT-IDX2)                           GA2JPGM 
00946         SET WS-SORT-IDX2  UP BY  1                                GA2JPGM 
00947         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2JPGM 
00948      ELSE                                                         GA2JPGM 
00949         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2JPGM 
00950            WS-CODE-FUNCTION (WS-SORT-IDX2)                        GA2JPGM 
00951            MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                    GA2JPGM 
00952               WS-SAVED-FIELDS                                     GA2JPGM 
00953            MOVE WS-SORTED-TAB (WS-SORT-IDX2) TO                   GA2JPGM 
00954               WS-SORTED-TAB (WS-SORT-IDX)                         GA2JPGM 
00955            MOVE WS-SAVED-FIELDS TO                                GA2JPGM 
00956               WS-SORTED-TAB (WS-SORT-IDX2)                        GA2JPGM 
00957            SET WS-SORT-IDX2  UP BY  1                             GA2JPGM 
00958            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2JPGM 
00959                                                                   GA2JPGM 
00960      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2JPGM 
00961      MOVE WS-SORTED-TAB (WS-SORT-IDX3)                            GA2JPGM 
00962        TO WS-SORTED-TAB (WS-SORT-IDX2).                           GA2JPGM 
00963      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2JPGM 
00964      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2JPGM 
00965                                                                   GA2JPGM 
00966  2040-ARE-WE-DONE-WITH-SORT.                                      GA2JPGM 
00967      MOVE '2040'  TO  WS-PARA-ID.                                 GA2JPGM 
00968      SET WS-SORT-IDX  UP BY  1.                                   GA2JPGM 
00969      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2JPGM 
00970         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2JPGM 
00971         SET WS-SORT-IDX2  UP BY  1                                GA2JPGM 
00972         MOVE '2030'  TO  WS-PARA-ID                               GA2JPGM 
00973         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2JPGM 
00974                                                                   GA2JPGM 
00975      SET WS-ADD-COUNT  TO  WS-SORT-IDX.                           GA2JPGM 
00976      MOVE HIGH-VALUES  TO  WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)    GA2JPGM 
00977                            WS-CODE-FUNCTION (WS-SORT-IDX).        GA2JPGM 
00978      MOVE GAG-ENTRY-COUNT  TO  GAG-ENTRY-COUNT.                   GA2JPGM 
00979                                                                   GA2JPGM 
00980      COMPUTE  WS-COPY-LENGTH  =                                   GA2JPGM 
00981           GAG-ENTRY-COUNT  *  GC-GCTABULR-ADIP-VARY-LEN.          GA2JPGM 
00982                                                                   GA2JPGM 
00983 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA2JPGM 
00984      EXEC CICS GETMAIN                                            GA2JPGM 
00985         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA2JPGM 
00986         LENGTH(WS-COPY-LENGTH)                                    GA2JPGM 
00987         INITIMG(WS-HEX-00) END-EXEC.                              GA2JPGM 
00988 ***  SERVICE RELOAD  COPY-OF-TABLE-AREA.                          GA2JPGM 
00989                                                                   GA2JPGM 
00990      SET COPY-IDX,  GAG-INDEX  TO 1.                              GA2JPGM 
00991                                                                   GA2JPGM 
00992      MOVE '2050'  TO  WS-PARA-ID.                                 GA2JPGM 
00993  2050-MAKE-A-COPY-OF-RECORD.                                      GA2JPGM 
00994      IF GAG-INDEX  NOT >  GAG-ENTRY-COUNT                         GA2JPGM 
00995         MOVE GAG-ENTRY (GAG-INDEX)  TO                            GA2JPGM 
00996            COPY-OF-TABLE (COPY-IDX)                               GA2JPGM 
00997         SET COPY-IDX, GAG-INDEX  UP BY  1                         GA2JPGM 
00998         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2JPGM 
00999                                                                   GA2JPGM 
01000      IF WS-ADD-COUNT  +  GAG-ENTRY-COUNT  >                       GA2JPGM 
01001        GC-GCTABULR-ADIP-VARY-MAX-OCUR                             GA2JPGM 
01002         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2JPGM 
01003 -    'EASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE       GA2JPGM 
01004         MOVE '2JL1'  TO  WS-ABEND-CODE                            GA2JPGM 
01005         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01006                                                                   GA2JPGM 
01007      SET WS-SORT-IDX, COPY-IDX, GAG-INDEX  TO  1.                 GA2JPGM 
01008      MOVE '2060'  TO  WS-PARA-ID.                                 GA2JPGM 
01009  2060-MERGE-IN-NEW-ENTRIES.                                       GA2JPGM 
01010      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2JPGM 
01011         SET GAG-INDEX  DOWN BY  1                                 GA2JPGM 
01012         SET GAG-ENTRY-COUNT  TO  GAG-INDEX                        GA2JPGM 
01013         MOVE GAG-ENTRY-COUNT  TO  GAG-ENTRY-COUNT                 GA2JPGM 
01014         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2JPGM 
01015                                                                   GA2JPGM 
01016      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2JPGM 
01017             =  HIGH-VALUES  AND                                   GA2JPGM 
01018         COPY-OF-TABLE (COPY-IDX)  NOT  =  HIGH-VALUES             GA2JPGM 
01019         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2JPGM 
01020                                                                   GA2JPGM 
01021      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2JPGM 
01022             NOT  =  HIGH-VALUES  AND                              GA2JPGM 
01023         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2JPGM 
01024         GO TO 2080-INSERT-NEW-ENTRY.                              GA2JPGM 
01025                                                                   GA2JPGM 
01026      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2JPGM 
01027             =  HIGH-VALUES  AND                                   GA2JPGM 
01028         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2JPGM 
01029         NEXT SENTENCE                                             GA2JPGM 
01030      ELSE                                                         GA2JPGM 
01031         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2JPGM 
01032            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA2JPGM 
01033            GO TO 2070-SAVE-COPIED-ENTRY                           GA2JPGM 
01034         ELSE                                                      GA2JPGM 
01035            IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <               GA2JPGM 
01036               COPY-PROCEDURE-ARGUMENT (COPY-IDX)                  GA2JPGM 
01037               GO TO 2080-INSERT-NEW-ENTRY.                        GA2JPGM 
01038                                                                   GA2JPGM 
01039      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2JPGM 
01040             =  HIGH-VALUES  AND                                   GA2JPGM 
01041         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2JPGM 
01042         NEXT SENTENCE                                             GA2JPGM 
01043      ELSE                                                         GA2JPGM 
01044         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2JPGM 
01045            COPY-CODE-FUNCTION (COPY-IDX)                          GA2JPGM 
01046            GO TO 2070-SAVE-COPIED-ENTRY                           GA2JPGM 
01047         ELSE                                                      GA2JPGM 
01048            IF WS-CODE-FUNCTION (WS-SORT-IDX) <                    GA2JPGM 
01049               COPY-CODE-FUNCTION (COPY-IDX)                       GA2JPGM 
01050               GO TO 2080-INSERT-NEW-ENTRY.                        GA2JPGM 
01051                                                                   GA2JPGM 
01052 ****************************************************************  GA2JPGM 
01053 **   AT THIS POINT THE NEW ENTRY'S THREE FIELDS MUST BE EQUAL TO  GA2JPGM 
01054 **   THE OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING  GA2JPGM 
01055 **   THE INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE   GA2JPGM 
01056 **   ENTRY FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.  GA2JPGM 
01057 ****************************************************************  GA2JPGM 
01058                                                                   GA2JPGM 
01059      SET WS-SORT-IDX  UP BY  1.                                   GA2JPGM 
01060                                                                   GA2JPGM 
01061  2070-SAVE-COPIED-ENTRY.                                          GA2JPGM 
01062      MOVE '2070'  TO  WS-PARA-ID.                                 GA2JPGM 
01063      MOVE COPY-OF-TABLE (COPY-IDX)  TO                            GA2JPGM 
01064         GAG-ENTRY (GAG-INDEX).                                    GA2JPGM 
01065                                                                   GA2JPGM 
01066      IF COPY-IDX  NOT >  GAG-ENTRY-COUNT                          GA2JPGM 
01067         SET COPY-IDX  UP BY  1                                    GA2JPGM 
01068         SET GAG-INDEX  UP BY  1                                   GA2JPGM 
01069         MOVE '2060'  TO  WS-PARA-ID                               GA2JPGM 
01070         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2JPGM 
01071      ELSE                                                         GA2JPGM 
01072         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2JPGM 
01073 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2JPGM 
01074         MOVE '2JL2'  TO  WS-ABEND-CODE                            GA2JPGM 
01075         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01076                                                                   GA2JPGM 
01077  2080-INSERT-NEW-ENTRY.                                           GA2JPGM 
01078      MOVE '2080'  TO  WS-PARA-ID.                                 GA2JPGM 
01079                                                                   GA2JPGM 
01080      MOVE WS-SORTED-TAB (WS-SORT-IDX) TO                          GA2JPGM 
01081         GAG-ENTRY (GAG-INDEX).                                    GA2JPGM 
01082                                                                   GA2JPGM 
01083      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2JPGM 
01084         SET WS-SORT-IDX  UP BY  1                                 GA2JPGM 
01085         SET GAG-INDEX  UP BY  1                                   GA2JPGM 
01086         MOVE '2060'  TO  WS-PARA-ID                               GA2JPGM 
01087         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2JPGM 
01088      ELSE                                                         GA2JPGM 
01089         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2JPGM 
01090 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2JPGM 
01091         MOVE '2JL3'  TO  WS-ABEND-CODE                            GA2JPGM 
01092         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01093                                                                   GA2JPGM 
01094  2090-UPDATE-ALL-LVL-TAB-REC.                                     GA2JPGM 
01095      MOVE '2090'  TO  WS-PARA-ID.                                 GA2JPGM 
01096                                                                   GA2JPGM 
01097 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2JPGM 
01098                                                                   GA2JPGM 
01099      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2JPGM 
01100      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2JPGM 
01101                                                                   GA2JPGM 
01102      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN        +GA2JPGM 
01103                     GC-GCTABULR-ADIP-FIXED-LEN +                  GA2JPGM 
01104              (GAG-ENTRY-COUNT  *  GC-GCTABULR-ADIP-VARY-LEN).     GA2JPGM 
01105                                                                   GA2JPGM 
01106      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2JPGM 
01107            GC-GCIOPARM-LEN      +  GCIO-RECORD-LENGTH.            GA2JPGM 
01108                                                                   GA2JPGM 
01109      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2JPGM 
01110         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2JPGM 
01111         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2JPGM 
01112                                                                   GA2JPGM 
01113      IF NOT GCIO-GOOD-RETURN                                      GA2JPGM 
01114         MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASGA2JPGM 
01115 -    'E CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE          GA2JPGM 
01116         MOVE '2JF2'  TO  WS-ABEND-CODE                            GA2JPGM 
01117         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01118                                                                   GA2JPGM 
01119      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2JPGM 
01120         VARYING MAP-IDX  FROM 1  BY  1                            GA2JPGM 
01121            UNTIL  MAP-IDX  >  WS-MAP-ROW.                         GA2JPGM 
01122                                                                   GA2JPGM 
01123 *--------- DARKEN SELECTION LINE 5 AND PAGING MESSAGE LINE 23.    GA2JPGM 
01124                                                                   GA2JPGM 
01125      MOVE DFHBMASD TO MAP-SELECT-LABEL-ATTR                       GA2JPGM 
01126                       MAP-SELECT-ATTR                             GA2JPGM 
01127                       MAP-SELECT-FROM-ATTR                        GA2JPGM 
01128                       MAP-SELECT-TO-LABEL-ATTR                    GA2JPGM 
01129                       MAP-SELECT-TO-ATTR                          GA2JPGM 
01130                       MAP-SELECT-OF-LABEL-ATTR                    GA2JPGM 
01131                       MAP-SELECT-OF-ATTR                          GA2JPGM 
01132                       MAP-SELECT-DISPLAY-LABEL-ATTR               GA2JPGM 
01133                       MAP-PAGING-LABEL-ATTR.                      GA2JPGM 
01134                                                                   GA2JPGM 
01135      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (1).                 GA2JPGM 
01136      EXEC CICS SEND   MAP('GA2JI01') MAPSET('GA2JSET') ERASE      GA2JPGM 
01137         FROM(GA2JI01O) CURSOR END-EXEC.                           GA2JPGM 
01138                                                                   GA2JPGM 
01139  2099-EXIT.   EXIT.                                               GA2JPGM 
01140 /                                                                 GA2JPGM 
01141 ******************************************************************GA2JPGM 
01142 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2JPGM 
01143 **                                                                GA2JPGM 
01144 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2JPGM 
01145 **  ALREADY ON THE OPERATORS SCREEN.                              GA2JPGM 
01146 **                                                                GA2JPGM 
01147 ******************************************************************GA2JPGM 
01148  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2JPGM 
01149                                                                   GA2JPGM 
01150      MOVE LOW-VALUES  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)        GA2JPGM 
01151                           MAP-CODE-FUNCTION (MAP-IDX).            GA2JPGM 
01152                                                                   GA2JPGM 
01153  2199-EXIT.   EXIT.                                               GA2JPGM 
01154 /                                                                 GA2JPGM 
01155 ******************************************************************GA2JPGM 
01156 **          X C T L   T O   D E L   S C R E E N                   GA2JPGM 
01157 **                                                                GA2JPGM 
01158 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2JPGM 
01159 ** DELETING ENTRIES.  WE READ THE ALL LEVEL TABULAR RECORD & PASS GA2JPGM 
01160 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, & ALL LEVEL TABULARGA2JPGM 
01161 ** RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU THE      GA2JPGM 
01162 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA2JPGM 
01163 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2JPGM 
01164 ******************************************************************GA2JPGM 
01165  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2JPGM 
01166      MOVE '3000'  TO  WS-PARA-ID.                                 GA2JPGM 
01167                                                                   GA2JPGM 
01168      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2JPGM 
01169            GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +   GA2JPGM 
01170            GC-GCTABULR-ADIP-FIXED-LEN +                           GA2JPGM 
01171           (GC-GCTABULR-ADIP-VARY-LEN    *                         GA2JPGM 
01172            GC-GCTABULR-ADIP-VARY-MAX-OCUR).                       GA2JPGM 
01173                                                                   GA2JPGM 
01174 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA2JPGM 
01175      EXEC CICS GETMAIN                                            GA2JPGM 
01176         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2JPGM 
01177         INITIMG(WS-HEX-00)                                        GA2JPGM 
01178         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2JPGM 
01179 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2JPGM 
01180 ***  ADD ALL-LEVEL-TAB-PNTR, 4096 GIVING  ALL-LEVEL-TAB-PNTR2.    GA2JPGM 
01181                                                                   GA2JPGM 
01182      MOVE LOW-VALUES  TO  GCIO-WORKFILE-KEY.                      GA2JPGM 
01183                                                                   GA2JPGM 
01184 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA2JPGM 
01185 **   EXEC CICS GETMAIN                                            GA2JPGM 
01186 **      SET(ADDRESS OF GCA-COMMAREA)                              GA2JPGM 
01187 **      INITIMG(WS-HEX-00)                                        GA2JPGM 
01188 **      LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA2JPGM 
01189 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2JPGM 
01190 **                                                                GA2JPGM 
01191      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2JPGM 
01192         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2JPGM 
01193         MOVE  'G'   TO  GCIO-WRK-STATUS-CODE                      GA2JPGM 
01194         MOVE  'G3'  TO  GCIO-WRK-RECORD-TYPE                      GA2JPGM 
01195         MOVE MAP-ID-LINE  TO GROUP-SPECIFIC-ID-LINE               GA2JPGM 
01196         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA2JPGM 
01197         MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM          GA2JPGM 
01198         MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM        GA2JPGM 
01199         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA2JPGM 
01200         MOVE SPACES  TO  GCA-L-O-B                                GA2JPGM 
01201                          GCA-PROV-CTL                             GA2JPGM 
01202                          GCA-BEN-PROV-ID                          GA2JPGM 
01203                          GCIO-WRK-LINE-OF-BUS                     GA2JPGM 
01204                          GCIO-WRK-PROVIDER-CONTROL                GA2JPGM 
01205         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2JPGM 
01206         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2JPGM 
01207         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2JPGM 
01208                            GCIO-WRK-PROVISION-ID                  GA2JPGM 
01209         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT    GA2JPGM 
01210                            GCIO-WRK-PROVISION-SLOT-NO             GA2JPGM 
01211         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2JPGM 
01212         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2JPGM 
01213                                                                   GA2JPGM 
01214      IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA2JPGM 
01215         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2JPGM 
01216         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2JPGM 
01217         MOVE  'C3'  TO  GCIO-WRK-RECORD-TYPE                      GA2JPGM 
01218         MOVE MAP-ID-LINE  TO CONTRACT-ID-LINE                     GA2JPGM 
01219         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2JPGM 
01220         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2JPGM 
01221         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2JPGM 
01222         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2JPGM 
01223         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2JPGM 
01224         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2JPGM 
01225         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2JPGM 
01226         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2JPGM 
01227         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2JPGM 
01228         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2JPGM 
01229                            GCIO-WRK-PROVISION-ID                  GA2JPGM 
01230         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT    GA2JPGM 
01231                            GCIO-WRK-PROVISION-SLOT-NO             GA2JPGM 
01232         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2JPGM 
01233         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2JPGM 
01234                                                                   GA2JPGM 
01235      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2JPGM 
01236         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA2JPGM 
01237         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2JPGM 
01238         MOVE  'C5'  TO  GCIO-WRK-RECORD-TYPE                      GA2JPGM 
01239         MOVE MAP-ID-LINE  TO BENEFIT-PROVISION-ID-LINE            GA2JPGM 
01240         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2JPGM 
01241         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2JPGM 
01242         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2JPGM 
01243         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2JPGM 
01244         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2JPGM 
01245         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2JPGM 
01246         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2JPGM 
01247         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2JPGM 
01248         MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID       GA2JPGM 
01249         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2JPGM 
01250                            GCIO-WRK-TAB-PROVISION-ID              GA2JPGM 
01251         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT    GA2JPGM 
01252                            GCIO-WRK-TAB-PROV-SLOT-NO              GA2JPGM 
01253         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.             GA2JPGM 
01254                                                                   GA2JPGM 
01255 **   MOVE WS-Y  TO  WS-YY.                                        GA2JPGM 
01256 **   IF  WS-M  >  2                                               GA2JPGM 
01257 **      DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2JPGM 
01258 **         REMAINDER  WS-REMAINDER                                GA2JPGM 
01259 *    ELSE                                                         GA2JPGM 
01260 *       MOVE 1  TO  WS-REMAINDER.                                 GA2JPGM 
01261 *    SET WS-M-IDX  TO  WS-M.                                      GA2JPGM 
01262 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2JPGM 
01263 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2JPGM 
01264 *    IF  WS-REMAINDER  =  ZERO                                    GA2JPGM 
01265 *       ADD 1  TO  WS-DDD.                                        GA2JPGM 
01266 *                                                                 GA2JPGM 
01267 *    MOVE  WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE                  GA2JPGM 
01268 *                        GCA-EFF-DT.                              GA2JPGM 
01269      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA2JPGM 
01270      MOVE  SPACES  TO  GCA-INTERNAL-TAB-ID,                       GA2JPGM 
01271                        GCA-INTERNAL-TAB-SLOT,                     GA2JPGM 
01272                        GCA-ADD-DEL-IND,                           GA2JPGM 
01273                        GCA-ALL-LEVEL-TAB-FUNC-CODE,               GA2JPGM 
01274                        GCA-OCCURS-ENTRY-COUNTER.                  GA2JPGM 
01275      MOVE  MAP-FROM-MENU-ID  TO GCA-FROM-MENU-ID.                 GA2JPGM 
01276      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2JPGM 
01277 ***  MOVE  ALL-LEVEL-TAB-PNTR TO GCA-RECORD-POINTER.              GA2JPGM 
01278                                                                   GA2JPGM 
01279      SET GCA-RECORD-POINTER TO ADDRESS                            GA2JPGM 
01280      OF  IO-PARM-ALL-LVL-TAB-RECORD.                              GA2JPGM 
01281                                                                   GA2JPGM 
01282      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2JPGM 
01283      TO   GAG-ENTRY-COUNT.                                        GA2JPGM 
01284                                                                   GA2JPGM 
01285      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2JPGM 
01286                                                                   GA2JPGM 
01287      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2JPGM 
01288         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2JPGM 
01289         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2JPGM 
01290                                                                   GA2JPGM 
01291      IF  NOT GCIO-GOOD-RETURN                                     GA2JPGM 
01292         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2JPGM 
01293 -     'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE           GA2JPGM 
01294         MOVE '2JF3'  TO  WS-ABEND-CODE                            GA2JPGM 
01295         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01296                                                                   GA2JPGM 
01297 *    SET COMMAREA-PNTR TO ADDRESS                                 GA2JPGM 
01298 *    OF GCA-COMMAREA.                                             GA2JPGM 
01299 *                                                                 GA2JPGM 
01300 *    EXEC CICS XCTL  PROGRAM('GA1JPGM') COMMAREA(COMMAREA-PNTR)   GA2JPGM 
01301 *       LENGTH(4)  END-EXEC.                                      GA2JPGM 
01302      EXEC CICS XCTL  PROGRAM('GA1JPGM')                           GA2JPGM 
01303                      COMMAREA(DFHCOMMAREA)                        GA2JPGM 
01304                      LENGTH(LENGTH OF DFHCOMMAREA)                GA2JPGM 
01305      END-EXEC.                                                    GA2JPGM 
01306                                                                   GA2JPGM 
01307  3099-EXIT.   EXIT.                                               GA2JPGM 
01308 /                                                                 GA2JPGM 
01309 ***************************************************************** GA2JPGM 
01310 **          D I S P L A Y   F I R S T   S C R E E N               GA2JPGM 
01311 **                                                                GA2JPGM 
01312 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE DELETE GA2JPGM 
01313 ** PROGRAM, THAT PROGRAM WILL PASS THE ADDRESS OF A PARAMETER LISTGA2JPGM 
01314 ** CONTAINING THE FIELDS FROM THE HEADER OF THE SCREEN.           GA2JPGM 
01315 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2JPGM 
01316 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2JPGM 
01317 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2JPGM 
01318 ******************************************************************GA2JPGM 
01319  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2JPGM 
01320      MOVE '4000'  TO  WS-PARA-ID.                                 GA2JPGM 
01321 ***  MOVE LOW-VALUES TO SCREEN                                    GA2JPGM 
01322      MOVE LOW-VALUES TO GA2JI01I.                                 GA2JPGM 
01323                                                                   GA2JPGM 
01324      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2JPGM 
01325         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2JPGM 
01326            TO MAP-ERROR-MESSAGE                                   GA2JPGM 
01327         MOVE '2JC1'  TO  WS-ABEND-CODE                            GA2JPGM 
01328         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01329                                                                   GA2JPGM 
01330 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA2JPGM 
01331 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2JPGM 
01332                                                                   GA2JPGM 
01333 **   SET ADDRESS OF GCA-COMMAREA                                  GA2JPGM 
01334 **   TO  INCOMING-COMMAREA-PNTR.                                  GA2JPGM 
01335                                                                   GA2JPGM 
01336      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-ALL-LEVEL-TAB-ID.         GA2JPGM 
01337      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-ALL-LEVEL-TAB-SLOT.     GA2JPGM 
01338      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA2JPGM 
01339                                                                   GA2JPGM 
01340      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2JPGM 
01341         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-TITLE-LINE        GA2JPGM 
01342         MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2JPGM 
01343         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA2JPGM 
01344         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA2JPGM 
01345         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2JPGM 
01346         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA2JPGM 
01347         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2JPGM 
01348         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2JPGM 
01349         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2JPGM 
01350         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2JPGM 
01351                                                                   GA2JPGM 
01352      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2JPGM 
01353         MOVE CONTRACT-TITLE-LINE  TO  MAP-TITLE-LINE              GA2JPGM 
01354         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2JPGM 
01355         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA2JPGM 
01356         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA2JPGM 
01357         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2JPGM 
01358         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA2JPGM 
01359         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2JPGM 
01360         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2JPGM 
01361         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2JPGM 
01362         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2JPGM 
01363         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2JPGM 
01364         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2JPGM 
01365         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2JPGM 
01366         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2JPGM 
01367                                                                   GA2JPGM 
01368      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2JPGM 
01369         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-TITLE-LINE     GA2JPGM 
01370         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA2JPGM 
01371         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA2JPGM 
01372         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA2JPGM 
01373         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA2JPGM 
01374         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA2JPGM 
01375         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2JPGM 
01376         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA2JPGM 
01377         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2JPGM 
01378         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA2JPGM 
01379         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2JPGM 
01380         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA2JPGM 
01381         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2JPGM 
01382         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA2JPGM 
01383         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2JPGM 
01384                                                                   GA2JPGM 
01385 *--------- DARKEN SELECTION LINE 5 AND PAGING MESSAGE LINE 23.    GA2JPGM 
01386                                                                   GA2JPGM 
01387      MOVE DFHBMASD TO MAP-SELECT-LABEL-ATTR                       GA2JPGM 
01388                       MAP-SELECT-ATTR                             GA2JPGM 
01389                       MAP-SELECT-FROM-ATTR                        GA2JPGM 
01390                       MAP-SELECT-TO-LABEL-ATTR                    GA2JPGM 
01391                       MAP-SELECT-TO-ATTR                          GA2JPGM 
01392                       MAP-SELECT-OF-LABEL-ATTR                    GA2JPGM 
01393                       MAP-SELECT-OF-ATTR                          GA2JPGM 
01394                       MAP-SELECT-DISPLAY-LABEL-ATTR               GA2JPGM 
01395                       MAP-PAGING-LABEL-ATTR.                      GA2JPGM 
01396                                                                   GA2JPGM 
01397      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (1).                 GA2JPGM 
01398      EXEC CICS SEND   MAP('GA2JI01') MAPSET('GA2JSET') ERASE      GA2JPGM 
01399         FROM(GA2JI01O) CURSOR END-EXEC.                           GA2JPGM 
01400                                                                   GA2JPGM 
01401  4099-EXIT.   EXIT.                                               GA2JPGM 
01402 /                                                                 GA2JPGM 
01403 ***************************************************************** GA2JPGM 
01404 **        X C T L   T O   P R E V I O U S   M E N U               GA2JPGM 
01405 **                                                                GA2JPGM 
01406 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2JPGM 
01407 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD &       GA2JPGM 
01408 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA2JPGM 
01409 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM       GA2JPGM 
01410 ** MENU ID' FIELD).                                               GA2JPGM 
01411 ******************************************************************GA2JPGM 
01412  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2JPGM 
01413      MOVE '5000'  TO  WS-PARA-ID.                                 GA2JPGM 
01414                                                                   GA2JPGM 
01415      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2JPGM 
01416         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA2JPGM 
01417                                                                   GA2JPGM 
01418      IF  MAP-FROM-MENU-ID  = 'GC4A'                               GA2JPGM 
01419         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA2JPGM 
01420                                                                   GA2JPGM 
01421      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2JPGM 
01422         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA2JPGM 
01423                                                                   GA2JPGM 
01424      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA2JPGM 
01425         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA2JPGM 
01426                                                                   GA2JPGM 
01427                                                                   GA2JPGM 
01428  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA2JPGM 
01429      MOVE '5010'  TO  WS-PARA-ID.                                 GA2JPGM 
01430                                                                   GA2JPGM 
01431      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2JPGM 
01432          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2JPGM 
01433                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2JPGM 
01434                                                                   GA2JPGM 
01435 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA2JPGM 
01436      EXEC CICS GETMAIN                                            GA2JPGM 
01437         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA2JPGM 
01438         INITIMG(WS-HEX-00)                                        GA2JPGM 
01439         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01440 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA2JPGM 
01441                                                                   GA2JPGM 
01442      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2JPGM 
01443                                                                   GA2JPGM 
01444      MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           GA2JPGM 
01445      MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           GA2JPGM 
01446      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2JPGM 
01447      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM              GA2JPGM 
01448      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2JPGM 
01449      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2JPGM 
01450      MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS,           GA2JPGM 
01451                                   GCIO-WRK-PROVIDER-CONTROL.      GA2JPGM 
01452      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2JPGM 
01453      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2JPGM 
01454                                                                   GA2JPGM 
01455      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA2JPGM 
01456      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2JPGM 
01457                       GCIO-WRK-TAB-PROVISION-ID.                  GA2JPGM 
01458      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2JPGM 
01459                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2JPGM 
01460      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2JPGM 
01461                                                                   GA2JPGM 
01462      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA2JPGM 
01463      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA2JPGM 
01464                                                                   GA2JPGM 
01465      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      GA2JPGM 
01466                                                                   GA2JPGM 
01467      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2JPGM 
01468         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA2JPGM 
01469         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01470                                                                   GA2JPGM 
01471      IF  NOT GCIO2-GOOD-RETURN                                    GA2JPGM 
01472         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2JPGM 
01473 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2JPGM 
01474         MOVE '2JF4'  TO  WS-ABEND-CODE                            GA2JPGM 
01475         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01476                                                                   GA2JPGM 
01477      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2JPGM 
01478          GC-WORKFILE-KEY-LEN        +                             GA2JPGM 
01479                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2JPGM 
01480                                                                   GA2JPGM 
01481      EXEC CICS XCTL PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)    GA2JPGM 
01482         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01483                                                                   GA2JPGM 
01484      GO TO 5099-EXIT.                                             GA2JPGM 
01485                                                                   GA2JPGM 
01486  5020-XCTL-TO-CONTRACT-MENU.                                      GA2JPGM 
01487      MOVE '5020'  TO  WS-PARA-ID.                                 GA2JPGM 
01488                                                                   GA2JPGM 
01489      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2JPGM 
01490          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2JPGM 
01491                 GC-GCCONTR-MAX-REC-LEN.                           GA2JPGM 
01492                                                                   GA2JPGM 
01493 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA2JPGM 
01494      EXEC CICS GETMAIN                                            GA2JPGM 
01495         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA2JPGM 
01496         INITIMG(WS-HEX-00)                                        GA2JPGM 
01497         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01498 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA2JPGM 
01499 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA2JPGM 
01500                                                                   GA2JPGM 
01501      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2JPGM 
01502                                                                   GA2JPGM 
01503      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2JPGM 
01504      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA2JPGM 
01505      MOVE GCA-PLAN-CODE     TO GCIO-WRK-PLAN-CODE.                GA2JPGM 
01506      MOVE GCA-GROUP-NUM     TO GCIO-WRK-GROUP-NUM.                GA2JPGM 
01507      MOVE GCA-SECTION-NUM   TO GCIO-WRK-SECTION-NUM               GA2JPGM 
01508      MOVE GCA-PKG-CODE      TO GCIO-WRK-PKG-CODE.                 GA2JPGM 
01509      MOVE GCA-L-O-B         TO GCIO-WRK-LINE-OF-BUS.              GA2JPGM 
01510      MOVE GCA-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL.         GA2JPGM 
01511      MOVE GCA-FAM-REL-LVL   TO GCIO-WRK-FAMILY-RELATION-LVL.      GA2JPGM 
01512      MOVE GCA-EFFDT-CEN     TO GCIO-WRK-EFFDT-CEN.                GA2JPGM 
01513                                                                   GA2JPGM 
01514      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA2JPGM 
01515      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2JPGM 
01516                       GCIO-WRK-TAB-PROVISION-ID.                  GA2JPGM 
01517      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2JPGM 
01518                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2JPGM 
01519      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA2JPGM 
01520                                                                   GA2JPGM 
01521      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA2JPGM 
01522      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA2JPGM 
01523                                                                   GA2JPGM 
01524      MOVE 'RD '  TO  GCIO3-FILE-ACCESS-CODE.                      GA2JPGM 
01525                                                                   GA2JPGM 
01526      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2JPGM 
01527         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA2JPGM 
01528         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01529                                                                   GA2JPGM 
01530      IF  NOT GCIO3-GOOD-RETURN                                    GA2JPGM 
01531         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2JPGM 
01532 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2JPGM 
01533         MOVE '2JF5'  TO  WS-ABEND-CODE                            GA2JPGM 
01534         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01535                                                                   GA2JPGM 
01536      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2JPGM 
01537          GC-WORKFILE-KEY-LEN        +                             GA2JPGM 
01538                 GC-GCCONTR-MAX-REC-LEN.                           GA2JPGM 
01539                                                                   GA2JPGM 
01540      EXEC CICS XCTL PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)    GA2JPGM 
01541         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01542                                                                   GA2JPGM 
01543      GO TO 5099-EXIT.                                             GA2JPGM 
01544                                                                   GA2JPGM 
01545  5030-XCTL-TO-BEN-PROV-MENU.                                      GA2JPGM 
01546      MOVE '5030'  TO  WS-PARA-ID.                                 GA2JPGM 
01547                                                                   GA2JPGM 
01548      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2JPGM 
01549          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2JPGM 
01550                 GC-GCBENPRV-MAX-REC-LEN.                          GA2JPGM 
01551                                                                   GA2JPGM 
01552 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA2JPGM 
01553      EXEC CICS GETMAIN                                            GA2JPGM 
01554         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA2JPGM 
01555         INITIMG(WS-HEX-00)                                        GA2JPGM 
01556         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01557 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA2JPGM 
01558                                                                   GA2JPGM 
01559      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2JPGM 
01560                                                                   GA2JPGM 
01561      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2JPGM 
01562      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA2JPGM 
01563      MOVE GCA-PLAN-CODE     TO GCIO-WRK-PLAN-CODE.                GA2JPGM 
01564      MOVE GCA-GROUP-NUM     TO GCIO-WRK-GROUP-NUM.                GA2JPGM 
01565      MOVE GCA-SECTION-NUM   TO GCIO-WRK-SECTION-NUM               GA2JPGM 
01566      MOVE GCA-PKG-CODE      TO GCIO-WRK-PKG-CODE.                 GA2JPGM 
01567      MOVE GCA-L-O-B         TO GCIO-WRK-LINE-OF-BUS.              GA2JPGM 
01568      MOVE GCA-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL.         GA2JPGM 
01569      MOVE GCA-FAM-REL-LVL   TO GCIO-WRK-FAMILY-RELATION-LVL.      GA2JPGM 
01570      MOVE GCA-EFFDT-CEN     TO GCIO-WRK-EFFDT-CEN.                GA2JPGM 
01571      MOVE GCA-BEN-PROV-ID   TO GCIO-WRK-PROVISION-ID.             GA2JPGM 
01572                                                                   GA2JPGM 
01573      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA2JPGM 
01574      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA2JPGM 
01575      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA2JPGM 
01576      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2JPGM 
01577      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA2JPGM 
01578                                                                   GA2JPGM 
01579      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA2JPGM 
01580      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA2JPGM 
01581                                                                   GA2JPGM 
01582      MOVE 'RD '  TO  GCIO4-FILE-ACCESS-CODE.                      GA2JPGM 
01583                                                                   GA2JPGM 
01584      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2JPGM 
01585         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA2JPGM 
01586         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01587                                                                   GA2JPGM 
01588      IF  NOT GCIO4-GOOD-RETURN                                    GA2JPGM 
01589         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2JPGM 
01590 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2JPGM 
01591         MOVE '2JF6'  TO  WS-ABEND-CODE                            GA2JPGM 
01592         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2JPGM 
01593                                                                   GA2JPGM 
01594      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2JPGM 
01595          GC-WORKFILE-KEY-LEN        +                             GA2JPGM 
01596                 GC-GCBENPRV-MAX-REC-LEN.                          GA2JPGM 
01597                                                                   GA2JPGM 
01598      EXEC CICS XCTL PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)    GA2JPGM 
01599         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2JPGM 
01600                                                                   GA2JPGM 
01601      GO TO 5099-EXIT.                                             GA2JPGM 
01602                                                                   GA2JPGM 
01603                                                                   GA2JPGM 
01604                                                                   GA2JPGM 
01605  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA2JPGM 
01606      MOVE '5040'  TO  WS-PARA-ID.                                 GA2JPGM 
01607                                                                   GA2JPGM 
01608      EXEC CICS XCTL                                               GA2JPGM 
01609                PROGRAM('GTM1PGM')                                 GA2JPGM 
01610                END-EXEC.                                          GA2JPGM 
01611                                                                   GA2JPGM 
01612      GO  TO  5099-EXIT.                                           GA2JPGM 
01613                                                                   GA2JPGM 
01614  5099-EXIT.                                                       GA2JPGM 
01615      EXIT.                                                        GA2JPGM 
01616 /                                                                 GA2JPGM 
01617 ***************************************************************** GA2JPGM 
01618 **           X C T L   T O   M A I N   M E N U                    GA2JPGM 
01619 **                                                                GA2JPGM 
01620 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2JPGM 
01621 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2JPGM 
01622 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2JPGM 
01623 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2JPGM 
01624 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2JPGM 
01625 ** AND PROGRESS DOWN.                                             GA2JPGM 
01626 ******************************************************************GA2JPGM 
01627  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2JPGM 
01628      MOVE '6000'  TO  WS-PARA-ID.                                 GA2JPGM 
01629      MOVE '2JP1'  TO  WS-ABEND-CODE.                              GA2JPGM 
01630                                                                   GA2JPGM 
01631      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2JPGM 
01632                                                                   GA2JPGM 
01633  6099-EXIT.     EXIT.                                             GA2JPGM 
01634 /                                                                 GA2JPGM 
01635 /     E R R O R   M E S S A G E   T H E N   A B E N D             GA2JPGM 
01636 ******************************************************************GA2JPGM 
01637  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2JPGM 
01638                                                                   GA2JPGM 
01639      SET MAP-IDX   TO  7.                                         GA2JPGM 
01640      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX).           GA2JPGM 
01641                                                                   GA2JPGM 
01642 *--------- DARKEN SELECTION LINE 5 AND PAGING MESSAGE LINE 23.    GA2JPGM 
01643                                                                   GA2JPGM 
01644      MOVE DFHBMASD TO MAP-SELECT-LABEL-ATTR                       GA2JPGM 
01645                       MAP-SELECT-ATTR                             GA2JPGM 
01646                       MAP-SELECT-FROM-ATTR                        GA2JPGM 
01647                       MAP-SELECT-TO-LABEL-ATTR                    GA2JPGM 
01648                       MAP-SELECT-TO-ATTR                          GA2JPGM 
01649                       MAP-SELECT-OF-LABEL-ATTR                    GA2JPGM 
01650                       MAP-SELECT-OF-ATTR                          GA2JPGM 
01651                       MAP-SELECT-DISPLAY-LABEL-ATTR               GA2JPGM 
01652                       MAP-PAGING-LABEL-ATTR.                      GA2JPGM 
01653                                                                   GA2JPGM 
01654      EXEC CICS SEND   MAP('GA2JI01') MAPSET('GA2JSET') ERASE      GA2JPGM 
01655         FROM(GA2JI01O) CURSOR WAIT END-EXEC.                      GA2JPGM 
01656                                                                   GA2JPGM 
01657      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2JPGM 
01658                                                                   GA2JPGM 
01659  9999-EXIT.     EXIT.                                             GA2JPGM 
