00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GA2FPGM.                                         GA2FPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV001
00004  AUTHOR.         S BUCH.                                          GA2FPGM 
00005  DATE-WRITTEN.   11/20/84.                                        GA2FPGM 
00006  DATE-COMPILED.                                                   GA2FPGM 
00007      SKIP3                                                        GA2FPGM 
00008 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2FPGM 
00009 *******P R O G R A M   M O D I F I C A T I O N   S T A T U S *****GA2FPGM 
00010 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2FPGM 
00011 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2FPGM 
00012 *                                                                 GA2FPGM 
00013 *    XXXX   10/15/85    AHL   PARAGRAPH 2010-VALIDATE-ADD-ENTRIES GA2FPGM 
00014 *                             IS MODIFIED                         GA2FPGM 
00015 *                                                                 GA2FPGM 
00016 *   T529    03-19-86    MDD   ADDED CODE TO CHECK RETURN CODE FROMGA2FPGM 
00017 *                             GCVIOPGM FOR A VALUE OF '20', THIS  GA2FPGM 
00018 *                             MEANS THE EDIT TABLE IS EMPTY AND A GA2FPGM 
00019 *                             VALIDATION COULD NOT BE PERFORMED.  GA2FPGM 
00020 *                             PF4/16 CAN BE USED TO ACCEPT THE    GA2FPGM 
00021 *                             DATA AS SHOWN ON THE SCREEN AND TO  GA2FPGM 
00022 *                             CONTINUE PROCESSING.                GA2FPGM 
00023 *                                                                 GA2FPGM 
00024 *  EL500    06/18/86    DES   FIXED PF4/16 CODE TO ACCEPT EMPTY   GA2FPGM 
00025 *                             VALIDATION TABLE CONDITION ONLY,    GA2FPGM 
00026 *                             ALL OTHER ERRORS STILL MUST BE FIXEDGA2FPGM 
00027 *                                                                 GA2FPGM 
00028 *    D137    08/13/86  JLA  1. USE USER DEFINED LOGICAL MAP FOR   GA2FPGM 
00029 *                              SCREEN.  THIS REPLACES THE PARTIAL GA2FPGM 
00030 *                              USE OF BMS MAP AND USER DEFINED.   GA2FPGM 
00031 *                                                                 GA2FPGM 
00032 *    D0120   01/29/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT    GA2FPGM 
00033 *                           EXECUTED FROM TRANSACTION GTM1:       GA2FPGM 
00034 *                           1. PF1/PF13 - CONSTRUCT COMMAREA AS   GA2FPGM 
00035 *                              IF GC4A HAD CALLED, XCTL TO ADD    GA2FPGM 
00036 *                              SCREEN PROGRAM.                    GA2FPGM 
00037 *                           2. PF3/PF15 - CONSTRUCT COMMAREA AS   GA2FPGM 
00038 *                              IF GC4A HAD CALLED, XCTL TO        GA2FPGM 
00039 *                              GTM1PGM.                           GA2FPGM 
00040 *                                                                *GA2FPGM 
00041 *    D116     8/17/87  FRY    CAPTURE OPERATOR-ID WHEN A 'C3',   *GA2FPGM 
00042 *                             'C5', OR 'G3' RECORD IS UPDATED.   *GA2FPGM 
00043 *                                                                *GA2FPGM 
00044 *  11161  11/17/90  ENW   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA2FPGM 
00045 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA2FPGM 
00046 *                         ROUTINES. REMOVED HARD CODED LENGTHS.  *GA2FPGM 
00047 *                                                                *GA2FPGM 
00048 *  D12009 09/26/91  GDM   CONVERT TO COBOL II                    *GA2FPGM 
00049 *                                                                *GA2FPGM 
00050 *  D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD    *GA2FPGM 
00051 *                         FROM ONE POSITION TO TWO POSITIONS.    *GA2FPGM 
00052 *                                                                *GA2FPGM 
00053 *  14726/ 10/15/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000    *GA2FPGM 
00054 *  15057                  AND THE EXPANSION OF THE GROUP SPECIFIC*GA2FPGM 
00055 *                         AND CONTRACT KEY TO SUPPORT THE TEXAS  *GA2FPGM 
00056 *                         MERGER.                                *GA2FPGM 
00057 *                                                                *GA2FPGM 
00058 *  15380    05/03/99    GDM   MODIFY TO INCLIDE MULT06 FIELD     *GA2FPGM 
00059 *                             VALIDATION OPTION.                 *GA2FPGM 
00060 *                                                                *GA2FPGM 
00061 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA2FPGM 
00062 *                                                                *GA2FPGM 
00063 ******************************************************************GA2FPGM 
00064 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2FPGM 
00065 /                                                                 GA2FPGM 
00066 ******************************************************************GA2FPGM 
00067 *   GA2FPGM   ALL LEVEL TABULAR PROVISION MAINTENANCE PROGRAM     GA2FPGM 
00068 *               ANCILLARY RELATIONSHIP BENEFIT CODE  -  GA2F      GA2FPGM 
00069 *                                                                 GA2FPGM 
00070 *     THIS PROGRAM WILL ADD ENTRIES TO THE ANCILLARY RELATIONSHIP GA2FPGM 
00071 *   BENEFIT CODE ALL LEVEL TABULAR RECORD.                        GA2FPGM 
00072 *                                                                 GA2FPGM 
00073 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2FPGM 
00074 *   TO ADD ENTRIES TO THIS PARTICULAR ALL LEVEL TABULAR RECORD.   GA2FPGM 
00075 *   THE PROGRAM READS THE ENTRIES, & VALIDATES THE FORMAT OF EACH GA2FPGM 
00076 *   FIELD IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN   GA2FPGM 
00077 *   ERROR).  IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES GA2FPGM 
00078 *   IN ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER GA2FPGM 
00079 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2FPGM 
00080 *   ENTRIES FOR THIS ALL LEVEL TABULAR RECORD.                    GA2FPGM 
00081 *                                                                 GA2FPGM 
00082 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #AAR) GA2FPGM 
00083 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2FPGM 
00084 *   XCTL TO TRANS GA1F OR PROGRAM GA1FPGM.  THIS PROGRAM WILL     GA2FPGM 
00085 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2FPGM 
00086 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2FPGM 
00087 *   CODE.                                                         GA2FPGM 
00088 *                                                                 GA2FPGM 
00089 *   FUNC CODE: GA2F                                               GA2FPGM 
00090 *   MAPSET:    GA2FSETC  <<<< REDEFINED BY USER DEFINED MAP >>>>  GA2FPGM 
00091 *   FILES:     GCPSWORK                                           GA2FPGM 
00092 ******************************************************************GA2FPGM 
00093      SKIP3                                                        GA2FPGM 
00094  ENVIRONMENT DIVISION.                                            GA2FPGM 
00095 /                                                                 GA2FPGM 
00096  DATA DIVISION.                                                   GA2FPGM 
00097  WORKING-STORAGE SECTION.                                         GA2FPGM 
00098  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2FPGM 
00099      '***GA2FPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2FPGM 
00100  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2FPGM 
00101                                                                   GA2FPGM 
00102  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2FPGM 
00103                                                                   GA2FPGM 
00104  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2FPGM 
00105                                                                   GA2FPGM 
00106  01  COMMAREA-POINTER-AREA.                                       GA2FPGM 
00107      05  COMMAREA-PNTR-COMP               PIC S9(8) COMP.         GA2FPGM 
00108      05  COMMAREA-PNTR                    REDEFINES               GA2FPGM 
00109          COMMAREA-PNTR-COMP               USAGE IS POINTER.       GA2FPGM 
00110                                                                   GA2FPGM 
00111 ** MAP COBOL SCREEN DSECTS **                                     GA2FPGM 
00112  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2FPGM 
00113      '***  I/O MAPAREA ***'.                                      GA2FPGM 
00114  COPY GA2FSETC.                                                   GA2FPGM 
00115 /*****************************************************************GA2FPGM 
00116 ******************************************************************GA2FPGM 
00117 ******************************************************************GA2FPGM 
00118 **                                                              **GA2FPGM 
00119 **    THIS IS A USER DEFINED LOGICAL MAP.  ANY CHANGES TO       **GA2FPGM 
00120 **     MAPSET GA2FSETC AFFECTING IT\
00121 **     FOR HERE.                                                **GA2FPGM 
00122 **                                            JLA 8/13/86       **GA2FPGM 
00123 **                                                              **GA2FPGM 
00124 ******************************************************************GA2FPGM 
00125 ******************************************************************GA2FPGM 
00126 ******************************************************************GA2FPGM 
00127                                                                   GA2FPGM 
00128  01  MAP-USER-DEFINED     REDEFINES   GA2FI01I.                   GA2FPGM 
00129                                                                   GA2FPGM 
00130      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA2FPGM 
00131                                                                   GA2FPGM 
00132      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA2FPGM 
00133      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA2FPGM 
00134      05  MAP-FUNCTION-CODE                PIC X(04).              GA2FPGM 
00135                                                                   GA2FPGM 
00136      05  MAP-TITLE-LINE-LEN               PIC S9(4) COMP SYNC.    GA2FPGM 
00137      05  MAP-TITLE-LINE-ATTR              PIC X.                  GA2FPGM 
00138      05  MAP-TITLE-LINE                   PIC X(47).              GA2FPGM 
00139                                                                   GA2FPGM 
00140      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA2FPGM 
00141      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA2FPGM 
00142      05  MAP-SCREEN-ID                    PIC X(06).              GA2FPGM 
00143                                                                   GA2FPGM 
00144      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA2FPGM 
00145      05  MAP-ID-LINE-ATTR                 PIC X.                  GA2FPGM 
00146      05  MAP-ID-LINE                      PIC X(79).              GA2FPGM 
00147      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA2FPGM 
00148          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA2FPGM 
00149          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA2FPGM 
00150          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA2FPGM 
00151          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA2FPGM 
00152          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA2FPGM 
00153          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA2FPGM 
00154          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA2FPGM 
00155          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA2FPGM 
00156          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA2FPGM 
00157          10  FILLER                           PIC X(18).          GA2FPGM 
00158      05  CONTRACT-ID-LINE  REDEFINES  MAP-ID-LINE.                GA2FPGM 
00159          10  CONTRACT-ID-HEADING              PIC X(14).          GA2FPGM 
00160          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA2FPGM 
00161          10  CONTRACT-GROUP-NO                PIC X(6).           GA2FPGM 
00162          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA2FPGM 
00163          10  CONTRACT-SECTION-NO              PIC X(4).           GA2FPGM 
00164          10  CONTRACT-LOB-HEADING             PIC X(6).           GA2FPGM 
00165          10  CONTRACT-LOB                     PIC X.              GA2FPGM 
00166          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA2FPGM 
00167          10  CONTRACT-PROV-CTL                PIC XX.             GA2FPGM 
00168          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA2FPGM 
00169          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA2FPGM 
00170          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA2FPGM 
00171          10  CONTRACT-EFF-DATE                PIC X(6).           GA2FPGM 
00172          10  FILLER                           PIC X(09).          GA2FPGM 
00173      05  BENEFIT-PROVISION-ID-LINE  REDEFINES  MAP-ID-LINE.       GA2FPGM 
00174          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA2FPGM 
00175          10  BEN-PROV-GROUP-NO                PIC X(6).           GA2FPGM 
00176          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA2FPGM 
00177          10  BEN-PROV-SECTION-NO              PIC X(4).           GA2FPGM 
00178          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA2FPGM 
00179          10  BEN-PROV-LOB                     PIC X.              GA2FPGM 
00180          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA2FPGM 
00181          10  BEN-PROV-PROV-CTL                PIC XX.             GA2FPGM 
00182          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA2FPGM 
00183          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA2FPGM 
00184          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA2FPGM 
00185          10  BEN-PROV-EFF-DATE                PIC X(6).           GA2FPGM 
00186          10  BEN-PROV-ID-HEADING              PIC X(8).           GA2FPGM 
00187          10  BEN-PROV-ID-NO                   PIC X(6).           GA2FPGM 
00188          10  FILLER                           PIC X(09).          GA2FPGM 
00189                                                                   GA2FPGM 
00190      05  MAP-ALL-LEVEL-TAB-ID-LEN         PIC S9(4) COMP SYNC.    GA2FPGM 
00191      05  MAP-ALL-LEVEL-TAB-ID-ATTR        PIC X.                  GA2FPGM 
00192      05  MAP-ALL-LEVEL-TAB-ID             PIC X(06).              GA2FPGM 
00193                                                                   GA2FPGM 
00194      05  MAP-ALL-LEVEL-TAB-SLOT-LEN       PIC S9(4) COMP SYNC.    GA2FPGM 
00195      05  MAP-ALL-LEVEL-TAB-SLOT-ATTR      PIC X.                  GA2FPGM 
00196      05  MAP-ALL-LEVEL-TAB-SLOT           PIC X(07).              GA2FPGM 
00197                                                                   GA2FPGM 
00198      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA2FPGM 
00199      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA2FPGM 
00200      05  MAP-FROM-MENU-ID                 PIC X(04).              GA2FPGM 
00201                                                                   GA2FPGM 
00202      05  MAP-BEN-REL-CODE-ROW  OCCURS 15 TIMES INDEXED BY         GA2FPGM 
00203          MAP-IDX1.                                                GA2FPGM 
00204        10  MAP-BEN-REL-CODE-COL  OCCURS  3 TIMES INDEXED BY       GA2FPGM 
00205            MAP-IDX2.                                              GA2FPGM 
00206          15  MAP-BENEFIT-CODE-LEN      PIC S9(4) COMP SYNC.       GA2FPGM 
00207          15  MAP-BENEFIT-CODE-ATTR     PIC X.                     GA2FPGM 
00208          15  MAP-BENEFIT-CODE          PIC X(6).                  GA2FPGM 
00209                                                                   GA2FPGM 
00210      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA2FPGM 
00211      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA2FPGM 
00212      05  MAP-ERROR-MESSAGE                PIC X(79).              GA2FPGM 
00213      SKIP3                                                        GA2FPGM 
00214  01  FILLER.                                                      GA2FPGM 
00215 ****************************************************************  GA2FPGM 
00216 **   THESE FIELDS DESCRIBE THE NUMBER OF OCCURS FOR THE MAP.      GA2FPGM 
00217 ****************************************************************  GA2FPGM 
00218      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +15.  GA2FPGM 
00219      05  WS-MAP-COL                  PIC S999 COMP-3  VALUE +3.   GA2FPGM 
00220 /                                                                 GA2FPGM 
00221 ** ALTERNATIVE WORKFILE KEYS **                                   GA2FPGM 
00222  01  FILLER                      PIC X(32)  VALUE                 GA2FPGM 
00223      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2FPGM 
00224  01  WS-ALT-WORKFILE-KEYS.                                        GA2FPGM 
00225  COPY GCWRKKEY.                                                   GA2FPGM 
00226 /                                                                 GA2FPGM 
00227 ** HARDCOPY WORK AREA **                                          GA2FPGM 
00228  01  FILLER                      PIC X(26)  VALUE                 GA2FPGM 
00229      '*** HARDCOPY WORK AREA ***'.                                GA2FPGM 
00230  01  WS-HARDCOPY-COMMAREA.                                        GA2FPGM 
00231  COPY PRNCOBOL.                                                   GA2FPGM 
00232                                                                   GA2FPGM 
00233 ** WORKFIELDS **                                                  GA2FPGM 
00234  01  FILLER                           PIC X(16)                   GA2FPGM 
00235              VALUE  '** WORKFIELDS **'.                           GA2FPGM 
00236  01  WS-WORK-FIELDS.                                              GA2FPGM 
00237      05  WS-HEX-00                    PIC X.                      GA2FPGM 
00238      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2FPGM 
00239      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2FPGM 
00240        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2FPGM 
00241      05  WS-SAVED-FIELDS.                                         GA2FPGM 
00242        10  WS-SAVED-BENEFIT           PIC X(6).                   GA2FPGM 
00243 ****************************************************************  GA2FPGM 
00244 ** THESE FIELDS DESCRIBE THE TABULAR AREA COPIED INTO THIS AREA   GA2FPGM 
00245 ** PRIOR TO BEING MERGED BACK INTO THE TABULAR DURING THE SORT.   GA2FPGM 
00246 ****************************************************************  GA2FPGM 
00247      05  WS-SORTED-TAB     OCCURS 46 TIMES INDEXED BY             GA2FPGM 
00248          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2FPGM 
00249        10  WS-BENEFIT-CODE            PIC X(6).                   GA2FPGM 
00250                                                                   GA2FPGM 
00251 *** SWITCHES ***                                                  GA2FPGM 
00252  01  FILLER                           PIC X(14)                   GA2FPGM 
00253              VALUE  '** SWITCHES **'.                             GA2FPGM 
00254  01  WS-SWITCHES.                                                 GA2FPGM 
00255      05  WS-ERROR-SW                  PIC X.                      GA2FPGM 
00256                                                                   GA2FPGM 
00257 ** TITLE LINES **                                                 GA2FPGM 
00258  01  WS-TITLE-LINES.                                              GA2FPGM 
00259      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(47)  VALUE    GA2FPGM 
00260          ' GROUP SPECIFIC ALL LEVEL TABULAR MAINTENANCE  '.       GA2FPGM 
00261      05  CONTRACT-TITLE-LINE                  PIC X(47)  VALUE    GA2FPGM 
00262          '    CONTRACT ALL LEVEL TABULAR MAINTENANCE     '.       GA2FPGM 
00263      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(47)  VALUE    GA2FPGM 
00264          'BENEFIT PROVISION ALL LEVEL TABULAR MAINTENANCE'.       GA2FPGM 
00265                                                                   GA2FPGM 
00266 /                                                                 GA2FPGM 
00267 *** RECORD LENGTHS ***                                            GA2FPGM 
00268  01  FILLER                           PIC X(20)                   GA2FPGM 
00269              VALUE  '** RECORD LENGTHS **'.                       GA2FPGM 
00270  01  WS-RECORD-LENGTHS.                                           GA2FPGM 
00271     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA2FPGM 
00272     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA2FPGM 
00273     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2FPGM 
00274     05 GCVI-COMMAREA-LEN              PIC S9(4) COMP   VALUE +19. GA2FPGM 
00275     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2FPGM 
00276 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA2FPGM 
00277 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA2FPGM 
00278 *   05 GC-GCGRPSPC-FIXED-LEN          PIC S9(5) COMP-3 VALUE +410.GA2FPGM 
00279 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2FPGM 
00280 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA2FPGM 
00281 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA2FPGM 
00282 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2FPGM 
00283 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA2FPGM 
00284 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA2FPGM 
00285 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2FPGM 
00286 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA2FPGM 
00287 ****************************************************************  GA2FPGM 
00288 **   THESE FIELDS DESCRIBE THE TABULAR RECORD.                    GA2FPGM 
00289 ****************************************************************  GA2FPGM 
00290 *   05 GC-GCTABULR-AAR-FIXED-LEN      PIC S9(5) COMP-3 VALUE +40. GA2FPGM 
00291 *   05 GC-GCTABULR-AAR-VARY-LEN       PIC S9(5) COMP-3 VALUE +6.  GA2FPGM 
00292 *   05 GC-GCTABULR-AAR-VARY-MAX-OCUR  PIC S9(5) COMP-3 VALUE +660.GA2FPGM 
00293 /                                                                 GA2FPGM 
00294 ** ATTRIBUTES **                                                  GA2FPGM 
00295  COPY DFHBMSCA.                                                   GA2FPGM 
00296      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2FPGM 
00297 /                                                                 GA2FPGM 
00298 ** ATTENTION IDENTIFIERS **                                       GA2FPGM 
00299  COPY DFHAID.                                                     GA2FPGM 
00300      SKIP3                                                        GA2FPGM 
00301      SKIP3                                                        GA2FPGM 
00302 /                                                                 GA2FPGM 
00303  01  GCVIOPGMS-PARM.                                              GA2FPGM 
00304  COPY GCVINTRC.                                                   GA2FPGM 
00305                                                                   GA2FPGM 
00306  01  WS-GCPS-LENGTHS.                                             GA2FPGM 
00307      COPY GCCDRLEN.                                               GA2FPGM 
00308                                                                   GA2FPGM 
00309                                                                   GA2FPGM 
00310  01  WS-END                          PIC X(16)  VALUE             GA2FPGM 
00311      '*** W/S ENDS ***'.                                          GA2FPGM 
00312 /                                                                 GA2FPGM 
00313  LINKAGE SECTION.                                                 GA2FPGM 
00314 /                                                                 GA2FPGM 
00315  01  DFHCOMMAREA.                                                 GA2FPGM 
00316  COPY G2ALCKEC.                                                   GA2FPGM 
00317 /                                                                 GA2FPGM 
00318 *    05  INCOMING-COMMAREA-PNTR-COMP      PIC S9(8) COMP.         GA2FPGM 
00319 *    05  INCOMING-COMMAREA-PNTR           REDEFINES               GA2FPGM 
00320 *        INCOMING-COMMAREA-PNTR-COMP      USAGE IS POINTER.       GA2FPGM 
00321 *                                                                 GA2FPGM 
00322 *01  BLL-CELLS.                                                   GA2FPGM 
00323 *    02  FILLER                      PIC S9(8)  COMP.             GA2FPGM 
00324 *    02  COMMAREA-PNTR               PIC S9(8)  COMP.             GA2FPGM 
00325 *    02  ALL-LEVEL-TAB-PNTR          PIC S9(8)  COMP.             GA2FPGM 
00326 *    02  ALL-LEVEL-TAB-PNTR2         PIC S9(8)  COMP.             GA2FPGM 
00327 *    02  COPY-AREA-PNTR              PIC S9(8)  COMP.             GA2FPGM 
00328 *    02  GRP-SPEC-PNTR               PIC S9(8)  COMP.             GA2FPGM 
00329 *    02  CONTRACT-PNTR               PIC S9(8)  COMP.             GA2FPGM 
00330 *    02  CONTRACT-PNTR2              PIC S9(8)  COMP.             GA2FPGM 
00331 *    02  BEN-PROV-PNTR               PIC S9(8)  COMP.             GA2FPGM 
00332 *                                                                 GA2FPGM 
00333 *01  GCA-COMMAREA.                                                GA2FPGM 
00334 *COPY G2ALCKEC.                                                   GA2FPGM 
00335 /                                                                 GA2FPGM 
00336 ** I/O PARM, WORKFILE KEY, AND CONTRACT TABULAR RECORD **         GA2FPGM 
00337  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA2FPGM 
00338  COPY GCIOPRM1.                                                   GA2FPGM 
00339 /                                                                 GA2FPGM 
00340  COPY GCWRKDCC.                                                   GA2FPGM 
00341 /                                                                 GA2FPGM 
00342  COPY GCTAARC.                                                    GA2FPGM 
00343 /                                                                 GA2FPGM 
00344 ****************************************************************  GA2FPGM 
00345 ** THESE FIELDS DESCRIBE THE TABULAR AREA COPIED INTO THIS AREA   GA2FPGM 
00346 ** PRIOR TO BEING MERGED BACK INTO THE TABULAR DURING THE SORT.   GA2FPGM 
00347 ****************************************************************  GA2FPGM 
00348  01  COPY-OF-TABLE-AREA.                                          GA2FPGM 
00349      05  COPY-OF-TABLE    OCCURS 660 TIMES    INDEXED BY          GA2FPGM 
00350            COPY-IDX.                                              GA2FPGM 
00351        10  COPY-BENEFIT-CODE    PIC X(6).                         GA2FPGM 
00352 /                                                                 GA2FPGM 
00353 ** IO PARM, WITH WORKFILE KEY, AND RECORDS **                     GA2FPGM 
00354  01  IO-PARM-GRP-SPEC-RECORD.                                     GA2FPGM 
00355  COPY GCIOPRM2.                                                   GA2FPGM 
00356 /                                                                 GA2FPGM 
00357  COPY GCWRKDC2.                                                   GA2FPGM 
00358 /                                                                 GA2FPGM 
00359  COPY GCGROUPC.                                                   GA2FPGM 
00360 /                                                                 GA2FPGM 
00361                                                                   GA2FPGM 
00362  01  IO-PARM-CONTRACT-RECORD.                                     GA2FPGM 
00363  COPY GCIOPRM3.                                                   GA2FPGM 
00364 /                                                                 GA2FPGM 
00365  COPY GCWRKDC3.                                                   GA2FPGM 
00366 /                                                                 GA2FPGM 
00367  COPY GCCONTRC.                                                   GA2FPGM 
00368 /                                                                 GA2FPGM 
00369                                                                   GA2FPGM 
00370  01  IO-PARM-BEN-PROV-RECORD.                                     GA2FPGM 
00371  COPY GCIOPRM4.                                                   GA2FPGM 
00372 /                                                                 GA2FPGM 
00373  COPY GCWRKDC4.                                                   GA2FPGM 
00374 /                                                                 GA2FPGM 
00375  COPY GCBENPVC.                                                   GA2FPGM 
00376 /                                                                 GA2FPGM 
00377                                                                   GA2FPGM 
00378  PROCEDURE DIVISION.                                              GA2FPGM 
00379                                                                   GA2FPGM 
00380 ******************************************************************GA2FPGM 
00381 **                H O U S E K E E P I N G                         GA2FPGM 
00382 **                                                                GA2FPGM 
00383 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2FPGM 
00384 **                                                                GA2FPGM 
00385 ******************************************************************GA2FPGM 
00386  0000-HOUSEKEEPING SECTION.                                       GA2FPGM 
00387                                                                   GA2FPGM 
00388      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2FPGM 
00389                                                                   GA2FPGM 
00390      IF EIBAID  =  DFHCLEAR                                       GA2FPGM 
00391          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2FPGM 
00392                         ERASE                                     GA2FPGM 
00393          END-EXEC                                                 GA2FPGM 
00394          EXEC CICS RETURN                                         GA2FPGM 
00395          END-EXEC.                                                GA2FPGM 
00396                                                                   GA2FPGM 
00397      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2FPGM 
00398                END-EXEC.                                          GA2FPGM 
00399 /                                                                 GA2FPGM 
00400 ******************************************************************GA2FPGM 
00401 **                     M A I N L I N E                            GA2FPGM 
00402 **                                                                GA2FPGM 
00403 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2FPGM 
00404 **  TAKEN BY THE OPERATOR.                                        GA2FPGM 
00405 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2FPGM 
00406 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2FPGM 
00407 **     ADDITIONS FROM.                                            GA2FPGM 
00408 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2FPGM 
00409 **     KEY PF12 OR PF24.                                          GA2FPGM 
00410 **  3. RECEIVE THE SCREEN.                                        GA2FPGM 
00411 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2FPGM 
00412 **     MENU.                                                      GA2FPGM 
00413 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2FPGM 
00414 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2FPGM 
00415 **     (RETURN) TO THE DELETE PROGRAM (GA1FPGM).                  GA2FPGM 
00416 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2FPGM 
00417 **     (RETURN) TO THE PREVIOUS MENU.                             GA2FPGM 
00418 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2FPGM 
00419 **     NORMAL ADD PROCESSING, EXCEPT BYPASS EMPTY VALIDATION TABLEGA2FPGM 
00420 **     CONDITION FOR THE PROVISION ID ARGUMENT.                   GA2FPGM 
00421 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2FPGM 
00422 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2FPGM 
00423 **                                                                GA2FPGM 
00424 ******************************************************************GA2FPGM 
00425  1000-MAIN-LINE SECTION.                                          GA2FPGM 
00426                                                                   GA2FPGM 
00427      MOVE '1000'  TO  WS-PARA-ID.                                 GA2FPGM 
00428      IF EIBTRNID  NOT =  'GA2F'                                   GA2FPGM 
00429         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2FPGM 
00430         GO TO 1099-RETURN.                                        GA2FPGM 
00431                                                                   GA2FPGM 
00432      EXEC CICS RECEIVE   MAP('GA2FI01') MAPSET('GA2FSET')         GA2FPGM 
00433         INTO(GA2FI01I) END-EXEC.                                  GA2FPGM 
00434                                                                   GA2FPGM 
00435      IF MAP-SCREEN-ID  NOT = '002F00'                             GA2FPGM 
00436         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2FPGM 
00437                                                                   GA2FPGM 
00438      IF EIBAID  =  DFHENTER                                       GA2FPGM 
00439         PERFORM 2000-ADD-PROCESSING                               GA2FPGM 
00440         GO TO 1099-RETURN.                                        GA2FPGM 
00441                                                                   GA2FPGM 
00442      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2FPGM 
00443         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2FPGM 
00444                                                                   GA2FPGM 
00445      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2FPGM 
00446         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2FPGM 
00447                                                                   GA2FPGM 
00448      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2FPGM 
00449         PERFORM 2000-ADD-PROCESSING                               GA2FPGM 
00450         GO TO 1099-RETURN.                                        GA2FPGM 
00451                                                                   GA2FPGM 
00452      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA2FPGM 
00453      MOVE -1  TO  MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2).      GA2FPGM 
00454      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2FPGM 
00455 -    ' THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                  GA2FPGM 
00456      EXEC CICS SEND   MAP('GA2FI01') MAPSET('GA2FSET') DATAONLY   GA2FPGM 
00457         FROM(GA2FI01O) CURSOR END-EXEC.                           GA2FPGM 
00458                                                                   GA2FPGM 
00459  1099-RETURN.                                                     GA2FPGM 
00460 *    EXEC CICS RETURN   END-EXEC.                                 GA2FPGM 
00461      EXEC CICS RETURN TRANSID ('GA2F')                            GA2FPGM 
00462                COMMAREA (DFHCOMMAREA)                             GA2FPGM 
00463                END-EXEC.                                          GA2FPGM 
00464                                                                   GA2FPGM 
00465      GOBACK.                                                      GA2FPGM 
00466 /                                                                 GA2FPGM 
00467 ******************************************************************GA2FPGM 
00468 **               A D D   P R O C E S S I N G                      GA2FPGM 
00469 **                                                                GA2FPGM 
00470 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2FPGM 
00471 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2FPGM.            GA2FPGM 
00472 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2FPGM 
00473 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2FPGM 
00474 **     SKIP TO THE NEXT LINE.                                     GA2FPGM 
00475 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2FPGM 
00476 **     WITH SPECIAL CHARACTERS, AND NUMERIC FIELDS ARE TESTED     GA2FPGM 
00477 **     WITH THE NUMERIC CLASS TEST.  THE OPERATOR MUST ENTER SOME GA2FPGM 
00478 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2FPGM 
00479 **     DATA.                                                      GA2FPGM 
00480 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2FPGM 
00481 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2FPGM 
00482 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2FPGM 
00483 **     ORDER, FIELD BY FIELD.                                     GA2FPGM 
00484 **  6. THE ALL LEVEL TABULAR RECORD IS READ, AND A COPY OF THE    GA2FPGM 
00485 **     TABLE IS MADE.                                             GA2FPGM 
00486 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2FPGM 
00487 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2FPGM 
00488 **     BACK INTO THE RECORD.                                      GA2FPGM 
00489 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2FPGM 
00490 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2FPGM 
00491 **                                                                GA2FPGM 
00492 ******************************************************************GA2FPGM 
00493  2000-ADD-PROCESSING SECTION.                                     GA2FPGM 
00494                                                                   GA2FPGM 
00495      MOVE '2000'  TO  WS-PARA-ID.                                 GA2FPGM 
00496      MOVE 'N'     TO  WS-ERROR-SW.                                GA2FPGM 
00497      MOVE 'Y'     TO  GCVI-TABLE-SW.                              GA2FPGM 
00498      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2FPGM 
00499      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA2FPGM 
00500                                                                   GA2FPGM 
00501      MOVE '2005'  TO  WS-PARA-ID.                                 GA2FPGM 
00502  2005-RESET-ALL-ATTRIBUTES.                                       GA2FPGM 
00503      MOVE DFHBMUNF TO MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2). GA2FPGM 
00504      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2FPGM 
00505         SET MAP-IDX1  UP BY  1                                    GA2FPGM 
00506         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2FPGM 
00507                                                                   GA2FPGM 
00508      IF MAP-IDX2  <  WS-MAP-COL                                   GA2FPGM 
00509         SET MAP-IDX1  TO  1                                       GA2FPGM 
00510         SET MAP-IDX2  UP BY  1                                    GA2FPGM 
00511         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2FPGM 
00512                                                                   GA2FPGM 
00513      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA2FPGM 
00514      MOVE '2010'  TO  WS-PARA-ID.                                 GA2FPGM 
00515  2010-VALIDATE-ADD-ENTRIES.                                       GA2FPGM 
00516      IF MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2) =  ZERO         GA2FPGM 
00517         IF MAP-IDX1  <  WS-MAP-ROW                                GA2FPGM 
00518            SET MAP-IDX1  UP BY  1                                 GA2FPGM 
00519            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2FPGM 
00520         ELSE                                                      GA2FPGM 
00521            IF MAP-IDX2  <  WS-MAP-COL                             GA2FPGM 
00522               SET MAP-IDX1  TO  1                                 GA2FPGM 
00523               SET MAP-IDX2  UP BY  1                              GA2FPGM 
00524               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2FPGM 
00525            ELSE                                                   GA2FPGM 
00526               GO TO 2020-CHECK-FOR-ERRORS.                        GA2FPGM 
00527                                                                   GA2FPGM 
00528      IF  MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2)  =  ZERO       GA2FPGM 
00529         MOVE DFHBMUBF  TO                                         GA2FPGM 
00530            MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)             GA2FPGM 
00531         MOVE '??????'  TO                                         GA2FPGM 
00532            MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)                  GA2FPGM 
00533         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2FPGM 
00534            MOVE 'Y'  TO  WS-ERROR-SW                              GA2FPGM 
00535            MOVE -1   TO                                           GA2FPGM 
00536               MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2)           GA2FPGM 
00537            MOVE ' *** BENEFIT CODE IS INVALID ***'                GA2FPGM 
00538               TO MAP-ERROR-MESSAGE.                               GA2FPGM 
00539                                                                   GA2FPGM 
00540      IF MAP-BENEFIT-CODE-ATTR(MAP-IDX1, MAP-IDX2)  NOT =  DFHBMUBFGA2FPGM 
00541         MOVE 'MULT01' TO GCVI-FIELDS-KEY-ID                       GA2FPGM 
00542         MOVE  ZEROES  TO GCVI-RETURN-CODE                         GA2FPGM 
00543         MOVE MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)                GA2FPGM 
00544                       TO GCVI-VALUE-LEN-6                         GA2FPGM 
00545         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2FPGM 
00546                        COMMAREA(GCVIOPGMS-PARM)                   GA2FPGM 
00547                        LENGTH(GCVI-COMMAREA-LEN) END-EXEC         GA2FPGM 
00548         IF GCVI-VALUE-NOT-FOUND                                   GA2FPGM 
00549            MOVE 'MULT06' TO GCVI-FIELDS-KEY-ID                    GA2FPGM 
00550            MOVE  ZEROES  TO GCVI-RETURN-CODE                      GA2FPGM 
00551            MOVE MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)             GA2FPGM 
00552                          TO GCVI-VALUE-LEN-6                      GA2FPGM 
00553            EXEC CICS LINK PROGRAM('GCVIOPGM')                     GA2FPGM 
00554                           COMMAREA(GCVIOPGMS-PARM)                GA2FPGM 
00555                           LENGTH(GCVI-COMMAREA-LEN) END-EXEC      GA2FPGM 
00556            IF GCVI-VALUE-NOT-FOUND                                GA2FPGM 
00557               IF WS-ERROR-SW  NOT =  'Y'                          GA2FPGM 
00558                  MOVE DFHBMUBF  TO                                GA2FPGM 
00559                       MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)  GA2FPGM 
00560                  MOVE -1  TO                                      GA2FPGM 
00561                       MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2)   GA2FPGM 
00562                  MOVE '*** BENEFIT CODE IS NOT VALID ***'         GA2FPGM 
00563                    TO MAP-ERROR-MESSAGE                           GA2FPGM 
00564                  MOVE 'Y'  TO  WS-ERROR-SW                        GA2FPGM 
00565               ELSE                                                GA2FPGM 
00566                  MOVE DFHBMUBF  TO                                GA2FPGM 
00567                       MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)  GA2FPGM 
00568            ELSE                                                   GA2FPGM 
00569               IF GCVI-VALUE-NOT-LOADED                            GA2FPGM 
00570                  IF EIBAID  =  DFHPF4 OR  =  DFHPF16              GA2FPGM 
00571                     NEXT SENTENCE                                 GA2FPGM 
00572                  ELSE                                             GA2FPGM 
00573                     MOVE DFHBMUBF  TO                             GA2FPGM 
00574                         MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)GA2FPGM 
00575                     IF WS-ERROR-SW  NOT =  'Y'                    GA2FPGM 
00576                        MOVE 'Y'  TO  WS-ERROR-SW                  GA2FPGM 
00577                        MOVE -1  TO                                GA2FPGM 
00578                        MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA2FPGM 
00579               MOVE 'EDIT TABLE EMPTY - DATA NOT VALIDATED -  PRESSGA2FPGM 
00580 -                      ' PF4 / PF16 TO CONTINUE'                  GA2FPGM 
00581                 TO  MAP-ERROR-MESSAGE.                            GA2FPGM 
00582                                                                   GA2FPGM 
00583      IF  MAP-BENEFIT-CODE-ATTR (MAP-IDX1, MAP-IDX2)               GA2FPGM 
00584             NOT  =  DFHBMUBF                                      GA2FPGM 
00585         ADD 1  TO  WS-ADD-COUNT                                   GA2FPGM 
00586         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2FPGM 
00587         MOVE MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2)  TO            GA2FPGM 
00588            WS-BENEFIT-CODE (WS-SORT-IDX).                         GA2FPGM 
00589                                                                   GA2FPGM 
00590      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2FPGM 
00591         SET MAP-IDX1  UP BY  1                                    GA2FPGM 
00592         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2FPGM 
00593                                                                   GA2FPGM 
00594      IF MAP-IDX2  <  WS-MAP-COL                                   GA2FPGM 
00595         SET MAP-IDX1  TO  1                                       GA2FPGM 
00596         SET MAP-IDX2  UP BY  1                                    GA2FPGM 
00597         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2FPGM 
00598                                                                   GA2FPGM 
00599                                                                   GA2FPGM 
00600                                                                   GA2FPGM 
00601  2020-CHECK-FOR-ERRORS.                                           GA2FPGM 
00602      MOVE '2020'  TO  WS-PARA-ID.                                 GA2FPGM 
00603                                                                   GA2FPGM 
00604      IF WS-ERROR-SW  =  'Y' OR GCVI-TABLE-SW = 'N'                GA2FPGM 
00605         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA2FPGM 
00606                              MAP-TITLE-LINE                       GA2FPGM 
00607                              MAP-SCREEN-ID                        GA2FPGM 
00608                              MAP-ALL-LEVEL-TAB-ID                 GA2FPGM 
00609                              MAP-ALL-LEVEL-TAB-SLOT               GA2FPGM 
00610                              MAP-FROM-MENU-ID                     GA2FPGM 
00611                              MAP-ID-LINE                          GA2FPGM 
00612         MOVE '2100'  TO  WS-PARA-ID                               GA2FPGM 
00613         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2FPGM 
00614            VARYING MAP-IDX2  FROM  1  BY  1                       GA2FPGM 
00615               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2FPGM 
00616            AFTER MAP-IDX1  FROM  1  BY  1                         GA2FPGM 
00617               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2FPGM 
00618         EXEC CICS SEND   MAP('GA2FI01') MAPSET('GA2FSET')         GA2FPGM 
00619            DATAONLY FROM(GA2FI01O) CURSOR END-EXEC                GA2FPGM 
00620         GO TO 2099-EXIT.                                          GA2FPGM 
00621                                                                   GA2FPGM 
00622                                                                   GA2FPGM 
00623      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2FPGM 
00624         MOVE '                  *** ADD ENTRY NOT FOUND ***'      GA2FPGM 
00625            TO  MAP-ERROR-MESSAGE                                  GA2FPGM 
00626         SET MAP-IDX1,  MAP-IDX2  TO  1                            GA2FPGM 
00627         MOVE -1  TO  MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2)    GA2FPGM 
00628         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA2FPGM 
00629                              MAP-TITLE-LINE                       GA2FPGM 
00630                              MAP-SCREEN-ID                        GA2FPGM 
00631                              MAP-ALL-LEVEL-TAB-ID                 GA2FPGM 
00632                              MAP-ALL-LEVEL-TAB-SLOT               GA2FPGM 
00633                              MAP-FROM-MENU-ID                     GA2FPGM 
00634                              MAP-ID-LINE                          GA2FPGM 
00635         EXEC CICS SEND   MAP('GA2FI01') MAPSET('GA2FSET')         GA2FPGM 
00636            DATAONLY FROM(GA2FI01O) CURSOR END-EXEC                GA2FPGM 
00637         GO TO 2099-EXIT.                                          GA2FPGM 
00638                                                                   GA2FPGM 
00639  2025-CONTINUE-PROCESSING.                                        GA2FPGM 
00640                                                                   GA2FPGM 
00641      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2FPGM 
00642               GC-GCIOPARM-LEN                 +                   GA2FPGM 
00643               GC-WORKFILE-KEY-LEN             +                   GA2FPGM 
00644               GC-GCTABULR-AAR-FIXED-LEN       +                   GA2FPGM 
00645             (GC-GCTABULR-AAR-VARY-LEN *                           GA2FPGM 
00646             GC-GCTABULR-AAR-VARY-MAX-OCUR).                       GA2FPGM 
00647                                                                   GA2FPGM 
00648 ***  EXEC CICS GETMAIN SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00) GA2FPGM 
00649      EXEC CICS GETMAIN                                            GA2FPGM 
00650         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2FPGM 
00651         INITIMG(WS-HEX-00)                                        GA2FPGM 
00652         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2FPGM 
00653                                                                   GA2FPGM 
00654 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2FPGM 
00655 ***  ADD ALL-LEVEL-TAB-PNTR, 4096  GIVING  ALL-LEVEL-TAB-PNTR2.   GA2FPGM 
00656                                                                   GA2FPGM 
00657      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2FPGM 
00658         MOVE SPACES                TO GCIO-WORKFILE-KEY           GA2FPGM 
00659         MOVE  'G'                  TO GCIO-WRK-STATUS-CODE        GA2FPGM 
00660         MOVE  'G3'                 TO GCIO-WRK-RECORD-TYPE        GA2FPGM 
00661         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2FPGM 
00662         MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2FPGM 
00663         MOVE GRP-SPEC-GROUP-NO     TO GCIO-WRK-GROUP-NO           GA2FPGM 
00664         MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2FPGM 
00665         MOVE GRP-SPEC-SECTION-NO   TO GCIO-WRK-SECTION-NO         GA2FPGM 
00666         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2FPGM 
00667         MOVE SPACES                TO GCIO-WRK-LINE-OF-BUS        GA2FPGM 
00668                                       GCIO-WRK-PROVIDER-CONTROL   GA2FPGM 
00669         MOVE GRP-SPEC-FAM-REL-LVL  TO GCIO-WRK-FAMILY-RELATION-LVLGA2FPGM 
00670         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2FPGM 
00671         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-PROVISION-ID       GA2FPGM 
00672         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA2FPGM 
00673         MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID   GA2FPGM 
00674         MOVE ZEROES                TO GCIO-WRK-TAB-PROV-SLOT-NO.  GA2FPGM 
00675                                                                   GA2FPGM 
00676      IF  MAP-FROM-MENU-ID  = 'GC4A'  OR  'GTM1'                   GA2FPGM 
00677         MOVE SPACES                TO GCIO-WORKFILE-KEY           GA2FPGM 
00678         MOVE  'C'                  TO GCIO-WRK-STATUS-CODE        GA2FPGM 
00679         MOVE  'C3'                 TO GCIO-WRK-RECORD-TYPE        GA2FPGM 
00680         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2FPGM 
00681         MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2FPGM 
00682         MOVE CONTRACT-GROUP-NO     TO GCIO-WRK-GROUP-NO           GA2FPGM 
00683         MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2FPGM 
00684         MOVE CONTRACT-SECTION-NO   TO GCIO-WRK-SECTION-NO         GA2FPGM 
00685         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2FPGM 
00686         MOVE CONTRACT-LOB          TO GCIO-WRK-LINE-OF-BUS        GA2FPGM 
00687         MOVE CONTRACT-PROV-CTL     TO GCIO-WRK-PROVIDER-CONTROL   GA2FPGM 
00688         MOVE CONTRACT-FAM-REL-LVL  TO GCIO-WRK-FAMILY-RELATION-LVLGA2FPGM 
00689         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2FPGM 
00690         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-PROVISION-ID       GA2FPGM 
00691         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA2FPGM 
00692         MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID   GA2FPGM 
00693         MOVE ZEROES                TO GCIO-WRK-TAB-PROV-SLOT-NO.  GA2FPGM 
00694                                                                   GA2FPGM 
00695      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2FPGM 
00696         MOVE SPACES                TO GCIO-WORKFILE-KEY           GA2FPGM 
00697         MOVE  'C'                  TO GCIO-WRK-STATUS-CODE        GA2FPGM 
00698         MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE        GA2FPGM 
00699         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2FPGM 
00700         MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2FPGM 
00701         MOVE BEN-PROV-GROUP-NO     TO GCIO-WRK-GROUP-NO           GA2FPGM 
00702         MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2FPGM 
00703         MOVE BEN-PROV-SECTION-NO   TO GCIO-WRK-SECTION-NO         GA2FPGM 
00704         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2FPGM 
00705         MOVE BEN-PROV-LOB          TO GCIO-WRK-LINE-OF-BUS        GA2FPGM 
00706         MOVE BEN-PROV-PROV-CTL     TO GCIO-WRK-PROVIDER-CONTROL   GA2FPGM 
00707         MOVE BEN-PROV-FAM-REL-LVL  TO GCIO-WRK-FAMILY-RELATION-LVLGA2FPGM 
00708         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2FPGM 
00709         MOVE BEN-PROV-ID-NO        TO GCIO-WRK-PROVISION-ID       GA2FPGM 
00710         MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO  GA2FPGM 
00711         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-TAB-PROVISION-ID   GA2FPGM 
00712         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO. GA2FPGM 
00713                                                                   GA2FPGM 
00714      MOVE 'GCPSWORK'         TO  GCIO-FILE-DDNAME.                GA2FPGM 
00715      MOVE 1                  TO  GCIO-IO-AREA-TO-USE.             GA2FPGM 
00716      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2FPGM 
00717                                                                   GA2FPGM 
00718      MOVE GC-GCTABULR-AAR-VARY-MAX-OCUR                           GA2FPGM 
00719      TO   GAE-ENTRY-COUNT.                                        GA2FPGM 
00720                                                                   GA2FPGM 
00721      MOVE  'RU '             TO  GCIO-FILE-ACCESS-CODE.           GA2FPGM 
00722                                                                   GA2FPGM 
00723      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2FPGM 
00724         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2FPGM 
00725         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2FPGM 
00726                                                                   GA2FPGM 
00727      IF  NOT GCIO-GOOD-RETURN                                     GA2FPGM 
00728         MOVE '*** ERROR READING ALL LEVEL TABULAR.  CONTACT SYSTEMGA2FPGM 
00729 -    'S AREA ***'    TO  MAP-ERROR-MESSAGE                        GA2FPGM 
00730         MOVE '2FF1'  TO  WS-ABEND-CODE                            GA2FPGM 
00731         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
00732                                                                   GA2FPGM 
00733      IF  WS-ADD-COUNT  NOT >  ZERO                                GA2FPGM 
00734         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2FPGM 
00735                                                                   GA2FPGM 
00736      SET WS-SORT-IDX   TO  1.                                     GA2FPGM 
00737      SET WS-SORT-IDX2  TO  2.                                     GA2FPGM 
00738      MOVE '2030'       TO  WS-PARA-ID.                            GA2FPGM 
00739                                                                   GA2FPGM 
00740  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2FPGM 
00741      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2FPGM 
00742         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2FPGM 
00743                                                                   GA2FPGM 
00744      IF WS-BENEFIT-CODE (WS-SORT-IDX) <                           GA2FPGM 
00745         WS-BENEFIT-CODE (WS-SORT-IDX2)                            GA2FPGM 
00746         SET WS-SORT-IDX2  UP BY  1                                GA2FPGM 
00747         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2FPGM 
00748      ELSE                                                         GA2FPGM 
00749         IF WS-BENEFIT-CODE (WS-SORT-IDX) >                        GA2FPGM 
00750            WS-BENEFIT-CODE (WS-SORT-IDX2)                         GA2FPGM 
00751            MOVE WS-BENEFIT-CODE (WS-SORT-IDX)  TO                 GA2FPGM 
00752               WS-SAVED-BENEFIT                                    GA2FPGM 
00753            MOVE WS-BENEFIT-CODE (WS-SORT-IDX2)  TO                GA2FPGM 
00754               WS-BENEFIT-CODE (WS-SORT-IDX)                       GA2FPGM 
00755            MOVE WS-SAVED-BENEFIT       TO                         GA2FPGM 
00756               WS-BENEFIT-CODE (WS-SORT-IDX2)                      GA2FPGM 
00757            SET WS-SORT-IDX2  UP BY  1                             GA2FPGM 
00758            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2FPGM 
00759                                                                   GA2FPGM 
00760      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2FPGM 
00761      MOVE WS-BENEFIT-CODE (WS-SORT-IDX3)  TO                      GA2FPGM 
00762         WS-BENEFIT-CODE (WS-SORT-IDX2).                           GA2FPGM 
00763      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2FPGM 
00764      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2FPGM 
00765                                                                   GA2FPGM 
00766  2040-ARE-WE-DONE-WITH-SORT.                                      GA2FPGM 
00767      MOVE '2040'  TO  WS-PARA-ID.                                 GA2FPGM 
00768      SET WS-SORT-IDX  UP BY  1.                                   GA2FPGM 
00769      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2FPGM 
00770         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2FPGM 
00771         SET WS-SORT-IDX2  UP BY  1                                GA2FPGM 
00772         MOVE '2030'       TO  WS-PARA-ID                          GA2FPGM 
00773         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2FPGM 
00774                                                                   GA2FPGM 
00775      SET WS-ADD-COUNT      TO  WS-SORT-IDX.                       GA2FPGM 
00776      MOVE HIGH-VALUES      TO  WS-SORTED-TAB (WS-SORT-IDX).       GA2FPGM 
00777      MOVE GAE-ENTRY-COUNT  TO  GAE-ENTRY-COUNT.                   GA2FPGM 
00778                                                                   GA2FPGM 
00779      COMPUTE  WS-COPY-LENGTH  =                                   GA2FPGM 
00780           GAE-ENTRY-COUNT  *  GC-GCTABULR-AAR-VARY-LEN.           GA2FPGM 
00781                                                                   GA2FPGM 
00782 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA2FPGM 
00783      EXEC CICS GETMAIN                                            GA2FPGM 
00784         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA2FPGM 
00785         LENGTH(WS-COPY-LENGTH)                                    GA2FPGM 
00786         INITIMG(WS-HEX-00) END-EXEC.                              GA2FPGM 
00787 ***  SERVICE RELOAD  COPY-OF-TABLE-AREA.                          GA2FPGM 
00788                                                                   GA2FPGM 
00789      SET COPY-IDX,  GAE-INDEX  TO 1.                              GA2FPGM 
00790                                                                   GA2FPGM 
00791      MOVE '2050'  TO  WS-PARA-ID.                                 GA2FPGM 
00792  2050-MAKE-A-COPY-OF-RECORD.                                      GA2FPGM 
00793      IF GAE-INDEX  NOT >  GAE-ENTRY-COUNT                         GA2FPGM 
00794         MOVE GAE-ENTRY (GAE-INDEX)  TO                            GA2FPGM 
00795            COPY-OF-TABLE (COPY-IDX)                               GA2FPGM 
00796         SET COPY-IDX, GAE-INDEX  UP BY  1                         GA2FPGM 
00797         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2FPGM 
00798                                                                   GA2FPGM 
00799      IF WS-ADD-COUNT  +  GAE-ENTRY-COUNT  >                       GA2FPGM 
00800         GC-GCTABULR-AAR-VARY-MAX-OCUR                             GA2FPGM 
00801         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2FPGM 
00802 -    'EASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE       GA2FPGM 
00803         MOVE '2FL1'  TO  WS-ABEND-CODE                            GA2FPGM 
00804         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
00805                                                                   GA2FPGM 
00806      SET WS-SORT-IDX, COPY-IDX, GAE-INDEX  TO  1.                 GA2FPGM 
00807      MOVE '2060'  TO  WS-PARA-ID.                                 GA2FPGM 
00808  2060-MERGE-IN-NEW-ENTRIES.                                       GA2FPGM 
00809      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2FPGM 
00810         SET GAE-INDEX  DOWN BY  1                                 GA2FPGM 
00811         SET GAE-ENTRY-COUNT   TO  GAE-INDEX                       GA2FPGM 
00812         MOVE GAE-ENTRY-COUNT  TO  GAE-ENTRY-COUNT                 GA2FPGM 
00813         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2FPGM 
00814                                                                   GA2FPGM 
00815      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2FPGM 
00816             =  HIGH-VALUES  AND                                   GA2FPGM 
00817         COPY-OF-TABLE (COPY-IDX)  NOT  =  HIGH-VALUES             GA2FPGM 
00818         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2FPGM 
00819                                                                   GA2FPGM 
00820      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2FPGM 
00821             NOT  =  HIGH-VALUES  AND                              GA2FPGM 
00822         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2FPGM 
00823         GO TO 2080-INSERT-NEW-ENTRY.                              GA2FPGM 
00824                                                                   GA2FPGM 
00825      IF WS-SORTED-TAB (WS-SORT-IDX)                               GA2FPGM 
00826             =  HIGH-VALUES  AND                                   GA2FPGM 
00827         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2FPGM 
00828         NEXT SENTENCE                                             GA2FPGM 
00829      ELSE                                                         GA2FPGM 
00830         IF WS-BENEFIT-CODE (WS-SORT-IDX)  >                       GA2FPGM 
00831            COPY-BENEFIT-CODE (COPY-IDX)                           GA2FPGM 
00832            GO TO 2070-SAVE-COPIED-ENTRY                           GA2FPGM 
00833         ELSE                                                      GA2FPGM 
00834            IF WS-BENEFIT-CODE (WS-SORT-IDX)  <                    GA2FPGM 
00835               COPY-BENEFIT-CODE (COPY-IDX)                        GA2FPGM 
00836               GO TO 2080-INSERT-NEW-ENTRY.                        GA2FPGM 
00837                                                                   GA2FPGM 
00838 ****************************************************************  GA2FPGM 
00839 **   AT THIS POINT THE NEW ENTRY'S THREE FIELDS MUST BE EQUAL TO  GA2FPGM 
00840 **   THE OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING  GA2FPGM 
00841 **   THE INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE   GA2FPGM 
00842 **   ENTRY FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.  GA2FPGM 
00843 ****************************************************************  GA2FPGM 
00844                                                                   GA2FPGM 
00845      SET WS-SORT-IDX  UP BY  1.                                   GA2FPGM 
00846                                                                   GA2FPGM 
00847  2070-SAVE-COPIED-ENTRY.                                          GA2FPGM 
00848      MOVE '2070'  TO  WS-PARA-ID.                                 GA2FPGM 
00849      MOVE COPY-OF-TABLE (COPY-IDX)       TO                       GA2FPGM 
00850         GAE-ENTRY (GAE-INDEX).                                    GA2FPGM 
00851      IF COPY-IDX  NOT >  GAE-ENTRY-COUNT                          GA2FPGM 
00852         SET COPY-IDX  UP BY  1                                    GA2FPGM 
00853         SET GAE-INDEX  UP BY  1                                   GA2FPGM 
00854         MOVE '2060'  TO  WS-PARA-ID                               GA2FPGM 
00855         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2FPGM 
00856      ELSE                                                         GA2FPGM 
00857         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2FPGM 
00858 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2FPGM 
00859         MOVE '2FL2'  TO  WS-ABEND-CODE                            GA2FPGM 
00860         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
00861                                                                   GA2FPGM 
00862  2080-INSERT-NEW-ENTRY.                                           GA2FPGM 
00863      MOVE '2080'  TO  WS-PARA-ID.                                 GA2FPGM 
00864                                                                   GA2FPGM 
00865      MOVE WS-BENEFIT-CODE (WS-SORT-IDX)  TO                       GA2FPGM 
00866         GAE-BENEFIT-CODE (GAE-INDEX).                             GA2FPGM 
00867                                                                   GA2FPGM 
00868      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2FPGM 
00869         SET WS-SORT-IDX  UP BY  1                                 GA2FPGM 
00870         SET GAE-INDEX  UP BY  1                                   GA2FPGM 
00871         MOVE '2060'  TO  WS-PARA-ID                               GA2FPGM 
00872         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2FPGM 
00873      ELSE                                                         GA2FPGM 
00874         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2FPGM 
00875 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2FPGM 
00876         MOVE '2FL3'  TO  WS-ABEND-CODE                            GA2FPGM 
00877         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
00878                                                                   GA2FPGM 
00879  2090-UPDATE-ALL-LVL-TAB-REC.                                     GA2FPGM 
00880      MOVE '2090'  TO  WS-PARA-ID.                                 GA2FPGM 
00881                                                                   GA2FPGM 
00882 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2FPGM 
00883                                                                   GA2FPGM 
00884      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2FPGM 
00885      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2FPGM 
00886                                                                   GA2FPGM 
00887      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN        +GA2FPGM 
00888                     GC-GCTABULR-AAR-FIXED-LEN +                   GA2FPGM 
00889              (GAE-ENTRY-COUNT  *  GC-GCTABULR-AAR-VARY-LEN).      GA2FPGM 
00890                                                                   GA2FPGM 
00891      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2FPGM 
00892            GC-GCIOPARM-LEN      +  GCIO-RECORD-LENGTH.            GA2FPGM 
00893                                                                   GA2FPGM 
00894      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2FPGM 
00895         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2FPGM 
00896         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2FPGM 
00897                                                                   GA2FPGM 
00898      IF NOT GCIO-GOOD-RETURN                                      GA2FPGM 
00899         MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASGA2FPGM 
00900 -    'E CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE          GA2FPGM 
00901         MOVE '2FF2'  TO  WS-ABEND-CODE                            GA2FPGM 
00902         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
00903                                                                   GA2FPGM 
00904      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2FPGM 
00905         VARYING MAP-IDX2  FROM 1  BY  1                           GA2FPGM 
00906            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2FPGM 
00907         AFTER MAP-IDX1  FROM 1  BY  1                             GA2FPGM 
00908            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2FPGM 
00909                                                                   GA2FPGM 
00910      EXEC CICS SEND   MAP('GA2FI01') MAPSET('GA2FSET') ERASE      GA2FPGM 
00911         FROM(GA2FI01O) END-EXEC.                                  GA2FPGM 
00912                                                                   GA2FPGM 
00913  2099-EXIT.   EXIT.                                               GA2FPGM 
00914 /                                                                 GA2FPGM 
00915 ******************************************************************GA2FPGM 
00916 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2FPGM 
00917 **                                                                GA2FPGM 
00918 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2FPGM 
00919 **  ALREADY ON THE OPERATORS SCREEN.                              GA2FPGM 
00920 **                                                                GA2FPGM 
00921 ******************************************************************GA2FPGM 
00922  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2FPGM 
00923                                                                   GA2FPGM 
00924      MOVE LOW-VALUES  TO  MAP-BENEFIT-CODE (MAP-IDX1, MAP-IDX2).  GA2FPGM 
00925                                                                   GA2FPGM 
00926  2199-EXIT.   EXIT.                                               GA2FPGM 
00927 /                                                                 GA2FPGM 
00928 ******************************************************************GA2FPGM 
00929 **          X C T L   T O   D E L   S C R E E N                   GA2FPGM 
00930 **                                                                GA2FPGM 
00931 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2FPGM 
00932 ** DELETING ENTRIES.  WE READ THE ALL LEVEL TABULAR RECORD & PASS GA2FPGM 
00933 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, & ALL LEVEL TABULARGA2FPGM 
00934 ** RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU THE      GA2FPGM 
00935 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA2FPGM 
00936 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2FPGM 
00937 ******************************************************************GA2FPGM 
00938  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2FPGM 
00939      MOVE '3000'  TO  WS-PARA-ID.                                 GA2FPGM 
00940                                                                   GA2FPGM 
00941      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2FPGM 
00942            GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +   GA2FPGM 
00943            GC-GCTABULR-AAR-FIXED-LEN +                            GA2FPGM 
00944           (GC-GCTABULR-AAR-VARY-LEN     *                         GA2FPGM 
00945           GC-GCTABULR-AAR-VARY-MAX-OCUR).                         GA2FPGM 
00946                                                                   GA2FPGM 
00947 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA2FPGM 
00948      EXEC CICS GETMAIN                                            GA2FPGM 
00949         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2FPGM 
00950         INITIMG(WS-HEX-00)                                        GA2FPGM 
00951         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2FPGM 
00952                                                                   GA2FPGM 
00953 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2FPGM 
00954 ***  ADD ALL-LEVEL-TAB-PNTR, 4096 GIVING  ALL-LEVEL-TAB-PNTR2.    GA2FPGM 
00955                                                                   GA2FPGM 
00956      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2FPGM 
00957                                                                   GA2FPGM 
00958 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA2FPGM 
00959 *    EXEC CICS GETMAIN                                            GA2FPGM 
00960 *       SET(ADDRESS OF GCACOMMAREA)                               GA2FPGM 
00961 *       INITIMG(WS-HEX-00)                                        GA2FPGM 
00962 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA2FPGM 
00963                                                                   GA2FPGM 
00964 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2FPGM 
00965                                                                   GA2FPGM 
00966      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2FPGM 
00967         MOVE SPACES               TO  GCIO-WORKFILE-KEY           GA2FPGM 
00968         MOVE  'G'                 TO  GCIO-WRK-STATUS-CODE        GA2FPGM 
00969         MOVE  'G3'                TO  GCIO-WRK-RECORD-TYPE        GA2FPGM 
00970         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA2FPGM 
00971         MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM          GA2FPGM 
00972         MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM        GA2FPGM 
00973         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA2FPGM 
00974         MOVE SPACES  TO  GCA-L-O-B                                GA2FPGM 
00975                          GCA-PROV-CTL                             GA2FPGM 
00976                          GCA-BEN-PROV-ID                          GA2FPGM 
00977                          GCIO-WRK-LINE-OF-BUS                     GA2FPGM 
00978                          GCIO-WRK-PROVIDER-CONTROL                GA2FPGM 
00979         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2FPGM 
00980         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2FPGM 
00981         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2FPGM 
00982                                       GCIO-WRK-PROVISION-ID       GA2FPGM 
00983         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCA-ALL-LEVEL-TAB-SLOT     GA2FPGM 
00984                                        GCIO-WRK-PROVISION-SLOT-NO GA2FPGM 
00985         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2FPGM 
00986         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2FPGM 
00987                                                                   GA2FPGM 
00988      IF  MAP-FROM-MENU-ID  = 'GC4A'  OR  'GTM1'                   GA2FPGM 
00989         MOVE SPACES                TO GCIO-WORKFILE-KEY           GA2FPGM 
00990         MOVE  'C'                  TO GCIO-WRK-STATUS-CODE        GA2FPGM 
00991         MOVE  'C3'                 TO GCIO-WRK-RECORD-TYPE        GA2FPGM 
00992         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2FPGM 
00993         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2FPGM 
00994         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2FPGM 
00995         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2FPGM 
00996         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2FPGM 
00997         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2FPGM 
00998         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2FPGM 
00999         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2FPGM 
01000         MOVE SPACES                TO GCA-BEN-PROV-ID             GA2FPGM 
01001         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2FPGM 
01002                                       GCIO-WRK-PROVISION-ID       GA2FPGM 
01003         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCA-ALL-LEVEL-TAB-SLOT     GA2FPGM 
01004                                        GCIO-WRK-PROVISION-SLOT-NO GA2FPGM 
01005         MOVE SPACES                TO GCIO-WRK-TAB-PROVISION-ID   GA2FPGM 
01006         MOVE ZEROES                TO GCIO-WRK-TAB-PROV-SLOT-NO.  GA2FPGM 
01007                                                                   GA2FPGM 
01008      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2FPGM 
01009         MOVE SPACES                TO GCIO-WORKFILE-KEY           GA2FPGM 
01010         MOVE  'C'                  TO GCIO-WRK-STATUS-CODE        GA2FPGM 
01011         MOVE  'C5'                 TO GCIO-WRK-RECORD-TYPE        GA2FPGM 
01012         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2FPGM 
01013         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2FPGM 
01014         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2FPGM 
01015         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2FPGM 
01016         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2FPGM 
01017         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2FPGM 
01018         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2FPGM 
01019         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2FPGM 
01020         MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID       GA2FPGM 
01021         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2FPGM 
01022                                       GCIO-WRK-TAB-PROVISION-ID   GA2FPGM 
01023         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCA-ALL-LEVEL-TAB-SLOT     GA2FPGM 
01024                                        GCIO-WRK-TAB-PROV-SLOT-NO  GA2FPGM 
01025         MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO. GA2FPGM 
01026                                                                   GA2FPGM 
01027 *    MOVE WS-Y  TO  WS-YY.                                        GA2FPGM 
01028 *    IF  WS-M  >  2                                               GA2FPGM 
01029 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2FPGM 
01030 *          REMAINDER  WS-REMAINDER                                GA2FPGM 
01031 *    ELSE                                                         GA2FPGM 
01032 *       MOVE 1  TO  WS-REMAINDER.                                 GA2FPGM 
01033 *    SET WS-M-IDX  TO  WS-M.                                      GA2FPGM 
01034 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2FPGM 
01035 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2FPGM 
01036 *    IF  WS-REMAINDER  =  ZERO                                    GA2FPGM 
01037 *       ADD 1  TO  WS-DDD.                                        GA2FPGM 
01038                                                                   GA2FPGM 
01039 *    MOVE  WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE                  GA2FPGM 
01040 *                        GCA-EFF-DT.                              GA2FPGM 
01041      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA2FPGM 
01042      MOVE  SPACES  TO  GCA-INTERNAL-TAB-ID,                       GA2FPGM 
01043                        GCA-INTERNAL-TAB-SLOT,                     GA2FPGM 
01044                        GCA-ADD-DEL-IND,                           GA2FPGM 
01045                        GCA-ALL-LEVEL-TAB-FUNC-CODE,               GA2FPGM 
01046                        GCA-OCCURS-ENTRY-COUNTER.                  GA2FPGM 
01047      MOVE  MAP-FROM-MENU-ID  TO GCA-FROM-MENU-ID.                 GA2FPGM 
01048      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2FPGM 
01049 ***  MOVE  ALL-LEVEL-TAB-PNTR TO GCA-RECORD-POINTER.              GA2FPGM 
01050                                                                   GA2FPGM 
01051      SET GCA-RECORD-POINTER TO ADDRESS                            GA2FPGM 
01052      OF  IO-PARM-ALL-LVL-TAB-RECORD.                              GA2FPGM 
01053                                                                   GA2FPGM 
01054      MOVE GC-GCTABULR-AAR-VARY-MAX-OCUR                           GA2FPGM 
01055      TO   GAE-ENTRY-COUNT.                                        GA2FPGM 
01056                                                                   GA2FPGM 
01057      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2FPGM 
01058                                                                   GA2FPGM 
01059      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2FPGM 
01060         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2FPGM 
01061         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2FPGM 
01062                                                                   GA2FPGM 
01063      IF  NOT GCIO-GOOD-RETURN                                     GA2FPGM 
01064         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2FPGM 
01065 -     'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE           GA2FPGM 
01066         MOVE '2FF3'  TO  WS-ABEND-CODE                            GA2FPGM 
01067         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
01068                                                                   GA2FPGM 
01069 *    SET  COMMAREA-PNTR   TO                                      GA2FPGM 
01070 *         ADDRESS  OF GCA-COMMAREA.                               GA2FPGM 
01071                                                                   GA2FPGM 
01072 *    EXEC CICS XCTL  PROGRAM('GA1FPGM') COMMAREA(COMMAREA-PNTR)   GA2FPGM 
01073 *       LENGTH(4)  END-EXEC.                                      GA2FPGM 
01074      EXEC CICS XCTL  PROGRAM('GA1FPGM')                           GA2FPGM 
01075                      COMMAREA(DFHCOMMAREA)                        GA2FPGM 
01076                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2FPGM 
01077      END-EXEC.                                                    GA2FPGM 
01078                                                                   GA2FPGM 
01079  3099-EXIT.   EXIT.                                               GA2FPGM 
01080 /                                                                 GA2FPGM 
01081 ***************************************************************** GA2FPGM 
01082 **          D I S P L A Y   F I R S T   S C R E E N               GA2FPGM 
01083 **                                                                GA2FPGM 
01084 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE DELETE GA2FPGM 
01085 ** PROGRAM, THAT PROGRAM WILL PASS THE ADDRESS OF A PARAMETER LISTGA2FPGM 
01086 ** CONTAINING THE FIELDS FROM THE HEADER OF THE SCREEN.           GA2FPGM 
01087 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2FPGM 
01088 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2FPGM 
01089 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2FPGM 
01090 ******************************************************************GA2FPGM 
01091  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2FPGM 
01092      MOVE '4000'  TO  WS-PARA-ID.                                 GA2FPGM 
01093                                                                   GA2FPGM 
01094 *****  MOVE LOW-VALUES TO SCREEN BEFORE PROCESSING                GA2FPGM 
01095                                                                   GA2FPGM 
01096      MOVE LOW-VALUES TO GA2FI01I.                                 GA2FPGM 
01097                                                                   GA2FPGM 
01098      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2FPGM 
01099         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2FPGM 
01100            TO MAP-ERROR-MESSAGE                                   GA2FPGM 
01101         MOVE '2FC1'  TO  WS-ABEND-CODE                            GA2FPGM 
01102         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
01103                                                                   GA2FPGM 
01104 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA2FPGM 
01105 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2FPGM 
01106                                                                   GA2FPGM 
01107 *    SET ADDRESS OF GCA-COMMAREA                                  GA2FPGM 
01108 *    TO  INCOMING-COMMAREA-PNTR.                                  GA2FPGM 
01109                                                                   GA2FPGM 
01110      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-ALL-LEVEL-TAB-ID.         GA2FPGM 
01111      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-ALL-LEVEL-TAB-SLOT.     GA2FPGM 
01112      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA2FPGM 
01113                                                                   GA2FPGM 
01114      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2FPGM 
01115         MOVE GROUP-SPECIFIC-TITLE-LINE TO MAP-TITLE-LINE          GA2FPGM 
01116         MOVE 'GROUP SPECIFIC ID = '    TO GRP-SPEC-ID-HEADING     GA2FPGM 
01117         MOVE 'GRP= '                   TO GRP-SPEC-GROUP-HEADING  GA2FPGM 
01118         MOVE GCA-GRP-NO                TO GRP-SPEC-GROUP-NO       GA2FPGM 
01119         MOVE ' SEC= '                  TO GRP-SPEC-SECTION-HEADINGGA2FPGM 
01120         MOVE GCA-SECTN-NO              TO GRP-SPEC-SECTION-NO     GA2FPGM 
01121         MOVE ' FR= '                   TO GRP-SPEC-FAM-REL-HEADINGGA2FPGM 
01122         MOVE GCA-FAM-REL-LVL           TO GRP-SPEC-FAM-REL-LVL    GA2FPGM 
01123         MOVE ' EFDT= '                 TO GRP-SPEC-EFF-DT-HEADING GA2FPGM 
01124         MOVE GCA-EFFECTIVE-DATE        TO GRP-SPEC-EFF-DATE.      GA2FPGM 
01125                                                                   GA2FPGM 
01126      IF  GCA-FROM-MENU-ID  = 'GC4A'  OR  'GTM1'                   GA2FPGM 
01127         MOVE CONTRACT-TITLE-LINE TO MAP-TITLE-LINE                GA2FPGM 
01128         MOVE 'CONTRACT ID = '    TO CONTRACT-ID-HEADING           GA2FPGM 
01129         MOVE 'GRP= '             TO CONTRACT-GROUP-HEADING        GA2FPGM 
01130         MOVE GCA-GRP-NO          TO CONTRACT-GROUP-NO             GA2FPGM 
01131         MOVE ' SEC= '            TO CONTRACT-SECTION-HEADING      GA2FPGM 
01132         MOVE GCA-SECTN-NO        TO CONTRACT-SECTION-NO           GA2FPGM 
01133         MOVE ' LOB= '            TO CONTRACT-LOB-HEADING          GA2FPGM 
01134         MOVE GCA-L-O-B           TO CONTRACT-LOB                  GA2FPGM 
01135         MOVE ' PRV= '            TO CONTRACT-PROV-CTL-HEADING     GA2FPGM 
01136         MOVE GCA-PROV-CTL        TO CONTRACT-PROV-CTL             GA2FPGM 
01137         MOVE ' FR= '             TO CONTRACT-FAM-REL-HEADING      GA2FPGM 
01138         MOVE GCA-FAM-REL-LVL     TO CONTRACT-FAM-REL-LVL          GA2FPGM 
01139         MOVE ' EFDT= '           TO CONTRACT-EFF-DT-HEADING       GA2FPGM 
01140         MOVE GCA-EFFECTIVE-DATE  TO CONTRACT-EFF-DATE.            GA2FPGM 
01141                                                                   GA2FPGM 
01142      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2FPGM 
01143         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-TITLE-LINE     GA2FPGM 
01144         MOVE 'GRP= '             TO BEN-PROV-GROUP-HEADING        GA2FPGM 
01145         MOVE GCA-GRP-NO          TO BEN-PROV-GROUP-NO             GA2FPGM 
01146         MOVE ' SEC= '            TO BEN-PROV-SECTION-HEADING      GA2FPGM 
01147         MOVE GCA-SECTN-NO        TO BEN-PROV-SECTION-NO           GA2FPGM 
01148         MOVE ' LOB= '            TO BEN-PROV-LOB-HEADING          GA2FPGM 
01149         MOVE GCA-L-O-B           TO BEN-PROV-LOB                  GA2FPGM 
01150         MOVE ' PRV= '            TO BEN-PROV-PROV-CTL-HEADING     GA2FPGM 
01151         MOVE GCA-PROV-CTL        TO BEN-PROV-PROV-CTL             GA2FPGM 
01152         MOVE ' FR= '             TO BEN-PROV-FAM-REL-HEADING      GA2FPGM 
01153         MOVE GCA-FAM-REL-LVL     TO BEN-PROV-FAM-REL-LVL          GA2FPGM 
01154         MOVE ' EFDT= '           TO BEN-PROV-EFF-DT-HEADING       GA2FPGM 
01155         MOVE GCA-EFFECTIVE-DATE  TO BEN-PROV-EFF-DATE             GA2FPGM 
01156         MOVE ' BPVID= '          TO BEN-PROV-ID-HEADING           GA2FPGM 
01157         MOVE GCA-BEN-PROV-ID     TO BEN-PROV-ID-NO.               GA2FPGM 
01158                                                                   GA2FPGM 
01159      EXEC CICS SEND   MAP('GA2FI01') MAPSET('GA2FSET') ERASE      GA2FPGM 
01160         FROM(GA2FI01O) END-EXEC.                                  GA2FPGM 
01161                                                                   GA2FPGM 
01162  4099-EXIT.   EXIT.                                               GA2FPGM 
01163 /                                                                 GA2FPGM 
01164 ***************************************************************** GA2FPGM 
01165 **        X C T L   T O   P R E V I O U S   M E N U               GA2FPGM 
01166 **                                                                GA2FPGM 
01167 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2FPGM 
01168 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD &       GA2FPGM 
01169 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA2FPGM 
01170 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM       GA2FPGM 
01171 ** MENU ID' FIELD).                                               GA2FPGM 
01172 ******************************************************************GA2FPGM 
01173  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2FPGM 
01174      MOVE '5000'  TO  WS-PARA-ID.                                 GA2FPGM 
01175                                                                   GA2FPGM 
01176      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2FPGM 
01177         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA2FPGM 
01178                                                                   GA2FPGM 
01179      IF  MAP-FROM-MENU-ID  = 'GC4A'                               GA2FPGM 
01180         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA2FPGM 
01181                                                                   GA2FPGM 
01182      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2FPGM 
01183         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA2FPGM 
01184                                                                   GA2FPGM 
01185      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA2FPGM 
01186         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA2FPGM 
01187                                                                   GA2FPGM 
01188                                                                   GA2FPGM 
01189  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA2FPGM 
01190      MOVE '5010'  TO  WS-PARA-ID.                                 GA2FPGM 
01191                                                                   GA2FPGM 
01192      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2FPGM 
01193          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2FPGM 
01194                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2FPGM 
01195                                                                   GA2FPGM 
01196 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA2FPGM 
01197      EXEC CICS GETMAIN                                            GA2FPGM 
01198         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA2FPGM 
01199         INITIMG(WS-HEX-00)                                        GA2FPGM 
01200         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01201 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA2FPGM 
01202                                                                   GA2FPGM 
01203      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2FPGM 
01204                                                                   GA2FPGM 
01205      MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           GA2FPGM 
01206      MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           GA2FPGM 
01207      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2FPGM 
01208      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2FPGM 
01209      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2FPGM 
01210      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2FPGM 
01211      MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS,           GA2FPGM 
01212                                   GCIO-WRK-PROVIDER-CONTROL.      GA2FPGM 
01213      MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2FPGM 
01214      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2FPGM 
01215                                                                   GA2FPGM 
01216      MOVE 'GCPSWORK'        TO GCIO2-FILE-DDNAME.                 GA2FPGM 
01217      MOVE SPACES            TO GCIO-WRK-PROVISION-ID,             GA2FPGM 
01218                                GCIO-WRK-TAB-PROVISION-ID.         GA2FPGM 
01219      MOVE ZEROES            TO GCIO-WRK-PROVISION-SLOT-NO,        GA2FPGM 
01220                                GCIO-WRK-TAB-PROV-SLOT-NO.         GA2FPGM 
01221      MOVE GCIO-WORKFILE-KEY TO GCIO2-FILE-KEY.                    GA2FPGM 
01222                                                                   GA2FPGM 
01223      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA2FPGM 
01224      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA2FPGM 
01225                                                                   GA2FPGM 
01226      MOVE 'RD '             TO GCIO2-FILE-ACCESS-CODE.            GA2FPGM 
01227                                                                   GA2FPGM 
01228      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2FPGM 
01229         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA2FPGM 
01230         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01231                                                                   GA2FPGM 
01232      IF  NOT GCIO2-GOOD-RETURN                                    GA2FPGM 
01233         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2FPGM 
01234 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2FPGM 
01235         MOVE '2FF4'  TO  WS-ABEND-CODE                            GA2FPGM 
01236         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
01237                                                                   GA2FPGM 
01238      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2FPGM 
01239          GC-WORKFILE-KEY-LEN        +                             GA2FPGM 
01240                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2FPGM 
01241                                                                   GA2FPGM 
01242      EXEC CICS XCTL PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)    GA2FPGM 
01243         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01244                                                                   GA2FPGM 
01245      GO TO 5099-EXIT.                                             GA2FPGM 
01246                                                                   GA2FPGM 
01247  5020-XCTL-TO-CONTRACT-MENU.                                      GA2FPGM 
01248      MOVE '5020'  TO  WS-PARA-ID.                                 GA2FPGM 
01249                                                                   GA2FPGM 
01250      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2FPGM 
01251          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2FPGM 
01252                 GC-GCCONTR-MAX-REC-LEN.                           GA2FPGM 
01253                                                                   GA2FPGM 
01254 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA2FPGM 
01255      EXEC CICS GETMAIN                                            GA2FPGM 
01256         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA2FPGM 
01257         INITIMG(WS-HEX-00)                                        GA2FPGM 
01258         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01259 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA2FPGM 
01260 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA2FPGM 
01261                                                                   GA2FPGM 
01262      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2FPGM 
01263                                                                   GA2FPGM 
01264      MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           GA2FPGM 
01265      MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           GA2FPGM 
01266      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2FPGM 
01267      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2FPGM 
01268      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2FPGM 
01269      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2FPGM 
01270      MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           GA2FPGM 
01271      MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      GA2FPGM 
01272      MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2FPGM 
01273      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2FPGM 
01274                                                                   GA2FPGM 
01275      MOVE 'GCPSWORK'        TO GCIO3-FILE-DDNAME.                 GA2FPGM 
01276      MOVE SPACES            TO GCIO-WRK-PROVISION-ID,             GA2FPGM 
01277                                GCIO-WRK-TAB-PROVISION-ID.         GA2FPGM 
01278      MOVE ZEROES            TO GCIO-WRK-PROVISION-SLOT-NO,        GA2FPGM 
01279                                GCIO-WRK-TAB-PROV-SLOT-NO.         GA2FPGM 
01280      MOVE GCIO-WORKFILE-KEY TO GCIO3-FILE-KEY.                    GA2FPGM 
01281                                                                   GA2FPGM 
01282      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA2FPGM 
01283      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA2FPGM 
01284                                                                   GA2FPGM 
01285      MOVE 'RD '             TO  GCIO3-FILE-ACCESS-CODE.           GA2FPGM 
01286                                                                   GA2FPGM 
01287      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2FPGM 
01288         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA2FPGM 
01289         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01290                                                                   GA2FPGM 
01291      IF  NOT GCIO3-GOOD-RETURN                                    GA2FPGM 
01292         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2FPGM 
01293 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2FPGM 
01294         MOVE '2FF5'  TO  WS-ABEND-CODE                            GA2FPGM 
01295         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
01296                                                                   GA2FPGM 
01297      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2FPGM 
01298          GC-WORKFILE-KEY-LEN        +                             GA2FPGM 
01299                 GC-GCCONTR-MAX-REC-LEN.                           GA2FPGM 
01300                                                                   GA2FPGM 
01301      EXEC CICS XCTL PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)    GA2FPGM 
01302         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01303                                                                   GA2FPGM 
01304      GO TO 5099-EXIT.                                             GA2FPGM 
01305                                                                   GA2FPGM 
01306  5030-XCTL-TO-BEN-PROV-MENU.                                      GA2FPGM 
01307      MOVE '5030'  TO  WS-PARA-ID.                                 GA2FPGM 
01308                                                                   GA2FPGM 
01309      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2FPGM 
01310          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2FPGM 
01311                 GC-GCBENPRV-MAX-REC-LEN.                          GA2FPGM 
01312                                                                   GA2FPGM 
01313 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA2FPGM 
01314      EXEC CICS GETMAIN                                            GA2FPGM 
01315         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA2FPGM 
01316         INITIMG(WS-HEX-00)                                        GA2FPGM 
01317         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01318 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA2FPGM 
01319                                                                   GA2FPGM 
01320      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2FPGM 
01321                                                                   GA2FPGM 
01322      MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           GA2FPGM 
01323      MOVE 'C4'                 TO GCIO-WRK-RECORD-TYPE.           GA2FPGM 
01324      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2FPGM 
01325      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2FPGM 
01326      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2FPGM 
01327      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2FPGM 
01328      MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           GA2FPGM 
01329      MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      GA2FPGM 
01330      MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2FPGM 
01331      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2FPGM 
01332      MOVE GCA-BEN-PROV-ID      TO GCIO-WRK-PROVISION-ID.          GA2FPGM 
01333                                                                   GA2FPGM 
01334      MOVE 'GCPSWORK'        TO GCIO4-FILE-DDNAME.                 GA2FPGM 
01335      MOVE 9999999           TO GCIO-WRK-PROVISION-SLOT-NO.        GA2FPGM 
01336      MOVE SPACES            TO GCIO-WRK-TAB-PROVISION-ID.         GA2FPGM 
01337      MOVE ZEROES            TO GCIO-WRK-TAB-PROV-SLOT-NO.         GA2FPGM 
01338      MOVE GCIO-WORKFILE-KEY TO GCIO4-FILE-KEY.                    GA2FPGM 
01339                                                                   GA2FPGM 
01340      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA2FPGM 
01341      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA2FPGM 
01342                                                                   GA2FPGM 
01343      MOVE 'RD '             TO GCIO4-FILE-ACCESS-CODE.            GA2FPGM 
01344                                                                   GA2FPGM 
01345      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2FPGM 
01346         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA2FPGM 
01347         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01348                                                                   GA2FPGM 
01349      IF  NOT GCIO4-GOOD-RETURN                                    GA2FPGM 
01350         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2FPGM 
01351 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2FPGM 
01352         MOVE '2FF6'  TO  WS-ABEND-CODE                            GA2FPGM 
01353         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2FPGM 
01354                                                                   GA2FPGM 
01355      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2FPGM 
01356          GC-WORKFILE-KEY-LEN        +                             GA2FPGM 
01357                 GC-GCBENPRV-MAX-REC-LEN.                          GA2FPGM 
01358                                                                   GA2FPGM 
01359      EXEC CICS XCTL PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)    GA2FPGM 
01360         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2FPGM 
01361                                                                   GA2FPGM 
01362      GO TO 5099-EXIT.                                             GA2FPGM 
01363                                                                   GA2FPGM 
01364                                                                   GA2FPGM 
01365  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA2FPGM 
01366      MOVE '5040'  TO  WS-PARA-ID.                                 GA2FPGM 
01367                                                                   GA2FPGM 
01368      EXEC CICS XCTL                                               GA2FPGM 
01369                PROGRAM('GTM1PGM')                                 GA2FPGM 
01370                END-EXEC.                                          GA2FPGM 
01371                                                                   GA2FPGM 
01372      GO  TO  5099-EXIT.                                           GA2FPGM 
01373                                                                   GA2FPGM 
01374                                                                   GA2FPGM 
01375  5099-EXIT.                                                       GA2FPGM 
01376      EXIT.                                                        GA2FPGM 
01377 /                                                                 GA2FPGM 
01378 ***************************************************************** GA2FPGM 
01379 **           X C T L   T O   M A I N   M E N U                    GA2FPGM 
01380 **                                                                GA2FPGM 
01381 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2FPGM 
01382 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2FPGM 
01383 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2FPGM 
01384 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2FPGM 
01385 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2FPGM 
01386 ** AND PROGRESS DOWN.                                             GA2FPGM 
01387 ******************************************************************GA2FPGM 
01388  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2FPGM 
01389      MOVE '6000'  TO  WS-PARA-ID.                                 GA2FPGM 
01390      MOVE '2FP1'  TO  WS-ABEND-CODE.                              GA2FPGM 
01391                                                                   GA2FPGM 
01392      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2FPGM 
01393                                                                   GA2FPGM 
01394  6099-EXIT.     EXIT.                                             GA2FPGM 
01395 /                                                                 GA2FPGM 
01396 /    E R R O R   M E S S A G E   T H E N   A B E N D              GA2FPGM 
01397 ******************************************************************GA2FPGM 
01398  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2FPGM 
01399                                                                   GA2FPGM 
01400      SET MAP-IDX1  TO  7.                                         GA2FPGM 
01401      SET MAP-IDX2  TO  1.                                         GA2FPGM 
01402      MOVE -1  TO  MAP-BENEFIT-CODE-LEN (MAP-IDX1, MAP-IDX2).      GA2FPGM 
01403      EXEC CICS SEND   MAP('GA2FI01') MAPSET('GA2FSET') ERASE      GA2FPGM 
01404         FROM(GA2FI01O) CURSOR WAIT END-EXEC.                      GA2FPGM 
01405                                                                   GA2FPGM 
01406      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2FPGM 
01407                                                                   GA2FPGM 
01408  9999-EXIT.     EXIT.                                             GA2FPGM 
