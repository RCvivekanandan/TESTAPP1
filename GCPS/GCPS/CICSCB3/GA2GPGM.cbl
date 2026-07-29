00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GA2GPGM.                                         GA2GPGM 
00003 **** THIS IS A COBOL/2 PROGRAM *****                                 LV001
00004  AUTHOR.         S BUCH.                                          GA2GPGM 
00005  DATE-WRITTEN.   11/08/84.                                        GA2GPGM 
00006  DATE-COMPILED.                                                   GA2GPGM 
00007      SKIP3                                                        GA2GPGM 
00008 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2GPGM 
00009 *****  P R O G R A M   M O D I F I C A T I O N    S T A T U S ****GA2GPGM 
00010 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2GPGM 
00011 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2GPGM 
00012 *                                                                 GA2GPGM 
00013 *   XXXX    06/16/85    AHL   PARAGRAPH 2010-VALIDATE-ADD-ENTRIES GA2GPGM 
00014 *                             IS MODIFIED.                        GA2GPGM 
00015 *                                                                 GA2GPGM 
00016 *   T529    03-19-86    MDD   ADDED CODE TO CHECK RETURN CODE FROMGA2GPGM 
00017 *                             GCVIOPGM FOR A VALUE OF '20', THIS  GA2GPGM 
00018 *                             MEANS THE EDIT TABLE IS EMPTY AND A GA2GPGM 
00019 *                             VALIDATION COULD NOT BE PERFORMED.  GA2GPGM 
00020 *                             PF4/16 CAN BE USED TO ACCEPT THE    GA2GPGM 
00021 *                             DATA AS SHOWN ON THE SCREEN AND TO  GA2GPGM 
00022 *                             CONTINUE PROCESSING.                GA2GPGM 
00023 *                                                                 GA2GPGM 
00024 *  EL500    06/19/86    DES   FIXED PF4/16 CODE TO ACCEPT EMPTY   GA2GPGM 
00025 *                             VALIDATION TABLE CONDITION ONLY,    GA2GPGM 
00026 *                             ALL OTHER ERRORS STILL MUST BE FIXEDGA2GPGM 
00027 *                                                                 GA2GPGM 
00028 *  D0120    03/17/87    JLA   CHANGES FOR SINGLE TABULAR SUPPORT  GA2GPGM 
00029 *                             THAT EXECUTES FROM TRANSACTION GTM1:GA2GPGM 
00030 *                             1. PF1/PF13 - CONSTRUCT COMMAREA AS GA2GPGM 
00031 *                                IF GC4A HAD CALLED, XCTL TO      GA2GPGM 
00032 *                                DELETE SCREEN PROGRAM.           GA2GPGM 
00033 *                             2. PF3/PF15 - XCTL TO GTM1PGM       GA2GPGM 
00034 *                                WITHOUT PASSING ANY COMMAREA.    GA2GPGM 
00035 *                                                               * GA2GPGM 
00036 *  D116      8/17/87    FRY   CAPTURE OPERATOR-ID WHEN A 'C3',  * GA2GPGM 
00037 *                             'C6', OR 'G4' RECORD IS UPDATED.  * GA2GPGM 
00038 *                                                               * GA2GPGM 
00039 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GA2GPGM 
00040 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GA2GPGM 
00041 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GA2GPGM 
00042 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GA2GPGM 
00043 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GA2GPGM 
00044 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GA2GPGM 
00045 *                       6. REMOVE HARDCOPY ROUTINE.              *GA2GPGM 
00046 *                       7. >>> CONVERT TO COBOL/II <<<<          *GA2GPGM 
00047 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  *GA2GPGM 
00048 *                                                                *GA2GPGM 
00049 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD      *GA2GPGM 
00050 *                           FROM ONE POSITION TO TWO POSITIONS.  *GA2GPGM 
00051 *                                                                *GA2GPGM 
00052 *14726/ 10/28/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000 AND  *GA2GPGM 
00053 *15057                  THE EXPANSION OF THE GROUP SPECIFIC AND  *GA2GPGM 
00054 *                       CONTRACT KEY TO SUPPORT THE TEXAS MERGER.*GA2GPGM 
00055 *                                                                *GA2GPGM 
00056 * 14726/  04/11/98  AB   EXPANDED THE SCREEN / MAP               *GA2GPGM 
00057 * 15057                  TO INCLUDE THE ENTIRE KEY               *GA2GPGM 
00058 *                                                                *GA2GPGM 
00059 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP       *GA2GPGM 
00060 *                        2. ADD DELADD-OPTION = 'GAS5UPD'        *GA2GPGM 
00061 *                                                                *GA2GPGM 
00062 *  15380    05/03/99    GDM   MODIFY TO INCLIDE MULT06 FIELD     *GA2GPGM 
00063 *                             VALIDATION OPTION.                 *GA2GPGM 
00064 *                                                                *GA2GPGM 
00065 * P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *GA2GPGM 
00066 *                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *GA2GPGM 
00067 *                                                                *GA2GPGM 
00068 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA2GPGM 
SI0724*                                                                *00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00009180
SI0724*                           GCCDRLEN                             *00009190
00069 *                                                                *GA2GPGM 
00070 *                                                                *GA2GPGM 
00071 *                                                                *GA2GPGM 
00072 ******************************************************************GA2GPGM 
00073 /                                                                 GA2GPGM 
00074 ******************************************************************GA2GPGM 
00075 *   GA2GPGM      ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM   GA2GPGM 
00076 *                  BENEFIT GROUP BY BENEFIT-PROVISION ID - GA2G   GA2GPGM 
00077 *                                                                 GA2GPGM 
00078 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2GPGM 
00079 *   TABULAR PROVISION ID ARGUMENTS.                               GA2GPGM 
00080 *                                                                 GA2GPGM 
00081 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2GPGM 
00082 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2GPGM 
00083 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2GPGM 
00084 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2GPGM 
00085 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2GPGM 
00086 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2GPGM 
00087 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2GPGM 
00088 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2GPGM 
00089 *                                                                 GA2GPGM 
00090 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IBGR)GA2GPGM 
00091 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2GPGM 
00092 *   XCTL TO TRANS GA1G OR PROGRAM GA1GPGM.  THIS PROGRAM WILL     GA2GPGM 
00093 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2GPGM 
00094 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2GPGM 
00095 *   CODE.                                                         GA2GPGM 
00096 *                                                                 GA2GPGM 
00097 *   FUNC CODE: GA2G                                               GA2GPGM 
00098 *   MAPSET:    GA2GSETC                                           GA2GPGM 
00099 *   FILES:     GCPSWORK                                           GA2GPGM 
00100 ******************************************************************GA2GPGM 
00101      SKIP3                                                        GA2GPGM 
00102  ENVIRONMENT DIVISION.                                            GA2GPGM 
00103 /                                                                 GA2GPGM 
00104  DATA DIVISION.                                                   GA2GPGM 
00105  WORKING-STORAGE SECTION.                                         GA2GPGM 
00106  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2GPGM 
00107      '***GA2GPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2GPGM 
00108  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2GPGM 
00109                                                                   GA2GPGM 
00110  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2GPGM 
00111                                                                   GA2GPGM 
00112  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2GPGM 
00113                                                                   GA2GPGM 
00114  01  COMMAREA-POINTER-AREA.                                       GA2GPGM 
00115      05  COMMAREA-PNTR-COMP      PIC S9(8)  COMP.                 GA2GPGM 
00116      05  COMMAREA-PNTR  REDEFINES                                 GA2GPGM 
00117          COMMAREA-PNTR-COMP      USAGE IS POINTER.                GA2GPGM 
00118                                                                   GA2GPGM 
00119 ** MAP COBOL SCREEN DSECTS **                                     GA2GPGM 
00120  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2GPGM 
00121      '***  I/O MAPAREA ***'.                                      GA2GPGM 
00122  COPY GA2GSETC.                                                   GA2GPGM 
00123 /                                                                 GA2GPGM 
00124 ******************************************************************GA2GPGM 
00125 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2GPGM 
00126 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2GPGM 
00127 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2GPGM 
00128 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2GPGM 
00129 **  REDEFINES.                                                    GA2GPGM 
00130 ****************************************************************  GA2GPGM 
00131      SKIP3                                                        GA2GPGM 
00132  01  FILLER     REDEFINES   GA2GI01I.                             GA2GPGM 
00133      05  FILLER                              PIC X(89).           GA2GPGM 
00134      05  GROUP-SPECIFIC-ID-LINE.                                  GA2GPGM 
00135          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA2GPGM 
00136          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA2GPGM 
00137          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA2GPGM 
00138          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA2GPGM 
00139          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA2GPGM 
00140          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA2GPGM 
00141          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA2GPGM 
00142          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA2GPGM 
00143          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA2GPGM 
00144          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA2GPGM 
00145          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA2GPGM 
00146          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA2GPGM 
00147          10  FILLER                          PIC X(16).           GA2GPGM 
00148      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA2GPGM 
00149          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2GPGM 
00150          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2GPGM 
00151          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2GPGM 
00152          10  CONTRACT-GROUP-NO               PIC X(9).            GA2GPGM 
00153          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2GPGM 
00154          10  CONTRACT-SECTION-NO             PIC X(5).            GA2GPGM 
00155          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2GPGM 
00156          10  CONTRACT-PKG-CODE               PIC X(3).            GA2GPGM 
00157          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2GPGM 
00158          10  CONTRACT-LOB                    PIC X.               GA2GPGM 
00159          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2GPGM 
00160          10  CONTRACT-PROV-CTL               PIC XX.              GA2GPGM 
00161          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2GPGM 
00162          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2GPGM 
00163          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2GPGM 
00164          10  CONTRACT-EFF-DATE               PIC X(6).            GA2GPGM 
00165          10  FILLER                          PIC X(1).            GA2GPGM 
00166      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA2GPGM 
00167                                     GROUP-SPECIFIC-ID-LINE.       GA2GPGM 
00168          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA2GPGM 
00169          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA2GPGM 
00170          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA2GPGM 
00171          10  BEN-PROV-GROUP-NO               PIC X(9).            GA2GPGM 
00172          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA2GPGM 
00173          10  BEN-PROV-SECTION-NO             PIC X(5).            GA2GPGM 
00174          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA2GPGM 
00175          10  BEN-PROV-PKG-CODE               PIC X(3).            GA2GPGM 
00176          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA2GPGM 
00177          10  BEN-PROV-LOB                    PIC X.               GA2GPGM 
00178          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA2GPGM 
00179          10  BEN-PROV-PROV-CTL               PIC XX.              GA2GPGM 
00180          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA2GPGM 
00181          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA2GPGM 
00182          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA2GPGM 
00183          10  BEN-PROV-EFF-DATE               PIC X(6).            GA2GPGM 
00184          10  BEN-PROV-ID-HEADING             PIC X(6).            GA2GPGM 
00185          10  BEN-PROV-ID-NO                  PIC X(6).            GA2GPGM 
00186          10  FILLER                          PIC X(4).            GA2GPGM 
00187      05  FILLER                              PIC X(78).           GA2GPGM 
00188      05  MAP-PROVISION-ID-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED   GA2GPGM 
00189          BY MAP-IDX1.                                             GA2GPGM 
00190        10  MAP-PROVISION-ID-ARGUMENT-COL  OCCURS 3 TIMES INDEXED  GA2GPGM 
00191            BY MAP-IDX2.                                           GA2GPGM 
00192          15  MAP-PROVISION-ID-ARGUMENT-LEN   PIC S9(4) COMP SYNC. GA2GPGM 
00193          15  MAP-PROVISION-ID-ARGUMENT-ATTR  PIC X.               GA2GPGM 
00194          15  MAP-PROVISION-ID-ARGUMENT       PIC X(6).            GA2GPGM 
00195      SKIP3                                                        GA2GPGM 
00196  01  FILLER.                                                      GA2GPGM 
00197 ****************************************************************  GA2GPGM 
00198 **   FIELDS DESCRIBING NUMBER OF OCCURS FOR MAP.                  GA2GPGM 
00199 ****************************************************************  GA2GPGM 
00200      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +14.  GA2GPGM 
00201      05  WS-MAP-COL                  PIC S999 COMP-3  VALUE +3.   GA2GPGM 
00202 /                                                                 GA2GPGM 
00203 ** ALTERNATIVE WORKFILE KEYS **                                   GA2GPGM 
00204  01  FILLER                      PIC X(32)  VALUE                 GA2GPGM 
00205      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2GPGM 
00206  01  WS-ALT-WORKFILE-KEYS.                                        GA2GPGM 
00207  COPY GCWRKKEY.                                                   GA2GPGM 
00208 /                                                                 GA2GPGM 
00209                                                                   GA2GPGM 
00210 ** DATE FORMATTING AREA **                                        GA2GPGM 
00211  01  HGADATES-COMMAREA.                                           GA2GPGM 
00212  COPY HGCDAT01.                                                   GA2GPGM 
00213                                                                   GA2GPGM 
00214                                                                   GA2GPGM 
00215 ** WORKFIELDS **                                                  GA2GPGM 
00216  01  FILLER                           PIC X(16)                   GA2GPGM 
00217              VALUE '** WORKFIELDS **'.                            GA2GPGM 
00218  01  WS-WORK-FIELDS.                                              GA2GPGM 
00219      05  WS-HEX-00                    PIC X.                      GA2GPGM 
00220      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2GPGM 
00221      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2GPGM 
00222        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2GPGM 
00223      05  WS-SAVED-FIELDS.                                         GA2GPGM 
00224        10  WS-SAVED-BENEFIT                PIC X(6).              GA2GPGM 
00225      05  WS-PROV-ID-ARGUMENT-ENTRY  OCCURS 43 TIMES INDEXED BY    GA2GPGM 
00226          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2GPGM 
00227        10  WS-PROVISION-ID-ARGUMENT       PIC X(6).               GA2GPGM 
00228 /                                                                 GA2GPGM 
00229 *** SWITCHES ***                                                  GA2GPGM 
00230  01  FILLER                           PIC X(14)                   GA2GPGM 
00231              VALUE '** SWITCHES **'.                              GA2GPGM 
00232  01  WS-SWITCHES.                                                 GA2GPGM 
00233      05  WS-ERROR-SW                  PIC X.                      GA2GPGM 
00234                                                                   GA2GPGM 
00235 ** TITLE LINES **                                                 GA2GPGM 
00236  01  WS-TITLE-LINES.                                              GA2GPGM 
00237      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA2GPGM 
00238          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA2GPGM 
00239      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA2GPGM 
00240          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA2GPGM 
00241      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA2GPGM 
00242          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA2GPGM 
00243                                                                   GA2GPGM 
00244 *** RECORD LENGTHS ***                                            GA2GPGM 
00245  01  FILLER                           PIC X(20)                   GA2GPGM 
00246              VALUE '** RECORD LENGTHS **'.                        GA2GPGM 
00247  01  WS-RECORD-LENGTHS.                                           GA2GPGM 
00248     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA2GPGM 
00249     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA2GPGM 
00250     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2GPGM 
00251     05 WS-GCVI-COMMAREA-LEN           PIC S9(4) COMP   VALUE +19. GA2GPGM 
00252     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2GPGM 
00253                                                                   GA2GPGM 
00254 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA2GPGM 
00255  01  FILLER.                                                      GA2GPGM 
00256      COPY GCCDRLEN.                                               GA2GPGM 
00257                                                                   GA2GPGM 
00258 /                                                                 GA2GPGM 
00259 ** ATTRIBUTES **                                                  GA2GPGM 
00260  COPY DFHBMSCA.                                                   GA2GPGM 
00261      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2GPGM 
00262 /                                                                 GA2GPGM 
00263 ** ATTENTION IDENTIFIERS **                                       GA2GPGM 
00264  COPY DFHAID.                                                     GA2GPGM 
00265 /                                                                 GA2GPGM 
00266  01  GCVIOPGMS-PARM.                                              GA2GPGM 
00267  COPY GCVINTR2.                                                   GA2GPGM 
00268      SKIP3                                                        GA2GPGM 
00269  01  WS-END                          PIC X(16)  VALUE             GA2GPGM 
00270      '*** W/S ENDS ***'.                                          GA2GPGM 
00271 /                                                                 GA2GPGM 
00272  LINKAGE SECTION.                                                 GA2GPGM 
00273  01  DFHCOMMAREA.                                                 GA2GPGM 
00274  COPY G2ALCKEC.                                                   GA2GPGM 
00275  COPY GACDACWA.                                                   GA2GPGM 
00276 *    05  INCOMING-COMMAREA-PNTR   USAGE IS POINTER.               GA2GPGM 
00277      05  GAS1UPD-PASSED-AREA.                                     GA2GPGM 
00278          07  LVL2-B-SW           PIC X.                           GA2GPGM 
00279          07  LVL2-F-SW           PIC X.                           GA2GPGM 
00280          07  LVL2-G-SW           PIC X.                           GA2GPGM 
00281          07  INTR-TAB-PGM-ID     PIC X(8).                        GA2GPGM 
00282          07  FILLER              PIC X(9).                        GA2GPGM 
00283      05  DELADD-OPTION           PIC X(7).                        GA2GPGM 
00284                                                                   GA2GPGM 
00285                                                                   GA2GPGM 
00286 *01  GCA-COMMAREA.                                                GA2GPGM 
00287 *COPY G2ALCKEC.                                                   GA2GPGM 
00288 /                                                                 GA2GPGM 
00289 ** I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD **         GA2GPGM 
00290  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2GPGM 
00291  COPY GCIOPRM1.                                                   GA2GPGM 
00292 /                                                                 GA2GPGM 
00293  COPY GCWRKDCC.                                                   GA2GPGM 
00294 /                                                                 GA2GPGM 
00295  COPY GCTIBGRC.                                                   GA2GPGM 
00296 /                                                                 GA2GPGM 
00297 ****************************************************************  GA2GPGM 
00298 ** COPY OF THE TABULAR PORTION OF THE RECORD, THIS AREA USED IN   GA2GPGM 
00299 ** SORTING PROCESS.                                               GA2GPGM 
00300 ****************************************************************  GA2GPGM 
00301  01  COPY-TABULAR-TABLE-AREA.                                     GA2GPGM 
00302      05  COPY-TABULAR-TABLE  OCCURS 659 TIMES INDEXED BY          GA2GPGM 
00303            COPY-IDX.                                              GA2GPGM 
00304        10  COPY-PROVISION-ID-ARGUMENT   PIC X(6).                 GA2GPGM 
00305 /                                                                 GA2GPGM 
00306 ** IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD **             GA2GPGM 
00307  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2GPGM 
00308  COPY GCIOPRM2.                                                   GA2GPGM 
00309 /                                                                 GA2GPGM 
00310  COPY GCWRKDC2.                                                   GA2GPGM 
00311 /                                                                 GA2GPGM 
00312  COPY GCTABMC.                                                    GA2GPGM 
00313 /                                                                 GA2GPGM 
00314                                                                   GA2GPGM 
00315  PROCEDURE DIVISION.                                              GA2GPGM 
00316                                                                   GA2GPGM 
00317 ******************************************************************GA2GPGM 
00318 **                H O U S E K E E P I N G                         GA2GPGM 
00319 **                                                                GA2GPGM 
00320 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2GPGM 
00321 **                                                                GA2GPGM 
00322 ******************************************************************GA2GPGM 
00323  0000-HOUSEKEEPING    SECTION.                                    GA2GPGM 
00324                                                                   GA2GPGM 
00325      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2GPGM 
00326                                                                   GA2GPGM 
00327      IF EIBAID  =  DFHCLEAR                                       GA2GPGM 
00328          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2GPGM 
00329                         ERASE                                     GA2GPGM 
00330          END-EXEC                                                 GA2GPGM 
00331          EXEC CICS RETURN                                         GA2GPGM 
00332          END-EXEC.                                                GA2GPGM 
00333                                                                   GA2GPGM 
00334      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2GPGM 
00335                END-EXEC.                                          GA2GPGM 
00336  0000-EXIT.                                                       GA2GPGM 
00337         EXIT.                                                     GA2GPGM 
00338 ******************************************************************GA2GPGM 
00339 **                     M A I N L I N E                            GA2GPGM 
00340 **                                                                GA2GPGM 
00341 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2GPGM 
00342 **  TAKEN BY THE OPERATOR.                                        GA2GPGM 
00343 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2GPGM 
00344 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2GPGM 
00345 **     ADDITIONS FROM.                                            GA2GPGM 
00346 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2GPGM 
00347 **     KEY PF12 OR PF24.                                          GA2GPGM 
00348 **  3. RECEIVE THE SCREEN.                                        GA2GPGM 
00349 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2GPGM 
00350 **     MENU.                                                      GA2GPGM 
00351 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2GPGM 
00352 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2GPGM 
00353 **     (RETURN) TO THE DELETE PROGRAM (GA1GPGM).                  GA2GPGM 
00354 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2GPGM 
00355 **     (RETURN) TO THE PREVIOUS MENU.                             GA2GPGM 
00356 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2GPGM 
00357 **     NORMAL ADD PROCESSING, EXCEPT BYPASS EMPTY VALIDATION TABLEGA2GPGM 
00358 **     CONDITION FOR THE PROVISION ID ARGUMENT.                   GA2GPGM 
00359 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2GPGM 
00360 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2GPGM 
00361 **                                                                GA2GPGM 
00362 ******************************************************************GA2GPGM 
00363  1000-MAIN-LINE   SECTION.                                        GA2GPGM 
00364                                                                   GA2GPGM 
00365      MOVE '1000'  TO  WS-PARA-ID.                                 GA2GPGM 
00366                                                                   GA2GPGM 
00367      IF EIBTRNID  NOT =  'GA2G'                                   GA2GPGM 
00368          PERFORM 4000-DISPLAY-FIRST-SCREEN                        GA2GPGM 
00369          GO TO 1099-RETURN.                                       GA2GPGM 
00370                                                                   GA2GPGM 
00371      EXEC CICS RECEIVE   MAP('GA2GI01') MAPSET('GA2GSET')         GA2GPGM 
00372         INTO(GA2GI01I) END-EXEC.                                  GA2GPGM 
00373                                                                   GA2GPGM 
00374      IF SCRNIDNI  NOT =  '002G00'                                 GA2GPGM 
00375         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2GPGM 
00376                                                                   GA2GPGM 
00377      IF EIBAID  =  DFHENTER                                       GA2GPGM 
00378         PERFORM 2000-ADD-PROCESSING                               GA2GPGM 
00379         GO TO 1099-RETURN.                                        GA2GPGM 
00380                                                                   GA2GPGM 
00381      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2GPGM 
00382         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2GPGM 
00383                                                                   GA2GPGM 
00384      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2GPGM 
00385         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2GPGM 
00386                                                                   GA2GPGM 
00387      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2GPGM 
00388         PERFORM 2000-ADD-PROCESSING                               GA2GPGM 
00389         GO TO 1099-RETURN.                                        GA2GPGM 
00390                                                                   GA2GPGM 
00391      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2GPGM 
00392      MOVE -1  TO                                                  GA2GPGM 
00393         MAP-PROVISION-ID-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).       GA2GPGM 
00394      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2GPGM 
00395 -    ' THIS PROGRAM ***'  TO  ERRMSGO.                            GA2GPGM 
00396      EXEC CICS SEND   MAP('GA2GI01') MAPSET('GA2GSET') DATAONLY   GA2GPGM 
00397         FROM(GA2GI01O) CURSOR END-EXEC.                           GA2GPGM 
00398                                                                   GA2GPGM 
00399  1099-RETURN.                                                     GA2GPGM 
00400      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2GPGM 
00401         (DELADD-OPTION = 'GAS1UPD') OR                            GA2GPGM 
00402         (DELADD-OPTION = 'GAS2UPD') OR                            GA2GPGM 
00403         (DELADD-OPTION = 'GAS3UPD') OR                            GA2GPGM 
00404         (DELADD-OPTION = 'GAS4UPD') OR                            GA2GPGM 
00405         (DELADD-OPTION = 'GAS5UPD')                               GA2GPGM 
00406          EXEC CICS RETURN END-EXEC                                GA2GPGM 
00407      ELSE                                                         GA2GPGM 
00408          EXEC CICS RETURN TRANSID('GA2G')                         GA2GPGM 
00409                    COMMAREA(DFHCOMMAREA)                          GA2GPGM 
00410                    LENGTH  (EIBCALEN)                             GA2GPGM 
00411                    END-EXEC.                                      GA2GPGM 
00412                                                                   GA2GPGM 
00413      GOBACK.                                                      GA2GPGM 
00414  1099-EXIT.                                                       GA2GPGM 
00415        EXIT.                                                      GA2GPGM 
00416 /*****************************************************************GA2GPGM 
00417 **               A D D   P R O C E S S I N G                      GA2GPGM 
00418 **                                                                GA2GPGM 
00419 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2GPGM 
00420 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2GPGM.            GA2GPGM 
00421 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2GPGM 
00422 **  2. DETERMINE IF ANY VALUE WERE ENTERED FOR THIS LINE.   IF NOTGA2GPGM 
00423 **     SKIP TO THE NEXT LINE.                                     GA2GPGM 
00424 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2GPGM 
00425 **     WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2GPGM 
00426 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2GPGM 
00427 **     DATA.                                                      GA2GPGM 
00428 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2GPGM 
00429 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2GPGM 
00430 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2GPGM 
00431 **     ORDER, FIELD BY FIELD.                                     GA2GPGM 
00432 **  6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2GPGM 
00433 **     MADE.                                                      GA2GPGM 
00434 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2GPGM 
00435 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2GPGM 
00436 **     BACK INTO THE RECORD.                                      GA2GPGM 
00437 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2GPGM 
00438 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2GPGM 
00439 **                                                                GA2GPGM 
00440 ******************************************************************GA2GPGM 
00441  2000-ADD-PROCESSING SECTION.                                     GA2GPGM 
00442                                                                   GA2GPGM 
00443      MOVE '2000'  TO  WS-PARA-ID.                                 GA2GPGM 
00444      MOVE 'N'     TO  WS-ERROR-SW.                                GA2GPGM 
00445      MOVE 'Y'     TO  GCVI2-TABLE-SW.                             GA2GPGM 
00446      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2GPGM 
00447      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2GPGM 
00448                                                                   GA2GPGM 
00449      MOVE '2005'  TO  WS-PARA-ID.                                 GA2GPGM 
00450  2005-RESET-ALL-ATTRIBUTES.                                       GA2GPGM 
00451      MOVE DFHBMUNF  TO                                            GA2GPGM 
00452         MAP-PROVISION-ID-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2).      GA2GPGM 
00453      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2GPGM 
00454         SET MAP-IDX1  UP BY  1                                    GA2GPGM 
00455         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2GPGM 
00456      IF MAP-IDX2  <  WS-MAP-COL                                   GA2GPGM 
00457         SET MAP-IDX1  TO  1                                       GA2GPGM 
00458         SET MAP-IDX2  UP BY  1                                    GA2GPGM 
00459         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2GPGM 
00460                                                                   GA2GPGM 
00461      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2GPGM 
00462      MOVE '2010'  TO  WS-PARA-ID.                                 GA2GPGM 
00463  2010-VALIDATE-ADD-ENTRIES.                                       GA2GPGM 
00464      IF MAP-PROVISION-ID-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)        GA2GPGM 
00465            =  ZERO                                                GA2GPGM 
00466         IF MAP-IDX1  <  WS-MAP-ROW                                GA2GPGM 
00467            SET MAP-IDX1  UP BY  1                                 GA2GPGM 
00468            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2GPGM 
00469         ELSE                                                      GA2GPGM 
00470            IF MAP-IDX2  <  WS-MAP-COL                             GA2GPGM 
00471               SET MAP-IDX1  TO  1                                 GA2GPGM 
00472               SET MAP-IDX2  UP BY  1                              GA2GPGM 
00473               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2GPGM 
00474            ELSE                                                   GA2GPGM 
00475               GO TO 2020-CHECK-FOR-ERRORS.                        GA2GPGM 
00476                                                                   GA2GPGM 
00477      IF MAP-PROVISION-ID-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2) = ZERO GA2GPGM 
00478         MOVE DFHBMUBF  TO                                         GA2GPGM 
00479            MAP-PROVISION-ID-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA2GPGM 
00480         MOVE '??????' TO                                          GA2GPGM 
00481            MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)         GA2GPGM 
00482         IF WS-ERROR-SW  NOT =  'Y'                                GA2GPGM 
00483            MOVE 'Y'  TO  WS-ERROR-SW                              GA2GPGM 
00484            MOVE -1   TO                                           GA2GPGM 
00485               MAP-PROVISION-ID-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)  GA2GPGM 
00486            MOVE ' *** PROVISION ID ARGUMENT IS INVALID ***'       GA2GPGM 
00487               TO  ERRMSGO.                                        GA2GPGM 
00488                                                                   GA2GPGM 
00489      IF MAP-PROVISION-ID-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA2GPGM 
00490          NOT = DFHBMUBF                                           GA2GPGM 
00491         MOVE  'MULT01' TO GCVI2-FIELDS-KEY-ID                     GA2GPGM 
00492         MOVE  ZEROES   TO GCVI2-RETURN-CODE                       GA2GPGM 
00493         MOVE MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)       GA2GPGM 
00494                  TO GCVI2-VALUE-LEN-6                             GA2GPGM 
00495         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2GPGM 
00496                        COMMAREA(GCVIOPGMS-PARM)                   GA2GPGM 
00497                        LENGTH(WS-GCVI-COMMAREA-LEN) END-EXEC      GA2GPGM 
00498         IF GCVI2-VALUE-NOT-FOUND                                  GA2GPGM 
00499            MOVE  'MULT06' TO GCVI2-FIELDS-KEY-ID                  GA2GPGM 
00500            MOVE  ZEROES   TO GCVI2-RETURN-CODE                    GA2GPGM 
00501            MOVE MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)    GA2GPGM 
00502                     TO GCVI2-VALUE-LEN-6                          GA2GPGM 
00503            EXEC CICS LINK PROGRAM('GCVIOPGM')                     GA2GPGM 
00504                           COMMAREA(GCVIOPGMS-PARM)                GA2GPGM 
00505                           LENGTH(WS-GCVI-COMMAREA-LEN) END-EXEC   GA2GPGM 
00506            IF GCVI2-VALUE-NOT-FOUND                               GA2GPGM 
00507               IF WS-ERROR-SW NOT = 'Y'                            GA2GPGM 
00508                  MOVE DFHBMUBF  TO MAP-PROVISION-ID-ARGUMENT-ATTR GA2GPGM 
00509                                             (MAP-IDX1, MAP-IDX2)  GA2GPGM 
00510                  MOVE -1  TO MAP-PROVISION-ID-ARGUMENT-LEN        GA2GPGM 
00511                                       (MAP-IDX1, MAP-IDX2)        GA2GPGM 
00512                  MOVE ' *** PROVISION ID ARGUMENT IS INVALID ***' GA2GPGM 
00513                     TO  ERRMSGO                                   GA2GPGM 
00514                  MOVE 'Y' TO WS-ERROR-SW                          GA2GPGM 
00515               ELSE                                                GA2GPGM 
00516                  MOVE DFHBMUBF  TO MAP-PROVISION-ID-ARGUMENT-ATTR GA2GPGM 
00517                                             (MAP-IDX1, MAP-IDX2)  GA2GPGM 
00518            ELSE                                                   GA2GPGM 
00519               IF GCVI2-VALUE-NOT-LOADED                           GA2GPGM 
00520                  IF EIBAID  =  DFHPF4 OR  =  DFHPF16              GA2GPGM 
00521                     NEXT SENTENCE                                 GA2GPGM 
00522                  ELSE                                             GA2GPGM 
00523                     MOVE DFHBMUBF                                 GA2GPGM 
00524                     TO   MAP-PROVISION-ID-ARGUMENT-ATTR           GA2GPGM 
00525                                   (MAP-IDX1, MAP-IDX2)            GA2GPGM 
00526                     IF WS-ERROR-SW  NOT =  'Y'                    GA2GPGM 
00527                        MOVE 'Y' TO  WS-ERROR-SW                   GA2GPGM 
00528                        MOVE -1  TO  MAP-PROVISION-ID-ARGUMENT-LEN GA2GPGM 
00529                                              (MAP-IDX1, MAP-IDX2) GA2GPGM 
00530               MOVE 'EDIT TABLE EMPTY - DATA NOT VALIDATED -  PRESSGA2GPGM 
00531 -                     ' PF4 / PF16 TO CONTINUE'  TO  ERRMSGO.     GA2GPGM 
00532                                                                   GA2GPGM 
00533      IF MAP-PROVISION-ID-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)  NOT =GA2GPGM 
00534                                                           DFHBMUBFGA2GPGM 
00535         ADD 1  TO  WS-ADD-COUNT                                   GA2GPGM 
00536         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2GPGM 
00537         MOVE MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2) TO    GA2GPGM 
00538            WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX).                GA2GPGM 
00539                                                                   GA2GPGM 
00540      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2GPGM 
00541         SET MAP-IDX1  UP BY  1                                    GA2GPGM 
00542         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2GPGM 
00543      IF MAP-IDX2  <  WS-MAP-COL                                   GA2GPGM 
00544         SET MAP-IDX1  TO  1                                       GA2GPGM 
00545         SET MAP-IDX2  UP BY  1                                    GA2GPGM 
00546         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2GPGM 
00547                                                                   GA2GPGM 
00548  2020-CHECK-FOR-ERRORS.                                           GA2GPGM 
00549      MOVE '2020'  TO  WS-PARA-ID.                                 GA2GPGM 
00550      IF INCEXCI  NOT =  'I' AND  NOT =  'E'                       GA2GPGM 
00551         MOVE -1  TO  INCEXCL                                      GA2GPGM 
00552         MOVE 'Y'  TO  WS-ERROR-SW                                 GA2GPGM 
00553         MOVE '*** INCLUDE/EXCLUDE FIELD VALUE NOT VALID ***'  TO  GA2GPGM 
00554            ERRMSGO.                                               GA2GPGM 
00555                                                                   GA2GPGM 
00556      IF WS-ERROR-SW  =  'Y' OR GCVI2-TABLE-SW = 'N'               GA2GPGM 
00557         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2GPGM 
00558            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2GPGM 
00559            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2GPGM 
00560            INCEXCO                                                GA2GPGM 
00561         MOVE '2100'  TO  WS-PARA-ID                               GA2GPGM 
00562         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2GPGM 
00563            VARYING MAP-IDX2 FROM  1  BY  1                        GA2GPGM 
00564               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2GPGM 
00565            AFTER MAP-IDX1 FROM  1  BY  1                          GA2GPGM 
00566               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2GPGM 
00567         MOVE '2020'  TO  WS-PARA-ID                               GA2GPGM 
00568         EXEC CICS SEND   MAP('GA2GI01') MAPSET('GA2GSET')         GA2GPGM 
00569            DATAONLY FROM(GA2GI01O) CURSOR END-EXEC                GA2GPGM 
00570         GO TO 2099-EXIT.                                          GA2GPGM 
00571                                                                   GA2GPGM 
00572      IF WS-ADD-COUNT  NOT >  ZERO AND                             GA2GPGM 
00573         INCEXCI  =  INEXDRKI                                      GA2GPGM 
00574         MOVE '*** NO ADD ENTRY FOUND OR INC/EXC FIELD CHANGE ***' GA2GPGM 
00575            TO  ERRMSGO                                            GA2GPGM 
00576         MOVE -1  TO  INCEXCL                                      GA2GPGM 
00577         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2GPGM 
00578            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2GPGM 
00579            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2GPGM 
00580            INCEXCO                                                GA2GPGM 
00581         EXEC CICS SEND   MAP('GA2GI01') MAPSET('GA2GSET')         GA2GPGM 
00582            DATAONLY FROM(GA2GI01O) CURSOR END-EXEC                GA2GPGM 
00583         GO TO 2099-EXIT.                                          GA2GPGM 
00584                                                                   GA2GPGM 
00585  2025-CONTINUE-PROCESSING.                                        GA2GPGM 
00586                                                                   GA2GPGM 
00587      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2GPGM 
00588               GC-GCIOPARM-LEN                 +                   GA2GPGM 
00589               GC-WORKFILE-KEY-LEN             +                   GA2GPGM 
00590               GC-GCTABULR-IBGR-FIXED-LEN      +                   GA2GPGM 
00591              (GC-GCTABULR-IBGR-VARY-LEN       *                   GA2GPGM 
00592               GC-GCTABULR-IBGR-VARY-MAX-OCUR)                     GA2GPGM 
00593                                                                   GA2GPGM 
00594                                                                   GA2GPGM 
00595      EXEC CICS                                                    GA2GPGM 
00596         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2GPGM 
00597         INITIMG(WS-HEX-00)                                        GA2GPGM 
00598         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2GPGM 
00599      END-EXEC.                                                    GA2GPGM 
00600                                                                   GA2GPGM 
00601      IF  FRMNUIDI  =  'GS3A'                                      GA2GPGM 
00602         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2GPGM 
00603         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2GPGM 
00604         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA2GPGM 
00605         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2GPGM 
00606 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2GPGM 
00607         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2GPGM 
00608 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2GPGM 
00609         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2GPGM 
00610         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2GPGM 
00611         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2GPGM 
00612                          GCIO-WRK-PROVIDER-CONTROL                GA2GPGM 
00613         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2GPGM 
00614         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2GPGM 
00615                                                                   GA2GPGM 
00616      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2GPGM 
00617         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2GPGM 
00618         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2GPGM 
00619         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2GPGM 
00620         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2GPGM 
00621 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2GPGM 
00622         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2GPGM 
00623 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2GPGM 
00624         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2GPGM 
00625         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2GPGM 
00626         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2GPGM 
00627         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2GPGM 
00628         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2GPGM 
00629         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2GPGM 
00630                                                                   GA2GPGM 
00631      IF  FRMNUIDI  =  'GC8A'                                      GA2GPGM 
00632         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2GPGM 
00633         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2GPGM 
00634         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA2GPGM 
00635         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2GPGM 
00636 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2GPGM 
00637         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2GPGM 
00638 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2GPGM 
00639         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2GPGM 
00640         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2GPGM 
00641         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2GPGM 
00642         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2GPGM 
00643         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2GPGM 
00644         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2GPGM 
00645                                                                   GA2GPGM 
00646      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2GPGM 
00647      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2GPGM 
00648      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2GPGM 
00649      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2GPGM 
00650      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA2GPGM 
00651      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2GPGM 
00652      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2GPGM 
00653                                                                   GA2GPGM 
00654      MOVE  GC-GCTABULR-IBGR-VARY-MAX-OCUR                         GA2GPGM 
00655            TO  GX1-ENTRY-COUNT.                                   GA2GPGM 
00656      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2GPGM 
00657                                                                   GA2GPGM 
00658      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2GPGM 
00659         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2GPGM 
00660         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2GPGM 
00661                                                                   GA2GPGM 
00662      IF  NOT GCIO-GOOD-RETURN                                     GA2GPGM 
00663         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2GPGM 
00664 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2GPGM 
00665         MOVE '2GF1'  TO  WS-ABEND-CODE                            GA2GPGM 
00666         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
00667                                                                   GA2GPGM 
00668      MOVE INCEXCI  TO  INEXDRKO,  GX1-INCLUDE-EXCLUDE-IND.        GA2GPGM 
00669      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2GPGM 
00670         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2GPGM 
00671                                                                   GA2GPGM 
00672       SET WS-SORT-IDX  TO  1.                                     GA2GPGM 
00673       SET WS-SORT-IDX2  TO  2.                                    GA2GPGM 
00674       MOVE '2030'  TO  WS-PARA-ID.                                GA2GPGM 
00675                                                                   GA2GPGM 
00676  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2GPGM 
00677      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2GPGM 
00678         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2GPGM 
00679                                                                   GA2GPGM 
00680      IF WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX) <                  GA2GPGM 
00681         WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX2)                   GA2GPGM 
00682         SET WS-SORT-IDX2  UP BY  1                                GA2GPGM 
00683         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2GPGM 
00684      ELSE                                                         GA2GPGM 
00685         IF WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX) >               GA2GPGM 
00686            WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX2)                GA2GPGM 
00687            MOVE WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX) TO         GA2GPGM 
00688               WS-SAVED-BENEFIT                                    GA2GPGM 
00689            MOVE WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX2) TO        GA2GPGM 
00690               WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX)              GA2GPGM 
00691            MOVE WS-SAVED-BENEFIT TO                               GA2GPGM 
00692               WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX2)             GA2GPGM 
00693            SET WS-SORT-IDX2  UP BY  1                             GA2GPGM 
00694            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2GPGM 
00695                                                                   GA2GPGM 
00696      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2GPGM 
00697      MOVE WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX3) TO              GA2GPGM 
00698         WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX2).                  GA2GPGM 
00699      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2GPGM 
00700      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2GPGM 
00701                                                                   GA2GPGM 
00702  2040-ARE-WE-DONE-WITH-SORT.                                      GA2GPGM 
00703      MOVE '2040'  TO  WS-PARA-ID.                                 GA2GPGM 
00704      SET WS-SORT-IDX  UP BY  1.                                   GA2GPGM 
00705      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2GPGM 
00706         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2GPGM 
00707         SET WS-SORT-IDX2  UP BY  1                                GA2GPGM 
00708         MOVE '2030'  TO  WS-PARA-ID                               GA2GPGM 
00709         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2GPGM 
00710      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2GPGM 
00711      MOVE HIGH-VALUES TO WS-PROV-ID-ARGUMENT-ENTRY (WS-SORT-IDX). GA2GPGM 
00712      MOVE GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT.                   GA2GPGM 
00713                                                                   GA2GPGM 
00714      COMPUTE WS-COPY-LENGTH  =                                    GA2GPGM 
00715              GX1-ENTRY-COUNT  *  GC-GCTABULR-IBGR-VARY-LEN.       GA2GPGM 
00716                                                                   GA2GPGM 
00717      EXEC CICS                                                    GA2GPGM 
00718         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA2GPGM 
00719         LENGTH      (WS-COPY-LENGTH)                              GA2GPGM 
00720         INITIMG     (WS-HEX-00)                                   GA2GPGM 
00721      END-EXEC.                                                    GA2GPGM 
00722                                                                   GA2GPGM 
00723      MOVE GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT.                   GA2GPGM 
00724      SET COPY-IDX,  GX1-INDEX  TO  1.                             GA2GPGM 
00725                                                                   GA2GPGM 
00726      MOVE '2050'  TO  WS-PARA-ID.                                 GA2GPGM 
00727  2050-MAKE-A-COPY-OF-RECORD.                                      GA2GPGM 
00728      IF GX1-INDEX  NOT >  GX1-ENTRY-COUNT                         GA2GPGM 
00729         MOVE GX1-ENTRY (GX1-INDEX)  TO                            GA2GPGM 
00730            COPY-TABULAR-TABLE (COPY-IDX)                          GA2GPGM 
00731         SET COPY-IDX, GX1-INDEX  UP BY  1                         GA2GPGM 
00732         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2GPGM 
00733                                                                   GA2GPGM 
00734      IF WS-ADD-COUNT  +  GX1-ENTRY-COUNT  >                       GA2GPGM 
00735         GC-GCTABULR-IBGR-VARY-MAX-OCUR                            GA2GPGM 
00736         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2GPGM 
00737 -    'EASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                 GA2GPGM 
00738         MOVE '2GL1'  TO  WS-ABEND-CODE                            GA2GPGM 
00739         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
00740                                                                   GA2GPGM 
00741      SET WS-SORT-IDX,  COPY-IDX,  GX1-INDEX  TO  1.               GA2GPGM 
00742                                                                   GA2GPGM 
00743      MOVE '2060'  TO  WS-PARA-ID.                                 GA2GPGM 
00744  2060-MERGE-IN-NEW-ENTRIES.                                       GA2GPGM 
00745      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2GPGM 
00746         SET GX1-INDEX  DOWN BY  1                                 GA2GPGM 
00747         SET GX1-ENTRY-COUNT  TO  GX1-INDEX                        GA2GPGM 
00748         MOVE GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT                 GA2GPGM 
00749         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2GPGM 
00750                                                                   GA2GPGM 
00751      IF WS-PROV-ID-ARGUMENT-ENTRY (WS-SORT-IDX)                   GA2GPGM 
00752               =  HIGH-VALUES  AND                                 GA2GPGM 
00753         COPY-TABULAR-TABLE (COPY-IDX)  NOT =  HIGH-VALUES         GA2GPGM 
00754         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2GPGM 
00755                                                                   GA2GPGM 
00756      IF WS-PROV-ID-ARGUMENT-ENTRY (WS-SORT-IDX)                   GA2GPGM 
00757               NOT =  HIGH-VALUES AND                              GA2GPGM 
00758         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2GPGM 
00759         GO TO 2080-INSERT-NEW-ENTRY.                              GA2GPGM 
00760                                                                   GA2GPGM 
00761      IF WS-PROV-ID-ARGUMENT-ENTRY (WS-SORT-IDX)                   GA2GPGM 
00762               =  HIGH-VALUES AND                                  GA2GPGM 
00763         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2GPGM 
00764         NEXT SENTENCE                                             GA2GPGM 
00765      ELSE                                                         GA2GPGM 
00766         IF WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX) >               GA2GPGM 
00767            COPY-PROVISION-ID-ARGUMENT (COPY-IDX)                  GA2GPGM 
00768            GO TO 2070-SAVE-COPIED-ENTRY                           GA2GPGM 
00769         ELSE                                                      GA2GPGM 
00770            IF WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX) <            GA2GPGM 
00771               COPY-PROVISION-ID-ARGUMENT (COPY-IDX)               GA2GPGM 
00772               GO TO 2080-INSERT-NEW-ENTRY.                        GA2GPGM 
00773                                                                   GA2GPGM 
00774 ******************************************************************GA2GPGM 
00775 **   AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2GPGM 
00776 **   OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2GPGM 
00777 **   INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2GPGM 
00778 **   FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2GPGM 
00779 ******************************************************************GA2GPGM 
00780                                                                   GA2GPGM 
00781      SET WS-SORT-IDX  UP BY  1.                                   GA2GPGM 
00782                                                                   GA2GPGM 
00783  2070-SAVE-COPIED-ENTRY.                                          GA2GPGM 
00784      MOVE '2070'  TO  WS-PARA-ID.                                 GA2GPGM 
00785      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA2GPGM 
00786         GX1-ENTRY (GX1-INDEX).                                    GA2GPGM 
00787      IF COPY-IDX  NOT >  GX1-ENTRY-COUNT                          GA2GPGM 
00788         SET COPY-IDX  UP BY  1                                    GA2GPGM 
00789         SET GX1-INDEX  UP BY  1                                   GA2GPGM 
00790         MOVE '2060'  TO  WS-PARA-ID                               GA2GPGM 
00791         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2GPGM 
00792      ELSE                                                         GA2GPGM 
00793         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2GPGM 
00794 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2GPGM 
00795         MOVE '2GL2'  TO  WS-ABEND-CODE                            GA2GPGM 
00796         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
00797                                                                   GA2GPGM 
00798  2080-INSERT-NEW-ENTRY.                                           GA2GPGM 
00799      MOVE '2080'  TO  WS-PARA-ID.                                 GA2GPGM 
00800                                                                   GA2GPGM 
00801      MOVE WS-PROVISION-ID-ARGUMENT (WS-SORT-IDX) TO               GA2GPGM 
00802         GX1-PROVISION-ID-ARGUMENT (GX1-INDEX).                    GA2GPGM 
00803      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2GPGM 
00804         SET WS-SORT-IDX  UP BY  1                                 GA2GPGM 
00805         SET GX1-INDEX  UP BY  1                                   GA2GPGM 
00806         MOVE '2060'  TO  WS-PARA-ID                               GA2GPGM 
00807         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2GPGM 
00808      ELSE                                                         GA2GPGM 
00809         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2GPGM 
00810 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2GPGM 
00811         MOVE '2GL3'  TO  WS-ABEND-CODE                            GA2GPGM 
00812         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
00813                                                                   GA2GPGM 
00814  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2GPGM 
00815      MOVE '2090'  TO  WS-PARA-ID.                                 GA2GPGM 
00816                                                                   GA2GPGM 
00817 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2GPGM 
00818                                                                   GA2GPGM 
00819      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2GPGM 
00820      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2GPGM 
00821                                                                   GA2GPGM 
00822      COMPUTE  GCIO-RECORD-LENGTH  =                               GA2GPGM 
00823               GC-WORKFILE-KEY-LEN             +                   GA2GPGM 
00824               GC-GCTABULR-IBGR-FIXED-LEN      +                   GA2GPGM 
00825              (GC-GCTABULR-IBGR-VARY-LEN       *  GX1-ENTRY-COUNT).GA2GPGM 
00826                                                                   GA2GPGM 
00827      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2GPGM 
00828            GC-GCIOPARM-LEN   +  GCIO-RECORD-LENGTH.               GA2GPGM 
00829                                                                   GA2GPGM 
00830      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2GPGM 
00831         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2GPGM 
00832         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2GPGM 
00833                                                                   GA2GPGM 
00834      IF NOT GCIO-GOOD-RETURN                                      GA2GPGM 
00835         MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR RECORGA2GPGM 
00836 -    'D.  CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                  GA2GPGM 
00837         MOVE '2GF2'  TO  WS-ABEND-CODE                            GA2GPGM 
00838         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
00839                                                                   GA2GPGM 
00840      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2GPGM 
00841         VARYING MAP-IDX2 FROM 1  BY  1                            GA2GPGM 
00842            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2GPGM 
00843         AFTER MAP-IDX1 FROM 1  BY  1                              GA2GPGM 
00844            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2GPGM 
00845                                                                   GA2GPGM 
00846      EXEC CICS SEND   MAP('GA2GI01') MAPSET('GA2GSET') ERASE      GA2GPGM 
00847         FROM(GA2GI01O) END-EXEC.                                  GA2GPGM 
00848                                                                   GA2GPGM 
00849  2099-EXIT.   EXIT.                                               GA2GPGM 
00850 /                                                                 GA2GPGM 
00851 ******************************************************************GA2GPGM 
00852 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2GPGM 
00853 **                                                                GA2GPGM 
00854 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2GPGM 
00855 **  ALREADY ON THE OPERATORS SCREEN.                              GA2GPGM 
00856 **                                                                GA2GPGM 
00857 ******************************************************************GA2GPGM 
00858  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2GPGM 
00859                                                                   GA2GPGM 
00860      MOVE LOW-VALUES  TO                                          GA2GPGM 
00861           MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2).         GA2GPGM 
00862                                                                   GA2GPGM 
00863  2199-EXIT.   EXIT.                                               GA2GPGM 
00864 /                                                                 GA2GPGM 
00865 ******************************************************************GA2GPGM 
00866 **          X C T L   T O   D E L   S C R E E N                   GA2GPGM 
00867 **                                                                GA2GPGM 
00868 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2GPGM 
00869 ** DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2GPGM 
00870 ** PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2GPGM 
00871 ** TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2GPGM 
00872 ** THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2GPGM 
00873 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2GPGM 
00874 ******************************************************************GA2GPGM 
00875  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2GPGM 
00876      MOVE '3000'  TO  WS-PARA-ID.                                 GA2GPGM 
00877                                                                   GA2GPGM 
00878      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2GPGM 
00879               GC-GCIOPARM-LEN                 +                   GA2GPGM 
00880               GC-WORKFILE-KEY-LEN             +                   GA2GPGM 
00881               GC-GCTABULR-IBGR-FIXED-LEN      +                   GA2GPGM 
00882              (GC-GCTABULR-IBGR-VARY-LEN       *                   GA2GPGM 
00883               GC-GCTABULR-IBGR-VARY-MAX-OCUR)                     GA2GPGM 
00884                                                                   GA2GPGM 
00885                                                                   GA2GPGM 
00886      EXEC CICS                                                    GA2GPGM 
00887         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2GPGM 
00888         INITIMG(WS-HEX-00)                                        GA2GPGM 
00889         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2GPGM 
00890      END-EXEC.                                                    GA2GPGM 
00891                                                                   GA2GPGM 
00892 *    EXEC CICS                                                    GA2GPGM 
00893 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2GPGM 
00894 *       LENGTH      (WS-COMMUNICATION-KEY-LEN)                    GA2GPGM 
00895 *       INITIMG     (WS-HEX-00)                                   GA2GPGM 
00896 *    END-EXEC.                                                    GA2GPGM 
00897                                                                   GA2GPGM 
00898      IF  FRMNUIDI  =  'GS3A'                                      GA2GPGM 
00899         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2GPGM 
00900         MOVE   'G'   TO  GCIO-WRK-STATUS-CODE                     GA2GPGM 
00901         MOVE   'G4'  TO  GCIO-WRK-RECORD-TYPE                     GA2GPGM 
00902         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2GPGM 
00903         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2GPGM 
00904         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2GPGM 
00905         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2GPGM 
00906         MOVE  SPACES  TO  GCIO-WRK-LINE-OF-BUS                    GA2GPGM 
00907                           GCIO-WRK-PROVIDER-CONTROL               GA2GPGM 
00908         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2GPGM 
00909         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2GPGM 
00910                                                                   GA2GPGM 
00911      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2GPGM 
00912         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2GPGM 
00913         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2GPGM 
00914         MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE                     GA2GPGM 
00915         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2GPGM 
00916         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2GPGM 
00917         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2GPGM 
00918         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2GPGM 
00919         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2GPGM 
00920         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2GPGM 
00921         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2GPGM 
00922         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2GPGM 
00923                                                                   GA2GPGM 
00924      IF  FRMNUIDI  =  'GC8A'                                      GA2GPGM 
00925         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2GPGM 
00926         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2GPGM 
00927         MOVE   'C6'  TO  GCIO-WRK-RECORD-TYPE                     GA2GPGM 
00928         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2GPGM 
00929         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2GPGM 
00930         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2GPGM 
00931         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2GPGM 
00932         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2GPGM 
00933         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2GPGM 
00934         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2GPGM 
00935         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                 GA2GPGM 
00936         MOVE  GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID.          GA2GPGM 
00937                                                                   GA2GPGM 
00938      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2GPGM 
00939      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2GPGM 
00940      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2GPGM 
00941      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2GPGM 
00942      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2GPGM 
00943      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2GPGM 
00944      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2GPGM 
00945      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2GPGM 
00946                                                                   GA2GPGM 
00947      MOVE INCEXCI TO GCA-I-E-INDC.                                GA2GPGM 
00948                                                                   GA2GPGM 
00949      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2GPGM 
00950 *    MOVE  SPACES  TO  GCA-EFFECTIVE-DATE.                        GA2GPGM 
00951      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2GPGM 
00952                                                                   GA2GPGM 
00953      SET GCA-RECORD-POINTER                                       GA2GPGM 
00954          TO ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD.              GA2GPGM 
00955                                                                   GA2GPGM 
00956      MOVE  GC-GCTABULR-IBGR-VARY-MAX-OCUR                         GA2GPGM 
00957            TO  GX1-ENTRY-COUNT.                                   GA2GPGM 
00958      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2GPGM 
00959                                                                   GA2GPGM 
00960      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2GPGM 
00961         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2GPGM 
00962         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2GPGM 
00963                                                                   GA2GPGM 
00964      IF  NOT GCIO-GOOD-RETURN                                     GA2GPGM 
00965         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR RECORD.GA2GPGM 
00966 -    ' CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                     GA2GPGM 
00967         MOVE '2GF3'  TO  WS-ABEND-CODE                            GA2GPGM 
00968         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
00969                                                                   GA2GPGM 
00970 *    SET  COMMAREA-PNTR   TO                                      GA2GPGM 
00971 *         ADDRESS  OF GCA-COMMAREA.                               GA2GPGM 
00972                                                                   GA2GPGM 
00973 *    EXEC CICS XCTL  PROGRAM('GA1GPGM') COMMAREA(COMMAREA-PNTR)   GA2GPGM 
00974 *       LENGTH(4)  END-EXEC.                                      GA2GPGM 
00975      EXEC CICS XCTL  PROGRAM('GA1GPGM')                           GA2GPGM 
00976                      COMMAREA(DFHCOMMAREA)                        GA2GPGM 
00977                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2GPGM 
00978      END-EXEC.                                                    GA2GPGM 
00979                                                                   GA2GPGM 
00980  3099-EXIT.   EXIT.                                               GA2GPGM 
00981 /                                                                 GA2GPGM 
00982 ***************************************************************** GA2GPGM 
00983 **          D I S P L A Y   F I R S T   S C R E E N               GA2GPGM 
00984 **                                                                GA2GPGM 
00985 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2GPGM 
00986 ** OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2GPGM 
00987 ** THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2GPGM 
00988 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2GPGM 
00989 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2GPGM 
00990 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2GPGM 
00991 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2GPGM 
00992 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2GPGM 
00993 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2GPGM 
00994 ******************************************************************GA2GPGM 
00995  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2GPGM 
00996      MOVE '4000'  TO  WS-PARA-ID.                                 GA2GPGM 
00997                                                                   GA2GPGM 
00998      MOVE LOW-VALUES  TO  GA2GI01O.                               GA2GPGM 
00999                                                                   GA2GPGM 
01000      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA2GPGM 
01001         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2GPGM 
01002            TO  ERRMSGO                                            GA2GPGM 
01003         MOVE '2GC1'  TO  WS-ABEND-CODE                            GA2GPGM 
01004         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
01005                                                                   GA2GPGM 
01006 *    SET  ADDRESS OF  GCA-COMMAREA  TO                            GA2GPGM 
01007 *         INCOMING-COMMAREA-PNTR.                                 GA2GPGM 
01008                                                                   GA2GPGM 
01009      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA2GPGM 
01010      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA2GPGM 
01011      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA2GPGM 
01012      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA2GPGM 
01013      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA2GPGM 
01014      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA2GPGM 
01015      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA2GPGM 
01016      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA2GPGM 
01017                                                                   GA2GPGM 
01018      MOVE GCA-I-E-INDC TO INCEXCO,                                GA2GPGM 
01019                        INEXDRKO.                                  GA2GPGM 
01020                                                                   GA2GPGM 
01021      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2GPGM 
01022         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA2GPGM 
01023 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2GPGM 
01024         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2GPGM 
01025         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2GPGM 
01026         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2GPGM 
01027         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2GPGM 
01028         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2GPGM 
01029         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2GPGM 
01030         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2GPGM 
01031         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2GPGM 
01032         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2GPGM 
01033         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2GPGM 
01034         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2GPGM 
01035         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2GPGM 
01036                                                                   GA2GPGM 
01037      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2GPGM 
01038         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA2GPGM 
01039 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2GPGM 
01040         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2GPGM 
01041         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2GPGM 
01042         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2GPGM 
01043         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2GPGM 
01044         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2GPGM 
01045         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2GPGM 
01046         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2GPGM 
01047         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2GPGM 
01048         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2GPGM 
01049         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2GPGM 
01050         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2GPGM 
01051         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2GPGM 
01052         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2GPGM 
01053         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2GPGM 
01054         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2GPGM 
01055         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2GPGM 
01056                                                                   GA2GPGM 
01057      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2GPGM 
01058         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA2GPGM 
01059         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2GPGM 
01060         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2GPGM 
01061         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2GPGM 
01062         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2GPGM 
01063         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2GPGM 
01064         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2GPGM 
01065         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2GPGM 
01066         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2GPGM 
01067         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2GPGM 
01068         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2GPGM 
01069         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2GPGM 
01070         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2GPGM 
01071         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2GPGM 
01072         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2GPGM 
01073         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2GPGM 
01074         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2GPGM 
01075         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2GPGM 
01076         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2GPGM 
01077                                                                   GA2GPGM 
01078      EXEC CICS SEND   MAP('GA2GI01') MAPSET('GA2GSET') ERASE      GA2GPGM 
01079         FROM(GA2GI01O) END-EXEC.                                  GA2GPGM 
01080                                                                   GA2GPGM 
01081  4099-EXIT.   EXIT.                                               GA2GPGM 
01082 /                                                                 GA2GPGM 
01083 ***************************************************************** GA2GPGM 
01084 **        X C T L   T O   P R E V I O U S   M E N U               GA2GPGM 
01085 **                                                                GA2GPGM 
01086 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2GPGM 
01087 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2GPGM 
01088 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2GPGM 
01089 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2GPGM 
01090 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2GPGM 
01091 ******************************************************************GA2GPGM 
01092  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2GPGM 
01093      MOVE '5000'  TO  WS-PARA-ID.                                 GA2GPGM 
01094                                                                   GA2GPGM 
01095                                                                   GA2GPGM 
01096 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA2GPGM 
01097 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA2GPGM 
01098 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA2GPGM 
01099                                                                   GA2GPGM 
01100      IF  ALTBFNCI  =  'GTM1'                                      GA2GPGM 
01101          EXEC CICS XCTL                                           GA2GPGM 
01102                    PROGRAM('GTM1PGM')                             GA2GPGM 
01103                    END-EXEC.                                      GA2GPGM 
01104                                                                   GA2GPGM 
01105                                                                   GA2GPGM 
01106      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2GPGM 
01107               GC-GCIOPARM-LEN             +                       GA2GPGM 
01108               GC-WORKFILE-KEY-LEN         +                       GA2GPGM 
01109               GC-GCTABULR-ABM-FIXED-LEN   +                       GA2GPGM 
01110              (GC-GCTABULR-ABM-VARY-LEN    *                       GA2GPGM 
01111               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA2GPGM 
01112                                                                   GA2GPGM 
01113      EXEC CICS                                                    GA2GPGM 
01114         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA2GPGM 
01115         INITIMG(WS-HEX-00)                                        GA2GPGM 
01116         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA2GPGM 
01117      END-EXEC.                                                    GA2GPGM 
01118                                                                   GA2GPGM 
01119 *    EXEC CICS                                                    GA2GPGM 
01120 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2GPGM 
01121 *       INITIMG(WS-HEX-00)                                        GA2GPGM 
01122 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2GPGM 
01123 *    END-EXEC.                                                    GA2GPGM 
01124                                                                   GA2GPGM 
01125      IF  FRMNUIDI  =  'GS3A'                                      GA2GPGM 
01126         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2GPGM 
01127         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2GPGM 
01128         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA2GPGM 
01129         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2GPGM 
01130         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2GPGM 
01131         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2GPGM 
01132         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2GPGM 
01133         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS                     GA2GPGM 
01134                          GCIO-WRK-PROVIDER-CONTROL                GA2GPGM 
01135         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2GPGM 
01136         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2GPGM 
01137         MOVE SPACES TO GCA-BEN-PROV-ID                            GA2GPGM 
01138         MOVE ALTABIDI TO  GCIO-WRK-PROVISION-ID                   GA2GPGM 
01139                           GCA-ALL-LEVEL-TAB-ID                    GA2GPGM 
01140         MOVE ALTBSLTI TO  GCIO-WRK-PROVISION-SLOT-NO              GA2GPGM 
01141                           GCA-ALL-LEVEL-TAB-SLOT                  GA2GPGM 
01142         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA2GPGM 
01143                        GCA-INTERNAL-TAB-ID                        GA2GPGM 
01144                        GCA-INTERNAL-TAB-SLOT                      GA2GPGM 
01145         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA2GPGM 
01146                                                                   GA2GPGM 
01147      IF  FRMNUIDI  =  'GC4A'                                      GA2GPGM 
01148         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2GPGM 
01149         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2GPGM 
01150         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2GPGM 
01151         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2GPGM 
01152         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2GPGM 
01153         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2GPGM 
01154         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2GPGM 
01155         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2GPGM 
01156         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2GPGM 
01157         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2GPGM 
01158         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2GPGM 
01159         MOVE SPACES TO GCA-BEN-PROV-ID                            GA2GPGM 
01160         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA2GPGM 
01161                          GCA-ALL-LEVEL-TAB-ID                     GA2GPGM 
01162         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA2GPGM 
01163                          GCA-ALL-LEVEL-TAB-SLOT                   GA2GPGM 
01164         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA2GPGM 
01165                        GCA-INTERNAL-TAB-ID                        GA2GPGM 
01166                        GCA-INTERNAL-TAB-SLOT                      GA2GPGM 
01167         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA2GPGM 
01168                                                                   GA2GPGM 
01169      IF  FRMNUIDI  =  'GC8A'                                      GA2GPGM 
01170         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2GPGM 
01171         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2GPGM 
01172         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA2GPGM 
01173         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2GPGM 
01174         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2GPGM 
01175         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2GPGM 
01176         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2GPGM 
01177         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2GPGM 
01178         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2GPGM 
01179         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2GPGM 
01180         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2GPGM 
01181         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA2GPGM 
01182         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2GPGM 
01183         MOVE ALTABIDI TO GCIO-WRK-TAB-PROVISION-ID                GA2GPGM 
01184                          GCA-ALL-LEVEL-TAB-ID                     GA2GPGM 
01185         MOVE ALTBSLTI TO GCIO-WRK-TAB-PROV-SLOT-NO                GA2GPGM 
01186                          GCA-ALL-LEVEL-TAB-SLOT                   GA2GPGM 
01187         MOVE SPACES TO GCA-INTERNAL-TAB-ID                        GA2GPGM 
01188                        GCA-INTERNAL-TAB-SLOT.                     GA2GPGM 
01189                                                                   GA2GPGM 
01190      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA2GPGM 
01191 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA2GPGM 
01192 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA2GPGM 
01193 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA2GPGM 
01194 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA2GPGM 
01195 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA2GPGM 
01196      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2GPGM 
01197      SET GCA-RECORD-POINTER                                       GA2GPGM 
01198          TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                  GA2GPGM 
01199                                                                   GA2GPGM 
01200      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO                       GA2GPGM 
01201           GAA-ENTRY-COUNT.                                        GA2GPGM 
01202      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2GPGM 
01203                                                                   GA2GPGM 
01204      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2GPGM 
01205         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA2GPGM 
01206         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA2GPGM 
01207                                                                   GA2GPGM 
01208      IF  NOT GCIO2-GOOD-RETURN                                    GA2GPGM 
01209         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2GPGM 
01210 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2GPGM 
01211         MOVE '2GF4'  TO  WS-ABEND-CODE                            GA2GPGM 
01212         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2GPGM 
01213                                                                   GA2GPGM 
01214 *    SET  COMMAREA-PNTR                                           GA2GPGM 
01215 *         TO   ADDRESS  OF  GCA-COMMAREA.                         GA2GPGM 
01216                                                                   GA2GPGM 
01217      IF  ALTBFNCI  =  'GA1B'                                      GA2GPGM 
01218 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA2GPGM 
01219 *          LENGTH(4) END-EXEC.                                    GA2GPGM 
01220         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA2GPGM 
01221                         COMMAREA(DFHCOMMAREA)                     GA2GPGM 
01222                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2GPGM 
01223         END-EXEC.                                                 GA2GPGM 
01224                                                                   GA2GPGM 
01225      IF  ALTBFNCI  =  'GA1C'                                      GA2GPGM 
01226 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA2GPGM 
01227 *          LENGTH(4) END-EXEC.                                    GA2GPGM 
01228         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA2GPGM 
01229                         COMMAREA(DFHCOMMAREA)                     GA2GPGM 
01230                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2GPGM 
01231         END-EXEC.                                                 GA2GPGM 
01232                                                                   GA2GPGM 
01233      IF  ALTBFNCI  =  'GA1D'                                      GA2GPGM 
01234 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA2GPGM 
01235 *          LENGTH(4) END-EXEC.                                    GA2GPGM 
01236         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA2GPGM 
01237                         COMMAREA(DFHCOMMAREA)                     GA2GPGM 
01238                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2GPGM 
01239         END-EXEC.                                                 GA2GPGM 
01240                                                                   GA2GPGM 
01241      IF  ALTBFNCI  =  'GA1E'                                      GA2GPGM 
01242 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA2GPGM 
01243 *          LENGTH(4) END-EXEC.                                    GA2GPGM 
01244         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA2GPGM 
01245                         COMMAREA(DFHCOMMAREA)                     GA2GPGM 
01246                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2GPGM 
01247         END-EXEC.                                                 GA2GPGM 
01248                                                                   GA2GPGM 
01249      IF  ALTBFNCI  =  'GA1P'                                      GA2GPGM 
01250         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA2GPGM 
01251                         COMMAREA(DFHCOMMAREA)                     GA2GPGM 
01252                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2GPGM 
01253         END-EXEC.                                                 GA2GPGM 
01254                                                                   GA2GPGM 
01255  5099-EXIT.                                                       GA2GPGM 
01256      EXIT.                                                        GA2GPGM 
01257 /                                                                 GA2GPGM 
01258 ***************************************************************** GA2GPGM 
01259 **           X C T L   T O   M A I N   M E N U                    GA2GPGM 
01260 **                                                                GA2GPGM 
01261 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2GPGM 
01262 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2GPGM 
01263 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2GPGM 
01264 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2GPGM 
01265 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2GPGM 
01266 ** AND PROGRESS DOWN.                                             GA2GPGM 
01267 ******************************************************************GA2GPGM 
01268  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2GPGM 
01269      MOVE '6000'  TO  WS-PARA-ID.                                 GA2GPGM 
01270      MOVE '2GP1'  TO  WS-ABEND-CODE.                              GA2GPGM 
01271                                                                   GA2GPGM 
01272      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2GPGM 
01273                                                                   GA2GPGM 
01274  6099-EXIT.     EXIT.                                             GA2GPGM 
01275 /*****************************************************************GA2GPGM 
01276 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA2GPGM 
01277 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA2GPGM 
01278 ******************************************************************GA2GPGM 
01279  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA2GPGM 
01280  9800-010.                                                        GA2GPGM 
01281                                                                   GA2GPGM 
01282      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA2GPGM 
01283      MOVE 'M'   TO  HGADATE-FORM1.                                GA2GPGM 
01284      MOVE 'J'   TO  HGADATE-FORM2.                                GA2GPGM 
01285      MOVE ZEROS TO  HGADATE-RETURN                                GA2GPGM 
01286                     HGADATE-AMOUNT.                               GA2GPGM 
01287      EXEC CICS LINK PROGRAM ('HGADATES')                          GA2GPGM 
01288                     COMMAREA(HGADATES-COMMAREA)                   GA2GPGM 
01289                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA2GPGM 
01290                     END-EXEC.                                     GA2GPGM 
01291                                                                   GA2GPGM 
01292  9800-900-900-EXIT.                                               GA2GPGM 
01293      EXIT.                                                        GA2GPGM 
01294 /*****************************************************************GA2GPGM 
01295 *     E R R O R   M E S S A G E   T H E N   A B E N D             GA2GPGM 
01296 ******************************************************************GA2GPGM 
01297  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2GPGM 
01298                                                                   GA2GPGM 
01299      SET MAP-IDX1  TO  7.                                         GA2GPGM 
01300      SET MAP-IDX2  TO  1.                                         GA2GPGM 
01301      MOVE -1  TO                                                  GA2GPGM 
01302         MAP-PROVISION-ID-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).       GA2GPGM 
01303      EXEC CICS SEND   MAP('GA2GI01') MAPSET('GA2GSET') ERASE      GA2GPGM 
01304         FROM(GA2GI01O) CURSOR WAIT END-EXEC.                      GA2GPGM 
01305                                                                   GA2GPGM 
01306      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2GPGM 
01307                                                                   GA2GPGM 
01308  9999-EXIT.     EXIT.                                             GA2GPGM 
