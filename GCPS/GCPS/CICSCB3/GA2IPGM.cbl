00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GA2IPGM.                                         GA2IPGM 
00003 **** THIS IS A COBOL/2 PROGRAM *****                                 LV001
00004  AUTHOR.         S BUCH.                                          GA2IPGM 
00005  DATE-WRITTEN.   11/13/84.                                        GA2IPGM 
00006  DATE-COMPILED.                                                   GA2IPGM 
00007      SKIP3                                                        GA2IPGM 
00008 *** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2IPGM 
00009 ****** P R O G R A M   M O D I F I C A T I O N   L O G       *****GA2IPGM 
00010 *** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2IPGM 
00011 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2IPGM 
00012 *                                                                 GA2IPGM 
00013 *   T529    03-19-86    MDD   ADDED CODE TO CHECK RETURN CODE FROMGA2IPGM 
00014 *                             GCVIOPGM FOR A VALUE OF '20', THIS  GA2IPGM 
00015 *                             MEANS THE EDIT TABLE IS EMPTY AND A GA2IPGM 
00016 *                             VALIDATION COULD NOT BE PERFORMED.  GA2IPGM 
00017 *                             PF4/16 CAN BE USED TO ACCEPT THE    GA2IPGM 
00018 *                             DATA AS SHOWN ON THE SCREEN AND TO  GA2IPGM 
00019 *                             CONTINUE PROCESSING.                GA2IPGM 
00020 *                                                                 GA2IPGM 
00021 *  EL500    06/18/86    DES   PF4/16 CAUSE VALIDATION TABLE EMPTY GA2IPGM 
00022 *                             CONDITION TO BE IGNORED             GA2IPGM 
00023 *                                                                 GA2IPGM 
00024 * D0120     03/17/87    JLA   CHANGES FOR SINGLE TABULAR SUPPORT  GA2IPGM 
00025 *                             THAT EXECUTES FROM TRANSACTION GTM1:GA2IPGM 
00026 *                             1. PF1/PF13 - CONSTRUCT COMMAREA AS GA2IPGM 
00027 *                                IF GC4A HAD CALLED, XCTL TO      GA2IPGM 
00028 *                                DELETE SCREEN PROGRAM.           GA2IPGM 
00029 *                             2. PF3/PF15 - XCTL TO GTM1PGM       GA2IPGM 
00030 *                                WITHOUT PASSING ANY COMMAREA.    GA2IPGM 
00031 *                                                               * GA2IPGM 
00032 * D116       8/17/87    FRY   CAPTURE OPERATOR-ID WHEN A 'C3',  * GA2IPGM 
00033 *                             'C6', OR 'G4' RECORD IS UPDATED.  * GA2IPGM 
00034 *                                                               * GA2IPGM 
00035 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GA2IPGM 
00036 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GA2IPGM 
00037 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GA2IPGM 
00038 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GA2IPGM 
00039 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GA2IPGM 
00040 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GA2IPGM 
00041 *                       6. REMOVE HARDCOPY ROUTINE.              *GA2IPGM 
00042 *                       7. >>> CONVERT TO COBOL/II <<<<          *GA2IPGM 
00043 *                                                               * GA2IPGM 
00044 *                                                               * GA2IPGM 
00045 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD     * GA2IPGM 
00046 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2IPGM 
00047 *                                                               * GA2IPGM 
00048 *14726/ 11/11/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000 AND * GA2IPGM 
00049 *15057                  THE EXPANSION OF THE GROUP SPECIFIC AND * GA2IPGM 
00050 *                       CONTRACT KEY TO SUPPORT THE TEXAS       * GA2IPGM 
00051 *                       MERGER.                                 * GA2IPGM 
00052 *                                                               * GA2IPGM 
00053 * 14726/  04/11/98  AB   EXPANDED THE SCREEN / MAP              * GA2IPGM 
00054 * 15057                  TO INCLUDE THE ENTIRE KEY              * GA2IPGM 
00055 *                                                               * GA2IPGM 
00056 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP      * GA2IPGM 
00057 *                        2. ADD DELADD-OPTION = 'GAS5UPD'       * GA2IPGM 
00058 *                                                               * GA2IPGM 
00059 * P????   11/19/99  FRY  ADD LENGTH PARAMETER TO THE RETURN     * GA2IPGM 
00060 *                        COMMAND WHEN DFHCOMMAREA IS SPECIFIED. * GA2IPGM 
00061 *                                                               * GA2IPGM 
00062 *                                                               * GA2IPGM 
00063 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA2IPGM 
00064 *                                                               * GA2IPGM 
SI0724*                                                                *00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00009180
SI0724*                           GCCDRLEN                             *00009190
00065 *                                                               * GA2IPGM 
00066 ***************************************************************** GA2IPGM 
00067 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2IPGM 
00068 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2IPGM 
00069 /                                                                 GA2IPGM 
00070 ******************************************************************GA2IPGM 
00071 *   GA2IPGM      ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM   GA2IPGM 
00072 *                    PROVIDER-GROUP BY PROVIDER-TYPES - GA2I      GA2IPGM 
00073 *                                                                 GA2IPGM 
00074 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2IPGM 
00075 *   TABULAR PROVISION ID ARGUMENTS.                               GA2IPGM 
00076 *                                                                 GA2IPGM 
00077 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2IPGM 
00078 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2IPGM 
00079 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2IPGM 
00080 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2IPGM 
00081 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2IPGM 
00082 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2IPGM 
00083 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2IPGM 
00084 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2IPGM 
00085 *                                                                 GA2IPGM 
00086 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IPGT)GA2IPGM 
00087 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2IPGM 
00088 *   XCTL TO TRANS GA1I OR PROGRAM GA1IPGM.  THIS PROGRAM WILL     GA2IPGM 
00089 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2IPGM 
00090 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2IPGM 
00091 *   CODE.                                                         GA2IPGM 
00092 *                                                                 GA2IPGM 
00093 *   FUNC CODE: GA2I                                               GA2IPGM 
00094 *   MAPSET:    GA2ISETC                                           GA2IPGM 
00095 *   FILES:     GCPSWORK                                           GA2IPGM 
00096 ******************************************************************GA2IPGM 
00097      SKIP3                                                        GA2IPGM 
00098  ENVIRONMENT DIVISION.                                            GA2IPGM 
00099 /                                                                 GA2IPGM 
00100  DATA DIVISION.                                                   GA2IPGM 
00101  WORKING-STORAGE SECTION.                                         GA2IPGM 
00102  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2IPGM 
00103      '***GA2IPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2IPGM 
00104  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2IPGM 
00105                                                                   GA2IPGM 
00106  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2IPGM 
00107                                                                   GA2IPGM 
00108  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2IPGM 
00109                                                                   GA2IPGM 
00110  01  COMMAREA-POINTER-AREA.                                       GA2IPGM 
00111      05  COMMAREA-PNTR-COMP      PIC S9(8)  COMP.                 GA2IPGM 
00112      05  COMMAREA-PNTR  REDEFINES                                 GA2IPGM 
00113          COMMAREA-PNTR-COMP      USAGE IS POINTER.                GA2IPGM 
00114                                                                   GA2IPGM 
00115 ** MAP COBOL SCREEN DSECTS **                                     GA2IPGM 
00116  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2IPGM 
00117      '***  I/O MAPAREA ***'.                                      GA2IPGM 
00118  COPY GA2ISETC.                                                   GA2IPGM 
00119 /                                                                 GA2IPGM 
00120 ******************************************************************GA2IPGM 
00121 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2IPGM 
00122 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2IPGM 
00123 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2IPGM 
00124 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2IPGM 
00125 **  REDEFINES.                                                    GA2IPGM 
00126 ******************************************************************GA2IPGM 
00127      SKIP3                                                        GA2IPGM 
00128  01  FILLER     REDEFINES   GA2II01I.                             GA2IPGM 
00129      05  FILLER                              PIC X(89).           GA2IPGM 
00130      05  GROUP-SPECIFIC-ID-LINE.                                  GA2IPGM 
00131          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA2IPGM 
00132          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA2IPGM 
00133          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA2IPGM 
00134          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA2IPGM 
00135          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA2IPGM 
00136          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA2IPGM 
00137          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA2IPGM 
00138          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA2IPGM 
00139          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA2IPGM 
00140          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA2IPGM 
00141          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA2IPGM 
00142          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA2IPGM 
00143          10  FILLER                          PIC X(16).           GA2IPGM 
00144      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA2IPGM 
00145          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2IPGM 
00146          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2IPGM 
00147          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2IPGM 
00148          10  CONTRACT-GROUP-NO               PIC X(9).            GA2IPGM 
00149          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2IPGM 
00150          10  CONTRACT-SECTION-NO             PIC X(5).            GA2IPGM 
00151          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2IPGM 
00152          10  CONTRACT-PKG-CODE               PIC X(3).            GA2IPGM 
00153          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2IPGM 
00154          10  CONTRACT-LOB                    PIC X.               GA2IPGM 
00155          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2IPGM 
00156          10  CONTRACT-PROV-CTL               PIC XX.              GA2IPGM 
00157          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2IPGM 
00158          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2IPGM 
00159          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2IPGM 
00160          10  CONTRACT-EFF-DATE               PIC X(6).            GA2IPGM 
00161          10  FILLER                          PIC X(1).            GA2IPGM 
00162      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA2IPGM 
00163                                     GROUP-SPECIFIC-ID-LINE.       GA2IPGM 
00164          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA2IPGM 
00165          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA2IPGM 
00166          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA2IPGM 
00167          10  BEN-PROV-GROUP-NO               PIC X(9).            GA2IPGM 
00168          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA2IPGM 
00169          10  BEN-PROV-SECTION-NO             PIC X(5).            GA2IPGM 
00170          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA2IPGM 
00171          10  BEN-PROV-PKG-CODE               PIC X(3).            GA2IPGM 
00172          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA2IPGM 
00173          10  BEN-PROV-LOB                    PIC X.               GA2IPGM 
00174          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA2IPGM 
00175          10  BEN-PROV-PROV-CTL               PIC XX.              GA2IPGM 
00176          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA2IPGM 
00177          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA2IPGM 
00178          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA2IPGM 
00179          10  BEN-PROV-EFF-DATE               PIC X(6).            GA2IPGM 
00180          10  BEN-PROV-ID-HEADING             PIC X(6).            GA2IPGM 
00181          10  BEN-PROV-ID-NO                  PIC X(6).            GA2IPGM 
00182          10  FILLER                          PIC X(4).            GA2IPGM 
00183      05  FILLER                              PIC X(78).           GA2IPGM 
00184      05  MAP-PROVIDER-TYP-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED   GA2IPGM 
00185          BY MAP-IDX1.                                             GA2IPGM 
00186        10  MAP-PROVIDER-TYP-ARGUMENT-COL  OCCURS 3 TIMES INDEXED  GA2IPGM 
00187            BY MAP-IDX2.                                           GA2IPGM 
00188          15  MAP-PROVIDER-TYP-ARGUMENT-LEN   PIC S9(4) COMP SYNC. GA2IPGM 
00189          15  MAP-PROVIDER-TYP-ARGUMENT-ATTR  PIC X.               GA2IPGM 
00190          15  MAP-PROVIDER-TYP-ARGUMENT       PIC X(2).            GA2IPGM 
00191      SKIP3                                                        GA2IPGM 
00192  01  FILLER.                                                      GA2IPGM 
00193 ****************************************************************  GA2IPGM 
00194 **   THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.      GA2IPGM 
00195 ****************************************************************  GA2IPGM 
00196      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +14.  GA2IPGM 
00197      05  WS-MAP-COL                  PIC S999 COMP-3  VALUE +3.   GA2IPGM 
00198 /                                                                 GA2IPGM 
00199 ** ALTERNATIVE WORKFILE KEYS **                                   GA2IPGM 
00200  01  FILLER                      PIC X(32)  VALUE                 GA2IPGM 
00201      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2IPGM 
00202  01  WS-ALT-WORKFILE-KEYS.                                        GA2IPGM 
00203  COPY GCWRKKEY.                                                   GA2IPGM 
00204 /                                                                 GA2IPGM 
00205                                                                   GA2IPGM 
00206 ** DATE FORMATTING AREA **                                        GA2IPGM 
00207  01  HGADATES-COMMAREA.                                           GA2IPGM 
00208  COPY HGCDAT01.                                                   GA2IPGM 
00209                                                                   GA2IPGM 
00210 ** WORKFIELDS **                                                  GA2IPGM 
00211  01  FILLER                           PIC X(16)                   GA2IPGM 
00212              VALUE '** WORKFIELDS **'.                            GA2IPGM 
00213  01  WS-WORK-FIELDS.                                              GA2IPGM 
00214      05  WS-HEX-00                    PIC X.                      GA2IPGM 
00215      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2IPGM 
00216      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2IPGM 
00217        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2IPGM 
00218 ****************************************************************  GA2IPGM 
00219 ** THIS MUST BE CHANGED TO MATCH ONE OCCURENCE OF ENTRY IN TABULARGA2IPGM 
00220 ** RECORD.                                                        GA2IPGM 
00221 ****************************************************************  GA2IPGM 
00222      05  WS-QUOTE-COMP            PIC X   VALUE QUOTE.            GA2IPGM 
00223      05  WS-SAVED-FIELDS.                                         GA2IPGM 
00224        10  WS-SAVED-PROVIDER-TYP-ARGUMENT  PIC X(2).              GA2IPGM 
00225 ****************************************************************  GA2IPGM 
00226 ****************************************************************  GA2IPGM 
00227 ** THIS IS THE AREA IN WHICH THE SORTING OF NEW ENTRIES HAPPENS.  GA2IPGM 
00228 ** NAMES MUST CHANGE ACCORDINGLY, AND ONE MORE OCCURENCE IS       GA2IPGM 
00229 ** PROVIDED THAN IS FOUND ON THE SCREEN, THIS IS FOR THE TRAILER. GA2IPGM 
00230 ****************************************************************  GA2IPGM 
00231      05  WS-PROV-TYP-ARGUMENT-ENTRY  OCCURS 43 TIMES INDEXED BY   GA2IPGM 
00232          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2IPGM 
00233        10  WS-PROVIDER-TYP-ARGUMENT       PIC X(2).               GA2IPGM 
00234 /                                                                 GA2IPGM 
00235 *** SWITCHES ***                                                  GA2IPGM 
00236  01  FILLER                           PIC X(14)                   GA2IPGM 
00237              VALUE '** SWITCHES **'.                              GA2IPGM 
00238  01  WS-SWITCHES.                                                 GA2IPGM 
00239      05  WS-ERROR-SW                  PIC X.                      GA2IPGM 
00240                                                                   GA2IPGM 
00241 ** TITLE LINES **                                                 GA2IPGM 
00242  01  WS-TITLE-LINES.                                              GA2IPGM 
00243      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA2IPGM 
00244          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA2IPGM 
00245      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA2IPGM 
00246          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA2IPGM 
00247      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA2IPGM 
00248          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA2IPGM 
00249                                                                   GA2IPGM 
00250 *** RECORD LENGTHS ***                                            GA2IPGM 
00251  01  FILLER                           PIC X(20)                   GA2IPGM 
00252              VALUE '** RECORD LENGTHS **'.                        GA2IPGM 
00253  01  WS-RECORD-LENGTHS.                                           GA2IPGM 
00254     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA2IPGM 
00255     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA2IPGM 
00256     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2IPGM 
00257     05 WS-GCVI-COMMAREA-LEN           PIC S9(4) COMP   VALUE +19. GA2IPGM 
00258     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2IPGM 
00259 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA2IPGM 
00260  01  FILLER.                                                      GA2IPGM 
00261      COPY GCCDRLEN.                                               GA2IPGM 
00262                                                                   GA2IPGM 
00263 ** ATTRIBUTES **                                                  GA2IPGM 
00264  COPY DFHBMSCA.                                                   GA2IPGM 
00265      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2IPGM 
00266 /                                                                 GA2IPGM 
00267 ** ATTENTION IDENTIFIERS **                                       GA2IPGM 
00268  COPY DFHAID.                                                     GA2IPGM 
00269 /                                                                 GA2IPGM 
00270  01  GCVIOPGMS-PARM.                                              GA2IPGM 
00271  COPY GCVINTR2.                                                   GA2IPGM 
00272      SKIP3                                                        GA2IPGM 
00273      SKIP3                                                        GA2IPGM 
00274  01  WS-END                          PIC X(16)  VALUE             GA2IPGM 
00275      '*** W/S ENDS ***'.                                          GA2IPGM 
00276 /                                                                 GA2IPGM 
00277  LINKAGE SECTION.                                                 GA2IPGM 
00278  01  DFHCOMMAREA.                                                 GA2IPGM 
00279  COPY G2ALCKEC.                                                   GA2IPGM 
00280  COPY GACDACWA.                                                   GA2IPGM 
00281 *    05  INCOMING-COMMAREA-PNTR   USAGE IS POINTER.               GA2IPGM 
00282      05  GAS1UPD-PASSED-AREA.                                     GA2IPGM 
00283          07  LVL2-B-SW           PIC X.                           GA2IPGM 
00284          07  LVL2-F-SW           PIC X.                           GA2IPGM 
00285          07  LVL2-G-SW           PIC X.                           GA2IPGM 
00286          07  INTR-TAB-PGM-ID     PIC X(8).                        GA2IPGM 
00287          07  FILLER              PIC X(9).                        GA2IPGM 
00288      05  DELADD-OPTION           PIC X(7).                        GA2IPGM 
00289                                                                   GA2IPGM 
00290                                                                   GA2IPGM 
00291 *01  GCA-COMMAREA.                                                GA2IPGM 
00292 *COPY G2ALCKEC.                                                   GA2IPGM 
00293 /                                                                 GA2IPGM 
00294 ** I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD **         GA2IPGM 
00295  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2IPGM 
00296  COPY GCIOPRM1.                                                   GA2IPGM 
00297 /                                                                 GA2IPGM 
00298  COPY GCWRKDCC.                                                   GA2IPGM 
00299 /                                                                 GA2IPGM 
00300  COPY GCTIPGTC.                                                   GA2IPGM 
00301 /                                                                 GA2IPGM 
00302 ****************************************************************  GA2IPGM 
00303 ** THIS AREA MUST BE CHANGED TO MATCH THE TABLE FROM THE TABULAR  GA2IPGM 
00304 ** RECORD; FIELD NAMES, TYPES, AND THE NUMBER OF OCCURENCES.      GA2IPGM 
00305 ****************************************************************  GA2IPGM 
00306  01  COPY-TABULAR-TABLE-AREA.                                     GA2IPGM 
00307      05  COPY-TABULAR-TABLE  OCCURS 1979 TIMES INDEXED BY         GA2IPGM 
00308            COPY-IDX.                                              GA2IPGM 
00309        10  COPY-PROVIDER-TYP-ARGUMENT   PIC X(2).                 GA2IPGM 
00310 /                                                                 GA2IPGM 
00311 ** IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD **             GA2IPGM 
00312  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2IPGM 
00313  COPY GCIOPRM2.                                                   GA2IPGM 
00314 /                                                                 GA2IPGM 
00315  COPY GCWRKDC2.                                                   GA2IPGM 
00316 /                                                                 GA2IPGM 
00317  COPY GCTABMC.                                                    GA2IPGM 
00318 /                                                                 GA2IPGM 
00319                                                                   GA2IPGM 
00320  PROCEDURE DIVISION.                                              GA2IPGM 
00321                                                                   GA2IPGM 
00322 ******************************************************************GA2IPGM 
00323 **                H O U S E K E E P I N G                         GA2IPGM 
00324 **                                                                GA2IPGM 
00325 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2IPGM 
00326 **                                                                GA2IPGM 
00327 ******************************************************************GA2IPGM 
00328  0000-HOUSEKEEPING  SECTION.                                      GA2IPGM 
00329                                                                   GA2IPGM 
00330      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2IPGM 
00331                                                                   GA2IPGM 
00332      IF EIBAID  =  DFHCLEAR                                       GA2IPGM 
00333          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2IPGM 
00334                         ERASE                                     GA2IPGM 
00335          END-EXEC                                                 GA2IPGM 
00336          EXEC CICS RETURN                                         GA2IPGM 
00337          END-EXEC.                                                GA2IPGM 
00338                                                                   GA2IPGM 
00339      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2IPGM 
00340                END-EXEC.                                          GA2IPGM 
00341  0000-EXIT.                                                       GA2IPGM 
00342        EXIT.                                                      GA2IPGM 
00343 /*****************************************************************GA2IPGM 
00344 **                     M A I N L I N E                            GA2IPGM 
00345 **                                                                GA2IPGM 
00346 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2IPGM 
00347 **  TAKEN BY THE OPERATOR.                                        GA2IPGM 
00348 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2IPGM 
00349 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2IPGM 
00350 **     ADDITIONS FROM.                                            GA2IPGM 
00351 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2IPGM 
00352 **     KEY PF12 OR PF24.                                          GA2IPGM 
00353 **  3. RECEIVE THE SCREEN.                                        GA2IPGM 
00354 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2IPGM 
00355 **     MENU.                                                      GA2IPGM 
00356 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2IPGM 
00357 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2IPGM 
00358 **     (RETURN) TO THE DELETE PROGRAM (GA1IPGM).                  GA2IPGM 
00359 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2IPGM 
00360 **     (RETURN) TO THE PREVIOUS MENU.                             GA2IPGM 
00361 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2IPGM 
00362 **     NORMAL ADD LOGIC, EXCEPT BYPASS EMPTY VALIDATION TABLE     GA2IPGM 
00363 **     CONDITION FOR PROVIDER TYPE ARGUMENT FIELD.                GA2IPGM 
00364 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2IPGM 
00365 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2IPGM 
00366 **                                                                GA2IPGM 
00367 ******************************************************************GA2IPGM 
00368  1000-MAIN-LINE  SECTION.                                         GA2IPGM 
00369                                                                   GA2IPGM 
00370      MOVE '1000'  TO  WS-PARA-ID.                                 GA2IPGM 
00371      IF EIBTRNID  NOT =  'GA2I'                                   GA2IPGM 
00372         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2IPGM 
00373         GO TO 1099-RETURN.                                        GA2IPGM 
00374                                                                   GA2IPGM 
00375      EXEC CICS RECEIVE   MAP('GA2II01') MAPSET('GA2ISET')         GA2IPGM 
00376         INTO(GA2II01I) END-EXEC.                                  GA2IPGM 
00377                                                                   GA2IPGM 
00378      IF SCRNIDNI  NOT =  '002I00'                                 GA2IPGM 
00379         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2IPGM 
00380                                                                   GA2IPGM 
00381      IF EIBAID  =  DFHENTER                                       GA2IPGM 
00382         PERFORM 2000-ADD-PROCESSING                               GA2IPGM 
00383         GO TO 1099-RETURN.                                        GA2IPGM 
00384                                                                   GA2IPGM 
00385      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2IPGM 
00386         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2IPGM 
00387                                                                   GA2IPGM 
00388      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2IPGM 
00389         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2IPGM 
00390                                                                   GA2IPGM 
00391      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2IPGM 
00392         PERFORM 2000-ADD-PROCESSING                               GA2IPGM 
00393         GO TO 1099-RETURN.                                        GA2IPGM 
00394                                                                   GA2IPGM 
00395      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2IPGM 
00396      MOVE -1  TO                                                  GA2IPGM 
00397         MAP-PROVIDER-TYP-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).       GA2IPGM 
00398      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2IPGM 
00399 -    ' THIS PROGRAM ***'  TO  ERRMSGO.                            GA2IPGM 
00400      EXEC CICS SEND   MAP('GA2II01') MAPSET('GA2ISET') DATAONLY   GA2IPGM 
00401         FROM(GA2II01O) CURSOR END-EXEC.                           GA2IPGM 
00402                                                                   GA2IPGM 
00403  1099-RETURN.                                                     GA2IPGM 
00404      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2IPGM 
00405         (DELADD-OPTION = 'GAS1UPD') OR                            GA2IPGM 
00406         (DELADD-OPTION = 'GAS2UPD') OR                            GA2IPGM 
00407         (DELADD-OPTION = 'GAS3UPD') OR                            GA2IPGM 
00408         (DELADD-OPTION = 'GAS4UPD') OR                            GA2IPGM 
00409         (DELADD-OPTION = 'GAS5UPD')                               GA2IPGM 
00410          EXEC CICS RETURN END-EXEC                                GA2IPGM 
00411      ELSE                                                         GA2IPGM 
00412          EXEC CICS RETURN TRANSID('GA2I')                         GA2IPGM 
00413                    COMMAREA(DFHCOMMAREA)                          GA2IPGM 
00414                    LENGTH  (EIBCALEN)                             GA2IPGM 
00415                    END-EXEC.                                      GA2IPGM 
00416                                                                   GA2IPGM 
00417      GOBACK.                                                      GA2IPGM 
00418  1099-EXIT.                                                       GA2IPGM 
00419        EXIT.                                                      GA2IPGM 
00420 /*****************************************************************GA2IPGM 
00421 **               A D D   P R O C E S S I N G                      GA2IPGM 
00422 **                                                                GA2IPGM 
00423 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2IPGM 
00424 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2IPGM.            GA2IPGM 
00425 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2IPGM 
00426 **  2. DETERMINE IF ANY VALUE WERE ENTERED FOR THIS LINE.   IF NOTGA2IPGM 
00427 **     SKIP TO THE NEXT LINE.                                     GA2IPGM 
00428 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2IPGM 
00429 **     WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2IPGM 
00430 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2IPGM 
00431 **     DATA.                                                      GA2IPGM 
00432 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2IPGM 
00433 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2IPGM 
00434 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2IPGM 
00435 **     ORDER, FIELD BY FIELD.                                     GA2IPGM 
00436 **  6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2IPGM 
00437 **     MADE.                                                      GA2IPGM 
00438 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2IPGM 
00439 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2IPGM 
00440 **     BACK INTO THE RECORD.                                      GA2IPGM 
00441 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2IPGM 
00442 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2IPGM 
00443 **                                                                GA2IPGM 
00444 ******************************************************************GA2IPGM 
00445  2000-ADD-PROCESSING SECTION.                                     GA2IPGM 
00446                                                                   GA2IPGM 
00447      MOVE '2000'  TO  WS-PARA-ID.                                 GA2IPGM 
00448      MOVE 'N'     TO  WS-ERROR-SW.                                GA2IPGM 
00449      MOVE 'Y'     TO  GCVI2-TABLE-SW.                             GA2IPGM 
00450      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2IPGM 
00451      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2IPGM 
00452                                                                   GA2IPGM 
00453      MOVE '2005'  TO  WS-PARA-ID.                                 GA2IPGM 
00454  2005-RESET-ALL-ATTRIBUTES.                                       GA2IPGM 
00455      MOVE DFHBMUNF  TO                                            GA2IPGM 
00456         MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2).      GA2IPGM 
00457      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2IPGM 
00458         SET MAP-IDX1  UP BY  1                                    GA2IPGM 
00459         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2IPGM 
00460      IF MAP-IDX2  <  WS-MAP-COL                                   GA2IPGM 
00461         SET MAP-IDX1  TO  1                                       GA2IPGM 
00462         SET MAP-IDX2  UP BY  1                                    GA2IPGM 
00463         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2IPGM 
00464                                                                   GA2IPGM 
00465      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2IPGM 
00466      MOVE '2010'  TO  WS-PARA-ID.                                 GA2IPGM 
00467  2010-VALIDATE-ADD-ENTRIES.                                       GA2IPGM 
00468      IF MAP-PROVIDER-TYP-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)        GA2IPGM 
00469            =  ZERO                                                GA2IPGM 
00470         IF MAP-IDX1  <  WS-MAP-ROW                                GA2IPGM 
00471            SET MAP-IDX1  UP BY  1                                 GA2IPGM 
00472            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2IPGM 
00473         ELSE                                                      GA2IPGM 
00474            IF MAP-IDX2  <  WS-MAP-COL                             GA2IPGM 
00475               SET MAP-IDX1  TO  1                                 GA2IPGM 
00476               SET MAP-IDX2  UP BY  1                              GA2IPGM 
00477               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2IPGM 
00478            ELSE                                                   GA2IPGM 
00479               GO TO 2020-CHECK-FOR-ERRORS.                        GA2IPGM 
00480                                                                   GA2IPGM 
00481      IF MAP-PROVIDER-TYP-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2) = ZERO GA2IPGM 
00482         MOVE DFHBMUBF  TO                                         GA2IPGM 
00483            MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA2IPGM 
00484         MOVE '??????' TO                                          GA2IPGM 
00485            MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)         GA2IPGM 
00486         IF WS-ERROR-SW  NOT =  'Y'                                GA2IPGM 
00487            MOVE 'Y'  TO  WS-ERROR-SW                              GA2IPGM 
00488            MOVE -1   TO                                           GA2IPGM 
00489               MAP-PROVIDER-TYP-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)  GA2IPGM 
00490            MOVE ' *** PROVIDER TYPE ARGUMENT IS INVALID ***'      GA2IPGM 
00491               TO  ERRMSGO                                         GA2IPGM 
00492         ELSE                                                      GA2IPGM 
00493            NEXT SENTENCE                                          GA2IPGM 
00494      ELSE                                                         GA2IPGM 
00495         MOVE MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)  TO   GA2IPGM 
00496            WS-SAVED-PROVIDER-TYP-ARGUMENT                         GA2IPGM 
00497         INSPECT WS-SAVED-PROVIDER-TYP-ARGUMENT                    GA2IPGM 
00498                 REPLACING  ALL  WS-QUOTE-COMP BY '\
00499         INSPECT WS-SAVED-PROVIDER-TYP-ARGUMENT                    GA2IPGM 
00500               REPLACING   CHARACTERS BY  WS-QUOTE-COMP            GA2IPGM 
00501         IF WS-SAVED-PROVIDER-TYP-ARGUMENT NOT = QUOTES            GA2IPGM 
00502            MOVE DFHBMUBF  TO                                      GA2IPGM 
00503               MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2) GA2IPGM 
00504            IF WS-ERROR-SW NOT = 'Y'                               GA2IPGM 
00505                MOVE 'Y' TO WS-ERROR-SW                            GA2IPGM 
00506                MOVE -1 TO                                         GA2IPGM 
00507                   MAP-PROVIDER-TYP-ARGUMENT-LEN                   GA2IPGM 
00508                      (MAP-IDX1, MAP-IDX2)                         GA2IPGM 
00509                MOVE ' *** PROVIDER TYPE ARGUMENT IS INVALID ***'  GA2IPGM 
00510                   TO ERRMSGO.                                     GA2IPGM 
00511                                                                   GA2IPGM 
00512      IF MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA2IPGM 
00513            NOT  =  DFHBMUBF                                       GA2IPGM 
00514         ADD 1  TO  WS-ADD-COUNT                                   GA2IPGM 
00515         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2IPGM 
00516         MOVE MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2) TO    GA2IPGM 
00517            WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX).                GA2IPGM 
00518                                                                   GA2IPGM 
00519      IF MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA2IPGM 
00520          NOT = DFHBMUBF                                           GA2IPGM 
00521         MOVE  'BPVE01' TO GCVI2-FIELDS-KEY-ID                     GA2IPGM 
00522         MOVE  ZEROES   TO GCVI2-RETURN-CODE                       GA2IPGM 
00523         MOVE MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)       GA2IPGM 
00524                  TO GCVI2-VALUE-LEN-2                             GA2IPGM 
00525         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2IPGM 
00526                        COMMAREA(GCVIOPGMS-PARM)                   GA2IPGM 
00527                        LENGTH(WS-GCVI-COMMAREA-LEN) END-EXEC      GA2IPGM 
00528         IF GCVI2-VALUE-NOT-FOUND                                  GA2IPGM 
00529            IF WS-ERROR-SW NOT = 'Y'                               GA2IPGM 
00530               MOVE DFHBMUBF  TO                                   GA2IPGM 
00531                 MAP-PROVIDER-TYP-ARGUMENT-ATTR(MAP-IDX1, MAP-IDX2)GA2IPGM 
00532               MOVE -1  TO                                         GA2IPGM 
00533                  MAP-PROVIDER-TYP-ARGUMENT-LEN(MAP-IDX1, MAP-IDX2)GA2IPGM 
00534               MOVE '*** PROVIDER TYPE CODE INVALID ***'  TO       GA2IPGM 
00535                                                         ERRMSGO   GA2IPGM 
00536               MOVE 'Y' TO WS-ERROR-SW                             GA2IPGM 
00537            ELSE                                                   GA2IPGM 
00538               MOVE DFHBMUBF  TO                                   GA2IPGM 
00539                 MAP-PROVIDER-TYP-ARGUMENT-ATTR(MAP-IDX1, MAP-IDX2)GA2IPGM 
00540         ELSE                                                      GA2IPGM 
00541            IF GCVI2-VALUE-NOT-LOADED                              GA2IPGM 
00542               IF EIBAID  =  DFHPF4 OR  =  DFHPF16                 GA2IPGM 
00543                  NEXT SENTENCE                                    GA2IPGM 
00544               ELSE                                                GA2IPGM 
00545                  MOVE DFHBMUBF  TO                                GA2IPGM 
00546                 MAP-PROVIDER-TYP-ARGUMENT-ATTR(MAP-IDX1, MAP-IDX2)GA2IPGM 
00547                  IF WS-ERROR-SW  NOT =  'Y'                       GA2IPGM 
00548                     MOVE 'Y'  TO  WS-ERROR-SW                     GA2IPGM 
00549                     MOVE 'N' TO GCVI2-TABLE-SW                    GA2IPGM 
00550                     MOVE -1  TO                                   GA2IPGM 
00551                  MAP-PROVIDER-TYP-ARGUMENT-LEN(MAP-IDX1, MAP-IDX2)GA2IPGM 
00552               MOVE 'EDIT TABLE EMPTY - DATA NOT VALIDATED -  PRESSGA2IPGM 
00553 -              ' PF4 / PF16 TO CONTINUE' TO  ERRMSGO.             GA2IPGM 
00554                                                                   GA2IPGM 
00555      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2IPGM 
00556         SET MAP-IDX1  UP BY  1                                    GA2IPGM 
00557         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2IPGM 
00558      IF MAP-IDX2  <  WS-MAP-COL                                   GA2IPGM 
00559         SET MAP-IDX1  TO  1                                       GA2IPGM 
00560         SET MAP-IDX2  UP BY  1                                    GA2IPGM 
00561         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2IPGM 
00562                                                                   GA2IPGM 
00563  2020-CHECK-FOR-ERRORS.                                           GA2IPGM 
00564      MOVE '2020'  TO  WS-PARA-ID.                                 GA2IPGM 
00565                                                                   GA2IPGM 
00566      IF INCEXCI  NOT =  'I' AND  NOT =  'E'                       GA2IPGM 
00567         MOVE -1  TO  INCEXCL                                      GA2IPGM 
00568         MOVE 'Y'  TO  WS-ERROR-SW                                 GA2IPGM 
00569         MOVE '*** INCLUDE/EXCLUDE FIELD VALUE NOT VALID ***'  TO  GA2IPGM 
00570            ERRMSGO.                                               GA2IPGM 
00571                                                                   GA2IPGM 
00572      IF WS-ERROR-SW  =  'Y' OR GCVI2-TABLE-SW = 'N'               GA2IPGM 
00573         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2IPGM 
00574            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2IPGM 
00575            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2IPGM 
00576            INCEXCO                                                GA2IPGM 
00577         MOVE '2100'  TO  WS-PARA-ID                               GA2IPGM 
00578         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2IPGM 
00579            VARYING MAP-IDX2 FROM  1  BY  1                        GA2IPGM 
00580               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2IPGM 
00581            AFTER MAP-IDX1 FROM  1  BY  1                          GA2IPGM 
00582               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2IPGM 
00583         EXEC CICS SEND   MAP('GA2II01') MAPSET('GA2ISET')         GA2IPGM 
00584            DATAONLY FROM(GA2II01O) CURSOR END-EXEC                GA2IPGM 
00585         GO TO 2099-EXIT.                                          GA2IPGM 
00586                                                                   GA2IPGM 
00587      IF WS-ADD-COUNT  NOT >  ZERO AND                             GA2IPGM 
00588         INCEXCI  =  INEXDRKI                                      GA2IPGM 
00589         MOVE '*** NO ADD ENTRY FOUND OR INC/EXC FIELD CHANGE ***' GA2IPGM 
00590            TO  ERRMSGO                                            GA2IPGM 
00591         MOVE -1  TO  INCEXCL                                      GA2IPGM 
00592         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2IPGM 
00593            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2IPGM 
00594            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2IPGM 
00595            INCEXCO                                                GA2IPGM 
00596         EXEC CICS SEND   MAP('GA2II01') MAPSET('GA2ISET')         GA2IPGM 
00597            DATAONLY FROM(GA2II01O) CURSOR END-EXEC                GA2IPGM 
00598         GO TO 2099-EXIT.                                          GA2IPGM 
00599                                                                   GA2IPGM 
00600  2025-CONTINUE-PROCESSING.                                        GA2IPGM 
00601                                                                   GA2IPGM 
00602      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2IPGM 
00603               GC-GCIOPARM-LEN                 +                   GA2IPGM 
00604               GC-WORKFILE-KEY-LEN             +                   GA2IPGM 
00605               GC-GCTABULR-IPGT-FIXED-LEN      +                   GA2IPGM 
00606              (GC-GCTABULR-IPGT-VARY-LEN       *                   GA2IPGM 
00607               GC-GCTABULR-IPGT-VARY-MAX-OCUR)                     GA2IPGM 
00608                                                                   GA2IPGM 
00609                                                                   GA2IPGM 
00610      EXEC CICS                                                    GA2IPGM 
00611         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2IPGM 
00612         INITIMG(WS-HEX-00)                                        GA2IPGM 
00613         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2IPGM 
00614      END-EXEC.                                                    GA2IPGM 
00615                                                                   GA2IPGM 
00616      IF  FRMNUIDI  =  'GS3A'                                      GA2IPGM 
00617         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2IPGM 
00618         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2IPGM 
00619         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA2IPGM 
00620         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2IPGM 
00621 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2IPGM 
00622         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2IPGM 
00623 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2IPGM 
00624         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2IPGM 
00625         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2IPGM 
00626         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2IPGM 
00627                          GCIO-WRK-PROVIDER-CONTROL                GA2IPGM 
00628         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2IPGM 
00629         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2IPGM 
00630                                                                   GA2IPGM 
00631      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2IPGM 
00632         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2IPGM 
00633         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2IPGM 
00634         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2IPGM 
00635         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2IPGM 
00636 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2IPGM 
00637         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2IPGM 
00638 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2IPGM 
00639         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2IPGM 
00640         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2IPGM 
00641         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2IPGM 
00642         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2IPGM 
00643         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2IPGM 
00644         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2IPGM 
00645                                                                   GA2IPGM 
00646      IF  FRMNUIDI  =  'GC8A'                                      GA2IPGM 
00647         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2IPGM 
00648         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2IPGM 
00649         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA2IPGM 
00650         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2IPGM 
00651 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2IPGM 
00652         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2IPGM 
00653 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2IPGM 
00654         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2IPGM 
00655         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2IPGM 
00656         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2IPGM 
00657         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2IPGM 
00658         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2IPGM 
00659         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2IPGM 
00660                                                                   GA2IPGM 
00661      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2IPGM 
00662      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2IPGM 
00663      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2IPGM 
00664      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2IPGM 
00665      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA2IPGM 
00666      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2IPGM 
00667      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2IPGM 
00668                                                                   GA2IPGM 
00669      MOVE  GC-GCTABULR-IPGT-VARY-MAX-OCUR                         GA2IPGM 
00670            TO  GX3-ENTRY-COUNT.                                   GA2IPGM 
00671      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2IPGM 
00672                                                                   GA2IPGM 
00673      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2IPGM 
00674         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2IPGM 
00675         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2IPGM 
00676                                                                   GA2IPGM 
00677      IF  NOT GCIO-GOOD-RETURN                                     GA2IPGM 
00678         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2IPGM 
00679 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2IPGM 
00680         MOVE '2IF1'  TO  WS-ABEND-CODE                            GA2IPGM 
00681         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
00682                                                                   GA2IPGM 
00683      MOVE INCEXCI  TO  INEXDRKO,  GX3-INCLUDE-EXCLUDE-IND.        GA2IPGM 
00684      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2IPGM 
00685         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2IPGM 
00686                                                                   GA2IPGM 
00687       SET WS-SORT-IDX  TO  1.                                     GA2IPGM 
00688       SET WS-SORT-IDX2  TO  2.                                    GA2IPGM 
00689       MOVE '2030'  TO  WS-PARA-ID.                                GA2IPGM 
00690                                                                   GA2IPGM 
00691  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2IPGM 
00692      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2IPGM 
00693         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2IPGM 
00694                                                                   GA2IPGM 
00695      IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) <                  GA2IPGM 
00696         WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2)                   GA2IPGM 
00697         SET WS-SORT-IDX2  UP BY  1                                GA2IPGM 
00698         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2IPGM 
00699      ELSE                                                         GA2IPGM 
00700         IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) >               GA2IPGM 
00701            WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2)                GA2IPGM 
00702            MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) TO         GA2IPGM 
00703               WS-SAVED-PROVIDER-TYP-ARGUMENT                      GA2IPGM 
00704            MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2) TO        GA2IPGM 
00705               WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX)              GA2IPGM 
00706            MOVE WS-SAVED-PROVIDER-TYP-ARGUMENT  TO                GA2IPGM 
00707               WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2)             GA2IPGM 
00708            SET WS-SORT-IDX2  UP BY  1                             GA2IPGM 
00709            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2IPGM 
00710                                                                   GA2IPGM 
00711      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2IPGM 
00712      MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX3) TO              GA2IPGM 
00713         WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2).                  GA2IPGM 
00714      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2IPGM 
00715      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2IPGM 
00716                                                                   GA2IPGM 
00717  2040-ARE-WE-DONE-WITH-SORT.                                      GA2IPGM 
00718      MOVE '2040'  TO  WS-PARA-ID.                                 GA2IPGM 
00719      SET WS-SORT-IDX  UP BY  1.                                   GA2IPGM 
00720      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2IPGM 
00721         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2IPGM 
00722         SET WS-SORT-IDX2  UP BY  1                                GA2IPGM 
00723         MOVE '2030'  TO  WS-PARA-ID                               GA2IPGM 
00724         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2IPGM 
00725      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2IPGM 
00726      MOVE HIGH-VALUES TO WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX).GA2IPGM 
00727      MOVE GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT.                   GA2IPGM 
00728                                                                   GA2IPGM 
00729      COMPUTE  WS-COPY-LENGTH  =                                   GA2IPGM 
00730                GX3-ENTRY-COUNT  *  GC-GCTABULR-IPGT-VARY-LEN.     GA2IPGM 
00731                                                                   GA2IPGM 
00732      EXEC CICS                                                    GA2IPGM 
00733         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA2IPGM 
00734         LENGTH      (WS-COPY-LENGTH)                              GA2IPGM 
00735         INITIMG     (WS-HEX-00)                                   GA2IPGM 
00736      END-EXEC.                                                    GA2IPGM 
00737                                                                   GA2IPGM 
00738      MOVE GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT.                   GA2IPGM 
00739      SET COPY-IDX,  GX3-INDEX  TO  1.                             GA2IPGM 
00740                                                                   GA2IPGM 
00741      MOVE '2050'  TO  WS-PARA-ID.                                 GA2IPGM 
00742  2050-MAKE-A-COPY-OF-RECORD.                                      GA2IPGM 
00743      IF GX3-INDEX  NOT >  GX3-ENTRY-COUNT                         GA2IPGM 
00744         MOVE GX3-ENTRY (GX3-INDEX)  TO                            GA2IPGM 
00745            COPY-TABULAR-TABLE (COPY-IDX)                          GA2IPGM 
00746         SET COPY-IDX, GX3-INDEX  UP BY  1                         GA2IPGM 
00747         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2IPGM 
00748                                                                   GA2IPGM 
00749      IF WS-ADD-COUNT  +  GX3-ENTRY-COUNT  >                       GA2IPGM 
00750                          GC-GCTABULR-IPGT-VARY-MAX-OCUR           GA2IPGM 
00751         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2IPGM 
00752 -    'EASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                 GA2IPGM 
00753         MOVE '2IL1'  TO  WS-ABEND-CODE                            GA2IPGM 
00754         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
00755                                                                   GA2IPGM 
00756      SET WS-SORT-IDX,  COPY-IDX,  GX3-INDEX  TO  1.               GA2IPGM 
00757                                                                   GA2IPGM 
00758      MOVE '2060'  TO  WS-PARA-ID.                                 GA2IPGM 
00759  2060-MERGE-IN-NEW-ENTRIES.                                       GA2IPGM 
00760      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2IPGM 
00761         SET GX3-INDEX  DOWN BY  1                                 GA2IPGM 
00762         SET GX3-ENTRY-COUNT  TO  GX3-INDEX                        GA2IPGM 
00763         MOVE GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT                 GA2IPGM 
00764         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2IPGM 
00765                                                                   GA2IPGM 
00766      IF WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX)                  GA2IPGM 
00767               =  HIGH-VALUES  AND                                 GA2IPGM 
00768         COPY-TABULAR-TABLE (COPY-IDX)  NOT =  HIGH-VALUES         GA2IPGM 
00769         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2IPGM 
00770                                                                   GA2IPGM 
00771      IF WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX)                  GA2IPGM 
00772               NOT =  HIGH-VALUES AND                              GA2IPGM 
00773         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2IPGM 
00774         GO TO 2080-INSERT-NEW-ENTRY.                              GA2IPGM 
00775                                                                   GA2IPGM 
00776      IF WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX)                  GA2IPGM 
00777               =  HIGH-VALUES AND                                  GA2IPGM 
00778         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2IPGM 
00779         NEXT SENTENCE                                             GA2IPGM 
00780      ELSE                                                         GA2IPGM 
00781         IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) >               GA2IPGM 
00782            COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)                  GA2IPGM 
00783            GO TO 2070-SAVE-COPIED-ENTRY                           GA2IPGM 
00784         ELSE                                                      GA2IPGM 
00785            IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) <            GA2IPGM 
00786               COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)               GA2IPGM 
00787               GO TO 2080-INSERT-NEW-ENTRY.                        GA2IPGM 
00788                                                                   GA2IPGM 
00789 **   AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2IPGM 
00790 **   OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2IPGM 
00791 **   INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2IPGM 
00792 **   FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2IPGM 
00793                                                                   GA2IPGM 
00794      SET WS-SORT-IDX  UP BY  1.                                   GA2IPGM 
00795                                                                   GA2IPGM 
00796  2070-SAVE-COPIED-ENTRY.                                          GA2IPGM 
00797      MOVE '2070'  TO  WS-PARA-ID.                                 GA2IPGM 
00798      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA2IPGM 
00799         GX3-ENTRY (GX3-INDEX).                                    GA2IPGM 
00800      IF COPY-IDX  NOT >  GX3-ENTRY-COUNT                          GA2IPGM 
00801         SET COPY-IDX  UP BY  1                                    GA2IPGM 
00802         SET GX3-INDEX  UP BY  1                                   GA2IPGM 
00803         MOVE '2060'  TO  WS-PARA-ID                               GA2IPGM 
00804         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2IPGM 
00805      ELSE                                                         GA2IPGM 
00806         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2IPGM 
00807 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2IPGM 
00808         MOVE '2IL2'  TO  WS-ABEND-CODE                            GA2IPGM 
00809         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
00810                                                                   GA2IPGM 
00811  2080-INSERT-NEW-ENTRY.                                           GA2IPGM 
00812      MOVE '2080'  TO  WS-PARA-ID.                                 GA2IPGM 
00813      MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) TO               GA2IPGM 
00814         GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX).                   GA2IPGM 
00815                                                                   GA2IPGM 
00816      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2IPGM 
00817         SET WS-SORT-IDX  UP BY  1                                 GA2IPGM 
00818         SET GX3-INDEX  UP BY  1                                   GA2IPGM 
00819         MOVE '2060'  TO  WS-PARA-ID                               GA2IPGM 
00820         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2IPGM 
00821      ELSE                                                         GA2IPGM 
00822         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2IPGM 
00823 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2IPGM 
00824         MOVE '2IL3'  TO  WS-ABEND-CODE                            GA2IPGM 
00825         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
00826                                                                   GA2IPGM 
00827  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2IPGM 
00828      MOVE '2090'  TO  WS-PARA-ID.                                 GA2IPGM 
00829                                                                   GA2IPGM 
00830 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2IPGM 
00831                                                                   GA2IPGM 
00832      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2IPGM 
00833      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2IPGM 
00834                                                                   GA2IPGM 
00835      COMPUTE  GCIO-RECORD-LENGTH  =                               GA2IPGM 
00836               GC-WORKFILE-KEY-LEN             +                   GA2IPGM 
00837               GC-GCTABULR-IPGT-FIXED-LEN      +                   GA2IPGM 
00838              (GC-GCTABULR-IPGT-VARY-LEN       *  GX3-ENTRY-COUNT).GA2IPGM 
00839                                                                   GA2IPGM 
00840      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2IPGM 
00841            GC-GCIOPARM-LEN   +  GCIO-RECORD-LENGTH.               GA2IPGM 
00842                                                                   GA2IPGM 
00843      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2IPGM 
00844         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2IPGM 
00845         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2IPGM 
00846                                                                   GA2IPGM 
00847      IF NOT GCIO-GOOD-RETURN                                      GA2IPGM 
00848         MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR RECORGA2IPGM 
00849 -    'D.  CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                  GA2IPGM 
00850         MOVE '2IF2'  TO  WS-ABEND-CODE                            GA2IPGM 
00851         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
00852                                                                   GA2IPGM 
00853      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2IPGM 
00854         VARYING MAP-IDX2 FROM 1  BY  1                            GA2IPGM 
00855            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2IPGM 
00856         AFTER MAP-IDX1 FROM 1  BY  1                              GA2IPGM 
00857            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2IPGM 
00858                                                                   GA2IPGM 
00859      EXEC CICS SEND   MAP('GA2II01') MAPSET('GA2ISET') ERASE      GA2IPGM 
00860         FROM(GA2II01O) END-EXEC.                                  GA2IPGM 
00861                                                                   GA2IPGM 
00862  2099-EXIT.   EXIT.                                               GA2IPGM 
00863 /                                                                 GA2IPGM 
00864 ******************************************************************GA2IPGM 
00865 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2IPGM 
00866 **                                                                GA2IPGM 
00867 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2IPGM 
00868 **  ALREADY ON THE OPERATORS SCREEN.                              GA2IPGM 
00869 **                                                                GA2IPGM 
00870 ******************************************************************GA2IPGM 
00871  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2IPGM 
00872                                                                   GA2IPGM 
00873      MOVE LOW-VALUES  TO                                          GA2IPGM 
00874           MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2).         GA2IPGM 
00875                                                                   GA2IPGM 
00876  2199-EXIT.   EXIT.                                               GA2IPGM 
00877 /                                                                 GA2IPGM 
00878 ******************************************************************GA2IPGM 
00879 **          X C T L   T O   D E L   S C R E E N                   GA2IPGM 
00880 **                                                                GA2IPGM 
00881 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2IPGM 
00882 ** DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2IPGM 
00883 ** PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2IPGM 
00884 ** TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2IPGM 
00885 ** THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2IPGM 
00886 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2IPGM 
00887 ******************************************************************GA2IPGM 
00888  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2IPGM 
00889      MOVE '3000'  TO  WS-PARA-ID.                                 GA2IPGM 
00890                                                                   GA2IPGM 
00891      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2IPGM 
00892               GC-GCIOPARM-LEN                 +                   GA2IPGM 
00893               GC-WORKFILE-KEY-LEN             +                   GA2IPGM 
00894               GC-GCTABULR-IPGT-FIXED-LEN      +                   GA2IPGM 
00895              (GC-GCTABULR-IPGT-VARY-LEN       *                   GA2IPGM 
00896               GC-GCTABULR-IPGT-VARY-MAX-OCUR)                     GA2IPGM 
00897                                                                   GA2IPGM 
00898                                                                   GA2IPGM 
00899      EXEC CICS                                                    GA2IPGM 
00900         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2IPGM 
00901         INITIMG(WS-HEX-00)                                        GA2IPGM 
00902         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2IPGM 
00903      END-EXEC.                                                    GA2IPGM 
00904                                                                   GA2IPGM 
00905 *    EXEC CICS                                                    GA2IPGM 
00906 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2IPGM 
00907 *       LENGTH      (WS-COMMUNICATION-KEY-LEN)                    GA2IPGM 
00908 *       INITIMG     (WS-HEX-00)                                   GA2IPGM 
00909 *    END-EXEC.                                                    GA2IPGM 
00910                                                                   GA2IPGM 
00911      IF  FRMNUIDI  =  'GS3A'                                      GA2IPGM 
00912         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2IPGM 
00913         MOVE   'G'   TO  GCIO-WRK-STATUS-CODE                     GA2IPGM 
00914         MOVE   'G4'  TO  GCIO-WRK-RECORD-TYPE                     GA2IPGM 
00915         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2IPGM 
00916         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2IPGM 
00917         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2IPGM 
00918         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2IPGM 
00919         MOVE  SPACES  TO  GCIO-WRK-LINE-OF-BUS                    GA2IPGM 
00920                           GCIO-WRK-PROVIDER-CONTROL               GA2IPGM 
00921         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2IPGM 
00922         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2IPGM 
00923                                                                   GA2IPGM 
00924      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2IPGM 
00925         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2IPGM 
00926         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2IPGM 
00927         MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE                     GA2IPGM 
00928         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2IPGM 
00929         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2IPGM 
00930         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2IPGM 
00931         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2IPGM 
00932         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2IPGM 
00933         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2IPGM 
00934         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2IPGM 
00935         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2IPGM 
00936                                                                   GA2IPGM 
00937      IF  FRMNUIDI  =  'GC8A'                                      GA2IPGM 
00938         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2IPGM 
00939         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2IPGM 
00940         MOVE   'C6'  TO  GCIO-WRK-RECORD-TYPE                     GA2IPGM 
00941         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2IPGM 
00942         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2IPGM 
00943         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2IPGM 
00944         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2IPGM 
00945         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2IPGM 
00946         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2IPGM 
00947         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2IPGM 
00948         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                 GA2IPGM 
00949         MOVE  GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID.          GA2IPGM 
00950                                                                   GA2IPGM 
00951      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2IPGM 
00952      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2IPGM 
00953      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2IPGM 
00954      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2IPGM 
00955      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2IPGM 
00956      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2IPGM 
00957      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2IPGM 
00958      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2IPGM 
00959                                                                   GA2IPGM 
00960      MOVE INCEXCI TO GCA-I-E-INDC.                                GA2IPGM 
00961                                                                   GA2IPGM 
00962      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2IPGM 
00963 *    MOVE SPACES  TO  GCA-EFFECTIVE-DATE.                         GA2IPGM 
00964      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2IPGM 
00965                                                                   GA2IPGM 
00966      SET GCA-RECORD-POINTER                                       GA2IPGM 
00967          TO ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD.              GA2IPGM 
00968                                                                   GA2IPGM 
00969      MOVE  GC-GCTABULR-IPGT-VARY-MAX-OCUR                         GA2IPGM 
00970            TO  GX3-ENTRY-COUNT.                                   GA2IPGM 
00971      MOVE 'RD '  TO  GCIO-FILE-ACCESS-CODE.                       GA2IPGM 
00972                                                                   GA2IPGM 
00973      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2IPGM 
00974         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2IPGM 
00975         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2IPGM 
00976                                                                   GA2IPGM 
00977      IF  NOT GCIO-GOOD-RETURN                                     GA2IPGM 
00978         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR RECORD.GA2IPGM 
00979 -    ' CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                     GA2IPGM 
00980         MOVE '2IF3'  TO  WS-ABEND-CODE                            GA2IPGM 
00981         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
00982                                                                   GA2IPGM 
00983 *    SET  COMMAREA-PNTR   TO                                      GA2IPGM 
00984 *         ADDRESS  OF GCA-COMMAREA.                               GA2IPGM 
00985                                                                   GA2IPGM 
00986 *    EXEC CICS XCTL  PROGRAM('GA1IPGM') COMMAREA(COMMAREA-PNTR)   GA2IPGM 
00987 *       LENGTH(4)  END-EXEC.                                      GA2IPGM 
00988      EXEC CICS XCTL  PROGRAM('GA1IPGM')                           GA2IPGM 
00989                      COMMAREA(DFHCOMMAREA)                        GA2IPGM 
00990                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2IPGM 
00991      END-EXEC.                                                    GA2IPGM 
00992                                                                   GA2IPGM 
00993  3099-EXIT.   EXIT.                                               GA2IPGM 
00994 /                                                                 GA2IPGM 
00995 ***************************************************************** GA2IPGM 
00996 **          D I S P L A Y   F I R S T   S C R E E N               GA2IPGM 
00997 **                                                                GA2IPGM 
00998 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2IPGM 
00999 ** OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2IPGM 
01000 ** THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2IPGM 
01001 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2IPGM 
01002 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2IPGM 
01003 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2IPGM 
01004 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2IPGM 
01005 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2IPGM 
01006 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2IPGM 
01007 ******************************************************************GA2IPGM 
01008  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2IPGM 
01009      MOVE '4000'  TO  WS-PARA-ID.                                 GA2IPGM 
01010                                                                   GA2IPGM 
01011      MOVE LOW-VALUES  TO  GA2II01O.                               GA2IPGM 
01012                                                                   GA2IPGM 
01013      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA2IPGM 
01014         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2IPGM 
01015            TO  ERRMSGO                                            GA2IPGM 
01016         MOVE '2IC1'  TO  WS-ABEND-CODE                            GA2IPGM 
01017         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
01018                                                                   GA2IPGM 
01019 *    SET  ADDRESS OF  GCA-COMMAREA  TO                            GA2IPGM 
01020 *         INCOMING-COMMAREA-PNTR.                                 GA2IPGM 
01021                                                                   GA2IPGM 
01022      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA2IPGM 
01023      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA2IPGM 
01024      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA2IPGM 
01025      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA2IPGM 
01026      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA2IPGM 
01027      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA2IPGM 
01028      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA2IPGM 
01029      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA2IPGM 
01030                                                                   GA2IPGM 
01031      MOVE GCA-I-E-INDC TO INCEXCO,                                GA2IPGM 
01032                        INEXDRKO.                                  GA2IPGM 
01033                                                                   GA2IPGM 
01034      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2IPGM 
01035         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA2IPGM 
01036 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2IPGM 
01037         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2IPGM 
01038         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2IPGM 
01039         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2IPGM 
01040         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2IPGM 
01041         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2IPGM 
01042         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2IPGM 
01043         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2IPGM 
01044         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2IPGM 
01045         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2IPGM 
01046         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2IPGM 
01047         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2IPGM 
01048         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2IPGM 
01049                                                                   GA2IPGM 
01050      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2IPGM 
01051         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA2IPGM 
01052 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2IPGM 
01053         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2IPGM 
01054         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2IPGM 
01055         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2IPGM 
01056         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2IPGM 
01057         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2IPGM 
01058         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2IPGM 
01059         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2IPGM 
01060         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2IPGM 
01061         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2IPGM 
01062         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2IPGM 
01063         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2IPGM 
01064         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2IPGM 
01065         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2IPGM 
01066         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2IPGM 
01067         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2IPGM 
01068         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2IPGM 
01069                                                                   GA2IPGM 
01070      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2IPGM 
01071         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA2IPGM 
01072         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2IPGM 
01073         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2IPGM 
01074         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2IPGM 
01075         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2IPGM 
01076         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2IPGM 
01077         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2IPGM 
01078         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2IPGM 
01079         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2IPGM 
01080         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2IPGM 
01081         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2IPGM 
01082         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2IPGM 
01083         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2IPGM 
01084         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2IPGM 
01085         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2IPGM 
01086         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2IPGM 
01087         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2IPGM 
01088         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2IPGM 
01089         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2IPGM 
01090                                                                   GA2IPGM 
01091      EXEC CICS SEND   MAP('GA2II01') MAPSET('GA2ISET') ERASE      GA2IPGM 
01092         FROM(GA2II01O) END-EXEC.                                  GA2IPGM 
01093                                                                   GA2IPGM 
01094  4099-EXIT.   EXIT.                                               GA2IPGM 
01095 /                                                                 GA2IPGM 
01096 ***************************************************************** GA2IPGM 
01097 **        X C T L   T O   P R E V I O U S   M E N U               GA2IPGM 
01098 **                                                                GA2IPGM 
01099 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2IPGM 
01100 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2IPGM 
01101 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2IPGM 
01102 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2IPGM 
01103 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2IPGM 
01104 ******************************************************************GA2IPGM 
01105  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2IPGM 
01106      MOVE '5000'  TO  WS-PARA-ID.                                 GA2IPGM 
01107                                                                   GA2IPGM 
01108                                                                   GA2IPGM 
01109 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA2IPGM 
01110 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA2IPGM 
01111 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA2IPGM 
01112                                                                   GA2IPGM 
01113      IF  ALTBFNCI  =  'GTM1'                                      GA2IPGM 
01114          EXEC CICS XCTL                                           GA2IPGM 
01115                    PROGRAM('GTM1PGM')                             GA2IPGM 
01116                    END-EXEC.                                      GA2IPGM 
01117                                                                   GA2IPGM 
01118                                                                   GA2IPGM 
01119      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2IPGM 
01120               GC-GCIOPARM-LEN             +                       GA2IPGM 
01121               GC-WORKFILE-KEY-LEN         +                       GA2IPGM 
01122               GC-GCTABULR-ABM-FIXED-LEN   +                       GA2IPGM 
01123              (GC-GCTABULR-ABM-VARY-LEN    *                       GA2IPGM 
01124               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA2IPGM 
01125                                                                   GA2IPGM 
01126      EXEC CICS                                                    GA2IPGM 
01127         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA2IPGM 
01128         INITIMG(WS-HEX-00)                                        GA2IPGM 
01129         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA2IPGM 
01130      END-EXEC.                                                    GA2IPGM 
01131                                                                   GA2IPGM 
01132 *    EXEC CICS                                                    GA2IPGM 
01133 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2IPGM 
01134 *       INITIMG(WS-HEX-00)                                        GA2IPGM 
01135 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2IPGM 
01136 *    END-EXEC.                                                    GA2IPGM 
01137                                                                   GA2IPGM 
01138      IF  FRMNUIDI  =  'GS3A'                                      GA2IPGM 
01139         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2IPGM 
01140         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2IPGM 
01141         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA2IPGM 
01142         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2IPGM 
01143         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2IPGM 
01144         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2IPGM 
01145         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2IPGM 
01146         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS                     GA2IPGM 
01147                          GCIO-WRK-PROVIDER-CONTROL                GA2IPGM 
01148         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2IPGM 
01149         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2IPGM 
01150         MOVE SPACES TO GCA-BEN-PROV-ID                            GA2IPGM 
01151         MOVE ALTABIDI TO  GCIO-WRK-PROVISION-ID                   GA2IPGM 
01152                           GCA-ALL-LEVEL-TAB-ID                    GA2IPGM 
01153         MOVE ALTBSLTI TO  GCIO-WRK-PROVISION-SLOT-NO              GA2IPGM 
01154                           GCA-ALL-LEVEL-TAB-SLOT                  GA2IPGM 
01155         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA2IPGM 
01156                        GCA-INTERNAL-TAB-ID                        GA2IPGM 
01157                        GCA-INTERNAL-TAB-SLOT                      GA2IPGM 
01158         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA2IPGM 
01159                                                                   GA2IPGM 
01160      IF  FRMNUIDI  =  'GC4A'                                      GA2IPGM 
01161         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2IPGM 
01162         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2IPGM 
01163         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2IPGM 
01164         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2IPGM 
01165         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2IPGM 
01166         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2IPGM 
01167         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2IPGM 
01168         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2IPGM 
01169         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2IPGM 
01170         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2IPGM 
01171         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2IPGM 
01172         MOVE SPACES TO GCA-BEN-PROV-ID                            GA2IPGM 
01173         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA2IPGM 
01174                          GCA-ALL-LEVEL-TAB-ID                     GA2IPGM 
01175         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA2IPGM 
01176                          GCA-ALL-LEVEL-TAB-SLOT                   GA2IPGM 
01177         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA2IPGM 
01178                        GCA-INTERNAL-TAB-ID                        GA2IPGM 
01179                        GCA-INTERNAL-TAB-SLOT                      GA2IPGM 
01180         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA2IPGM 
01181                                                                   GA2IPGM 
01182      IF  FRMNUIDI  =  'GC8A'                                      GA2IPGM 
01183         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2IPGM 
01184         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2IPGM 
01185         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA2IPGM 
01186         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2IPGM 
01187         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2IPGM 
01188         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2IPGM 
01189         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2IPGM 
01190         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2IPGM 
01191         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2IPGM 
01192         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2IPGM 
01193         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2IPGM 
01194         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA2IPGM 
01195         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2IPGM 
01196         MOVE ALTABIDI TO GCIO-WRK-TAB-PROVISION-ID                GA2IPGM 
01197                          GCA-ALL-LEVEL-TAB-ID                     GA2IPGM 
01198         MOVE ALTBSLTI TO GCIO-WRK-TAB-PROV-SLOT-NO                GA2IPGM 
01199                          GCA-ALL-LEVEL-TAB-SLOT                   GA2IPGM 
01200         MOVE SPACES TO GCA-INTERNAL-TAB-ID                        GA2IPGM 
01201                        GCA-INTERNAL-TAB-SLOT.                     GA2IPGM 
01202                                                                   GA2IPGM 
01203      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA2IPGM 
01204 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA2IPGM 
01205 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA2IPGM 
01206 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA2IPGM 
01207 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA2IPGM 
01208 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA2IPGM 
01209      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2IPGM 
01210      SET GCA-RECORD-POINTER                                       GA2IPGM 
01211          TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                  GA2IPGM 
01212                                                                   GA2IPGM 
01213      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO                       GA2IPGM 
01214           GAA-ENTRY-COUNT.                                        GA2IPGM 
01215      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2IPGM 
01216                                                                   GA2IPGM 
01217      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2IPGM 
01218         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA2IPGM 
01219         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA2IPGM 
01220                                                                   GA2IPGM 
01221      IF  NOT GCIO2-GOOD-RETURN                                    GA2IPGM 
01222         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2IPGM 
01223 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2IPGM 
01224         MOVE '2IF4'  TO  WS-ABEND-CODE                            GA2IPGM 
01225         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2IPGM 
01226                                                                   GA2IPGM 
01227 *    SET  COMMAREA-PNTR                                           GA2IPGM 
01228 *         TO   ADDRESS  OF  GCA-COMMAREA.                         GA2IPGM 
01229                                                                   GA2IPGM 
01230      IF  ALTBFNCI  =  'GA1B'                                      GA2IPGM 
01231 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA2IPGM 
01232 *          LENGTH(4) END-EXEC.                                    GA2IPGM 
01233         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA2IPGM 
01234                         COMMAREA(DFHCOMMAREA)                     GA2IPGM 
01235                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2IPGM 
01236         END-EXEC.                                                 GA2IPGM 
01237                                                                   GA2IPGM 
01238      IF  ALTBFNCI  =  'GA1C'                                      GA2IPGM 
01239 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA2IPGM 
01240 *          LENGTH(4) END-EXEC.                                    GA2IPGM 
01241         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA2IPGM 
01242                         COMMAREA(DFHCOMMAREA)                     GA2IPGM 
01243                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2IPGM 
01244         END-EXEC.                                                 GA2IPGM 
01245                                                                   GA2IPGM 
01246      IF  ALTBFNCI  =  'GA1D'                                      GA2IPGM 
01247 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA2IPGM 
01248 *          LENGTH(4) END-EXEC.                                    GA2IPGM 
01249         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA2IPGM 
01250                         COMMAREA(DFHCOMMAREA)                     GA2IPGM 
01251                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2IPGM 
01252         END-EXEC.                                                 GA2IPGM 
01253                                                                   GA2IPGM 
01254      IF  ALTBFNCI  =  'GA1E'                                      GA2IPGM 
01255 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA2IPGM 
01256 *          LENGTH(4) END-EXEC.                                    GA2IPGM 
01257         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA2IPGM 
01258                         COMMAREA(DFHCOMMAREA)                     GA2IPGM 
01259                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2IPGM 
01260         END-EXEC.                                                 GA2IPGM 
01261                                                                   GA2IPGM 
01262      IF  ALTBFNCI  =  'GA1P'                                      GA2IPGM 
01263         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA2IPGM 
01264                         COMMAREA(DFHCOMMAREA)                     GA2IPGM 
01265                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2IPGM 
01266         END-EXEC.                                                 GA2IPGM 
01267                                                                   GA2IPGM 
01268  5099-EXIT.                                                       GA2IPGM 
01269      EXIT.                                                        GA2IPGM 
01270 /                                                                 GA2IPGM 
01271 ***************************************************************** GA2IPGM 
01272 **           X C T L   T O   M A I N   M E N U                    GA2IPGM 
01273 **                                                                GA2IPGM 
01274 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2IPGM 
01275 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2IPGM 
01276 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2IPGM 
01277 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2IPGM 
01278 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2IPGM 
01279 ** AND PROGRESS DOWN.                                             GA2IPGM 
01280 ******************************************************************GA2IPGM 
01281  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2IPGM 
01282      MOVE '6000'  TO  WS-PARA-ID.                                 GA2IPGM 
01283      MOVE '2IP1'  TO  WS-ABEND-CODE.                              GA2IPGM 
01284                                                                   GA2IPGM 
01285      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2IPGM 
01286                                                                   GA2IPGM 
01287  6099-EXIT.     EXIT.                                             GA2IPGM 
01288 /*****************************************************************GA2IPGM 
01289 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA2IPGM 
01290 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA2IPGM 
01291 ******************************************************************GA2IPGM 
01292  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA2IPGM 
01293  9800-010.                                                        GA2IPGM 
01294                                                                   GA2IPGM 
01295      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA2IPGM 
01296      MOVE 'M'   TO  HGADATE-FORM1.                                GA2IPGM 
01297      MOVE 'J'   TO  HGADATE-FORM2.                                GA2IPGM 
01298      MOVE ZEROS TO  HGADATE-RETURN                                GA2IPGM 
01299                     HGADATE-AMOUNT.                               GA2IPGM 
01300      EXEC CICS LINK PROGRAM ('HGADATES')                          GA2IPGM 
01301                     COMMAREA(HGADATES-COMMAREA)                   GA2IPGM 
01302                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA2IPGM 
01303                     END-EXEC.                                     GA2IPGM 
01304                                                                   GA2IPGM 
01305  9800-900-900-EXIT.                                               GA2IPGM 
01306      EXIT.                                                        GA2IPGM 
01307 /*****************************************************************GA2IPGM 
01308  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2IPGM 
01309                                                                   GA2IPGM 
01310      SET MAP-IDX1  TO  7.                                         GA2IPGM 
01311      SET MAP-IDX2  TO  1.                                         GA2IPGM 
01312      MOVE -1  TO                                                  GA2IPGM 
01313         MAP-PROVIDER-TYP-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).       GA2IPGM 
01314      EXEC CICS SEND   MAP('GA2II01') MAPSET('GA2ISET') ERASE      GA2IPGM 
01315         FROM(GA2II01O) CURSOR WAIT END-EXEC.                      GA2IPGM 
01316                                                                   GA2IPGM 
01317      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2IPGM 
01318                                                                   GA2IPGM 
01319  9999-EXIT.     EXIT.                                             GA2IPGM 
