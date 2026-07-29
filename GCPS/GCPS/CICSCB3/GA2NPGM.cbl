00001  ID DIVISION.                                                     01/12/06
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA2NPGM 
00003  PROGRAM-ID.     GA2NPGM.                                            LV004
00004  AUTHOR.         GARY D MULLINGS.                                 GA2NPGM 
00005  DATE-WRITTEN.   11/06/89.                                        GA2NPGM 
00006  DATE-COMPILED.                                                   GA2NPGM 
00007      SKIP3                                                        GA2NPGM 
00008 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2NPGM 
00009 *****  P R O G R A M   M O D I F I C A T I O N    S T A T U S ****GA2NPGM 
00010 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2NPGM 
00011 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2NPGM 
00012 *                                                                 GA2NPGM 
00013 *   D185    10/02/89    GDM   CREATES BASIC SKELETON RECORD FOR   GA2NPGM 
00014 *                             THE INTERNAL TABULAR RECORD #IDGD.  GA2NPGM 
00015 *                                                                 GA2NPGM 
00016 *           01/29/90    ENW   CHANGE ALL GX4 TO GX9.              GA2NPGM 
00017 *                                                                 GA2NPGM 
00018 * 11154  1/07/91  NGE  1. CHANGE GCA-I-E- FIELD IN COPYBOOKS    * GA2NPGM 
00019 *                         G2ALCKEC AND G2ALCKE2.                * GA2NPGM 
00020 *                                                               * GA2NPGM 
00021 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD     * GA2NPGM 
00022 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2NPGM 
00023 *                                                               * GA2NPGM 
00024 *14726/ 11/12/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000 AND * GA2NPGM 
00025 *15057                  THE EXPANSION OF THE GROUP SPECIFIC AND * GA2NPGM 
00026 *                       CONTRACT KEY TO SUPPORT THE TEXAS       * GA2NPGM 
00027 *                       MERGER.                                 * GA2NPGM 
00028 *                                                                 GA2NPGM 
00029 * 14726/  04/11/98  AB   EXPANDED THE SCREEN / MAP              * GA2NPGM 
00030 * 15057                  TO INCLUDE THE ENTIRE KEY              * GA2NPGM 
00031 *                                                                 GA2NPGM 
00032 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP        GA2NPGM 
00033 *                        2. ADD DELADD-OPTION = 'GAS5UPD'         GA2NPGM 
00034 *                                                               * GA2NPGM 
00035 * P????   11/19/99  FRY  ADD LENGTH PARAMETER TO THE RETURN     * GA2NPGM 
00036 *                        COMMAND WHEN DFHCOMMAREA IS SPECIFIED. * GA2NPGM 
00037 *                                                               * GA2NPGM 
00038 *                                                               * GA2NPGM 
00039 *         08-14-02   GTF RECOMPILE FOR OPID EXPANSION           * GA2NPGM 
00040 *                                                               * GA2NPGM 
00041 * D-356A  04/29/03  GTF  EXPANDED DIAGNOSIS CODE FROM 6 TO 10   * GA2NPGM 
00042 *                        BYTES. INCREASED # OF OCCURS FROM 659  * GA2NPGM 
00043 *                        TO 776 ON #IDGD TABULAR.               * GA2NPGM 
00044 *                                                               * GA2NPGM 
00045 *         01-11-06   NB  RECOMPILE FOR GCPPDIOC CHANGES         * GA2NPGM 
00046 *                                                               * GA2NPGM 
00046 * ICD-10  07/11/11   BA  CHANGE LOGIC FOR ICD-10 REQUIREMENTS.  * GA2NPGM 
SI0724*                                                               * 00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION     * 00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,       * 00009180
SI0724*                           GCCDRLEN                            * 00009190
00047 ***************************************************************** GA2NPGM 
00048 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2NPGM 
00049 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2NPGM 
00050 /                                                                 GA2NPGM 
00051 ******************************************************************GA2NPGM 
00052 *   GA2NPGM      ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM   GA2NPGM 
00053 *               DIAGNOSIS GROUP BY DIAGNOSIS CODE - GA2N          GA2NPGM 
00054 *                                                                 GA2NPGM 
00055 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2NPGM 
00056 *   TABULAR DIAGNOSIS CODES.                                      GA2NPGM 
00057 *                                                                 GA2NPGM 
00058 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2NPGM 
00059 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2NPGM 
00060 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2NPGM 
00061 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2NPGM 
00062 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2NPGM 
00063 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2NPGM 
00064 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2NPGM 
00065 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2NPGM 
00066 *                                                                 GA2NPGM 
00067 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IDGD)GA2NPGM 
00068 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2NPGM 
00069 *   XCTL TO TRANS GA1N OR PROGRAM GA1NPGM.                        GA2NPGM 
00070 *                                                                 GA2NPGM 
00071 *   FUNC CODE: GA2N                                               GA2NPGM 
00072 *   MAPSET:    GA2NSETC                                           GA2NPGM 
00073 *   FILES:     GCPSWORK                                           GA2NPGM 
00074 ******************************************************************GA2NPGM 
00075      SKIP3                                                        GA2NPGM 
00076  ENVIRONMENT DIVISION.                                            GA2NPGM 
00077 /                                                                 GA2NPGM 
00078  DATA DIVISION.                                                   GA2NPGM 
00079  WORKING-STORAGE SECTION.                                         GA2NPGM 
00080  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2NPGM 
00081      '***GA2NPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2NPGM 
00082  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2NPGM 
00083                                                                   GA2NPGM 
00084  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2NPGM 
00085                                                                   GA2NPGM 
00086  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2NPGM 
00087                                                                   GA2NPGM 
00088  01  COMMAREA-POINTER-AREA.                                       GA2NPGM 
00089      05  COMMAREA-PNTR-COMP                      PIC S9(08)  COMP.GA2NPGM 
00090      05  COMMAREA-PNTR  REDEFINES                                 GA2NPGM 
00091                             COMMAREA-PNTR-COMP USAGE IS POINTER.  GA2NPGM 
00092                                                                   GA2NPGM 
00093  01  INTERNAL-POINTER-AREA.                                       GA2NPGM 
00094      05  INTERNAL-TAB-PNTR-COMP                  PIC S9(08)  COMP.GA2NPGM 
00095      05  INTERNAL-TAB-PNTR       REDEFINES                        GA2NPGM 
00096                        INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.   GA2NPGM 
00097                                                                   GA2NPGM 
00098 ** MAP COBOL SCREEN DSECTS **                                     GA2NPGM 
00099  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2NPGM 
00100      '***  I/O MAPAREA ***'.                                      GA2NPGM 
00101  COPY GA2NSETC.                                                   GA2NPGM 
00102 /                                                                 GA2NPGM 
00103 ******************************************************************GA2NPGM 
00104 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2NPGM 
00105 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2NPGM 
00106 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2NPGM 
00107 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2NPGM 
00108 **  REDEFINES.                                                    GA2NPGM 
00109 ****************************************************************  GA2NPGM 
00110      SKIP3                                                        GA2NPGM 
00111  01  FILLER     REDEFINES   GA2NI01I.                             GA2NPGM 
00112      05  FILLER                              PIC X(89).           GA2NPGM 
00113      05  GROUP-SPECIFIC-ID-LINE.                                  GA2NPGM 
00114          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA2NPGM 
00115          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA2NPGM 
00116          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA2NPGM 
00117          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA2NPGM 
00118          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA2NPGM 
00119          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA2NPGM 
00120          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA2NPGM 
00121          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA2NPGM 
00122          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA2NPGM 
00123          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA2NPGM 
00124          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA2NPGM 
00125          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA2NPGM 
00126          10  FILLER                          PIC X(16).           GA2NPGM 
00127      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA2NPGM 
00128          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2NPGM 
00129          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2NPGM 
00130          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2NPGM 
00131          10  CONTRACT-GROUP-NO               PIC X(9).            GA2NPGM 
00132          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2NPGM 
00133          10  CONTRACT-SECTION-NO             PIC X(5).            GA2NPGM 
00134          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2NPGM 
00135          10  CONTRACT-PKG-CODE               PIC X(3).            GA2NPGM 
00136          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2NPGM 
00137          10  CONTRACT-LOB                    PIC X.               GA2NPGM 
00138          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2NPGM 
00139          10  CONTRACT-PROV-CTL               PIC XX.              GA2NPGM 
00140          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2NPGM 
00141          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2NPGM 
00142          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2NPGM 
00143          10  CONTRACT-EFF-DATE               PIC X(6).            GA2NPGM 
00144          10  FILLER                          PIC X(1).            GA2NPGM 
00145      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA2NPGM 
00146                                     GROUP-SPECIFIC-ID-LINE.       GA2NPGM 
00147          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA2NPGM 
00148          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA2NPGM 
00149          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA2NPGM 
00150          10  BEN-PROV-GROUP-NO               PIC X(9).            GA2NPGM 
00151          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA2NPGM 
00152          10  BEN-PROV-SECTION-NO             PIC X(5).            GA2NPGM 
00153          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA2NPGM 
00154          10  BEN-PROV-PKG-CODE               PIC X(3).            GA2NPGM 
00155          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA2NPGM 
00156          10  BEN-PROV-LOB                    PIC X.               GA2NPGM 
00157          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA2NPGM 
00158          10  BEN-PROV-PROV-CTL               PIC XX.              GA2NPGM 
00159          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA2NPGM 
00160          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA2NPGM 
00161          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA2NPGM 
00162          10  BEN-PROV-EFF-DATE               PIC X(6).            GA2NPGM 
00163          10  BEN-PROV-ID-HEADING             PIC X(6).            GA2NPGM 
00164          10  BEN-PROV-ID-NO                  PIC X(6).            GA2NPGM 
00165          10  FILLER                          PIC X(4).            GA2NPGM 
00166      05  FILLER                              PIC X(78).           GA2NPGM 
00167      05  MAP-DIAGNOSIS-CODE-ROW         OCCURS 14 TIMES INDEXED   GA2NPGM 
00168          BY MAP-IDX1.                                             GA2NPGM 
00169        10  MAP-DIAGNOSIS-CODE-COL         OCCURS 3 TIMES INDEXED  GA2NPGM 
00170            BY MAP-IDX2.                                           GA2NPGM 
00171          15  MAP-DIAGNOSIS-CODE-LEN          PIC S9(4) COMP SYNC. GA2NPGM 
00172          15  MAP-DIAGNOSIS-CODE-ATTR         PIC X.               GA2NPGM 
00173          15  MAP-DIAGNOSIS-CODE              PIC X(10).           GA2NPGM 
00174          15  FILLER                          PIC X.               GA2NPGM 
00175      SKIP3                                                        GA2NPGM 
00176  01  FILLER.                                                      GA2NPGM 
00177 ****************************************************************  GA2NPGM 
00178 **   FIELDS DESCRIBING NUMBER OF OCCURS FOR MAP.                  GA2NPGM 
00179 ****************************************************************  GA2NPGM 
00180      05  WS-MAP-ROW                  PIC S999 COMP    VALUE +14.  GA2NPGM 
00181      05  WS-MAP-COL                  PIC S999 COMP    VALUE +3.   GA2NPGM 
00182 /                                                                 GA2NPGM 
00183 ** ALTERNATIVE WORKFILE KEYS **                                   GA2NPGM 
00184  01  FILLER                      PIC X(32)  VALUE                 GA2NPGM 
00185      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2NPGM 
00186  01  WS-ALT-WORKFILE-KEYS.                                        GA2NPGM 
00187  COPY GCWRKKEY.                                                   GA2NPGM 
00188 /                                                                 GA2NPGM 
00189 ** WORKFIELDS **                                                  GA2NPGM 
00190  01  FILLER                           PIC X(16)                   GA2NPGM 
00191               VALUE '** WORKFIELDS **'.                           GA2NPGM 
00192  01  WS-WORK-FIELDS.                                              GA2NPGM 
00193      05  WS-HEX-00                    PIC X VALUE LOW-VALUES.     GA2NPGM 
00194      05  WS-ADD-COUNT                 PIC 999 COMP VALUE ZEROES.  GA2NPGM 
00195      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2NPGM 
00196        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2NPGM 
00197      05  WS-SAVED-FIELDS.                                         GA2NPGM 
00198        10  WS-SAVED-BENEFIT                PIC X(10).             GA2NPGM 
00198        10  WS-SAVED-DIAGNOSIS              PIC X(10).             GA2NPGM 
00199      05  WS-DIAG-CODE-ENTRY         OCCURS 43 TIMES INDEXED BY    GA2NPGM 
00200          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2NPGM 
00201        10  WS-DIAGNOSIS-CODE              PIC X(10).              GA2NPGM 
00202 /                                                                 GA2NPGM 
00203 *** SWITCHES ***                                                  GA2NPGM 
00204  01  FILLER                           PIC X(14)                   GA2NPGM 
00205               VALUE '** SWITCHES **'.                             GA2NPGM 
00206  01  WS-SWITCHES.                                                 GA2NPGM 
00207      05  WS-ERROR-SW                  PIC X.                      GA2NPGM 
00208                                                                   GA2NPGM 
00209 ** TITLE LINES **                                                 GA2NPGM 
00210  01  WS-TITLE-LINES.                                              GA2NPGM 
00211      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA2NPGM 
00212          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA2NPGM 
00213      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA2NPGM 
00214          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA2NPGM 
00215      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA2NPGM 
00216          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA2NPGM 
00217                                                                   GA2NPGM 
00218 *** RECORD LENGTHS ***                                            GA2NPGM 
00219  01  FILLER                           PIC X(20)                   GA2NPGM 
00220               VALUE '** RECORD LENGTHS **'.                       GA2NPGM 
00221  01  WS-RECORD-LENGTHS.                                           GA2NPGM 
00222     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP VALUE ZEROES.GA2NPGM 
00223     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP VALUE ZEROES.GA2NPGM 
00224     05 WS-COPY-LENGTH                 PIC S9(4) COMP VALUE ZEROES.GA2NPGM 
00225     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2NPGM 
00226 ****************************************************************  GA2NPGM 
00227 /                                                                 GA2NPGM 
00228  01  WT-00-GA2NPGM-TABLES.                                        GA2NPGM 
00229      05  FILLER                   PIC X(16)  VALUE                GA2NPGM 
00230          '*GA2NPGM TABLES*'.                                      GA2NPGM 
00231                                                                   GA2NPGM 
00232  01  WT-01-TABLE.                                                 GA2NPGM 
00233      05  FILLER                  PIC X(16) VALUE                  GA2NPGM 
00234          '* WT-01-TABLE  *'.                                      GA2NPGM 
00235 ******************************************************************GA2NPGM 
00236 *    WT-01   MESSAGE TABLE                                       *GA2NPGM 
00237 ******************************************************************GA2NPGM 
00238  01  FILLER.                                                      GA2NPGM 
00239      05  WT-01-MESSAGE-VALUES.                                    GA2NPGM 
00240                                                                   GA2NPGM 
00241 *----------------------------------------------------------------*GA2NPGM 
00242          10  WT-01-ENTRY-001.                                     GA2NPGM 
00243              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00244              15  WT-01-MESSAGE-TEXT-001.                          GA2NPGM 
00245                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00246                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00247                  20  FILLER          PIC X(3)  VALUE  '001'.      GA2NPGM 
00248                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00249                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00250                           '** INVALID REQUEST. THE PF KEY USED HASGA2NPGM 
00251 -                   ' NO MEANING TO THIS PROGRAM **'.             GA2NPGM 
00252              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00253                                                                   GA2NPGM 
00254 *----------------------------------------------------------------*GA2NPGM 
00255          10  WT-01-ENTRY-002.                                     GA2NPGM 
00256              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00257              15  WT-01-MESSAGE-TEXT-002.                          GA2NPGM 
00258                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00259                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00260                  20  FILLER          PIC X(3)  VALUE  '002'.      GA2NPGM 
00261                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00262                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00263                      '** FUTURE USE **'.                          GA2NPGM 
00264              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00265                                                                   GA2NPGM 
00266 *----------------------------------------------------------------*GA2NPGM 
00267          10  WT-01-ENTRY-003.                                     GA2NPGM 
00268              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00269              15  WT-01-MESSAGE-TEXT-003.                          GA2NPGM 
00270                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00271                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00272                  20  FILLER          PIC X(3)  VALUE  '003'.      GA2NPGM 
00273                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00274                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00275                      '** DIAGNOSIS CODE IS INVALID **'.           GA2NPGM 
00276              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00277 *----------------------------------------------------------------*GA2NPGM 
00278          10  WT-01-ENTRY-004.                                     GA2NPGM 
00279              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00280              15  WT-01-MESSAGE-TEXT-004.                          GA2NPGM 
00281                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00282                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00283                  20  FILLER          PIC X(3)  VALUE  '004'.      GA2NPGM 
00284                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00285                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00286                  '** INCLUDE/EXCLUDE FIELD VALUE NOT VALID **'.   GA2NPGM 
00287              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00288 *----------------------------------------------------------------*GA2NPGM 
00289          10  WT-01-ENTRY-005.                                     GA2NPGM 
00290              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00291              15  WT-01-MESSAGE-TEXT-005.                          GA2NPGM 
00292                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00293                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00294                  20  FILLER          PIC X(3)  VALUE  '005'.      GA2NPGM 
00295                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00296                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00297                            '** NO ADD ENTRY FOUND OR INC/EXC FIELDGA2NPGM 
00298 -                    'CHANGE **'.                                 GA2NPGM 
00299              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00300 *----------------------------------------------------------------*GA2NPGM 
00301          10  WT-01-ENTRY-006.                                     GA2NPGM 
00302              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00303              15  WT-01-MESSAGE-TEXT-006.                          GA2NPGM 
00304                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00305                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00306                  20  FILLER          PIC X(3)  VALUE  '006'.      GA2NPGM 
00307                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00308                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00309                               '** ERROR READING ALL LEVEL INTERNALGA2NPGM 
00310 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2NPGM 
00311              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00312                                                                   GA2NPGM 
00313 *----------------------------------------------------------------*GA2NPGM 
00314          10  WT-01-ENTRY-007.                                     GA2NPGM 
00315              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00316              15  WT-01-MESSAGE-TEXT-007.                          GA2NPGM 
00317                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00318                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00319                  20  FILLER          PIC X(3)  VALUE  '007'.      GA2NPGM 
00320                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00321                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00322                              '** PROGRAM ABOUT TO EXCEED MAX RECORGA2NPGM 
00323 -                    ' SIZE. CONTACT SYSTEMS AREA **'.            GA2NPGM 
00324              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00325 *----------------------------------------------------------------*GA2NPGM 
00326          10  WT-01-ENTRY-008.                                     GA2NPGM 
00327              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00328              15  WT-01-MESSAGE-TEXT-008.                          GA2NPGM 
00329                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00330                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00331                  20  FILLER          PIC X(3)  VALUE  '008'.      GA2NPGM 
00332                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00333                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00334                              '** PROGRAM SUBSCRIPT ABOUT TO EXCEEDGA2NPGM 
00335 -                    ' ITS MAX. CONTACT SYSTEMS AREA **'.         GA2NPGM 
00336              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00337                                                                   GA2NPGM 
00338 *----------------------------------------------------------------*GA2NPGM 
00339          10  WT-01-ENTRY-009.                                     GA2NPGM 
00340              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00341              15  WT-01-MESSAGE-TEXT-009.                          GA2NPGM 
00342                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00343                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00344                  20  FILLER          PIC X(3)  VALUE  '009'.      GA2NPGM 
00345                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00346                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00347                             '** ERROR REWRITING ALL LEVEL INTERNALGA2NPGM 
00348 -                    ' TABULAR RECORD **'.                        GA2NPGM 
00349              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00350                                                                   GA2NPGM 
00351 *----------------------------------------------------------------*GA2NPGM 
00352          10  WT-01-ENTRY-010.                                     GA2NPGM 
00353              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00354              15  WT-01-MESSAGE-TEXT-010.                          GA2NPGM 
00355                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00356                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00357                  20  FILLER          PIC X(3)  VALUE  '010'.      GA2NPGM 
00358                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00359                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00360                               '** ERROR READING ALL LEVEL INTERNALGA2NPGM 
00361 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2NPGM 
00362              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00363                                                                   GA2NPGM 
00364 *----------------------------------------------------------------*GA2NPGM 
00365          10  WT-01-ENTRY-011.                                     GA2NPGM 
00366              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00367              15  WT-01-MESSAGE-TEXT-011.                          GA2NPGM 
00368                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00369                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00370                  20  FILLER          PIC X(3)  VALUE  '011'.      GA2NPGM 
00371                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00372                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00373                      '** COMMAREA LENGTH IS INVALID **'.          GA2NPGM 
00374              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00375                                                                   GA2NPGM 
00376 *----------------------------------------------------------------*GA2NPGM 
00377          10  WT-01-ENTRY-012.                                     GA2NPGM 
00378              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00379              15  WT-01-MESSAGE-TEXT-012.                          GA2NPGM 
00380                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00381                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00382                  20  FILLER          PIC X(3)  VALUE  '012'.      GA2NPGM 
00383                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00384                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00385                      '**** FUTURE USE ****'.                      GA2NPGM 
00386              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00387                                                                   GA2NPGM 
00388 *----------------------------------------------------------------*GA2NPGM 
00389          10  WT-01-ENTRY-013.                                     GA2NPGM 
00390              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2NPGM 
00391              15  WT-01-MESSAGE-TEXT-013.                          GA2NPGM 
00392                  20  FILLER          PIC X(4)  VALUE  'GA2N'.     GA2NPGM 
00393                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2NPGM 
00394                  20  FILLER          PIC X(3)  VALUE  '013'.      GA2NPGM 
00395                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2NPGM 
00396                  20  FILLER          PIC X(70) VALUE              GA2NPGM 
00397                      '**** FUTURE USE ****'.                      GA2NPGM 
00398              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2NPGM 
00399                                                                   GA2NPGM 
00400 *----------------------------------------------------------------*GA2NPGM 
00401                                                                   GA2NPGM 
00402      05  WT-01-MESSAGE-TABLE         REDEFINES                    GA2NPGM 
00403          WT-01-MESSAGE-VALUES        OCCURS 013 TIMES             GA2NPGM 
00404                                      INDEXED BY WT-01-INDEX.      GA2NPGM 
00405          10  WT-01-ENTRY.                                         GA2NPGM 
00406              15  FILLER              PIC X(02).                   GA2NPGM 
00407              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GA2NPGM 
00408              15  FILLER              PIC X(02).                   GA2NPGM 
00409                                                                   GA2NPGM 
00410 *----------------------------------------------------------------*GA2NPGM 
00411 /                                                                 GA2NPGM 
00412 ** ATTRIBUTES **                                                  GA2NPGM 
00413  COPY DFHBMSCA.                                                   GA2NPGM 
00414      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2NPGM 
00415 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA2NPGM 
00416  01  FILLER.                                                      GA2NPGM 
00417      COPY GCCDRLEN.                                               GA2NPGM 
00418 /                                                                 GA2NPGM 
00419 ** IO PARM AREA **                                                GA2NPGM 
00420  01  GCPPDIO-PARM-AREA.                                           GA2NPGM 
00421  COPY GCPPDIOC.                                                   GA2NPGM 
00422 /                                                                 GA2NPGM 
00423 ** ATTENTION IDENTIFIERS **                                       GA2NPGM 
00424  COPY DFHAID.                                                     GA2NPGM 
00425 /                                                                 GA2NPGM 
00426  01  WS-END                          PIC X(16)  VALUE             GA2NPGM 
00427      '*** W/S ENDS ***'.                                          GA2NPGM 
00428 /                                                                 GA2NPGM 
00429  LINKAGE SECTION.                                                 GA2NPGM 
00430                                                                   GA2NPGM 
00431  01  DFHCOMMAREA.                                                 GA2NPGM 
00432  COPY G2ALCKEC.                                                   GA2NPGM 
00433  COPY GACDACWA.                                                   GA2NPGM 
00434 *    05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA2NPGM 
00435      05  GAS1UPD-PASSED-AREA.                                     GA2NPGM 
00436          07  LVL2-B-SW          PIC X.                            GA2NPGM 
00437          07  LVL2-F-SW          PIC X.                            GA2NPGM 
00438          07  LVL2-G-SW          PIC X.                            GA2NPGM 
00439          07  INTR-TAB-PGM-ID    PIC X(8).                         GA2NPGM 
00440          07  FILLER             PIC X(9).                         GA2NPGM 
00441      05  DELADD-OPTION          PIC X(7).                         GA2NPGM 
00442                                                                   GA2NPGM 
00443 *01  GCA-COMMAREA.                                                GA2NPGM 
00444 *COPY G2ALCKEC.                                                   GA2NPGM 
00445                                                                   GA2NPGM 
00446 /                                                                 GA2NPGM 
00447 ** I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD **         GA2NPGM 
00448  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2NPGM 
00449  COPY GCIOPRM1.                                                   GA2NPGM 
00450 /                                                                 GA2NPGM 
00451  COPY GCWRKDCC.                                                   GA2NPGM 
00452 /                                                                 GA2NPGM 
00453  COPY GCTIDGDC.                                                   GA2NPGM 
00454 /                                                                 GA2NPGM 
00455 ****************************************************************  GA2NPGM 
00456 ** COPY OF THE TABULAR PORTION OF THE RECORD, THIS AREA USED IN   GA2NPGM 
00457 ** SORTING PROCESS.                                               GA2NPGM 
00458 ****************************************************************  GA2NPGM 
00459  01  COPY-TABULAR-TABLE-AREA.                                     GA2NPGM 
00460      05  COPY-TABULAR-TABLE  OCCURS 776 TIMES INDEXED BY          GA2NPGM 
00461            COPY-IDX.                                              GA2NPGM 
00462        10  COPY-DIAGNOSIS-CODE          PIC X(10).                GA2NPGM 
00463 /                                                                 GA2NPGM 
00464 ** IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD **             GA2NPGM 
00465  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2NPGM 
00466  COPY GCIOPRM2.                                                   GA2NPGM 
00467 /                                                                 GA2NPGM 
00468  COPY GCWRKDC2.                                                   GA2NPGM 
00469 /                                                                 GA2NPGM 
00470  COPY GCTABMC.                                                    GA2NPGM 
00471 /                                                                 GA2NPGM 
00472                                                                   GA2NPGM 
00473  PROCEDURE DIVISION.                                              GA2NPGM 
00474                                                                   GA2NPGM 
00475 ******************************************************************GA2NPGM 
00476 **                     M A I N L I N E                            GA2NPGM 
00477 **                                                                GA2NPGM 
00478 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2NPGM 
00479 **  TAKEN BY THE OPERATOR.                                        GA2NPGM 
00480 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2NPGM 
00481 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2NPGM 
00482 **     ADDITIONS FROM.                                            GA2NPGM 
00483 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2NPGM 
00484 **     KEY PF12 OR PF24.                                          GA2NPGM 
00485 **  3. RECEIVE THE SCREEN.                                        GA2NPGM 
00486 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2NPGM 
00487 **     MENU.                                                      GA2NPGM 
00488 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2NPGM 
00489 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2NPGM 
00490 **     (RETURN) TO THE DELETE PROGRAM (GA1NPGM).                  GA2NPGM 
00491 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2NPGM 
00492 **     (RETURN) TO THE PREVIOUS MENU.                             GA2NPGM 
00493 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2NPGM 
00494 **     NORMAL ADD PROCESSING, EXCEPT BYPASS EMPTY VALIDATION TABLEGA2NPGM 
00495 **     CONDITION FOR THE DIAGNOSIS CODE.                          GA2NPGM 
00496 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2NPGM 
00497 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2NPGM 
00498 **                                                                GA2NPGM 
00499 ******************************************************************GA2NPGM 
00500  1000-MAIN-LINE SECTION.                                          GA2NPGM 
00501                                                                   GA2NPGM 
00502      MOVE '1000'  TO  WS-PARA-ID.                                 GA2NPGM 
00503                                                                   GA2NPGM 
00504      IF EIBAID  =  DFHCLEAR                                       GA2NPGM 
00505          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2NPGM 
00506                         ERASE                                     GA2NPGM 
00507          END-EXEC                                                 GA2NPGM 
00508          EXEC CICS RETURN                                         GA2NPGM 
00509          END-EXEC.                                                GA2NPGM 
00510                                                                   GA2NPGM 
00511      IF EIBTRNID  NOT =  'GA2N'                                   GA2NPGM 
00512         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2NPGM 
00513         GO TO 1099-RETURN.                                        GA2NPGM 
00514                                                                   GA2NPGM 
00515      EXEC CICS RECEIVE   MAP('GA2NI01') MAPSET('GA2NSET')         GA2NPGM 
00516         INTO(GA2NI01I) END-EXEC.                                  GA2NPGM 
00517                                                                   GA2NPGM 
00518      IF SCRNIDNI  NOT =  '002N00'                                 GA2NPGM 
00519         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2NPGM 
00520                                                                   GA2NPGM 
00521      IF EIBAID  =  DFHENTER                                       GA2NPGM 
00522         PERFORM 2000-ADD-PROCESSING                               GA2NPGM 
00523         GO TO 1099-RETURN.                                        GA2NPGM 
00524                                                                   GA2NPGM 
00525      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2NPGM 
00526         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2NPGM 
00527                                                                   GA2NPGM 
00528      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2NPGM 
00529         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2NPGM 
00530                                                                   GA2NPGM 
00531      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2NPGM 
00532         PERFORM 2000-ADD-PROCESSING                               GA2NPGM 
00533         GO TO 1099-RETURN.                                        GA2NPGM 
00534                                                                   GA2NPGM 
00535      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2NPGM 
00536      MOVE -1  TO                                                  GA2NPGM 
00537         MAP-DIAGNOSIS-CODE-LEN (MAP-IDX1, MAP-IDX2).              GA2NPGM 
00538         SET WT-01-INDEX TO +01.                                   GA2NPGM 
00539         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2NPGM 
00540                                                                   GA2NPGM 
00541      EXEC CICS SEND   MAP('GA2NI01') MAPSET('GA2NSET') DATAONLY   GA2NPGM 
00542      FROM(GA2NI01O) CURSOR END-EXEC.                              GA2NPGM 
00543      GO TO 1099-RETURN.                                           GA2NPGM 
00544                                                                   GA2NPGM 
00545  1000-EXIT.  EXIT.                                                GA2NPGM 
00546                                                                   GA2NPGM 
00547  1099-RETURN.                                                     GA2NPGM 
                                                                                
00548      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2NPGM 
00549         (DELADD-OPTION = 'GAS1UPD') OR                            GA2NPGM 
00550         (DELADD-OPTION = 'GAS2UPD') OR                            GA2NPGM 
00551         (DELADD-OPTION = 'GAS3UPD') OR                            GA2NPGM 
00552         (DELADD-OPTION = 'GAS4UPD') OR                            GA2NPGM 
00553         (DELADD-OPTION = 'GAS5UPD')                               GA2NPGM 
00554          EXEC CICS RETURN   END-EXEC                              GA2NPGM 
00555      ELSE                                                         GA2NPGM 
00556          EXEC CICS RETURN TRANSID('GA2N')                         GA2NPGM 
00557                    COMMAREA(DFHCOMMAREA)                          GA2NPGM 
00558                    LENGTH  (EIBCALEN)                             GA2NPGM 
00559                    END-EXEC.                                      GA2NPGM 
00560                                                                   GA2NPGM 
00561      GOBACK.                                                      GA2NPGM 
00562  1099-EXIT.  EXIT.                                                GA2NPGM 
00563 /                                                                 GA2NPGM 
00564 ******************************************************************GA2NPGM 
00565 **               A D D   P R O C E S S I N G                      GA2NPGM 
00566 **                                                                GA2NPGM 
00567 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2NPGM 
00568 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2NPGM.            GA2NPGM 
00569 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2NPGM 
00570 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2NPGM 
00571 **     SKIP TO THE NEXT LINE.                                     GA2NPGM 
00572 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2NPGM 
00573 **     WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2NPGM 
00574 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2NPGM 
00575 **     DATA.                                                      GA2NPGM 
00576 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2NPGM 
00577 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2NPGM 
00578 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2NPGM 
00579 **     ORDER, FIELD BY FIELD.                                     GA2NPGM 
00580 **  6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2NPGM 
00581 **     MADE.                                                      GA2NPGM 
00582 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2NPGM 
00583 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2NPGM 
00584 **     BACK INTO THE RECORD.                                      GA2NPGM 
00585 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2NPGM 
00586 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2NPGM 
00587 **                                                                GA2NPGM 
00588 ******************************************************************GA2NPGM 
00589  2000-ADD-PROCESSING SECTION.                                     GA2NPGM 
00590                                                                   GA2NPGM 
00591      MOVE '2000'  TO  WS-PARA-ID.                                 GA2NPGM 
00592      MOVE 'N'     TO  WS-ERROR-SW.                                GA2NPGM 
00593      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2NPGM 
00594      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2NPGM 
00595                                                                   GA2NPGM 
00596 ***  D185     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY         GA2NPGM 
00597 *                                                                 GA2NPGM 
00598 *    MOVE LOW-VALUES TO GA2NI01I.                                 GA2NPGM 
00599 *                                                                 GA2NPGM 
00600      MOVE '2005'  TO  WS-PARA-ID.                                 GA2NPGM 
                                                                                
00601  2005-RESET-ALL-ATTRIBUTES.                                       GA2NPGM 
00602                                                                   GA2NPGM 
00603      PERFORM WITH TEST BEFORE                                     GA2NPGM 
00604       VARYING MAP-IDX1 FROM 1 BY 1 UNTIL MAP-IDX2 > WS-MAP-COL    GA2NPGM 
00605           MOVE DFHBMUNF TO                                        GA2NPGM 
00606                   MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2)    GA2NPGM 
00607       IF MAP-IDX1 = WS-MAP-ROW                                    GA2NPGM 
00608         SET MAP-IDX2 UP BY 1                                      GA2NPGM 
00609         SET MAP-IDX1 TO 1                                         GA2NPGM 
00610         SET MAP-IDX1 DOWN BY 1                                    GA2NPGM 
00611       END-IF                                                      GA2NPGM 
00612      END-PERFORM.                                                 GA2NPGM 
00613                                                                   GA2NPGM 
00614      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2NPGM 
00615      MOVE '2010'  TO  WS-PARA-ID.                                 GA2NPGM 
                                                                                
00616  2010-VALIDATE-ADD-ENTRIES.                                       GA2NPGM 
                                                                                
00617      IF MAP-DIAGNOSIS-CODE-LEN (MAP-IDX1, MAP-IDX2)               GA2NPGM 
00618            =  ZERO                                                GA2NPGM 
00619         IF MAP-IDX1  <  WS-MAP-ROW                                GA2NPGM 
00620            SET MAP-IDX1  UP BY  1                                 GA2NPGM 
00621            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2NPGM 
00622         ELSE                                                      GA2NPGM 
00623            IF MAP-IDX2  <  WS-MAP-COL                             GA2NPGM 
00624               SET MAP-IDX1  TO  1                                 GA2NPGM 
00625               SET MAP-IDX2  UP BY  1                              GA2NPGM 
00626               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2NPGM 
00627            ELSE                                                   GA2NPGM 
00628               GO TO 2020-CHECK-FOR-ERRORS.                        GA2NPGM 
00629                                                                   GA2NPGM 
00630      IF MAP-DIAGNOSIS-CODE-LEN (MAP-IDX1, MAP-IDX2) = ZERO        GA2NPGM 
00631         MOVE DFHBMUBF  TO                                         GA2NPGM 
00632            MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2)           GA2NPGM 
00633         MOVE '??????' TO                                          GA2NPGM 
00634            MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2)                GA2NPGM 
00635         IF WS-ERROR-SW  NOT =  'Y'                                GA2NPGM 
00636            MOVE 'Y'  TO  WS-ERROR-SW                              GA2NPGM 
00637            MOVE -1   TO                                           GA2NPGM 
00638               MAP-DIAGNOSIS-CODE-LEN (MAP-IDX1, MAP-IDX2)         GA2NPGM 
00639            SET WT-01-INDEX TO +03                                 GA2NPGM 
00640            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                   GA2NPGM 
00641                                                                   GA2NPGM 
      *** ICD-10 START                                                  GA2NPGM 
           MOVE MAP-DIAGNOSIS-CODE(MAP-IDX1, MAP-IDX2)                          
                                             TO WS-SAVED-DIAGNOSIS              
                                                GCPPDIO-SVC-CD.         GA2NPGM 
                                                                                
           IF WS-SAVED-DIAGNOSIS (1:3) = 'BIT'                                  
              MOVE 'PRCDR04 '    TO  GCPPDIO-REQUEST-TYPE                       
           ELSE                                                                 
              MOVE 'PRCDR03 '    TO  GCPPDIO-REQUEST-TYPE               GA2NPGM 
                                                                                
              IF GCPPDIO-SVC-CD (1:1)  IS NUMERIC                               
                 MOVE '9'         TO  GCPPDIO-SVC-CD-SYS-ID                     
              ELSE                                                              
                 IF  GCPPDIO-SVC-CD (1:1) = 'V'                                 
                 AND GCPPDIO-SVC-CD (6:1) = SPACE                               
                    MOVE '9' TO GCPPDIO-SVC-CD-SYS-ID                           
                 ELSE                                                           
                    MOVE '1' TO  GCPPDIO-SVC-CD-SYS-ID                          
                 END-IF                                                         
              END-IF                                                            
           END-IF                                                               
      *** ICD-10 END                                                            
                                                                                
00648      EXEC CICS LINK PROGRAM('GCPPDIO')                            GA2NPGM 
00649           COMMAREA(GCPPDIO-PARM-AREA)                             GA2NPGM 
00650           LENGTH(GCPPDIO-CA-LEN)                                  GA2NPGM 
00651      END-EXEC.                                                    GA2NPGM 
00652                                                                   GA2NPGM 
00653      IF GCPPDIO-SUCCESSFUL                                        GA2NPGM 
00654          NEXT SENTENCE                                            GA2NPGM 
00655      ELSE                                                         GA2NPGM 
00656         IF GCPPDIO-REC-NOT-FOUND                                  GA2NPGM 
00657            MOVE DFHBMUBF                                          GA2NPGM 
00658               TO MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2)     GA2NPGM 
00659            IF WS-ERROR-SW NOT = 'Y'                               GA2NPGM 
00660               MOVE 'Y' TO WS-ERROR-SW                             GA2NPGM 
00661               MOVE -1 TO MAP-DIAGNOSIS-CODE-LEN(MAP-IDX1,MAP-IDX2)GA2NPGM 
00662 *             SET WT-01-INDEX TO +03                              GA2NPGM 
00663 *             PERFORM 9000-000-MOVE-MSG-TO-SCREEN                 GA2NPGM 
                    IF PCG-REQUEST-TYPE                                         
                       MOVE                                                     
                         'REQUESTED PREMIER CODE GROUP BIT IS NOT FOUND'        
00669                                                        TO ERRMSGO GA2NPGM 
                    ELSE                                                        
00669                  MOVE GCPPDIO-RETURN-MESSAGE           TO ERRMSGO GA2NPGM 
00664            ELSE                                                   GA2NPGM 
00665               MOVE DFHBMUBF TO                                    GA2NPGM 
00666                  MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2)     GA2NPGM 
00667         ELSE                                                      GA2NPGM 
00668            MOVE 'DEW1'  TO  WS-ABEND-CODE                         GA2NPGM 
00669            MOVE GCPPDIO-RETURN-MESSAGE TO ERRMSGO                 GA2NPGM 
00670            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA2NPGM 
00671                                                                   GA2NPGM 
00672      IF MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2) NOT =        GA2NPGM 
00673                                                           DFHBMUBFGA2NPGM 
00674         ADD 1  TO  WS-ADD-COUNT                                   GA2NPGM 
00675         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2NPGM 
00676         MOVE MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2) TO           GA2NPGM 
00677            WS-DIAGNOSIS-CODE (WS-SORT-IDX).                       GA2NPGM 
00678                                                                   GA2NPGM 
00679      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2NPGM 
00680         SET MAP-IDX1  UP BY  1                                    GA2NPGM 
00681         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2NPGM 
00682      IF MAP-IDX2  <  WS-MAP-COL                                   GA2NPGM 
00683         SET MAP-IDX1  TO  1                                       GA2NPGM 
00684         SET MAP-IDX2  UP BY  1                                    GA2NPGM 
00685         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2NPGM 
00686                                                                   GA2NPGM 
00687  2020-CHECK-FOR-ERRORS.                                           GA2NPGM 
                                                                                
00688      MOVE '2020'  TO  WS-PARA-ID.                                 GA2NPGM 
00689      IF INCEXCI  NOT =  'I' AND  NOT =  'E'                       GA2NPGM 
00690         MOVE -1  TO  INCEXCL                                      GA2NPGM 
00691         MOVE 'Y'  TO  WS-ERROR-SW                                 GA2NPGM 
00692         SET WT-01-INDEX TO +04                                    GA2NPGM 
00693         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2NPGM 
00694                                                                   GA2NPGM 
00695      IF WS-ERROR-SW  =  'Y'                                       GA2NPGM 
00696         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2NPGM 
00697            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2NPGM 
00698            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2NPGM 
00699            INCEXCO                                                GA2NPGM 
00700         MOVE '2100'  TO  WS-PARA-ID                               GA2NPGM 
00701         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2NPGM 
00702            VARYING MAP-IDX2 FROM  1  BY  1                        GA2NPGM 
00703               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2NPGM 
00704            AFTER MAP-IDX1 FROM  1  BY  1                          GA2NPGM 
00705               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2NPGM 
00706         MOVE '2020'  TO  WS-PARA-ID                               GA2NPGM 
00707         EXEC CICS SEND   MAP('GA2NI01') MAPSET('GA2NSET')         GA2NPGM 
00708            DATAONLY FROM(GA2NI01O) CURSOR END-EXEC                GA2NPGM 
00709         GO TO 2099-EXIT.                                          GA2NPGM 
00710                                                                   GA2NPGM 
00711      IF WS-ADD-COUNT  NOT >  ZERO AND                             GA2NPGM 
00712         INCEXCI  =  INEXDRKI                                      GA2NPGM 
00713         SET WT-01-INDEX TO +05                                    GA2NPGM 
00714         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
00715         MOVE -1  TO  INCEXCL                                      GA2NPGM 
00716         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2NPGM 
00717            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2NPGM 
00718            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2NPGM 
00719            INCEXCO                                                GA2NPGM 
00720         EXEC CICS SEND   MAP('GA2NI01') MAPSET('GA2NSET')         GA2NPGM 
00721            DATAONLY FROM(GA2NI01O) CURSOR END-EXEC                GA2NPGM 
00722         GO TO 2099-EXIT.                                          GA2NPGM 
00723                                                                   GA2NPGM 
00724  2025-CONTINUE-PROCESSING.                                        GA2NPGM 
00725                                                                   GA2NPGM 
00726      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN   =                  GA2NPGM 
00727               GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +            GA2NPGM 
00728                         GC-GCTABULR-IDGD-FIXED-LEN +              GA2NPGM 
00729      (GC-GCTABULR-IDGD-VARY-LEN * GC-GCTABULR-IDGD-VARY-MAX-OCUR).GA2NPGM 
00730                                                                   GA2NPGM 
00731      EXEC CICS                                                    GA2NPGM 
00732         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2NPGM 
00733         INITIMG(WS-HEX-00)                                        GA2NPGM 
00734         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2NPGM 
00735      END-EXEC.                                                    GA2NPGM 
00736                                                                   GA2NPGM 
00737      IF  FRMNUIDI  =  'GS3A'                                      GA2NPGM 
00738         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2NPGM 
00739         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2NPGM 
00740         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA2NPGM 
00741         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2NPGM 
00742 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2NPGM 
00743         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2NPGM 
00744 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2NPGM 
00745         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2NPGM 
00746         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2NPGM 
00747         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2NPGM 
00748                          GCIO-WRK-PROVIDER-CONTROL                GA2NPGM 
00749         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2NPGM 
00750         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2NPGM 
00751                                                                   GA2NPGM 
00752      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2NPGM 
00753         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2NPGM 
00754         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2NPGM 
00755         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2NPGM 
00756         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2NPGM 
00757 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2NPGM 
00758         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2NPGM 
00759 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2NPGM 
00760         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2NPGM 
00761         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2NPGM 
00762         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2NPGM 
00763         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2NPGM 
00764         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2NPGM 
00765         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2NPGM 
00766                                                                   GA2NPGM 
00767      IF  FRMNUIDI  =  'GC8A'                                      GA2NPGM 
00768         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2NPGM 
00769         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2NPGM 
00770         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA2NPGM 
00771         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2NPGM 
00772 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2NPGM 
00773         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2NPGM 
00774 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2NPGM 
00775         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2NPGM 
00776         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2NPGM 
00777         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2NPGM 
00778         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2NPGM 
00779         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2NPGM 
00780         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2NPGM 
00781                                                                   GA2NPGM 
00782      MOVE GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.               GA2NPGM 
00783      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2NPGM 
00784      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2NPGM 
00785      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2NPGM 
00786      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA2NPGM 
00787      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2NPGM 
00788      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2NPGM 
00789                                                                   GA2NPGM 
00790      MOVE GC-GCTABULR-IDGD-VARY-MAX-OCUR                          GA2NPGM 
00791        TO GX9-ENTRY-COUNT.                                        GA2NPGM 
00792                                                                   GA2NPGM 
00793      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2NPGM 
00794                                                                   GA2NPGM 
00795      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2NPGM 
00796         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2NPGM 
00797         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2NPGM 
00798                                                                   GA2NPGM 
00799      IF  NOT GCIO-GOOD-RETURN                                     GA2NPGM 
00800         SET WT-01-INDEX TO +06                                    GA2NPGM 
00801         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
00802         MOVE '2N01'  TO  WS-ABEND-CODE                            GA2NPGM 
00803         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
00804                                                                   GA2NPGM 
00805      MOVE INCEXCI  TO  INEXDRKO,  GX9-INCLUDE-EXCLUDE-IND.        GA2NPGM 
00806      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2NPGM 
00807         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2NPGM 
00808                                                                   GA2NPGM 
00809       SET WS-SORT-IDX  TO  1.                                     GA2NPGM 
00810       SET WS-SORT-IDX2  TO  2.                                    GA2NPGM 
00811       MOVE '2030'  TO  WS-PARA-ID.                                GA2NPGM 
00812                                                                   GA2NPGM 
00813  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2NPGM 
                                                                                
00814      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2NPGM 
00815         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2NPGM 
00816                                                                   GA2NPGM 
00817      IF WS-DIAGNOSIS-CODE (WS-SORT-IDX) <                         GA2NPGM 
00818         WS-DIAGNOSIS-CODE (WS-SORT-IDX2)                          GA2NPGM 
00819         SET WS-SORT-IDX2  UP BY  1                                GA2NPGM 
00820         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2NPGM 
00821      ELSE                                                         GA2NPGM 
00822         IF WS-DIAGNOSIS-CODE (WS-SORT-IDX) >                      GA2NPGM 
00823            WS-DIAGNOSIS-CODE (WS-SORT-IDX2)                       GA2NPGM 
00824            MOVE WS-DIAGNOSIS-CODE (WS-SORT-IDX) TO                GA2NPGM 
00825               WS-SAVED-BENEFIT                                    GA2NPGM 
00826            MOVE WS-DIAGNOSIS-CODE (WS-SORT-IDX2) TO               GA2NPGM 
00827               WS-DIAGNOSIS-CODE (WS-SORT-IDX)                     GA2NPGM 
00828            MOVE WS-SAVED-BENEFIT TO                               GA2NPGM 
00829               WS-DIAGNOSIS-CODE (WS-SORT-IDX2)                    GA2NPGM 
00830            SET WS-SORT-IDX2  UP BY  1                             GA2NPGM 
00831            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2NPGM 
00832                                                                   GA2NPGM 
00833      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2NPGM 
00834      MOVE WS-DIAGNOSIS-CODE (WS-SORT-IDX3) TO                     GA2NPGM 
00835         WS-DIAGNOSIS-CODE (WS-SORT-IDX2).                         GA2NPGM 
00836      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2NPGM 
00837      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2NPGM 
00838                                                                   GA2NPGM 
00839  2040-ARE-WE-DONE-WITH-SORT.                                      GA2NPGM 
                                                                                
00840      MOVE '2040'  TO  WS-PARA-ID.                                 GA2NPGM 
00841      SET WS-SORT-IDX  UP BY  1.                                   GA2NPGM 
00842      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2NPGM 
00843         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2NPGM 
00844         SET WS-SORT-IDX2  UP BY  1                                GA2NPGM 
00845         MOVE '2030'  TO  WS-PARA-ID                               GA2NPGM 
00846         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2NPGM 
00847      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2NPGM 
00848      MOVE HIGH-VALUES TO WS-DIAG-CODE-ENTRY (WS-SORT-IDX).        GA2NPGM 
00849      MOVE GX9-ENTRY-COUNT  TO  GX9-ENTRY-COUNT.                   GA2NPGM 
00850                                                                   GA2NPGM 
00851      COMPUTE  WS-COPY-LENGTH  =                                   GA2NPGM 
00852                GC-GCTABULR-IDGD-VARY-MAX-OCUR *                   GA2NPGM 
00853                                    GC-GCTABULR-IDGD-VARY-LEN.     GA2NPGM 
00854                                                                   GA2NPGM 
00855      EXEC CICS                                                    GA2NPGM 
00856         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA2NPGM 
00857         LENGTH(WS-COPY-LENGTH)                                    GA2NPGM 
00858         INITIMG(WS-HEX-00)                                        GA2NPGM 
00859      END-EXEC.                                                    GA2NPGM 
00860      SET COPY-IDX,  GX9-INDEX  TO  1.                             GA2NPGM 
00861                                                                   GA2NPGM 
00862      MOVE '2050'  TO  WS-PARA-ID.                                 GA2NPGM 
                                                                                
00863  2050-MAKE-A-COPY-OF-RECORD.                                      GA2NPGM 
                                                                                
00864      MOVE GX9-ENTRIES TO COPY-TABULAR-TABLE-AREA.                 GA2NPGM 
00865                                                                   GA2NPGM 
00866      IF WS-ADD-COUNT  +  GX9-ENTRY-COUNT >                        GA2NPGM 
00867                      GC-GCTABULR-IDGD-VARY-MAX-OCUR               GA2NPGM 
00868         SET WT-01-INDEX TO +07                                    GA2NPGM 
00869         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
00870         MOVE '2N02'  TO  WS-ABEND-CODE                            GA2NPGM 
00871         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
00872                                                                   GA2NPGM 
00873      SET WS-SORT-IDX,  COPY-IDX,  GX9-INDEX  TO  1.               GA2NPGM 
00874                                                                   GA2NPGM 
00875      MOVE '2060'  TO  WS-PARA-ID.                                 GA2NPGM 
                                                                                
00876  2060-MERGE-IN-NEW-ENTRIES.                                       GA2NPGM 
                                                                                
00877      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2NPGM 
00878         SET GX9-INDEX  DOWN BY  1                                 GA2NPGM 
00879         SET GX9-ENTRY-COUNT  TO  GX9-INDEX                        GA2NPGM 
00880         MOVE GX9-ENTRY-COUNT  TO  GX9-ENTRY-COUNT                 GA2NPGM 
00881         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2NPGM 
00882                                                                   GA2NPGM 
00883      IF WS-DIAG-CODE-ENTRY (WS-SORT-IDX)                          GA2NPGM 
00884               =  HIGH-VALUES  AND                                 GA2NPGM 
00885         COPY-TABULAR-TABLE (COPY-IDX)  NOT =  HIGH-VALUES         GA2NPGM 
00886         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2NPGM 
00887                                                                   GA2NPGM 
00888      IF WS-DIAG-CODE-ENTRY (WS-SORT-IDX)                          GA2NPGM 
00889               NOT =  HIGH-VALUES AND                              GA2NPGM 
00890         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2NPGM 
00891         GO TO 2080-INSERT-NEW-ENTRY.                              GA2NPGM 
00892                                                                   GA2NPGM 
00893      IF WS-DIAG-CODE-ENTRY (WS-SORT-IDX)                          GA2NPGM 
00894               =  HIGH-VALUES AND                                  GA2NPGM 
00895         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2NPGM 
00896         NEXT SENTENCE                                             GA2NPGM 
00897      ELSE                                                         GA2NPGM 
00898         IF WS-DIAGNOSIS-CODE (WS-SORT-IDX) >                      GA2NPGM 
00899            COPY-DIAGNOSIS-CODE (COPY-IDX)                         GA2NPGM 
00900            GO TO 2070-SAVE-COPIED-ENTRY                           GA2NPGM 
00901         ELSE                                                      GA2NPGM 
00902            IF WS-DIAGNOSIS-CODE (WS-SORT-IDX) <                   GA2NPGM 
00903               COPY-DIAGNOSIS-CODE (COPY-IDX)                      GA2NPGM 
00904               GO TO 2080-INSERT-NEW-ENTRY.                        GA2NPGM 
00905                                                                   GA2NPGM 
00906 ******************************************************************GA2NPGM 
00907 **   AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2NPGM 
00908 **   OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2NPGM 
00909 **   INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2NPGM 
00910 **   FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2NPGM 
00911 ******************************************************************GA2NPGM 
00912                                                                   GA2NPGM 
00913      SET WS-SORT-IDX  UP BY  1.                                   GA2NPGM 
00914                                                                   GA2NPGM 
00915  2070-SAVE-COPIED-ENTRY.                                          GA2NPGM 
                                                                                
00916      MOVE '2070'  TO  WS-PARA-ID.                                 GA2NPGM 
00917      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA2NPGM 
00918         GX9-ENTRY (GX9-INDEX).                                    GA2NPGM 
00919      IF COPY-IDX  NOT >  GX9-ENTRY-COUNT                          GA2NPGM 
00920         SET COPY-IDX  UP BY  1                                    GA2NPGM 
00921         SET GX9-INDEX  UP BY  1                                   GA2NPGM 
00922         MOVE '2060'  TO  WS-PARA-ID                               GA2NPGM 
00923         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2NPGM 
00924      ELSE                                                         GA2NPGM 
00925         SET WT-01-INDEX TO +08                                    GA2NPGM 
00926         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
00927         MOVE '2N03'  TO  WS-ABEND-CODE                            GA2NPGM 
00928         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
00929                                                                   GA2NPGM 
00930  2080-INSERT-NEW-ENTRY.                                           GA2NPGM 
                                                                                
00931      MOVE '2080'  TO  WS-PARA-ID.                                 GA2NPGM 
00932                                                                   GA2NPGM 
00933      MOVE WS-DIAGNOSIS-CODE (WS-SORT-IDX) TO                      GA2NPGM 
00934         GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX).                       GA2NPGM 
00935      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2NPGM 
00936         SET WS-SORT-IDX  UP BY  1                                 GA2NPGM 
00937         SET GX9-INDEX  UP BY  1                                   GA2NPGM 
00938         MOVE '2060'  TO  WS-PARA-ID                               GA2NPGM 
00939         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2NPGM 
00940      ELSE                                                         GA2NPGM 
00941         SET WT-01-INDEX TO +08                                    GA2NPGM 
00942         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
00943         MOVE '2N04'  TO  WS-ABEND-CODE                            GA2NPGM 
00944         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
00945                                                                   GA2NPGM 
00946  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2NPGM 
                                                                                
00947      MOVE '2090'  TO  WS-PARA-ID.                                 GA2NPGM 
00948                                                                   GA2NPGM 
00949 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2NPGM 
00950                                                                   GA2NPGM 
00951      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2NPGM 
00952      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2NPGM 
00953                                                                   GA2NPGM 
00954      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN +       GA2NPGM 
00955                     GC-GCTABULR-IDGD-FIXED-LEN +                  GA2NPGM 
00956              (GX9-ENTRY-COUNT  *  GC-GCTABULR-IDGD-VARY-LEN).     GA2NPGM 
00957                                                                   GA2NPGM 
00958      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN   =                  GA2NPGM 
00959            GC-GCIOPARM-LEN  +  GCIO-RECORD-LENGTH.                GA2NPGM 
00960                                                                   GA2NPGM 
00961      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2NPGM 
00962         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2NPGM 
00963         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2NPGM 
00964                                                                   GA2NPGM 
00965      IF NOT GCIO-GOOD-RETURN                                      GA2NPGM 
00966         SET WT-01-INDEX TO +09                                    GA2NPGM 
00967         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
00968         MOVE '2N05'  TO  WS-ABEND-CODE                            GA2NPGM 
00969         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
00970                                                                   GA2NPGM 
00971      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2NPGM 
00972         VARYING MAP-IDX2 FROM 1  BY  1                            GA2NPGM 
00973            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2NPGM 
00974         AFTER MAP-IDX1 FROM 1  BY  1                              GA2NPGM 
00975            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2NPGM 
00976                                                                   GA2NPGM 
00977      EXEC CICS SEND   MAP('GA2NI01') MAPSET('GA2NSET') ERASE      GA2NPGM 
00978         FROM(GA2NI01O) END-EXEC.                                  GA2NPGM 
00979                                                                   GA2NPGM 
00980  2099-EXIT.   EXIT.                                               GA2NPGM 
00981 /                                                                 GA2NPGM 
00982 ******************************************************************GA2NPGM 
00983 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2NPGM 
00984 **                                                                GA2NPGM 
00985 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2NPGM 
00986 **  ALREADY ON THE OPERATORS SCREEN.                              GA2NPGM 
00987 **                                                                GA2NPGM 
00988 ******************************************************************GA2NPGM 
00989  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2NPGM 
00990                                                                   GA2NPGM 
00991      MOVE LOW-VALUES  TO                                          GA2NPGM 
00992           MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2).                GA2NPGM 
00993                                                                   GA2NPGM 
00994  2199-EXIT.   EXIT.                                               GA2NPGM 
00995 /                                                                 GA2NPGM 
00996 ******************************************************************GA2NPGM 
00997 **          X C T L   T O   D E L   S C R E E N                   GA2NPGM 
00998 **                                                                GA2NPGM 
00999 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2NPGM 
01000 ** DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2NPGM 
01001 ** PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2NPGM 
01002 ** TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2NPGM 
01003 ** THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2NPGM 
01004 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2NPGM 
01005 ******************************************************************GA2NPGM 
01006  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2NPGM 
                                                                                
01007      MOVE '3000'  TO  WS-PARA-ID.                                 GA2NPGM 
01008                                                                   GA2NPGM 
01009      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2NPGM 
01010            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA2NPGM 
01011            GC-GCTABULR-IDGD-FIXED-LEN +                           GA2NPGM 
01012      (GC-GCTABULR-IDGD-VARY-LEN * GC-GCTABULR-IDGD-VARY-MAX-OCUR).GA2NPGM 
01013                                                                   GA2NPGM 
01014      EXEC CICS                                                    GA2NPGM 
01015         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2NPGM 
01016         INITIMG(WS-HEX-00)                                        GA2NPGM 
01017         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2NPGM 
01018      END-EXEC.                                                    GA2NPGM 
01019                                                                   GA2NPGM 
01020 *    EXEC CICS                                                    GA2NPGM 
01021 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2NPGM 
01022 *       INITIMG(WS-HEX-00)                                        GA2NPGM 
01023 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2NPGM 
01024 *    END-EXEC.                                                    GA2NPGM 
01025                                                                   GA2NPGM 
01026      IF  FRMNUIDI  =  'GS3A'                                      GA2NPGM 
01027         MOVE SPACES  TO  GCIO-WORKFILE-KEY                        GA2NPGM 
01028         MOVE   'G'   TO  GCIO-WRK-STATUS-CODE                     GA2NPGM 
01029         MOVE   'G4'  TO  GCIO-WRK-RECORD-TYPE                     GA2NPGM 
01030 ****    MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA2NPGM 
01031         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2NPGM 
01032         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2NPGM 
01033         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2NPGM 
01034         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2NPGM 
01035         MOVE  SPACES  TO  GCIO-WRK-LINE-OF-BUS                    GA2NPGM 
01036                           GCIO-WRK-PROVIDER-CONTROL               GA2NPGM 
01037         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2NPGM 
01038         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2NPGM 
01039                                                                   GA2NPGM 
01040      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2NPGM 
01041         MOVE SPACES  TO  GCIO-WORKFILE-KEY                        GA2NPGM 
01042         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2NPGM 
01043         MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE                     GA2NPGM 
01044 ****    MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA2NPGM 
01045         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2NPGM 
01046         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2NPGM 
01047         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2NPGM 
01048         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2NPGM 
01049         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2NPGM 
01050         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2NPGM 
01051         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2NPGM 
01052         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2NPGM 
01053                                                                   GA2NPGM 
01054      IF  FRMNUIDI  =  'GC8A'                                      GA2NPGM 
01055         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2NPGM 
01056         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2NPGM 
01057         MOVE   'C6'  TO  GCIO-WRK-RECORD-TYPE                     GA2NPGM 
01058 ****    MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA2NPGM 
01059         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2NPGM 
01060         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2NPGM 
01061         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2NPGM 
01062         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2NPGM 
01063         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2NPGM 
01064         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2NPGM 
01065         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2NPGM 
01066         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                 GA2NPGM 
01067         MOVE  GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID.           GA2NPGM 
01068                                                                   GA2NPGM 
01069      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2NPGM 
01070      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2NPGM 
01071      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2NPGM 
01072      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2NPGM 
01073      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2NPGM 
01074      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2NPGM 
01075      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2NPGM 
01076      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2NPGM 
01077      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2NPGM 
01078      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2NPGM 
01079      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2NPGM 
01080      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2NPGM 
01081                                                                   GA2NPGM 
01082      MOVE INCEXCI TO GCA-I-E-INDC.                                GA2NPGM 
01083                                                                   GA2NPGM 
01084      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2NPGM 
01085 *    MOVE  SPACES  TO  GCA-EFFECTIVE-DATE.                        GA2NPGM 
01086      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2NPGM 
01087                                                                   GA2NPGM 
01088      SET GCA-RECORD-POINTER                                       GA2NPGM 
01089        TO ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD.                 GA2NPGM 
01090                                                                   GA2NPGM 
01091      MOVE GC-GCTABULR-IDGD-VARY-MAX-OCUR                          GA2NPGM 
01092        TO GX9-ENTRY-COUNT.                                        GA2NPGM 
01093                                                                   GA2NPGM 
01094      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2NPGM 
01095                                                                   GA2NPGM 
01096      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2NPGM 
01097         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2NPGM 
01098         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2NPGM 
01099                                                                   GA2NPGM 
01100      IF  NOT GCIO-GOOD-RETURN                                     GA2NPGM 
01101         SET WT-01-INDEX TO +10                                    GA2NPGM 
01102         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
01103         MOVE '2N06'  TO  WS-ABEND-CODE                            GA2NPGM 
01104         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
01105                                                                   GA2NPGM 
01106 *    SET COMMAREA-PNTR                                            GA2NPGM 
01107 *      TO ADDRESS OF GCA-COMMAREA.                                GA2NPGM 
01108                                                                   GA2NPGM 
01109 *    EXEC CICS XCTL  PROGRAM('GA1NPGM') COMMAREA(COMMAREA-PNTR)   GA2NPGM 
01110 *       LENGTH(4)  END-EXEC.                                      GA2NPGM 
01111      EXEC CICS XCTL  PROGRAM('GA1NPGM')                           GA2NPGM 
01112                      COMMAREA(DFHCOMMAREA)                        GA2NPGM 
01113                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2NPGM 
01114      END-EXEC.                                                    GA2NPGM 
01115                                                                   GA2NPGM 
01116  3099-EXIT.   EXIT.                                               GA2NPGM 
01117 /                                                                 GA2NPGM 
01118 ***************************************************************** GA2NPGM 
01119 **          D I S P L A Y   F I R S T   S C R E E N               GA2NPGM 
01120 **                                                                GA2NPGM 
01121 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2NPGM 
01122 ** OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2NPGM 
01123 ** THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2NPGM 
01124 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2NPGM 
01125 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2NPGM 
01126 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2NPGM 
01127 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2NPGM 
01128 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2NPGM 
01129 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2NPGM 
01130 ******************************************************************GA2NPGM 
01131  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2NPGM 
                                                                                
01132      MOVE '4000'  TO  WS-PARA-ID.                                 GA2NPGM 
01133                                                                   GA2NPGM 
01134 ***  D185     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY         GA2NPGM 
01135 *                                                                 GA2NPGM 
01136      MOVE LOW-VALUES TO GA2NI01I.                                 GA2NPGM 
01137                                                                   GA2NPGM 
01138      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA2NPGM 
01139         SET WT-01-INDEX TO +11                                    GA2NPGM 
01140         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
01141         MOVE '2N07'  TO  WS-ABEND-CODE                            GA2NPGM 
01142         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
01143                                                                   GA2NPGM 
01144 *    SET ADDRESS OF GCA-COMMAREA                                  GA2NPGM 
01145 *      TO INCOMING-COMMAREA-PNTR.                                 GA2NPGM 
01146                                                                   GA2NPGM 
01147      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA2NPGM 
01148      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA2NPGM 
01149      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA2NPGM 
01150      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA2NPGM 
01151      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA2NPGM 
01152      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA2NPGM 
01153      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA2NPGM 
01154      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA2NPGM 
01155                                                                   GA2NPGM 
01156      MOVE GCA-I-E-INDC TO INCEXCO,                                GA2NPGM 
01157                        INEXDRKO.                                  GA2NPGM 
01158                                                                   GA2NPGM 
01159      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2NPGM 
01160         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA2NPGM 
01161 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2NPGM 
01162         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2NPGM 
01163         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2NPGM 
01164         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2NPGM 
01165         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2NPGM 
01166         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2NPGM 
01167         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2NPGM 
01168         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2NPGM 
01169         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2NPGM 
01170         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2NPGM 
01171         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2NPGM 
01172         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2NPGM 
01173         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2NPGM 
01174                                                                   GA2NPGM 
01175      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2NPGM 
01176         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA2NPGM 
01177 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2NPGM 
01178         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2NPGM 
01179         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2NPGM 
01180         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2NPGM 
01181         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2NPGM 
01182         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2NPGM 
01183         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2NPGM 
01184         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2NPGM 
01185         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2NPGM 
01186         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2NPGM 
01187         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2NPGM 
01188         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2NPGM 
01189         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2NPGM 
01190         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2NPGM 
01191         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2NPGM 
01192         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2NPGM 
01193         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2NPGM 
01194                                                                   GA2NPGM 
01195      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2NPGM 
01196         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA2NPGM 
01197         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2NPGM 
01198         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2NPGM 
01199         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2NPGM 
01200         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2NPGM 
01201         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2NPGM 
01202         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2NPGM 
01203         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2NPGM 
01204         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2NPGM 
01205         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2NPGM 
01206         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2NPGM 
01207         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2NPGM 
01208         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2NPGM 
01209         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2NPGM 
01210         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2NPGM 
01211         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2NPGM 
01212         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2NPGM 
01213         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2NPGM 
01214         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2NPGM 
01215                                                                   GA2NPGM 
01216      EXEC CICS SEND   MAP('GA2NI01') MAPSET('GA2NSET') ERASE      GA2NPGM 
01217         FROM(GA2NI01O) END-EXEC.                                  GA2NPGM 
01218                                                                   GA2NPGM 
01219  4099-EXIT.   EXIT.                                               GA2NPGM 
01220 /                                                                 GA2NPGM 
01221 ***************************************************************** GA2NPGM 
01222 **        X C T L   T O   P R E V I O U S   M E N U               GA2NPGM 
01223 **                                                                GA2NPGM 
01224 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2NPGM 
01225 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2NPGM 
01226 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2NPGM 
01227 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2NPGM 
01228 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2NPGM 
01229 ******************************************************************GA2NPGM 
01230  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2NPGM 
                                                                                
01231      MOVE '5000'  TO  WS-PARA-ID.                                 GA2NPGM 
01233                                                                   GA2NPGM 
01234 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA2NPGM 
01235 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA2NPGM 
01236 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA2NPGM 
01237                                                                   GA2NPGM 
01238      IF  ALTBFNCI  =  'GTM1'                                      GA2NPGM 
01239          EXEC CICS XCTL                                           GA2NPGM 
01240                    PROGRAM('GTM1PGM')                             GA2NPGM 
01241                    END-EXEC.                                      GA2NPGM 
01242                                                                   GA2NPGM 
01243                                                                   GA2NPGM 
01244      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2NPGM 
01245            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA2NPGM 
01246            GC-GCTABULR-ABM-FIXED-LEN     +                        GA2NPGM 
01247           (GC-GCTABULR-ABM-VARY-LEN  *                            GA2NPGM 
01248               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA2NPGM 
01249                                                                   GA2NPGM 
01250      EXEC CICS                                                    GA2NPGM 
01251         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA2NPGM 
01252         INITIMG(WS-HEX-00)                                        GA2NPGM 
01253         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA2NPGM 
01254      END-EXEC.                                                    GA2NPGM 
01255                                                                   GA2NPGM 
01256 *    EXEC CICS                                                    GA2NPGM 
01257 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2NPGM 
01258 *       INITIMG(WS-HEX-00)                                        GA2NPGM 
01259 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2NPGM 
01260 *    END-EXEC.                                                    GA2NPGM 
01261                                                                   GA2NPGM 
01262      IF  FRMNUIDI  =  'GS3A'                                      GA2NPGM 
01263         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2NPGM 
01264         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2NPGM 
01265         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA2NPGM 
01266         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2NPGM 
01267         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2NPGM 
01268         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2NPGM 
01269         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2NPGM 
01270         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2NPGM 
01271                          GCIO-WRK-PROVIDER-CONTROL                GA2NPGM 
01272         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2NPGM 
01273         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2NPGM 
01274         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2NPGM 
01275         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA2NPGM 
01276                            GCA-ALL-LEVEL-TAB-ID                   GA2NPGM 
01277         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA2NPGM 
01278                            GCA-ALL-LEVEL-TAB-SLOT                 GA2NPGM 
01279         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA2NPGM 
01280                            GCA-INTERNAL-TAB-ID,                   GA2NPGM 
01281                            GCA-INTERNAL-TAB-SLOT                  GA2NPGM 
01282         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2NPGM 
01283                                                                   GA2NPGM 
01284      IF  FRMNUIDI  =  'GC4A'                                      GA2NPGM 
01285         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2NPGM 
01286         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2NPGM 
01287         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2NPGM 
01288         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2NPGM 
01289         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2NPGM 
01290         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2NPGM 
01291         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2NPGM 
01292         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2NPGM 
01293         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2NPGM 
01294         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2NPGM 
01295         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2NPGM 
01296         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2NPGM 
01297         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA2NPGM 
01298                            GCA-ALL-LEVEL-TAB-ID                   GA2NPGM 
01299         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA2NPGM 
01300                            GCA-ALL-LEVEL-TAB-SLOT                 GA2NPGM 
01301         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA2NPGM 
01302                            GCA-INTERNAL-TAB-ID,                   GA2NPGM 
01303                            GCA-INTERNAL-TAB-SLOT                  GA2NPGM 
01304         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2NPGM 
01305                                                                   GA2NPGM 
01306      IF  FRMNUIDI  =  'GC8A'                                      GA2NPGM 
01307         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2NPGM 
01308         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2NPGM 
01309         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA2NPGM 
01310         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2NPGM 
01311         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2NPGM 
01312         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2NPGM 
01313         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2NPGM 
01314         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2NPGM 
01315         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2NPGM 
01316         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2NPGM 
01317         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2NPGM 
01318         MOVE GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID            GA2NPGM 
01319         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2NPGM 
01320         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA2NPGM 
01321                            GCA-ALL-LEVEL-TAB-ID                   GA2NPGM 
01322         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA2NPGM 
01323                            GCA-ALL-LEVEL-TAB-SLOT                 GA2NPGM 
01324         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA2NPGM 
01325                          GCA-INTERNAL-TAB-SLOT.                   GA2NPGM 
01326                                                                   GA2NPGM 
01327      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.             GA2NPGM 
01328 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA2NPGM 
01329 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA2NPGM 
01330 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA2NPGM 
01331 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA2NPGM 
01332 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA2NPGM 
01333      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2NPGM 
01334                                                                   GA2NPGM 
01335      SET GCA-RECORD-POINTER                                       GA2NPGM 
01336        TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                    GA2NPGM 
01337                                                                   GA2NPGM 
01338      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GA2NPGM 
01339        TO GAA-ENTRY-COUNT.                                        GA2NPGM 
01340                                                                   GA2NPGM 
01341      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2NPGM 
01342                                                                   GA2NPGM 
01343      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2NPGM 
01344         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA2NPGM 
01345         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA2NPGM 
01346                                                                   GA2NPGM 
01347      IF  NOT GCIO2-GOOD-RETURN                                    GA2NPGM 
01348         SET WT-01-INDEX TO +06                                    GA2NPGM 
01349         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2NPGM 
01350         MOVE '2N08'  TO  WS-ABEND-CODE                            GA2NPGM 
01351         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2NPGM 
01352                                                                   GA2NPGM 
01353 *    SET COMMAREA-PNTR                                            GA2NPGM 
01354 *      TO ADDRESS OF GCA-COMMAREA.                                GA2NPGM 
01355                                                                   GA2NPGM 
01356      IF  ALTBFNCI  =  'GA1B'                                      GA2NPGM 
01357 *       EXEC CICS                                                 GA2NPGM 
01358 *          XCTL  PROGRAM('GA1BPGM')                               GA2NPGM 
01359 *          COMMAREA(COMMAREA-PNTR)                                GA2NPGM 
01360 *          LENGTH(4)                                              GA2NPGM 
01361 *       END-EXEC.                                                 GA2NPGM 
01362         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA2NPGM 
01363                         COMMAREA(DFHCOMMAREA)                     GA2NPGM 
01364                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2NPGM 
01365         END-EXEC.                                                 GA2NPGM 
01366                                                                   GA2NPGM 
01367      IF  ALTBFNCI  =  'GA1C'                                      GA2NPGM 
01368 *       EXEC CICS                                                 GA2NPGM 
01369 *          XCTL  PROGRAM('GA1CPGM')                               GA2NPGM 
01370 *          COMMAREA(COMMAREA-PNTR)                                GA2NPGM 
01371 *          LENGTH(4)                                              GA2NPGM 
01372 *       END-EXEC.                                                 GA2NPGM 
01373         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA2NPGM 
01374                         COMMAREA(DFHCOMMAREA)                     GA2NPGM 
01375                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2NPGM 
01376         END-EXEC.                                                 GA2NPGM 
01377                                                                   GA2NPGM 
01378      IF  ALTBFNCI  =  'GA1D'                                      GA2NPGM 
01379 *       EXEC CICS                                                 GA2NPGM 
01380 *          XCTL  PROGRAM('GA1DPGM')                               GA2NPGM 
01381 *          COMMAREA(COMMAREA-PNTR)                                GA2NPGM 
01382 *          LENGTH(4)                                              GA2NPGM 
01383 *       END-EXEC.                                                 GA2NPGM 
01384         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA2NPGM 
01385                         COMMAREA(DFHCOMMAREA)                     GA2NPGM 
01386                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2NPGM 
01387         END-EXEC.                                                 GA2NPGM 
01388                                                                   GA2NPGM 
01389      IF  ALTBFNCI  =  'GA1E'                                      GA2NPGM 
01390 *       EXEC CICS                                                 GA2NPGM 
01391 *          XCTL  PROGRAM('GA1EPGM')                               GA2NPGM 
01392 *          COMMAREA(COMMAREA-PNTR)                                GA2NPGM 
01393 *          LENGTH(4)                                              GA2NPGM 
01394 *       END-EXEC.                                                 GA2NPGM 
01395         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA2NPGM 
01396                         COMMAREA(DFHCOMMAREA)                     GA2NPGM 
01397                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2NPGM 
01398         END-EXEC.                                                 GA2NPGM 
01399                                                                   GA2NPGM 
01400      IF  ALTBFNCI  =  'GA1P'                                      GA2NPGM 
01401         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA2NPGM 
01402                         COMMAREA(DFHCOMMAREA)                     GA2NPGM 
01403                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2NPGM 
01404         END-EXEC.                                                 GA2NPGM 
01405                                                                   GA2NPGM 
01406  5099-EXIT.                                                       GA2NPGM 
01407      EXIT.                                                        GA2NPGM 
01408 /                                                                 GA2NPGM 
01409 ***************************************************************** GA2NPGM 
01410 **           X C T L   T O   M A I N   M E N U                    GA2NPGM 
01411 **                                                                GA2NPGM 
01412 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2NPGM 
01413 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2NPGM 
01414 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2NPGM 
01415 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2NPGM 
01416 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2NPGM 
01417 ** AND PROGRESS DOWN.                                             GA2NPGM 
01418 ******************************************************************GA2NPGM 
01419  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2NPGM 
                                                                                
01420      MOVE '6000'  TO  WS-PARA-ID.                                 GA2NPGM 
01421      MOVE '2N09'  TO  WS-ABEND-CODE.                              GA2NPGM 
01422                                                                   GA2NPGM 
01423      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2NPGM 
01424                                                                   GA2NPGM 
01425  6099-EXIT.     EXIT.                                             GA2NPGM 
01426 /***************************************************************  GA2NPGM 
01427 *                                                              *  GA2NPGM 
01428 * 9000   MOVE MESSAGE TO SCREEN                                *  GA2NPGM 
01429 *                                                              *  GA2NPGM 
01430 ****************************************************************  GA2NPGM 
01431  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA2NPGM 
01432  9000-010.                                                        GA2NPGM 
01433                                                                   GA2NPGM 
01434          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                     GA2NPGM 
01435                    TO ERRMSGO.                                    GA2NPGM 
01436                                                                   GA2NPGM 
01437  9000-900-EXIT.                                                   GA2NPGM 
01438      EXIT.                                                        GA2NPGM 
01439 /*****************************************************************GA2NPGM 
01440  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2NPGM 
01441                                                                   GA2NPGM 
01442      SET MAP-IDX1  TO  7.                                         GA2NPGM 
01443      SET MAP-IDX2  TO  1.                                         GA2NPGM 
01444      MOVE -1  TO                                                  GA2NPGM 
01445         MAP-DIAGNOSIS-CODE-LEN (MAP-IDX1, MAP-IDX2).              GA2NPGM 
01446      EXEC CICS SEND   MAP('GA2NI01') MAPSET('GA2NSET') ERASE      GA2NPGM 
01447         FROM(GA2NI01O) CURSOR WAIT END-EXEC.                      GA2NPGM 
01448                                                                   GA2NPGM 
01449      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2NPGM 
01450                                                                   GA2NPGM 
01451  9999-EXIT.     EXIT.                                             GA2NPGM 
