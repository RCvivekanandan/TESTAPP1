00001  ID DIVISION.                                                     01/12/06
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA2OPGM 
00003  PROGRAM-ID.     GA2OPGM.                                            LV004
00004  AUTHOR.         GARY D MULLINGS.                                 GA2OPGM 
00005  DATE-WRITTEN.   10/25/89.                                        GA2OPGM 
00006  DATE-COMPILED.                                                   GA2OPGM 
00007      SKIP3                                                        GA2OPGM 
00008 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2OPGM 
00009 *******P R O G R A M   M O D I F I C A T I O N    S T A T U S ****GA2OPGM 
00010 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2OPGM 
00011 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2OPGM 
00012 *                                                                 GA2OPGM 
00013 *   D185    10/25/89    GDM   CREATES BASIC SKELETON RECORD FOR   GA2OPGM 
00014 *                             THE INTERNAL TABULAR RECORD #IPGP.  GA2OPGM 
00015 *                                                                 GA2OPGM 
00016 *           01/29/90    ENW   CHANGED ALL GX5 TO GXA.             GA2OPGM 
00017 *                                                                 GA2OPGM 
00018 * 11154  1/07/91  NGE  1. CHANGE GCA-I-E-  FIELD IN COPYBOOKS   * GA2OPGM 
00019 *                         G2ALCKEC AND G2ALCKE2.                * GA2OPGM 
00020 *                                                                 GA2OPGM 
00021 * 11154     03/02/91    ENW   CORRECTED GCPPDIO LOGIC. IT SHOULD* GA2OPGM 
00022 *                             HAVE BEEN VALIDATING PROCEDURE    * GA2OPGM 
00023 *                             CODES.                            * GA2OPGM 
00024 *                                                               * GA2OPGM 
00025 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD     * GA2OPGM 
00026 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2OPGM 
00027 *                                                               * GA2OPGM 
00028 *14726/ 11/12/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000 AND * GA2OPGM 
00029 *15057                  THE EXPANSION OF THE GROUP SPECIFIC AND * GA2OPGM 
00030 *                       CONTRACT KEY TO SUPPORT THE TEXAS       * GA2OPGM 
00031 *                       MERGER.                                 * GA2OPGM 
00032 *                                                               * GA2OPGM 
00033 * 14726/  04/11/98  AB   EXPANDED THE SCREEN / MAP               *GA2OPGM 
00034 * 15057                  TO INCLUDE THE ENTIRE KEY               *GA2OPGM 
00035 *                                                               * GA2OPGM 
00036 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP      * GA2OPGM 
00037 *                        2. ADD DELADD-OPTION = 'GAS5UPD'       * GA2OPGM 
00038 *                                                               * GA2OPGM 
00039 * P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN      * GA2OPGM 
00040 *                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.  * GA2OPGM 
00041 *                                                               * GA2OPGM 
00042 *                                                               * GA2OPGM 
00043 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA2OPGM 
00044 *                                                               * GA2OPGM 
00045 *   D365A   05/06/03    GTF EXPAND PROCEDURE ARGUMENT FROM 6 TO * GA2OPGM 
00046 *                           7 BYTES. CHANGE # OF OCCURS TO 1109 * GA2OPGM 
00047 *                           ON #IPGP TABULAR.                   * GA2OPGM 
00049 *                                                               * GA2OPGM 
00050 *   D365A   05/22/03    GTF REMOVE FILLER BYTE FROM MAP REDEFIN-* GA2OPGM 
00051 *                           ITION IN WORKING STORAGE.           * GA2OPGM 
00048 *                                                               * GA2OPGM 
00048 *ICD-10  07/06/11  BA EXPAND PROCED-CODE FROM 5 TO 7 BYTES.     * GA2OPGM 
00048 *                     EXPAND WS-SAVED-BENEFIT FROM 6 TO 7 BYTES.* GA2OPGM 
00048 *                     CHANGE LOGIC FOR ICD-10 REQUIREMENTS.     * GA2OPGM 
SI0724*                                                                *00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00009180
SI0724*                           GCCDRLEN                             *00009190
00052 ***************************************************************** GA2OPGM 
00053 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2OPGM 
00055 /                                                                 GA2OPGM 
00056 ******************************************************************GA2OPGM 
00057 *   GA2OPGM      ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM   GA2OPGM 
00058 *               PROCEDURE GROUP BY PROCEDURE CODE - GA2O          GA2OPGM 
00059 *                                                                 GA2OPGM 
00060 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2OPGM 
00061 *   TABULAR PROCEDURE CODES.                                      GA2OPGM 
00062 *                                                                 GA2OPGM 
00063 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2OPGM 
00064 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2OPGM 
00065 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2OPGM 
00066 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2OPGM 
00067 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2OPGM 
00068 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2OPGM 
00069 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2OPGM 
00070 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2OPGM 
00071 *                                                                 GA2OPGM 
00072 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IPGP)GA2OPGM 
00073 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2OPGM 
00074 *   XCTL TO TRANS GA1O OR PROGRAM GA1OPGM.                        GA2OPGM 
00075 *                                                                 GA2OPGM 
00076 *   FUNC CODE: GA2O                                               GA2OPGM 
00077 *   MAPSET:    GA2OSETC                                           GA2OPGM 
00078 *   FILES:     GCPSWORK                                           GA2OPGM 
00079 ******************************************************************GA2OPGM 
00080      SKIP3                                                        GA2OPGM 
00081  ENVIRONMENT DIVISION.                                            GA2OPGM 
00082 /                                                                 GA2OPGM 
00083  DATA DIVISION.                                                   GA2OPGM 
00084  WORKING-STORAGE SECTION.                                         GA2OPGM 
00085  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2OPGM 
00086      '***GA2OPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2OPGM 
00087  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2OPGM 
00088                                                                   GA2OPGM 
00089  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2OPGM 
00090                                                                   GA2OPGM 
00091  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2OPGM 
00092                                                                   GA2OPGM 
00093  01  PROCED-KEY.                                                  GA2OPGM 
00094      03  SYSTEM-INDICATOR        PIC X(1) VALUE SPACE.            GA2OPGM 
00095      03  PROCED-CODE             PIC X(7).                        GA2OPGM 
00096      03  PROCEDR-DIGIT REDEFINES PROCED-CODE                      GA2OPGM 
00097              OCCURS 7 TIMES      PIC X(1).                        GA2OPGM 
00098                                                                   GA2OPGM 
00099  01  COMMAREA-POINTER-AREA.                                       GA2OPGM 
00100      05  COMMAREA-PNTR-COMP                      PIC S9(08)  COMP.GA2OPGM 
00101      05  COMMAREA-PNTR  REDEFINES                                 GA2OPGM 
00102                             COMMAREA-PNTR-COMP USAGE IS POINTER.  GA2OPGM 
00103                                                                   GA2OPGM 
00104  01  INTERNAL-POINTER-AREA.                                       GA2OPGM 
00105      05  INTERNAL-TAB-PNTR-COMP                  PIC S9(08)  COMP.GA2OPGM 
00106      05  INTERNAL-TAB-PNTR       REDEFINES                        GA2OPGM 
00107                        INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.   GA2OPGM 
00108                                                                   GA2OPGM 
00109      05  WS-GCA-RECORD-POINTER             COMP  PIC S9(08)  SYNC.GA2OPGM 
00110      05  WS-GCA-RECORD-PNTR         REDEFINES                     GA2OPGM 
00111                     WS-GCA-RECORD-POINTER USAGE IS POINTER.       GA2OPGM 
00112                                                                   GA2OPGM 
00113 ** MAP COBOL SCREEN DSECTS **                                     GA2OPGM 
00114  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2OPGM 
00115      '***  I/O MAPAREA ***'.                                      GA2OPGM 
00116  COPY GA2OSETC.                                                   GA2OPGM 
00117 /                                                                 GA2OPGM 
00118 ******************************************************************GA2OPGM 
00119 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2OPGM 
00120 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2OPGM 
00121 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2OPGM 
00122 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2OPGM 
00123 **  REDEFINES.                                                    GA2OPGM 
00124 ****************************************************************  GA2OPGM 
00125      SKIP3                                                        GA2OPGM 
00126  01  FILLER     REDEFINES   GA2OI01I.                             GA2OPGM 
00127      05  FILLER                              PIC X(89).           GA2OPGM 
00128      05  GROUP-SPECIFIC-ID-LINE.                                  GA2OPGM 
00129          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA2OPGM 
00130          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA2OPGM 
00131          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA2OPGM 
00132          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA2OPGM 
00133          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA2OPGM 
00134          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA2OPGM 
00135          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA2OPGM 
00136          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA2OPGM 
00137          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA2OPGM 
00138          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA2OPGM 
00139          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA2OPGM 
00140          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA2OPGM 
00141          10  FILLER                          PIC X(16).           GA2OPGM 
00142      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA2OPGM 
00143          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2OPGM 
00144          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2OPGM 
00145          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2OPGM 
00146          10  CONTRACT-GROUP-NO               PIC X(9).            GA2OPGM 
00147          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2OPGM 
00148          10  CONTRACT-SECTION-NO             PIC X(5).            GA2OPGM 
00149          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2OPGM 
00150          10  CONTRACT-PKG-CODE               PIC X(3).            GA2OPGM 
00151          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2OPGM 
00152          10  CONTRACT-LOB                    PIC X.               GA2OPGM 
00153          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2OPGM 
00154          10  CONTRACT-PROV-CTL               PIC XX.              GA2OPGM 
00155          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2OPGM 
00156          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2OPGM 
00157          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2OPGM 
00158          10  CONTRACT-EFF-DATE               PIC X(6).            GA2OPGM 
00159          10  FILLER                          PIC X(1).            GA2OPGM 
00160      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA2OPGM 
00161                                     GROUP-SPECIFIC-ID-LINE.       GA2OPGM 
00162          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA2OPGM 
00163          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA2OPGM 
00164          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA2OPGM 
00165          10  BEN-PROV-GROUP-NO               PIC X(9).            GA2OPGM 
00166          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA2OPGM 
00167          10  BEN-PROV-SECTION-NO             PIC X(5).            GA2OPGM 
00168          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA2OPGM 
00169          10  BEN-PROV-PKG-CODE               PIC X(3).            GA2OPGM 
00170          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA2OPGM 
00171          10  BEN-PROV-LOB                    PIC X.               GA2OPGM 
00172          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA2OPGM 
00173          10  BEN-PROV-PROV-CTL               PIC XX.              GA2OPGM 
00174          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA2OPGM 
00175          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA2OPGM 
00176          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA2OPGM 
00177          10  BEN-PROV-EFF-DATE               PIC X(6).            GA2OPGM 
00178          10  BEN-PROV-ID-HEADING             PIC X(6).            GA2OPGM 
00179          10  BEN-PROV-ID-NO                  PIC X(6).            GA2OPGM 
00180          10  FILLER                          PIC X(4).            GA2OPGM 
00181      05  FILLER                              PIC X(78).           GA2OPGM 
00182      05  MAP-PROCEDURE-CODE-ROW         OCCURS 14 TIMES INDEXED   GA2OPGM 
00183          BY MAP-IDX1.                                             GA2OPGM 
00184        10  MAP-PROCEDURE-CODE-COL         OCCURS 3 TIMES INDEXED  GA2OPGM 
00185            BY MAP-IDX2.                                           GA2OPGM 
00186          15  MAP-PROCEDURE-CODE-LEN          PIC S9(4) COMP SYNC. GA2OPGM 
00187          15  MAP-PROCEDURE-CODE-ATTR         PIC X.               GA2OPGM 
00188          15  MAP-PROCEDURE-CODE              PIC X(7).            GA2OPGM 
00189 *        15  FILLER                          PIC X.               GA2OPGM 
00190      SKIP3                                                        GA2OPGM 
00191  01  FILLER.                                                      GA2OPGM 
00192 ****************************************************************  GA2OPGM 
00193 **   FIELDS DESCRIBING NUMBER OF OCCURS FOR MAP.                  GA2OPGM 
00194 ****************************************************************  GA2OPGM 
00195      05  WS-MAP-ROW                  PIC S999 COMP    VALUE +14.  GA2OPGM 
00196      05  WS-MAP-COL                  PIC S999 COMP    VALUE +3.   GA2OPGM 
00197 /                                                                 GA2OPGM 
00198 ** ALTERNATIVE WORKFILE KEYS **                                   GA2OPGM 
00199  01  FILLER                      PIC X(32)  VALUE                 GA2OPGM 
00200      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2OPGM 
00201  01  WS-ALT-WORKFILE-KEYS.                                        GA2OPGM 
00202  COPY GCWRKKEY.                                                   GA2OPGM 
00203 /                                                                 GA2OPGM 
00204 ** WORKFIELDS **                                                  GA2OPGM 
00205  01  FILLER                           PIC X(16)                   GA2OPGM 
00206               VALUE '** WORKFIELDS **'.                           GA2OPGM 
00207  01  WS-WORK-FIELDS.                                              GA2OPGM 
00208      05  WS-HEX-00                    PIC X VALUE LOW-VALUES.     GA2OPGM 
00209      05  WS-ADD-COUNT                 PIC 999 COMP VALUE ZEROES.  GA2OPGM 
00210      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2OPGM 
00211        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2OPGM 
00212      05  WS-SAVED-FIELDS.                                         GA2OPGM 
00213        10  WS-SAVED-BENEFIT                PIC X(7).              GA2OPGM 
00213        10  WS-SAVED-PROCEDURE              PIC X(7).              GA2OPGM 
00214      05  WS-DIAG-CODE-ENTRY         OCCURS 43 TIMES INDEXED BY    GA2OPGM 
00215          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2OPGM 
00216        10  WS-PROCEDURE-CODE              PIC X(7).               GA2OPGM 
00217 /                                                                 GA2OPGM 
00218 *** SWITCHES ***                                                  GA2OPGM 
00219  01  FILLER                           PIC X(14)                   GA2OPGM 
00220               VALUE '** SWITCHES **'.                             GA2OPGM 
00221  01  WS-SWITCHES.                                                 GA2OPGM 
00222      05  WS-ERROR-SW                  PIC X.                      GA2OPGM 
00223                                                                   GA2OPGM 
00224 ** TITLE LINES **                                                 GA2OPGM 
00225  01  WS-TITLE-LINES.                                              GA2OPGM 
00226      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA2OPGM 
00227          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA2OPGM 
00228      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA2OPGM 
00229          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA2OPGM 
00230      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA2OPGM 
00231          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA2OPGM 
00232                                                                   GA2OPGM 
00233 *** RECORD LENGTHS ***                                            GA2OPGM 
00234  01  FILLER                           PIC X(20)                   GA2OPGM 
00235               VALUE '** RECORD LENGTHS **'.                       GA2OPGM 
00236  01  WS-RECORD-LENGTHS.                                           GA2OPGM 
00237     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP VALUE ZEROES.GA2OPGM 
00238     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP VALUE ZEROES.GA2OPGM 
00239     05 WS-COPY-LENGTH                 PIC S9(4) COMP VALUE ZEROES.GA2OPGM 
00240     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2OPGM 
00241 ****************************************************************  GA2OPGM 
00242 /                                                                 GA2OPGM 
00243  01  WT-00-GA2OPGM-TABLES.                                        GA2OPGM 
00244      05  FILLER                   PIC X(16)  VALUE                GA2OPGM 
00245          '*GA2OPGM TABLES*'.                                      GA2OPGM 
00246                                                                   GA2OPGM 
00247  01  WT-01-TABLE.                                                 GA2OPGM 
00248      05  FILLER                  PIC X(16) VALUE                  GA2OPGM 
00249          '* WT-01-TABLE  *'.                                      GA2OPGM 
00250 ******************************************************************GA2OPGM 
00251 *    WT-01   MESSAGE TABLE                                       *GA2OPGM 
00252 ******************************************************************GA2OPGM 
00253  01  FILLER.                                                      GA2OPGM 
00254      05  WT-01-MESSAGE-VALUES.                                    GA2OPGM 
00255                                                                   GA2OPGM 
00256 *----------------------------------------------------------------*GA2OPGM 
00257          10  WT-01-ENTRY-001.                                     GA2OPGM 
00258              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00259              15  WT-01-MESSAGE-TEXT-001.                          GA2OPGM 
00260                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00261                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00262                  20  FILLER          PIC X(3)  VALUE  '001'.      GA2OPGM 
00263                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00264                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00265                           '** INVALID REQUEST. THE PF KEY USED HASGA2OPGM 
00266 -                   ' NO MEANING TO THIS PROGRAM **'.             GA2OPGM 
00267              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00268                                                                   GA2OPGM 
00269 *----------------------------------------------------------------*GA2OPGM 
00270          10  WT-01-ENTRY-002.                                     GA2OPGM 
00271              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00272              15  WT-01-MESSAGE-TEXT-002.                          GA2OPGM 
00273                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00274                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00275                  20  FILLER          PIC X(3)  VALUE  '002'.      GA2OPGM 
00276                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00277                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00278                      '** FUTURE USE **'.                          GA2OPGM 
00279              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00280                                                                   GA2OPGM 
00281 *----------------------------------------------------------------*GA2OPGM 
00282          10  WT-01-ENTRY-003.                                     GA2OPGM 
00283              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00284              15  WT-01-MESSAGE-TEXT-003.                          GA2OPGM 
00285                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00286                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00287                  20  FILLER          PIC X(3)  VALUE  '003'.      GA2OPGM 
00288                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00289                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00290                      '** PROCEDURE CODE IS INVALID **'.           GA2OPGM 
00291              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00292 *----------------------------------------------------------------*GA2OPGM 
00293          10  WT-01-ENTRY-004.                                     GA2OPGM 
00294              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00295              15  WT-01-MESSAGE-TEXT-004.                          GA2OPGM 
00296                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00297                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00298                  20  FILLER          PIC X(3)  VALUE  '004'.      GA2OPGM 
00299                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00300                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00301                  '** INCLUDE/EXCLUDE FIELD VALUE NOT VALID **'.   GA2OPGM 
00302              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00303 *----------------------------------------------------------------*GA2OPGM 
00304          10  WT-01-ENTRY-005.                                     GA2OPGM 
00305              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00306              15  WT-01-MESSAGE-TEXT-005.                          GA2OPGM 
00307                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00308                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00309                  20  FILLER          PIC X(3)  VALUE  '005'.      GA2OPGM 
00310                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00311                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00312                            '** NO ADD ENTRY FOUND OR INC/EXC FIELDGA2OPGM 
00313 -                    'CHANGE **'.                                 GA2OPGM 
00314              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00315 *----------------------------------------------------------------*GA2OPGM 
00316          10  WT-01-ENTRY-006.                                     GA2OPGM 
00317              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00318              15  WT-01-MESSAGE-TEXT-006.                          GA2OPGM 
00319                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00320                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00321                  20  FILLER          PIC X(3)  VALUE  '006'.      GA2OPGM 
00322                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00323                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00324                               '** ERROR READING ALL LEVEL INTERNALGA2OPGM 
00325 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2OPGM 
00326              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00327                                                                   GA2OPGM 
00328 *----------------------------------------------------------------*GA2OPGM 
00329          10  WT-01-ENTRY-007.                                     GA2OPGM 
00330              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00331              15  WT-01-MESSAGE-TEXT-007.                          GA2OPGM 
00332                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00333                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00334                  20  FILLER          PIC X(3)  VALUE  '007'.      GA2OPGM 
00335                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00336                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00337                              '** PROGRAM ABOUT TO EXCEED MAX RECORGA2OPGM 
00338 -                    ' SIZE. CONTACT SYSTEMS AREA **'.            GA2OPGM 
00339              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00340 *----------------------------------------------------------------*GA2OPGM 
00341          10  WT-01-ENTRY-008.                                     GA2OPGM 
00342              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00343              15  WT-01-MESSAGE-TEXT-008.                          GA2OPGM 
00344                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00345                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00346                  20  FILLER          PIC X(3)  VALUE  '008'.      GA2OPGM 
00347                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00348                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00349                              '** PROGRAM SUBSCRIPT ABOUT TO EXCEEDGA2OPGM 
00350 -                    ' ITS MAX. CONTACT SYSTEMS AREA **'.         GA2OPGM 
00351              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00352                                                                   GA2OPGM 
00353 *----------------------------------------------------------------*GA2OPGM 
00354          10  WT-01-ENTRY-009.                                     GA2OPGM 
00355              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00356              15  WT-01-MESSAGE-TEXT-009.                          GA2OPGM 
00357                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00358                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00359                  20  FILLER          PIC X(3)  VALUE  '009'.      GA2OPGM 
00360                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00361                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00362                             '** ERROR REWRITING ALL LEVEL INTERNALGA2OPGM 
00363 -                    ' TABULAR RECORD **'.                        GA2OPGM 
00364              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00365                                                                   GA2OPGM 
00366 *----------------------------------------------------------------*GA2OPGM 
00367          10  WT-01-ENTRY-010.                                     GA2OPGM 
00368              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00369              15  WT-01-MESSAGE-TEXT-010.                          GA2OPGM 
00370                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00371                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00372                  20  FILLER          PIC X(3)  VALUE  '010'.      GA2OPGM 
00373                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00374                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00375                               '** ERROR READING ALL LEVEL INTERNALGA2OPGM 
00376 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2OPGM 
00377              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00378                                                                   GA2OPGM 
00379 *----------------------------------------------------------------*GA2OPGM 
00380          10  WT-01-ENTRY-011.                                     GA2OPGM 
00381              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00382              15  WT-01-MESSAGE-TEXT-011.                          GA2OPGM 
00383                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00384                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00385                  20  FILLER          PIC X(3)  VALUE  '011'.      GA2OPGM 
00386                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00387                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00388                      '** COMMAREA LENGTH IS INVALID **'.          GA2OPGM 
00389              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00390                                                                   GA2OPGM 
00391 *----------------------------------------------------------------*GA2OPGM 
00392          10  WT-01-ENTRY-012.                                     GA2OPGM 
00393              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00394              15  WT-01-MESSAGE-TEXT-012.                          GA2OPGM 
00395                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00396                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00397                  20  FILLER          PIC X(3)  VALUE  '012'.      GA2OPGM 
00398                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00399                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00400                      '**** FUTURE USE ****'.                      GA2OPGM 
00401              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00402                                                                   GA2OPGM 
00403 *----------------------------------------------------------------*GA2OPGM 
00404          10  WT-01-ENTRY-013.                                     GA2OPGM 
00405              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2OPGM 
00406              15  WT-01-MESSAGE-TEXT-013.                          GA2OPGM 
00407                  20  FILLER          PIC X(4)  VALUE  'GA2O'.     GA2OPGM 
00408                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2OPGM 
00409                  20  FILLER          PIC X(3)  VALUE  '013'.      GA2OPGM 
00410                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2OPGM 
00411                  20  FILLER          PIC X(70) VALUE              GA2OPGM 
00412                      '**** FUTURE USE ****'.                      GA2OPGM 
00413              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2OPGM 
00414                                                                   GA2OPGM 
00415 *----------------------------------------------------------------*GA2OPGM 
00416                                                                   GA2OPGM 
00417      05  WT-01-MESSAGE-TABLE         REDEFINES                    GA2OPGM 
00418          WT-01-MESSAGE-VALUES        OCCURS 013 TIMES             GA2OPGM 
00419                                      INDEXED BY WT-01-INDEX.      GA2OPGM 
00420          10  WT-01-ENTRY.                                         GA2OPGM 
00421              15  FILLER              PIC X(02).                   GA2OPGM 
00422              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GA2OPGM 
00423              15  FILLER              PIC X(02).                   GA2OPGM 
00424                                                                   GA2OPGM 
00425 *----------------------------------------------------------------*GA2OPGM 
00426 /                                                                 GA2OPGM 
00427 ** ATTRIBUTES **                                                  GA2OPGM 
00428  COPY DFHBMSCA.                                                   GA2OPGM 
00429      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2OPGM 
00430 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA2OPGM 
00431  01  FILLER.                                                      GA2OPGM 
00432      COPY GCCDRLEN.                                               GA2OPGM 
00433 /                                                                 GA2OPGM 
00434 ** IO PARM AREA **                                                GA2OPGM 
00435  01  GCPPDIO-PARM-AREA.                                           GA2OPGM 
00436  COPY GCPPDIOC.                                                   GA2OPGM 
00437 /                                                                 GA2OPGM 
00438 ** ATTENTION IDENTIFIERS **                                       GA2OPGM 
00439  COPY DFHAID.                                                     GA2OPGM 
00440 /                                                                 GA2OPGM 
00441  01  WS-END                          PIC X(16)  VALUE             GA2OPGM 
00442      '*** W/S ENDS ***'.                                          GA2OPGM 
00443 /                                                                 GA2OPGM 
00444  LINKAGE SECTION.                                                 GA2OPGM 
00445                                                                   GA2OPGM 
00446  01  DFHCOMMAREA.                                                 GA2OPGM 
00447  COPY G2ALCKEC.                                                   GA2OPGM 
00448  COPY GACDACWA.                                                   GA2OPGM 
00449 *    05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA2OPGM 
00450      05  GAS1UPD-PASSED-AREA.                                     GA2OPGM 
00451          07  LVL2-B-SW          PIC X.                            GA2OPGM 
00452          07  LVL2-F-SW          PIC X.                            GA2OPGM 
00453          07  LVL2-G-SW          PIC X.                            GA2OPGM 
00454          07  INTR-TAB-PGM-ID    PIC X(8).                         GA2OPGM 
00455          07  FILLER             PIC X(9).                         GA2OPGM 
00456      05  DELADD-OPTION          PIC X(7).                         GA2OPGM 
00457                                                                   GA2OPGM 
00458 *01  GCA-COMMAREA.                                                GA2OPGM 
00459 *COPY G2ALCKEC.                                                   GA2OPGM 
00460                                                                   GA2OPGM 
00461 /                                                                 GA2OPGM 
00462 ** I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD **         GA2OPGM 
00463  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2OPGM 
00464  COPY GCIOPRM1.                                                   GA2OPGM 
00465 /                                                                 GA2OPGM 
00466  COPY GCWRKDCC.                                                   GA2OPGM 
00467 /                                                                 GA2OPGM 
00468  COPY GCTIPGPC.                                                   GA2OPGM 
00469 /                                                                 GA2OPGM 
00470 ****************************************************************  GA2OPGM 
00471 ** COPY OF THE TABULAR PORTION OF THE RECORD, THIS AREA USED IN   GA2OPGM 
00472 ** SORTING PROCESS.                                               GA2OPGM 
00473 ****************************************************************  GA2OPGM 
00474  01  COPY-TABULAR-TABLE-AREA.                                     GA2OPGM 
00475      05  COPY-TABULAR-TABLE  OCCURS 1109 TIMES INDEXED BY         GA2OPGM 
00476            COPY-IDX.                                              GA2OPGM 
00477        10  COPY-PROCEDURE-CODE          PIC X(7).                 GA2OPGM 
00478 /                                                                 GA2OPGM 
00479 ** IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD **             GA2OPGM 
00480  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2OPGM 
00481  COPY GCIOPRM2.                                                   GA2OPGM 
00482 /                                                                 GA2OPGM 
00483  COPY GCWRKDC2.                                                   GA2OPGM 
00484 /                                                                 GA2OPGM 
00485  COPY GCTABMC.                                                    GA2OPGM 
00486 /                                                                 GA2OPGM 
00487                                                                   GA2OPGM 
00488  PROCEDURE DIVISION.                                              GA2OPGM 
00489                                                                   GA2OPGM 
00490 ******************************************************************GA2OPGM 
00491 **                     M A I N L I N E                            GA2OPGM 
00492 **                                                                GA2OPGM 
00493 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2OPGM 
00494 **  TAKEN BY THE OPERATOR.                                        GA2OPGM 
00495 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2OPGM 
00496 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2OPGM 
00497 **     ADDITIONS FROM.                                            GA2OPGM 
00498 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2OPGM 
00499 **     KEY PF12 OR PF24.                                          GA2OPGM 
00500 **  3. RECEIVE THE SCREEN.                                        GA2OPGM 
00501 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2OPGM 
00502 **     MENU.                                                      GA2OPGM 
00503 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2OPGM 
00504 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2OPGM 
00505 **     (RETURN) TO THE DELETE PROGRAM (GA1OPGM).                  GA2OPGM 
00506 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2OPGM 
00507 **     (RETURN) TO THE PREVIOUS MENU.                             GA2OPGM 
00508 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2OPGM 
00509 **     NORMAL ADD PROCESSING, EXCEPT BYPASS EMPTY VALIDATION TABLEGA2OPGM 
00510 **     CONDITION FOR THE PROCEDURE CODE.                          GA2OPGM 
00511 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2OPGM 
00512 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2OPGM 
00513 **                                                                GA2OPGM 
00514 ******************************************************************GA2OPGM 
00515  1000-MAIN-LINE SECTION.                                          GA2OPGM 
00516                                                                   GA2OPGM 
00517      MOVE '1000'  TO  WS-PARA-ID.                                 GA2OPGM 
00518                                                                   GA2OPGM 
00519      IF EIBAID  =  DFHCLEAR                                       GA2OPGM 
00520          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2OPGM 
00521                         ERASE                                     GA2OPGM 
00522          END-EXEC                                                 GA2OPGM 
00523          EXEC CICS RETURN                                         GA2OPGM 
00524          END-EXEC.                                                GA2OPGM 
00525                                                                   GA2OPGM 
00526      IF EIBTRNID  NOT =  'GA2O'                                   GA2OPGM 
00527         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2OPGM 
00528         GO TO 1099-RETURN.                                        GA2OPGM 
00529                                                                   GA2OPGM 
00530      EXEC CICS RECEIVE   MAP('GA2OI01') MAPSET('GA2OSET')         GA2OPGM 
00531         INTO(GA2OI01I) END-EXEC.                                  GA2OPGM 
00532                                                                   GA2OPGM 
00533      IF SCRNIDNI  NOT =  '002O00'                                 GA2OPGM 
00534         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2OPGM 
00535                                                                   GA2OPGM 
00536      IF EIBAID  =  DFHENTER                                       GA2OPGM 
00537         PERFORM 2000-ADD-PROCESSING                               GA2OPGM 
00538         GO TO 1099-RETURN.                                        GA2OPGM 
00539                                                                   GA2OPGM 
00540      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2OPGM 
00541         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2OPGM 
00542                                                                   GA2OPGM 
00543      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2OPGM 
00544         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2OPGM 
00545                                                                   GA2OPGM 
00546      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2OPGM 
00547         PERFORM 2000-ADD-PROCESSING                               GA2OPGM 
00548         GO TO 1099-RETURN.                                        GA2OPGM 
00549                                                                   GA2OPGM 
00550      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2OPGM 
00551      MOVE -1  TO                                                  GA2OPGM 
00552         MAP-PROCEDURE-CODE-LEN (MAP-IDX1, MAP-IDX2).              GA2OPGM 
00553         SET WT-01-INDEX TO +01.                                   GA2OPGM 
00554         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2OPGM 
00555                                                                   GA2OPGM 
00556      EXEC CICS SEND   MAP('GA2OI01') MAPSET('GA2OSET') DATAONLY   GA2OPGM 
00557      FROM(GA2OI01O) CURSOR END-EXEC.                              GA2OPGM 
00558      GO TO 1099-RETURN.                                           GA2OPGM 
00559                                                                   GA2OPGM 
00560  1000-EXIT.  EXIT.                                                GA2OPGM 
00561                                                                   GA2OPGM 
00562  1099-RETURN.                                                     GA2OPGM 
                                                                                
00563      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2OPGM 
00564         (DELADD-OPTION = 'GAS1UPD') OR                            GA2OPGM 
00565         (DELADD-OPTION = 'GAS2UPD') OR                            GA2OPGM 
00566         (DELADD-OPTION = 'GAS3UPD') OR                            GA2OPGM 
00567         (DELADD-OPTION = 'GAS4UPD') OR                            GA2OPGM 
00568         (DELADD-OPTION = 'GAS5UPD')                               GA2OPGM 
00569          EXEC CICS RETURN   END-EXEC                              GA2OPGM 
00570      ELSE                                                         GA2OPGM 
00571          EXEC CICS RETURN TRANSID('GA2O')                         GA2OPGM 
00572                    COMMAREA(DFHCOMMAREA)                          GA2OPGM 
00573                    LENGTH  (EIBCALEN)                             GA2OPGM 
00574                    END-EXEC.                                      GA2OPGM 
00575                                                                   GA2OPGM 
00576      GOBACK.                                                      GA2OPGM 
00577  1099-EXIT.  EXIT.                                                GA2OPGM 
00578 /                                                                 GA2OPGM 
00579 ******************************************************************GA2OPGM 
00580 **               A D D   P R O C E S S I N G                      GA2OPGM 
00581 **                                                                GA2OPGM 
00582 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2OPGM 
00583 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2OPGM.            GA2OPGM 
00584 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2OPGM 
00585 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2OPGM 
00586 **     SKIP TO THE NEXT LINE.                                     GA2OPGM 
00587 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2OPGM 
00588 **     WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2OPGM 
00589 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2OPGM 
00590 **     DATA.                                                      GA2OPGM 
00591 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2OPGM 
00592 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2OPGM 
00593 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2OPGM 
00594 **     ORDER, FIELD BY FIELD.                                     GA2OPGM 
00595 **  6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2OPGM 
00596 **     MADE.                                                      GA2OPGM 
00597 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2OPGM 
00598 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2OPGM 
00599 **     BACK INTO THE RECORD.                                      GA2OPGM 
00600 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2OPGM 
00601 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2OPGM 
00602 **                                                                GA2OPGM 
00603 ******************************************************************GA2OPGM 
00604  2000-ADD-PROCESSING SECTION.                                     GA2OPGM 
00605                                                                   GA2OPGM 
00606      MOVE '2000'  TO  WS-PARA-ID.                                 GA2OPGM 
00607      MOVE 'N'     TO  WS-ERROR-SW.                                GA2OPGM 
00608      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2OPGM 
00609      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2OPGM 
00610                                                                   GA2OPGM 
00611 ***  D185     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY         GA2OPGM 
00612 *                                                                 GA2OPGM 
00613 *    MOVE LOW-VALUES TO GA2OI01I.                                 GA2OPGM 
00614 *                                                                 GA2OPGM 
00615      MOVE '2005'  TO  WS-PARA-ID.                                 GA2OPGM 
                                                                                
00616  2005-RESET-ALL-ATTRIBUTES.                                       GA2OPGM 
00617                                                                   GA2OPGM 
00618      PERFORM WITH TEST BEFORE                                     GA2OPGM 
00619       VARYING MAP-IDX1 FROM 1 BY 1 UNTIL MAP-IDX2 > WS-MAP-COL    GA2OPGM 
00620           MOVE DFHBMUNF TO                                        GA2OPGM 
00621                   MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2)    GA2OPGM 
00622       IF MAP-IDX1 = WS-MAP-ROW                                    GA2OPGM 
00623         SET MAP-IDX2 UP BY 1                                      GA2OPGM 
00624         SET MAP-IDX1 TO 1                                         GA2OPGM 
00625         SET MAP-IDX1 DOWN BY 1                                    GA2OPGM 
00626       END-IF                                                      GA2OPGM 
00627      END-PERFORM.                                                 GA2OPGM 
00628                                                                   GA2OPGM 
00629      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2OPGM 
00630      MOVE '2010'  TO  WS-PARA-ID.                                 GA2OPGM 
                                                                                
00631  2010-VALIDATE-ADD-ENTRIES.                                       GA2OPGM 
                                                                                
00632      IF MAP-PROCEDURE-CODE-LEN (MAP-IDX1, MAP-IDX2)               GA2OPGM 
00633            =  ZERO                                                GA2OPGM 
00634         IF MAP-IDX1  <  WS-MAP-ROW                                GA2OPGM 
00635            SET MAP-IDX1  UP BY  1                                 GA2OPGM 
00636            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2OPGM 
00637         ELSE                                                      GA2OPGM 
00638            IF MAP-IDX2  <  WS-MAP-COL                             GA2OPGM 
00639               SET MAP-IDX1  TO  1                                 GA2OPGM 
00640               SET MAP-IDX2  UP BY  1                              GA2OPGM 
00641               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2OPGM 
00642            ELSE                                                   GA2OPGM 
00643               GO TO 2020-CHECK-FOR-ERRORS.                        GA2OPGM 
00644                                                                   GA2OPGM 
00645      IF MAP-PROCEDURE-CODE-LEN (MAP-IDX1, MAP-IDX2) = ZERO        GA2OPGM 
00646         MOVE DFHBMUBF  TO                                         GA2OPGM 
00647            MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2)           GA2OPGM 
00648         MOVE '???????' TO                                         GA2OPGM 
00649            MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2)                GA2OPGM 
00650         IF WS-ERROR-SW  NOT =  'Y'                                GA2OPGM 
00651            MOVE 'Y'  TO  WS-ERROR-SW                              GA2OPGM 
00652            MOVE -1   TO                                           GA2OPGM 
00653               MAP-PROCEDURE-CODE-LEN (MAP-IDX1, MAP-IDX2)         GA2OPGM 
00654            SET WT-01-INDEX TO +03                                 GA2OPGM 
00655            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                   GA2OPGM 
                                                                                
      *** ICD-10 START                                                  GA2OPGM 
00657      MOVE MAP-PROCEDURE-CODE(MAP-IDX1, MAP-IDX2)                  GA2OPGM 
                                             TO WS-SAVED-PROCEDURE      GA2OPGM 
                                                PROCED-CODE.                    
                                                                                
           IF WS-SAVED-PROCEDURE (1:3) = 'BIT'                          GA2OPGM 
              MOVE 'PRCDR04 '  TO  GCPPDIO-REQUEST-TYPE                         
           ELSE                                                                 
      *** ICD-10 END                                                            
              MOVE 'PRCDR03 '  TO  GCPPDIO-REQUEST-TYPE                 GA2OPGM 
                                                                                
              IF PROCEDR-DIGIT(5)  =  SPACE                             GA2OPGM 
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
           END-IF                                                               
                                                                                
           MOVE PROCED-KEY     TO  GCPPDIO-SERVICE-CODE-AREA.                   
                                                                                
00669      EXEC CICS LINK PROGRAM('GCPPDIO')                            GA2OPGM 
00670           COMMAREA(GCPPDIO-PARM-AREA)                             GA2OPGM 
00671           LENGTH(GCPPDIO-CA-LEN)                                  GA2OPGM 
00672      END-EXEC.                                                    GA2OPGM 
00673                                                                   GA2OPGM 
00674      IF GCPPDIO-SUCCESSFUL                                        GA2OPGM 
00675         NEXT SENTENCE                                             GA2OPGM 
00676      ELSE                                                         GA2OPGM 
00677         IF GCPPDIO-REC-NOT-FOUND                                  GA2OPGM 
00678            MOVE DFHBMUBF                                          GA2OPGM 
00679               TO MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2)     GA2OPGM 
00680            IF WS-ERROR-SW NOT = 'Y'                               GA2OPGM 
00681               MOVE 'Y' TO WS-ERROR-SW                             GA2OPGM 
00682               MOVE -1 TO MAP-PROCEDURE-CODE-LEN(MAP-IDX1,MAP-IDX2)GA2OPGM 
00683 *             SET WT-01-INDEX TO +03                              GA2OPGM 
00684 *             PERFORM 9000-000-MOVE-MSG-TO-SCREEN                 GA2OPGM 
                    IF PCG-REQUEST-TYPE                                         
                       MOVE                                                     
                         'REQUESTED PREMIER CODE GROUP BIT IS NOT FOUND'        
                                                             TO ERRMSGO         
                    ELSE                                                        
                       MOVE GCPPDIO-RETURN-MESSAGE           TO ERRMSGO         
00685            ELSE                                                   GA2OPGM 
00686               MOVE DFHBMUBF TO                                    GA2OPGM 
00687                  MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2)     GA2OPGM 
00688         ELSE                                                      GA2OPGM 
00689            MOVE 'DEW1'  TO  WS-ABEND-CODE                         GA2OPGM 
00690            MOVE GCPPDIO-RETURN-MESSAGE TO ERRMSGO                 GA2OPGM 
00691            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA2OPGM 
00692                                                                   GA2OPGM 
00693      IF MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2) NOT =        GA2OPGM 
00694                                                           DFHBMUBFGA2OPGM 
00695         ADD 1  TO  WS-ADD-COUNT                                   GA2OPGM 
00696         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2OPGM 
00697         MOVE MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2) TO           GA2OPGM 
00698            WS-PROCEDURE-CODE (WS-SORT-IDX).                       GA2OPGM 
                                                                                
00700      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2OPGM 
00701         SET MAP-IDX1  UP BY  1                                    GA2OPGM 
00702         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2OPGM 
00703      IF MAP-IDX2  <  WS-MAP-COL                                   GA2OPGM 
00704         SET MAP-IDX1  TO  1                                       GA2OPGM 
00705         SET MAP-IDX2  UP BY  1                                    GA2OPGM 
00706         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2OPGM 
00707                                                                   GA2OPGM 
00708  2020-CHECK-FOR-ERRORS.                                           GA2OPGM 
                                                                                
00709      MOVE '2020'  TO  WS-PARA-ID.                                 GA2OPGM 
00710      IF INCEXCI  NOT =  'I' AND  NOT =  'E'                       GA2OPGM 
00711         MOVE -1  TO  INCEXCL                                      GA2OPGM 
00712         MOVE 'Y'  TO  WS-ERROR-SW                                 GA2OPGM 
00713         SET WT-01-INDEX TO +04                                    GA2OPGM 
00714         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2OPGM 
00715                                                                   GA2OPGM 
00716      IF WS-ERROR-SW  =  'Y'                                       GA2OPGM 
00717         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2OPGM 
00718            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2OPGM 
00719            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2OPGM 
00720            INCEXCO                                                GA2OPGM 
00721         MOVE '2100'  TO  WS-PARA-ID                               GA2OPGM 
00722         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2OPGM 
00723            VARYING MAP-IDX2 FROM  1  BY  1                        GA2OPGM 
00724               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2OPGM 
00725            AFTER MAP-IDX1 FROM  1  BY  1                          GA2OPGM 
00726               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2OPGM 
00727         MOVE '2020'  TO  WS-PARA-ID                               GA2OPGM 
00728         EXEC CICS SEND   MAP('GA2OI01') MAPSET('GA2OSET')         GA2OPGM 
00729            DATAONLY FROM(GA2OI01O) CURSOR END-EXEC                GA2OPGM 
00730         GO TO 2099-EXIT.                                          GA2OPGM 
00731                                                                   GA2OPGM 
00732      IF WS-ADD-COUNT  NOT >  ZERO AND                             GA2OPGM 
00733         INCEXCI  =  INEXDRKI                                      GA2OPGM 
00734         SET WT-01-INDEX TO +05                                    GA2OPGM 
00735         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
00736         MOVE -1  TO  INCEXCL                                      GA2OPGM 
00737         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2OPGM 
00738            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2OPGM 
00739            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2OPGM 
00740            INCEXCO                                                GA2OPGM 
00741         EXEC CICS SEND   MAP('GA2OI01') MAPSET('GA2OSET')         GA2OPGM 
00742            DATAONLY FROM(GA2OI01O) CURSOR END-EXEC                GA2OPGM 
00743         GO TO 2099-EXIT.                                          GA2OPGM 
00744                                                                   GA2OPGM 
00745  2025-CONTINUE-PROCESSING.                                        GA2OPGM 
00746                                                                   GA2OPGM 
00747      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN   =                  GA2OPGM 
00748               GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +            GA2OPGM 
00749                         GC-GCTABULR-IPGP-FIXED-LEN +              GA2OPGM 
00750      (GC-GCTABULR-IPGP-VARY-LEN * GC-GCTABULR-IPGP-VARY-MAX-OCUR).GA2OPGM 
00751                                                                   GA2OPGM 
00752      EXEC CICS                                                    GA2OPGM 
00753         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2OPGM 
00754         INITIMG(WS-HEX-00)                                        GA2OPGM 
00755         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2OPGM 
00756      END-EXEC.                                                    GA2OPGM 
00757                                                                   GA2OPGM 
00758      IF  FRMNUIDI  =  'GS3A'                                      GA2OPGM 
00759         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2OPGM 
00760         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2OPGM 
00761         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA2OPGM 
00762         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2OPGM 
00763 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2OPGM 
00764         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2OPGM 
00765 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2OPGM 
00766         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2OPGM 
00767         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2OPGM 
00768         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2OPGM 
00769                          GCIO-WRK-PROVIDER-CONTROL                GA2OPGM 
00770         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2OPGM 
00771         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2OPGM 
00772                                                                   GA2OPGM 
00773      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2OPGM 
00774         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2OPGM 
00775         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2OPGM 
00776         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2OPGM 
00777         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2OPGM 
00778 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2OPGM 
00779         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2OPGM 
00780 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2OPGM 
00781         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2OPGM 
00782         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2OPGM 
00783         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2OPGM 
00784         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2OPGM 
00785         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2OPGM 
00786         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2OPGM 
00787                                                                   GA2OPGM 
00788      IF  FRMNUIDI  =  'GC8A'                                      GA2OPGM 
00789         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2OPGM 
00790         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2OPGM 
00791         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA2OPGM 
00792         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2OPGM 
00793 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2OPGM 
00794         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2OPGM 
00795 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2OPGM 
00796         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2OPGM 
00797         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2OPGM 
00798         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2OPGM 
00799         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2OPGM 
00800         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2OPGM 
00801         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2OPGM 
00802                                                                   GA2OPGM 
00803      MOVE GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.               GA2OPGM 
00804      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2OPGM 
00805      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2OPGM 
00806      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2OPGM 
00807      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA2OPGM 
00808      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2OPGM 
00809      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2OPGM 
00810                                                                   GA2OPGM 
00811      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GA2OPGM 
00812        TO GXA-ENTRY-COUNT.                                        GA2OPGM 
00813                                                                   GA2OPGM 
00814      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2OPGM 
00815                                                                   GA2OPGM 
00816      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2OPGM 
00817         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2OPGM 
00818         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2OPGM 
00819                                                                   GA2OPGM 
00820      IF  NOT GCIO-GOOD-RETURN                                     GA2OPGM 
00821         SET WT-01-INDEX TO +06                                    GA2OPGM 
00822         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
00823         MOVE '2O01'  TO  WS-ABEND-CODE                            GA2OPGM 
00824         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
00825                                                                   GA2OPGM 
00826      MOVE INCEXCI  TO  INEXDRKO,  GXA-INCLUDE-EXCLUDE-IND.        GA2OPGM 
00827      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2OPGM 
00828         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2OPGM 
00829                                                                   GA2OPGM 
00830       SET WS-SORT-IDX  TO  1.                                     GA2OPGM 
00831       SET WS-SORT-IDX2  TO  2.                                    GA2OPGM 
00832       MOVE '2030'  TO  WS-PARA-ID.                                GA2OPGM 
00833                                                                   GA2OPGM 
00834  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2OPGM 
                                                                                
00835      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2OPGM 
00836         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2OPGM 
00837                                                                   GA2OPGM 
00838      IF WS-PROCEDURE-CODE (WS-SORT-IDX) <                         GA2OPGM 
00839         WS-PROCEDURE-CODE (WS-SORT-IDX2)                          GA2OPGM 
00840         SET WS-SORT-IDX2  UP BY  1                                GA2OPGM 
00841         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2OPGM 
00842      ELSE                                                         GA2OPGM 
00843         IF WS-PROCEDURE-CODE (WS-SORT-IDX) >                      GA2OPGM 
00844            WS-PROCEDURE-CODE (WS-SORT-IDX2)                       GA2OPGM 
00845            MOVE WS-PROCEDURE-CODE (WS-SORT-IDX) TO                GA2OPGM 
00846               WS-SAVED-BENEFIT                                    GA2OPGM 
00847            MOVE WS-PROCEDURE-CODE (WS-SORT-IDX2) TO               GA2OPGM 
00848               WS-PROCEDURE-CODE (WS-SORT-IDX)                     GA2OPGM 
00849            MOVE WS-SAVED-BENEFIT TO                               GA2OPGM 
00850               WS-PROCEDURE-CODE (WS-SORT-IDX2)                    GA2OPGM 
00851            SET WS-SORT-IDX2  UP BY  1                             GA2OPGM 
00852            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2OPGM 
00853                                                                   GA2OPGM 
00854      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2OPGM 
00855      MOVE WS-PROCEDURE-CODE (WS-SORT-IDX3) TO                     GA2OPGM 
00856         WS-PROCEDURE-CODE (WS-SORT-IDX2).                         GA2OPGM 
00857      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2OPGM 
00858      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2OPGM 
00859                                                                   GA2OPGM 
00860  2040-ARE-WE-DONE-WITH-SORT.                                      GA2OPGM 
                                                                                
00861      MOVE '2040'  TO  WS-PARA-ID.                                 GA2OPGM 
00862      SET WS-SORT-IDX  UP BY  1.                                   GA2OPGM 
00863      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2OPGM 
00864         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2OPGM 
00865         SET WS-SORT-IDX2  UP BY  1                                GA2OPGM 
00866         MOVE '2030'  TO  WS-PARA-ID                               GA2OPGM 
00867         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2OPGM 
00868      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2OPGM 
00869      MOVE HIGH-VALUES TO WS-DIAG-CODE-ENTRY (WS-SORT-IDX).        GA2OPGM 
00870      MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   GA2OPGM 
00871                                                                   GA2OPGM 
00872      COMPUTE  WS-COPY-LENGTH  =                                   GA2OPGM 
00873                GC-GCTABULR-IPGP-VARY-MAX-OCUR *                   GA2OPGM 
00874                                    GC-GCTABULR-IPGP-VARY-LEN.     GA2OPGM 
00875                                                                   GA2OPGM 
00876      EXEC CICS                                                    GA2OPGM 
00877         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA2OPGM 
00878         LENGTH(WS-COPY-LENGTH)                                    GA2OPGM 
00879         INITIMG(WS-HEX-00)                                        GA2OPGM 
00880      END-EXEC.                                                    GA2OPGM 
00881      SET COPY-IDX,  GXA-INDEX  TO  1.                             GA2OPGM 
00882                                                                   GA2OPGM 
00883      MOVE '2050'  TO  WS-PARA-ID.                                 GA2OPGM 
                                                                                
00884  2050-MAKE-A-COPY-OF-RECORD.                                      GA2OPGM 
                                                                                
00885      MOVE GXA-ENTRIES TO COPY-TABULAR-TABLE-AREA.                 GA2OPGM 
00886                                                                   GA2OPGM 
00887      IF WS-ADD-COUNT  +  GXA-ENTRY-COUNT >                        GA2OPGM 
00888                      GC-GCTABULR-IPGP-VARY-MAX-OCUR               GA2OPGM 
00889         SET WT-01-INDEX TO +07                                    GA2OPGM 
00890         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
00891         MOVE '2O02'  TO  WS-ABEND-CODE                            GA2OPGM 
00892         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
00893                                                                   GA2OPGM 
00894      SET WS-SORT-IDX,  COPY-IDX,  GXA-INDEX  TO  1.               GA2OPGM 
00895                                                                   GA2OPGM 
00896      MOVE '2060'  TO  WS-PARA-ID.                                 GA2OPGM 
                                                                                
00897  2060-MERGE-IN-NEW-ENTRIES.                                       GA2OPGM 
                                                                                
00898      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2OPGM 
00899         SET GXA-INDEX  DOWN BY  1                                 GA2OPGM 
00900         SET GXA-ENTRY-COUNT  TO  GXA-INDEX                        GA2OPGM 
00901         MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT                 GA2OPGM 
00902         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2OPGM 
00903                                                                   GA2OPGM 
00904      IF WS-DIAG-CODE-ENTRY (WS-SORT-IDX)                          GA2OPGM 
00905               =  HIGH-VALUES  AND                                 GA2OPGM 
00906         COPY-TABULAR-TABLE (COPY-IDX)  NOT =  HIGH-VALUES         GA2OPGM 
00907         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2OPGM 
00908                                                                   GA2OPGM 
00909      IF WS-DIAG-CODE-ENTRY (WS-SORT-IDX)                          GA2OPGM 
00910               NOT =  HIGH-VALUES AND                              GA2OPGM 
00911         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2OPGM 
00912         GO TO 2080-INSERT-NEW-ENTRY.                              GA2OPGM 
00913                                                                   GA2OPGM 
00914      IF WS-DIAG-CODE-ENTRY (WS-SORT-IDX)                          GA2OPGM 
00915               =  HIGH-VALUES AND                                  GA2OPGM 
00916         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2OPGM 
00917         NEXT SENTENCE                                             GA2OPGM 
00918      ELSE                                                         GA2OPGM 
00919         IF WS-PROCEDURE-CODE (WS-SORT-IDX) >                      GA2OPGM 
00920            COPY-PROCEDURE-CODE (COPY-IDX)                         GA2OPGM 
00921            GO TO 2070-SAVE-COPIED-ENTRY                           GA2OPGM 
00922         ELSE                                                      GA2OPGM 
00923            IF WS-PROCEDURE-CODE (WS-SORT-IDX) <                   GA2OPGM 
00924               COPY-PROCEDURE-CODE (COPY-IDX)                      GA2OPGM 
00925               GO TO 2080-INSERT-NEW-ENTRY.                        GA2OPGM 
00926                                                                   GA2OPGM 
00927 ******************************************************************GA2OPGM 
00928 **   AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2OPGM 
00929 **   OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2OPGM 
00930 **   INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2OPGM 
00931 **   FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2OPGM 
00932 ******************************************************************GA2OPGM 
00933                                                                   GA2OPGM 
00934      SET WS-SORT-IDX  UP BY  1.                                   GA2OPGM 
00935                                                                   GA2OPGM 
00936  2070-SAVE-COPIED-ENTRY.                                          GA2OPGM 
                                                                                
00937      MOVE '2070'  TO  WS-PARA-ID.                                 GA2OPGM 
00938      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA2OPGM 
00939         GXA-ENTRY (GXA-INDEX).                                    GA2OPGM 
00940      IF COPY-IDX  NOT >  GXA-ENTRY-COUNT                          GA2OPGM 
00941         SET COPY-IDX  UP BY  1                                    GA2OPGM 
00942         SET GXA-INDEX  UP BY  1                                   GA2OPGM 
00943         MOVE '2060'  TO  WS-PARA-ID                               GA2OPGM 
00944         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2OPGM 
00945      ELSE                                                         GA2OPGM 
00946         SET WT-01-INDEX TO +08                                    GA2OPGM 
00947         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
00948         MOVE '2O03'  TO  WS-ABEND-CODE                            GA2OPGM 
00949         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
00950                                                                   GA2OPGM 
00951  2080-INSERT-NEW-ENTRY.                                           GA2OPGM 
                                                                                
00952      MOVE '2080'  TO  WS-PARA-ID.                                 GA2OPGM 
00953                                                                   GA2OPGM 
00954      MOVE WS-PROCEDURE-CODE (WS-SORT-IDX) TO                      GA2OPGM 
00955         GXA-PROCEDURE-ARGUMENT (GXA-INDEX).                       GA2OPGM 
00956      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2OPGM 
00957         SET WS-SORT-IDX  UP BY  1                                 GA2OPGM 
00958         SET GXA-INDEX  UP BY  1                                   GA2OPGM 
00959         MOVE '2060'  TO  WS-PARA-ID                               GA2OPGM 
00960         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2OPGM 
00961      ELSE                                                         GA2OPGM 
00962         SET WT-01-INDEX TO +08                                    GA2OPGM 
00963         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
00964         MOVE '2O04'  TO  WS-ABEND-CODE                            GA2OPGM 
00965         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
00966                                                                   GA2OPGM 
00967  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2OPGM 
                                                                                
00968      MOVE '2090'  TO  WS-PARA-ID.                                 GA2OPGM 
00969                                                                   GA2OPGM 
00970 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2OPGM 
00971                                                                   GA2OPGM 
00972      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2OPGM 
00973      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2OPGM 
00974                                                                   GA2OPGM 
00975      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN +       GA2OPGM 
00976                     GC-GCTABULR-IPGP-FIXED-LEN +                  GA2OPGM 
00977              (GXA-ENTRY-COUNT  *  GC-GCTABULR-IPGP-VARY-LEN).     GA2OPGM 
00978                                                                   GA2OPGM 
00979      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN   =                  GA2OPGM 
00980            GC-GCIOPARM-LEN  +  GCIO-RECORD-LENGTH.                GA2OPGM 
00981                                                                   GA2OPGM 
00982      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2OPGM 
00983         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2OPGM 
00984         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2OPGM 
00985                                                                   GA2OPGM 
00986      IF NOT GCIO-GOOD-RETURN                                      GA2OPGM 
00987         SET WT-01-INDEX TO +09                                    GA2OPGM 
00988         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
00989         MOVE '2O05'  TO  WS-ABEND-CODE                            GA2OPGM 
00990         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
00991                                                                   GA2OPGM 
00992      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2OPGM 
00993         VARYING MAP-IDX2 FROM 1  BY  1                            GA2OPGM 
00994            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2OPGM 
00995         AFTER MAP-IDX1 FROM 1  BY  1                              GA2OPGM 
00996            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2OPGM 
00997                                                                   GA2OPGM 
00998      EXEC CICS SEND   MAP('GA2OI01') MAPSET('GA2OSET') ERASE      GA2OPGM 
00999         FROM(GA2OI01O) END-EXEC.                                  GA2OPGM 
01000                                                                   GA2OPGM 
01001  2099-EXIT.   EXIT.                                               GA2OPGM 
01002 /                                                                 GA2OPGM 
01003 ******************************************************************GA2OPGM 
01004 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2OPGM 
01005 **                                                                GA2OPGM 
01006 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2OPGM 
01007 **  ALREADY ON THE OPERATORS SCREEN.                              GA2OPGM 
01008 **                                                                GA2OPGM 
01009 ******************************************************************GA2OPGM 
01010  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2OPGM 
01011                                                                   GA2OPGM 
01012      MOVE LOW-VALUES  TO                                          GA2OPGM 
01013           MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2).                GA2OPGM 
01014                                                                   GA2OPGM 
01015  2199-EXIT.   EXIT.                                               GA2OPGM 
01016 /                                                                 GA2OPGM 
01017 ******************************************************************GA2OPGM 
01018 **          X C T L   T O   D E L   S C R E E N                   GA2OPGM 
01019 **                                                                GA2OPGM 
01020 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2OPGM 
01021 ** DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2OPGM 
01022 ** PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2OPGM 
01023 ** TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2OPGM 
01024 ** THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2OPGM 
01025 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2OPGM 
01026 ******************************************************************GA2OPGM 
01027  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2OPGM 
                                                                                
01028      MOVE '3000'  TO  WS-PARA-ID.                                 GA2OPGM 
01029                                                                   GA2OPGM 
01030      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2OPGM 
01031            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA2OPGM 
01032            GC-GCTABULR-IPGP-FIXED-LEN +                           GA2OPGM 
01033      (GC-GCTABULR-IPGP-VARY-LEN * GC-GCTABULR-IPGP-VARY-MAX-OCUR).GA2OPGM 
01034                                                                   GA2OPGM 
01035      EXEC CICS                                                    GA2OPGM 
01036         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2OPGM 
01037         INITIMG(WS-HEX-00)                                        GA2OPGM 
01038         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2OPGM 
01039      END-EXEC.                                                    GA2OPGM 
01040                                                                   GA2OPGM 
01041 *    EXEC CICS                                                    GA2OPGM 
01042 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2OPGM 
01043 *       INITIMG(WS-HEX-00)                                        GA2OPGM 
01044 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2OPGM 
01045 *    END-EXEC.                                                    GA2OPGM 
01046                                                                   GA2OPGM 
01047      IF  FRMNUIDI  =  'GS3A'                                      GA2OPGM 
01048         MOVE SPACES  TO  GCIO-WORKFILE-KEY                        GA2OPGM 
01049         MOVE   'G'   TO  GCIO-WRK-STATUS-CODE                     GA2OPGM 
01050         MOVE   'G4'  TO  GCIO-WRK-RECORD-TYPE                     GA2OPGM 
01051 ****    MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA2OPGM 
01052         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2OPGM 
01053         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2OPGM 
01054         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2OPGM 
01055         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2OPGM 
01056         MOVE  SPACES  TO  GCIO-WRK-LINE-OF-BUS                    GA2OPGM 
01057                           GCIO-WRK-PROVIDER-CONTROL               GA2OPGM 
01058         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2OPGM 
01059         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2OPGM 
01060                                                                   GA2OPGM 
01061      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2OPGM 
01062         MOVE SPACES  TO  GCIO-WORKFILE-KEY                        GA2OPGM 
01063         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2OPGM 
01064         MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE                     GA2OPGM 
01065 ****    MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA2OPGM 
01066         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2OPGM 
01067         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2OPGM 
01068         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2OPGM 
01069         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2OPGM 
01070         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2OPGM 
01071         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2OPGM 
01072         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2OPGM 
01073         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2OPGM 
01074                                                                   GA2OPGM 
01075      IF  FRMNUIDI  =  'GC8A'                                      GA2OPGM 
01076         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2OPGM 
01077         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2OPGM 
01078         MOVE   'C6'  TO  GCIO-WRK-RECORD-TYPE                     GA2OPGM 
01079 ****    MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA2OPGM 
01080         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2OPGM 
01081         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2OPGM 
01082         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2OPGM 
01083         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2OPGM 
01084         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2OPGM 
01085         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2OPGM 
01086         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2OPGM 
01087         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                 GA2OPGM 
01088         MOVE  GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID.           GA2OPGM 
01089                                                                   GA2OPGM 
01090      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2OPGM 
01091      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2OPGM 
01092      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2OPGM 
01093      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2OPGM 
01094      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2OPGM 
01095      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2OPGM 
01096      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2OPGM 
01097      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2OPGM 
01098                                                                   GA2OPGM 
01099      MOVE INCEXCI TO GCA-I-E-INDC.                                GA2OPGM 
01100                                                                   GA2OPGM 
01101      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2OPGM 
01102 *    MOVE  SPACES  TO  GCA-EFFECTIVE-DATE.                        GA2OPGM 
01103      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2OPGM 
01104                                                                   GA2OPGM 
01105      SET GCA-RECORD-POINTER                                       GA2OPGM 
01106        TO ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD.                 GA2OPGM 
01107                                                                   GA2OPGM 
01108      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GA2OPGM 
01109        TO GXA-ENTRY-COUNT.                                        GA2OPGM 
01110                                                                   GA2OPGM 
01111      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2OPGM 
01112                                                                   GA2OPGM 
01113      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2OPGM 
01114         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2OPGM 
01115         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2OPGM 
01116                                                                   GA2OPGM 
01117      IF  NOT GCIO-GOOD-RETURN                                     GA2OPGM 
01118         SET WT-01-INDEX TO +10                                    GA2OPGM 
01119         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
01120         MOVE '2O06'  TO  WS-ABEND-CODE                            GA2OPGM 
01121         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
01122                                                                   GA2OPGM 
01123 *    SET COMMAREA-PNTR                                            GA2OPGM 
01124 *      TO ADDRESS OF GCA-COMMAREA.                                GA2OPGM 
01125                                                                   GA2OPGM 
01126 *    EXEC CICS XCTL  PROGRAM('GA1OPGM') COMMAREA(COMMAREA-PNTR)   GA2OPGM 
01127 *       LENGTH(4)  END-EXEC.                                      GA2OPGM 
01128      EXEC CICS XCTL  PROGRAM('GA1OPGM')                           GA2OPGM 
01129                      COMMAREA(DFHCOMMAREA)                        GA2OPGM 
01130                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2OPGM 
01131      END-EXEC.                                                    GA2OPGM 
01132                                                                   GA2OPGM 
01133  3099-EXIT.   EXIT.                                               GA2OPGM 
01134 /                                                                 GA2OPGM 
01135 ***************************************************************** GA2OPGM 
01136 **          D I S P L A Y   F I R S T   S C R E E N               GA2OPGM 
01137 **                                                                GA2OPGM 
01138 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2OPGM 
01139 ** OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2OPGM 
01140 ** THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2OPGM 
01141 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2OPGM 
01142 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2OPGM 
01143 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2OPGM 
01144 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2OPGM 
01145 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2OPGM 
01146 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2OPGM 
01147 ******************************************************************GA2OPGM 
01148  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2OPGM 
                                                                                
01149      MOVE '4000'  TO  WS-PARA-ID.                                 GA2OPGM 
01150                                                                   GA2OPGM 
01151 ***  D185     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY         GA2OPGM 
01152 *                                                                 GA2OPGM 
01153      MOVE LOW-VALUES TO GA2OI01I.                                 GA2OPGM 
01154                                                                   GA2OPGM 
01155      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA2OPGM 
01156         SET WT-01-INDEX TO +11                                    GA2OPGM 
01157         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
01158         MOVE '2O07'  TO  WS-ABEND-CODE                            GA2OPGM 
01159         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
01160                                                                   GA2OPGM 
01161 *    SET ADDRESS OF GCA-COMMAREA                                  GA2OPGM 
01162 *      TO INCOMING-COMMAREA-PNTR.                                 GA2OPGM 
01163                                                                   GA2OPGM 
01164      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA2OPGM 
01165      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA2OPGM 
01166      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA2OPGM 
01167      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA2OPGM 
01168      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA2OPGM 
01169      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA2OPGM 
01170      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA2OPGM 
01171      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA2OPGM 
01172                                                                   GA2OPGM 
01173      MOVE GCA-I-E-INDC  TO INCEXCO,                               GA2OPGM 
01174                        INEXDRKO.                                  GA2OPGM 
01175                                                                   GA2OPGM 
01176      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2OPGM 
01177         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA2OPGM 
01178 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2OPGM 
01179         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2OPGM 
01180         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2OPGM 
01181         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2OPGM 
01182         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2OPGM 
01183         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2OPGM 
01184         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2OPGM 
01185         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2OPGM 
01186         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2OPGM 
01187         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2OPGM 
01188         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2OPGM 
01189         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2OPGM 
01190         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2OPGM 
01191                                                                   GA2OPGM 
01192      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2OPGM 
01193         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA2OPGM 
01194 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2OPGM 
01195         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2OPGM 
01196         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2OPGM 
01197         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2OPGM 
01198         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2OPGM 
01199         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2OPGM 
01200         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2OPGM 
01201         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2OPGM 
01202         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2OPGM 
01203         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2OPGM 
01204         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2OPGM 
01205         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2OPGM 
01206         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2OPGM 
01207         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2OPGM 
01208         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2OPGM 
01209         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2OPGM 
01210         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2OPGM 
01211                                                                   GA2OPGM 
01212      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2OPGM 
01213         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA2OPGM 
01214         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2OPGM 
01215         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2OPGM 
01216         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2OPGM 
01217         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2OPGM 
01218         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2OPGM 
01219         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2OPGM 
01220         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2OPGM 
01221         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2OPGM 
01222         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2OPGM 
01223         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2OPGM 
01224         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2OPGM 
01225         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2OPGM 
01226         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2OPGM 
01227         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2OPGM 
01228         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2OPGM 
01229         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2OPGM 
01230         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2OPGM 
01231         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2OPGM 
01232                                                                   GA2OPGM 
01233      EXEC CICS SEND   MAP('GA2OI01') MAPSET('GA2OSET') ERASE      GA2OPGM 
01234         FROM(GA2OI01O) END-EXEC.                                  GA2OPGM 
01235                                                                   GA2OPGM 
01236  4099-EXIT.   EXIT.                                               GA2OPGM 
01237 /                                                                 GA2OPGM 
01238 ***************************************************************** GA2OPGM 
01239 **        X C T L   T O   P R E V I O U S   M E N U               GA2OPGM 
01240 **                                                                GA2OPGM 
01241 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2OPGM 
01242 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2OPGM 
01243 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2OPGM 
01244 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2OPGM 
01245 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2OPGM 
01246 ******************************************************************GA2OPGM 
01247  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2OPGM 
                                                                                
01248      MOVE '5000'  TO  WS-PARA-ID.                                 GA2OPGM 
01249                                                                   GA2OPGM 
01250                                                                   GA2OPGM 
01251 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA2OPGM 
01252 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA2OPGM 
01253 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA2OPGM 
01254                                                                   GA2OPGM 
01255      IF  ALTBFNCI  =  'GTM1'                                      GA2OPGM 
01256          EXEC CICS XCTL                                           GA2OPGM 
01257                    PROGRAM('GTM1PGM')                             GA2OPGM 
01258                    END-EXEC.                                      GA2OPGM 
01259                                                                   GA2OPGM 
01260                                                                   GA2OPGM 
01261      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2OPGM 
01262            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA2OPGM 
01263            GC-GCTABULR-ABM-FIXED-LEN     +                        GA2OPGM 
01264           (GC-GCTABULR-ABM-VARY-LEN  *                            GA2OPGM 
01265               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA2OPGM 
01266                                                                   GA2OPGM 
01267      EXEC CICS                                                    GA2OPGM 
01268         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA2OPGM 
01269         INITIMG(WS-HEX-00)                                        GA2OPGM 
01270         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA2OPGM 
01271      END-EXEC.                                                    GA2OPGM 
01272                                                                   GA2OPGM 
01273 *    EXEC CICS                                                    GA2OPGM 
01274 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2OPGM 
01275 *       INITIMG(WS-HEX-00)                                        GA2OPGM 
01276 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2OPGM 
01277 *    END-EXEC.                                                    GA2OPGM 
01278                                                                   GA2OPGM 
01279      IF  FRMNUIDI  =  'GS3A'                                      GA2OPGM 
01280         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2OPGM 
01281         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2OPGM 
01282         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA2OPGM 
01283         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2OPGM 
01284         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2OPGM 
01285         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2OPGM 
01286         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2OPGM 
01287         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2OPGM 
01288                          GCIO-WRK-PROVIDER-CONTROL                GA2OPGM 
01289         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2OPGM 
01290         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2OPGM 
01291         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2OPGM 
01292         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA2OPGM 
01293                            GCA-ALL-LEVEL-TAB-ID                   GA2OPGM 
01294         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA2OPGM 
01295                            GCA-ALL-LEVEL-TAB-SLOT                 GA2OPGM 
01296         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA2OPGM 
01297                            GCA-INTERNAL-TAB-ID,                   GA2OPGM 
01298                            GCA-INTERNAL-TAB-SLOT                  GA2OPGM 
01299         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2OPGM 
01300                                                                   GA2OPGM 
01301      IF  FRMNUIDI  =  'GC4A'                                      GA2OPGM 
01302         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2OPGM 
01303         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2OPGM 
01304         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2OPGM 
01305         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2OPGM 
01306         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2OPGM 
01307         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2OPGM 
01308         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2OPGM 
01309         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2OPGM 
01310         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2OPGM 
01311         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2OPGM 
01312         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2OPGM 
01313         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2OPGM 
01314         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA2OPGM 
01315                            GCA-ALL-LEVEL-TAB-ID                   GA2OPGM 
01316         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA2OPGM 
01317                            GCA-ALL-LEVEL-TAB-SLOT                 GA2OPGM 
01318         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA2OPGM 
01319                            GCA-INTERNAL-TAB-ID,                   GA2OPGM 
01320                            GCA-INTERNAL-TAB-SLOT                  GA2OPGM 
01321         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2OPGM 
01322                                                                   GA2OPGM 
01323      IF  FRMNUIDI  =  'GC8A'                                      GA2OPGM 
01324         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2OPGM 
01325         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2OPGM 
01326         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA2OPGM 
01327         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2OPGM 
01328         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2OPGM 
01329         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2OPGM 
01330         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2OPGM 
01331         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2OPGM 
01332         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2OPGM 
01333         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2OPGM 
01334         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2OPGM 
01335         MOVE GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID            GA2OPGM 
01336         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2OPGM 
01337         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA2OPGM 
01338                            GCA-ALL-LEVEL-TAB-ID                   GA2OPGM 
01339         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA2OPGM 
01340                            GCA-ALL-LEVEL-TAB-SLOT                 GA2OPGM 
01341         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA2OPGM 
01342                          GCA-INTERNAL-TAB-SLOT.                   GA2OPGM 
01343                                                                   GA2OPGM 
01344      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.             GA2OPGM 
01345 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA2OPGM 
01346 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA2OPGM 
01347 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA2OPGM 
01348 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA2OPGM 
01349 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA2OPGM 
01350      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2OPGM 
01351                                                                   GA2OPGM 
01352      SET GCA-RECORD-POINTER                                       GA2OPGM 
01353        TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                    GA2OPGM 
01354                                                                   GA2OPGM 
01355      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GA2OPGM 
01356        TO GAA-ENTRY-COUNT.                                        GA2OPGM 
01357                                                                   GA2OPGM 
01358      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2OPGM 
01359                                                                   GA2OPGM 
01360      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2OPGM 
01361         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA2OPGM 
01362         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA2OPGM 
01363                                                                   GA2OPGM 
01364      IF  NOT GCIO2-GOOD-RETURN                                    GA2OPGM 
01365         SET WT-01-INDEX TO +06                                    GA2OPGM 
01366         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2OPGM 
01367         MOVE '2O08'  TO  WS-ABEND-CODE                            GA2OPGM 
01368         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2OPGM 
01369                                                                   GA2OPGM 
01370 *    SET COMMAREA-PNTR                                            GA2OPGM 
01371 *      TO ADDRESS OF GCA-COMMAREA.                                GA2OPGM 
01372                                                                   GA2OPGM 
01373      IF  ALTBFNCI  =  'GA1B'                                      GA2OPGM 
01374 *       EXEC CICS                                                 GA2OPGM 
01375 *          XCTL  PROGRAM('GA1BPGM')                               GA2OPGM 
01376 *          COMMAREA(COMMAREA-PNTR)                                GA2OPGM 
01377 *          LENGTH(4)                                              GA2OPGM 
01378 *       END-EXEC.                                                 GA2OPGM 
01379         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA2OPGM 
01380                         COMMAREA(DFHCOMMAREA)                     GA2OPGM 
01381                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2OPGM 
01382         END-EXEC.                                                 GA2OPGM 
01383                                                                   GA2OPGM 
01384      IF  ALTBFNCI  =  'GA1C'                                      GA2OPGM 
01385 *       EXEC CICS                                                 GA2OPGM 
01386 *          XCTL  PROGRAM('GA1CPGM')                               GA2OPGM 
01387 *          COMMAREA(COMMAREA-PNTR)                                GA2OPGM 
01388 *          LENGTH(4)                                              GA2OPGM 
01389 *       END-EXEC.                                                 GA2OPGM 
01390         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA2OPGM 
01391                         COMMAREA(DFHCOMMAREA)                     GA2OPGM 
01392                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2OPGM 
01393         END-EXEC.                                                 GA2OPGM 
01394                                                                   GA2OPGM 
01395      IF  ALTBFNCI  =  'GA1D'                                      GA2OPGM 
01396 *       EXEC CICS                                                 GA2OPGM 
01397 *          XCTL  PROGRAM('GA1DPGM')                               GA2OPGM 
01398 *          COMMAREA(COMMAREA-PNTR)                                GA2OPGM 
01399 *          LENGTH(4)                                              GA2OPGM 
01400 *       END-EXEC.                                                 GA2OPGM 
01401         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA2OPGM 
01402                         COMMAREA(DFHCOMMAREA)                     GA2OPGM 
01403                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2OPGM 
01404         END-EXEC.                                                 GA2OPGM 
01405                                                                   GA2OPGM 
01406      IF  ALTBFNCI  =  'GA1E'                                      GA2OPGM 
01407 *       EXEC CICS                                                 GA2OPGM 
01408 *          XCTL  PROGRAM('GA1EPGM')                               GA2OPGM 
01409 *          COMMAREA(COMMAREA-PNTR)                                GA2OPGM 
01410 *          LENGTH(4)                                              GA2OPGM 
01411 *       END-EXEC.                                                 GA2OPGM 
01412         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA2OPGM 
01413                         COMMAREA(DFHCOMMAREA)                     GA2OPGM 
01414                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2OPGM 
01415         END-EXEC.                                                 GA2OPGM 
01416                                                                   GA2OPGM 
01417      IF  ALTBFNCI  =  'GA1P'                                      GA2OPGM 
01418         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA2OPGM 
01419                         COMMAREA(DFHCOMMAREA)                     GA2OPGM 
01420                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2OPGM 
01421         END-EXEC.                                                 GA2OPGM 
01422                                                                   GA2OPGM 
01423  5099-EXIT.                                                       GA2OPGM 
01424      EXIT.                                                        GA2OPGM 
01425 /                                                                 GA2OPGM 
01426 ***************************************************************** GA2OPGM 
01427 **           X C T L   T O   M A I N   M E N U                    GA2OPGM 
01428 **                                                                GA2OPGM 
01429 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2OPGM 
01430 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2OPGM 
01431 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2OPGM 
01432 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2OPGM 
01433 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2OPGM 
01434 ** AND PROGRESS DOWN.                                             GA2OPGM 
01435 ******************************************************************GA2OPGM 
01436  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2OPGM 
                                                                                
01437      MOVE '6000'  TO  WS-PARA-ID.                                 GA2OPGM 
01438      MOVE '2O09'  TO  WS-ABEND-CODE.                              GA2OPGM 
01439                                                                   GA2OPGM 
01440      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2OPGM 
01441                                                                   GA2OPGM 
01442  6099-EXIT.     EXIT.                                             GA2OPGM 
01443 /***************************************************************  GA2OPGM 
01444 *                                                              *  GA2OPGM 
01445 * 9000   MOVE MESSAGE TO SCREEN                                *  GA2OPGM 
01446 *                                                              *  GA2OPGM 
01447 ****************************************************************  GA2OPGM 
01448  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA2OPGM 
01449  9000-010.                                                        GA2OPGM 
01450                                                                   GA2OPGM 
01451          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                     GA2OPGM 
01452                    TO ERRMSGO.                                    GA2OPGM 
01453                                                                   GA2OPGM 
01454  9000-900-EXIT.                                                   GA2OPGM 
01455      EXIT.                                                        GA2OPGM 
01456 /                                                                 GA2OPGM 
01457 ******************************************************************GA2OPGM 
01458  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2OPGM 
01459                                                                   GA2OPGM 
01460      SET MAP-IDX1  TO  7.                                         GA2OPGM 
01461      SET MAP-IDX2  TO  1.                                         GA2OPGM 
01462      MOVE -1  TO                                                  GA2OPGM 
01463         MAP-PROCEDURE-CODE-LEN (MAP-IDX1, MAP-IDX2).              GA2OPGM 
01464      EXEC CICS SEND   MAP('GA2OI01') MAPSET('GA2OSET') ERASE      GA2OPGM 
01465         FROM(GA2OI01O) CURSOR WAIT END-EXEC.                      GA2OPGM 
01466                                                                   GA2OPGM 
01467      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2OPGM 
01468                                                                   GA2OPGM 
01469  9999-EXIT.     EXIT.                                             GA2OPGM 
