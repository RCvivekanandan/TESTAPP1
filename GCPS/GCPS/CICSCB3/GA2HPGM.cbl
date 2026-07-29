00001 *      LAST MAINTENANCE TIME: 11.27.01  DATE: 01/06/86            01/12/06
00002  ID DIVISION.                                                     GA2HPGM 
00003  PROGRAM-ID.     GA2HPGM.                                            LV002
00004 **** THIS IS A COBOL/2 PROGRAM *****                              GA2HPGM 
00005  AUTHOR.         S BUCH.                                          GA2HPGM 
00006  DATE-WRITTEN.   11/13/84.                                        GA2HPGM 
00007  DATE-COMPILED.                                                   GA2HPGM 
00008      SKIP3                                                        GA2HPGM 
00009      SKIP3                                                        GA2HPGM 
00010      SKIP3                                                        GA2HPGM 
00011 ******************************************************************GA2HPGM 
00012 *   GA2HPGM      ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM   GA2HPGM 
00013 *                  PROVIDER-GROUP BY PROVIDER NUMBERS - GA2H      GA2HPGM 
00014 *                                                                 GA2HPGM 
00015 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2HPGM 
00016 *   TABULAR PROVISION ID ARGUMENTS.                               GA2HPGM 
00017 *                                                                 GA2HPGM 
00018 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2HPGM 
00019 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2HPGM 
00020 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2HPGM 
00021 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2HPGM 
00022 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2HPGM 
00023 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2HPGM 
00024 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2HPGM 
00025 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2HPGM 
00026 *                                                                 GA2HPGM 
00027 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IPGN)GA2HPGM 
00028 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2HPGM 
00029 *   XCTL TO TRANS GA2H OR PROGRAM GA2HPGM.  THIS PROGRAM WILL     GA2HPGM 
00030 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2HPGM 
00031 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2HPGM 
00032 *   CODE.                                                         GA2HPGM 
00033 *                                                                 GA2HPGM 
00034 *   FUNC CODE: GA2H                                               GA2HPGM 
00035 *   MAPSET:    GA2HSETC                                           GA2HPGM 
00036 *   FILES:     GCPSWORK                                           GA2HPGM 
00037 ******************************************************************GA2HPGM 
00038 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00039 *                                                                 GA2HPGM 
00040 *    TAILORING INSTRUCTIONS:                                      GA2HPGM 
00041 *                                                                 GA2HPGM 
00042 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA2HPGM 
00043 *                                                                 GA2HPGM 
00044 *              ADD PROGRAM FUNCTION CODE      EX. /GCAI/GA2H/     GA2HPGM 
00045 *              PART OF FUNCTION CODE              /AI/2H/         GA2HPGM 
00046 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IPGN/    GA2HPGM 
00047 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX2/       GA2HPGM 
00048 *                                                                 GA2HPGM 
00049 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA2HPGM 
00050 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA2HPGM 
00051 *     OCCURANCES.                                                 GA2HPGM 
00052 *                                                                 GA2HPGM 
00053 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA2HPGM 
00054 *     THAT MUST BE CHANGED.                                       GA2HPGM 
00055 *                                                                 GA2HPGM 
00056 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00057 ***************************************************************** GA2HPGM 
00058 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*             * GA2HPGM 
00059 *-*         U P D A T E   H I S T O R Y         *-*             * GA2HPGM 
00060 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*             * GA2HPGM 
00061 *                                                               * GA2HPGM 
00062 *-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------*    * GA2HPGM 
00063 *                                                               * GA2HPGM 
00064 * M050  12/17/86  AHL  REVISION RELATED TO PROVIDER NUMBER      * GA2HPGM 
00065 *                      ARGUMENT DUE TO ITS PICTURE BEING        * GA2HPGM 
00066 *                      CHANGED FROM COMP-3 S9(10) TO X(10).     * GA2HPGM 
00067 *                      NUMBER OF OCCURS IS ALSO CHANGED         * GA2HPGM 
00068 *                      FROM 659 TO 395.                         * GA2HPGM 
00069 *                                                               * GA2HPGM 
00070 *                                                               * GA2HPGM 
00071 * D0120 03/17/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT       * GA2HPGM 
00072 *                      THAT EXECUTES FROM TRANSACTION GTM1:     * GA2HPGM 
00073 *                      1. PF1/PF13 - CONSTRUCT COMMAREA AS      * GA2HPGM 
00074 *                         IF GC4A HAD CALLED, XCTL TO           * GA2HPGM 
00075 *                         DELETE SCREEN PROGRAM.                * GA2HPGM 
00076 *                      2. PF3/PF15 - XCTL TO GTM1PGM            * GA2HPGM 
00077 *                         WITHOUT PASSING ANY COMMAREA.         * GA2HPGM 
00078 *                                                               * GA2HPGM 
00079 * D116   8/17/87  FRY  CAPTURE OPERATOR-ID WHEN A 'C3', 'C6',   * GA2HPGM 
00080 *                      OR 'G4' RECORD IS UPDATED.               * GA2HPGM 
00081 *                                                               * GA2HPGM 
00082 * D1013 10/06/87  FCG  REMOVE LINK TO DB0340 AND REPLACE WITH   * GA2HPGM 
00083 *                      LINK TO GCPPDIO. REPLACED LOGIC CODE     * GA2HPGM 
00084 *                      TO PROCESS WITH NEW INTERFACE PROGRAM.   * GA2HPGM 
00085 *                                                               * GA2HPGM 
00086 * M1010 12/15/88  ENW  REMOVED NUMERIC CHECK FOR PROVIDER.      * GA2HPGM 
00087 *                                                               * GA2HPGM 
00088 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GA2HPGM 
00089 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GA2HPGM 
00090 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GA2HPGM 
00091 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GA2HPGM 
00092 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GA2HPGM 
00093 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GA2HPGM 
00094 *                       6. REMOVE HARDCOPY ROUTINE.              *GA2HPGM 
00095 *                       7. >>> CONVERT TO COBOL/II <<<<          *GA2HPGM 
00096 *                                                               * GA2HPGM 
00097 *                                                               * GA2HPGM 
00098 *D12009 08/28/91  TPM   INCREASED THE FMAILY RELATION FIELD     * GA2HPGM 
00099 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2HPGM 
00100 *                                                               * GA2HPGM 
00101 *14726/ 11/11/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000 AND * GA2HPGM 
00102 *15057                  THE EXPANSION OF THE GROUP SPECIFIC AND * GA2HPGM 
00103 *                       CONTRACT KEY TO SUPPORT THE TEXAS       * GA2HPGM 
00104 *                       MERGER.                                 * GA2HPGM 
00105 *                                                               * GA2HPGM 
00106 * 14726/  04/11/98  AB   EXPANDED THE SCREEN / MAP               *GA2HPGM 
00107 * 15057                  TO INCLUDE THE ENTIRE KEY               *GA2HPGM 
00108 *                                                               * GA2HPGM 
00109 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP      * GA2HPGM 
00110 *                        2. ADD DELADD-OPTION = 'GAS5UPD'       * GA2HPGM 
00111 *                                                               * GA2HPGM 
00112 * P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN      * GA2HPGM 
00113 *                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.  * GA2HPGM 
00114 *                                                               * GA2HPGM 
00115 *                                                               * GA2HPGM 
00116 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA2HPGM 
00117 *                                                                *GA2HPGM 
00118 *            01-11-06   NB    RECOMPILE FOR GCPPDIOC CHANGES     *GA2HPGM 
00119 *                                                                *GA2HPGM 
00119 *            07-26-11   BA    RECOMPILE FOR GCPPDIOC CHANGES     *GA2HPGM 
SI0724*                                                                *00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00009180
SI0724*                           GCCDRLEN                             *00009190
00120 ***************************************************************** GA2HPGM 
00121 ***************************************************************** GA2HPGM 
00122 ***************************************************************** GA2HPGM 
00123      SKIP3                                                        GA2HPGM 
00124  ENVIRONMENT DIVISION.                                            GA2HPGM 
00125      EJECT                                                        GA2HPGM 
00126  DATA DIVISION.                                                   GA2HPGM 
00127  WORKING-STORAGE SECTION.                                         GA2HPGM 
00128  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2HPGM 
00129      '***GA2HPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2HPGM 
00130  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2HPGM 
00131                                                                   GA2HPGM 
00132  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2HPGM 
00133                                                                   GA2HPGM 
00134  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2HPGM 
00135                                                                   GA2HPGM 
00136  01  COMMAREA-POINTER-AREA.                                       GA2HPGM 
00137      05  COMMAREA-PNTR-COMP      PIC S9(8)  COMP.                 GA2HPGM 
00138      05  COMMAREA-PNTR  REDEFINES                                 GA2HPGM 
00139          COMMAREA-PNTR-COMP      USAGE IS POINTER.                GA2HPGM 
00140                                                                   GA2HPGM 
00141 ** MAP COBOL SCREEN DSECTS **                                     GA2HPGM 
00142  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2HPGM 
00143      '***  I/O MAPAREA ***'.                                      GA2HPGM 
00144  COPY GA2HSETC.                                                   GA2HPGM 
00145      EJECT                                                        GA2HPGM 
00146 ******************************************************************GA2HPGM 
00147 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2HPGM 
00148 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2HPGM 
00149 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2HPGM 
00150 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2HPGM 
00151 **  REDEFINES.                                                    GA2HPGM 
00152 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00153 **                                                                GA2HPGM 
00154 **   THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE GA2HPGM 
00155 **   FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED     GA2HPGM 
00156 **   TO MATCH THE MAP.                                            GA2HPGM 
00157 **                                                                GA2HPGM 
00158 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00159      SKIP3                                                        GA2HPGM 
00160  01  FILLER     REDEFINES   GA2HI01I.                             GA2HPGM 
00161      05  FILLER                              PIC X(89).           GA2HPGM 
00162      05  GROUP-SPECIFIC-ID-LINE.                                  GA2HPGM 
00163          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA2HPGM 
00164          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA2HPGM 
00165          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA2HPGM 
00166          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA2HPGM 
00167          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA2HPGM 
00168          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA2HPGM 
00169          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA2HPGM 
00170          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA2HPGM 
00171          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA2HPGM 
00172          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA2HPGM 
00173          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA2HPGM 
00174          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA2HPGM 
00175          10  FILLER                          PIC X(16).           GA2HPGM 
00176      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA2HPGM 
00177          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2HPGM 
00178          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2HPGM 
00179          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2HPGM 
00180          10  CONTRACT-GROUP-NO               PIC X(9).            GA2HPGM 
00181          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2HPGM 
00182          10  CONTRACT-SECTION-NO             PIC X(5).            GA2HPGM 
00183          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2HPGM 
00184          10  CONTRACT-PKG-CODE               PIC X(3).            GA2HPGM 
00185          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2HPGM 
00186          10  CONTRACT-LOB                    PIC X.               GA2HPGM 
00187          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2HPGM 
00188          10  CONTRACT-PROV-CTL               PIC XX.              GA2HPGM 
00189          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2HPGM 
00190          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2HPGM 
00191          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2HPGM 
00192          10  CONTRACT-EFF-DATE               PIC X(6).            GA2HPGM 
00193          10  FILLER                          PIC X(1).            GA2HPGM 
00194      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA2HPGM 
00195                                     GROUP-SPECIFIC-ID-LINE.       GA2HPGM 
00196          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA2HPGM 
00197          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA2HPGM 
00198          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA2HPGM 
00199          10  BEN-PROV-GROUP-NO               PIC X(9).            GA2HPGM 
00200          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA2HPGM 
00201          10  BEN-PROV-SECTION-NO             PIC X(5).            GA2HPGM 
00202          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA2HPGM 
00203          10  BEN-PROV-PKG-CODE               PIC X(3).            GA2HPGM 
00204          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA2HPGM 
00205          10  BEN-PROV-LOB                    PIC X.               GA2HPGM 
00206          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA2HPGM 
00207          10  BEN-PROV-PROV-CTL               PIC XX.              GA2HPGM 
00208          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA2HPGM 
00209          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA2HPGM 
00210          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA2HPGM 
00211          10  BEN-PROV-EFF-DATE               PIC X(6).            GA2HPGM 
00212          10  BEN-PROV-ID-HEADING             PIC X(6).            GA2HPGM 
00213          10  BEN-PROV-ID-NO                  PIC X(6).            GA2HPGM 
00214          10  FILLER                          PIC X(4).            GA2HPGM 
00215      05  FILLER                              PIC X(78).           GA2HPGM 
00216      05  MAP-PROVIDER-NO-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED    GA2HPGM 
00217          BY MAP-IDX1.                                             GA2HPGM 
00218        10  MAP-PROVIDER-NO-ARGUMENT-COL  OCCURS 3 TIMES INDEXED   GA2HPGM 
00219            BY MAP-IDX2.                                           GA2HPGM 
00220          15  MAP-PROVIDER-NO-ARGUMENT-LEN   PIC S9(4) COMP SYNC.  GA2HPGM 
00221          15  MAP-PROVIDER-NO-ARGUMENT-ATTR  PIC X.                GA2HPGM 
00222          15  MAP-PROVIDER-NO-ARGUMENT       PIC X(10).            GA2HPGM 
00223      SKIP3                                                        GA2HPGM 
00224 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00225  01  FILLER.                                                      GA2HPGM 
00226 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00227 **   THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.      GA2HPGM 
00228 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00229      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +14.  GA2HPGM 
00230      05  WS-MAP-COL                  PIC S999 COMP-3  VALUE +3.   GA2HPGM 
00231      EJECT                                                        GA2HPGM 
00232 ** ALTERNATIVE WORKFILE KEYS **                                   GA2HPGM 
00233  01  FILLER                      PIC X(32)  VALUE                 GA2HPGM 
00234      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2HPGM 
00235  01  WS-ALT-WORKFILE-KEYS.                                        GA2HPGM 
00236  COPY GCWRKKEY.                                                   GA2HPGM 
00237      EJECT                                                        GA2HPGM 
00238 ** DATE FORMATTING AREA **                                        GA2HPGM 
00239  01  HGADATES-COMMAREA.                                           GA2HPGM 
00240  COPY HGCDAT01.                                                   GA2HPGM 
00241                                                                   GA2HPGM 
00242                                                                   GA2HPGM 
00243 ** WORKFIELDS **                                                  GA2HPGM 
00244  01  FILLER                           PIC X(16)                   GA2HPGM 
00245              VALUE '** WORKFIELDS **'.                            GA2HPGM 
00246  01  LINK-LENGTH                      PIC S9(04) COMP SYNC.       GA2HPGM 
00247  01  WS-WORK-FIELDS.                                              GA2HPGM 
00248      05  WS-HEX-00                    PIC X.                      GA2HPGM 
00249      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2HPGM 
00250      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2HPGM 
00251        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2HPGM 
00252 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00253 ** THIS MUST BE CHANGED TO MATCH ONE OCCURENCE OF ENTRY IN TABULARGA2HPGM 
00254 ** RECORD.                                                        GA2HPGM 
00255 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00256      05  WS-SAVED-FIELDS.                                         GA2HPGM 
00257        10  WS-SAVED-PROVIDER-NO-ARGUMENT  PIC  X(10).             GA2HPGM 
00258 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00259 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00260 ** THIS IS THE AREA IN WHICH THE SORTING OF NEW ENTRIES HAPPENS.  GA2HPGM 
00261 ** NAMES MUST CHANGE ACCORDINGLY, AND ONE MORE OCCURENCE IS       GA2HPGM 
00262 ** PROVIDED THAN IS FOUND ON THE SCREEN, THIS IS FOR THE TRAILER. GA2HPGM 
00263 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00264      05  WS-PROV-NO-ARGUMENT-ENTRY  OCCURS 43 TIMES INDEXED BY    GA2HPGM 
00265          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2HPGM 
00266        10  WS-PROVIDER-NO-ARGUMENT       PIC  X(10).              GA2HPGM 
00267 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00268      EJECT                                                        GA2HPGM 
00269 *** SWITCHES ***                                                  GA2HPGM 
00270  01  FILLER                           PIC X(14)                   GA2HPGM 
00271              VALUE '** SWITCHES **'.                              GA2HPGM 
00272  01  WS-SWITCHES.                                                 GA2HPGM 
00273      05  WS-ERROR-SW                  PIC X.                      GA2HPGM 
00274                                                                   GA2HPGM 
00275 ** TITLE LINES **                                                 GA2HPGM 
00276  01  WS-TITLE-LINES.                                              GA2HPGM 
00277      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA2HPGM 
00278          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA2HPGM 
00279      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA2HPGM 
00280          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA2HPGM 
00281      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA2HPGM 
00282          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA2HPGM 
00283                                                                   GA2HPGM 
00284 *** RECORD LENGTHS ***                                            GA2HPGM 
00285  01  FILLER                           PIC X(20)                   GA2HPGM 
00286              VALUE '** RECORD LENGTHS **'.                        GA2HPGM 
00287  01  WS-RECORD-LENGTHS.                                           GA2HPGM 
00288     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA2HPGM 
00289     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA2HPGM 
00290     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2HPGM 
00291     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2HPGM 
00292                                                                   GA2HPGM 
00293 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA2HPGM 
00294  01  FILLER.                                                      GA2HPGM 
00295      COPY GCCDRLEN.                                               GA2HPGM 
00296                                                                   GA2HPGM 
00297      EJECT                                                        GA2HPGM 
00298  COPY COBXIO.                                                     GA2HPGM 
00299      EJECT                                                        GA2HPGM 
00300 ** ATTRIBUTES **                                                  GA2HPGM 
00301  COPY DFHBMSCA.                                                   GA2HPGM 
00302      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2HPGM 
00303      EJECT                                                        GA2HPGM 
00304 ** ATTENTION IDENTIFIERS **                                       GA2HPGM 
00305  COPY DFHAID.                                                     GA2HPGM 
00306      SKIP3                                                        GA2HPGM 
00307      SKIP3                                                        GA2HPGM 
00308  01  WS-END                          PIC X(16)  VALUE             GA2HPGM 
00309      '*** W/S ENDS ***'.                                          GA2HPGM 
00310      EJECT                                                        GA2HPGM 
00311  LINKAGE SECTION.                                                 GA2HPGM 
00312  01  DFHCOMMAREA.                                                 GA2HPGM 
00313  COPY G2ALCKEC.                                                   GA2HPGM 
00314  COPY GACDACWA.                                                   GA2HPGM 
00315 *    05  INCOMING-COMMAREA-PNTR  USAGE IS POINTER.                GA2HPGM 
00316      05  GAS1UPD-PASSED-AREA.                                     GA2HPGM 
00317          07  LVL2-B-SW          PIC X.                            GA2HPGM 
00318          07  LVL2-F-SW          PIC X.                            GA2HPGM 
00319          07  LVL2-G-SW          PIC X.                            GA2HPGM 
00320          07  INTR-TAB-PGM-ID    PIC X(8).                         GA2HPGM 
00321          07  FILLER             PIC X(9).                         GA2HPGM 
00322      05  DELADD-OPTION          PIC X(7).                         GA2HPGM 
00323                                                                   GA2HPGM 
00324                                                                   GA2HPGM 
00325 *01  GCA-COMMAREA.                                                GA2HPGM 
00326 *COPY G2ALCKEC.                                                   GA2HPGM 
00327      EJECT                                                        GA2HPGM 
00328 ** I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD **         GA2HPGM 
00329  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2HPGM 
00330  COPY GCIOPRM1.                                                   GA2HPGM 
00331      EJECT                                                        GA2HPGM 
00332  COPY GCWRKDCC.                                                   GA2HPGM 
00333      EJECT                                                        GA2HPGM 
00334  COPY GCTIPGNC.                                                   GA2HPGM 
00335      EJECT                                                        GA2HPGM 
00336 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00337 ** THIS AREA MUST BE CHANGED TO MATCH THE TABLE FROM THE TABULAR  GA2HPGM 
00338 ** RECORD; FIELD NAMES, TYPES, AND THE NUMBER OF OCCURENCES.      GA2HPGM 
00339 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00340  01  COPY-TABULAR-TABLE-AREA.                                     GA2HPGM 
00341      05  COPY-TABULAR-TABLE  OCCURS 395 TIMES INDEXED BY          GA2HPGM 
00342            COPY-IDX.                                              GA2HPGM 
00343        10  COPY-PROVIDER-NO-ARGUMENT   PIC  X(10).                GA2HPGM 
00344 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00345      EJECT                                                        GA2HPGM 
00346 ** IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD **             GA2HPGM 
00347  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2HPGM 
00348  COPY GCIOPRM2.                                                   GA2HPGM 
00349      EJECT                                                        GA2HPGM 
00350  COPY GCWRKDC2.                                                   GA2HPGM 
00351      EJECT                                                        GA2HPGM 
00352  COPY GCTABMC.                                                    GA2HPGM 
00353      EJECT                                                        GA2HPGM 
00354 ** IO PARM AREA **                                                GA2HPGM 
00355  01  GCPPDIO-PARM-AREA.                                           GA2HPGM 
00356  COPY GCPPDIOC.                                                   GA2HPGM 
00357                                                                   GA2HPGM 
00358      EJECT                                                        GA2HPGM 
00359  PROCEDURE DIVISION.                                              GA2HPGM 
00360                                                                   GA2HPGM 
00361 ******************************************************************GA2HPGM 
00362 **                H O U S E K E E P I N G                         GA2HPGM 
00363 **                                                                GA2HPGM 
00364 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2HPGM 
00365 **                                                                GA2HPGM 
00366 ******************************************************************GA2HPGM 
00367  0000-HOUSEKEEPING   SECTION.                                     GA2HPGM 
00368                                                                   GA2HPGM 
00369      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2HPGM 
00370                                                                   GA2HPGM 
00371      IF EIBAID  =  DFHCLEAR                                       GA2HPGM 
00372          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2HPGM 
00373                         ERASE                                     GA2HPGM 
00374          END-EXEC                                                 GA2HPGM 
00375          EXEC CICS RETURN                                         GA2HPGM 
00376          END-EXEC.                                                GA2HPGM 
00377                                                                   GA2HPGM 
00378      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2HPGM 
00379                END-EXEC.                                          GA2HPGM 
00380  0000-EXIT.                                                       GA2HPGM 
00381        EXIT.                                                      GA2HPGM 
00382 /*****************************************************************GA2HPGM 
00383 **                     M A I N L I N E                            GA2HPGM 
00384 **                                                                GA2HPGM 
00385 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2HPGM 
00386 **  TAKEN BY THE OPERATOR.                                        GA2HPGM 
00387 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2HPGM 
00388 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2HPGM 
00389 **     ADDITIONS FROM.                                            GA2HPGM 
00390 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2HPGM 
00391 **     KEY PF12 OR PF24.                                          GA2HPGM 
00392 **  3. RECEIVE THE SCREEN.                                        GA2HPGM 
00393 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2HPGM 
00394 **     MENU.                                                      GA2HPGM 
00395 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2HPGM 
00396 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2HPGM 
00397 **     (RETURN) TO THE DELETE PROGRAM (GA2HPGM).                  GA2HPGM 
00398 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2HPGM 
00399 **     (RETURN) TO THE PREVIOUS MENU.                             GA2HPGM 
00400 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2HPGM 
00401 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2HPGM 
00402 **                                                                GA2HPGM 
00403 ******************************************************************GA2HPGM 
00404  1000-MAIN-LINE    SECTION.                                       GA2HPGM 
00405                                                                   GA2HPGM 
00406      MOVE '1000'  TO  WS-PARA-ID.                                 GA2HPGM 
00407      IF EIBTRNID  NOT =  'GA2H'                                   GA2HPGM 
00408         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2HPGM 
00409         GO TO 1099-RETURN.                                        GA2HPGM 
00410                                                                   GA2HPGM 
00411      EXEC CICS RECEIVE   MAP('GA2HI01') MAPSET('GA2HSET')         GA2HPGM 
00412         INTO(GA2HI01I) END-EXEC.                                  GA2HPGM 
00413                                                                   GA2HPGM 
00414      IF SCRNIDNI  NOT =  '002H00'                                 GA2HPGM 
00415         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2HPGM 
00416                                                                   GA2HPGM 
00417      IF EIBAID  =  DFHENTER                                       GA2HPGM 
00418         PERFORM 2000-ADD-PROCESSING                               GA2HPGM 
00419         GO TO 1099-RETURN.                                        GA2HPGM 
00420                                                                   GA2HPGM 
00421      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2HPGM 
00422         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2HPGM 
00423                                                                   GA2HPGM 
00424      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2HPGM 
00425         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2HPGM 
00426                                                                   GA2HPGM 
00427      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2HPGM 
00428      MOVE -1  TO                                                  GA2HPGM 
00429         MAP-PROVIDER-NO-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).        GA2HPGM 
00430      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2HPGM 
00431 -    ' THIS PROGRAM ***'  TO  ERRMSGO.                            GA2HPGM 
00432      EXEC CICS SEND   MAP('GA2HI01') MAPSET('GA2HSET') DATAONLY   GA2HPGM 
00433         FROM(GA2HI01O) CURSOR END-EXEC.                           GA2HPGM 
00434                                                                   GA2HPGM 
00435  1099-RETURN.                                                     GA2HPGM 
00436      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2HPGM 
00437         (DELADD-OPTION = 'GAS1UPD') OR                            GA2HPGM 
00438         (DELADD-OPTION = 'GAS2UPD') OR                            GA2HPGM 
00439         (DELADD-OPTION = 'GAS3UPD') OR                            GA2HPGM 
00440         (DELADD-OPTION = 'GAS4UPD') OR                            GA2HPGM 
00441         (DELADD-OPTION = 'GAS5UPD')                               GA2HPGM 
00442          EXEC CICS RETURN   END-EXEC                              GA2HPGM 
00443      ELSE                                                         GA2HPGM 
00444          EXEC CICS RETURN TRANSID('GA2H')                         GA2HPGM 
00445                    COMMAREA(DFHCOMMAREA)                          GA2HPGM 
00446                    LENGTH  (EIBCALEN)                             GA2HPGM 
00447                    END-EXEC.                                      GA2HPGM 
00448                                                                   GA2HPGM 
00449      GOBACK.                                                      GA2HPGM 
00450                                                                   GA2HPGM 
00451  1099-EXIT.                                                       GA2HPGM 
00452        EXIT.                                                      GA2HPGM 
00453 /*****************************************************************GA2HPGM 
00454 **               A D D   P R O C E S S I N G                      GA2HPGM 
00455 **                                                                GA2HPGM 
00456 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2HPGM 
00457 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2HPGM.            GA2HPGM 
00458 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2HPGM 
00459 **  2. DETERMINE IF ANY VALUE WERE ENTERED FOR THIS LINE.   IF NOTGA2HPGM 
00460 **     SKIP TO THE NEXT LINE.                                     GA2HPGM 
00461 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2HPGM 
00462 **     WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2HPGM 
00463 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2HPGM 
00464 **     DATA.                                                      GA2HPGM 
00465 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2HPGM 
00466 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2HPGM 
00467 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2HPGM 
00468 **     ORDER, FIELD BY FIELD.                                     GA2HPGM 
00469 **  6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2HPGM 
00470 **     MADE.                                                      GA2HPGM 
00471 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2HPGM 
00472 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2HPGM 
00473 **     BACK INTO THE RECORD.                                      GA2HPGM 
00474 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2HPGM 
00475 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2HPGM 
00476 **                                                                GA2HPGM 
00477 ******************************************************************GA2HPGM 
00478  2000-ADD-PROCESSING SECTION.                                     GA2HPGM 
00479                                                                   GA2HPGM 
00480      MOVE '2000'  TO  WS-PARA-ID.                                 GA2HPGM 
00481      MOVE 'N'  TO  WS-ERROR-SW.                                   GA2HPGM 
00482      MOVE ZERO  TO  WS-ADD-COUNT.                                 GA2HPGM 
00483      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2HPGM 
00484                                                                   GA2HPGM 
00485      MOVE '2005'  TO  WS-PARA-ID.                                 GA2HPGM 
00486  2005-RESET-ALL-ATTRIBUTES.                                       GA2HPGM 
00487 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00488 ** CHANGE TO MATCH TABULAR ENTRY'S FIELD NAMES, AND POSSIBLY THE  GA2HPGM 
00489 ** ADDITION OF THE PROCESSING OF ANOTHER INDEX FOR ROWS.          GA2HPGM 
00490 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00491      MOVE DFHBMUNF  TO                                            GA2HPGM 
00492         MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2).       GA2HPGM 
00493      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2HPGM 
00494         SET MAP-IDX1  UP BY  1                                    GA2HPGM 
00495         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2HPGM 
00496      IF MAP-IDX2  <  WS-MAP-COL                                   GA2HPGM 
00497         SET MAP-IDX1  TO  1                                       GA2HPGM 
00498         SET MAP-IDX2  UP BY  1                                    GA2HPGM 
00499         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2HPGM 
00500                                                                   GA2HPGM 
00501      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2HPGM 
00502      MOVE '2010'  TO  WS-PARA-ID.                                 GA2HPGM 
00503  2010-VALIDATE-ADD-ENTRIES.                                       GA2HPGM 
00504      IF MAP-PROVIDER-NO-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)         GA2HPGM 
00505            =  ZERO                                                GA2HPGM 
00506         IF MAP-IDX1  <  WS-MAP-ROW                                GA2HPGM 
00507            SET MAP-IDX1  UP BY  1                                 GA2HPGM 
00508            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2HPGM 
00509         ELSE                                                      GA2HPGM 
00510            IF MAP-IDX2  <  WS-MAP-COL                             GA2HPGM 
00511               SET MAP-IDX1  TO  1                                 GA2HPGM 
00512               SET MAP-IDX2  UP BY  1                              GA2HPGM 
00513               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2HPGM 
00514            ELSE                                                   GA2HPGM 
00515               GO TO 2020-CHECK-FOR-ERRORS.                        GA2HPGM 
00516                                                                   GA2HPGM 
00517 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00518 ** NOTE THE FOLLOWING SET OF IF STATEMENTS, THE FIRST HANDLES A   GA2HPGM 
00519 ** CHARACTER FIELD, AND THE SECOND TWO HANDLE NUMERIC FIELDS.     GA2HPGM 
00520 ** THIS PROGRAM HAS A RE-OCCURRING GROUP OF THREE FIELDS, WITH    GA2HPGM 
00521 ** THE ABOVE CHARACTERISTIC.  IF YOUR PROGRAM HAS A CHARACTER     GA2HPGM 
00522 ** FIELD THEN YOU WILL WANT TO USE THE FIRST IF STMT. IF IT HAS   GA2HPGM 
00523 ** NUMERIC FIELDS YOU WILL WANT TO USE THE SECOND IF STMT.        GA2HPGM 
00524 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00525 ** NUMERIC CHECK COMMENTED OUT FOR M1010.                         GA2HPGM 
00526 *    IF MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2)           GA2HPGM 
00527 *          NOT NUMERIC                                            GA2HPGM 
00528 *       MOVE DFHBMUBF  TO                                         GA2HPGM 
00529 *          MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)     GA2HPGM 
00530 *       IF WS-ERROR-SW  NOT =  'Y'                                GA2HPGM 
00531 *          MOVE 'Y'  TO  WS-ERROR-SW                              GA2HPGM 
00532 *          MOVE -1  TO                                            GA2HPGM 
00533 *             MAP-PROVIDER-NO-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)   GA2HPGM 
00534 *          MOVE '  *** PROVIDER NUMBER ARGUMENT IS INVALID ***'   GA2HPGM 
00535 *             TO  ERRMSGO                                         GA2HPGM 
00536 *       ELSE                                                      GA2HPGM 
00537 *          NEXT SENTENCE                                          GA2HPGM 
00538 *    ELSE                                                         GA2HPGM 
00539      PERFORM 2200-EDIT-PROVIDER-NO-ARGUMENT THRU 2200-EXIT.       GA2HPGM 
00540                                                                   GA2HPGM 
00541                                                                   GA2HPGM 
00542 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00543                                                                   GA2HPGM 
00544 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00545 ** CHANGE TO MATCH TABULAR ENTRY'S FIELD NAMES, AND POSSIBLY THE  GA2HPGM 
00546 ** ADDITION OF THE PROCESSING OF ANOTHER INDEX FOR ROWS.          GA2HPGM 
00547 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00548      IF MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)        GA2HPGM 
00549            NOT  =  DFHBMUBF                                       GA2HPGM 
00550         ADD 1  TO  WS-ADD-COUNT                                   GA2HPGM 
00551         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2HPGM 
00552         MOVE MAP-PROVIDER-NO-ARGUMENT (MAP-IDX1, MAP-IDX2) TO     GA2HPGM 
00553            WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX).                 GA2HPGM 
00554 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00555      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2HPGM 
00556         SET MAP-IDX1  UP BY  1                                    GA2HPGM 
00557         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2HPGM 
00558      IF MAP-IDX2  <  WS-MAP-COL                                   GA2HPGM 
00559         SET MAP-IDX1  TO  1                                       GA2HPGM 
00560         SET MAP-IDX2  UP BY  1                                    GA2HPGM 
00561         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2HPGM 
00562                                                                   GA2HPGM 
00563      EJECT                                                        GA2HPGM 
00564  2020-CHECK-FOR-ERRORS.                                           GA2HPGM 
00565      MOVE '2020'  TO  WS-PARA-ID.                                 GA2HPGM 
00566 **+**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2HPGM 
00567 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD, THE        GA2HPGM 
00568 ** VALIDATION CHECK SHOULD GO HERE AND ITS FIELD NAME SHOULD      GA2HPGM 
00569 ** BE ADDED TO THE LOW-VALUE MOVE LIST.                           GA2HPGM 
00570 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2HPGM 
00571      IF INCEXCI  NOT =  'I' AND  NOT =  'E'                       GA2HPGM 
00572         MOVE -1  TO  INCEXCL                                      GA2HPGM 
00573         MOVE 'Y'  TO  WS-ERROR-SW                                 GA2HPGM 
00574         MOVE '*** INCLUDE/EXCLUDE FIELD VALUE NOT VALID ***'  TO  GA2HPGM 
00575            ERRMSGO.                                               GA2HPGM 
00576                                                                   GA2HPGM 
00577      IF WS-ERROR-SW  =  'Y'                                       GA2HPGM 
00578         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2HPGM 
00579            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2HPGM 
00580            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2HPGM 
00581 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++GA2HPGM 
00582 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA2HPGM 
00583 **  ADD ITS MAP FIELD NAME HERE.                                  GA2HPGM 
00584 ******************************************************************GA2HPGM 
00585            INCEXCO                                                GA2HPGM 
00586         MOVE '2100'  TO  WS-PARA-ID                               GA2HPGM 
00587         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2HPGM 
00588            VARYING MAP-IDX2 FROM  1  BY  1                        GA2HPGM 
00589               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2HPGM 
00590            AFTER MAP-IDX1 FROM  1  BY  1                          GA2HPGM 
00591               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2HPGM 
00592         EXEC CICS SEND   MAP('GA2HI01') MAPSET('GA2HSET')         GA2HPGM 
00593            DATAONLY FROM(GA2HI01O) CURSOR END-EXEC                GA2HPGM 
00594         GO TO 2099-EXIT.                                          GA2HPGM 
00595                                                                   GA2HPGM 
00596 **+**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2HPGM 
00597 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,            GA2HPGM 
00598 ** A CHECK TO SEE IF IT WAS CHANGED SHOULD BE ADDED HERE,         GA2HPGM 
00599 ** THE CURSOR SHOULD GO TO THE I/E FIELD, AND THE I/E FIELD NAME  GA2HPGM 
00600 ** SHOULD BE ADDED TO THE LOW-VALUE MOVE LIST, ELSE THE CURSOR    GA2HPGM 
00601 ** SHOULD GO TO THE 1ST OCCURS FIELD ON THE SCREEN.               GA2HPGM 
00602 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA2HPGM 
00603      IF WS-ADD-COUNT  NOT >  ZERO AND                             GA2HPGM 
00604         INCEXCI  =  INEXDRKI                                      GA2HPGM 
00605         MOVE '*** NO ADD ENTRY FOUND OR INC/EXC FIELD CHANGE ***' GA2HPGM 
00606            TO  ERRMSGO                                            GA2HPGM 
00607         MOVE -1  TO  INCEXCL                                      GA2HPGM 
00608         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2HPGM 
00609            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2HPGM 
00610            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2HPGM 
00611 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++GA2HPGM 
00612 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA2HPGM 
00613 **  ADD ITS MAP FIELD NAME HERE.                                  GA2HPGM 
00614 ******************************************************************GA2HPGM 
00615            INCEXCO                                                GA2HPGM 
00616         EXEC CICS SEND   MAP('GA2HI01') MAPSET('GA2HSET')         GA2HPGM 
00617            DATAONLY FROM(GA2HI01O) CURSOR END-EXEC                GA2HPGM 
00618         GO TO 2099-EXIT.                                          GA2HPGM 
00619                                                                   GA2HPGM 
00620                                                                   GA2HPGM 
00621      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2HPGM 
00622               GC-GCIOPARM-LEN                 +                   GA2HPGM 
00623               GC-WORKFILE-KEY-LEN             +                   GA2HPGM 
00624               GC-GCTABULR-IPGN-FIXED-LEN      +                   GA2HPGM 
00625              (GC-GCTABULR-IPGN-VARY-LEN       *                   GA2HPGM 
00626               GC-GCTABULR-IPGN-VARY-MAX-OCUR)                     GA2HPGM 
00627                                                                   GA2HPGM 
00628                                                                   GA2HPGM 
00629      EXEC CICS                                                    GA2HPGM 
00630         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2HPGM 
00631         INITIMG(WS-HEX-00)                                        GA2HPGM 
00632         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2HPGM 
00633      END-EXEC.                                                    GA2HPGM 
00634                                                                   GA2HPGM 
00635      IF  FRMNUIDI  =  'GS3A'                                      GA2HPGM 
00636         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2HPGM 
00637         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2HPGM 
00638         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA2HPGM 
00639         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2HPGM 
00640 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2HPGM 
00641         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2HPGM 
00642 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2HPGM 
00643         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2HPGM 
00644         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2HPGM 
00645         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2HPGM 
00646                          GCIO-WRK-PROVIDER-CONTROL                GA2HPGM 
00647         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2HPGM 
00648         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2HPGM 
00649                                                                   GA2HPGM 
00650      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2HPGM 
00651         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2HPGM 
00652         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2HPGM 
00653         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2HPGM 
00654         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2HPGM 
00655 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2HPGM 
00656         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2HPGM 
00657 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2HPGM 
00658         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2HPGM 
00659         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2HPGM 
00660         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2HPGM 
00661         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2HPGM 
00662         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2HPGM 
00663         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2HPGM 
00664                                                                   GA2HPGM 
00665      IF  FRMNUIDI  =  'GC8A'                                      GA2HPGM 
00666         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2HPGM 
00667         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2HPGM 
00668         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA2HPGM 
00669         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2HPGM 
00670 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA2HPGM 
00671         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2HPGM 
00672 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA2HPGM 
00673         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2HPGM 
00674         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2HPGM 
00675         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2HPGM 
00676         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2HPGM 
00677         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2HPGM 
00678         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2HPGM 
00679                                                                   GA2HPGM 
00680      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2HPGM 
00681      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2HPGM 
00682      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2HPGM 
00683      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2HPGM 
00684      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA2HPGM 
00685      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2HPGM 
00686      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2HPGM 
00687                                                                   GA2HPGM 
00688      MOVE  GC-GCTABULR-IPGN-VARY-MAX-OCUR                         GA2HPGM 
00689            TO  GX2-ENTRY-COUNT.                                   GA2HPGM 
00690      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2HPGM 
00691                                                                   GA2HPGM 
00692      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2HPGM 
00693         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2HPGM 
00694         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2HPGM 
00695                                                                   GA2HPGM 
00696      IF  NOT GCIO-GOOD-RETURN                                     GA2HPGM 
00697         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2HPGM 
00698 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2HPGM 
00699         MOVE '2HF1'  TO  WS-ABEND-CODE                            GA2HPGM 
00700         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
00701                                                                   GA2HPGM 
00702      MOVE INCEXCI  TO  INEXDRKO,  GX2-INCLUDE-EXCLUDE-IND.        GA2HPGM 
00703      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2HPGM 
00704         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2HPGM 
00705                                                                   GA2HPGM 
00706       SET WS-SORT-IDX  TO  1.                                     GA2HPGM 
00707       SET WS-SORT-IDX2  TO  2.                                    GA2HPGM 
00708       MOVE '2030'  TO  WS-PARA-ID.                                GA2HPGM 
00709                                                                   GA2HPGM 
00710  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2HPGM 
00711      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2HPGM 
00712         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2HPGM 
00713                                                                   GA2HPGM 
00714 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00715 ** CHANGE TO MATCH TABULAR ENTRY'S FIELD NAMES, AND POSSIBLY THE  GA2HPGM 
00716 ** ADDITION OF THE PROCESSING OF ANOTHER INDEX FOR ROWS.          GA2HPGM 
00717 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00718      IF WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX) <                   GA2HPGM 
00719         WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX2)                    GA2HPGM 
00720         SET WS-SORT-IDX2  UP BY  1                                GA2HPGM 
00721         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2HPGM 
00722      ELSE                                                         GA2HPGM 
00723         IF WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX) >                GA2HPGM 
00724            WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX2)                 GA2HPGM 
00725            MOVE WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX) TO          GA2HPGM 
00726               WS-SAVED-PROVIDER-NO-ARGUMENT                       GA2HPGM 
00727            MOVE WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX2) TO         GA2HPGM 
00728               WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX)               GA2HPGM 
00729            MOVE WS-SAVED-PROVIDER-NO-ARGUMENT  TO                 GA2HPGM 
00730               WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX2)              GA2HPGM 
00731            SET WS-SORT-IDX2  UP BY  1                             GA2HPGM 
00732            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2HPGM 
00733                                                                   GA2HPGM 
00734      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2HPGM 
00735      MOVE WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX3) TO               GA2HPGM 
00736         WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX2).                   GA2HPGM 
00737      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2HPGM 
00738      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2HPGM 
00739                                                                   GA2HPGM 
00740  2040-ARE-WE-DONE-WITH-SORT.                                      GA2HPGM 
00741      MOVE '2040'  TO  WS-PARA-ID.                                 GA2HPGM 
00742      SET WS-SORT-IDX  UP BY  1.                                   GA2HPGM 
00743      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2HPGM 
00744         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2HPGM 
00745         SET WS-SORT-IDX2  UP BY  1                                GA2HPGM 
00746         MOVE '2030'  TO  WS-PARA-ID                               GA2HPGM 
00747         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2HPGM 
00748      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2HPGM 
00749      MOVE HIGH-VALUES TO WS-PROV-NO-ARGUMENT-ENTRY (WS-SORT-IDX). GA2HPGM 
00750                                                                   GA2HPGM 
00751      COMPUTE  WS-COPY-LENGTH  =                                   GA2HPGM 
00752              GX2-ENTRY-COUNT  *  GC-GCTABULR-IPGN-VARY-LEN.       GA2HPGM 
00753                                                                   GA2HPGM 
00754      EXEC CICS                                                    GA2HPGM 
00755         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA2HPGM 
00756         LENGTH      (WS-COPY-LENGTH)                              GA2HPGM 
00757         INITIMG     (WS-HEX-00)                                   GA2HPGM 
00758      END-EXEC.                                                    GA2HPGM 
00759                                                                   GA2HPGM 
00760      MOVE GX2-ENTRY-COUNT  TO  GX2-ENTRY-COUNT.                   GA2HPGM 
00761      SET COPY-IDX,  GX2-INDEX  TO  1.                             GA2HPGM 
00762                                                                   GA2HPGM 
00763      MOVE '2050'  TO  WS-PARA-ID.                                 GA2HPGM 
00764  2050-MAKE-A-COPY-OF-RECORD.                                      GA2HPGM 
00765      IF GX2-INDEX  NOT >  GX2-ENTRY-COUNT                         GA2HPGM 
00766         MOVE GX2-ENTRY (GX2-INDEX)  TO                            GA2HPGM 
00767            COPY-TABULAR-TABLE (COPY-IDX)                          GA2HPGM 
00768         SET COPY-IDX, GX2-INDEX  UP BY  1                         GA2HPGM 
00769         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2HPGM 
00770                                                                   GA2HPGM 
00771      IF WS-ADD-COUNT  +  GX2-ENTRY-COUNT  >                       GA2HPGM 
00772                          GC-GCTABULR-IPGN-VARY-MAX-OCUR           GA2HPGM 
00773         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2HPGM 
00774 -    'EASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                 GA2HPGM 
00775         MOVE '2HL1'  TO  WS-ABEND-CODE                            GA2HPGM 
00776         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
00777                                                                   GA2HPGM 
00778      SET WS-SORT-IDX,  COPY-IDX,  GX2-INDEX  TO  1.               GA2HPGM 
00779                                                                   GA2HPGM 
00780      MOVE '2060'  TO  WS-PARA-ID.                                 GA2HPGM 
00781  2060-MERGE-IN-NEW-ENTRIES.                                       GA2HPGM 
00782      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2HPGM 
00783         SET GX2-INDEX  DOWN BY  1                                 GA2HPGM 
00784         SET GX2-ENTRY-COUNT  TO  GX2-INDEX                        GA2HPGM 
00785         MOVE GX2-ENTRY-COUNT  TO  GX2-ENTRY-COUNT                 GA2HPGM 
00786         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2HPGM 
00787                                                                   GA2HPGM 
00788 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00789 ** CHANGE TO MATCH TABULAR ENTRY'S FIELD NAMES, AND POSSIBLY THE  GA2HPGM 
00790 ** ADDITION OF THE PROCESSING OF ANOTHER INDEX FOR ROWS.          GA2HPGM 
00791 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00792      IF WS-PROV-NO-ARGUMENT-ENTRY (WS-SORT-IDX)                   GA2HPGM 
00793               =  HIGH-VALUES  AND                                 GA2HPGM 
00794         COPY-TABULAR-TABLE (COPY-IDX)  NOT =  HIGH-VALUES         GA2HPGM 
00795         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2HPGM 
00796                                                                   GA2HPGM 
00797      IF WS-PROV-NO-ARGUMENT-ENTRY (WS-SORT-IDX)                   GA2HPGM 
00798               NOT =  HIGH-VALUES AND                              GA2HPGM 
00799         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2HPGM 
00800         GO TO 2080-INSERT-NEW-ENTRY.                              GA2HPGM 
00801                                                                   GA2HPGM 
00802      IF WS-PROV-NO-ARGUMENT-ENTRY (WS-SORT-IDX)                   GA2HPGM 
00803               =  HIGH-VALUES AND                                  GA2HPGM 
00804         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2HPGM 
00805         NEXT SENTENCE                                             GA2HPGM 
00806      ELSE                                                         GA2HPGM 
00807         IF WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX) >                GA2HPGM 
00808            COPY-PROVIDER-NO-ARGUMENT (COPY-IDX)                   GA2HPGM 
00809            GO TO 2070-SAVE-COPIED-ENTRY                           GA2HPGM 
00810         ELSE                                                      GA2HPGM 
00811            IF WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX) <             GA2HPGM 
00812               COPY-PROVIDER-NO-ARGUMENT (COPY-IDX)                GA2HPGM 
00813               GO TO 2080-INSERT-NEW-ENTRY.                        GA2HPGM 
00814                                                                   GA2HPGM 
00815 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00816                                                                   GA2HPGM 
00817 **   AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2HPGM 
00818 **   OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2HPGM 
00819 **   INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2HPGM 
00820 **   FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2HPGM 
00821                                                                   GA2HPGM 
00822      SET WS-SORT-IDX  UP BY  1.                                   GA2HPGM 
00823                                                                   GA2HPGM 
00824  2070-SAVE-COPIED-ENTRY.                                          GA2HPGM 
00825      MOVE '2070'  TO  WS-PARA-ID.                                 GA2HPGM 
00826      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA2HPGM 
00827         GX2-ENTRY (GX2-INDEX).                                    GA2HPGM 
00828      IF COPY-IDX  NOT >  GX2-ENTRY-COUNT                          GA2HPGM 
00829         SET COPY-IDX  UP BY  1                                    GA2HPGM 
00830         SET GX2-INDEX  UP BY  1                                   GA2HPGM 
00831         MOVE '2060'  TO  WS-PARA-ID                               GA2HPGM 
00832         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2HPGM 
00833      ELSE                                                         GA2HPGM 
00834         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2HPGM 
00835 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2HPGM 
00836         MOVE '2HL2'  TO  WS-ABEND-CODE                            GA2HPGM 
00837         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
00838                                                                   GA2HPGM 
00839  2080-INSERT-NEW-ENTRY.                                           GA2HPGM 
00840      MOVE '2080'  TO  WS-PARA-ID.                                 GA2HPGM 
00841 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00842 ** CHANGE TO MATCH TABULAR ENTRY'S FIELD NAMES, AND POSSIBLY THE  GA2HPGM 
00843 ** ADDITION OF THE PROCESSING OF ANOTHER INDEX FOR ROWS.          GA2HPGM 
00844 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00845      MOVE WS-PROVIDER-NO-ARGUMENT (WS-SORT-IDX) TO                GA2HPGM 
00846         GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX).                     GA2HPGM 
00847 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00848      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2HPGM 
00849         SET WS-SORT-IDX  UP BY  1                                 GA2HPGM 
00850         SET GX2-INDEX  UP BY  1                                   GA2HPGM 
00851         MOVE '2060'  TO  WS-PARA-ID                               GA2HPGM 
00852         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2HPGM 
00853      ELSE                                                         GA2HPGM 
00854         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2HPGM 
00855 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2HPGM 
00856         MOVE '2HL3'  TO  WS-ABEND-CODE                            GA2HPGM 
00857         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
00858                                                                   GA2HPGM 
00859  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2HPGM 
00860      MOVE '2090'  TO  WS-PARA-ID.                                 GA2HPGM 
00861                                                                   GA2HPGM 
00862 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2HPGM 
00863                                                                   GA2HPGM 
00864      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2HPGM 
00865      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2HPGM 
00866                                                                   GA2HPGM 
00867      COMPUTE  GCIO-RECORD-LENGTH  =                               GA2HPGM 
00868               GC-WORKFILE-KEY-LEN             +                   GA2HPGM 
00869               GC-GCTABULR-IPGN-FIXED-LEN      +                   GA2HPGM 
00870              (GC-GCTABULR-IPGN-VARY-LEN       *  GX2-ENTRY-COUNT).GA2HPGM 
00871                                                                   GA2HPGM 
00872      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2HPGM 
00873            GC-GCIOPARM-LEN   +  GCIO-RECORD-LENGTH.               GA2HPGM 
00874                                                                   GA2HPGM 
00875      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2HPGM 
00876         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2HPGM 
00877         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2HPGM 
00878                                                                   GA2HPGM 
00879      IF NOT GCIO-GOOD-RETURN                                      GA2HPGM 
00880         MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR RECORGA2HPGM 
00881 -    'D.  CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                  GA2HPGM 
00882         MOVE '2HF2'  TO  WS-ABEND-CODE                            GA2HPGM 
00883         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
00884                                                                   GA2HPGM 
00885      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2HPGM 
00886         VARYING MAP-IDX2 FROM 1  BY  1                            GA2HPGM 
00887            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2HPGM 
00888         AFTER MAP-IDX1 FROM 1  BY  1                              GA2HPGM 
00889            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2HPGM 
00890                                                                   GA2HPGM 
00891      EXEC CICS SEND   MAP('GA2HI01') MAPSET('GA2HSET') ERASE      GA2HPGM 
00892         FROM(GA2HI01O) END-EXEC.                                  GA2HPGM 
00893                                                                   GA2HPGM 
00894  2099-EXIT.   EXIT.                                               GA2HPGM 
00895      EJECT                                                        GA2HPGM 
00896 ****************************************************************  GA2HPGM 
00897 ** THIS GENERIC ROUTINE WILL EDIT THE PROVIDER CODE.          **  GA2HPGM 
00898 **                                                            **  GA2HPGM 
00899 ** REQUIRED WORKING-STORAGE:                                  **  GA2HPGM 
00900 **   1.  01  WS-833            PIC S9(4) COMP VALUE +833.     **  GA2HPGM 
00901 **   2.  LAST 02 UNDER THE BLL CELLS AREA.                    **  GA2HPGM 
00902 **       02  GCPPDIO-BLL-PNTR   PIC S9(8)  COMP.              **  GA2HPGM 
00903 **   3.  LAST STMTS UNDER 01 COMMUNICATIONS-KEY-FIELDS.       **  GA2HPGM 
00904 **               COPY PFIOPARM.                               **  GA2HPGM 
00905 **               COPY PROVMSTR.                               **  GA2HPGM 
00906 **   4. 01  LINK-LENGTH    PIC S9(04) COMP SYNC.              **  GA2HPGM 
00907 **   5. COPY COBXIO.                                          **  GA2HPGM 
00908 ****************************************************************  GA2HPGM 
00909                                                                   GA2HPGM 
00910  2200-EDIT-PROVIDER-NO-ARGUMENT SECTION.                          GA2HPGM 
00911                                                                   GA2HPGM 
00912      EXEC CICS                                                    GA2HPGM 
00913         GETMAIN  SET(ADDRESS OF GCPPDIO-PARM-AREA)                GA2HPGM 
00914         LENGTH      (GCPPDIO-CA-LEN)                              GA2HPGM 
00915         INITIMG     (WS-HEX-00)                                   GA2HPGM 
00916      END-EXEC.                                                    GA2HPGM 
00917                                                                   GA2HPGM 
00918      MOVE 'PRVDR02 ' TO GCPPDIO-REQUEST-TYPE.                     GA2HPGM 
00919      MOVE 'T'        TO GCPPDIO-STATUS-SELECTION.                 GA2HPGM 
00920      MOVE MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2) TO      GA2HPGM 
00921         GCPPDIO-PROVIDER-NBR.                                     GA2HPGM 
00922                                                                   GA2HPGM 
00923      EXEC CICS LINK PROGRAM ('GCPPDIO')                           GA2HPGM 
00924                     COMMAREA(GCPPDIO-PARM-AREA)                   GA2HPGM 
00925                       LENGTH(GCPPDIO-CA-LEN)                      GA2HPGM 
00926                         END-EXEC.                                 GA2HPGM 
00927                                                                   GA2HPGM 
00928      IF GCPPDIO-SUCCESSFUL                                        GA2HPGM 
00929          GO TO 2200-EXIT.                                         GA2HPGM 
00930                                                                   GA2HPGM 
00931      IF GCPPDIO-REC-NOT-FOUND                                     GA2HPGM 
00932          MOVE DFHBMUBF TO                                         GA2HPGM 
00933             MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA2HPGM 
00934          IF WS-ERROR-SW NOT = 'Y'                                 GA2HPGM 
00935             MOVE 'Y' TO WS-ERROR-SW                               GA2HPGM 
00936             MOVE -1  TO                                           GA2HPGM 
00937              MAP-PROVIDER-NO-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)    GA2HPGM 
00938             MOVE '  *** PROVIDER NUMBER NOT FOUND ***'            GA2HPGM 
00939                 TO ERRMSGO                                        GA2HPGM 
00940             GO TO 2200-EXIT                                       GA2HPGM 
00941          ELSE                                                     GA2HPGM 
00942             GO TO 2200-EXIT.                                      GA2HPGM 
00943                                                                   GA2HPGM 
00944      IF GCPPDIO-PROVIDER-INACTIVE                                 GA2HPGM 
00945          MOVE DFHBMUBF TO                                         GA2HPGM 
00946            MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)     GA2HPGM 
00947          IF WS-ERROR-SW NOT = 'Y'                                 GA2HPGM 
00948             MOVE 'Y' TO WS-ERROR-SW                               GA2HPGM 
00949             MOVE -1  TO                                           GA2HPGM 
00950              MAP-PROVIDER-NO-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)    GA2HPGM 
00951             MOVE '  *** PROVIDER NUMBER INACTIVE ***'             GA2HPGM 
00952                 TO ERRMSGO                                        GA2HPGM 
00953             GO TO 2200-EXIT                                       GA2HPGM 
00954          ELSE                                                     GA2HPGM 
00955             GO TO 2200-EXIT.                                      GA2HPGM 
00956                                                                   GA2HPGM 
00957      MOVE '5FWH' TO WS-ABEND-CODE.                                GA2HPGM 
00958      MOVE GCPPDIO-RETURN-MESSAGE TO ERRMSGO.                      GA2HPGM 
00959      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA2HPGM 
00960                                                                   GA2HPGM 
00961  2200-EXIT. EXIT.                                                 GA2HPGM 
00962      EJECT                                                        GA2HPGM 
00963 ******************************************************************GA2HPGM 
00964 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2HPGM 
00965 **                                                                GA2HPGM 
00966 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2HPGM 
00967 **  ALREADY ON THE OPERATORS SCREEN.                              GA2HPGM 
00968 **                                                                GA2HPGM 
00969 ******************************************************************GA2HPGM 
00970  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2HPGM 
00971 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00972 ** CHANGE TO MATCH TABULAR ENTRY'S FIELD NAMES, AND POSSIBLY THE  GA2HPGM 
00973 ** ADDITION OF THE PROCESSING OF ANOTHER INDEX FOR ROWS.          GA2HPGM 
00974 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00975      MOVE LOW-VALUES  TO                                          GA2HPGM 
00976           MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2).        GA2HPGM 
00977 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
00978                                                                   GA2HPGM 
00979  2199-EXIT.   EXIT.                                               GA2HPGM 
00980      EJECT                                                        GA2HPGM 
00981 ******************************************************************GA2HPGM 
00982 **          X C T L   T O   D E L   S C R E E N                   GA2HPGM 
00983 **                                                                GA2HPGM 
00984 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2HPGM 
00985 ** DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2HPGM 
00986 ** PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2HPGM 
00987 ** TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2HPGM 
00988 ** THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2HPGM 
00989 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2HPGM 
00990 ******************************************************************GA2HPGM 
00991  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2HPGM 
00992      MOVE '3000'  TO  WS-PARA-ID.                                 GA2HPGM 
00993                                                                   GA2HPGM 
00994      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2HPGM 
00995               GC-GCIOPARM-LEN                 +                   GA2HPGM 
00996               GC-WORKFILE-KEY-LEN             +                   GA2HPGM 
00997               GC-GCTABULR-IPGN-FIXED-LEN      +                   GA2HPGM 
00998              (GC-GCTABULR-IPGN-VARY-LEN       *                   GA2HPGM 
00999               GC-GCTABULR-IPGN-VARY-MAX-OCUR)                     GA2HPGM 
01000                                                                   GA2HPGM 
01001                                                                   GA2HPGM 
01002      EXEC CICS                                                    GA2HPGM 
01003         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2HPGM 
01004         INITIMG(WS-HEX-00)                                        GA2HPGM 
01005         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2HPGM 
01006      END-EXEC.                                                    GA2HPGM 
01007                                                                   GA2HPGM 
01008 *    EXEC CICS                                                    GA2HPGM 
01009 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2HPGM 
01010 *       LENGTH      (WS-COMMUNICATION-KEY-LEN)                    GA2HPGM 
01011 *       INITIMG     (WS-HEX-00)                                   GA2HPGM 
01012 *    END-EXEC.                                                    GA2HPGM 
01013                                                                   GA2HPGM 
01014      IF  FRMNUIDI  =  'GS3A'                                      GA2HPGM 
01015         MOVE SPACES  TO  GCIO-WORKFILE-KEY                        GA2HPGM 
01016         MOVE   'G'   TO  GCIO-WRK-STATUS-CODE                     GA2HPGM 
01017         MOVE   'G4'  TO  GCIO-WRK-RECORD-TYPE                     GA2HPGM 
01018 ****    MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA2HPGM 
01019         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2HPGM 
01020         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2HPGM 
01021         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2HPGM 
01022         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2HPGM 
01023         MOVE  SPACES  TO  GCIO-WRK-LINE-OF-BUS                    GA2HPGM 
01024                           GCIO-WRK-PROVIDER-CONTROL               GA2HPGM 
01025         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2HPGM 
01026         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2HPGM 
01027                                                                   GA2HPGM 
01028      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2HPGM 
01029         MOVE SPACES  TO  GCIO-WORKFILE-KEY                        GA2HPGM 
01030         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2HPGM 
01031         MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE                     GA2HPGM 
01032 ****    MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA2HPGM 
01033         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2HPGM 
01034         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2HPGM 
01035         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2HPGM 
01036         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2HPGM 
01037         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2HPGM 
01038         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2HPGM 
01039         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2HPGM 
01040         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2HPGM 
01041                                                                   GA2HPGM 
01042      IF  FRMNUIDI  =  'GC8A'                                      GA2HPGM 
01043         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2HPGM 
01044         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2HPGM 
01045         MOVE   'C6'  TO  GCIO-WRK-RECORD-TYPE                     GA2HPGM 
01046 ****    MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA2HPGM 
01047         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2HPGM 
01048         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2HPGM 
01049         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2HPGM 
01050         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2HPGM 
01051         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2HPGM 
01052         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2HPGM 
01053         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2HPGM 
01054         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                 GA2HPGM 
01055         MOVE  GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID.           GA2HPGM 
01056                                                                   GA2HPGM 
01057      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2HPGM 
01058      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2HPGM 
01059      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2HPGM 
01060      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2HPGM 
01061      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2HPGM 
01062      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2HPGM 
01063      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2HPGM 
01064      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2HPGM 
01065 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
01066 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA2HPGM 
01067 ** FIELDS TO DISPLAY ON THE INITIAL DEL SCREEN THEY SHOULD BE     GA2HPGM 
01068 ** PASSED HERE.                                                   GA2HPGM 
01069 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
01070      MOVE INCEXCI TO GCA-I-E-INDC.                                GA2HPGM 
01071                                                                   GA2HPGM 
01072                                                                   GA2HPGM 
01073      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2HPGM 
01074 *    MOVE  SPACES  TO  GCA-EFFECTIVE-DATE.                        GA2HPGM 
01075      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2HPGM 
01076                                                                   GA2HPGM 
01077      SET GCA-RECORD-POINTER                                       GA2HPGM 
01078          TO ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD.              GA2HPGM 
01079                                                                   GA2HPGM 
01080      MOVE  GC-GCTABULR-IPGN-VARY-MAX-OCUR                         GA2HPGM 
01081            TO  GX2-ENTRY-COUNT.                                   GA2HPGM 
01082      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2HPGM 
01083                                                                   GA2HPGM 
01084      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2HPGM 
01085         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2HPGM 
01086         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2HPGM 
01087                                                                   GA2HPGM 
01088      IF  NOT GCIO-GOOD-RETURN                                     GA2HPGM 
01089         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR RECORD.GA2HPGM 
01090 -    ' CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                     GA2HPGM 
01091         MOVE '2HF3'  TO  WS-ABEND-CODE                            GA2HPGM 
01092         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
01093                                                                   GA2HPGM 
01094 *    SET  COMMAREA-PNTR   TO                                      GA2HPGM 
01095 *         ADDRESS  OF GCA-COMMAREA.                               GA2HPGM 
01096                                                                   GA2HPGM 
01097 *    EXEC CICS XCTL  PROGRAM('GA1HPGM') COMMAREA(COMMAREA-PNTR)   GA2HPGM 
01098 *       LENGTH(4)  END-EXEC.                                      GA2HPGM 
01099      EXEC CICS XCTL  PROGRAM('GA1HPGM')                           GA2HPGM 
01100                      COMMAREA(DFHCOMMAREA)                        GA2HPGM 
01101                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2HPGM 
01102      END-EXEC.                                                    GA2HPGM 
01103                                                                   GA2HPGM 
01104  3099-EXIT.   EXIT.                                               GA2HPGM 
01105      EJECT                                                        GA2HPGM 
01106 ***************************************************************** GA2HPGM 
01107 **          D I S P L A Y   F I R S T   S C R E E N               GA2HPGM 
01108 **                                                                GA2HPGM 
01109 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2HPGM 
01110 ** OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2HPGM 
01111 ** THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2HPGM 
01112 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2HPGM 
01113 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2HPGM 
01114 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2HPGM 
01115 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2HPGM 
01116 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2HPGM 
01117 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2HPGM 
01118 ******************************************************************GA2HPGM 
01119  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2HPGM 
01120      MOVE '4000'  TO  WS-PARA-ID.                                 GA2HPGM 
01121                                                                   GA2HPGM 
01122      MOVE LOW-VALUES  TO GA2HI01O.                                GA2HPGM 
01123                                                                   GA2HPGM 
01124      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA2HPGM 
01125         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2HPGM 
01126            TO  ERRMSGO                                            GA2HPGM 
01127         MOVE '2HC1'  TO  WS-ABEND-CODE                            GA2HPGM 
01128         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
01129                                                                   GA2HPGM 
01130 *    SET  ADDRESS OF  GCA-COMMAREA  TO                            GA2HPGM 
01131 *         INCOMING-COMMAREA-PNTR.                                 GA2HPGM 
01132                                                                   GA2HPGM 
01133      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA2HPGM 
01134      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA2HPGM 
01135      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA2HPGM 
01136      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA2HPGM 
01137      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA2HPGM 
01138      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA2HPGM 
01139      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA2HPGM 
01140      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA2HPGM 
01141 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
01142 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA2HPGM 
01143 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA2HPGM 
01144 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
01145      MOVE GCA-I-E-INDC TO INCEXCO,                                GA2HPGM 
01146                        INEXDRKO.                                  GA2HPGM 
01147                                                                   GA2HPGM 
01148      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2HPGM 
01149         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA2HPGM 
01150 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2HPGM 
01151         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2HPGM 
01152         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2HPGM 
01153         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2HPGM 
01154         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2HPGM 
01155         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2HPGM 
01156         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2HPGM 
01157         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2HPGM 
01158         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2HPGM 
01159         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2HPGM 
01160         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2HPGM 
01161         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2HPGM 
01162         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2HPGM 
01163                                                                   GA2HPGM 
01164      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2HPGM 
01165         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA2HPGM 
01166 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2HPGM 
01167         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2HPGM 
01168         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2HPGM 
01169         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2HPGM 
01170         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2HPGM 
01171         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2HPGM 
01172         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2HPGM 
01173         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2HPGM 
01174         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2HPGM 
01175         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2HPGM 
01176         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2HPGM 
01177         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2HPGM 
01178         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2HPGM 
01179         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2HPGM 
01180         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2HPGM 
01181         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2HPGM 
01182         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2HPGM 
01183                                                                   GA2HPGM 
01184      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2HPGM 
01185         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA2HPGM 
01186         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2HPGM 
01187         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2HPGM 
01188         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2HPGM 
01189         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2HPGM 
01190         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2HPGM 
01191         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2HPGM 
01192         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2HPGM 
01193         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2HPGM 
01194         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2HPGM 
01195         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2HPGM 
01196         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2HPGM 
01197         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2HPGM 
01198         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2HPGM 
01199         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2HPGM 
01200         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2HPGM 
01201         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2HPGM 
01202         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2HPGM 
01203         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2HPGM 
01204 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA2HPGM 
01205                                                                   GA2HPGM 
01206      EXEC CICS SEND   MAP('GA2HI01') MAPSET('GA2HSET') ERASE      GA2HPGM 
01207         FROM(GA2HI01O) END-EXEC.                                  GA2HPGM 
01208                                                                   GA2HPGM 
01209  4099-EXIT.   EXIT.                                               GA2HPGM 
01210      EJECT                                                        GA2HPGM 
01211 ***************************************************************** GA2HPGM 
01212 **        X C T L   T O   P R E V I O U S   M E N U               GA2HPGM 
01213 **                                                                GA2HPGM 
01214 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2HPGM 
01215 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2HPGM 
01216 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2HPGM 
01217 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2HPGM 
01218 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2HPGM 
01219 ******************************************************************GA2HPGM 
01220  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2HPGM 
01221      MOVE '5000'  TO  WS-PARA-ID.                                 GA2HPGM 
01222                                                                   GA2HPGM 
01223                                                                   GA2HPGM 
01224 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA2HPGM 
01225 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA2HPGM 
01226 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA2HPGM 
01227                                                                   GA2HPGM 
01228      IF  ALTBFNCI  =  'GTM1'                                      GA2HPGM 
01229          EXEC CICS XCTL                                           GA2HPGM 
01230                    PROGRAM('GTM1PGM')                             GA2HPGM 
01231                    END-EXEC.                                      GA2HPGM 
01232                                                                   GA2HPGM 
01233                                                                   GA2HPGM 
01234      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2HPGM 
01235               GC-GCIOPARM-LEN             +                       GA2HPGM 
01236               GC-WORKFILE-KEY-LEN         +                       GA2HPGM 
01237               GC-GCTABULR-ABM-FIXED-LEN   +                       GA2HPGM 
01238              (GC-GCTABULR-ABM-VARY-LEN    *                       GA2HPGM 
01239               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA2HPGM 
01240                                                                   GA2HPGM 
01241      EXEC CICS                                                    GA2HPGM 
01242         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA2HPGM 
01243         INITIMG(WS-HEX-00)                                        GA2HPGM 
01244         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA2HPGM 
01245      END-EXEC.                                                    GA2HPGM 
01246                                                                   GA2HPGM 
01247 *    EXEC CICS                                                    GA2HPGM 
01248 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2HPGM 
01249 *       INITIMG(WS-HEX-00)                                        GA2HPGM 
01250 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2HPGM 
01251 *    END-EXEC.                                                    GA2HPGM 
01252                                                                   GA2HPGM 
01253                                                                   GA2HPGM 
01254      IF  FRMNUIDI  =  'GS3A'                                      GA2HPGM 
01255         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2HPGM 
01256         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2HPGM 
01257         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA2HPGM 
01258         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2HPGM 
01259         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2HPGM 
01260         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2HPGM 
01261         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2HPGM 
01262         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2HPGM 
01263                          GCIO-WRK-PROVIDER-CONTROL                GA2HPGM 
01264         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2HPGM 
01265         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2HPGM 
01266         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2HPGM 
01267         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA2HPGM 
01268                            GCA-ALL-LEVEL-TAB-ID                   GA2HPGM 
01269         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA2HPGM 
01270                            GCA-ALL-LEVEL-TAB-SLOT                 GA2HPGM 
01271         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA2HPGM 
01272                            GCA-INTERNAL-TAB-ID,                   GA2HPGM 
01273                            GCA-INTERNAL-TAB-SLOT                  GA2HPGM 
01274         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2HPGM 
01275                                                                   GA2HPGM 
01276      IF  FRMNUIDI  =  'GC4A'                                      GA2HPGM 
01277         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2HPGM 
01278         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2HPGM 
01279         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2HPGM 
01280         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2HPGM 
01281         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2HPGM 
01282         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2HPGM 
01283         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2HPGM 
01284         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2HPGM 
01285         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2HPGM 
01286         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2HPGM 
01287         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2HPGM 
01288         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2HPGM 
01289         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA2HPGM 
01290                            GCA-ALL-LEVEL-TAB-ID                   GA2HPGM 
01291         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA2HPGM 
01292                            GCA-ALL-LEVEL-TAB-SLOT                 GA2HPGM 
01293         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA2HPGM 
01294                            GCA-INTERNAL-TAB-ID,                   GA2HPGM 
01295                            GCA-INTERNAL-TAB-SLOT                  GA2HPGM 
01296         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2HPGM 
01297                                                                   GA2HPGM 
01298      IF  FRMNUIDI  =  'GC8A'                                      GA2HPGM 
01299         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2HPGM 
01300         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2HPGM 
01301         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA2HPGM 
01302         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2HPGM 
01303         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2HPGM 
01304         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2HPGM 
01305         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2HPGM 
01306         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2HPGM 
01307         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2HPGM 
01308         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2HPGM 
01309         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2HPGM 
01310         MOVE GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID            GA2HPGM 
01311         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2HPGM 
01312         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA2HPGM 
01313                            GCA-ALL-LEVEL-TAB-ID                   GA2HPGM 
01314         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA2HPGM 
01315                            GCA-ALL-LEVEL-TAB-SLOT                 GA2HPGM 
01316         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA2HPGM 
01317                          GCA-INTERNAL-TAB-SLOT.                   GA2HPGM 
01318                                                                   GA2HPGM 
01319      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA2HPGM 
01320 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA2HPGM 
01321 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA2HPGM 
01322 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA2HPGM 
01323 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA2HPGM 
01324 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA2HPGM 
01325      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2HPGM 
01326      SET GCA-RECORD-POINTER                                       GA2HPGM 
01327          TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                  GA2HPGM 
01328                                                                   GA2HPGM 
01329      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO                       GA2HPGM 
01330           GAA-ENTRY-COUNT.                                        GA2HPGM 
01331      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2HPGM 
01332                                                                   GA2HPGM 
01333      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2HPGM 
01334         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA2HPGM 
01335         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA2HPGM 
01336                                                                   GA2HPGM 
01337      IF  NOT GCIO2-GOOD-RETURN                                    GA2HPGM 
01338         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2HPGM 
01339 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2HPGM 
01340         MOVE '2HF4'  TO  WS-ABEND-CODE                            GA2HPGM 
01341         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2HPGM 
01342                                                                   GA2HPGM 
01343 *    SET  COMMAREA-PNTR                                           GA2HPGM 
01344 *         TO   ADDRESS  OF  GCA-COMMAREA.                         GA2HPGM 
01345                                                                   GA2HPGM 
01346      IF  ALTBFNCI  =  'GA1B'                                      GA2HPGM 
01347 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA2HPGM 
01348 *          LENGTH(4) END-EXEC.                                    GA2HPGM 
01349         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA2HPGM 
01350                         COMMAREA(DFHCOMMAREA)                     GA2HPGM 
01351                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2HPGM 
01352         END-EXEC.                                                 GA2HPGM 
01353                                                                   GA2HPGM 
01354      IF  ALTBFNCI  =  'GA1C'                                      GA2HPGM 
01355 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA2HPGM 
01356 *          LENGTH(4) END-EXEC.                                    GA2HPGM 
01357         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA2HPGM 
01358                         COMMAREA(DFHCOMMAREA)                     GA2HPGM 
01359                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2HPGM 
01360         END-EXEC.                                                 GA2HPGM 
01361                                                                   GA2HPGM 
01362      IF  ALTBFNCI  =  'GA1D'                                      GA2HPGM 
01363 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA2HPGM 
01364 *          LENGTH(4) END-EXEC.                                    GA2HPGM 
01365         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA2HPGM 
01366                         COMMAREA(DFHCOMMAREA)                     GA2HPGM 
01367                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2HPGM 
01368         END-EXEC.                                                 GA2HPGM 
01369                                                                   GA2HPGM 
01370      IF  ALTBFNCI  =  'GA1E'                                      GA2HPGM 
01371 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA2HPGM 
01372 *          LENGTH(4) END-EXEC.                                    GA2HPGM 
01373         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA2HPGM 
01374                         COMMAREA(DFHCOMMAREA)                     GA2HPGM 
01375                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2HPGM 
01376         END-EXEC.                                                 GA2HPGM 
01377                                                                   GA2HPGM 
01378      IF  ALTBFNCI  =  'GA1P'                                      GA2HPGM 
01379         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA2HPGM 
01380                         COMMAREA(DFHCOMMAREA)                     GA2HPGM 
01381                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2HPGM 
01382         END-EXEC.                                                 GA2HPGM 
01383                                                                   GA2HPGM 
01384  5099-EXIT.                                                       GA2HPGM 
01385      EXIT.                                                        GA2HPGM 
01386      EJECT                                                        GA2HPGM 
01387 ***************************************************************** GA2HPGM 
01388 **           X C T L   T O   M A I N   M E N U                    GA2HPGM 
01389 **                                                                GA2HPGM 
01390 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2HPGM 
01391 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2HPGM 
01392 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2HPGM 
01393 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2HPGM 
01394 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2HPGM 
01395 ** AND PROGRESS DOWN.                                             GA2HPGM 
01396 ******************************************************************GA2HPGM 
01397  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2HPGM 
01398      MOVE '6000'  TO  WS-PARA-ID.                                 GA2HPGM 
01399      MOVE '2HP1'  TO  WS-ABEND-CODE.                              GA2HPGM 
01400                                                                   GA2HPGM 
01401      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2HPGM 
01402                                                                   GA2HPGM 
01403  6099-EXIT.     EXIT.                                             GA2HPGM 
01404      EJECT                                                        GA2HPGM 
01405 /*****************************************************************GA2HPGM 
01406 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA2HPGM 
01407 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA2HPGM 
01408 ******************************************************************GA2HPGM 
01409  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA2HPGM 
01410  9800-010.                                                        GA2HPGM 
01411                                                                   GA2HPGM 
01412      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA2HPGM 
01413      MOVE 'M'   TO  HGADATE-FORM1.                                GA2HPGM 
01414      MOVE 'J'   TO  HGADATE-FORM2.                                GA2HPGM 
01415      MOVE ZEROS TO  HGADATE-RETURN                                GA2HPGM 
01416                     HGADATE-AMOUNT.                               GA2HPGM 
01417      EXEC CICS LINK PROGRAM ('HGADATES')                          GA2HPGM 
01418                     COMMAREA(HGADATES-COMMAREA)                   GA2HPGM 
01419                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA2HPGM 
01420                     END-EXEC.                                     GA2HPGM 
01421                                                                   GA2HPGM 
01422  9800-900-900-EXIT.                                               GA2HPGM 
01423      EXIT.                                                        GA2HPGM 
01424 /*****************************************************************GA2HPGM 
01425  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2HPGM 
01426                                                                   GA2HPGM 
01427      SET MAP-IDX1  TO  7.                                         GA2HPGM 
01428      SET MAP-IDX2  TO  1.                                         GA2HPGM 
01429      MOVE -1  TO                                                  GA2HPGM 
01430         MAP-PROVIDER-NO-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).        GA2HPGM 
01431      EXEC CICS SEND   MAP('GA2HI01') MAPSET('GA2HSET') ERASE      GA2HPGM 
01432         FROM(GA2HI01O) CURSOR WAIT END-EXEC.                      GA2HPGM 
01433                                                                   GA2HPGM 
01434      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2HPGM 
01435                                                                   GA2HPGM 
01436  9999-EXIT.     EXIT.                                             GA2HPGM 
