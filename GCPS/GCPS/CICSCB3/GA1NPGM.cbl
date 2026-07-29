00001  IDENTIFICATION DIVISION.                                         01/12/06
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA1NPGM 
00003  PROGRAM-ID.     GA1NPGM.                                            LV004
00004  AUTHOR.         GARY D MULLINGS.                                 GA1NPGM 
00005  DATE-WRITTEN.   10/13/89.                                        GA1NPGM 
00006  DATE-COMPILED.                                                   GA1NPGM 
00007      SKIP3                                                        GA1NPGM 
00008 ******************************************************************GA1NPGM 
00009 *   GA1NPGM   ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM      GA1NPGM 
00010 *             DIAGNOSIS GROUP BY DIAGNOSIS CODE - GA1N            GA1NPGM 
00011 *                                                                 GA1NPGM 
00012 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1NPGM 
00013 *   CURRENTLY ON THE ALL LEVEL INTERNAL TABULAR RECORD - #IDGD.   GA1NPGM 
00014 *                                                                 GA1NPGM 
00015 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1NPGM 
00016 *   ALL LEVEL INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN    GA1NPGM 
00017 *   DECIDE IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN     GA1NPGM 
00018 *   ENTRY WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE    GA1NPGM 
00019 *   RECORD WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED   GA1NPGM 
00020 *   BACK INTO THE RECORD BEFORE UPDATING THE RECORD.              GA1NPGM 
00021 *                                                                 GA1NPGM 
00022 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #IDGD) GA1NPGM 
00023 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1NPGM 
00024 *   XCTL TO TRANS GA2N OR PROGRAM GA2NPGM.  THIS PROGRAM WILL     GA1NPGM 
00025 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1NPGM 
00026 *   TABLE.                                                        GA1NPGM 
00027 *                                                                 GA1NPGM 
00028 *   FUNC CODE: GA1N                                               GA1NPGM 
00029 *   MAPSET:    GA1NSETC                                           GA1NPGM 
00030 *   FILES:     GCPSWORK                                           GA1NPGM 
00031 *                                                                 GA1NPGM 
00032 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00033 *                                                                 GA1NPGM 
00034 *    TAILORING INSTRUCTIONS:                                      GA1NPGM 
00035 *                                                                 GA1NPGM 
00036 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1NPGM 
00037 *                                                                 GA1NPGM 
00038 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1N/     GA1NPGM 
00039 *              SCREEN PAGE NUMBER                 /009I/001N/     GA1NPGM 
00040 *              ADD PROGRAM FUNCTION CODE          /GCAI/GA2N/     GA1NPGM 
00041 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IDGD/    GA1NPGM 
00042 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX1/       GA1NPGM 
00043 *                                                                 GA1NPGM 
00044 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1NPGM 
00045 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1NPGM 
00046 *     OCCURANCES.                                                 GA1NPGM 
00047 *                                                                 GA1NPGM 
00048 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1NPGM 
00049 *     THAT MUST BE CHANGED.                                       GA1NPGM 
00050 *                                                                 GA1NPGM 
00051 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00052      SKIP3                                                        GA1NPGM 
00053 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA1NPGM 
00054 *****  P R O G R A M   M O D I F I C A T I O N    S T A T U S ****GA1NPGM 
00055 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA1NPGM 
00056 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA1NPGM 
00057 *                                                                 GA1NPGM 
00058 *   D185    10/02/89    GDM   CREATES BASIC SKELETON RECORD FOR   GA1NPGM 
00059 *                             THE INTERNAL TABULAR RECORD #IDGD.  GA1NPGM 
00060 *                                                                 GA1NPGM 
00061 * R1681  11/30/89    NGE  FIX INCL/EXCL IND MISSING FROM SCREEN.  GA1NPGM 
00062 *                                                                 GA1NPGM 
00063 *        01/29/90    ENW  CHANGED ALL GX4 TO GX9.                 GA1NPGM 
00064 *                                                                 GA1NPGM 
00065 *                      ----ACCUM TABULAR MODIFICATIONS ----     * GA1NPGM 
00066 * 11154  1/03/91  NGE  1. EXPAND OCCUR LENGTH FROM 132 TO 176.  * GA1NPGM 
00067 *                      2. INCREASE MAX OCCURS FROM 29 TO 46.    * GA1NPGM 
00068 *                      3. CHANGE GCA-I-E- FIELD IN COPYBOOKS    * GA1NPGM 
00069 *                         G2ALCKEC AND G2ALCKE2.                * GA1NPGM 
00070 *                      4. REPLACE DATE CONVERSION ROUTINE.      * GA1NPGM 
00071 *                      5. FIX ERROR MESSAGE TABLE.              * GA1NPGM 
00072 *                                                               * GA1NPGM 
00073 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD     * GA1NPGM 
00074 *                           FROM ONE POSITION TO TWO POSITIONS. * GA1NPGM 
00075 *                                                               * GA1NPGM 
00076 *14726/ 11/11/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000     * GA1NPGM 
00077 *15057                  AND THE EXPANSION OF THE GROUP SPECIFIC * GA1NPGM 
00078 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS   * GA1NPGM 
00079 *                       MERGER.                                 * GA1NPGM 
00080 *                                                                 GA1NPGM 
00081 * 14726/  05/15/98  AB   EXPANDED THE SCREEN / MAP              * GA1NPGM 
00082 * 15057                  TO INCLUDE THE ENTIRE KEY              * GA1NPGM 
00083 *                                                               * GA1NPGM 
00084 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP      * GA1NPGM 
00085 *                        2. ADD DELADD-OPTION = 'GAS5UPD'       * GA1NPGM 
00086 *                                                               * GA1NPGM 
00087 * P????   11/19/99  FRY  ADD LENGTH PARAMETER TO THE RETURN     * GA1NPGM 
00088 *                        COMMAND WHEN DFHCOMMAREA IS SPECIFIED. * GA1NPGM 
00089 *                                                               * GA1NPGM 
00090 *                                                               * GA1NPGM 
00091 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA1NPGM 
00092 *                                                               * GA1NPGM 
00093 *  D-356A    04/29/03  GTF   EXPANDED DIAGNOSIS CODE FROM 6 TO  * GA1NPGM 
00094 *                            10 BYTES. CHANGED # OF OCCURS FROM * GA1NPGM 
00095 *                            659 TO 776 FOR #IDGD TABULAR.      * GA1NPGM 
00096 *                                                               * GA1NPGM 
00097 *           01-11-06   NB    RECOMPILE FOR GCPPDIOC CHANGES     * GA1NPGM 
00096 *                                                               * GA1NPGM 
00097 *           07-26-11   BA    RECOMPILE FOR GCPPDIOC CHANGES     * GA1NPGM 
SI0724*                                                               * 00030141
SI0724* P56703     05/08/24  SI  RECOMPILE - PEAQ COPYBOOK EXPANSION  * 00030150
SI0724*                          COPY ABM, ACP, ACL, ADL, AOL,        * 00030160
SI0724*                          GCCDRLEN                             * 00030170
00098 *                                                               * GA1NPGM 
00099 ***************************************************************** GA1NPGM 
00100 *                                                               * GA1NPGM 
00101 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1NPGM 
00102 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1NPGM 
00103 /                                                                 GA1NPGM 
00104 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1NPGM 
00105      EJECT                                                        GA1NPGM 
00106  ENVIRONMENT DIVISION.                                            GA1NPGM 
00107      EJECT                                                        GA1NPGM 
00108  DATA DIVISION.                                                   GA1NPGM 
00109  WORKING-STORAGE SECTION.                                         GA1NPGM 
00110  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1NPGM 
00111      '***GA1NPGM WS BEGINS***'.                                   GA1NPGM 
00112  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1NPGM 
00113                                                                   GA1NPGM 
00114  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1NPGM 
00115  01  COMMAREA-POINTER-AREA.                                       GA1NPGM 
00116      05  COMMAREA-PNTR-COMP                      PIC S9(08)  COMP.GA1NPGM 
00117      05  COMMAREA-PNTR  REDEFINES                                 GA1NPGM 
00118                             COMMAREA-PNTR-COMP USAGE IS POINTER.  GA1NPGM 
00119                                                                   GA1NPGM 
00120  01  INTERNAL-POINTER-AREA.                                       GA1NPGM 
00121      05  INTERNAL-TAB-PNTR-COMP                  PIC S9(08)  COMP.GA1NPGM 
00122      05  INTERNAL-TAB-PNTR       REDEFINES                        GA1NPGM 
00123                        INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.   GA1NPGM 
00124                                                                   GA1NPGM 
00125  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1NPGM 
00126                                                                   GA1NPGM 
00127 ** MAP COBOL SCREEN DSECTS **                                     GA1NPGM 
00128  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1NPGM 
00129      '***  I/O MAPAREA ***'.                                      GA1NPGM 
00130  COPY GA1NSETC.                                                   GA1NPGM 
00131      EJECT                                                        GA1NPGM 
00132 ******************************************************************GA1NPGM 
00133 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA1NPGM 
00134 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1NPGM 
00135 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1NPGM 
00136 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1NPGM 
00137 **  REDEFINES.                                                    GA1NPGM 
00138 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00139 **                                                                GA1NPGM 
00140 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1NPGM 
00141 **  FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1NPGM 
00142 **  MATCH THE MAP.                                                GA1NPGM 
00143 **                                                                GA1NPGM 
00144 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00145                                                                   GA1NPGM 
00146  01  FILLER     REDEFINES   GA1NI01I.                             GA1NPGM 
00147      05  FILLER                              PIC X(83).           GA1NPGM 
00148      05  GROUP-SPECIFIC-ID-LINE.                                  GA1NPGM 
00149          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA1NPGM 
00150          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA1NPGM 
00151          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA1NPGM 
00152          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA1NPGM 
00153          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA1NPGM 
00154          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA1NPGM 
00155          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA1NPGM 
00156          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA1NPGM 
00157          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA1NPGM 
00158          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA1NPGM 
00159          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA1NPGM 
00160          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA1NPGM 
00161          10  FILLER                          PIC X(16).           GA1NPGM 
00162      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA1NPGM 
00163          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1NPGM 
00164          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1NPGM 
00165          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1NPGM 
00166          10  CONTRACT-GROUP-NO               PIC X(9).            GA1NPGM 
00167          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1NPGM 
00168          10  CONTRACT-SECTION-NO             PIC X(5).            GA1NPGM 
00169          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1NPGM 
00170          10  CONTRACT-PKG-CODE               PIC X(3).            GA1NPGM 
00171          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1NPGM 
00172          10  CONTRACT-LOB                    PIC X.               GA1NPGM 
00173          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1NPGM 
00174          10  CONTRACT-PROV-CTL               PIC XX.              GA1NPGM 
00175          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1NPGM 
00176          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1NPGM 
00177          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1NPGM 
00178          10  CONTRACT-EFF-DATE               PIC X(6).            GA1NPGM 
00179          10  FILLER                          PIC X(1).            GA1NPGM 
00180      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA1NPGM 
00181                                     GROUP-SPECIFIC-ID-LINE.       GA1NPGM 
00182          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA1NPGM 
00183          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA1NPGM 
00184          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA1NPGM 
00185          10  BEN-PROV-GROUP-NO               PIC X(9).            GA1NPGM 
00186          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA1NPGM 
00187          10  BEN-PROV-SECTION-NO             PIC X(5).            GA1NPGM 
00188          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA1NPGM 
00189          10  BEN-PROV-PKG-CODE               PIC X(3).            GA1NPGM 
00190          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA1NPGM 
00191          10  BEN-PROV-LOB                    PIC X.               GA1NPGM 
00192          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA1NPGM 
00193          10  BEN-PROV-PROV-CTL               PIC XX.              GA1NPGM 
00194          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA1NPGM 
00195          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA1NPGM 
00196          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA1NPGM 
00197          10  BEN-PROV-EFF-DATE               PIC X(6).            GA1NPGM 
00198          10  BEN-PROV-ID-HEADING             PIC X(6).            GA1NPGM 
00199          10  BEN-PROV-ID-NO                  PIC X(6).            GA1NPGM 
00200          10  FILLER                          PIC X(4).            GA1NPGM 
00201      05  FILLER                              PIC X(74).           GA1NPGM 
00202      05  MAP-DIAGNOSIS-CODE-ROW         OCCURS 14 TIMES INDEXED   GA1NPGM 
00203          BY MAP-IDX1.                                             GA1NPGM 
00204        10  MAP-DIAGNOSIS-CODE-COL         OCCURS 3 TIMES INDEXED  GA1NPGM 
00205            BY MAP-IDX2.                                           GA1NPGM 
00206          15  MAP-ACTION-CODE-LEN             PIC S9(4) COMP SYNC. GA1NPGM 
00207          15  MAP-ACTION-CODE-ATTR            PIC X.               GA1NPGM 
00208          15  MAP-ACTION-CODE                 PIC X.               GA1NPGM 
00209          15  MAP-DIAGNOSIS-CODE-LEN          PIC S9(4) COMP SYNC. GA1NPGM 
00210          15  MAP-DIAGNOSIS-CODE-ATTR         PIC X.               GA1NPGM 
00211          15  MAP-DIAGNOSIS-CODE              PIC X(10).           GA1NPGM 
00212          15  FILLER                          PIC X.               GA1NPGM 
00213 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00214  01  WS-MAP-OCCURS-COUNTERS.                                      GA1NPGM 
00215 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00216 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1NPGM 
00217 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00218      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +14.     GA1NPGM 
00219      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +3.      GA1NPGM 
00220 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00221      EJECT                                                        GA1NPGM 
00222 ** ALTERNATIVE WORKFILE KEYS **                                   GA1NPGM 
00223  01  FILLER                      PIC X(32)  VALUE                 GA1NPGM 
00224      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1NPGM 
00225  01  WS-ALT-WORKFILE-KEYS.                                        GA1NPGM 
00226  COPY GCWRKKEY.                                                   GA1NPGM 
00227      EJECT                                                        GA1NPGM 
00228                                                                   GA1NPGM 
00229 ** DATE FORMATTING AREA **                                        GA1NPGM 
00230      EJECT                                                        GA1NPGM 
00231 ** WORKFIELDS, AND SWITCHES **                                    GA1NPGM 
00232  01  WS-WORK-FIELDS.                                              GA1NPGM 
00233      05  WS-HEX-00                     PIC X.                     GA1NPGM 
00234      05  WS-QUOTIENT                   PIC 999  COMP-3.           GA1NPGM 
00235      05  WS-REMAINDER                  PIC 999  COMP-3.           GA1NPGM 
00236      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1NPGM 
00237 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00238 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00239 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00240      05  WS-SAVED-FIELDS.                                         GA1NPGM 
00241        10  WS-SAVED-DIAGNOSIS-CODE                                GA1NPGM 
00242                                        PIC X(10).                 GA1NPGM 
00243 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00244  01  WS-SWITCHES.                                                 GA1NPGM 
00245      05  WS-ERROR-SW                   PIC X.                     GA1NPGM 
00246                                                                   GA1NPGM 
00247 ** TITLE LINES **                                                 GA1NPGM 
00248  01  WS-TITLE-LINES.                                              GA1NPGM 
00249      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA1NPGM 
00250          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA1NPGM 
00251      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA1NPGM 
00252          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA1NPGM 
00253      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA1NPGM 
00254          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA1NPGM 
00255                                                                   GA1NPGM 
00256      EJECT                                                        GA1NPGM 
00257 ** ATTRIBUTES **                                                  GA1NPGM 
00258  COPY DFHBMSCA.                                                   GA1NPGM 
00259      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1NPGM 
00260      EJECT                                                        GA1NPGM 
00261 ** ATTENTION IDENTIFIERS **                                       GA1NPGM 
00262  COPY DFHAID.                                                     GA1NPGM 
00263      EJECT                                                        GA1NPGM 
00264 ** RECORD LENGTHS **                                              GA1NPGM 
00265  01  WS-RECORD-LENGTHS.                                           GA1NPGM 
00266     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA1NPGM 
00267     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA1NPGM 
00268     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1NPGM 
00269     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1NPGM 
00270 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00271 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00272 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00273 ****************************************************************  GA1NPGM 
00274 /                                                                 GA1NPGM 
00275  01  WT-00-GA1NPGM-TABLES.                                        GA1NPGM 
00276      05  FILLER                   PIC X(16)  VALUE                GA1NPGM 
00277          '*GA1NPGM TABLES*'.                                      GA1NPGM 
00278                                                                   GA1NPGM 
00279  01  WT-01-TABLE.                                                 GA1NPGM 
00280      05  FILLER                  PIC X(16) VALUE                  GA1NPGM 
00281          '* WT-01-TABLE  *'.                                      GA1NPGM 
00282 ******************************************************************GA1NPGM 
00283 *    WT-01   MESSAGE TABLE                                       *GA1NPGM 
00284 ******************************************************************GA1NPGM 
00285  01  FILLER.                                                      GA1NPGM 
00286      05  WT-01-MESSAGE-VALUES.                                    GA1NPGM 
00287                                                                   GA1NPGM 
00288 *----------------------------------------------------------------*GA1NPGM 
00289          10  WT-01-ENTRY-001.                                     GA1NPGM 
00290              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00291              15  WT-01-MESSAGE-TEXT-001.                          GA1NPGM 
00292                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00293                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00294                  20  FILLER          PIC X(3)  VALUE  '001'.      GA1NPGM 
00295                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00296                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00297                           '** INVALID REQUEST. THE PF KEY USED HASGA1NPGM 
00298 -                   ' NO MEANING TO THIS PROGRAM **'.             GA1NPGM 
00299              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00300                                                                   GA1NPGM 
00301 *----------------------------------------------------------------*GA1NPGM 
00302          10  WT-01-ENTRY-002.                                     GA1NPGM 
00303              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00304              15  WT-01-MESSAGE-TEXT-002.                          GA1NPGM 
00305                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00306                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00307                  20  FILLER          PIC X(3)  VALUE  '002'.      GA1NPGM 
00308                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00309                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00310                      '** INVALID ACTION CODE FOUND **'.           GA1NPGM 
00311              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00312                                                                   GA1NPGM 
00313 *----------------------------------------------------------------*GA1NPGM 
00314          10  WT-01-ENTRY-003.                                     GA1NPGM 
00315              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00316              15  WT-01-MESSAGE-TEXT-003.                          GA1NPGM 
00317                  20  FILLER          PIC X(4)  VALUE  'GA10'.     GA1NPGM 
00318                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00319                  20  FILLER          PIC X(3)  VALUE  '003'.      GA1NPGM 
00320                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00321                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00322                           '** ERROR READING ALL LEVEL TABULARS CONGA1NPGM 
00323 -                   'TACT SYSTEMS AREA **          '.             GA1NPGM 
00324              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00325 *----------------------------------------------------------------*GA1NPGM 
00326          10  WT-01-ENTRY-004.                                     GA1NPGM 
00327              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00328              15  WT-01-MESSAGE-TEXT-004.                          GA1NPGM 
00329                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00330                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00331                  20  FILLER          PIC X(3)  VALUE  '004'.      GA1NPGM 
00332                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00333                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00334                           '** PROGRAM ERROR IN 2040-DELETE, CONTACGA1NPGM 
00335 -                   'T SYSTEM AREA **              '.             GA1NPGM 
00336              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00337 *----------------------------------------------------------------*GA1NPGM 
00338          10  WT-01-ENTRY-005.                                     GA1NPGM 
00339              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00340              15  WT-01-MESSAGE-TEXT-005.                          GA1NPGM 
00341                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00342                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00343                  20  FILLER          PIC X(3)  VALUE  '005'.      GA1NPGM 
00344                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00345                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00346                           '** PROGRAM ERROR IN 2050-SAVE, CONTACT GA1NPGM 
00347 -                   'SYSTEM AREA **                '.             GA1NPGM 
00348              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00349 *----------------------------------------------------------------*GA1NPGM 
00350          10  WT-01-ENTRY-006.                                     GA1NPGM 
00351              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00352              15  WT-01-MESSAGE-TEXT-006.                          GA1NPGM 
00353                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00354                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00355                  20  FILLER          PIC X(3)  VALUE  '006'.      GA1NPGM 
00356                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00357                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00358                           '** REWRITE ERROR, INTERNAL TABULAR FILEGA1NPGM 
00359 -                   ', CONTACT SYSTEM AREA **      '.             GA1NPGM 
00360              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00361                                                                   GA1NPGM 
00362 *----------------------------------------------------------------*GA1NPGM 
00363          10  WT-01-ENTRY-007.                                     GA1NPGM 
00364              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00365              15  WT-01-MESSAGE-TEXT-007.                          GA1NPGM 
00366                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00367                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00368                  20  FILLER          PIC X(3)  VALUE  '007'.      GA1NPGM 
00369                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00370                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00371                           '** READ ERROR, INTERNAL TABULAR FILE, CGA1NPGM 
00372 -                   'ONTACT SYSTEM AREA **         '.             GA1NPGM 
00373              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00374 *----------------------------------------------------------------*GA1NPGM 
00375          10  WT-01-ENTRY-008.                                     GA1NPGM 
00376              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00377              15  WT-01-MESSAGE-TEXT-008.                          GA1NPGM 
00378                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00379                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00380                  20  FILLER          PIC X(3)  VALUE  '008'.      GA1NPGM 
00381                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00382                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00383               ' ** COMMAREA LENGTH IS INVALID **'.                GA1NPGM 
00384              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00385                                                                   GA1NPGM 
00386 *----------------------------------------------------------------*GA1NPGM 
00387          10  WT-01-ENTRY-009.                                     GA1NPGM 
00388              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00389              15  WT-01-MESSAGE-TEXT-009.                          GA1NPGM 
00390                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00391                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00392                  20  FILLER          PIC X(3)  VALUE  '009'.      GA1NPGM 
00393                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00394                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00395            '** INVALID EFFECTIVE DATE DISCOVERED **'.             GA1NPGM 
00396              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00397                                                                   GA1NPGM 
00398 *----------------------------------------------------------------*GA1NPGM 
00399          10  WT-01-ENTRY-010.                                     GA1NPGM 
00400              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00401              15  WT-01-MESSAGE-TEXT-010.                          GA1NPGM 
00402                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00403                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00404                  20  FILLER          PIC X(3)  VALUE  '010'.      GA1NPGM 
00405                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00406                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00407                  '** NO MORE ENTRIES TO DELETE **'.               GA1NPGM 
00408              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00409                                                                   GA1NPGM 
00410 *----------------------------------------------------------------*GA1NPGM 
00411          10  WT-01-ENTRY-011.                                     GA1NPGM 
00412              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1NPGM 
00413              15  WT-01-MESSAGE-TEXT-011.                          GA1NPGM 
00414                  20  FILLER          PIC X(4)  VALUE  'GA1N'.     GA1NPGM 
00415                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1NPGM 
00416                  20  FILLER          PIC X(3)  VALUE  '011'.      GA1NPGM 
00417                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1NPGM 
00418                  20  FILLER          PIC X(70) VALUE              GA1NPGM 
00419                  '** NO MORE ENTRIES TO DISPLAY **'.              GA1NPGM 
00420              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1NPGM 
00421                                                                   GA1NPGM 
00422 *----------------------------------------------------------------*GA1NPGM 
00423                                                                   GA1NPGM 
00424      05  WT-01-MESSAGE-TABLE         REDEFINES                    GA1NPGM 
00425          WT-01-MESSAGE-VALUES        OCCURS 011 TIMES             GA1NPGM 
00426                                      INDEXED BY WT-01-INDEX.      GA1NPGM 
00427          10  WT-01-ENTRY.                                         GA1NPGM 
00428              15  FILLER              PIC X(02).                   GA1NPGM 
00429              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GA1NPGM 
00430              15  FILLER              PIC X(02).                   GA1NPGM 
00431 *----------------------------------------------------------------*GA1NPGM 
00432 /                                                                 GA1NPGM 
00433 ***                                                               GA1NPGM 
00434 ***                                                               GA1NPGM 
00435 ***                                                               GA1NPGM 
00436 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA1NPGM 
00437  01  FILLER.                                                      GA1NPGM 
00438      COPY GCCDRLEN.                                               GA1NPGM 
00439 /                                                                 GA1NPGM 
00440 ** IO PARM AREA **                                                GA1NPGM 
00441  01  GCPPDIO-PARM-AREA.                                           GA1NPGM 
00442  COPY GCPPDIOC.                                                   GA1NPGM 
00443 /                                                                 GA1NPGM 
00444  01  WS-END                      PIC X(16)  VALUE                 GA1NPGM 
00445      '*** W/S ENDS ***'.                                          GA1NPGM 
00446      EJECT                                                        GA1NPGM 
00447  LINKAGE SECTION.                                                 GA1NPGM 
00448                                                                   GA1NPGM 
00449  01  DFHCOMMAREA.                                                 GA1NPGM 
00450  COPY G2ALCKEC.                                                   GA1NPGM 
00451  COPY GACDACWA.                                                   GA1NPGM 
00452 *    05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA1NPGM 
00453      05  GAS1UPD-PASSED-AREA.                                     GA1NPGM 
00454          07  LVL2-B-SW           PIC X.                           GA1NPGM 
00455          07  LVL2-F-SW           PIC X.                           GA1NPGM 
00456          07  LVL2-G-SW           PIC X.                           GA1NPGM 
00457          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1NPGM 
00458          07  FILLER              PIC X(9).                        GA1NPGM 
00459      05  DELADD-OPTION           PIC X(7).                        GA1NPGM 
00460                                                                   GA1NPGM 
00461 *01  GCA-COMMAREA.                                                GA1NPGM 
00462 *COPY G2ALCKEC.                                                   GA1NPGM 
00463      EJECT                                                        GA1NPGM 
00464  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA1NPGM 
00465  COPY GCIOPRM1.                                                   GA1NPGM 
00466      EJECT                                                        GA1NPGM 
00467  COPY GCWRKDCC.                                                   GA1NPGM 
00468      SKIP3                                                        GA1NPGM 
00469      SKIP3                                                        GA1NPGM 
00470      SKIP3                                                        GA1NPGM 
00471  COPY GCTIDGDC.                                                   GA1NPGM 
00472      EJECT                                                        GA1NPGM 
00473 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00474 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00475 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00476  01  COPY-TABULAR-TABLE-AREA.                                     GA1NPGM 
00477      05  COPY-TABULAR-TABLE  OCCURS 776 TIMES INDEXED BY          GA1NPGM 
00478            COPY-IDX.                                              GA1NPGM 
00479        10  COPY-DIAGNOSIS-CODE                PIC X(10).          GA1NPGM 
00480 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00481      EJECT                                                        GA1NPGM 
00482  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA1NPGM 
00483  COPY GCIOPRM2.                                                   GA1NPGM 
00484      EJECT                                                        GA1NPGM 
00485  COPY GCWRKDC2.                                                   GA1NPGM 
00486      EJECT                                                        GA1NPGM 
00487  COPY GCTABMC.                                                    GA1NPGM 
00488      EJECT                                                        GA1NPGM 
00489                                                                   GA1NPGM 
00490  PROCEDURE DIVISION.                                              GA1NPGM 
00491                                                                   GA1NPGM 
00492 ******************************************************************GA1NPGM 
00493 **                     M A I N L I N E                            GA1NPGM 
00494 **                                                                GA1NPGM 
00495 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1NPGM 
00496 **  TAKEN BY THE OPERATOR.                                        GA1NPGM 
00497 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1NPGM 
00498 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1NPGM 
00499 **     ADDITIONS FROM.                                            GA1NPGM 
00500 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1NPGM 
00501 **     KEY PF12 OR PF24.                                          GA1NPGM 
00502 **  3. RECEIVE THE SCREEN.                                        GA1NPGM 
00503 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1NPGM 
00504 **     MENU.                                                      GA1NPGM 
00505 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1NPGM 
00506 **     LOGIC.                                                     GA1NPGM 
00507 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1NPGM 
00508 **     (RETURN) TO THE ADD PROGRAM (GA2NPGM).                     GA1NPGM 
00509 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1NPGM 
00510 **     (RETURN) TO THE PREVIOUS MENU.                             GA1NPGM 
00511 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1NPGM 
00512 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1NPGM 
00513 **                                                                GA1NPGM 
00514 ******************************************************************GA1NPGM 
00515  1000-MAIN-LINE SECTION.                                          GA1NPGM 
00516                                                                   GA1NPGM 
00517      MOVE '1000'  TO  WS-PARA-ID.                                 GA1NPGM 
00518                                                                   GA1NPGM 
00519      IF EIBAID  =  DFHCLEAR                                       GA1NPGM 
00520          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1NPGM 
00521                         ERASE                                     GA1NPGM 
00522          END-EXEC                                                 GA1NPGM 
00523          EXEC CICS RETURN                                         GA1NPGM 
00524          END-EXEC.                                                GA1NPGM 
00525                                                                   GA1NPGM 
00526      IF EIBTRNID  NOT =  'GA1N'                                   GA1NPGM 
00527         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1NPGM 
00528         GO TO 1099-RETURN.                                        GA1NPGM 
00529                                                                   GA1NPGM 
00530      EXEC CICS RECEIVE   MAP('GA1NI01') MAPSET('GA1NSET')         GA1NPGM 
00531         INTO(GA1NI01I) END-EXEC.                                  GA1NPGM 
00532                                                                   GA1NPGM 
00533      IF SCRNIDNI  NOT =  '001N00'                                 GA1NPGM 
00534         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1NPGM 
00535                                                                   GA1NPGM 
00536      IF EIBAID  =  DFHENTER                                       GA1NPGM 
00537         PERFORM 2000-DELETE-PROCESSING                            GA1NPGM 
00538         GO TO 1099-RETURN.                                        GA1NPGM 
00539                                                                   GA1NPGM 
00540      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1NPGM 
00541         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1NPGM 
00542                                                                   GA1NPGM 
00543      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1NPGM 
00544         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1NPGM 
00545                                                                   GA1NPGM 
00546      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1NPGM 
00547      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1NPGM 
00548      SET WT-01-INDEX TO +01.                                      GA1NPGM 
00549      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1NPGM 
00550      EXEC CICS SEND   MAP('GA1NI01') MAPSET('GA1NSET') DATAONLY   GA1NPGM 
00551         FROM(GA1NI01O) CURSOR END-EXEC.                           GA1NPGM 
00552      GO TO 1099-RETURN.                                           GA1NPGM 
00553                                                                   GA1NPGM 
00554  1000-EXIT. EXIT.                                                 GA1NPGM 
00555                                                                   GA1NPGM 
00556  1099-RETURN.                                                     GA1NPGM 
00557      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA1NPGM 
00558         (DELADD-OPTION = 'GAS1UPD') OR                            GA1NPGM 
00559         (DELADD-OPTION = 'GAS2UPD') OR                            GA1NPGM 
00560         (DELADD-OPTION = 'GAS3UPD') OR                            GA1NPGM 
00561         (DELADD-OPTION = 'GAS4UPD') OR                            GA1NPGM 
00562         (DELADD-OPTION = 'GAS5UPD')                               GA1NPGM 
00563          EXEC CICS RETURN END-EXEC                                GA1NPGM 
00564      ELSE                                                         GA1NPGM 
00565          EXEC CICS RETURN TRANSID('GA1N')                         GA1NPGM 
00566                    COMMAREA(DFHCOMMAREA)                          GA1NPGM 
00567                    LENGTH  (EIBCALEN)                             GA1NPGM 
00568                    END-EXEC.                                      GA1NPGM 
00569                                                                   GA1NPGM 
00570      GOBACK.                                                      GA1NPGM 
00571      EJECT                                                        GA1NPGM 
00572  1099-EXIT. EXIT.                                                 GA1NPGM 
00573 ******************************************************************GA1NPGM 
00574 **              D E L E T E   P R O C E S S I N G                 GA1NPGM 
00575 **                                                                GA1NPGM 
00576 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1NPGM 
00577 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1NPGM 
00578 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1NPGM 
00579 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1NPGM 
00580 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1NPGM 
00581 **    BACK INTO THE RECORD THAT WE READ.)                         GA1NPGM 
00582 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1NPGM 
00583 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1NPGM 
00584 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1NPGM 
00585 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1NPGM 
00586 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1NPGM 
00587 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1NPGM 
00588 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1NPGM 
00589 **    ENTRY.                                                      GA1NPGM 
00590 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1NPGM 
00591 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1NPGM 
00592 **    RECORD.                                                     GA1NPGM 
00593 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1NPGM 
00594 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1NPGM 
00595 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1NPGM 
00596 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1NPGM 
00597 **    ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1NPGM 
00598 **    SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1NPGM 
00599 **    BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1NPGM 
00600 **                                                                GA1NPGM 
00601 ******************************************************************GA1NPGM 
00602  2000-DELETE-PROCESSING SECTION.                                  GA1NPGM 
00603                                                                   GA1NPGM 
00604      MOVE '2000'  TO  WS-PARA-ID.                                 GA1NPGM 
00605      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1NPGM 
00606      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1NPGM 
00607      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1NPGM 
00608                                                                   GA1NPGM 
00609      MOVE '2010'  TO  WS-PARA-ID.                                 GA1NPGM 
00610  2010-VALIDATE-ACT-CODE.                                          GA1NPGM 
00611      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1NPGM 
00612         ADD 1  TO  WS-DELETE-COUNT.                               GA1NPGM 
00613      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D' OR           GA1NPGM 
00614         = SPACE OR =  LOW-VALUES                                  GA1NPGM 
00615         MOVE DFHBMUNF  TO                                         GA1NPGM 
00616            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1NPGM 
00617 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00618 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00619 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00620         MOVE DFHBMASF  TO                                         GA1NPGM 
00621            MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2)           GA1NPGM 
00622      ELSE                                                         GA1NPGM 
00623         MOVE DFHBMUBF  TO                                         GA1NPGM 
00624            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1NPGM 
00625         MOVE DFHBMABF  TO                                         GA1NPGM 
00626            MAP-DIAGNOSIS-CODE-ATTR (MAP-IDX1, MAP-IDX2)           GA1NPGM 
00627 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00628         IF WS-ERROR-SW  NOT =  'Y'                                GA1NPGM 
00629            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1NPGM 
00630            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1NPGM 
00631                                                                   GA1NPGM 
00632      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1NPGM 
00633         SET MAP-IDX1   UP BY  1                                   GA1NPGM 
00634      ELSE                                                         GA1NPGM 
00635         IF MAP-IDX2  <  WS-MAP-COL                                GA1NPGM 
00636            SET MAP-IDX1  TO  1                                    GA1NPGM 
00637            SET MAP-IDX2  UP BY  1                                 GA1NPGM 
00638         ELSE                                                      GA1NPGM 
00639            GO TO 2020-DONE-VALIDATE-A-C.                          GA1NPGM 
00640 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00641 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00642 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00643      IF MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2)                   GA1NPGM 
00644         NOT =  LOW-VALUES                                         GA1NPGM 
00645 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00646         GO TO 2010-VALIDATE-ACT-CODE.                             GA1NPGM 
00647                                                                   GA1NPGM 
00648  2020-DONE-VALIDATE-A-C.                                          GA1NPGM 
00649      MOVE '2020'  TO  WS-PARA-ID.                                 GA1NPGM 
00650      SET MAP-IDX1   TO  1.                                        GA1NPGM 
00651                                                                   GA1NPGM 
00652      IF WS-ERROR-SW  =  'Y'                                       GA1NPGM 
00653         SET WT-01-INDEX TO +02                                    GA1NPGM 
00654         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1NPGM 
00655         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1NPGM 
00656            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA1NPGM 
00657            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA1NPGM 
00658 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00659 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1NPGM 
00660 **  ADD ITS MAP FIELD NAME HERE.                                  GA1NPGM 
00661 ****************************************************************  GA1NPGM 
00662            INCEXCO                                                GA1NPGM 
00663         MOVE '2100'  TO  WS-PARA-ID                               GA1NPGM 
00664         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1NPGM 
00665            VARYING MAP-IDX2 FROM  1  BY  1                        GA1NPGM 
00666                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1NPGM 
00667              AFTER MAP-IDX1 FROM  1  BY  1                        GA1NPGM 
00668                             UNTIL MAP-IDX1  >  WS-MAP-ROW         GA1NPGM 
00669         EXEC CICS SEND   MAP('GA1NI01') MAPSET('GA1NSET') DATAONLYGA1NPGM 
00670            FROM(GA1NI01O) CURSOR END-EXEC                         GA1NPGM 
00671         GO TO 2099-EXIT.                                          GA1NPGM 
00672                                                                   GA1NPGM 
00673      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1NPGM 
00674            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA1NPGM 
00675                    GC-GCTABULR-IDGD-FIXED-LEN +                   GA1NPGM 
00676      (GC-GCTABULR-IDGD-VARY-MAX-OCUR * GC-GCTABULR-IDGD-VARY-LEN).GA1NPGM 
00677                                                                   GA1NPGM 
00678      EXEC CICS                                                    GA1NPGM 
00679         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA1NPGM 
00680         INITIMG(WS-HEX-00)                                        GA1NPGM 
00681         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA1NPGM 
00682      END-EXEC.                                                    GA1NPGM 
00683                                                                   GA1NPGM 
00684      IF  FRMNUIDI  =  'GS3A'                                      GA1NPGM 
00685         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1NPGM 
00686         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1NPGM 
00687         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA1NPGM 
00688         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1NPGM 
00689 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1NPGM 
00690         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1NPGM 
00691 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1NPGM 
00692         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1NPGM 
00693         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1NPGM 
00694         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1NPGM 
00695                          GCIO-WRK-PROVIDER-CONTROL                GA1NPGM 
00696         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1NPGM 
00697         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1NPGM 
00698                                                                   GA1NPGM 
00699      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1NPGM 
00700         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1NPGM 
00701         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1NPGM 
00702         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1NPGM 
00703         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1NPGM 
00704 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1NPGM 
00705         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1NPGM 
00706 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1NPGM 
00707         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1NPGM 
00708         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1NPGM 
00709         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1NPGM 
00710         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1NPGM 
00711         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1NPGM 
00712         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1NPGM 
00713                                                                   GA1NPGM 
00714      IF  FRMNUIDI  =  'GC8A'                                      GA1NPGM 
00715         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1NPGM 
00716         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1NPGM 
00717         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA1NPGM 
00718         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1NPGM 
00719 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1NPGM 
00720         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1NPGM 
00721 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1NPGM 
00722         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1NPGM 
00723         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1NPGM 
00724         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1NPGM 
00725         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1NPGM 
00726         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1NPGM 
00727         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1NPGM 
00728                                                                   GA1NPGM 
00729      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA1NPGM 
00730      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1NPGM 
00731      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA1NPGM 
00732      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA1NPGM 
00733      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA1NPGM 
00734      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA1NPGM 
00735      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1NPGM 
00736                                                                   GA1NPGM 
00737      IF WS-DELETE-COUNT  =  ZERO                                  GA1NPGM 
00738         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1NPGM 
00739                                                                   GA1NPGM 
00740 ******************************************************************GA1NPGM 
00741 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1NPGM 
00742 *                                                                 GA1NPGM 
00743 ******************************************************************GA1NPGM 
00744                                                                   GA1NPGM 
00745      MOVE GC-GCTABULR-IDGD-VARY-MAX-OCUR                          GA1NPGM 
00746        TO GX9-ENTRY-COUNT.                                        GA1NPGM 
00747                                                                   GA1NPGM 
00748      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1NPGM 
00749                                                                   GA1NPGM 
00750      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1NPGM 
00751         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1NPGM 
00752         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1NPGM 
00753                                                                   GA1NPGM 
00754      IF  NOT GCIO-GOOD-RETURN                                     GA1NPGM 
00755         SET WT-01-INDEX TO +03                                    GA1NPGM 
00756         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1NPGM 
00757         MOVE '1N01'  TO  WS-ABEND-CODE                            GA1NPGM 
00758         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1NPGM 
00759                                                                   GA1NPGM 
00760      COMPUTE WS-COPY-LENGTH  =                                    GA1NPGM 
00761              GX9-ENTRY-COUNT  *  GC-GCTABULR-IDGD-VARY-LEN.       GA1NPGM 
00762                                                                   GA1NPGM 
00763      EXEC CICS                                                    GA1NPGM 
00764         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA1NPGM 
00765         LENGTH      (WS-COPY-LENGTH)                              GA1NPGM 
00766         INITIMG     (WS-HEX-00)                                   GA1NPGM 
00767      END-EXEC.                                                    GA1NPGM 
00768                                                                   GA1NPGM 
00769      MOVE GX9-ENTRY-COUNT  TO  GX9-ENTRY-COUNT.                   GA1NPGM 
00770      SET COPY-IDX,  GX9-INDEX  TO  1.                             GA1NPGM 
00771                                                                   GA1NPGM 
00772      MOVE '2030'  TO  WS-PARA-ID.                                 GA1NPGM 
00773  2030-MAKE-A-COPY-OF-RECORD.                                      GA1NPGM 
00774      IF GX9-INDEX  NOT >  GX9-ENTRY-COUNT                         GA1NPGM 
00775         MOVE GX9-ENTRY (GX9-INDEX)  TO                            GA1NPGM 
00776            COPY-TABULAR-TABLE (COPY-IDX)                          GA1NPGM 
00777            SET COPY-IDX,  GX9-INDEX  UP BY  1                     GA1NPGM 
00778            GO TO 2030-MAKE-A-COPY-OF-RECORD.                      GA1NPGM 
00779      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GX9-INDEX  TO  1.         GA1NPGM 
00780                                                                   GA1NPGM 
00781      MOVE '2040'  TO  WS-PARA-ID.                                 GA1NPGM 
00782  2040-DELETE-MARKED-ENTRIES.                                      GA1NPGM 
00783 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00784 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00785 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00786      IF MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2)         =         GA1NPGM 
00787            LOW-VALUES                                             GA1NPGM 
00788         GO TO 2060-SAVE-REST-OF-COPY.                             GA1NPGM 
00789                                                                   GA1NPGM 
00790      IF MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2)         >         GA1NPGM 
00791         COPY-DIAGNOSIS-CODE (COPY-IDX)                            GA1NPGM 
00792         GO TO 2050-SAVE-COPIED-ENTRY                              GA1NPGM 
00793      ELSE                                                         GA1NPGM 
00794         IF MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2)         <      GA1NPGM 
00795            COPY-DIAGNOSIS-CODE (COPY-IDX)                         GA1NPGM 
00796            MOVE '1N02'  TO  WS-ABEND-CODE                         GA1NPGM 
00797            SET WT-01-INDEX TO +04                                 GA1NPGM 
00798            PERFORM 9000-000-MOVE-MSG-TO-SCREEN                    GA1NPGM 
00799            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1NPGM 
00800                                                                   GA1NPGM 
00801 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00802                                                                   GA1NPGM 
00803      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1NPGM 
00804         IF MAP-IDX1   <  WS-MAP-ROW                               GA1NPGM 
00805            SET MAP-IDX1   UP BY  1                                GA1NPGM 
00806            GO TO 2050-SAVE-COPIED-ENTRY                           GA1NPGM 
00807         ELSE                                                      GA1NPGM 
00808            IF MAP-IDX2  <  WS-MAP-COL                             GA1NPGM 
00809               SET MAP-IDX1  TO  1                                 GA1NPGM 
00810               SET MAP-IDX2  UP BY  1                              GA1NPGM 
00811               GO TO 2050-SAVE-COPIED-ENTRY                        GA1NPGM 
00812            ELSE                                                   GA1NPGM 
00813               GO TO 2060-SAVE-REST-OF-COPY.                       GA1NPGM 
00814                                                                   GA1NPGM 
00815      SET COPY-IDX  UP BY  1.                                      GA1NPGM 
00816      IF COPY-IDX  NOT <  GX9-ENTRY-COUNT                          GA1NPGM 
00817         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    GA1NPGM 
00818            GX9-ENTRY (GX9-INDEX)                                  GA1NPGM 
00819         SET  GX9-ENTRY-COUNT  TO  GX9-INDEX                       GA1NPGM 
00820         MOVE GX9-ENTRY-COUNT  TO  GX9-ENTRY-COUNT                 GA1NPGM 
00821         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1NPGM 
00822      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1NPGM 
00823         SET MAP-IDX1   UP BY  1                                   GA1NPGM 
00824         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1NPGM 
00825      IF MAP-IDX2  <  WS-MAP-COL                                   GA1NPGM 
00826         SET MAP-IDX1  TO  1                                       GA1NPGM 
00827         SET MAP-IDX2  UP BY  1                                    GA1NPGM 
00828         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1NPGM 
00829      ELSE                                                         GA1NPGM 
00830         GO TO 2060-SAVE-REST-OF-COPY.                             GA1NPGM 
00831                                                                   GA1NPGM 
00832  2050-SAVE-COPIED-ENTRY.                                          GA1NPGM 
00833      MOVE '2050'  TO  WS-PARA-ID.                                 GA1NPGM 
00834      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1NPGM 
00835         GX9-ENTRY (GX9-INDEX).                                    GA1NPGM 
00836                                                                   GA1NPGM 
00837      SET GX9-INDEX  UP BY  1.                                     GA1NPGM 
00838      IF COPY-IDX  <  GX9-ENTRY-COUNT                              GA1NPGM 
00839         SET COPY-IDX  UP BY  1                                    GA1NPGM 
00840         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1NPGM 
00841      ELSE                                                         GA1NPGM 
00842 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1NPGM 
00843 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1NPGM 
00844 ***      THE TABLE OF ENTRIES.                                    GA1NPGM 
00845         MOVE '1N03'  TO  WS-ABEND-CODE                            GA1NPGM 
00846         SET WT-01-INDEX TO +05                                    GA1NPGM 
00847         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1NPGM 
00848         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1NPGM 
00849                                                                   GA1NPGM 
00850  2060-SAVE-REST-OF-COPY.                                          GA1NPGM 
00851      MOVE '2060'  TO  WS-PARA-ID.                                 GA1NPGM 
00852      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1NPGM 
00853         GX9-ENTRY (GX9-INDEX).                                    GA1NPGM 
00854                                                                   GA1NPGM 
00855      SET GX9-INDEX  UP BY  1.                                     GA1NPGM 
00856      IF COPY-IDX  <  GX9-ENTRY-COUNT                              GA1NPGM 
00857         SET COPY-IDX  UP BY  1                                    GA1NPGM 
00858         GO TO 2060-SAVE-REST-OF-COPY.                             GA1NPGM 
00859                                                                   GA1NPGM 
00860      SET GX9-INDEX  DOWN BY  1.                                   GA1NPGM 
00861      SET GX9-ENTRY-COUNT  TO  GX9-INDEX.                          GA1NPGM 
00862      MOVE GX9-ENTRY-COUNT  TO  GX9-ENTRY-COUNT.                   GA1NPGM 
00863                                                                   GA1NPGM 
00864  2070-UPDATE-MODIFIED-REC.                                        GA1NPGM 
00865      MOVE '2070'  TO  WS-PARA-ID.                                 GA1NPGM 
00866                                                                   GA1NPGM 
00867 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1NPGM 
00868                                                                   GA1NPGM 
00869      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1NPGM 
00870      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1NPGM 
00871                                                                   GA1NPGM 
00872      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1NPGM 
00873         GC-WORKFILE-KEY-LEN +  GC-GCTABULR-IDGD-FIXED-LEN +       GA1NPGM 
00874             (GX9-ENTRY-COUNT  *  GC-GCTABULR-IDGD-VARY-LEN).      GA1NPGM 
00875                                                                   GA1NPGM 
00876      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1NPGM 
00877         GC-GCIOPARM-LEN + GCIO-RECORD-LENGTH.                     GA1NPGM 
00878                                                                   GA1NPGM 
00879      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1NPGM 
00880         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1NPGM 
00881         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1NPGM 
00882                                                                   GA1NPGM 
00883      IF GCIO-GOOD-RETURN                                          GA1NPGM 
00884         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1NPGM 
00885      SET WT-01-INDEX TO +06.                                      GA1NPGM 
00886      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1NPGM 
00887      MOVE '1N04'  TO  WS-ABEND-CODE.                              GA1NPGM 
00888      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1NPGM 
00889                                                                   GA1NPGM 
00890  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1NPGM 
00891      MOVE  '2080'  TO  WS-PARA-ID.                                GA1NPGM 
00892                                                                   GA1NPGM 
00893      MOVE GC-GCTABULR-IDGD-VARY-MAX-OCUR                          GA1NPGM 
00894        TO GX9-ENTRY-COUNT.                                        GA1NPGM 
00895                                                                   GA1NPGM 
00896      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1NPGM 
00897                                                                   GA1NPGM 
00898      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1NPGM 
00899         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1NPGM 
00900         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1NPGM 
00901                                                                   GA1NPGM 
00902      IF GCIO-GOOD-RETURN                                          GA1NPGM 
00903         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1NPGM 
00904      MOVE '1N05'  TO  WS-ABEND-CODE.                              GA1NPGM 
00905      SET WT-01-INDEX TO +07.                                      GA1NPGM 
00906      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1NPGM 
00907      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1NPGM 
00908                                                                   GA1NPGM 
00909  2090-BUILD-NEXT-DISPLAY.                                         GA1NPGM 
00910      MOVE  '2090'  TO  WS-PARA-ID.                                GA1NPGM 
00911      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1NPGM 
00912      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1NPGM 
00913      SET GX9-INDEX  TO  1.                                        GA1NPGM 
00914 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00915 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00916 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00917      IF MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2)         =         GA1NPGM 
00918            LOW-VALUES                                             GA1NPGM 
00919         MOVE GX9-ENTRY (GX9-INDEX)  TO  WS-SAVED-FIELDS           GA1NPGM 
00920      ELSE                                                         GA1NPGM 
00921         MOVE MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2) TO           GA1NPGM 
00922            WS-SAVED-DIAGNOSIS-CODE.                               GA1NPGM 
00923 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00924                                                                   GA1NPGM 
00925      PERFORM 4500-FILL-THE-SCREEN.                                GA1NPGM 
00926      EXEC CICS SEND   MAP('GA1NI01') MAPSET('GA1NSET') ERASE      GA1NPGM 
00927         FROM(GA1NI01O) END-EXEC.                                  GA1NPGM 
00928                                                                   GA1NPGM 
00929  2099-EXIT.   EXIT.                                               GA1NPGM 
00930      EJECT                                                        GA1NPGM 
00931  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1NPGM 
00932 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00933 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
00934 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00935      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1NPGM 
00936         MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2).                  GA1NPGM 
00937 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
00938                                                                   GA1NPGM 
00939  2199-EXIT.   EXIT.                                               GA1NPGM 
00940      EJECT                                                        GA1NPGM 
00941 ******************************************************************GA1NPGM 
00942 **          X C T L   T O   A D D   S C R E E N                   GA1NPGM 
00943 **                                                                GA1NPGM 
00944 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1NPGM 
00945 ** ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1NPGM 
00946 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1NPGM 
00947 ** TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1NPGM 
00948 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1NPGM 
00949 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1NPGM 
00950 ******************************************************************GA1NPGM 
00951  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1NPGM 
00952      MOVE '3000'  TO  WS-PARA-ID.                                 GA1NPGM 
00953                                                                   GA1NPGM 
00954 *    EXEC CICS                                                    GA1NPGM 
00955 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1NPGM 
00956 *       INITIMG(WS-HEX-00)                                        GA1NPGM 
00957 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1NPGM 
00958 *    END-EXEC.                                                    GA1NPGM 
00959                                                                   GA1NPGM 
00960 *    IF  FRMNUIDI  =  'GS3A'                                      GA1NPGM 
00961 **      MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA1NPGM 
00962 *       MOVE  GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                   GA1NPGM 
00963 *       MOVE  GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO               GA1NPGM 
00964 *       MOVE  GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1NPGM 
00965 *       MOVE  GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1NPGM 
00966 *       MOVE  SPACES  TO  GCA-L-O-B,                              GA1NPGM 
00967 *                         GCA-PROV-CTL,                           GA1NPGM 
00968 *                         GCA-BEN-PROV-ID.                        GA1NPGM 
00969                                                                   GA1NPGM 
00970 *    IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1NPGM 
00971 **      MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA1NPGM 
00972 *       MOVE  CONTRACT-GROUP-NO  TO  GCA-GRP-NO                   GA1NPGM 
00973 *       MOVE  CONTRACT-SECTION-NO  TO  GCA-SECTN-NO               GA1NPGM 
00974 *       MOVE  CONTRACT-LOB  TO  GCA-L-O-B                         GA1NPGM 
00975 *       MOVE  CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                 GA1NPGM 
00976 *       MOVE  CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1NPGM 
00977 *       MOVE  CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1NPGM 
00978 *       MOVE  SPACES  TO  GCA-BEN-PROV-ID.                        GA1NPGM 
00979                                                                   GA1NPGM 
00980 *    IF  FRMNUIDI  =  'GC8A'                                      GA1NPGM 
00981 **      MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA1NPGM 
00982 *       MOVE  BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                   GA1NPGM 
00983 *       MOVE  BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO               GA1NPGM 
00984 *       MOVE  BEN-PROV-LOB  TO  GCA-L-O-B                         GA1NPGM 
00985 *       MOVE  BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                 GA1NPGM 
00986 *       MOVE  BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1NPGM 
00987 *       MOVE  BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1NPGM 
00988 *       MOVE  BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                GA1NPGM 
00989                                                                   GA1NPGM 
00990      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1NPGM 
00991      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1NPGM 
00992      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1NPGM 
00993      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1NPGM 
00994      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1NPGM 
00995      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1NPGM 
00996      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1NPGM 
00997      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1NPGM 
00998 *    MOVE  ZEROES  TO  GCA-EFF-DT.                                GA1NPGM 
00999 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01000 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA1NPGM 
01001 ** FIELDS TO DISPLAY ON THE INITIAL ADD SCREEN THEY SHOULD BE     GA1NPGM 
01002 ** PASSED HERE.                                                   GA1NPGM 
01003 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01004      MOVE INCEXCI TO GCA-I-E-INDC.                                GA1NPGM 
01005                                                                   GA1NPGM 
01006 *    SET COMMAREA-PNTR                                            GA1NPGM 
01007 *      TO ADDRESS OF GCA-COMMAREA.                                GA1NPGM 
01008                                                                   GA1NPGM 
01009 *    EXEC CICS XCTL  PROGRAM('GA2NPGM') COMMAREA(COMMAREA-PNTR)   GA1NPGM 
01010 *       LENGTH(4) END-EXEC.                                       GA1NPGM 
01011      EXEC CICS XCTL PROGRAM('GA2NPGM')                            GA1NPGM 
01012                     COMMAREA(DFHCOMMAREA)                         GA1NPGM 
01013                     LENGTH (LENGTH OF DFHCOMMAREA)                GA1NPGM 
01014      END-EXEC.                                                    GA1NPGM 
01015                                                                   GA1NPGM 
01016  3099-EXIT.   EXIT.                                               GA1NPGM 
01017      EJECT                                                        GA1NPGM 
01018 ***************************************************************** GA1NPGM 
01019 **          D I S P L A Y   F I R S T   S C R E E N               GA1NPGM 
01020 **                                                                GA1NPGM 
01021 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1NPGM 
01022 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1NPGM 
01023 ** ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1NPGM 
01024 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1NPGM 
01025 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1NPGM 
01026 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1NPGM 
01027 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1NPGM 
01028 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1NPGM 
01029 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1NPGM 
01030 ** DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1NPGM 
01031 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1NPGM 
01032 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1NPGM 
01033 ******************************************************************GA1NPGM 
01034  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1NPGM 
01035      MOVE '4000'  TO  WS-PARA-ID.                                 GA1NPGM 
01036                                                                   GA1NPGM 
01037 ***  D185     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY         GA1NPGM 
01038 *                                                                 GA1NPGM 
01039      MOVE LOW-VALUES TO GA1NI01I.                                 GA1NPGM 
01040                                                                   GA1NPGM 
01041      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1NPGM 
01042         SET WT-01-INDEX TO +08                                    GA1NPGM 
01043         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1NPGM 
01044         MOVE '1N06'  TO  WS-ABEND-CODE                            GA1NPGM 
01045         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1NPGM 
01046                                                                   GA1NPGM 
01047 *    SET ADDRESS OF GCA-COMMAREA                                  GA1NPGM 
01048 *      TO INCOMING-COMMAREA-PNTR.                                 GA1NPGM 
01049                                                                   GA1NPGM 
01050      SET ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD                   GA1NPGM 
01051        TO GCA-RECORD-POINTER.                                     GA1NPGM 
01052                                                                   GA1NPGM 
01053      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA1NPGM 
01054      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA1NPGM 
01055      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA1NPGM 
01056      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA1NPGM 
01057      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA1NPGM 
01058      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA1NPGM 
01059      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA1NPGM 
01060      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA1NPGM 
01061 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01062 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1NPGM 
01063 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1NPGM 
01064 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01065      MOVE GX9-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               GA1NPGM 
01066      MOVE GCA-I-E-INDC TO INCEXCO.                                GA1NPGM 
01067                                                                   GA1NPGM 
01068      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1NPGM 
01069         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA1NPGM 
01070 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA1NPGM 
01071         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA1NPGM 
01072         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA1NPGM 
01073         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA1NPGM 
01074         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA1NPGM 
01075         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1NPGM 
01076         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA1NPGM 
01077         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA1NPGM 
01078         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA1NPGM 
01079         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1NPGM 
01080         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1NPGM 
01081         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1NPGM 
01082         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1NPGM 
01083                                                                   GA1NPGM 
01084      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1NPGM 
01085         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA1NPGM 
01086 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1NPGM 
01087         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA1NPGM 
01088         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA1NPGM 
01089         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA1NPGM 
01090         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA1NPGM 
01091         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1NPGM 
01092         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA1NPGM 
01093         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA1NPGM 
01094         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA1NPGM 
01095         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1NPGM 
01096         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1NPGM 
01097         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1NPGM 
01098         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1NPGM 
01099         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1NPGM 
01100         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1NPGM 
01101         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1NPGM 
01102         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1NPGM 
01103                                                                   GA1NPGM 
01104      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1NPGM 
01105         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA1NPGM 
01106         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA1NPGM 
01107         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA1NPGM 
01108         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA1NPGM 
01109         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA1NPGM 
01110         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA1NPGM 
01111         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA1NPGM 
01112         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA1NPGM 
01113         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA1NPGM 
01114         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA1NPGM 
01115         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1NPGM 
01116         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA1NPGM 
01117         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1NPGM 
01118         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA1NPGM 
01119         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1NPGM 
01120         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA1NPGM 
01121         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1NPGM 
01122         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA1NPGM 
01123         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1NPGM 
01124                                                                   GA1NPGM 
01125      SET GX9-INDEX  TO  1.                                        GA1NPGM 
01126      MOVE GX9-ENTRY (GX9-INDEX)  TO  WS-SAVED-FIELDS.             GA1NPGM 
01127                                                                   GA1NPGM 
01128      PERFORM 4500-FILL-THE-SCREEN.                                GA1NPGM 
01129      EXEC CICS SEND   MAP('GA1NI01') MAPSET('GA1NSET') ERASE      GA1NPGM 
01130         FROM(GA1NI01O) END-EXEC.                                  GA1NPGM 
01131                                                                   GA1NPGM 
01132  4099-EXIT.   EXIT.                                               GA1NPGM 
01133      EJECT                                                        GA1NPGM 
01134 ***************************************************************** GA1NPGM 
01135 **             F I L L   T H E   S C R E E N                      GA1NPGM 
01136 **                                                                GA1NPGM 
01137 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1NPGM 
01138 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1NPGM 
01139 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1NPGM 
01140 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1NPGM 
01141 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1NPGM 
01142 ******************************************************************GA1NPGM 
01143  4500-FILL-THE-SCREEN SECTION.                                    GA1NPGM 
01144                                                                   GA1NPGM 
01145      MOVE '4500'  TO  WS-PARA-ID.                                 GA1NPGM 
01146      MOVE  GX9-ENTRY-COUNT  TO  GX9-ENTRY-COUNT.                  GA1NPGM 
01147      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1NPGM 
01148                                                                   GA1NPGM 
01149      IF GX9-ENTRY-COUNT  NOT >  1                                 GA1NPGM 
01150         MOVE '4530'  TO  WS-PARA-ID                               GA1NPGM 
01151         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1NPGM 
01152                                                                   GA1NPGM 
01153      SET GX9-INDEX  TO  1.                                        GA1NPGM 
01154      MOVE '4510'  TO  WS-PARA-ID.                                 GA1NPGM 
01155  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1NPGM 
01156 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01157 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
01158 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01159      IF GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX)         <              GA1NPGM 
01160            WS-SAVED-DIAGNOSIS-CODE                                GA1NPGM 
01161 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01162         SET GX9-INDEX  UP BY  1                                   GA1NPGM 
01163         IF  GX9-INDEX  <  GX9-ENTRY-COUNT                         GA1NPGM 
01164            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1NPGM 
01165         ELSE                                                      GA1NPGM 
01166            SET GX9-INDEX  TO  1.                                  GA1NPGM 
01167                                                                   GA1NPGM 
01168      MOVE '4520'  TO  WS-PARA-ID.                                 GA1NPGM 
01169  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1NPGM 
01170      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1NPGM 
01171      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1NPGM 
01172 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01173 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
01174 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01175      MOVE GX9-DIAGNOSIS-ARGUMENT (GX9-INDEX)    TO                GA1NPGM 
01176         MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2).                  GA1NPGM 
01177 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01178                                                                   GA1NPGM 
01179      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1NPGM 
01180         SET  MAP-IDX1  UP BY  1                                   GA1NPGM 
01181      ELSE                                                         GA1NPGM 
01182         IF MAP-IDX2  <  WS-MAP-COL                                GA1NPGM 
01183            SET  MAP-IDX1  TO  1                                   GA1NPGM 
01184            SET  MAP-IDX2  UP BY  1                                GA1NPGM 
01185         ELSE                                                      GA1NPGM 
01186            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1NPGM 
01187                                                                   GA1NPGM 
01188      IF GX9-INDEX  <  (GX9-ENTRY-COUNT - 1 )                      GA1NPGM 
01189         SET  GX9-INDEX  UP BY  1                                  GA1NPGM 
01190         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1NPGM 
01191                                                                   GA1NPGM 
01192      MOVE '4530'  TO  WS-PARA-ID.                                 GA1NPGM 
01193  4530-FILL-REST-WITH-NULLS.                                       GA1NPGM 
01194      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1NPGM 
01195 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01196 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1NPGM 
01197 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01198      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1NPGM 
01199         MAP-DIAGNOSIS-CODE (MAP-IDX1, MAP-IDX2).                  GA1NPGM 
01200 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1NPGM 
01201      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1NPGM 
01202         SET  MAP-IDX1   UP BY  1                                  GA1NPGM 
01203         GO TO 4530-FILL-REST-WITH-NULLS                           GA1NPGM 
01204      ELSE                                                         GA1NPGM 
01205         IF MAP-IDX2  <  WS-MAP-COL                                GA1NPGM 
01206            SET  MAP-IDX1  TO  1                                   GA1NPGM 
01207            SET  MAP-IDX2  UP BY 1                                 GA1NPGM 
01208            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1NPGM 
01209                                                                   GA1NPGM 
01210      MOVE '4540'  TO  WS-PARA-ID.                                 GA1NPGM 
01211  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1NPGM 
01212      IF GX9-ENTRY-COUNT  =  1                                     GA1NPGM 
01213         SET WT-01-INDEX TO +10                                    GA1NPGM 
01214         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1NPGM 
01215         GO TO 4599-EXIT.                                          GA1NPGM 
01216                                                                   GA1NPGM 
01217      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1NPGM 
01218         SET WT-01-INDEX TO +11                                    GA1NPGM 
01219         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA1NPGM 
01220                                                                   GA1NPGM 
01221  4599-EXIT.     EXIT.                                             GA1NPGM 
01222      EJECT                                                        GA1NPGM 
01223 ***************************************************************** GA1NPGM 
01224 **        X C T L   T O   P R E V I O U S   M E N U               GA1NPGM 
01225 **                                                                GA1NPGM 
01226 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1NPGM 
01227 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1NPGM 
01228 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1NPGM 
01229 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1NPGM 
01230 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1NPGM 
01231 ******************************************************************GA1NPGM 
01232  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1NPGM 
01233      MOVE '5000'  TO  WS-PARA-ID.                                 GA1NPGM 
01234                                                                   GA1NPGM 
01235                                                                   GA1NPGM 
01236 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA1NPGM 
01237 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA1NPGM 
01238 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA1NPGM 
01239                                                                   GA1NPGM 
01240      IF  ALTBFNCI  =  'GTM1'                                      GA1NPGM 
01241          EXEC CICS XCTL                                           GA1NPGM 
01242                    PROGRAM('GTM1PGM')                             GA1NPGM 
01243                    END-EXEC.                                      GA1NPGM 
01244                                                                   GA1NPGM 
01245                                                                   GA1NPGM 
01246      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA1NPGM 
01247          GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +                 GA1NPGM 
01248                 GC-GCTABULR-ABM-FIXED-LEN +                       GA1NPGM 
01249         (GC-GCTABULR-ABM-VARY-MAX-OCUR *                          GA1NPGM 
01250                 GC-GCTABULR-ABM-VARY-LEN).                        GA1NPGM 
01251                                                                   GA1NPGM 
01252      EXEC CICS                                                    GA1NPGM 
01253         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA1NPGM 
01254         INITIMG(WS-HEX-00)                                        GA1NPGM 
01255         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA1NPGM 
01256      END-EXEC.                                                    GA1NPGM 
01257                                                                   GA1NPGM 
01258 *    EXEC CICS                                                    GA1NPGM 
01259 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1NPGM 
01260 *       INITIMG(WS-HEX-00)                                        GA1NPGM 
01261 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1NPGM 
01262 *    END-EXEC.                                                    GA1NPGM 
01263                                                                   GA1NPGM 
01264      IF  FRMNUIDI  =  'GS3A'                                      GA1NPGM 
01265         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1NPGM 
01266         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1NPGM 
01267         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA1NPGM 
01268         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1NPGM 
01269         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1NPGM 
01270         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1NPGM 
01271         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1NPGM 
01272         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1NPGM 
01273                          GCIO-WRK-PROVIDER-CONTROL                GA1NPGM 
01274         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1NPGM 
01275         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1NPGM 
01276         MOVE SPACES TO GCA-BEN-PROV-ID                            GA1NPGM 
01277         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA1NPGM 
01278                          GCA-ALL-LEVEL-TAB-ID                     GA1NPGM 
01279         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA1NPGM 
01280                          GCA-ALL-LEVEL-TAB-SLOT                   GA1NPGM 
01281         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA1NPGM 
01282                        GCA-INTERNAL-TAB-ID                        GA1NPGM 
01283                        GCA-INTERNAL-TAB-SLOT                      GA1NPGM 
01284         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA1NPGM 
01285                                                                   GA1NPGM 
01286      IF  FRMNUIDI  =  'GC4A'                                      GA1NPGM 
01287         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1NPGM 
01288         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1NPGM 
01289         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1NPGM 
01290         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1NPGM 
01291         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1NPGM 
01292         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1NPGM 
01293         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1NPGM 
01294         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1NPGM 
01295         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1NPGM 
01296         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1NPGM 
01297         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1NPGM 
01298         MOVE SPACES TO GCA-BEN-PROV-ID                            GA1NPGM 
01299         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA1NPGM 
01300                          GCA-ALL-LEVEL-TAB-ID                     GA1NPGM 
01301         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA1NPGM 
01302                          GCA-ALL-LEVEL-TAB-SLOT                   GA1NPGM 
01303         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA1NPGM 
01304                        GCA-INTERNAL-TAB-ID                        GA1NPGM 
01305                        GCA-INTERNAL-TAB-SLOT                      GA1NPGM 
01306         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA1NPGM 
01307                                                                   GA1NPGM 
01308      IF  FRMNUIDI  =  'GC8A'                                      GA1NPGM 
01309         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1NPGM 
01310         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1NPGM 
01311         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA1NPGM 
01312         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1NPGM 
01313         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1NPGM 
01314         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1NPGM 
01315         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1NPGM 
01316         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1NPGM 
01317         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1NPGM 
01318         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1NPGM 
01319         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1NPGM 
01320         MOVE GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID            GA1NPGM 
01321         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1NPGM 
01322         MOVE ALTABIDI TO GCIO-WRK-TAB-PROVISION-ID                GA1NPGM 
01323                          GCA-ALL-LEVEL-TAB-ID                     GA1NPGM 
01324         MOVE ALTBSLTI TO GCIO-WRK-TAB-PROV-SLOT-NO                GA1NPGM 
01325                          GCA-ALL-LEVEL-TAB-SLOT                   GA1NPGM 
01326         MOVE SPACES TO GCA-INTERNAL-TAB-ID                        GA1NPGM 
01327                        GCA-INTERNAL-TAB-SLOT.                     GA1NPGM 
01328                                                                   GA1NPGM 
01329      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA1NPGM 
01330 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA1NPGM 
01331 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA1NPGM 
01332 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA1NPGM 
01333 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA1NPGM 
01334 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA1NPGM 
01335      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1NPGM 
01336                                                                   GA1NPGM 
01337      SET GCA-RECORD-POINTER                                       GA1NPGM 
01338        TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                    GA1NPGM 
01339                                                                   GA1NPGM 
01340      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GA1NPGM 
01341        TO GAA-ENTRY-COUNT.                                        GA1NPGM 
01342                                                                   GA1NPGM 
01343      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1NPGM 
01344                                                                   GA1NPGM 
01345      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1NPGM 
01346         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA1NPGM 
01347         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA1NPGM 
01348                                                                   GA1NPGM 
01349      IF  NOT GCIO2-GOOD-RETURN                                    GA1NPGM 
01350         SET WT-01-INDEX TO +03                                    GA1NPGM 
01351         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1NPGM 
01352         MOVE '1N08'  TO  WS-ABEND-CODE                            GA1NPGM 
01353         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1NPGM 
01354                                                                   GA1NPGM 
01355 *    SET COMMAREA-PNTR                                            GA1NPGM 
01356 *      TO ADDRESS OF GCA-COMMAREA.                                GA1NPGM 
01357                                                                   GA1NPGM 
01358      IF  ALTBFNCI  =  'GA1B'                                      GA1NPGM 
01359 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA1NPGM 
01360 *          LENGTH(4) END-EXEC.                                    GA1NPGM 
01361         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA1NPGM 
01362                         COMMAREA(DFHCOMMAREA)                     GA1NPGM 
01363                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1NPGM 
01364         END-EXEC.                                                 GA1NPGM 
01365                                                                   GA1NPGM 
01366      IF  ALTBFNCI  =  'GA1C'                                      GA1NPGM 
01367 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA1NPGM 
01368 *          LENGTH(4) END-EXEC.                                    GA1NPGM 
01369         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA1NPGM 
01370                         COMMAREA(DFHCOMMAREA)                     GA1NPGM 
01371                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1NPGM 
01372         END-EXEC.                                                 GA1NPGM 
01373                                                                   GA1NPGM 
01374      IF  ALTBFNCI  =  'GA1D'                                      GA1NPGM 
01375 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA1NPGM 
01376 *          LENGTH(4) END-EXEC.                                    GA1NPGM 
01377         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA1NPGM 
01378                         COMMAREA(DFHCOMMAREA)                     GA1NPGM 
01379                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1NPGM 
01380         END-EXEC.                                                 GA1NPGM 
01381                                                                   GA1NPGM 
01382      IF  ALTBFNCI  =  'GA1E'                                      GA1NPGM 
01383 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA1NPGM 
01384 *          LENGTH(4) END-EXEC.                                    GA1NPGM 
01385         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA1NPGM 
01386                         COMMAREA(DFHCOMMAREA)                     GA1NPGM 
01387                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1NPGM 
01388         END-EXEC.                                                 GA1NPGM 
01389                                                                   GA1NPGM 
01390      IF  ALTBFNCI  =  'GA1P'                                      GA1NPGM 
01391         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA1NPGM 
01392                         COMMAREA(DFHCOMMAREA)                     GA1NPGM 
01393                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1NPGM 
01394         END-EXEC.                                                 GA1NPGM 
01395                                                                   GA1NPGM 
01396  5099-EXIT.                                                       GA1NPGM 
01397      EXIT.                                                        GA1NPGM 
01398      EJECT                                                        GA1NPGM 
01399 ***************************************************************** GA1NPGM 
01400 **           X C T L   T O   M A I N   M E N U                    GA1NPGM 
01401 **                                                                GA1NPGM 
01402 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1NPGM 
01403 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1NPGM 
01404 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1NPGM 
01405 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1NPGM 
01406 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1NPGM 
01407 ** AND PROGRESS DOWN.                                             GA1NPGM 
01408 ******************************************************************GA1NPGM 
01409  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1NPGM 
01410      MOVE '6000'  TO  WS-PARA-ID.                                 GA1NPGM 
01411      MOVE '1N09'  TO  WS-ABEND-CODE.                              GA1NPGM 
01412                                                                   GA1NPGM 
01413      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1NPGM 
01414                                                                   GA1NPGM 
01415  6099-EXIT.     EXIT.                                             GA1NPGM 
01416      EJECT                                                        GA1NPGM 
01417 /***************************************************************  GA1NPGM 
01418 *                                                              *  GA1NPGM 
01419 * 9000   MOVE MESSAGE TO SCREEN                                *  GA1NPGM 
01420 *                                                              *  GA1NPGM 
01421 ****************************************************************  GA1NPGM 
01422  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA1NPGM 
01423  9000-010.                                                        GA1NPGM 
01424                                                                   GA1NPGM 
01425          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                     GA1NPGM 
01426                    TO ERRMSGO.                                    GA1NPGM 
01427                                                                   GA1NPGM 
01428  9000-900-EXIT.                                                   GA1NPGM 
01429      EXIT.                                                        GA1NPGM 
01430 /                                                                 GA1NPGM 
01431 ***************************************************************** GA1NPGM 
01432  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1NPGM 
01433                                                                   GA1NPGM 
01434      SET MAP-IDX1 TO 7.                                           GA1NPGM 
01435      SET MAP-IDX2 TO 1.                                           GA1NPGM 
01436      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1NPGM 
01437                                                                   GA1NPGM 
01438      EXEC CICS SEND   MAP('GA1NI01') MAPSET('GA1NSET') ERASE      GA1NPGM 
01439         FROM(GA1NI01O) WAIT END-EXEC.                             GA1NPGM 
01440                                                                   GA1NPGM 
01441      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1NPGM 
01442                                                                   GA1NPGM 
01443  9999-EXIT.     EXIT.                                             GA1NPGM 
