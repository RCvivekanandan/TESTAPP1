00001  IDENTIFICATION DIVISION.                                         01/12/06
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA1OPGM 
00003  PROGRAM-ID.     GA1OPGM.                                            LV004
00004  AUTHOR.         GARY D MULLINGS.                                 GA1OPGM 
00005  DATE-WRITTEN.   10/25/89.                                        GA1OPGM 
00006  DATE-COMPILED.                                                   GA1OPGM 
00007      SKIP3                                                        GA1OPGM 
00008 ******************************************************************GA1OPGM 
00009 *   GA1OPGM   ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM      GA1OPGM 
00010 *             PROCEDURE GROUP BY PROCEDURE CODE - GA1O            GA1OPGM 
00011 *                                                                 GA1OPGM 
00012 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1OPGM 
00013 *   CURRENTLY ON THE ALL LEVEL INTERNAL TABULAR RECORD - #IPGP.   GA1OPGM 
00014 *                                                                 GA1OPGM 
00015 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1OPGM 
00016 *   ALL LEVEL INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN    GA1OPGM 
00017 *   DECIDE IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN     GA1OPGM 
00018 *   ENTRY WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE    GA1OPGM 
00019 *   RECORD WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED   GA1OPGM 
00020 *   BACK INTO THE RECORD BEFORE UPDATING THE RECORD.              GA1OPGM 
00021 *                                                                 GA1OPGM 
00022 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #IPGP) GA1OPGM 
00023 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1OPGM 
00024 *   XCTL TO TRANS GA2O OR PROGRAM GA2OPGM.  THIS PROGRAM WILL     GA1OPGM 
00025 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1OPGM 
00026 *   TABLE.                                                        GA1OPGM 
00027 *                                                                 GA1OPGM 
00028 *   FUNC CODE: GA1O                                               GA1OPGM 
00029 *   MAPSET:    GA1OSETC                                           GA1OPGM 
00030 *   FILES:     GCPSWORK                                           GA1OPGM 
00031 *                                                                 GA1OPGM 
00032 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00033 *                                                                 GA1OPGM 
00034 *    TAILORING INSTRUCTIONS:                                      GA1OPGM 
00035 *                                                                 GA1OPGM 
00036 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1OPGM 
00037 *                                                                 GA1OPGM 
00038 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1O/     GA1OPGM 
00039 *              SCREEN PAGE NUMBER                 /009I/001O/     GA1OPGM 
00040 *              ADD PROGRAM FUNCTION CODE          /GCAI/GA2O/     GA1OPGM 
00041 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IPGP/    GA1OPGM 
00042 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX1/       GA1OPGM 
00043 *                                                                 GA1OPGM 
00044 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1OPGM 
00045 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1OPGM 
00046 *     OCCURANCES.                                                 GA1OPGM 
00047 *                                                                 GA1OPGM 
00048 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1OPGM 
00049 *     THAT MUST BE CHANGED.                                       GA1OPGM 
00050 *                                                                 GA1OPGM 
00051 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00052      SKIP3                                                        GA1OPGM 
00053 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA1OPGM 
00054 *****  P R O G R A M   M O D I F I C A T I O N    S T A T U S ****GA1OPGM 
00055 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA1OPGM 
00056 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA1OPGM 
00057 *                                                                 GA1OPGM 
00058 *   D185    10/25/89    GDM   CREATES BASIC SKELETON RECORD FOR   GA1OPGM 
00059 *                             THE INTERNAL TABULAR RECORD #IPGP.  GA1OPGM 
00060 *                                                                 GA1OPGM 
00061 * R1681   11/30/89   NGE   FIX INCL/EXCL IND MISSING FROM SCREEN. GA1OPGM 
00062 *                                                                 GA1OPGM 
00063 *         01/29/90   ENW   CHANGE ALL GX5 GXA.                    GA1OPGM 
00064 *                                                                 GA1OPGM 
00065 *                      ----ACCUM TABULAR MODIFICATIONS ----     * GA1OPGM 
00066 * 11154  1/03/91  NGE  1. EXPAND OCCUR LENGTH FROM 132 TO 176.  * GA1OPGM 
00067 *                      2. INCREASE MAX OCCURS FROM 29 TO 46.    * GA1OPGM 
00068 *                      3. CHANGE GCA-I-E FIELD IN COPYBOOKS     * GA1OPGM 
00069 *                         G2ALCKEC AND G2ALCKE2.                * GA1OPGM 
00070 *                      4. REPLACE DATE CONVERSION ROUTINE.      * GA1OPGM 
00071 *                      5. FIX ERROR MESSAGE TABLE.              * GA1OPGM 
00072 *                                                                 GA1OPGM 
00073 *                                                               * GA1OPGM 
00074 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD     * GA1OPGM 
00075 *                           FROM ONE POSITION TO TWO POSITIONS. * GA1OPGM 
00076 *                                                                *GA1OPGM 
00077 *14726/ 11/12/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000      *GA1OPGM 
00078 *15057                  AND THE EXPANSION OF THE GROUP SPECIFIC  *GA1OPGM 
00079 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS    *GA1OPGM 
00080 *                       MERGER.                                  *GA1OPGM 
00081 *                                                                *GA1OPGM 
00082 * 14726/  04/15/98  AB   EXPANDED THE SCREEN / MAP               *GA1OPGM 
00083 * 15057                  TO INCLUDE THE ENTIRE KEY               *GA1OPGM 
00084 *                                                                *GA1OPGM 
00085 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP       *GA1OPGM 
00086 *                        2. ADD DELADD-OPTION = 'GAS5UPD'        *GA1OPGM 
00087 *                                                                *GA1OPGM 
00088 * P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *GA1OPGM 
00089 *                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *GA1OPGM 
00090 *                                                                *GA1OPGM 
00091 *                                                                *GA1OPGM 
00092 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA1OPGM 
00093 *                                                                *GA1OPGM 
00094 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO*GA1OPGM 
00095 *                             7 BYTES. CHANGE # OF OCCURS TO     *GA1OPGM 
00096 *                             1109 ON #IPGP TABULAR.             *GA1OPGM 
00097 *                                                                *GA1OPGM 
00098 *   D365A   05/15/03    GTF   REMOVE 1 BYTE OF FILLER FROM MAP   *GA1OPGM 
00099 *                             REDEFINE IN WORKING STORAGE.       *GA1OPGM 
00100 *                                                                *GA1OPGM 
00101 *   D365A   05/19/03    GTF   ADD 82 BYTES AFTER OCCURS FOR PROP-*GA1OPGM 
00102 *                             ER SCREEN ALIGNMENT.               *GA1OPGM 
00103 *                                                                *GA1OPGM 
00104 *            01-11-06   NB    RECOMPILE FOR GCPPDIOC CHANGES     *GA1OPGM 
      *                                                                *        
      *           07-26-11    BA    RECOMPILE FOR GCPPDIOC CHANGES     *        
SI0724*                                                                *00030141
SI0724* P56703     05/08/24   SI    RECOMPILE R PEAQ COPYBOOK EXPANSION*00030150
SI0724*                             COPY ABM, ACP, ACL, ADL, AOL,      *00030160
SI0724*                             GCCDRLEN                           *00030170
00105 ***************************************************************** GA1OPGM 
00106 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1OPGM 
00107 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1OPGM 
00108 /                                                                 GA1OPGM 
00109 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1OPGM 
00110      EJECT                                                        GA1OPGM 
00111  ENVIRONMENT DIVISION.                                            GA1OPGM 
00112      EJECT                                                        GA1OPGM 
00113  DATA DIVISION.                                                   GA1OPGM 
00114  WORKING-STORAGE SECTION.                                         GA1OPGM 
00115  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1OPGM 
00116      '***GA1OPGM WS BEGINS***'.                                   GA1OPGM 
00117  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1OPGM 
00118                                                                   GA1OPGM 
00119  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1OPGM 
00120  01  COMMAREA-POINTER-AREA.                                       GA1OPGM 
00121      05  COMMAREA-PNTR-COMP                      PIC S9(08)  COMP.GA1OPGM 
00122      05  COMMAREA-PNTR  REDEFINES                                 GA1OPGM 
00123                             COMMAREA-PNTR-COMP USAGE IS POINTER.  GA1OPGM 
00124                                                                   GA1OPGM 
00125  01  INTERNAL-POINTER-AREA.                                       GA1OPGM 
00126      05  INTERNAL-TAB-PNTR-COMP                  PIC S9(08)  COMP.GA1OPGM 
00127      05  INTERNAL-TAB-PNTR       REDEFINES                        GA1OPGM 
00128                        INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.   GA1OPGM 
00129                                                                   GA1OPGM 
00130  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1OPGM 
00131                                                                   GA1OPGM 
00132 ** MAP COBOL SCREEN DSECTS **                                     GA1OPGM 
00133  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1OPGM 
00134      '***  I/O MAPAREA ***'.                                      GA1OPGM 
00135  COPY GA1OSETC.                                                   GA1OPGM 
00136      EJECT                                                        GA1OPGM 
00137 ******************************************************************GA1OPGM 
00138 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA1OPGM 
00139 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1OPGM 
00140 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1OPGM 
00141 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1OPGM 
00142 **  REDEFINES.                                                    GA1OPGM 
00143 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00144 **                                                                GA1OPGM 
00145 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1OPGM 
00146 **  FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1OPGM 
00147 **  MATCH THE MAP.                                                GA1OPGM 
00148 **                                                                GA1OPGM 
00149 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00150                                                                   GA1OPGM 
00151  01  FILLER     REDEFINES   GA1OI01I.                             GA1OPGM 
00152      05  FILLER                              PIC X(83).           GA1OPGM 
00153      05  GROUP-SPECIFIC-ID-LINE.                                  GA1OPGM 
00154          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA1OPGM 
00155          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA1OPGM 
00156          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA1OPGM 
00157          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA1OPGM 
00158          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA1OPGM 
00159          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA1OPGM 
00160          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA1OPGM 
00161          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA1OPGM 
00162          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA1OPGM 
00163          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA1OPGM 
00164          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA1OPGM 
00165          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA1OPGM 
00166          10  FILLER                          PIC X(16).           GA1OPGM 
00167      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA1OPGM 
00168          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1OPGM 
00169          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1OPGM 
00170          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1OPGM 
00171          10  CONTRACT-GROUP-NO               PIC X(9).            GA1OPGM 
00172          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1OPGM 
00173          10  CONTRACT-SECTION-NO             PIC X(5).            GA1OPGM 
00174          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1OPGM 
00175          10  CONTRACT-PKG-CODE               PIC X(3).            GA1OPGM 
00176          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1OPGM 
00177          10  CONTRACT-LOB                    PIC X.               GA1OPGM 
00178          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1OPGM 
00179          10  CONTRACT-PROV-CTL               PIC XX.              GA1OPGM 
00180          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1OPGM 
00181          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1OPGM 
00182          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1OPGM 
00183          10  CONTRACT-EFF-DATE               PIC X(6).            GA1OPGM 
00184          10  FILLER                          PIC X(1).            GA1OPGM 
00185      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA1OPGM 
00186                                     GROUP-SPECIFIC-ID-LINE.       GA1OPGM 
00187          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA1OPGM 
00188          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA1OPGM 
00189          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA1OPGM 
00190          10  BEN-PROV-GROUP-NO               PIC X(9).            GA1OPGM 
00191          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA1OPGM 
00192          10  BEN-PROV-SECTION-NO             PIC X(5).            GA1OPGM 
00193          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA1OPGM 
00194          10  BEN-PROV-PKG-CODE               PIC X(3).            GA1OPGM 
00195          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA1OPGM 
00196          10  BEN-PROV-LOB                    PIC X.               GA1OPGM 
00197          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA1OPGM 
00198          10  BEN-PROV-PROV-CTL               PIC XX.              GA1OPGM 
00199          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA1OPGM 
00200          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA1OPGM 
00201          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA1OPGM 
00202          10  BEN-PROV-EFF-DATE               PIC X(6).            GA1OPGM 
00203          10  BEN-PROV-ID-HEADING             PIC X(6).            GA1OPGM 
00204          10  BEN-PROV-ID-NO                  PIC X(6).            GA1OPGM 
00205          10  FILLER                          PIC X(4).            GA1OPGM 
00206      05  FILLER                              PIC X(74).           GA1OPGM 
00207      05  MAP-PROCEDURE-CODE-ROW         OCCURS 14 TIMES INDEXED   GA1OPGM 
00208          BY MAP-IDX1.                                             GA1OPGM 
00209        10  MAP-PROCEDURE-CODE-COL         OCCURS 3 TIMES INDEXED  GA1OPGM 
00210            BY MAP-IDX2.                                           GA1OPGM 
00211          15  MAP-ACTION-CODE-LEN             PIC S9(4) COMP SYNC. GA1OPGM 
00212          15  MAP-ACTION-CODE-ATTR            PIC X.               GA1OPGM 
00213          15  MAP-ACTION-CODE                 PIC X.               GA1OPGM 
00214          15  MAP-PROCEDURE-CODE-LEN          PIC S9(4) COMP SYNC. GA1OPGM 
00215          15  MAP-PROCEDURE-CODE-ATTR         PIC X.               GA1OPGM 
00216          15  MAP-PROCEDURE-CODE              PIC X(7).            GA1OPGM 
00217 *        15  FILLER                          PIC X.               GA1OPGM 
00218 **+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   GA1OPGM 
00219 *------ AFTER OCCURS                                              GA1OPGM 
00220      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA1OPGM 
00221      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA1OPGM 
00222      05  MAP-ERROR-MESSAGE                PIC X(79).              GA1OPGM 
00223      SKIP3                                                        GA1OPGM 
00224 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00225 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00226  01  WS-MAP-OCCURS-COUNTERS.                                      GA1OPGM 
00227 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00228 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1OPGM 
00229 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00230      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +14.     GA1OPGM 
00231      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +3.      GA1OPGM 
00232 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00233      EJECT                                                        GA1OPGM 
00234 ** ALTERNATIVE WORKFILE KEYS **                                   GA1OPGM 
00235  01  FILLER                      PIC X(32)  VALUE                 GA1OPGM 
00236      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1OPGM 
00237  01  WS-ALT-WORKFILE-KEYS.                                        GA1OPGM 
00238  COPY GCWRKKEY.                                                   GA1OPGM 
00239      EJECT                                                        GA1OPGM 
00240                                                                   GA1OPGM 
00241 ** WORKFIELDS, AND SWITCHES **                                    GA1OPGM 
00242  01  WS-WORK-FIELDS.                                              GA1OPGM 
00243      05  WS-HEX-00                     PIC X.                     GA1OPGM 
00244      05  WS-QUOTIENT                   PIC 999  COMP-3.           GA1OPGM 
00245      05  WS-REMAINDER                  PIC 999  COMP-3.           GA1OPGM 
00246      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1OPGM 
00247 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00248 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00249 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00250      05  WS-SAVED-FIELDS.                                         GA1OPGM 
00251        10  WS-SAVED-PROCEDURE-CODE                                GA1OPGM 
00252                                        PIC X(7).                  GA1OPGM 
00253 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00254  01  WS-SWITCHES.                                                 GA1OPGM 
00255      05  WS-ERROR-SW                   PIC X.                     GA1OPGM 
00256                                                                   GA1OPGM 
00257 ** TITLE LINES **                                                 GA1OPGM 
00258  01  WS-TITLE-LINES.                                              GA1OPGM 
00259      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA1OPGM 
00260          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA1OPGM 
00261      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA1OPGM 
00262          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA1OPGM 
00263      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA1OPGM 
00264          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA1OPGM 
00265                                                                   GA1OPGM 
00266      EJECT                                                        GA1OPGM 
00267 ** ATTRIBUTES **                                                  GA1OPGM 
00268  COPY DFHBMSCA.                                                   GA1OPGM 
00269      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1OPGM 
00270      EJECT                                                        GA1OPGM 
00271 ** ATTENTION IDENTIFIERS **                                       GA1OPGM 
00272  COPY DFHAID.                                                     GA1OPGM 
00273      EJECT                                                        GA1OPGM 
00274 ** RECORD LENGTHS **                                              GA1OPGM 
00275  01  WS-RECORD-LENGTHS.                                           GA1OPGM 
00276     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA1OPGM 
00277     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA1OPGM 
00278     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1OPGM 
00279     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1OPGM 
00280 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00281 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00282 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00283 ****************************************************************  GA1OPGM 
00284 /                                                                 GA1OPGM 
00285  01  WT-00-GA1OPGM-TABLES.                                        GA1OPGM 
00286      05  FILLER                   PIC X(16)  VALUE                GA1OPGM 
00287          '*GA1OPGM TABLES*'.                                      GA1OPGM 
00288                                                                   GA1OPGM 
00289  01  WT-01-TABLE.                                                 GA1OPGM 
00290      05  FILLER                  PIC X(16) VALUE                  GA1OPGM 
00291          '* WT-01-TABLE  *'.                                      GA1OPGM 
00292 ******************************************************************GA1OPGM 
00293 *    WT-01   MESSAGE TABLE                                       *GA1OPGM 
00294 ******************************************************************GA1OPGM 
00295  01  FILLER.                                                      GA1OPGM 
00296      05  WT-01-MESSAGE-VALUES.                                    GA1OPGM 
00297                                                                   GA1OPGM 
00298 *----------------------------------------------------------------*GA1OPGM 
00299          10  WT-01-ENTRY-001.                                     GA1OPGM 
00300              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00301              15  WT-01-MESSAGE-TEXT-001.                          GA1OPGM 
00302                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00303                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00304                  20  FILLER          PIC X(3)  VALUE  '001'.      GA1OPGM 
00305                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00306                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00307                           '** INVALID REQUEST. THE PF KEY USED HASGA1OPGM 
00308 -                   ' NO MEANING TO THIS PROGRAM **'.             GA1OPGM 
00309              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00310                                                                   GA1OPGM 
00311 *----------------------------------------------------------------*GA1OPGM 
00312          10  WT-01-ENTRY-002.                                     GA1OPGM 
00313              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00314              15  WT-01-MESSAGE-TEXT-002.                          GA1OPGM 
00315                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00316                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00317                  20  FILLER          PIC X(3)  VALUE  '002'.      GA1OPGM 
00318                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00319                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00320                      '** INVALID ACTION CODE FOUND **'.           GA1OPGM 
00321              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00322                                                                   GA1OPGM 
00323 *----------------------------------------------------------------*GA1OPGM 
00324          10  WT-01-ENTRY-003.                                     GA1OPGM 
00325              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00326              15  WT-01-MESSAGE-TEXT-003.                          GA1OPGM 
00327                  20  FILLER          PIC X(4)  VALUE  'GA10'.     GA1OPGM 
00328                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00329                  20  FILLER          PIC X(3)  VALUE  '003'.      GA1OPGM 
00330                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00331                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00332                           '** READ ERROR ALL LEVEL TABULAR FILE, CGA1OPGM 
00333 -                   'TACT SYSTEM AREA **           '.             GA1OPGM 
00334              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00335 *----------------------------------------------------------------*GA1OPGM 
00336          10  WT-01-ENTRY-004.                                     GA1OPGM 
00337              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00338              15  WT-01-MESSAGE-TEXT-004.                          GA1OPGM 
00339                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00340                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00341                  20  FILLER          PIC X(3)  VALUE  '004'.      GA1OPGM 
00342                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00343                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00344                           '** PROGRAM ERROR IN PARA 2040-DELETE, CGA1OPGM 
00345 -                   'ONTACT SYSTEM AREA **         '.             GA1OPGM 
00346              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00347 *----------------------------------------------------------------*GA1OPGM 
00348          10  WT-01-ENTRY-005.                                     GA1OPGM 
00349              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00350              15  WT-01-MESSAGE-TEXT-005.                          GA1OPGM 
00351                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00352                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00353                  20  FILLER          PIC X(3)  VALUE  '005'.      GA1OPGM 
00354                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00355                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00356                           '** PROGRAM ERROR IN PARA 2050-SAVE, CONGA1OPGM 
00357 -                   'TACT SYSTEM AREA **           '.             GA1OPGM 
00358              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00359 *----------------------------------------------------------------*GA1OPGM 
00360          10  WT-01-ENTRY-006.                                     GA1OPGM 
00361              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00362              15  WT-01-MESSAGE-TEXT-006.                          GA1OPGM 
00363                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00364                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00365                  20  FILLER          PIC X(3)  VALUE  '006'.      GA1OPGM 
00366                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00367                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00368                           '** REWRITE ERROR INTERNAL TABULAR FILE,GA1OPGM 
00369 -                   ' CONTACT SYSTEM AREA **       '.             GA1OPGM 
00370              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00371                                                                   GA1OPGM 
00372 *----------------------------------------------------------------*GA1OPGM 
00373          10  WT-01-ENTRY-007.                                     GA1OPGM 
00374              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00375              15  WT-01-MESSAGE-TEXT-007.                          GA1OPGM 
00376                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00377                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00378                  20  FILLER          PIC X(3)  VALUE  '007'.      GA1OPGM 
00379                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00380                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00381                           '** READ ERROR INTERNAL TABULAR FILE, COGA1OPGM 
00382 -                   'NTACT SYSTEM AREA **          '.             GA1OPGM 
00383              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00384 *----------------------------------------------------------------*GA1OPGM 
00385          10  WT-01-ENTRY-008.                                     GA1OPGM 
00386              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00387              15  WT-01-MESSAGE-TEXT-008.                          GA1OPGM 
00388                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00389                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00390                  20  FILLER          PIC X(3)  VALUE  '008'.      GA1OPGM 
00391                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00392                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00393               ' ** COMMAREA LENGTH IS INVALID **'.                GA1OPGM 
00394              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00395                                                                   GA1OPGM 
00396 *----------------------------------------------------------------*GA1OPGM 
00397          10  WT-01-ENTRY-009.                                     GA1OPGM 
00398              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00399              15  WT-01-MESSAGE-TEXT-009.                          GA1OPGM 
00400                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00401                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00402                  20  FILLER          PIC X(3)  VALUE  '009'.      GA1OPGM 
00403                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00404                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00405            '** INVALID EFFECTIVE DATE DISCOVERED **'.             GA1OPGM 
00406              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00407                                                                   GA1OPGM 
00408 *----------------------------------------------------------------*GA1OPGM 
00409          10  WT-01-ENTRY-010.                                     GA1OPGM 
00410              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00411              15  WT-01-MESSAGE-TEXT-010.                          GA1OPGM 
00412                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00413                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00414                  20  FILLER          PIC X(3)  VALUE  '010'.      GA1OPGM 
00415                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00416                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00417                  '** NO MORE ENTRIES TO DELETE **'.               GA1OPGM 
00418              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00419                                                                   GA1OPGM 
00420 *----------------------------------------------------------------*GA1OPGM 
00421          10  WT-01-ENTRY-011.                                     GA1OPGM 
00422              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1OPGM 
00423              15  WT-01-MESSAGE-TEXT-011.                          GA1OPGM 
00424                  20  FILLER          PIC X(4)  VALUE  'GA1O'.     GA1OPGM 
00425                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1OPGM 
00426                  20  FILLER          PIC X(3)  VALUE  '011'.      GA1OPGM 
00427                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1OPGM 
00428                  20  FILLER          PIC X(70) VALUE              GA1OPGM 
00429                  '** NO MORE ENTRIES TO DISPLAY **'.              GA1OPGM 
00430              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1OPGM 
00431                                                                   GA1OPGM 
00432 *----------------------------------------------------------------*GA1OPGM 
00433                                                                   GA1OPGM 
00434      05  WT-01-MESSAGE-TABLE         REDEFINES                    GA1OPGM 
00435          WT-01-MESSAGE-VALUES        OCCURS 011 TIMES             GA1OPGM 
00436                                      INDEXED BY WT-01-INDEX.      GA1OPGM 
00437          10  WT-01-ENTRY.                                         GA1OPGM 
00438              15  FILLER              PIC X(02).                   GA1OPGM 
00439              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GA1OPGM 
00440              15  FILLER              PIC X(02).                   GA1OPGM 
00441 *****                                                             GA1OPGM 
00442 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA1OPGM 
00443  01  FILLER.                                                      GA1OPGM 
00444      COPY GCCDRLEN.                                               GA1OPGM 
00445 /                                                                 GA1OPGM 
00446 ** IO PARM AREA **                                                GA1OPGM 
00447  01  GCPPDIO-PARM-AREA.                                           GA1OPGM 
00448  COPY GCPPDIOC.                                                   GA1OPGM 
00449 /                                                                 GA1OPGM 
00450  01  WS-END                      PIC X(16)  VALUE                 GA1OPGM 
00451      '*** W/S ENDS ***'.                                          GA1OPGM 
00452      EJECT                                                        GA1OPGM 
00453  LINKAGE SECTION.                                                 GA1OPGM 
00454                                                                   GA1OPGM 
00455  01  DFHCOMMAREA.                                                 GA1OPGM 
00456  COPY G2ALCKEC.                                                   GA1OPGM 
00457  COPY GACDACWA.                                                   GA1OPGM 
00458 *    05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA1OPGM 
00459      05  GAS1UPD-PASSED-AREA.                                     GA1OPGM 
00460          07  LVL2-B-SW           PIC X.                           GA1OPGM 
00461          07  LVL2-F-SW           PIC X.                           GA1OPGM 
00462          07  LVL2-G-SW           PIC X.                           GA1OPGM 
00463          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1OPGM 
00464          07  FILLER              PIC X(9).                        GA1OPGM 
00465      05  DELADD-OPTION           PIC X(7).                        GA1OPGM 
00466                                                                   GA1OPGM 
00467 *01  GCA-COMMAREA.                                                GA1OPGM 
00468 *COPY G2ALCKEC.                                                   GA1OPGM 
00469      EJECT                                                        GA1OPGM 
00470  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA1OPGM 
00471  COPY GCIOPRM1.                                                   GA1OPGM 
00472      EJECT                                                        GA1OPGM 
00473  COPY GCWRKDCC.                                                   GA1OPGM 
00474      SKIP3                                                        GA1OPGM 
00475      SKIP3                                                        GA1OPGM 
00476      SKIP3                                                        GA1OPGM 
00477  COPY GCTIPGPC.                                                   GA1OPGM 
00478      EJECT                                                        GA1OPGM 
00479 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00480 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00481 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00482  01  COPY-TABULAR-TABLE-AREA.                                     GA1OPGM 
00483      05  COPY-TABULAR-TABLE  OCCURS 1109 TIMES INDEXED BY         GA1OPGM 
00484            COPY-IDX.                                              GA1OPGM 
00485        10  COPY-PROCEDURE-CODE                PIC X(7).           GA1OPGM 
00486 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00487      EJECT                                                        GA1OPGM 
00488  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA1OPGM 
00489  COPY GCIOPRM2.                                                   GA1OPGM 
00490      EJECT                                                        GA1OPGM 
00491  COPY GCWRKDC2.                                                   GA1OPGM 
00492      EJECT                                                        GA1OPGM 
00493  COPY GCTABMC.                                                    GA1OPGM 
00494      EJECT                                                        GA1OPGM 
00495                                                                   GA1OPGM 
00496  PROCEDURE DIVISION.                                              GA1OPGM 
00497                                                                   GA1OPGM 
00498 ******************************************************************GA1OPGM 
00499 **                     M A I N L I N E                            GA1OPGM 
00500 **                                                                GA1OPGM 
00501 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1OPGM 
00502 **  TAKEN BY THE OPERATOR.                                        GA1OPGM 
00503 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1OPGM 
00504 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1OPGM 
00505 **     ADDITIONS FROM.                                            GA1OPGM 
00506 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1OPGM 
00507 **     KEY PF12 OR PF24.                                          GA1OPGM 
00508 **  3. RECEIVE THE SCREEN.                                        GA1OPGM 
00509 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1OPGM 
00510 **     MENU.                                                      GA1OPGM 
00511 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1OPGM 
00512 **     LOGIC.                                                     GA1OPGM 
00513 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1OPGM 
00514 **     (RETURN) TO THE ADD PROGRAM (GA2OPGM).                     GA1OPGM 
00515 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1OPGM 
00516 **     (RETURN) TO THE PREVIOUS MENU.                             GA1OPGM 
00517 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1OPGM 
00518 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1OPGM 
00519 **                                                                GA1OPGM 
00520 ******************************************************************GA1OPGM 
00521  1000-MAIN-LINE SECTION.                                          GA1OPGM 
00522                                                                   GA1OPGM 
00523      MOVE '1000'  TO  WS-PARA-ID.                                 GA1OPGM 
00524                                                                   GA1OPGM 
00525      IF EIBAID  =  DFHCLEAR                                       GA1OPGM 
00526          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1OPGM 
00527                         ERASE                                     GA1OPGM 
00528          END-EXEC                                                 GA1OPGM 
00529          EXEC CICS RETURN                                         GA1OPGM 
00530          END-EXEC.                                                GA1OPGM 
00531                                                                   GA1OPGM 
00532      IF EIBTRNID  NOT =  'GA1O'                                   GA1OPGM 
00533         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1OPGM 
00534         GO TO 1099-RETURN.                                        GA1OPGM 
00535                                                                   GA1OPGM 
00536      EXEC CICS RECEIVE   MAP('GA1OI01') MAPSET('GA1OSET')         GA1OPGM 
00537         INTO(GA1OI01I) END-EXEC.                                  GA1OPGM 
00538                                                                   GA1OPGM 
00539      IF SCRNIDNI  NOT =  '001O00'                                 GA1OPGM 
00540         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1OPGM 
00541                                                                   GA1OPGM 
00542      IF EIBAID  =  DFHENTER                                       GA1OPGM 
00543         PERFORM 2000-DELETE-PROCESSING                            GA1OPGM 
00544         GO TO 1099-RETURN.                                        GA1OPGM 
00545                                                                   GA1OPGM 
00546      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1OPGM 
00547         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1OPGM 
00548                                                                   GA1OPGM 
00549      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1OPGM 
00550         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1OPGM 
00551                                                                   GA1OPGM 
00552      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1OPGM 
00553      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1OPGM 
00554      SET WT-01-INDEX TO +01.                                      GA1OPGM 
00555      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1OPGM 
00556      EXEC CICS SEND   MAP('GA1OI01') MAPSET('GA1OSET') DATAONLY   GA1OPGM 
00557         FROM(GA1OI01O) CURSOR END-EXEC.                           GA1OPGM 
00558      GO TO 1099-RETURN.                                           GA1OPGM 
00559                                                                   GA1OPGM 
00560  1000-EXIT. EXIT.                                                 GA1OPGM 
00561                                                                   GA1OPGM 
00562  1099-RETURN.                                                     GA1OPGM 
00563      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA1OPGM 
00564         (DELADD-OPTION = 'GAS1UPD') OR                            GA1OPGM 
00565         (DELADD-OPTION = 'GAS2UPD') OR                            GA1OPGM 
00566         (DELADD-OPTION = 'GAS3UPD') OR                            GA1OPGM 
00567         (DELADD-OPTION = 'GAS4UPD') OR                            GA1OPGM 
00568         (DELADD-OPTION = 'GAS5UPD')                               GA1OPGM 
00569          EXEC CICS RETURN   END-EXEC                              GA1OPGM 
00570      ELSE                                                         GA1OPGM 
00571          EXEC CICS RETURN TRANSID('GA1O')                         GA1OPGM 
00572                    COMMAREA(DFHCOMMAREA)                          GA1OPGM 
00573                    LENGTH  (EIBCALEN)                             GA1OPGM 
00574                    END-EXEC.                                      GA1OPGM 
00575                                                                   GA1OPGM 
00576      GOBACK.                                                      GA1OPGM 
00577      EJECT                                                        GA1OPGM 
00578  1099-EXIT. EXIT.                                                 GA1OPGM 
00579 ******************************************************************GA1OPGM 
00580 **              D E L E T E   P R O C E S S I N G                 GA1OPGM 
00581 **                                                                GA1OPGM 
00582 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1OPGM 
00583 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1OPGM 
00584 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1OPGM 
00585 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1OPGM 
00586 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1OPGM 
00587 **    BACK INTO THE RECORD THAT WE READ.)                         GA1OPGM 
00588 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1OPGM 
00589 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1OPGM 
00590 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1OPGM 
00591 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1OPGM 
00592 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1OPGM 
00593 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1OPGM 
00594 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1OPGM 
00595 **    ENTRY.                                                      GA1OPGM 
00596 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1OPGM 
00597 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1OPGM 
00598 **    RECORD.                                                     GA1OPGM 
00599 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1OPGM 
00600 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1OPGM 
00601 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1OPGM 
00602 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1OPGM 
00603 **    ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1OPGM 
00604 **    SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1OPGM 
00605 **    BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1OPGM 
00606 **                                                                GA1OPGM 
00607 ******************************************************************GA1OPGM 
00608  2000-DELETE-PROCESSING SECTION.                                  GA1OPGM 
00609                                                                   GA1OPGM 
00610      MOVE '2000'  TO  WS-PARA-ID.                                 GA1OPGM 
00611      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1OPGM 
00612      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1OPGM 
00613      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1OPGM 
00614                                                                   GA1OPGM 
00615      MOVE '2010'  TO  WS-PARA-ID.                                 GA1OPGM 
00616  2010-VALIDATE-ACT-CODE.                                          GA1OPGM 
00617      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1OPGM 
00618         ADD 1  TO  WS-DELETE-COUNT.                               GA1OPGM 
00619      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D' OR           GA1OPGM 
00620         = SPACE OR =  LOW-VALUES                                  GA1OPGM 
00621         MOVE DFHBMUNF  TO                                         GA1OPGM 
00622            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1OPGM 
00623 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00624 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00625 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00626         MOVE DFHBMASF  TO                                         GA1OPGM 
00627            MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2)           GA1OPGM 
00628      ELSE                                                         GA1OPGM 
00629         MOVE DFHBMUBF  TO                                         GA1OPGM 
00630            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1OPGM 
00631         MOVE DFHBMABF  TO                                         GA1OPGM 
00632            MAP-PROCEDURE-CODE-ATTR (MAP-IDX1, MAP-IDX2)           GA1OPGM 
00633 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00634         IF WS-ERROR-SW  NOT =  'Y'                                GA1OPGM 
00635            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1OPGM 
00636            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1OPGM 
00637                                                                   GA1OPGM 
00638      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1OPGM 
00639         SET MAP-IDX1   UP BY  1                                   GA1OPGM 
00640      ELSE                                                         GA1OPGM 
00641         IF MAP-IDX2  <  WS-MAP-COL                                GA1OPGM 
00642            SET MAP-IDX1  TO  1                                    GA1OPGM 
00643            SET MAP-IDX2  UP BY  1                                 GA1OPGM 
00644         ELSE                                                      GA1OPGM 
00645            GO TO 2020-DONE-VALIDATE-A-C.                          GA1OPGM 
00646 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00647 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00648 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00649      IF MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2)                   GA1OPGM 
00650         NOT =  LOW-VALUES                                         GA1OPGM 
00651 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00652         GO TO 2010-VALIDATE-ACT-CODE.                             GA1OPGM 
00653                                                                   GA1OPGM 
00654  2020-DONE-VALIDATE-A-C.                                          GA1OPGM 
00655      MOVE '2020'  TO  WS-PARA-ID.                                 GA1OPGM 
00656      SET MAP-IDX1   TO  1.                                        GA1OPGM 
00657                                                                   GA1OPGM 
00658      IF WS-ERROR-SW  =  'Y'                                       GA1OPGM 
00659         SET WT-01-INDEX TO +02                                    GA1OPGM 
00660         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1OPGM 
00661         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1OPGM 
00662            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA1OPGM 
00663            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA1OPGM 
00664 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00665 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1OPGM 
00666 **  ADD ITS MAP FIELD NAME HERE.                                  GA1OPGM 
00667 ****************************************************************  GA1OPGM 
00668            INCEXCO                                                GA1OPGM 
00669         MOVE '2100'  TO  WS-PARA-ID                               GA1OPGM 
00670         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1OPGM 
00671            VARYING MAP-IDX2 FROM  1  BY  1                        GA1OPGM 
00672                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1OPGM 
00673              AFTER MAP-IDX1 FROM  1  BY  1                        GA1OPGM 
00674                             UNTIL MAP-IDX1  >  WS-MAP-ROW         GA1OPGM 
00675         EXEC CICS SEND   MAP('GA1OI01') MAPSET('GA1OSET') DATAONLYGA1OPGM 
00676            FROM(GA1OI01O) CURSOR END-EXEC                         GA1OPGM 
00677         GO TO 2099-EXIT.                                          GA1OPGM 
00678                                                                   GA1OPGM 
00679      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1OPGM 
00680            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA1OPGM 
00681                    GC-GCTABULR-IPGP-FIXED-LEN +                   GA1OPGM 
00682      (GC-GCTABULR-IPGP-VARY-MAX-OCUR * GC-GCTABULR-IPGP-VARY-LEN).GA1OPGM 
00683                                                                   GA1OPGM 
00684      EXEC CICS                                                    GA1OPGM 
00685         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA1OPGM 
00686         INITIMG(WS-HEX-00)                                        GA1OPGM 
00687         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA1OPGM 
00688      END-EXEC.                                                    GA1OPGM 
00689                                                                   GA1OPGM 
00690      IF  FRMNUIDI  =  'GS3A'                                      GA1OPGM 
00691         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1OPGM 
00692         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1OPGM 
00693         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA1OPGM 
00694         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1OPGM 
00695 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1OPGM 
00696         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1OPGM 
00697 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1OPGM 
00698         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1OPGM 
00699         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1OPGM 
00700         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1OPGM 
00701                          GCIO-WRK-PROVIDER-CONTROL                GA1OPGM 
00702         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1OPGM 
00703         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1OPGM 
00704                                                                   GA1OPGM 
00705      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1OPGM 
00706         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1OPGM 
00707         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1OPGM 
00708         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1OPGM 
00709         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1OPGM 
00710 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1OPGM 
00711         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1OPGM 
00712 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1OPGM 
00713         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1OPGM 
00714         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1OPGM 
00715         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1OPGM 
00716         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1OPGM 
00717         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1OPGM 
00718         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1OPGM 
00719                                                                   GA1OPGM 
00720      IF  FRMNUIDI  =  'GC8A'                                      GA1OPGM 
00721         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1OPGM 
00722         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1OPGM 
00723         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA1OPGM 
00724         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1OPGM 
00725 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1OPGM 
00726         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1OPGM 
00727 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1OPGM 
00728         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1OPGM 
00729         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1OPGM 
00730         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1OPGM 
00731         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1OPGM 
00732         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1OPGM 
00733         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1OPGM 
00734                                                                   GA1OPGM 
00735      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA1OPGM 
00736      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1OPGM 
00737      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA1OPGM 
00738      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA1OPGM 
00739      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA1OPGM 
00740      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA1OPGM 
00741      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1OPGM 
00742                                                                   GA1OPGM 
00743      IF WS-DELETE-COUNT  =  ZERO                                  GA1OPGM 
00744         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1OPGM 
00745                                                                   GA1OPGM 
00746 ******************************************************************GA1OPGM 
00747 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1OPGM 
00748 *                                                                 GA1OPGM 
00749 ******************************************************************GA1OPGM 
00750                                                                   GA1OPGM 
00751      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GA1OPGM 
00752        TO GXA-ENTRY-COUNT.                                        GA1OPGM 
00753                                                                   GA1OPGM 
00754      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1OPGM 
00755                                                                   GA1OPGM 
00756      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1OPGM 
00757         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1OPGM 
00758         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1OPGM 
00759                                                                   GA1OPGM 
00760      IF  NOT GCIO-GOOD-RETURN                                     GA1OPGM 
00761         SET WT-01-INDEX TO +03                                    GA1OPGM 
00762         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1OPGM 
00763         MOVE '1O01'  TO  WS-ABEND-CODE                            GA1OPGM 
00764         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1OPGM 
00765                                                                   GA1OPGM 
00766      COMPUTE WS-COPY-LENGTH  =                                    GA1OPGM 
00767              GXA-ENTRY-COUNT  *  GC-GCTABULR-IPGP-VARY-LEN.       GA1OPGM 
00768                                                                   GA1OPGM 
00769      EXEC CICS                                                    GA1OPGM 
00770         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA1OPGM 
00771         LENGTH      (WS-COPY-LENGTH)                              GA1OPGM 
00772         INITIMG     (WS-HEX-00)                                   GA1OPGM 
00773      END-EXEC.                                                    GA1OPGM 
00774                                                                   GA1OPGM 
00775      MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   GA1OPGM 
00776      SET COPY-IDX,  GXA-INDEX  TO  1.                             GA1OPGM 
00777                                                                   GA1OPGM 
00778      MOVE '2030'  TO  WS-PARA-ID.                                 GA1OPGM 
00779  2030-MAKE-A-COPY-OF-RECORD.                                      GA1OPGM 
00780      IF GXA-INDEX  NOT >  GXA-ENTRY-COUNT                         GA1OPGM 
00781         MOVE GXA-ENTRY (GXA-INDEX)  TO                            GA1OPGM 
00782            COPY-TABULAR-TABLE (COPY-IDX)                          GA1OPGM 
00783            SET COPY-IDX,  GXA-INDEX  UP BY  1                     GA1OPGM 
00784            GO TO 2030-MAKE-A-COPY-OF-RECORD.                      GA1OPGM 
00785      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GXA-INDEX  TO  1.         GA1OPGM 
00786                                                                   GA1OPGM 
00787      MOVE '2040'  TO  WS-PARA-ID.                                 GA1OPGM 
00788  2040-DELETE-MARKED-ENTRIES.                                      GA1OPGM 
00789 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00790 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00791 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00792      IF MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2)         =         GA1OPGM 
00793            LOW-VALUES                                             GA1OPGM 
00794         GO TO 2060-SAVE-REST-OF-COPY.                             GA1OPGM 
00795                                                                   GA1OPGM 
00796      IF MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2)         >         GA1OPGM 
00797         COPY-PROCEDURE-CODE (COPY-IDX)                            GA1OPGM 
00798         GO TO 2050-SAVE-COPIED-ENTRY                              GA1OPGM 
00799      ELSE                                                         GA1OPGM 
00800         IF MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2)         <      GA1OPGM 
00801            COPY-PROCEDURE-CODE (COPY-IDX)                         GA1OPGM 
00802            MOVE '1O02'  TO  WS-ABEND-CODE                         GA1OPGM 
00803            SET WT-01-INDEX TO +04                                 GA1OPGM 
00804            PERFORM 9000-000-MOVE-MSG-TO-SCREEN                    GA1OPGM 
00805            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1OPGM 
00806                                                                   GA1OPGM 
00807 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00808                                                                   GA1OPGM 
00809      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1OPGM 
00810         IF MAP-IDX1   <  WS-MAP-ROW                               GA1OPGM 
00811            SET MAP-IDX1   UP BY  1                                GA1OPGM 
00812            GO TO 2050-SAVE-COPIED-ENTRY                           GA1OPGM 
00813         ELSE                                                      GA1OPGM 
00814            IF MAP-IDX2  <  WS-MAP-COL                             GA1OPGM 
00815               SET MAP-IDX1  TO  1                                 GA1OPGM 
00816               SET MAP-IDX2  UP BY  1                              GA1OPGM 
00817               GO TO 2050-SAVE-COPIED-ENTRY                        GA1OPGM 
00818            ELSE                                                   GA1OPGM 
00819               GO TO 2060-SAVE-REST-OF-COPY.                       GA1OPGM 
00820                                                                   GA1OPGM 
00821      SET COPY-IDX  UP BY  1.                                      GA1OPGM 
00822      IF COPY-IDX  NOT <  GXA-ENTRY-COUNT                          GA1OPGM 
00823         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    GA1OPGM 
00824            GXA-ENTRY (GXA-INDEX)                                  GA1OPGM 
00825         SET  GXA-ENTRY-COUNT  TO  GXA-INDEX                       GA1OPGM 
00826         MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT                 GA1OPGM 
00827         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1OPGM 
00828      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1OPGM 
00829         SET MAP-IDX1   UP BY  1                                   GA1OPGM 
00830         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1OPGM 
00831      IF MAP-IDX2  <  WS-MAP-COL                                   GA1OPGM 
00832         SET MAP-IDX1  TO  1                                       GA1OPGM 
00833         SET MAP-IDX2  UP BY  1                                    GA1OPGM 
00834         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1OPGM 
00835      ELSE                                                         GA1OPGM 
00836         GO TO 2060-SAVE-REST-OF-COPY.                             GA1OPGM 
00837                                                                   GA1OPGM 
00838  2050-SAVE-COPIED-ENTRY.                                          GA1OPGM 
00839      MOVE '2050'  TO  WS-PARA-ID.                                 GA1OPGM 
00840      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1OPGM 
00841         GXA-ENTRY (GXA-INDEX).                                    GA1OPGM 
00842                                                                   GA1OPGM 
00843      SET GXA-INDEX  UP BY  1.                                     GA1OPGM 
00844      IF COPY-IDX  <  GXA-ENTRY-COUNT                              GA1OPGM 
00845         SET COPY-IDX  UP BY  1                                    GA1OPGM 
00846         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1OPGM 
00847      ELSE                                                         GA1OPGM 
00848 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1OPGM 
00849 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1OPGM 
00850 ***      THE TABLE OF ENTRIES.                                    GA1OPGM 
00851         MOVE '1O03'  TO  WS-ABEND-CODE                            GA1OPGM 
00852         SET WT-01-INDEX TO +05                                    GA1OPGM 
00853         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1OPGM 
00854         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1OPGM 
00855                                                                   GA1OPGM 
00856  2060-SAVE-REST-OF-COPY.                                          GA1OPGM 
00857      MOVE '2060'  TO  WS-PARA-ID.                                 GA1OPGM 
00858      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1OPGM 
00859         GXA-ENTRY (GXA-INDEX).                                    GA1OPGM 
00860                                                                   GA1OPGM 
00861      SET GXA-INDEX  UP BY  1.                                     GA1OPGM 
00862      IF COPY-IDX  <  GXA-ENTRY-COUNT                              GA1OPGM 
00863         SET COPY-IDX  UP BY  1                                    GA1OPGM 
00864         GO TO 2060-SAVE-REST-OF-COPY.                             GA1OPGM 
00865                                                                   GA1OPGM 
00866      SET GXA-INDEX  DOWN BY  1.                                   GA1OPGM 
00867      SET GXA-ENTRY-COUNT  TO  GXA-INDEX.                          GA1OPGM 
00868      MOVE GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                   GA1OPGM 
00869                                                                   GA1OPGM 
00870  2070-UPDATE-MODIFIED-REC.                                        GA1OPGM 
00871      MOVE '2070'  TO  WS-PARA-ID.                                 GA1OPGM 
00872                                                                   GA1OPGM 
00873 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1OPGM 
00874                                                                   GA1OPGM 
00875      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1OPGM 
00876      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1OPGM 
00877                                                                   GA1OPGM 
00878      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1OPGM 
00879         GC-WORKFILE-KEY-LEN +  GC-GCTABULR-IPGP-FIXED-LEN +       GA1OPGM 
00880             (GXA-ENTRY-COUNT  *  GC-GCTABULR-IPGP-VARY-LEN).      GA1OPGM 
00881                                                                   GA1OPGM 
00882      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1OPGM 
00883         GC-GCIOPARM-LEN + GCIO-RECORD-LENGTH.                     GA1OPGM 
00884                                                                   GA1OPGM 
00885      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1OPGM 
00886         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1OPGM 
00887         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1OPGM 
00888                                                                   GA1OPGM 
00889      IF GCIO-GOOD-RETURN                                          GA1OPGM 
00890         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1OPGM 
00891      SET WT-01-INDEX TO +06.                                      GA1OPGM 
00892      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1OPGM 
00893      MOVE '1O04'  TO  WS-ABEND-CODE.                              GA1OPGM 
00894      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1OPGM 
00895                                                                   GA1OPGM 
00896  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1OPGM 
00897      MOVE  '2080'  TO  WS-PARA-ID.                                GA1OPGM 
00898                                                                   GA1OPGM 
00899      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GA1OPGM 
00900        TO GXA-ENTRY-COUNT.                                        GA1OPGM 
00901                                                                   GA1OPGM 
00902      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1OPGM 
00903                                                                   GA1OPGM 
00904      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1OPGM 
00905         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1OPGM 
00906         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1OPGM 
00907                                                                   GA1OPGM 
00908      IF GCIO-GOOD-RETURN                                          GA1OPGM 
00909         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1OPGM 
00910      MOVE '1O05'  TO  WS-ABEND-CODE.                              GA1OPGM 
00911      SET WT-01-INDEX TO +07.                                      GA1OPGM 
00912      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1OPGM 
00913      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1OPGM 
00914                                                                   GA1OPGM 
00915  2090-BUILD-NEXT-DISPLAY.                                         GA1OPGM 
00916      MOVE  '2090'  TO  WS-PARA-ID.                                GA1OPGM 
00917      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1OPGM 
00918      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1OPGM 
00919      SET GXA-INDEX  TO  1.                                        GA1OPGM 
00920 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00921 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00922 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00923      IF MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2)         =         GA1OPGM 
00924            LOW-VALUES                                             GA1OPGM 
00925         MOVE GXA-ENTRY (GXA-INDEX)  TO  WS-SAVED-FIELDS           GA1OPGM 
00926      ELSE                                                         GA1OPGM 
00927         MOVE MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2) TO           GA1OPGM 
00928            WS-SAVED-PROCEDURE-CODE.                               GA1OPGM 
00929 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00930                                                                   GA1OPGM 
00931      PERFORM 4500-FILL-THE-SCREEN.                                GA1OPGM 
00932      EXEC CICS SEND   MAP('GA1OI01') MAPSET('GA1OSET') ERASE      GA1OPGM 
00933         FROM(GA1OI01O) END-EXEC.                                  GA1OPGM 
00934                                                                   GA1OPGM 
00935  2099-EXIT.   EXIT.                                               GA1OPGM 
00936      EJECT                                                        GA1OPGM 
00937  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1OPGM 
00938 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00939 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
00940 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00941      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1OPGM 
00942         MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2).                  GA1OPGM 
00943 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
00944                                                                   GA1OPGM 
00945  2199-EXIT.   EXIT.                                               GA1OPGM 
00946      EJECT                                                        GA1OPGM 
00947 ******************************************************************GA1OPGM 
00948 **          X C T L   T O   A D D   S C R E E N                   GA1OPGM 
00949 **                                                                GA1OPGM 
00950 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1OPGM 
00951 ** ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1OPGM 
00952 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1OPGM 
00953 ** TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1OPGM 
00954 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1OPGM 
00955 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1OPGM 
00956 ******************************************************************GA1OPGM 
00957  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1OPGM 
00958      MOVE '3000'  TO  WS-PARA-ID.                                 GA1OPGM 
00959                                                                   GA1OPGM 
00960 *    EXEC CICS                                                    GA1OPGM 
00961 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1OPGM 
00962 *       INITIMG(WS-HEX-00)                                        GA1OPGM 
00963 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1OPGM 
00964 *    END-EXEC.                                                    GA1OPGM 
00965                                                                   GA1OPGM 
00966 *    IF  FRMNUIDI  =  'GS3A'                                      GA1OPGM 
00967 **      MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA1OPGM 
00968 *       MOVE  GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                   GA1OPGM 
00969 *       MOVE  GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO               GA1OPGM 
00970 *       MOVE  GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1OPGM 
00971 *       MOVE  GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1OPGM 
00972 *       MOVE  SPACES  TO  GCA-L-O-B,                              GA1OPGM 
00973 *                         GCA-PROV-CTL,                           GA1OPGM 
00974 *                         GCA-BEN-PROV-ID.                        GA1OPGM 
00975                                                                   GA1OPGM 
00976 *    IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1OPGM 
00977 **      MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA1OPGM 
00978 *       MOVE  CONTRACT-GROUP-NO  TO  GCA-GRP-NO                   GA1OPGM 
00979 *       MOVE  CONTRACT-SECTION-NO  TO  GCA-SECTN-NO               GA1OPGM 
00980 *       MOVE  CONTRACT-LOB  TO  GCA-L-O-B                         GA1OPGM 
00981 *       MOVE  CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                 GA1OPGM 
00982 *       MOVE  CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1OPGM 
00983 *       MOVE  CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1OPGM 
00984 *       MOVE  SPACES  TO  GCA-BEN-PROV-ID.                        GA1OPGM 
00985                                                                   GA1OPGM 
00986 *    IF  FRMNUIDI  =  'GC8A'                                      GA1OPGM 
00987 **      MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA1OPGM 
00988 *       MOVE  BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                   GA1OPGM 
00989 *       MOVE  BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO               GA1OPGM 
00990 *       MOVE  BEN-PROV-LOB  TO  GCA-L-O-B                         GA1OPGM 
00991 *       MOVE  BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                 GA1OPGM 
00992 *       MOVE  BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1OPGM 
00993 *       MOVE  BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1OPGM 
00994 *       MOVE  BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                GA1OPGM 
00995                                                                   GA1OPGM 
00996      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1OPGM 
00997      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1OPGM 
00998      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1OPGM 
00999      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1OPGM 
01000      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1OPGM 
01001      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1OPGM 
01002      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1OPGM 
01003      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1OPGM 
01004 *    MOVE  ZEROES  TO  GCA-EFF-DT.                                GA1OPGM 
01005 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01006 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA1OPGM 
01007 ** FIELDS TO DISPLAY ON THE INITIAL ADD SCREEN THEY SHOULD BE     GA1OPGM 
01008 ** PASSED HERE.                                                   GA1OPGM 
01009 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01010      MOVE INCEXCI TO GCA-I-E-INDC.                                GA1OPGM 
01011                                                                   GA1OPGM 
01012 *    SET COMMAREA-PNTR                                            GA1OPGM 
01013 *      TO ADDRESS OF GCA-COMMAREA.                                GA1OPGM 
01014                                                                   GA1OPGM 
01015 *    EXEC CICS XCTL  PROGRAM('GA2OPGM') COMMAREA(COMMAREA-PNTR)   GA1OPGM 
01016 *       LENGTH(4) END-EXEC.                                       GA1OPGM 
01017      EXEC CICS XCTL PROGRAM('GA2OPGM')                            GA1OPGM 
01018                     COMMAREA(DFHCOMMAREA)                         GA1OPGM 
01019                     LENGTH(LENGTH OF DFHCOMMAREA)                 GA1OPGM 
01020      END-EXEC.                                                    GA1OPGM 
01021                                                                   GA1OPGM 
01022  3099-EXIT.   EXIT.                                               GA1OPGM 
01023      EJECT                                                        GA1OPGM 
01024 ***************************************************************** GA1OPGM 
01025 **          D I S P L A Y   F I R S T   S C R E E N               GA1OPGM 
01026 **                                                                GA1OPGM 
01027 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1OPGM 
01028 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1OPGM 
01029 ** ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1OPGM 
01030 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1OPGM 
01031 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1OPGM 
01032 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1OPGM 
01033 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1OPGM 
01034 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1OPGM 
01035 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1OPGM 
01036 ** DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1OPGM 
01037 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1OPGM 
01038 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1OPGM 
01039 ******************************************************************GA1OPGM 
01040  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1OPGM 
01041      MOVE '4000'  TO  WS-PARA-ID.                                 GA1OPGM 
01042                                                                   GA1OPGM 
01043 ***  D184     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY         GA1OPGM 
01044 *                                                                 GA1OPGM 
01045      MOVE LOW-VALUES TO GA1OI01I.                                 GA1OPGM 
01046                                                                   GA1OPGM 
01047      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1OPGM 
01048         SET WT-01-INDEX TO +08                                    GA1OPGM 
01049         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1OPGM 
01050         MOVE '1O06'  TO  WS-ABEND-CODE                            GA1OPGM 
01051         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1OPGM 
01052                                                                   GA1OPGM 
01053 *    SET ADDRESS OF GCA-COMMAREA                                  GA1OPGM 
01054 *      TO INCOMING-COMMAREA-PNTR.                                 GA1OPGM 
01055                                                                   GA1OPGM 
01056      SET ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD                   GA1OPGM 
01057        TO GCA-RECORD-POINTER.                                     GA1OPGM 
01058                                                                   GA1OPGM 
01059      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA1OPGM 
01060      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA1OPGM 
01061      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA1OPGM 
01062      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA1OPGM 
01063      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA1OPGM 
01064      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA1OPGM 
01065      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA1OPGM 
01066      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA1OPGM 
01067 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01068 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1OPGM 
01069 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1OPGM 
01070 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01071      MOVE GXA-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               GA1OPGM 
01072      MOVE GCA-I-E-INDC TO INCEXCO.                                GA1OPGM 
01073                                                                   GA1OPGM 
01074      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1OPGM 
01075         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA1OPGM 
01076 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA1OPGM 
01077         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA1OPGM 
01078         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA1OPGM 
01079         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA1OPGM 
01080         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA1OPGM 
01081         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1OPGM 
01082         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA1OPGM 
01083         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA1OPGM 
01084         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA1OPGM 
01085         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1OPGM 
01086         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1OPGM 
01087         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1OPGM 
01088         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1OPGM 
01089                                                                   GA1OPGM 
01090      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1OPGM 
01091         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA1OPGM 
01092 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1OPGM 
01093         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA1OPGM 
01094         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA1OPGM 
01095         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA1OPGM 
01096         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA1OPGM 
01097         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1OPGM 
01098         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA1OPGM 
01099         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA1OPGM 
01100         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA1OPGM 
01101         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1OPGM 
01102         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1OPGM 
01103         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1OPGM 
01104         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1OPGM 
01105         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1OPGM 
01106         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1OPGM 
01107         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1OPGM 
01108         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1OPGM 
01109                                                                   GA1OPGM 
01110      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1OPGM 
01111         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA1OPGM 
01112         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA1OPGM 
01113         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA1OPGM 
01114         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA1OPGM 
01115         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA1OPGM 
01116         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA1OPGM 
01117         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA1OPGM 
01118         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA1OPGM 
01119         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA1OPGM 
01120         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA1OPGM 
01121         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1OPGM 
01122         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA1OPGM 
01123         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1OPGM 
01124         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA1OPGM 
01125         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1OPGM 
01126         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA1OPGM 
01127         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1OPGM 
01128         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA1OPGM 
01129         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1OPGM 
01130                                                                   GA1OPGM 
01131      SET GXA-INDEX  TO  1.                                        GA1OPGM 
01132      MOVE GXA-ENTRY (GXA-INDEX)  TO  WS-SAVED-FIELDS.             GA1OPGM 
01133                                                                   GA1OPGM 
01134      PERFORM 4500-FILL-THE-SCREEN.                                GA1OPGM 
01135      EXEC CICS SEND   MAP('GA1OI01') MAPSET('GA1OSET') ERASE      GA1OPGM 
01136         FROM(GA1OI01O) END-EXEC.                                  GA1OPGM 
01137                                                                   GA1OPGM 
01138  4099-EXIT.   EXIT.                                               GA1OPGM 
01139      EJECT                                                        GA1OPGM 
01140 ***************************************************************** GA1OPGM 
01141 **             F I L L   T H E   S C R E E N                      GA1OPGM 
01142 **                                                                GA1OPGM 
01143 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1OPGM 
01144 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1OPGM 
01145 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1OPGM 
01146 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1OPGM 
01147 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1OPGM 
01148 ******************************************************************GA1OPGM 
01149  4500-FILL-THE-SCREEN SECTION.                                    GA1OPGM 
01150                                                                   GA1OPGM 
01151      MOVE '4500'  TO  WS-PARA-ID.                                 GA1OPGM 
01152      MOVE  GXA-ENTRY-COUNT  TO  GXA-ENTRY-COUNT.                  GA1OPGM 
01153      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1OPGM 
01154                                                                   GA1OPGM 
01155      IF GXA-ENTRY-COUNT  NOT >  1                                 GA1OPGM 
01156         MOVE '4530'  TO  WS-PARA-ID                               GA1OPGM 
01157         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1OPGM 
01158                                                                   GA1OPGM 
01159      SET GXA-INDEX  TO  1.                                        GA1OPGM 
01160      MOVE '4510'  TO  WS-PARA-ID.                                 GA1OPGM 
01161  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1OPGM 
01162 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01163 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
01164 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01165      IF GXA-PROCEDURE-ARGUMENT (GXA-INDEX)         <              GA1OPGM 
01166            WS-SAVED-PROCEDURE-CODE                                GA1OPGM 
01167 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01168         SET GXA-INDEX  UP BY  1                                   GA1OPGM 
01169         IF  GXA-INDEX  <  GXA-ENTRY-COUNT                         GA1OPGM 
01170            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1OPGM 
01171         ELSE                                                      GA1OPGM 
01172            SET GXA-INDEX  TO  1.                                  GA1OPGM 
01173                                                                   GA1OPGM 
01174      MOVE '4520'  TO  WS-PARA-ID.                                 GA1OPGM 
01175  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1OPGM 
01176      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1OPGM 
01177      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1OPGM 
01178 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01179 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
01180 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01181      MOVE GXA-PROCEDURE-ARGUMENT (GXA-INDEX)    TO                GA1OPGM 
01182         MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2).                  GA1OPGM 
01183 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01184                                                                   GA1OPGM 
01185      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1OPGM 
01186         SET  MAP-IDX1  UP BY  1                                   GA1OPGM 
01187      ELSE                                                         GA1OPGM 
01188         IF MAP-IDX2  <  WS-MAP-COL                                GA1OPGM 
01189            SET  MAP-IDX1  TO  1                                   GA1OPGM 
01190            SET  MAP-IDX2  UP BY  1                                GA1OPGM 
01191         ELSE                                                      GA1OPGM 
01192            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1OPGM 
01193                                                                   GA1OPGM 
01194      IF GXA-INDEX  <  (GXA-ENTRY-COUNT - 1 )                      GA1OPGM 
01195         SET  GXA-INDEX  UP BY  1                                  GA1OPGM 
01196         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1OPGM 
01197                                                                   GA1OPGM 
01198      MOVE '4530'  TO  WS-PARA-ID.                                 GA1OPGM 
01199  4530-FILL-REST-WITH-NULLS.                                       GA1OPGM 
01200      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1OPGM 
01201 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01202 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1OPGM 
01203 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01204      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1OPGM 
01205         MAP-PROCEDURE-CODE (MAP-IDX1, MAP-IDX2).                  GA1OPGM 
01206 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1OPGM 
01207      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1OPGM 
01208         SET  MAP-IDX1   UP BY  1                                  GA1OPGM 
01209         GO TO 4530-FILL-REST-WITH-NULLS                           GA1OPGM 
01210      ELSE                                                         GA1OPGM 
01211         IF MAP-IDX2  <  WS-MAP-COL                                GA1OPGM 
01212            SET  MAP-IDX1  TO  1                                   GA1OPGM 
01213            SET  MAP-IDX2  UP BY 1                                 GA1OPGM 
01214            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1OPGM 
01215                                                                   GA1OPGM 
01216      MOVE '4540'  TO  WS-PARA-ID.                                 GA1OPGM 
01217  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1OPGM 
01218      IF GXA-ENTRY-COUNT  =  1                                     GA1OPGM 
01219         SET WT-01-INDEX TO +10                                    GA1OPGM 
01220         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1OPGM 
01221         GO TO 4599-EXIT.                                          GA1OPGM 
01222                                                                   GA1OPGM 
01223      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1OPGM 
01224         SET WT-01-INDEX TO +11                                    GA1OPGM 
01225         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA1OPGM 
01226                                                                   GA1OPGM 
01227  4599-EXIT.     EXIT.                                             GA1OPGM 
01228      EJECT                                                        GA1OPGM 
01229 ***************************************************************** GA1OPGM 
01230 **        X C T L   T O   P R E V I O U S   M E N U               GA1OPGM 
01231 **                                                                GA1OPGM 
01232 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1OPGM 
01233 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1OPGM 
01234 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1OPGM 
01235 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1OPGM 
01236 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1OPGM 
01237 ******************************************************************GA1OPGM 
01238  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1OPGM 
01239      MOVE '5000'  TO  WS-PARA-ID.                                 GA1OPGM 
01240                                                                   GA1OPGM 
01241                                                                   GA1OPGM 
01242 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA1OPGM 
01243 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA1OPGM 
01244 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA1OPGM 
01245                                                                   GA1OPGM 
01246      IF  ALTBFNCI  =  'GTM1'                                      GA1OPGM 
01247          EXEC CICS XCTL                                           GA1OPGM 
01248                    PROGRAM('GTM1PGM')                             GA1OPGM 
01249                    END-EXEC.                                      GA1OPGM 
01250                                                                   GA1OPGM 
01251                                                                   GA1OPGM 
01252      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA1OPGM 
01253          GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +                 GA1OPGM 
01254                 GC-GCTABULR-ABM-FIXED-LEN +                       GA1OPGM 
01255         (GC-GCTABULR-ABM-VARY-MAX-OCUR *                          GA1OPGM 
01256                 GC-GCTABULR-ABM-VARY-LEN).                        GA1OPGM 
01257                                                                   GA1OPGM 
01258      EXEC CICS                                                    GA1OPGM 
01259         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA1OPGM 
01260         INITIMG(WS-HEX-00)                                        GA1OPGM 
01261         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA1OPGM 
01262      END-EXEC.                                                    GA1OPGM 
01263                                                                   GA1OPGM 
01264 *    EXEC CICS                                                    GA1OPGM 
01265 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1OPGM 
01266 *       INITIMG(WS-HEX-00)                                        GA1OPGM 
01267 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1OPGM 
01268 *    END-EXEC.                                                    GA1OPGM 
01269                                                                   GA1OPGM 
01270      IF  FRMNUIDI  =  'GS3A'                                      GA1OPGM 
01271         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1OPGM 
01272         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1OPGM 
01273         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA1OPGM 
01274         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1OPGM 
01275         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1OPGM 
01276         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1OPGM 
01277         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1OPGM 
01278         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1OPGM 
01279                          GCIO-WRK-PROVIDER-CONTROL                GA1OPGM 
01280         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1OPGM 
01281         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1OPGM 
01282         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1OPGM 
01283         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1OPGM 
01284                            GCA-ALL-LEVEL-TAB-ID                   GA1OPGM 
01285         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1OPGM 
01286                            GCA-ALL-LEVEL-TAB-SLOT                 GA1OPGM 
01287         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1OPGM 
01288                            GCA-INTERNAL-TAB-ID,                   GA1OPGM 
01289                            GCA-INTERNAL-TAB-SLOT                  GA1OPGM 
01290         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1OPGM 
01291                                                                   GA1OPGM 
01292      IF  FRMNUIDI  =  'GC4A'                                      GA1OPGM 
01293         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1OPGM 
01294         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1OPGM 
01295         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1OPGM 
01296         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1OPGM 
01297         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1OPGM 
01298         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1OPGM 
01299         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1OPGM 
01300         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1OPGM 
01301         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1OPGM 
01302         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1OPGM 
01303         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1OPGM 
01304         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1OPGM 
01305         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1OPGM 
01306                            GCA-ALL-LEVEL-TAB-ID                   GA1OPGM 
01307         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1OPGM 
01308                            GCA-ALL-LEVEL-TAB-SLOT                 GA1OPGM 
01309         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1OPGM 
01310                            GCA-INTERNAL-TAB-ID,                   GA1OPGM 
01311                            GCA-INTERNAL-TAB-SLOT                  GA1OPGM 
01312         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1OPGM 
01313                                                                   GA1OPGM 
01314      IF  FRMNUIDI  =  'GC8A'                                      GA1OPGM 
01315         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1OPGM 
01316         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1OPGM 
01317         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA1OPGM 
01318         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1OPGM 
01319         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1OPGM 
01320         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1OPGM 
01321         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1OPGM 
01322         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1OPGM 
01323         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1OPGM 
01324         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1OPGM 
01325         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1OPGM 
01326         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA1OPGM 
01327         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1OPGM 
01328         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA1OPGM 
01329                            GCA-ALL-LEVEL-TAB-ID                   GA1OPGM 
01330         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA1OPGM 
01331                            GCA-ALL-LEVEL-TAB-SLOT                 GA1OPGM 
01332         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA1OPGM 
01333                          GCA-INTERNAL-TAB-SLOT.                   GA1OPGM 
01334                                                                   GA1OPGM 
01335      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA1OPGM 
01336 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA1OPGM 
01337 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA1OPGM 
01338 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA1OPGM 
01339 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA1OPGM 
01340 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA1OPGM 
01341      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1OPGM 
01342                                                                   GA1OPGM 
01343      SET GCA-RECORD-POINTER                                       GA1OPGM 
01344        TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                    GA1OPGM 
01345                                                                   GA1OPGM 
01346      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GA1OPGM 
01347        TO GAA-ENTRY-COUNT.                                        GA1OPGM 
01348                                                                   GA1OPGM 
01349      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1OPGM 
01350                                                                   GA1OPGM 
01351      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1OPGM 
01352         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA1OPGM 
01353         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA1OPGM 
01354                                                                   GA1OPGM 
01355      IF  NOT GCIO2-GOOD-RETURN                                    GA1OPGM 
01356         SET WT-01-INDEX TO +03                                    GA1OPGM 
01357         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1OPGM 
01358         MOVE '1O08'  TO  WS-ABEND-CODE                            GA1OPGM 
01359         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1OPGM 
01360                                                                   GA1OPGM 
01361 *    SET COMMAREA-PNTR                                            GA1OPGM 
01362 *      TO ADDRESS OF GCA-COMMAREA.                                GA1OPGM 
01363                                                                   GA1OPGM 
01364      IF  ALTBFNCI  =  'GA1B'                                      GA1OPGM 
01365 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA1OPGM 
01366 *          LENGTH(4) END-EXEC.                                    GA1OPGM 
01367         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA1OPGM 
01368                         COMMAREA(DFHCOMMAREA)                     GA1OPGM 
01369                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1OPGM 
01370         END-EXEC.                                                 GA1OPGM 
01371                                                                   GA1OPGM 
01372      IF  ALTBFNCI  =  'GA1C'                                      GA1OPGM 
01373 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA1OPGM 
01374 *          LENGTH(4) END-EXEC.                                    GA1OPGM 
01375         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA1OPGM 
01376                         COMMAREA(DFHCOMMAREA)                     GA1OPGM 
01377                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1OPGM 
01378         END-EXEC.                                                 GA1OPGM 
01379                                                                   GA1OPGM 
01380      IF  ALTBFNCI  =  'GA1D'                                      GA1OPGM 
01381 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA1OPGM 
01382 *          LENGTH(4) END-EXEC.                                    GA1OPGM 
01383         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA1OPGM 
01384                         COMMAREA(DFHCOMMAREA)                     GA1OPGM 
01385                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1OPGM 
01386         END-EXEC.                                                 GA1OPGM 
01387                                                                   GA1OPGM 
01388      IF  ALTBFNCI  =  'GA1E'                                      GA1OPGM 
01389 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA1OPGM 
01390 *          LENGTH(4) END-EXEC.                                    GA1OPGM 
01391         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA1OPGM 
01392                         COMMAREA(DFHCOMMAREA)                     GA1OPGM 
01393                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1OPGM 
01394         END-EXEC.                                                 GA1OPGM 
01395                                                                   GA1OPGM 
01396      IF  ALTBFNCI  =  'GA1P'                                      GA1OPGM 
01397         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA1OPGM 
01398                         COMMAREA(DFHCOMMAREA)                     GA1OPGM 
01399                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1OPGM 
01400         END-EXEC.                                                 GA1OPGM 
01401                                                                   GA1OPGM 
01402  5099-EXIT.                                                       GA1OPGM 
01403      EXIT.                                                        GA1OPGM 
01404      EJECT                                                        GA1OPGM 
01405 ***************************************************************** GA1OPGM 
01406 **           X C T L   T O   M A I N   M E N U                    GA1OPGM 
01407 **                                                                GA1OPGM 
01408 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1OPGM 
01409 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1OPGM 
01410 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1OPGM 
01411 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1OPGM 
01412 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1OPGM 
01413 ** AND PROGRESS DOWN.                                             GA1OPGM 
01414 ******************************************************************GA1OPGM 
01415  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1OPGM 
01416      MOVE '6000'  TO  WS-PARA-ID.                                 GA1OPGM 
01417      MOVE '1O09'  TO  WS-ABEND-CODE.                              GA1OPGM 
01418                                                                   GA1OPGM 
01419      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1OPGM 
01420                                                                   GA1OPGM 
01421  6099-EXIT.     EXIT.                                             GA1OPGM 
01422 /***************************************************************  GA1OPGM 
01423 *                                                              *  GA1OPGM 
01424 * 9000   MOVE MESSAGE TO SCREEN                                *  GA1OPGM 
01425 *                                                              *  GA1OPGM 
01426 ****************************************************************  GA1OPGM 
01427  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA1OPGM 
01428                                                                   GA1OPGM 
01429          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                     GA1OPGM 
01430                    TO ERRMSGO.                                    GA1OPGM 
01431                                                                   GA1OPGM 
01432  9000-EXIT.  EXIT.                                                GA1OPGM 
01433 /                                                                 GA1OPGM 
01434 ***************************************************************** GA1OPGM 
01435  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1OPGM 
01436                                                                   GA1OPGM 
01437      SET MAP-IDX1 TO 7.                                           GA1OPGM 
01438      SET MAP-IDX2 TO 1.                                           GA1OPGM 
01439      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1OPGM 
01440                                                                   GA1OPGM 
01441      EXEC CICS SEND   MAP('GA1OI01') MAPSET('GA1OSET') ERASE      GA1OPGM 
01442         FROM(GA1OI01O) WAIT END-EXEC.                             GA1OPGM 
01443                                                                   GA1OPGM 
01444      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1OPGM 
01445                                                                   GA1OPGM 
01446  9999-EXIT.     EXIT.                                             GA1OPGM 
