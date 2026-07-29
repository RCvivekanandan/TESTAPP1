00001 *      LAST MAINTENANCE TIME:  8.50.49  DATE: 11/16/84            08/20/03
00002  IDENTIFICATION DIVISION.                                         GA1GPGM 
00003  PROGRAM-ID.     GA1GPGM.                                            LV001
00004 **** THIS IS A COBOL/2 PROGRAM ******                             GA1GPGM 
00005  AUTHOR.         S BUCH.                                          GA1GPGM 
00006  DATE-WRITTEN.   10/29/84.                                        GA1GPGM 
00007  DATE-COMPILED.                                                   GA1GPGM 
00008      SKIP3                                                        GA1GPGM 
00009 ******************************************************************GA1GPGM 
00010 *   GA1GPGM   ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM      GA1GPGM 
00011 *              BENEFIT GROUP BY BENEFIT-PROVISION ID - GA1G       GA1GPGM 
00012 *                                                                 GA1GPGM 
00013 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1GPGM 
00014 *   CURRENTLY ON THE ALL LEVEL INTERNAL TABULAR RECORD.           GA1GPGM 
00015 *                                                                 GA1GPGM 
00016 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1GPGM 
00017 *   ALL LEVEL INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN    GA1GPGM 
00018 *   DECIDE IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN     GA1GPGM 
00019 *   ENTRY WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE    GA1GPGM 
00020 *   RECORD WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED   GA1GPGM 
00021 *   BACK INTO THE RECORD BEFORE UPDATING THE RECORD.              GA1GPGM 
00022 *                                                                 GA1GPGM 
00023 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #IBGR) GA1GPGM 
00024 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1GPGM 
00025 *   XCTL TO TRANS GA2G OR PROGRAM GA2GPGM.  THIS PROGRAM WILL     GA1GPGM 
00026 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1GPGM 
00027 *   TABLE.                                                        GA1GPGM 
00028 *                                                                 GA1GPGM 
00029 *   FUNC CODE: GA1G                                               GA1GPGM 
00030 *   MAPSET:    GA1GSETC                                           GA1GPGM 
00031 *   FILES:     GCPSWORK                                           GA1GPGM 
00032 *                                                                 GA1GPGM 
00033 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00034 *                                                                 GA1GPGM 
00035 *    TAILORING INSTRUCTIONS:                                      GA1GPGM 
00036 *                                                                 GA1GPGM 
00037 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1GPGM 
00038 *                                                                 GA1GPGM 
00039 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1G/     GA1GPGM 
00040 *              SCREEN PAGE NUMBER                 /009I/001G/     GA1GPGM 
00041 *              ADD PROGRAM FUNCTION CODE          /GCAI/GA2G/     GA1GPGM 
00042 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IBGR/    GA1GPGM 
00043 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX1/       GA1GPGM 
00044 *                                                                 GA1GPGM 
00045 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1GPGM 
00046 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1GPGM 
00047 *     OCCURANCES.                                                 GA1GPGM 
00048 *                                                                 GA1GPGM 
00049 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1GPGM 
00050 *     THAT MUST BE CHANGED.                                       GA1GPGM 
00051 *                                                                 GA1GPGM 
00052 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00053      SKIP3                                                        GA1GPGM 
00054 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1GPGM 
00055 *   DATE    PROGRAMMER  MAINTENANCE                             * GA1GPGM 
00056 * --------  ----------  --------------------------------------- * GA1GPGM 
00057 * 11-19-85      LET     REMOVED ALL HANDLE CONDITIONS EXCEPT    * GA1GPGM 
00058 *                       FOR MAPFAIL.                            * GA1GPGM 
00059 *                                                               * GA1GPGM 
00060 *03/16/87       JLA     CHANGES FOR SINGLE TABULAR SUPPORT THAT * GA1GPGM 
00061 * (D0120)               ARE EXECUTED FROM TRANSACTION GTM1:     * GA1GPGM 
00062 *                       1. PF1/PF13 - CONSTRUCT COMMAREA AS IF  * GA1GPGM 
00063 *                          GC4A HAD CALLED, XCTL TO ADD SCREEN  * GA1GPGM 
00064 *                          PROGRAM.                             * GA1GPGM 
00065 *                       2. PF3/PF15 - XCTL TO GTM1PGM WITHOUT   * GA1GPGM 
00066 *                          PASSING ANY COMMAREA.                * GA1GPGM 
00067 *                                                               * GA1GPGM 
00068 * 8/17/87       FRY     CAPTURE OPERATOR-ID WHEN A 'C3', 'C6',  * GA1GPGM 
00069 * (D116)                OR 'G4' RECORD IS UPDATED.              * GA1GPGM 
00070 *                                                               * GA1GPGM 
00071 * 11/30/89     NGE      FIX EMCL/EXCL IND MISSING ON THE SCREEN * GA1GPGM 
00072 *  (1681)                                                       * GA1GPGM 
00073 *                                                                *GA1GPGM 
00074 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GA1GPGM 
00075 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GA1GPGM 
00076 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GA1GPGM 
00077 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GA1GPGM 
00078 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GA1GPGM 
00079 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GA1GPGM 
00080 *                       6. REMOVE HARDCOPY ROUTINE.              *GA1GPGM 
00081 *                       7. >>> CONVERT TO COBOL/II <<<<          *GA1GPGM 
00082 *                                                                *GA1GPGM 
00083 *                                                               * GA1GPGM 
00084 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION           * GA1GPGM 
00085 *                  FIELD    FROM ONE POSITION TO TWO POSITIONS. * GA1GPGM 
00086 *                                                               * GA1GPGM 
00087 *14726/ 10/28/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000     * GA1GPGM 
00088 *15057                  AND THE EXPANSION OF THE GROUP SPECIFIC * GA1GPGM 
00089 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS   * GA1GPGM 
00090 *                       MERGER.                                 * GA1GPGM 
00091 *                                                               * GA1GPGM 
00092 * 14726/  05/15/98  AB   EXPANDED THE SCREEN / MAP               *GA1GPGM 
00093 * 15057                  TO INCLUDE THE ENTIRE KEY               *GA1GPGM 
00094 *                                                               * GA1GPGM 
00095 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP      * GA1GPGM 
00096 *                        2. ADD DELADD-OPTION = 'GAS5UPD'       * GA1GPGM 
00097 *                                                               * GA1GPGM 
00098 * P????  11/19/99 FRY   ADD LENGTH PARAMETER TO THE RETURN       *GA1GPGM 
00099 *                       COMMAND WHEN DFHCOMMAREA IS SPECIFIED.   *GA1GPGM 
00100 *                                                                *GA1GPGM 
00101 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA1GPGM 
SI0724*                                                                *00030141
SI0724* P56703     05/08/24   SI  CHANGES FOR PEAQ COPYBOOK EXPANSION  *00030150
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00030160
SI0724*                           GCCDRLEN                             *00030170
00102 *                                                                *GA1GPGM 
00103 *                                                                *GA1GPGM 
00104 *                                                                *GA1GPGM 
00105 ******************************************************************GA1GPGM 
00106 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1GPGM 
00107 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1GPGM 
00108      EJECT                                                        GA1GPGM 
00109  ENVIRONMENT DIVISION.                                            GA1GPGM 
00110      EJECT                                                        GA1GPGM 
00111  DATA DIVISION.                                                   GA1GPGM 
00112  WORKING-STORAGE SECTION.                                         GA1GPGM 
00113  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1GPGM 
00114      '***GA1GPGM WS BEGINS***'.                                   GA1GPGM 
00115  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1GPGM 
00116                                                                   GA1GPGM 
00117  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1GPGM 
00118                                                                   GA1GPGM 
00119  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1GPGM 
00120  01  COMMAREA-POINTER-AREA.                                       GA1GPGM 
00121      05  COMMAREA-PNTR-COMP      PIC S9(8)  COMP.                 GA1GPGM 
00122      05  COMMAREA-PNTR  REDEFINES                                 GA1GPGM 
00123          COMMAREA-PNTR-COMP      USAGE IS POINTER.                GA1GPGM 
00124                                                                   GA1GPGM 
00125 ** MAP COBOL SCREEN DSECTS **                                     GA1GPGM 
00126  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1GPGM 
00127      '***  I/O MAPAREA ***'.                                      GA1GPGM 
00128  COPY GA1GSETC.                                                   GA1GPGM 
00129      EJECT                                                        GA1GPGM 
00130 ******************************************************************GA1GPGM 
00131 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA1GPGM 
00132 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1GPGM 
00133 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1GPGM 
00134 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1GPGM 
00135 **  REDEFINES.                                                    GA1GPGM 
00136 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00137 **                                                                GA1GPGM 
00138 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1GPGM 
00139 **  FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1GPGM 
00140 **  MATCH THE MAP.                                                GA1GPGM 
00141 **                                                                GA1GPGM 
00142 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00143                                                                   GA1GPGM 
00144  01  FILLER     REDEFINES   GA1GI01I.                             GA1GPGM 
00145      05  FILLER                              PIC X(83).           GA1GPGM 
00146      05  GROUP-SPECIFIC-ID-LINE.                                  GA1GPGM 
00147          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA1GPGM 
00148          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA1GPGM 
00149          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA1GPGM 
00150          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA1GPGM 
00151          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA1GPGM 
00152          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA1GPGM 
00153          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA1GPGM 
00154          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA1GPGM 
00155          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA1GPGM 
00156          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA1GPGM 
00157          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA1GPGM 
00158          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA1GPGM 
00159          10  FILLER                          PIC X(16).           GA1GPGM 
00160      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA1GPGM 
00161          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1GPGM 
00162          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1GPGM 
00163          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1GPGM 
00164          10  CONTRACT-GROUP-NO               PIC X(9).            GA1GPGM 
00165          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1GPGM 
00166          10  CONTRACT-SECTION-NO             PIC X(5).            GA1GPGM 
00167          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1GPGM 
00168          10  CONTRACT-PKG-CODE               PIC X(3).            GA1GPGM 
00169          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1GPGM 
00170          10  CONTRACT-LOB                    PIC X.               GA1GPGM 
00171          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1GPGM 
00172          10  CONTRACT-PROV-CTL               PIC XX.              GA1GPGM 
00173          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1GPGM 
00174          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1GPGM 
00175          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1GPGM 
00176          10  CONTRACT-EFF-DATE               PIC X(6).            GA1GPGM 
00177          10  FILLER                          PIC X(1).            GA1GPGM 
00178      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA1GPGM 
00179                                     GROUP-SPECIFIC-ID-LINE.       GA1GPGM 
00180          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA1GPGM 
00181          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA1GPGM 
00182          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA1GPGM 
00183          10  BEN-PROV-GROUP-NO               PIC X(9).            GA1GPGM 
00184          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA1GPGM 
00185          10  BEN-PROV-SECTION-NO             PIC X(5).            GA1GPGM 
00186          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA1GPGM 
00187          10  BEN-PROV-PKG-CODE               PIC X(3).            GA1GPGM 
00188          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA1GPGM 
00189          10  BEN-PROV-LOB                    PIC X.               GA1GPGM 
00190          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA1GPGM 
00191          10  BEN-PROV-PROV-CTL               PIC XX.              GA1GPGM 
00192          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA1GPGM 
00193          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA1GPGM 
00194          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA1GPGM 
00195          10  BEN-PROV-EFF-DATE               PIC X(6).            GA1GPGM 
00196          10  BEN-PROV-ID-HEADING             PIC X(6).            GA1GPGM 
00197          10  BEN-PROV-ID-NO                  PIC X(6).            GA1GPGM 
00198          10  FILLER                          PIC X(4).            GA1GPGM 
00199      05  FILLER                              PIC X(74).           GA1GPGM 
00200      05  MAP-PROVISION-ID-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED   GA1GPGM 
00201          BY MAP-IDX1.                                             GA1GPGM 
00202        10  MAP-PROVISION-ID-ARGUMENT-COL  OCCURS 3 TIMES INDEXED  GA1GPGM 
00203            BY MAP-IDX2.                                           GA1GPGM 
00204          15  MAP-ACTION-CODE-LEN             PIC S9(4) COMP SYNC. GA1GPGM 
00205          15  MAP-ACTION-CODE-ATTR            PIC X.               GA1GPGM 
00206          15  MAP-ACTION-CODE                 PIC X.               GA1GPGM 
00207          15  MAP-PROVISION-ID-ARGUMENT-LEN   PIC S9(4) COMP SYNC. GA1GPGM 
00208          15  MAP-PROVISION-ID-ARGUMENT-ATTR  PIC X.               GA1GPGM 
00209          15  MAP-PROVISION-ID-ARGUMENT       PIC X(6).            GA1GPGM 
00210 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00211  01  WS-MAP-OCCURS-COUNTERS.                                      GA1GPGM 
00212 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00213 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1GPGM 
00214 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00215      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +14.     GA1GPGM 
00216      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +3.      GA1GPGM 
00217 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00218                                                                   GA1GPGM 
00219 ** ALTERNATIVE WORKFILE KEYS **                                   GA1GPGM 
00220  01  FILLER                      PIC X(32)  VALUE                 GA1GPGM 
00221      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1GPGM 
00222  01  WS-ALT-WORKFILE-KEYS.                                        GA1GPGM 
00223  COPY GCWRKKEY.                                                   GA1GPGM 
00224      EJECT                                                        GA1GPGM 
00225                                                                   GA1GPGM 
00226 ** DATE FORMATTING AREA **                                        GA1GPGM 
00227  01  HGADATES-COMMAREA.                                           GA1GPGM 
00228  COPY HGCDAT01.                                                   GA1GPGM 
00229                                                                   GA1GPGM 
00230 ** WORKFIELDS, AND SWITCHES **                                    GA1GPGM 
00231  01  WS-WORK-FIELDS.                                              GA1GPGM 
00232      05  WS-HEX-00                     PIC X.                     GA1GPGM 
00233      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1GPGM 
00234 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00235 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00236 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00237      05  WS-SAVED-FIELDS.                                         GA1GPGM 
00238        10  WS-SAVED-PROVISION-ID-ARGUMENT                         GA1GPGM 
00239                                        PIC X(6).                  GA1GPGM 
00240 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00241  01  WS-SWITCHES.                                                 GA1GPGM 
00242      05  WS-ERROR-SW                   PIC X.                     GA1GPGM 
00243                                                                   GA1GPGM 
00244 ** TITLE LINES **                                                 GA1GPGM 
00245  01  WS-TITLE-LINES.                                              GA1GPGM 
00246      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA1GPGM 
00247          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA1GPGM 
00248      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA1GPGM 
00249          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA1GPGM 
00250      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA1GPGM 
00251          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA1GPGM 
00252                                                                   GA1GPGM 
00253 /* ATTRIBUTES **                                                  GA1GPGM 
00254  COPY DFHBMSCA.                                                   GA1GPGM 
00255      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1GPGM 
00256      EJECT                                                        GA1GPGM 
00257 ** ATTENTION IDENTIFIERS **                                       GA1GPGM 
00258  COPY DFHAID.                                                     GA1GPGM 
00259      EJECT                                                        GA1GPGM 
00260 ** RECORD LENGTHS **                                              GA1GPGM 
00261  01  WS-RECORD-LENGTHS.                                           GA1GPGM 
00262     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA1GPGM 
00263     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA1GPGM 
00264     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1GPGM 
00265     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1GPGM 
00266                                                                   GA1GPGM 
00267 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA1GPGM 
00268  01  FILLER.                                                      GA1GPGM 
00269      COPY GCCDRLEN.                                               GA1GPGM 
00270                                                                   GA1GPGM 
00271                                                                   GA1GPGM 
00272  01  WS-END                      PIC X(16)  VALUE                 GA1GPGM 
00273      '*** W/S ENDS ***'.                                          GA1GPGM 
00274 /                                                                 GA1GPGM 
00275  LINKAGE SECTION.                                                 GA1GPGM 
00276  01  DFHCOMMAREA.                                                 GA1GPGM 
00277  COPY G2ALCKEC.                                                   GA1GPGM 
00278  COPY GACDACWA.                                                   GA1GPGM 
00279 *    05  INCOMING-COMMAREA-PNTR   USAGE IS POINTER.               GA1GPGM 
00280      05  GAS1UPD-PASSED-AREA.                                     GA1GPGM 
00281          07  LVL2-B-SW           PIC X.                           GA1GPGM 
00282          07  LVL2-F-SW           PIC X.                           GA1GPGM 
00283          07  LVL2-G-SW           PIC X.                           GA1GPGM 
00284          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1GPGM 
00285          07  FILLER              PIC X(9).                        GA1GPGM 
00286      05  DELADD-OPTION           PIC X(7).                        GA1GPGM 
00287                                                                   GA1GPGM 
00288 *01  GCA-COMMAREA.                                                GA1GPGM 
00289 *COPY G2ALCKEC.                                                   GA1GPGM 
00290 /                                                                 GA1GPGM 
00291  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA1GPGM 
00292  COPY GCIOPRM1.                                                   GA1GPGM 
00293      SKIP3                                                        GA1GPGM 
00294  COPY GCWRKDCC.                                                   GA1GPGM 
00295      SKIP3                                                        GA1GPGM 
00296  COPY GCTIBGRC.                                                   GA1GPGM 
00297 /*+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00298 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00299 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00300  01  COPY-TABULAR-TABLE-AREA.                                     GA1GPGM 
00301      05  COPY-TABULAR-TABLE  OCCURS 659 TIMES INDEXED BY          GA1GPGM 
00302            COPY-IDX.                                              GA1GPGM 
00303        10  COPY-PROVISION-ID-ARGUMENT         PIC X(6).           GA1GPGM 
00304 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00305                                                                   GA1GPGM 
00306  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA1GPGM 
00307  COPY GCIOPRM2.                                                   GA1GPGM 
00308      EJECT                                                        GA1GPGM 
00309  COPY GCWRKDC2.                                                   GA1GPGM 
00310      EJECT                                                        GA1GPGM 
00311  COPY GCTABMC.                                                    GA1GPGM 
00312      EJECT                                                        GA1GPGM 
00313                                                                   GA1GPGM 
00314  PROCEDURE DIVISION.                                              GA1GPGM 
00315                                                                   GA1GPGM 
00316 ******************************************************************GA1GPGM 
00317 **               H O U S E K E E P I N G                          GA1GPGM 
00318 **                                                                GA1GPGM 
00319 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA1GPGM 
00320 **                                                                GA1GPGM 
00321 ******************************************************************GA1GPGM 
00322  0000-HOUSEKEEPING  SECTION.                                      GA1GPGM 
00323                                                                   GA1GPGM 
00324      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1GPGM 
00325                                                                   GA1GPGM 
00326      IF EIBAID  =  DFHCLEAR                                       GA1GPGM 
00327          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1GPGM 
00328                         ERASE                                     GA1GPGM 
00329          END-EXEC                                                 GA1GPGM 
00330          EXEC CICS RETURN                                         GA1GPGM 
00331          END-EXEC.                                                GA1GPGM 
00332                                                                   GA1GPGM 
00333      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1GPGM 
00334                                   END-EXEC.                       GA1GPGM 
00335  0000-EXIT.                                                       GA1GPGM 
00336        EXIT.                                                      GA1GPGM 
00337 /*****************************************************************GA1GPGM 
00338 **                     M A I N L I N E                            GA1GPGM 
00339 **                                                                GA1GPGM 
00340 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1GPGM 
00341 **  TAKEN BY THE OPERATOR.                                        GA1GPGM 
00342 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1GPGM 
00343 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1GPGM 
00344 **     ADDITIONS FROM.                                            GA1GPGM 
00345 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1GPGM 
00346 **     KEY PF12 OR PF24.                                          GA1GPGM 
00347 **  3. RECEIVE THE SCREEN.                                        GA1GPGM 
00348 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1GPGM 
00349 **     MENU.                                                      GA1GPGM 
00350 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1GPGM 
00351 **     LOGIC.                                                     GA1GPGM 
00352 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1GPGM 
00353 **     (RETURN) TO THE ADD PROGRAM (GA2GPGM).                     GA1GPGM 
00354 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1GPGM 
00355 **     (RETURN) TO THE PREVIOUS MENU.                             GA1GPGM 
00356 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1GPGM 
00357 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1GPGM 
00358 **                                                                GA1GPGM 
00359 ******************************************************************GA1GPGM 
00360  1000-MAIN-LINE   SECTION.                                        GA1GPGM 
00361                                                                   GA1GPGM 
00362      MOVE '1000'  TO  WS-PARA-ID.                                 GA1GPGM 
00363                                                                   GA1GPGM 
00364      IF EIBTRNID  NOT =  'GA1G'                                   GA1GPGM 
00365         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1GPGM 
00366         GO TO 1099-RETURN.                                        GA1GPGM 
00367                                                                   GA1GPGM 
00368      EXEC CICS RECEIVE   MAP('GA1GI01') MAPSET('GA1GSET')         GA1GPGM 
00369         INTO(GA1GI01I) END-EXEC.                                  GA1GPGM 
00370                                                                   GA1GPGM 
00371      IF SCRNIDNI  NOT =  '001G00'                                 GA1GPGM 
00372         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1GPGM 
00373                                                                   GA1GPGM 
00374      IF EIBAID  =  DFHENTER                                       GA1GPGM 
00375         PERFORM 2000-DELETE-PROCESSING                            GA1GPGM 
00376         GO TO 1099-RETURN.                                        GA1GPGM 
00377                                                                   GA1GPGM 
00378      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1GPGM 
00379         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1GPGM 
00380                                                                   GA1GPGM 
00381      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1GPGM 
00382         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1GPGM 
00383                                                                   GA1GPGM 
00384      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1GPGM 
00385      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1GPGM 
00386      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1GPGM 
00387 -    'THIS PROGRAM ***'  TO  ERRMSGO.                             GA1GPGM 
00388      EXEC CICS SEND   MAP('GA1GI01') MAPSET('GA1GSET') DATAONLY   GA1GPGM 
00389         FROM(GA1GI01O) CURSOR END-EXEC.                           GA1GPGM 
00390                                                                   GA1GPGM 
00391  1099-RETURN.                                                     GA1GPGM 
00392      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA1GPGM 
00393         (DELADD-OPTION = 'GAS1UPD') OR                            GA1GPGM 
00394         (DELADD-OPTION = 'GAS2UPD') OR                            GA1GPGM 
00395         (DELADD-OPTION = 'GAS3UPD') OR                            GA1GPGM 
00396         (DELADD-OPTION = 'GAS4UPD') OR                            GA1GPGM 
00397         (DELADD-OPTION = 'GAS5UPD')                               GA1GPGM 
00398          EXEC CICS RETURN END-EXEC                                GA1GPGM 
00399      ELSE                                                         GA1GPGM 
00400          EXEC CICS RETURN TRANSID('GA1G')                         GA1GPGM 
00401                    COMMAREA(DFHCOMMAREA)                          GA1GPGM 
00402                    LENGTH  (EIBCALEN)                             GA1GPGM 
00403                    END-EXEC.                                      GA1GPGM 
00404                                                                   GA1GPGM 
00405      GOBACK.                                                      GA1GPGM 
00406                                                                   GA1GPGM 
00407  1999-EXIT.                                                       GA1GPGM 
00408        EXIT.                                                      GA1GPGM 
00409 /*****************************************************************GA1GPGM 
00410 **              D E L E T E   P R O C E S S I N G                 GA1GPGM 
00411 **                                                                GA1GPGM 
00412 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1GPGM 
00413 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1GPGM 
00414 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1GPGM 
00415 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1GPGM 
00416 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1GPGM 
00417 **    BACK INTO THE RECORD THAT WE READ.)                         GA1GPGM 
00418 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1GPGM 
00419 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1GPGM 
00420 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1GPGM 
00421 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1GPGM 
00422 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1GPGM 
00423 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1GPGM 
00424 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1GPGM 
00425 **    ENTRY.                                                      GA1GPGM 
00426 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1GPGM 
00427 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1GPGM 
00428 **    RECORD.                                                     GA1GPGM 
00429 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1GPGM 
00430 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1GPGM 
00431 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1GPGM 
00432 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1GPGM 
00433 **    ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1GPGM 
00434 **    SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1GPGM 
00435 **    BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1GPGM 
00436 **                                                                GA1GPGM 
00437 ******************************************************************GA1GPGM 
00438  2000-DELETE-PROCESSING SECTION.                                  GA1GPGM 
00439                                                                   GA1GPGM 
00440      MOVE '2000'  TO  WS-PARA-ID.                                 GA1GPGM 
00441      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1GPGM 
00442      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1GPGM 
00443      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1GPGM 
00444                                                                   GA1GPGM 
00445      MOVE '2010'  TO  WS-PARA-ID.                                 GA1GPGM 
00446  2010-VALIDATE-ACT-CODE.                                          GA1GPGM 
00447      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1GPGM 
00448         ADD 1  TO  WS-DELETE-COUNT.                               GA1GPGM 
00449      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D' OR           GA1GPGM 
00450         = SPACE OR =  LOW-VALUES                                  GA1GPGM 
00451         MOVE DFHBMUNF  TO                                         GA1GPGM 
00452            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1GPGM 
00453 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00454 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00455 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00456         MOVE DFHBMASF  TO                                         GA1GPGM 
00457            MAP-PROVISION-ID-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA1GPGM 
00458      ELSE                                                         GA1GPGM 
00459         MOVE DFHBMUBF  TO                                         GA1GPGM 
00460            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1GPGM 
00461         MOVE DFHBMABF  TO                                         GA1GPGM 
00462            MAP-PROVISION-ID-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA1GPGM 
00463 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00464         IF WS-ERROR-SW  NOT =  'Y'                                GA1GPGM 
00465            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1GPGM 
00466            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1GPGM 
00467                                                                   GA1GPGM 
00468      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1GPGM 
00469         SET MAP-IDX1   UP BY  1                                   GA1GPGM 
00470      ELSE                                                         GA1GPGM 
00471         IF MAP-IDX2  <  WS-MAP-COL                                GA1GPGM 
00472            SET MAP-IDX1  TO  1                                    GA1GPGM 
00473            SET MAP-IDX2  UP BY  1                                 GA1GPGM 
00474         ELSE                                                      GA1GPGM 
00475            GO TO 2020-DONE-VALIDATE-A-C.                          GA1GPGM 
00476 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00477 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00478 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00479      IF MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)            GA1GPGM 
00480         NOT =  LOW-VALUES                                         GA1GPGM 
00481 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00482         GO TO 2010-VALIDATE-ACT-CODE.                             GA1GPGM 
00483                                                                   GA1GPGM 
00484  2020-DONE-VALIDATE-A-C.                                          GA1GPGM 
00485      MOVE '2020'  TO  WS-PARA-ID.                                 GA1GPGM 
00486      SET MAP-IDX1   TO  1.                                        GA1GPGM 
00487                                                                   GA1GPGM 
00488      IF WS-ERROR-SW  =  'Y'                                       GA1GPGM 
00489         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1GPGM 
00490            ERRMSGO                                                GA1GPGM 
00491         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1GPGM 
00492            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA1GPGM 
00493            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA1GPGM 
00494 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00495 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1GPGM 
00496 **  ADD ITS MAP FIELD NAME HERE.                                  GA1GPGM 
00497 ****************************************************************  GA1GPGM 
00498            INCEXCO                                                GA1GPGM 
00499         MOVE '2100'  TO  WS-PARA-ID                               GA1GPGM 
00500         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1GPGM 
00501            VARYING MAP-IDX2 FROM  1  BY  1                        GA1GPGM 
00502                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1GPGM 
00503              AFTER MAP-IDX1 FROM  1  BY  1                        GA1GPGM 
00504                             UNTIL MAP-IDX1  >  WS-MAP-ROW         GA1GPGM 
00505         EXEC CICS SEND   MAP('GA1GI01') MAPSET('GA1GSET') DATAONLYGA1GPGM 
00506            FROM(GA1GI01O) CURSOR END-EXEC                         GA1GPGM 
00507         GO TO 2099-EXIT.                                          GA1GPGM 
00508                                                                   GA1GPGM 
00509      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1GPGM 
00510               GC-GCIOPARM-LEN                 +                   GA1GPGM 
00511               GC-WORKFILE-KEY-LEN             +                   GA1GPGM 
00512               GC-GCTABULR-IBGR-FIXED-LEN      +                   GA1GPGM 
00513              (GC-GCTABULR-IBGR-VARY-LEN       *                   GA1GPGM 
00514               GC-GCTABULR-IBGR-VARY-MAX-OCUR)                     GA1GPGM 
00515                                                                   GA1GPGM 
00516                                                                   GA1GPGM 
00517      EXEC CICS                                                    GA1GPGM 
00518         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA1GPGM 
00519         INITIMG(WS-HEX-00)                                        GA1GPGM 
00520         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA1GPGM 
00521      END-EXEC.                                                    GA1GPGM 
00522                                                                   GA1GPGM 
00523      IF  FRMNUIDI  =  'GS3A'                                      GA1GPGM 
00524         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1GPGM 
00525         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1GPGM 
00526         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA1GPGM 
00527         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1GPGM 
00528 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1GPGM 
00529         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1GPGM 
00530 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1GPGM 
00531         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1GPGM 
00532         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1GPGM 
00533         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1GPGM 
00534                          GCIO-WRK-PROVIDER-CONTROL                GA1GPGM 
00535         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1GPGM 
00536         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1GPGM 
00537                                                                   GA1GPGM 
00538      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1GPGM 
00539         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1GPGM 
00540         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1GPGM 
00541         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1GPGM 
00542         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1GPGM 
00543 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1GPGM 
00544         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1GPGM 
00545 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1GPGM 
00546         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1GPGM 
00547         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1GPGM 
00548         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1GPGM 
00549         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1GPGM 
00550         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1GPGM 
00551         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1GPGM 
00552                                                                   GA1GPGM 
00553      IF  FRMNUIDI  =  'GC8A'                                      GA1GPGM 
00554         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1GPGM 
00555         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1GPGM 
00556         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA1GPGM 
00557         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1GPGM 
00558 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1GPGM 
00559         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1GPGM 
00560 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1GPGM 
00561         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1GPGM 
00562         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1GPGM 
00563         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1GPGM 
00564         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1GPGM 
00565         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1GPGM 
00566         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1GPGM 
00567                                                                   GA1GPGM 
00568      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA1GPGM 
00569      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1GPGM 
00570      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA1GPGM 
00571      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA1GPGM 
00572      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA1GPGM 
00573      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA1GPGM 
00574      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1GPGM 
00575                                                                   GA1GPGM 
00576      IF WS-DELETE-COUNT  =  ZERO                                  GA1GPGM 
00577         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1GPGM 
00578                                                                   GA1GPGM 
00579 ******************************************************************GA1GPGM 
00580 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1GPGM 
00581 *                                                                 GA1GPGM 
00582 ******************************************************************GA1GPGM 
00583                                                                   GA1GPGM 
00584      MOVE  GC-GCTABULR-IBGR-VARY-MAX-OCUR                         GA1GPGM 
00585            TO  GX1-ENTRY-COUNT.                                   GA1GPGM 
00586                                                                   GA1GPGM 
00587      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1GPGM 
00588                                                                   GA1GPGM 
00589      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1GPGM 
00590         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1GPGM 
00591         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1GPGM 
00592                                                                   GA1GPGM 
00593      IF  NOT GCIO-GOOD-RETURN                                     GA1GPGM 
00594         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1GPGM 
00595 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1GPGM 
00596         MOVE '1GF1'  TO  WS-ABEND-CODE                            GA1GPGM 
00597         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1GPGM 
00598                                                                   GA1GPGM 
00599      COMPUTE WS-COPY-LENGTH  =                                    GA1GPGM 
00600              GX1-ENTRY-COUNT  *  GC-GCTABULR-IBGR-VARY-LEN.       GA1GPGM 
00601                                                                   GA1GPGM 
00602      EXEC CICS                                                    GA1GPGM 
00603         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA1GPGM 
00604         LENGTH      (WS-COPY-LENGTH)                              GA1GPGM 
00605         INITIMG     (WS-HEX-00)                                   GA1GPGM 
00606      END-EXEC.                                                    GA1GPGM 
00607                                                                   GA1GPGM 
00608      MOVE GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT.                   GA1GPGM 
00609      SET COPY-IDX,  GX1-INDEX  TO  1.                             GA1GPGM 
00610                                                                   GA1GPGM 
00611      MOVE '2030'  TO  WS-PARA-ID.                                 GA1GPGM 
00612  2030-MAKE-A-COPY-OF-RECORD.                                      GA1GPGM 
00613      IF GX1-INDEX  NOT >  GX1-ENTRY-COUNT                         GA1GPGM 
00614         MOVE GX1-ENTRY (GX1-INDEX)  TO                            GA1GPGM 
00615            COPY-TABULAR-TABLE (COPY-IDX)                          GA1GPGM 
00616            SET COPY-IDX,  GX1-INDEX  UP BY  1                     GA1GPGM 
00617            GO TO 2030-MAKE-A-COPY-OF-RECORD.                      GA1GPGM 
00618      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GX1-INDEX  TO  1.         GA1GPGM 
00619                                                                   GA1GPGM 
00620      MOVE '2040'  TO  WS-PARA-ID.                                 GA1GPGM 
00621  2040-DELETE-MARKED-ENTRIES.                                      GA1GPGM 
00622 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00623 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00624 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00625      IF MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)  =         GA1GPGM 
00626            LOW-VALUES                                             GA1GPGM 
00627         GO TO 2060-SAVE-REST-OF-COPY.                             GA1GPGM 
00628                                                                   GA1GPGM 
00629      IF MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)  >         GA1GPGM 
00630         COPY-PROVISION-ID-ARGUMENT (COPY-IDX)                     GA1GPGM 
00631         GO TO 2050-SAVE-COPIED-ENTRY                              GA1GPGM 
00632      ELSE                                                         GA1GPGM 
00633         IF MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)  <      GA1GPGM 
00634            COPY-PROVISION-ID-ARGUMENT (COPY-IDX)                  GA1GPGM 
00635            MOVE '1GL1'  TO  WS-ABEND-CODE                         GA1GPGM 
00636            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1GPGM 
00637 -    'RM SYSTEMS AREA ***'  TO  ERRMSGO                           GA1GPGM 
00638            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1GPGM 
00639                                                                   GA1GPGM 
00640 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00641                                                                   GA1GPGM 
00642      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1GPGM 
00643         IF MAP-IDX1   <  WS-MAP-ROW                               GA1GPGM 
00644            SET MAP-IDX1   UP BY  1                                GA1GPGM 
00645            GO TO 2050-SAVE-COPIED-ENTRY                           GA1GPGM 
00646         ELSE                                                      GA1GPGM 
00647            IF MAP-IDX2  <  WS-MAP-COL                             GA1GPGM 
00648               SET MAP-IDX1  TO  1                                 GA1GPGM 
00649               SET MAP-IDX2  UP BY  1                              GA1GPGM 
00650               GO TO 2050-SAVE-COPIED-ENTRY                        GA1GPGM 
00651            ELSE                                                   GA1GPGM 
00652               GO TO 2060-SAVE-REST-OF-COPY.                       GA1GPGM 
00653                                                                   GA1GPGM 
00654      SET COPY-IDX  UP BY  1.                                      GA1GPGM 
00655      IF COPY-IDX  NOT <  GX1-ENTRY-COUNT                          GA1GPGM 
00656         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    GA1GPGM 
00657            GX1-ENTRY (GX1-INDEX)                                  GA1GPGM 
00658         SET  GX1-ENTRY-COUNT  TO  GX1-INDEX                       GA1GPGM 
00659         MOVE GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT                 GA1GPGM 
00660         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1GPGM 
00661      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1GPGM 
00662         SET MAP-IDX1   UP BY  1                                   GA1GPGM 
00663         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1GPGM 
00664      IF MAP-IDX2  <  WS-MAP-COL                                   GA1GPGM 
00665         SET MAP-IDX1  TO  1                                       GA1GPGM 
00666         SET MAP-IDX2  UP BY  1                                    GA1GPGM 
00667         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1GPGM 
00668      ELSE                                                         GA1GPGM 
00669         GO TO 2060-SAVE-REST-OF-COPY.                             GA1GPGM 
00670                                                                   GA1GPGM 
00671  2050-SAVE-COPIED-ENTRY.                                          GA1GPGM 
00672      MOVE '2050'  TO  WS-PARA-ID.                                 GA1GPGM 
00673      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1GPGM 
00674         GX1-ENTRY (GX1-INDEX).                                    GA1GPGM 
00675                                                                   GA1GPGM 
00676      SET GX1-INDEX  UP BY  1.                                     GA1GPGM 
00677      IF COPY-IDX  <  GX1-ENTRY-COUNT                              GA1GPGM 
00678         SET COPY-IDX  UP BY  1                                    GA1GPGM 
00679         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1GPGM 
00680      ELSE                                                         GA1GPGM 
00681 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1GPGM 
00682 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1GPGM 
00683 ***      THE TABLE OF ENTRIES.                                    GA1GPGM 
00684         MOVE '1GL2'  TO  WS-ABEND-CODE                            GA1GPGM 
00685         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1GPGM 
00686 -    'SYSTEMS AREA ***'  TO  ERRMSGO                              GA1GPGM 
00687         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1GPGM 
00688                                                                   GA1GPGM 
00689  2060-SAVE-REST-OF-COPY.                                          GA1GPGM 
00690      MOVE '2060'  TO  WS-PARA-ID.                                 GA1GPGM 
00691      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1GPGM 
00692         GX1-ENTRY (GX1-INDEX).                                    GA1GPGM 
00693                                                                   GA1GPGM 
00694      SET GX1-INDEX  UP BY  1.                                     GA1GPGM 
00695      IF COPY-IDX  <  GX1-ENTRY-COUNT                              GA1GPGM 
00696         SET COPY-IDX  UP BY  1                                    GA1GPGM 
00697         GO TO 2060-SAVE-REST-OF-COPY.                             GA1GPGM 
00698                                                                   GA1GPGM 
00699      SET GX1-INDEX  DOWN BY  1.                                   GA1GPGM 
00700      SET GX1-ENTRY-COUNT  TO  GX1-INDEX.                          GA1GPGM 
00701      MOVE GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT.                   GA1GPGM 
00702                                                                   GA1GPGM 
00703  2070-UPDATE-MODIFIED-REC.                                        GA1GPGM 
00704      MOVE '2070'  TO  WS-PARA-ID.                                 GA1GPGM 
00705                                                                   GA1GPGM 
00706 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1GPGM 
00707                                                                   GA1GPGM 
00708      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1GPGM 
00709      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1GPGM 
00710                                                                   GA1GPGM 
00711      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1GPGM 
00712               GC-WORKFILE-KEY-LEN             +                   GA1GPGM 
00713               GC-GCTABULR-IBGR-FIXED-LEN      +                   GA1GPGM 
00714              (GC-GCTABULR-IBGR-VARY-LEN       *  GX1-ENTRY-COUNT).GA1GPGM 
00715                                                                   GA1GPGM 
00716      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1GPGM 
00717            GC-GCIOPARM-LEN   +  GCIO-RECORD-LENGTH.               GA1GPGM 
00718                                                                   GA1GPGM 
00719      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1GPGM 
00720         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1GPGM 
00721         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1GPGM 
00722                                                                   GA1GPGM 
00723      IF GCIO-GOOD-RETURN                                          GA1GPGM 
00724         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1GPGM 
00725      MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR.  CONTACGA1GPGM 
00726 -    'T SYSTEMS AREA ***'  TO  ERRMSGO.                           GA1GPGM 
00727      MOVE '1GF2'  TO  WS-ABEND-CODE.                              GA1GPGM 
00728      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1GPGM 
00729                                                                   GA1GPGM 
00730  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1GPGM 
00731      MOVE  '2080'  TO  WS-PARA-ID.                                GA1GPGM 
00732                                                                   GA1GPGM 
00733      MOVE  GC-GCTABULR-IBGR-VARY-MAX-OCUR                         GA1GPGM 
00734            TO  GX1-ENTRY-COUNT.                                   GA1GPGM 
00735                                                                   GA1GPGM 
00736      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1GPGM 
00737                                                                   GA1GPGM 
00738      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1GPGM 
00739         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1GPGM 
00740         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1GPGM 
00741                                                                   GA1GPGM 
00742      IF GCIO-GOOD-RETURN                                          GA1GPGM 
00743         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1GPGM 
00744      MOVE '1GF3'  TO  WS-ABEND-CODE.                              GA1GPGM 
00745      MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTACT GA1GPGM 
00746 -    'SYSTEMS AREA ***'  TO  ERRMSGO.                             GA1GPGM 
00747      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1GPGM 
00748                                                                   GA1GPGM 
00749  2090-BUILD-NEXT-DISPLAY.                                         GA1GPGM 
00750      MOVE  '2090'  TO  WS-PARA-ID.                                GA1GPGM 
00751      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1GPGM 
00752      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1GPGM 
00753      SET GX1-INDEX  TO  1.                                        GA1GPGM 
00754 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00755 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00756 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00757      IF MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2)  =         GA1GPGM 
00758            LOW-VALUES                                             GA1GPGM 
00759         MOVE GX1-ENTRY (GX1-INDEX)  TO  WS-SAVED-FIELDS           GA1GPGM 
00760      ELSE                                                         GA1GPGM 
00761         MOVE MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2) TO    GA1GPGM 
00762            WS-SAVED-PROVISION-ID-ARGUMENT.                        GA1GPGM 
00763 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00764                                                                   GA1GPGM 
00765      PERFORM 4500-FILL-THE-SCREEN.                                GA1GPGM 
00766      EXEC CICS SEND   MAP('GA1GI01') MAPSET('GA1GSET') ERASE      GA1GPGM 
00767         FROM(GA1GI01O) END-EXEC.                                  GA1GPGM 
00768                                                                   GA1GPGM 
00769  2099-EXIT.   EXIT.                                               GA1GPGM 
00770      EJECT                                                        GA1GPGM 
00771  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1GPGM 
00772 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00773 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00774 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00775      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1GPGM 
00776         MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1GPGM 
00777 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00778                                                                   GA1GPGM 
00779  2199-EXIT.   EXIT.                                               GA1GPGM 
00780      EJECT                                                        GA1GPGM 
00781 ******************************************************************GA1GPGM 
00782 **          X C T L   T O   A D D   S C R E E N                   GA1GPGM 
00783 **                                                                GA1GPGM 
00784 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1GPGM 
00785 ** ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1GPGM 
00786 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1GPGM 
00787 ** TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1GPGM 
00788 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1GPGM 
00789 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1GPGM 
00790 ******************************************************************GA1GPGM 
00791  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1GPGM 
00792      MOVE '3000'  TO  WS-PARA-ID.                                 GA1GPGM 
00793                                                                   GA1GPGM 
00794 *    EXEC CICS                                                    GA1GPGM 
00795 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1GPGM 
00796 *       LENGTH      (WS-COMMUNICATION-KEY-LEN)                    GA1GPGM 
00797 *       INITIMG     (WS-HEX-00)                                   GA1GPGM 
00798 *    END-EXEC.                                                    GA1GPGM 
00799                                                                   GA1GPGM 
00800 *    IF  FRMNUIDI  =  'GS3A'                                      GA1GPGM 
00801 ***     MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA1GPGM 
00802 *       MOVE  GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                   GA1GPGM 
00803 *       MOVE  GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO               GA1GPGM 
00804 *       MOVE  GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1GPGM 
00805 *       MOVE  GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1GPGM 
00806 *       MOVE  SPACES  TO  GCA-L-O-B,                              GA1GPGM 
00807 *                         GCA-PROV-CTL,                           GA1GPGM 
00808 *                         GCA-BEN-PROV-ID.                        GA1GPGM 
00809                                                                   GA1GPGM 
00810 *    IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1GPGM 
00811 ***     MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA1GPGM 
00812 *       MOVE  CONTRACT-GROUP-NO  TO  GCA-GRP-NO                   GA1GPGM 
00813 *       MOVE  CONTRACT-SECTION-NO  TO  GCA-SECTN-NO               GA1GPGM 
00814 *       MOVE  CONTRACT-LOB  TO  GCA-L-O-B                         GA1GPGM 
00815 *       MOVE  CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                 GA1GPGM 
00816 *       MOVE  CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1GPGM 
00817 *       MOVE  CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1GPGM 
00818 *       MOVE  SPACES  TO  GCA-BEN-PROV-ID.                        GA1GPGM 
00819                                                                   GA1GPGM 
00820 *    IF  FRMNUIDI  =  'GC8A'                                      GA1GPGM 
00821 ***     MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA1GPGM 
00822 *       MOVE  BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                   GA1GPGM 
00823 *       MOVE  BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO               GA1GPGM 
00824 *       MOVE  BEN-PROV-LOB  TO  GCA-L-O-B                         GA1GPGM 
00825 *       MOVE  BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                 GA1GPGM 
00826 *       MOVE  BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1GPGM 
00827 *       MOVE  BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1GPGM 
00828 *       MOVE  BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                GA1GPGM 
00829                                                                   GA1GPGM 
00830      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1GPGM 
00831      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1GPGM 
00832      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1GPGM 
00833      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1GPGM 
00834      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1GPGM 
00835      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1GPGM 
00836      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1GPGM 
00837      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1GPGM 
00838 *    MOVE  ZEROES  TO  GCA-EFF-DT.                                GA1GPGM 
00839 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00840 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA1GPGM 
00841 ** FIELDS TO DISPLAY ON THE INITIAL ADD SCREEN THEY SHOULD BE     GA1GPGM 
00842 ** PASSED HERE.                                                   GA1GPGM 
00843 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00844      MOVE INCEXCI TO GCA-I-E-INDC.                                GA1GPGM 
00845                                                                   GA1GPGM 
00846 *    SET  COMMAREA-PNTR   TO                                      GA1GPGM 
00847 *         ADDRESS  OF GCA-COMMAREA.                               GA1GPGM 
00848                                                                   GA1GPGM 
00849 *    EXEC CICS XCTL  PROGRAM('GA2GPGM') COMMAREA(COMMAREA-PNTR)   GA1GPGM 
00850 *       LENGTH(4) END-EXEC.                                       GA1GPGM 
00851      EXEC CICS XCTL PROGRAM('GA2GPGM')                            GA1GPGM 
00852                     COMMAREA(DFHCOMMAREA)                         GA1GPGM 
00853                     LENGTH (LENGTH OF DFHCOMMAREA)                GA1GPGM 
00854      END-EXEC.                                                    GA1GPGM 
00855                                                                   GA1GPGM 
00856  3099-EXIT.   EXIT.                                               GA1GPGM 
00857      EJECT                                                        GA1GPGM 
00858 ***************************************************************** GA1GPGM 
00859 **          D I S P L A Y   F I R S T   S C R E E N               GA1GPGM 
00860 **                                                                GA1GPGM 
00861 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1GPGM 
00862 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1GPGM 
00863 ** ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1GPGM 
00864 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1GPGM 
00865 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1GPGM 
00866 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1GPGM 
00867 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1GPGM 
00868 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1GPGM 
00869 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1GPGM 
00870 ** DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1GPGM 
00871 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1GPGM 
00872 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1GPGM 
00873 ******************************************************************GA1GPGM 
00874  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1GPGM 
00875      MOVE '4000'  TO  WS-PARA-ID.                                 GA1GPGM 
00876                                                                   GA1GPGM 
00877      MOVE LOW-VALUES   TO GA1GI01I.                               GA1GPGM 
00878      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1GPGM 
00879         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1GPGM 
00880            TO ERRMSGO                                             GA1GPGM 
00881         MOVE '1GC1'  TO  WS-ABEND-CODE                            GA1GPGM 
00882         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1GPGM 
00883                                                                   GA1GPGM 
00884 *    SET  ADDRESS OF  GCA-COMMAREA  TO                            GA1GPGM 
00885 *         INCOMING-COMMAREA-PNTR.                                 GA1GPGM 
00886                                                                   GA1GPGM 
00887      SET ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD  TO              GA1GPGM 
00888          GCA-RECORD-POINTER.                                      GA1GPGM 
00889                                                                   GA1GPGM 
00890      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA1GPGM 
00891      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA1GPGM 
00892      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA1GPGM 
00893      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA1GPGM 
00894      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA1GPGM 
00895      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA1GPGM 
00896      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA1GPGM 
00897      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA1GPGM 
00898 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00899 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1GPGM 
00900 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1GPGM 
00901 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00902      MOVE GX1-INCLUDE-EXCLUDE-IND  TO  GCA-I-E-INDC.              GA1GPGM 
00903      MOVE GCA-I-E-INDC  TO  INCEXCO.                              GA1GPGM 
00904                                                                   GA1GPGM 
00905      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1GPGM 
00906         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA1GPGM 
00907 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA1GPGM 
00908         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA1GPGM 
00909         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA1GPGM 
00910         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA1GPGM 
00911         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA1GPGM 
00912         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1GPGM 
00913         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA1GPGM 
00914         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA1GPGM 
00915         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA1GPGM 
00916         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1GPGM 
00917         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1GPGM 
00918         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1GPGM 
00919         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1GPGM 
00920                                                                   GA1GPGM 
00921      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1GPGM 
00922         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA1GPGM 
00923 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1GPGM 
00924         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA1GPGM 
00925         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA1GPGM 
00926         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA1GPGM 
00927         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA1GPGM 
00928         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1GPGM 
00929         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA1GPGM 
00930         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA1GPGM 
00931         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA1GPGM 
00932         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1GPGM 
00933         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1GPGM 
00934         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1GPGM 
00935         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1GPGM 
00936         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1GPGM 
00937         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1GPGM 
00938         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1GPGM 
00939         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1GPGM 
00940                                                                   GA1GPGM 
00941      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1GPGM 
00942         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA1GPGM 
00943         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA1GPGM 
00944         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA1GPGM 
00945         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA1GPGM 
00946         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA1GPGM 
00947         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA1GPGM 
00948         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA1GPGM 
00949         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA1GPGM 
00950         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA1GPGM 
00951         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA1GPGM 
00952         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1GPGM 
00953         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA1GPGM 
00954         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1GPGM 
00955         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA1GPGM 
00956         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1GPGM 
00957         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA1GPGM 
00958         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1GPGM 
00959         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA1GPGM 
00960         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1GPGM 
00961                                                                   GA1GPGM 
00962      SET GX1-INDEX  TO  1.                                        GA1GPGM 
00963      MOVE GX1-ENTRY (GX1-INDEX)  TO  WS-SAVED-FIELDS.             GA1GPGM 
00964                                                                   GA1GPGM 
00965      PERFORM 4500-FILL-THE-SCREEN.                                GA1GPGM 
00966      EXEC CICS SEND   MAP('GA1GI01') MAPSET('GA1GSET') ERASE      GA1GPGM 
00967         FROM(GA1GI01O) END-EXEC.                                  GA1GPGM 
00968                                                                   GA1GPGM 
00969  4099-EXIT.   EXIT.                                               GA1GPGM 
00970      EJECT                                                        GA1GPGM 
00971 ***************************************************************** GA1GPGM 
00972 **             F I L L   T H E   S C R E E N                      GA1GPGM 
00973 **                                                                GA1GPGM 
00974 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1GPGM 
00975 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1GPGM 
00976 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1GPGM 
00977 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1GPGM 
00978 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1GPGM 
00979 ******************************************************************GA1GPGM 
00980  4500-FILL-THE-SCREEN SECTION.                                    GA1GPGM 
00981                                                                   GA1GPGM 
00982      MOVE '4500'  TO  WS-PARA-ID.                                 GA1GPGM 
00983      MOVE  GX1-ENTRY-COUNT  TO  GX1-ENTRY-COUNT.                  GA1GPGM 
00984      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1GPGM 
00985                                                                   GA1GPGM 
00986      IF GX1-ENTRY-COUNT  NOT >  1                                 GA1GPGM 
00987         MOVE '4530'  TO  WS-PARA-ID                               GA1GPGM 
00988         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1GPGM 
00989                                                                   GA1GPGM 
00990      SET GX1-INDEX  TO  1.                                        GA1GPGM 
00991      MOVE '4510'  TO  WS-PARA-ID.                                 GA1GPGM 
00992  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1GPGM 
00993 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00994 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
00995 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00996      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)  <                  GA1GPGM 
00997            WS-SAVED-PROVISION-ID-ARGUMENT                         GA1GPGM 
00998 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
00999         SET GX1-INDEX  UP BY  1                                   GA1GPGM 
01000         IF  GX1-INDEX  <  GX1-ENTRY-COUNT                         GA1GPGM 
01001            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1GPGM 
01002         ELSE                                                      GA1GPGM 
01003            SET GX1-INDEX  TO  1.                                  GA1GPGM 
01004                                                                   GA1GPGM 
01005      MOVE '4520'  TO  WS-PARA-ID.                                 GA1GPGM 
01006  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1GPGM 
01007      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1GPGM 
01008      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1GPGM 
01009 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
01010 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
01011 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
01012      MOVE GX1-PROVISION-ID-ARGUMENT (GX1-INDEX)  TO               GA1GPGM 
01013         MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1GPGM 
01014 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
01015                                                                   GA1GPGM 
01016      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1GPGM 
01017         SET  MAP-IDX1  UP BY  1                                   GA1GPGM 
01018      ELSE                                                         GA1GPGM 
01019         IF MAP-IDX2  <  WS-MAP-COL                                GA1GPGM 
01020            SET  MAP-IDX1  TO  1                                   GA1GPGM 
01021            SET  MAP-IDX2  UP BY  1                                GA1GPGM 
01022         ELSE                                                      GA1GPGM 
01023            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1GPGM 
01024                                                                   GA1GPGM 
01025      IF GX1-INDEX  <  (GX1-ENTRY-COUNT - 1 )                      GA1GPGM 
01026         SET  GX1-INDEX  UP BY  1                                  GA1GPGM 
01027         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1GPGM 
01028                                                                   GA1GPGM 
01029      MOVE '4530'  TO  WS-PARA-ID.                                 GA1GPGM 
01030  4530-FILL-REST-WITH-NULLS.                                       GA1GPGM 
01031      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1GPGM 
01032 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
01033 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1GPGM 
01034 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
01035      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1GPGM 
01036         MAP-PROVISION-ID-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1GPGM 
01037 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1GPGM 
01038      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1GPGM 
01039         SET  MAP-IDX1   UP BY  1                                  GA1GPGM 
01040         GO TO 4530-FILL-REST-WITH-NULLS                           GA1GPGM 
01041      ELSE                                                         GA1GPGM 
01042         IF MAP-IDX2  <  WS-MAP-COL                                GA1GPGM 
01043            SET  MAP-IDX1  TO  1                                   GA1GPGM 
01044            SET  MAP-IDX2  UP BY 1                                 GA1GPGM 
01045            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1GPGM 
01046                                                                   GA1GPGM 
01047      MOVE '4540'  TO  WS-PARA-ID.                                 GA1GPGM 
01048  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1GPGM 
01049      IF GX1-ENTRY-COUNT  =  1                                     GA1GPGM 
01050         MOVE '*** NO ENTRIES TO DELETE ***'  TO  ERRMSGO          GA1GPGM 
01051         GO TO 4599-EXIT.                                          GA1GPGM 
01052                                                                   GA1GPGM 
01053      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1GPGM 
01054         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'  TO  ERRMSGO.   GA1GPGM 
01055                                                                   GA1GPGM 
01056  4599-EXIT.     EXIT.                                             GA1GPGM 
01057      EJECT                                                        GA1GPGM 
01058 ***************************************************************** GA1GPGM 
01059 **        X C T L   T O   P R E V I O U S   M E N U               GA1GPGM 
01060 **                                                                GA1GPGM 
01061 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1GPGM 
01062 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1GPGM 
01063 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1GPGM 
01064 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1GPGM 
01065 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1GPGM 
01066 ******************************************************************GA1GPGM 
01067  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1GPGM 
01068      MOVE '5000'  TO  WS-PARA-ID.                                 GA1GPGM 
01069                                                                   GA1GPGM 
01070                                                                   GA1GPGM 
01071 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA1GPGM 
01072 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA1GPGM 
01073 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA1GPGM 
01074                                                                   GA1GPGM 
01075      IF  ALTBFNCI  =  'GTM1'                                      GA1GPGM 
01076          EXEC CICS XCTL                                           GA1GPGM 
01077                    PROGRAM('GTM1PGM')                             GA1GPGM 
01078                    END-EXEC.                                      GA1GPGM 
01079                                                                   GA1GPGM 
01080      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA1GPGM 
01081               GC-GCIOPARM-LEN             +                       GA1GPGM 
01082               GC-WORKFILE-KEY-LEN         +                       GA1GPGM 
01083               GC-GCTABULR-ABM-FIXED-LEN   +                       GA1GPGM 
01084              (GC-GCTABULR-ABM-VARY-LEN    *                       GA1GPGM 
01085               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA1GPGM 
01086                                                                   GA1GPGM 
01087      EXEC CICS                                                    GA1GPGM 
01088         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA1GPGM 
01089         INITIMG(WS-HEX-00)                                        GA1GPGM 
01090         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA1GPGM 
01091      END-EXEC.                                                    GA1GPGM 
01092                                                                   GA1GPGM 
01093 *    EXEC CICS                                                    GA1GPGM 
01094 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1GPGM 
01095 *       INITIMG(WS-HEX-00)                                        GA1GPGM 
01096 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1GPGM 
01097 *    END-EXEC.                                                    GA1GPGM 
01098                                                                   GA1GPGM 
01099      IF  FRMNUIDI  =  'GS3A'                                      GA1GPGM 
01100         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1GPGM 
01101         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1GPGM 
01102         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA1GPGM 
01103         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1GPGM 
01104         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1GPGM 
01105         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1GPGM 
01106         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1GPGM 
01107         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1GPGM 
01108                          GCIO-WRK-PROVIDER-CONTROL                GA1GPGM 
01109         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1GPGM 
01110         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1GPGM 
01111         MOVE SPACES TO GCA-BEN-PROV-ID                            GA1GPGM 
01112         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA1GPGM 
01113                          GCA-ALL-LEVEL-TAB-ID                     GA1GPGM 
01114         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA1GPGM 
01115                          GCA-ALL-LEVEL-TAB-SLOT                   GA1GPGM 
01116         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA1GPGM 
01117                        GCA-INTERNAL-TAB-ID                        GA1GPGM 
01118                        GCA-INTERNAL-TAB-SLOT                      GA1GPGM 
01119         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA1GPGM 
01120                                                                   GA1GPGM 
01121      IF  FRMNUIDI  =  'GC4A'                                      GA1GPGM 
01122         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1GPGM 
01123         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1GPGM 
01124         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1GPGM 
01125         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1GPGM 
01126         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1GPGM 
01127         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1GPGM 
01128         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1GPGM 
01129         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1GPGM 
01130         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1GPGM 
01131         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1GPGM 
01132         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1GPGM 
01133         MOVE SPACES TO GCA-BEN-PROV-ID                            GA1GPGM 
01134         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA1GPGM 
01135                          GCA-ALL-LEVEL-TAB-ID                     GA1GPGM 
01136         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA1GPGM 
01137                          GCA-ALL-LEVEL-TAB-SLOT                   GA1GPGM 
01138         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA1GPGM 
01139                        GCA-INTERNAL-TAB-ID                        GA1GPGM 
01140                        GCA-INTERNAL-TAB-SLOT                      GA1GPGM 
01141         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA1GPGM 
01142                                                                   GA1GPGM 
01143      IF  FRMNUIDI  =  'GC8A'                                      GA1GPGM 
01144         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1GPGM 
01145         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1GPGM 
01146         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA1GPGM 
01147         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1GPGM 
01148         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1GPGM 
01149         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1GPGM 
01150         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1GPGM 
01151         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1GPGM 
01152         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1GPGM 
01153         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1GPGM 
01154         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1GPGM 
01155         MOVE GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID            GA1GPGM 
01156         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1GPGM 
01157         MOVE ALTABIDI TO GCIO-WRK-TAB-PROVISION-ID                GA1GPGM 
01158                          GCA-ALL-LEVEL-TAB-ID                     GA1GPGM 
01159         MOVE ALTBSLTI TO GCIO-WRK-TAB-PROV-SLOT-NO                GA1GPGM 
01160                          GCA-ALL-LEVEL-TAB-SLOT                   GA1GPGM 
01161         MOVE SPACES TO GCA-INTERNAL-TAB-ID                        GA1GPGM 
01162                        GCA-INTERNAL-TAB-SLOT.                     GA1GPGM 
01163                                                                   GA1GPGM 
01164      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA1GPGM 
01165 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA1GPGM 
01166 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA1GPGM 
01167 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA1GPGM 
01168 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA1GPGM 
01169 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA1GPGM 
01170      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1GPGM 
01171                                                                   GA1GPGM 
01172      SET GCA-RECORD-POINTER                                       GA1GPGM 
01173          TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                  GA1GPGM 
01174                                                                   GA1GPGM 
01175      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO                       GA1GPGM 
01176           GAA-ENTRY-COUNT.                                        GA1GPGM 
01177                                                                   GA1GPGM 
01178      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1GPGM 
01179                                                                   GA1GPGM 
01180      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1GPGM 
01181         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA1GPGM 
01182         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA1GPGM 
01183                                                                   GA1GPGM 
01184      IF  NOT GCIO2-GOOD-RETURN                                    GA1GPGM 
01185         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1GPGM 
01186 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1GPGM 
01187         MOVE '1GF4'  TO  WS-ABEND-CODE                            GA1GPGM 
01188         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1GPGM 
01189                                                                   GA1GPGM 
01190 *    SET  COMMAREA-PNTR                                           GA1GPGM 
01191 *         TO   ADDRESS  OF  GCA-COMMAREA.                         GA1GPGM 
01192                                                                   GA1GPGM 
01193      IF  ALTBFNCI  =  'GA1B'                                      GA1GPGM 
01194 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA1GPGM 
01195 *          LENGTH(4) END-EXEC.                                    GA1GPGM 
01196         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA1GPGM 
01197                         COMMAREA(DFHCOMMAREA)                     GA1GPGM 
01198                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1GPGM 
01199         END-EXEC.                                                 GA1GPGM 
01200                                                                   GA1GPGM 
01201      IF  ALTBFNCI  =  'GA1C'                                      GA1GPGM 
01202 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA1GPGM 
01203 *          LENGTH(4) END-EXEC.                                    GA1GPGM 
01204         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA1GPGM 
01205                         COMMAREA(DFHCOMMAREA)                     GA1GPGM 
01206                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1GPGM 
01207         END-EXEC.                                                 GA1GPGM 
01208                                                                   GA1GPGM 
01209      IF  ALTBFNCI  =  'GA1D'                                      GA1GPGM 
01210 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA1GPGM 
01211 *          LENGTH(4) END-EXEC.                                    GA1GPGM 
01212         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA1GPGM 
01213                         COMMAREA(DFHCOMMAREA)                     GA1GPGM 
01214                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1GPGM 
01215         END-EXEC.                                                 GA1GPGM 
01216                                                                   GA1GPGM 
01217      IF  ALTBFNCI  =  'GA1E'                                      GA1GPGM 
01218 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA1GPGM 
01219 *          LENGTH(4) END-EXEC.                                    GA1GPGM 
01220         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA1GPGM 
01221                         COMMAREA(DFHCOMMAREA)                     GA1GPGM 
01222                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1GPGM 
01223         END-EXEC.                                                 GA1GPGM 
01224                                                                   GA1GPGM 
01225      IF  ALTBFNCI  =  'GA1P'                                      GA1GPGM 
01226         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA1GPGM 
01227                         COMMAREA(DFHCOMMAREA)                     GA1GPGM 
01228                         LENGTH (LENGTH OF DFHCOMMAREA)            GA1GPGM 
01229         END-EXEC.                                                 GA1GPGM 
01230                                                                   GA1GPGM 
01231  5099-EXIT.                                                       GA1GPGM 
01232      EXIT.                                                        GA1GPGM 
01233 /**************************************************************** GA1GPGM 
01234 **           X C T L   T O   M A I N   M E N U                    GA1GPGM 
01235 **                                                                GA1GPGM 
01236 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1GPGM 
01237 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1GPGM 
01238 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1GPGM 
01239 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1GPGM 
01240 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1GPGM 
01241 ** AND PROGRESS DOWN.                                             GA1GPGM 
01242 ******************************************************************GA1GPGM 
01243  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1GPGM 
01244      MOVE '6000'  TO  WS-PARA-ID.                                 GA1GPGM 
01245      MOVE '1GP1'  TO  WS-ABEND-CODE.                              GA1GPGM 
01246                                                                   GA1GPGM 
01247      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1GPGM 
01248                                                                   GA1GPGM 
01249  6099-EXIT.     EXIT.                                             GA1GPGM 
01250      EJECT                                                        GA1GPGM 
01251 /*****************************************************************GA1GPGM 
01252 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA1GPGM 
01253 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA1GPGM 
01254 ******************************************************************GA1GPGM 
01255  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA1GPGM 
01256  9800-010.                                                        GA1GPGM 
01257                                                                   GA1GPGM 
01258      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1GPGM 
01259      MOVE 'M'   TO  HGADATE-FORM1.                                GA1GPGM 
01260      MOVE 'J'   TO  HGADATE-FORM2.                                GA1GPGM 
01261      MOVE ZEROS TO  HGADATE-RETURN                                GA1GPGM 
01262                     HGADATE-AMOUNT.                               GA1GPGM 
01263      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1GPGM 
01264                     COMMAREA(HGADATES-COMMAREA)                   GA1GPGM 
01265                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1GPGM 
01266                     END-EXEC.                                     GA1GPGM 
01267                                                                   GA1GPGM 
01268  9800-900-900-EXIT.                                               GA1GPGM 
01269      EXIT.                                                        GA1GPGM 
01270 /*****************************************************************GA1GPGM 
01271 * 9810    J U L I A N    T O    G R E G O R I A N                *GA1GPGM 
01272 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *GA1GPGM 
01273 ******************************************************************GA1GPGM 
01274  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          GA1GPGM 
01275  9810-010.                                                        GA1GPGM 
01276                                                                   GA1GPGM 
01277      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1GPGM 
01278      MOVE 'J'   TO  HGADATE-FORM1.                                GA1GPGM 
01279      MOVE 'M'   TO  HGADATE-FORM2.                                GA1GPGM 
01280      MOVE ZEROS TO  HGADATE-RETURN                                GA1GPGM 
01281                     HGADATE-AMOUNT.                               GA1GPGM 
01282      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1GPGM 
01283                     COMMAREA(HGADATES-COMMAREA)                   GA1GPGM 
01284                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1GPGM 
01285                     END-EXEC.                                     GA1GPGM 
01286                                                                   GA1GPGM 
01287  9810-900-900-EXIT.                                               GA1GPGM 
01288      EXIT.                                                        GA1GPGM 
01289  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1GPGM 
01290                                                                   GA1GPGM 
01291      SET MAP-IDX1 TO 7.                                           GA1GPGM 
01292      SET MAP-IDX2 TO 1.                                           GA1GPGM 
01293      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1GPGM 
01294                                                                   GA1GPGM 
01295      EXEC CICS SEND   MAP('GA1GI01') MAPSET('GA1GSET') ERASE      GA1GPGM 
01296         FROM(GA1GI01O) WAIT END-EXEC.                             GA1GPGM 
01297                                                                   GA1GPGM 
01298      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1GPGM 
01299                                                                   GA1GPGM 
01300  9999-EXIT.     EXIT.                                             GA1GPGM 
