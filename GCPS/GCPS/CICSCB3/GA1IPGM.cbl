00001 *      LAST MAINTENANCE TIME:  8.45.43  DATE: 11/16/84            08/20/03
00002  IDENTIFICATION DIVISION.                                         GA1IPGM 
00003  PROGRAM-ID.     GA1IPGM.                                            LV001
00004 **** THIS IS A COBOL/2 PROGRAM *****                              GA1IPGM 
00005  AUTHOR.         S BUCH.                                          GA1IPGM 
00006  DATE-WRITTEN.   11/13/84.                                        GA1IPGM 
00007  DATE-COMPILED.                                                   GA1IPGM 
00008      SKIP3                                                        GA1IPGM 
00009 ******************************************************************GA1IPGM 
00010 *   GA1IPGM   ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM      GA1IPGM 
00011 *                 PROVIDER-GROUP BY PROVIDER TYPES - GA1I         GA1IPGM 
00012 *                                                                 GA1IPGM 
00013 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1IPGM 
00014 *   CURRENTLY ON THE ALL LEVEL INTERNAL TABULAR RECORD.           GA1IPGM 
00015 *                                                                 GA1IPGM 
00016 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1IPGM 
00017 *   ALL LEVEL INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN    GA1IPGM 
00018 *   DECIDE IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN     GA1IPGM 
00019 *   ENTRY WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE    GA1IPGM 
00020 *   RECORD WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED   GA1IPGM 
00021 *   BACK INTO THE RECORD BEFORE UPDATING THE RECORD.              GA1IPGM 
00022 *                                                                 GA1IPGM 
00023 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #IPGT) GA1IPGM 
00024 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1IPGM 
00025 *   XCTL TO TRANS GA2I OR PROGRAM GA2IPGM.  THIS PROGRAM WILL     GA1IPGM 
00026 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1IPGM 
00027 *   TABLE.                                                        GA1IPGM 
00028 *                                                                 GA1IPGM 
00029 *   FUNC CODE: GA1I                                               GA1IPGM 
00030 *   MAPSET:    GA1ISETC                                           GA1IPGM 
00031 *   FILES:     GCPSWORK                                           GA1IPGM 
00032 *                                                                 GA1IPGM 
00033 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00034 *                                                                 GA1IPGM 
00035 *    TAILORING INSTRUCTIONS:                                      GA1IPGM 
00036 *                                                                 GA1IPGM 
00037 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1IPGM 
00038 *                                                                 GA1IPGM 
00039 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1I/     GA1IPGM 
00040 *              SCREEN PAGE NUMBER                 /009I/001I/     GA1IPGM 
00041 *              ADD PROGRAM FUNCTION CODE          /GCAI/GA2I/     GA1IPGM 
00042 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IPGT/    GA1IPGM 
00043 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX3/       GA1IPGM 
00044 *                                                                 GA1IPGM 
00045 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1IPGM 
00046 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1IPGM 
00047 *     OCCURANCES.                                                 GA1IPGM 
00048 *                                                                 GA1IPGM 
00049 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1IPGM 
00050 *     THAT MUST BE CHANGED.                                       GA1IPGM 
00051 *                                                                 GA1IPGM 
00052 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00053      SKIP3                                                        GA1IPGM 
00054 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1IPGM 
00055 *   DATE    PROGRAMMER  MAINTENANCE                             * GA1IPGM 
00056 * --------  ----------  --------------------------------------- * GA1IPGM 
00057 * 11-19-85      LET     REMOVED ALL HANDLE CONDITIONS EXCEPT    * GA1IPGM 
00058 *                       FOR MAPFAIL.                            * GA1IPGM 
00059 *                                                               * GA1IPGM 
00060 *03/16/87       JLA     CHANGES FOR SINGLE TABULAR SUPPORT THAT * GA1IPGM 
00061 * (D0120)               ARE EXECUTED FROM TRANSACTION GTM1:     * GA1IPGM 
00062 *                       1. PF1/PF13 - CONSTRUCT COMMAREA AS IF  * GA1IPGM 
00063 *                          GC4A HAD CALLED, XCTL TO ADD SCREEN  * GA1IPGM 
00064 *                          PROGRAM.                             * GA1IPGM 
00065 *                       2. PF3/PF15 - XCTL TO GTM1PGM WITHOUT   * GA1IPGM 
00066 *                          PASSING ANY COMMAREA.                * GA1IPGM 
00067 *                                                               * GA1IPGM 
00068 * 8/17/87       FRY     CAPTURE OPERATOR-ID WHEN A 'C3', 'C6',  * GA1IPGM 
00069 * (D116)                OR 'G4' RECORD IS UPDATED.              * GA1IPGM 
00070 *                                                               * GA1IPGM 
00071 * 11/30/89      NGE     FIX INCL/EXCL IND MISSING FROM SCREEN.  * GA1IPGM 
00072 * (R1681)                                                       * GA1IPGM 
00073 *                      ----ACCUM TABULAR MODIFICATIONS ----     * GA1IPGM 
00074 * 11154 12/27/90  NGE  1. EXPAND OCCUR LENGTH FROM 132 TO 176.  * GA1IPGM 
00075 *                      2. INCREASE MAX OCCURS FROM 29 TO 46.    * GA1IPGM 
00076 *                      3. EXPAND DEFINITION FIELD TO 2 BYTES.   * GA1IPGM 
00077 *                      4. ADD AGE-LIMIT FIELDS.                 * GA1IPGM 
00078 *                      5. ADD RELATIONSHIP -IND FIELD.          * GA1IPGM 
00079 *                      6. INCREASE MAX REC LENGTH FOR ACCUM REC * GA1IPGM 
00080 *                         TO 8157.                              * GA1IPGM 
00081 *                      7. >>>> CONVERT TO COBOL/2 <<<<          * GA1IPGM 
00082 *                                                               * GA1IPGM 
00083 *                                                               * GA1IPGM 
00084 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION FIELD     * GA1IPGM 
00085 *                           FROM ONE POSITION TO TWO POSITIONS. * GA1IPGM 
00086 *                                                               * GA1IPGM 
00087 *14726/ 11/11/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000     * GA1IPGM 
00088 *15057                  AND THE EXPANSION OF THE GROUP SPECIFIC * GA1IPGM 
00089 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS   * GA1IPGM 
00090 *                       MERGER.                                 * GA1IPGM 
00091 *                                                               * GA1IPGM 
00092 *                                                               * GA1IPGM 
00093 * 14726/  05/15/98  AB   EXPANDED THE SCREEN / MAP              * GA1IPGM 
00094 * 15057                  TO INCLUDE THE ENTIRE KEY              * GA1IPGM 
00095 *                                                               * GA1IPGM 
00096 *  D341   10/07/98  GDM  1. XCTL TO NEW ACCUM TABULAR #ACP      * GA1IPGM 
00097 *                        2. ADD DELADD-OPTION = 'GAS5UPD'       * GA1IPGM 
00098 *                                                               * GA1IPGM 
00099 * P????   11/19/99  FRY  ADD LENGTH PARAMETER TO THE RETURN     * GA1IPGM 
00100 *                        COMMAND WHEN DFHCOMMAREA IS SPECIFIED. * GA1IPGM 
00101 *                                                               * GA1IPGM 
00102 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA1IPGM 
SI0724*                                                                *00030141
SI0724* P56703     05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION  *00030150
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00030160
SI0724*                           GCCDRLEN                             *00030170
00103 *                                                               * GA1IPGM 
00104 *                                                               * GA1IPGM 
00105 *                                                               * GA1IPGM 
00106 *                                                               * GA1IPGM 
00107 *                                                               * GA1IPGM 
00108 *                                                               * GA1IPGM 
00109 ***************************************************************** GA1IPGM 
00110 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1IPGM 
00111 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1IPGM 
00112      EJECT                                                        GA1IPGM 
00113  ENVIRONMENT DIVISION.                                            GA1IPGM 
00114      EJECT                                                        GA1IPGM 
00115  DATA DIVISION.                                                   GA1IPGM 
00116  WORKING-STORAGE SECTION.                                         GA1IPGM 
00117  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1IPGM 
00118      '***GA1IPGM WS BEGINS***'.                                   GA1IPGM 
00119  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1IPGM 
00120                                                                   GA1IPGM 
00121  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1IPGM 
00122                                                                   GA1IPGM 
00123  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1IPGM 
00124  01  COMMAREA-POINTER-AREA.                                       GA1IPGM 
00125      05  COMMAREA-PNTR-COMP          PIC S9(8)  COMP.             GA1IPGM 
00126      05  COMMAREA-PNTR  REDEFINES                                 GA1IPGM 
00127          COMMAREA-PNTR-COMP          USAGE IS POINTER.            GA1IPGM 
00128                                                                   GA1IPGM 
00129 ** MAP COBOL SCREEN DSECTS **                                     GA1IPGM 
00130  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1IPGM 
00131      '***  I/O MAPAREA ***'.                                      GA1IPGM 
00132  COPY GA1ISETC.                                                   GA1IPGM 
00133      EJECT                                                        GA1IPGM 
00134 ******************************************************************GA1IPGM 
00135 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA1IPGM 
00136 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1IPGM 
00137 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1IPGM 
00138 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1IPGM 
00139 **  REDEFINES.                                                    GA1IPGM 
00140 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00141 **                                                                GA1IPGM 
00142 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1IPGM 
00143 **  FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1IPGM 
00144 **  MATCH THE MAP.                                                GA1IPGM 
00145 **                                                                GA1IPGM 
00146 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00147                                                                   GA1IPGM 
00148  01  FILLER     REDEFINES   GA1II01I.                             GA1IPGM 
00149      05  FILLER                              PIC X(83).           GA1IPGM 
00150      05  GROUP-SPECIFIC-ID-LINE.                                  GA1IPGM 
00151          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA1IPGM 
00152          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA1IPGM 
00153          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA1IPGM 
00154          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA1IPGM 
00155          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA1IPGM 
00156          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA1IPGM 
00157          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA1IPGM 
00158          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA1IPGM 
00159          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA1IPGM 
00160          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA1IPGM 
00161          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA1IPGM 
00162          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA1IPGM 
00163          10  FILLER                          PIC X(16).           GA1IPGM 
00164      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA1IPGM 
00165          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1IPGM 
00166          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1IPGM 
00167          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1IPGM 
00168          10  CONTRACT-GROUP-NO               PIC X(9).            GA1IPGM 
00169          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1IPGM 
00170          10  CONTRACT-SECTION-NO             PIC X(5).            GA1IPGM 
00171          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1IPGM 
00172          10  CONTRACT-PKG-CODE               PIC X(3).            GA1IPGM 
00173          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1IPGM 
00174          10  CONTRACT-LOB                    PIC X.               GA1IPGM 
00175          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1IPGM 
00176          10  CONTRACT-PROV-CTL               PIC XX.              GA1IPGM 
00177          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1IPGM 
00178          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1IPGM 
00179          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1IPGM 
00180          10  CONTRACT-EFF-DATE               PIC X(6).            GA1IPGM 
00181          10  FILLER                          PIC X(1).            GA1IPGM 
00182      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA1IPGM 
00183                                     GROUP-SPECIFIC-ID-LINE.       GA1IPGM 
00184          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA1IPGM 
00185          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA1IPGM 
00186          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA1IPGM 
00187          10  BEN-PROV-GROUP-NO               PIC X(9).            GA1IPGM 
00188          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA1IPGM 
00189          10  BEN-PROV-SECTION-NO             PIC X(5).            GA1IPGM 
00190          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA1IPGM 
00191          10  BEN-PROV-PKG-CODE               PIC X(3).            GA1IPGM 
00192          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA1IPGM 
00193          10  BEN-PROV-LOB                    PIC X.               GA1IPGM 
00194          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA1IPGM 
00195          10  BEN-PROV-PROV-CTL               PIC XX.              GA1IPGM 
00196          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA1IPGM 
00197          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA1IPGM 
00198          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA1IPGM 
00199          10  BEN-PROV-EFF-DATE               PIC X(6).            GA1IPGM 
00200          10  BEN-PROV-ID-HEADING             PIC X(6).            GA1IPGM 
00201          10  BEN-PROV-ID-NO                  PIC X(6).            GA1IPGM 
00202          10  FILLER                          PIC X(4).            GA1IPGM 
00203      05  FILLER                              PIC X(74).           GA1IPGM 
00204      05  MAP-PROVIDER-TYP-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED   GA1IPGM 
00205          BY MAP-IDX1.                                             GA1IPGM 
00206        10  MAP-PROVIDER-TYP-ARGUMENT-COL  OCCURS 3 TIMES INDEXED  GA1IPGM 
00207            BY MAP-IDX2.                                           GA1IPGM 
00208          15  MAP-ACTION-CODE-LEN             PIC S9(4) COMP SYNC. GA1IPGM 
00209          15  MAP-ACTION-CODE-ATTR            PIC X.               GA1IPGM 
00210          15  MAP-ACTION-CODE                 PIC X.               GA1IPGM 
00211          15  MAP-PROVIDER-TYP-ARGUMENT-LEN   PIC S9(4) COMP SYNC. GA1IPGM 
00212          15  MAP-PROVIDER-TYP-ARGUMENT-ATTR  PIC X.               GA1IPGM 
00213          15  MAP-PROVIDER-TYP-ARGUMENT       PIC X(2).            GA1IPGM 
00214 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00215  01  WS-MAP-OCCURS-COUNTERS.                                      GA1IPGM 
00216 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00217 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1IPGM 
00218 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00219      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +14.     GA1IPGM 
00220      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +3.      GA1IPGM 
00221 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00222      EJECT                                                        GA1IPGM 
00223 ** ALTERNATIVE WORKFILE KEYS **                                   GA1IPGM 
00224  01  FILLER                      PIC X(32)  VALUE                 GA1IPGM 
00225      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1IPGM 
00226  01  WS-ALT-WORKFILE-KEYS.                                        GA1IPGM 
00227  COPY GCWRKKEY.                                                   GA1IPGM 
00228 /                                                                 GA1IPGM 
00229 ** DATE FORMATTING AREA **                                        GA1IPGM 
00230  01  HGADATES-COMMAREA.                                           GA1IPGM 
00231  COPY HGCDAT01.                                                   GA1IPGM 
00232 /                                                                 GA1IPGM 
00233 ** WORKFIELDS, AND SWITCHES **                                    GA1IPGM 
00234  01  WS-WORK-FIELDS.                                              GA1IPGM 
00235      05  WS-HEX-00                     PIC X.                     GA1IPGM 
00236      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1IPGM 
00237 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00238 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00239 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00240      05  WS-SAVED-FIELDS.                                         GA1IPGM 
00241        10  WS-SAVED-PROVIDER-TYP-ARGUMENT                         GA1IPGM 
00242                                        PIC X(2).                  GA1IPGM 
00243 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00244  01  WS-SWITCHES.                                                 GA1IPGM 
00245      05  WS-ERROR-SW                   PIC X.                     GA1IPGM 
00246                                                                   GA1IPGM 
00247 ** TITLE LINES **                                                 GA1IPGM 
00248  01  WS-TITLE-LINES.                                              GA1IPGM 
00249      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA1IPGM 
00250          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA1IPGM 
00251      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA1IPGM 
00252          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA1IPGM 
00253      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA1IPGM 
00254          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA1IPGM 
00255                                                                   GA1IPGM 
00256      EJECT                                                        GA1IPGM 
00257 ** ATTRIBUTES **                                                  GA1IPGM 
00258  COPY DFHBMSCA.                                                   GA1IPGM 
00259      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1IPGM 
00260      EJECT                                                        GA1IPGM 
00261 ** ATTENTION IDENTIFIERS **                                       GA1IPGM 
00262  COPY DFHAID.                                                     GA1IPGM 
00263      EJECT                                                        GA1IPGM 
00264 ** RECORD LENGTHS **                                              GA1IPGM 
00265  01  WS-RECORD-LENGTHS.                                           GA1IPGM 
00266     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA1IPGM 
00267     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA1IPGM 
00268     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1IPGM 
00269     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1IPGM 
00270 *                                                                 GA1IPGM 
00271 /-------- GENERIC CONTRACT GLOBALLY DEFINED LENGTHS ...ETC ------*GA1IPGM 
00272  01  FILLER.                                                      GA1IPGM 
00273  COPY GCCDRLEN.                                                   GA1IPGM 
00274                                                                   GA1IPGM 
00275  01  WS-END                      PIC X(16)  VALUE                 GA1IPGM 
00276      '*** W/S ENDS ***'.                                          GA1IPGM 
00277 /                                                                 GA1IPGM 
00278  LINKAGE SECTION.                                                 GA1IPGM 
00279  01  DFHCOMMAREA.                                                 GA1IPGM 
00280  COPY G2ALCKEC.                                                   GA1IPGM 
00281  COPY GACDACWA.                                                   GA1IPGM 
00282 *    05  INCOMING-COMMAREA-PNTR   USAGE IS  POINTER.              GA1IPGM 
00283      05  GAS1UPD-PASSED-AREA.                                     GA1IPGM 
00284          07  LVL2-B-SW           PIC X.                           GA1IPGM 
00285          07  LVL2-F-SW           PIC X.                           GA1IPGM 
00286          07  LVL2-G-SW           PIC X.                           GA1IPGM 
00287          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1IPGM 
00288          07  FILLER              PIC X(9).                        GA1IPGM 
00289      05  DELADD-OPTION           PIC X(7).                        GA1IPGM 
00290                                                                   GA1IPGM 
00291 *01  GCA-COMMAREA.                                                GA1IPGM 
00292 *COPY G2ALCKEC.                                                   GA1IPGM 
00293 /                                                                 GA1IPGM 
00294  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA1IPGM 
00295  COPY GCIOPRM1.                                                   GA1IPGM 
00296      SKIP3                                                        GA1IPGM 
00297  COPY GCWRKDCC.                                                   GA1IPGM 
00298      SKIP3                                                        GA1IPGM 
00299  COPY GCTIPGTC.                                                   GA1IPGM 
00300      EJECT                                                        GA1IPGM 
00301 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00302 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00303 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00304  01  COPY-TABULAR-TABLE-AREA.                                     GA1IPGM 
00305      05  COPY-TABULAR-TABLE  OCCURS 1979 TIMES INDEXED BY         GA1IPGM 
00306            COPY-IDX.                                              GA1IPGM 
00307        10  COPY-PROVIDER-TYP-ARGUMENT         PIC X(2).           GA1IPGM 
00308 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00309      EJECT                                                        GA1IPGM 
00310  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA1IPGM 
00311  COPY GCIOPRM2.                                                   GA1IPGM 
00312      EJECT                                                        GA1IPGM 
00313  COPY GCWRKDC2.                                                   GA1IPGM 
00314      EJECT                                                        GA1IPGM 
00315  COPY GCTABMC.                                                    GA1IPGM 
00316      EJECT                                                        GA1IPGM 
00317                                                                   GA1IPGM 
00318  PROCEDURE DIVISION.                                              GA1IPGM 
00319                                                                   GA1IPGM 
00320 ******************************************************************GA1IPGM 
00321 **               H O U S E K E E P I N G                          GA1IPGM 
00322 **                                                                GA1IPGM 
00323 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA1IPGM 
00324 **                                                                GA1IPGM 
00325 ******************************************************************GA1IPGM 
00326  0000-HOUSEKEEPING   SECTION.                                     GA1IPGM 
00327                                                                   GA1IPGM 
00328      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1IPGM 
00329                                                                   GA1IPGM 
00330      IF EIBAID  =  DFHCLEAR                                       GA1IPGM 
00331          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1IPGM 
00332                         ERASE                                     GA1IPGM 
00333          END-EXEC                                                 GA1IPGM 
00334          EXEC CICS RETURN                                         GA1IPGM 
00335          END-EXEC.                                                GA1IPGM 
00336                                                                   GA1IPGM 
00337      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1IPGM 
00338                                   END-EXEC.                       GA1IPGM 
00339  0000-EXIT.     EXIT.                                             GA1IPGM 
00340 ******************************************************************GA1IPGM 
00341 **                     M A I N L I N E                            GA1IPGM 
00342 **                                                                GA1IPGM 
00343 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1IPGM 
00344 **  TAKEN BY THE OPERATOR.                                        GA1IPGM 
00345 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1IPGM 
00346 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1IPGM 
00347 **     ADDITIONS FROM.                                            GA1IPGM 
00348 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1IPGM 
00349 **     KEY PF12 OR PF24.                                          GA1IPGM 
00350 **  3. RECEIVE THE SCREEN.                                        GA1IPGM 
00351 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1IPGM 
00352 **     MENU.                                                      GA1IPGM 
00353 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1IPGM 
00354 **     LOGIC.                                                     GA1IPGM 
00355 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1IPGM 
00356 **     (RETURN) TO THE ADD PROGRAM (GA2IPGM).                     GA1IPGM 
00357 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1IPGM 
00358 **     (RETURN) TO THE PREVIOUS MENU.                             GA1IPGM 
00359 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1IPGM 
00360 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1IPGM 
00361 **                                                                GA1IPGM 
00362 ******************************************************************GA1IPGM 
00363  1000-MAIN-LINE.                                                  GA1IPGM 
00364                                                                   GA1IPGM 
00365      MOVE '1000'  TO  WS-PARA-ID.                                 GA1IPGM 
00366      IF EIBTRNID  NOT =  'GA1I'                                   GA1IPGM 
00367         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1IPGM 
00368         GO TO 1099-RETURN.                                        GA1IPGM 
00369                                                                   GA1IPGM 
00370      EXEC CICS RECEIVE   MAP('GA1II01') MAPSET('GA1ISET')         GA1IPGM 
00371         INTO(GA1II01I) END-EXEC.                                  GA1IPGM 
00372                                                                   GA1IPGM 
00373      IF SCRNIDNI  NOT =  '001I00'                                 GA1IPGM 
00374         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1IPGM 
00375                                                                   GA1IPGM 
00376      IF EIBAID  =  DFHENTER                                       GA1IPGM 
00377         PERFORM 2000-DELETE-PROCESSING                            GA1IPGM 
00378         GO TO 1099-RETURN.                                        GA1IPGM 
00379                                                                   GA1IPGM 
00380      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1IPGM 
00381         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1IPGM 
00382                                                                   GA1IPGM 
00383      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1IPGM 
00384         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1IPGM 
00385                                                                   GA1IPGM 
00386      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1IPGM 
00387      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1IPGM 
00388      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1IPGM 
00389 -    'THIS PROGRAM ***'  TO  ERRMSGO.                             GA1IPGM 
00390      EXEC CICS SEND   MAP('GA1II01') MAPSET('GA1ISET') DATAONLY   GA1IPGM 
00391         FROM(GA1II01O) CURSOR END-EXEC.                           GA1IPGM 
00392                                                                   GA1IPGM 
00393  1099-RETURN.                                                     GA1IPGM 
00394      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA1IPGM 
00395         (DELADD-OPTION = 'GAS1UPD') OR                            GA1IPGM 
00396         (DELADD-OPTION = 'GAS2UPD') OR                            GA1IPGM 
00397         (DELADD-OPTION = 'GAS3UPD') OR                            GA1IPGM 
00398         (DELADD-OPTION = 'GAS4UPD') OR                            GA1IPGM 
00399         (DELADD-OPTION = 'GAS5UPD')                               GA1IPGM 
00400          EXEC CICS RETURN   END-EXEC                              GA1IPGM 
00401      ELSE                                                         GA1IPGM 
00402          EXEC CICS RETURN TRANSID('GA1I')                         GA1IPGM 
00403                    COMMAREA(DFHCOMMAREA)                          GA1IPGM 
00404                    LENGTH  (EIBCALEN)                             GA1IPGM 
00405                    END-EXEC.                                      GA1IPGM 
00406      GOBACK.                                                      GA1IPGM 
00407                                                                   GA1IPGM 
00408  1999-EXIT.     EXIT.                                             GA1IPGM 
00409 /*****************************************************************GA1IPGM 
00410 **              D E L E T E   P R O C E S S I N G                 GA1IPGM 
00411 **                                                                GA1IPGM 
00412 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1IPGM 
00413 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1IPGM 
00414 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1IPGM 
00415 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1IPGM 
00416 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1IPGM 
00417 **    BACK INTO THE RECORD THAT WE READ.)                         GA1IPGM 
00418 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1IPGM 
00419 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1IPGM 
00420 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1IPGM 
00421 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1IPGM 
00422 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1IPGM 
00423 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1IPGM 
00424 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1IPGM 
00425 **    ENTRY.                                                      GA1IPGM 
00426 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1IPGM 
00427 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1IPGM 
00428 **    RECORD.                                                     GA1IPGM 
00429 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1IPGM 
00430 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1IPGM 
00431 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1IPGM 
00432 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1IPGM 
00433 **    ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1IPGM 
00434 **    SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1IPGM 
00435 **    BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1IPGM 
00436 **                                                                GA1IPGM 
00437 ******************************************************************GA1IPGM 
00438  2000-DELETE-PROCESSING SECTION.                                  GA1IPGM 
00439                                                                   GA1IPGM 
00440      MOVE '2000'  TO  WS-PARA-ID.                                 GA1IPGM 
00441      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1IPGM 
00442      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1IPGM 
00443      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1IPGM 
00444                                                                   GA1IPGM 
00445      MOVE '2010'  TO  WS-PARA-ID.                                 GA1IPGM 
00446  2010-VALIDATE-ACT-CODE.                                          GA1IPGM 
00447      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1IPGM 
00448         ADD 1  TO  WS-DELETE-COUNT.                               GA1IPGM 
00449      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D' OR           GA1IPGM 
00450         = SPACE OR =  LOW-VALUES                                  GA1IPGM 
00451         MOVE DFHBMUNF  TO                                         GA1IPGM 
00452            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1IPGM 
00453 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00454 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00455 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00456         MOVE DFHBMASF  TO                                         GA1IPGM 
00457            MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA1IPGM 
00458      ELSE                                                         GA1IPGM 
00459         MOVE DFHBMUBF  TO                                         GA1IPGM 
00460            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1IPGM 
00461         MOVE DFHBMABF  TO                                         GA1IPGM 
00462            MAP-PROVIDER-TYP-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA1IPGM 
00463 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00464         IF WS-ERROR-SW  NOT =  'Y'                                GA1IPGM 
00465            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1IPGM 
00466            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1IPGM 
00467                                                                   GA1IPGM 
00468      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1IPGM 
00469         SET MAP-IDX1   UP BY  1                                   GA1IPGM 
00470      ELSE                                                         GA1IPGM 
00471         IF MAP-IDX2  <  WS-MAP-COL                                GA1IPGM 
00472            SET MAP-IDX1  TO  1                                    GA1IPGM 
00473            SET MAP-IDX2  UP BY  1                                 GA1IPGM 
00474         ELSE                                                      GA1IPGM 
00475            GO TO 2020-DONE-VALIDATE-A-C.                          GA1IPGM 
00476 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00477 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00478 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00479      IF MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)            GA1IPGM 
00480         NOT =  LOW-VALUES                                         GA1IPGM 
00481 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00482         GO TO 2010-VALIDATE-ACT-CODE.                             GA1IPGM 
00483                                                                   GA1IPGM 
00484  2020-DONE-VALIDATE-A-C.                                          GA1IPGM 
00485      MOVE '2020'  TO  WS-PARA-ID.                                 GA1IPGM 
00486      SET MAP-IDX1   TO  1.                                        GA1IPGM 
00487                                                                   GA1IPGM 
00488      IF WS-ERROR-SW  =  'Y'                                       GA1IPGM 
00489         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1IPGM 
00490            ERRMSGO                                                GA1IPGM 
00491         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1IPGM 
00492            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA1IPGM 
00493            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA1IPGM 
00494 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00495 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1IPGM 
00496 **  ADD ITS MAP FIELD NAME HERE.                                  GA1IPGM 
00497 ****************************************************************  GA1IPGM 
00498            INCEXCO                                                GA1IPGM 
00499         MOVE '2100'  TO  WS-PARA-ID                               GA1IPGM 
00500         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1IPGM 
00501            VARYING MAP-IDX2 FROM  1  BY  1                        GA1IPGM 
00502                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1IPGM 
00503              AFTER MAP-IDX1 FROM  1  BY  1                        GA1IPGM 
00504                             UNTIL MAP-IDX1  >  WS-MAP-ROW         GA1IPGM 
00505         EXEC CICS SEND   MAP('GA1II01') MAPSET('GA1ISET') DATAONLYGA1IPGM 
00506            FROM(GA1II01O) CURSOR END-EXEC                         GA1IPGM 
00507         GO TO 2099-EXIT.                                          GA1IPGM 
00508                                                                   GA1IPGM 
00509      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1IPGM 
00510               GC-GCIOPARM-LEN                 +                   GA1IPGM 
00511               GC-WORKFILE-KEY-LEN             +                   GA1IPGM 
00512               GC-GCTABULR-IPGT-FIXED-LEN      +                   GA1IPGM 
00513              (GC-GCTABULR-IPGT-VARY-LEN       *                   GA1IPGM 
00514               GC-GCTABULR-IPGT-VARY-MAX-OCUR).                    GA1IPGM 
00515                                                                   GA1IPGM 
00516      EXEC CICS                                                    GA1IPGM 
00517         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA1IPGM 
00518         INITIMG(WS-HEX-00)                                        GA1IPGM 
00519         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA1IPGM 
00520      END-EXEC.                                                    GA1IPGM 
00521                                                                   GA1IPGM 
00522      IF  FRMNUIDI  =  'GS3A'                                      GA1IPGM 
00523         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1IPGM 
00524         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1IPGM 
00525         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA1IPGM 
00526         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1IPGM 
00527 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1IPGM 
00528         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1IPGM 
00529 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1IPGM 
00530         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1IPGM 
00531         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1IPGM 
00532         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1IPGM 
00533                          GCIO-WRK-PROVIDER-CONTROL                GA1IPGM 
00534         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1IPGM 
00535         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1IPGM 
00536                                                                   GA1IPGM 
00537      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1IPGM 
00538         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1IPGM 
00539         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1IPGM 
00540         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1IPGM 
00541         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1IPGM 
00542 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1IPGM 
00543         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1IPGM 
00544 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1IPGM 
00545         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1IPGM 
00546         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1IPGM 
00547         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1IPGM 
00548         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1IPGM 
00549         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1IPGM 
00550         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1IPGM 
00551                                                                   GA1IPGM 
00552      IF  FRMNUIDI  =  'GC8A'                                      GA1IPGM 
00553         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1IPGM 
00554         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1IPGM 
00555         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA1IPGM 
00556         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1IPGM 
00557 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1IPGM 
00558         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1IPGM 
00559 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1IPGM 
00560         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1IPGM 
00561         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1IPGM 
00562         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1IPGM 
00563         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1IPGM 
00564         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1IPGM 
00565         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1IPGM 
00566                                                                   GA1IPGM 
00567      MOVE  GC-GCPSWORK-DDNAME TO  GCIO-FILE-DDNAME.               GA1IPGM 
00568      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1IPGM 
00569      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA1IPGM 
00570      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA1IPGM 
00571      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA1IPGM 
00572      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA1IPGM 
00573      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1IPGM 
00574                                                                   GA1IPGM 
00575      IF WS-DELETE-COUNT  =  ZERO                                  GA1IPGM 
00576         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1IPGM 
00577                                                                   GA1IPGM 
00578 ******************************************************************GA1IPGM 
00579 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1IPGM 
00580 *                                                                 GA1IPGM 
00581 ******************************************************************GA1IPGM 
00582                                                                   GA1IPGM 
00583      MOVE  GC-GCTABULR-IPGT-VARY-MAX-OCUR                         GA1IPGM 
00584            TO  GX3-ENTRY-COUNT.                                   GA1IPGM 
00585                                                                   GA1IPGM 
00586      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1IPGM 
00587                                                                   GA1IPGM 
00588      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1IPGM 
00589         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1IPGM 
00590         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1IPGM 
00591                                                                   GA1IPGM 
00592      IF  NOT GCIO-GOOD-RETURN                                     GA1IPGM 
00593         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1IPGM 
00594 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1IPGM 
00595         MOVE '1IF1'  TO  WS-ABEND-CODE                            GA1IPGM 
00596         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1IPGM 
00597                                                                   GA1IPGM 
00598      COMPUTE WS-COPY-LENGTH  =                                    GA1IPGM 
00599              GX3-ENTRY-COUNT  *  GC-GCTABULR-IPGT-VARY-LEN.       GA1IPGM 
00600                                                                   GA1IPGM 
00601      EXEC CICS                                                    GA1IPGM 
00602         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA1IPGM 
00603         INITIMG(WS-HEX-00)                                        GA1IPGM 
00604         LENGTH(WS-COPY-LENGTH)                                    GA1IPGM 
00605      END-EXEC.                                                    GA1IPGM 
00606                                                                   GA1IPGM 
00607      MOVE GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT.                   GA1IPGM 
00608      SET COPY-IDX,  GX3-INDEX  TO  1.                             GA1IPGM 
00609                                                                   GA1IPGM 
00610      MOVE '2030'  TO  WS-PARA-ID.                                 GA1IPGM 
00611  2030-MAKE-A-COPY-OF-RECORD.                                      GA1IPGM 
00612      IF GX3-INDEX  NOT >  GX3-ENTRY-COUNT                         GA1IPGM 
00613         MOVE GX3-ENTRY (GX3-INDEX)  TO                            GA1IPGM 
00614            COPY-TABULAR-TABLE (COPY-IDX)                          GA1IPGM 
00615            SET COPY-IDX,  GX3-INDEX  UP BY  1                     GA1IPGM 
00616            GO TO 2030-MAKE-A-COPY-OF-RECORD.                      GA1IPGM 
00617      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GX3-INDEX  TO  1.         GA1IPGM 
00618                                                                   GA1IPGM 
00619      MOVE '2040'  TO  WS-PARA-ID.                                 GA1IPGM 
00620  2040-DELETE-MARKED-ENTRIES.                                      GA1IPGM 
00621 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00622 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00623 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00624      IF MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)  =         GA1IPGM 
00625            LOW-VALUES                                             GA1IPGM 
00626         GO TO 2060-SAVE-REST-OF-COPY.                             GA1IPGM 
00627                                                                   GA1IPGM 
00628      IF MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)  >         GA1IPGM 
00629         COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)                     GA1IPGM 
00630         GO TO 2050-SAVE-COPIED-ENTRY                              GA1IPGM 
00631      ELSE                                                         GA1IPGM 
00632         IF MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)  <      GA1IPGM 
00633            COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)                  GA1IPGM 
00634            MOVE '1IL1'  TO  WS-ABEND-CODE                         GA1IPGM 
00635            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1IPGM 
00636 -    'RM SYSTEMS AREA ***'  TO  ERRMSGO                           GA1IPGM 
00637            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1IPGM 
00638                                                                   GA1IPGM 
00639 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00640                                                                   GA1IPGM 
00641      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1IPGM 
00642         IF MAP-IDX1   <  WS-MAP-ROW                               GA1IPGM 
00643            SET MAP-IDX1   UP BY  1                                GA1IPGM 
00644            GO TO 2050-SAVE-COPIED-ENTRY                           GA1IPGM 
00645         ELSE                                                      GA1IPGM 
00646            IF MAP-IDX2  <  WS-MAP-COL                             GA1IPGM 
00647               SET MAP-IDX1  TO  1                                 GA1IPGM 
00648               SET MAP-IDX2  UP BY  1                              GA1IPGM 
00649               GO TO 2050-SAVE-COPIED-ENTRY                        GA1IPGM 
00650            ELSE                                                   GA1IPGM 
00651               GO TO 2060-SAVE-REST-OF-COPY.                       GA1IPGM 
00652                                                                   GA1IPGM 
00653      SET COPY-IDX  UP BY  1.                                      GA1IPGM 
00654      IF COPY-IDX  NOT <  GX3-ENTRY-COUNT                          GA1IPGM 
00655         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    GA1IPGM 
00656            GX3-ENTRY (GX3-INDEX)                                  GA1IPGM 
00657         SET  GX3-ENTRY-COUNT  TO  GX3-INDEX                       GA1IPGM 
00658         MOVE GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT                 GA1IPGM 
00659         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1IPGM 
00660      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1IPGM 
00661         SET MAP-IDX1   UP BY  1                                   GA1IPGM 
00662         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1IPGM 
00663      IF MAP-IDX2  <  WS-MAP-COL                                   GA1IPGM 
00664         SET MAP-IDX1  TO  1                                       GA1IPGM 
00665         SET MAP-IDX2  UP BY  1                                    GA1IPGM 
00666         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1IPGM 
00667      ELSE                                                         GA1IPGM 
00668         GO TO 2060-SAVE-REST-OF-COPY.                             GA1IPGM 
00669                                                                   GA1IPGM 
00670  2050-SAVE-COPIED-ENTRY.                                          GA1IPGM 
00671      MOVE '2050'  TO  WS-PARA-ID.                                 GA1IPGM 
00672      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1IPGM 
00673         GX3-ENTRY (GX3-INDEX).                                    GA1IPGM 
00674                                                                   GA1IPGM 
00675      SET GX3-INDEX  UP BY  1.                                     GA1IPGM 
00676      IF COPY-IDX  <  GX3-ENTRY-COUNT                              GA1IPGM 
00677         SET COPY-IDX  UP BY  1                                    GA1IPGM 
00678         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1IPGM 
00679      ELSE                                                         GA1IPGM 
00680 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1IPGM 
00681 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1IPGM 
00682 ***      THE TABLE OF ENTRIES.                                    GA1IPGM 
00683         MOVE '1IL2'  TO  WS-ABEND-CODE                            GA1IPGM 
00684         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1IPGM 
00685 -    'SYSTEMS AREA ***'  TO  ERRMSGO                              GA1IPGM 
00686         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1IPGM 
00687                                                                   GA1IPGM 
00688  2060-SAVE-REST-OF-COPY.                                          GA1IPGM 
00689      MOVE '2060'  TO  WS-PARA-ID.                                 GA1IPGM 
00690      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1IPGM 
00691         GX3-ENTRY (GX3-INDEX).                                    GA1IPGM 
00692                                                                   GA1IPGM 
00693      SET GX3-INDEX  UP BY  1.                                     GA1IPGM 
00694      IF COPY-IDX  <  GX3-ENTRY-COUNT                              GA1IPGM 
00695         SET COPY-IDX  UP BY  1                                    GA1IPGM 
00696         GO TO 2060-SAVE-REST-OF-COPY.                             GA1IPGM 
00697                                                                   GA1IPGM 
00698      SET GX3-INDEX  DOWN BY  1.                                   GA1IPGM 
00699      SET GX3-ENTRY-COUNT  TO  GX3-INDEX.                          GA1IPGM 
00700      MOVE GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT.                   GA1IPGM 
00701                                                                   GA1IPGM 
00702  2070-UPDATE-MODIFIED-REC.                                        GA1IPGM 
00703      MOVE '2070'  TO  WS-PARA-ID.                                 GA1IPGM 
00704                                                                   GA1IPGM 
00705 *-- SET INDICATOR TO CAPTURE OPERAOR-ID.                          GA1IPGM 
00706                                                                   GA1IPGM 
00707      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1IPGM 
00708      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1IPGM 
00709                                                                   GA1IPGM 
00710      COMPUTE  GCIO-RECORD-LENGTH              =                   GA1IPGM 
00711               GC-WORKFILE-KEY-LEN             +                   GA1IPGM 
00712               GC-GCTABULR-IPGT-FIXED-LEN      +                   GA1IPGM 
00713              (GC-GCTABULR-IPGT-VARY-LEN       *                   GA1IPGM 
00714               GX3-ENTRY-COUNT).                                   GA1IPGM 
00715                                                                   GA1IPGM 
00716      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1IPGM 
00717         GC-GCIOPARM-LEN     +  GCIO-RECORD-LENGTH.                GA1IPGM 
00718                                                                   GA1IPGM 
00719      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1IPGM 
00720         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1IPGM 
00721         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1IPGM 
00722                                                                   GA1IPGM 
00723      IF GCIO-GOOD-RETURN                                          GA1IPGM 
00724         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1IPGM 
00725      MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR.  CONTACGA1IPGM 
00726 -    'T SYSTEMS AREA ***'  TO  ERRMSGO.                           GA1IPGM 
00727      MOVE '1IF2'  TO  WS-ABEND-CODE.                              GA1IPGM 
00728      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1IPGM 
00729                                                                   GA1IPGM 
00730  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1IPGM 
00731      MOVE  '2080'  TO  WS-PARA-ID.                                GA1IPGM 
00732                                                                   GA1IPGM 
00733      MOVE  GC-GCTABULR-IPGT-VARY-MAX-OCUR                         GA1IPGM 
00734            TO  GX3-ENTRY-COUNT.                                   GA1IPGM 
00735                                                                   GA1IPGM 
00736      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1IPGM 
00737                                                                   GA1IPGM 
00738      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1IPGM 
00739         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1IPGM 
00740         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1IPGM 
00741                                                                   GA1IPGM 
00742      IF GCIO-GOOD-RETURN                                          GA1IPGM 
00743         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1IPGM 
00744      MOVE '1IF3'  TO  WS-ABEND-CODE.                              GA1IPGM 
00745      MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTACT GA1IPGM 
00746 -    'SYSTEMS AREA ***'  TO  ERRMSGO.                             GA1IPGM 
00747      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1IPGM 
00748                                                                   GA1IPGM 
00749  2090-BUILD-NEXT-DISPLAY.                                         GA1IPGM 
00750      MOVE  '2090'  TO  WS-PARA-ID.                                GA1IPGM 
00751      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1IPGM 
00752      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1IPGM 
00753      SET GX3-INDEX  TO  1.                                        GA1IPGM 
00754 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00755 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00756 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00757      IF MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2)  =         GA1IPGM 
00758            LOW-VALUES                                             GA1IPGM 
00759         MOVE GX3-ENTRY (GX3-INDEX)  TO  WS-SAVED-FIELDS           GA1IPGM 
00760      ELSE                                                         GA1IPGM 
00761         MOVE MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2) TO    GA1IPGM 
00762            WS-SAVED-PROVIDER-TYP-ARGUMENT.                        GA1IPGM 
00763 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00764                                                                   GA1IPGM 
00765      PERFORM 4500-FILL-THE-SCREEN.                                GA1IPGM 
00766      EXEC CICS SEND   MAP('GA1II01') MAPSET('GA1ISET') ERASE      GA1IPGM 
00767         FROM(GA1II01O) END-EXEC.                                  GA1IPGM 
00768                                                                   GA1IPGM 
00769  2099-EXIT.   EXIT.                                               GA1IPGM 
00770      EJECT                                                        GA1IPGM 
00771  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1IPGM 
00772 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00773 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00774 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00775      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1IPGM 
00776         MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1IPGM 
00777 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00778                                                                   GA1IPGM 
00779  2199-EXIT.   EXIT.                                               GA1IPGM 
00780      EJECT                                                        GA1IPGM 
00781 ******************************************************************GA1IPGM 
00782 **          X C T L   T O   A D D   S C R E E N                   GA1IPGM 
00783 **                                                                GA1IPGM 
00784 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1IPGM 
00785 ** ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1IPGM 
00786 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1IPGM 
00787 ** TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1IPGM 
00788 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1IPGM 
00789 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1IPGM 
00790 ******************************************************************GA1IPGM 
00791  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1IPGM 
00792      MOVE '3000'  TO  WS-PARA-ID.                                 GA1IPGM 
00793                                                                   GA1IPGM 
00794 *    EXEC CICS                                                    GA1IPGM 
00795 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1IPGM 
00796 *       INITIMG(WS-HEX-00)                                        GA1IPGM 
00797 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1IPGM 
00798 *    END-EXEC.                                                    GA1IPGM 
00799                                                                   GA1IPGM 
00800 *    IF  FRMNUIDI  =  'GS3A'                                      GA1IPGM 
00801 ***     MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA1IPGM 
00802 *       MOVE  GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                   GA1IPGM 
00803 *       MOVE  GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO               GA1IPGM 
00804 *       MOVE  GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1IPGM 
00805 *       MOVE  GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1IPGM 
00806 *       MOVE  SPACES  TO  GCA-L-O-B,                              GA1IPGM 
00807 *                         GCA-PROV-CTL,                           GA1IPGM 
00808 *                         GCA-BEN-PROV-ID.                        GA1IPGM 
00809                                                                   GA1IPGM 
00810 *    IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1IPGM 
00811 ***     MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA1IPGM 
00812 *       MOVE  CONTRACT-GROUP-NO  TO  GCA-GRP-NO                   GA1IPGM 
00813 *       MOVE  CONTRACT-SECTION-NO  TO  GCA-SECTN-NO               GA1IPGM 
00814 *       MOVE  CONTRACT-LOB  TO  GCA-L-O-B                         GA1IPGM 
00815 *       MOVE  CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                 GA1IPGM 
00816 *       MOVE  CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1IPGM 
00817 *       MOVE  CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1IPGM 
00818 *       MOVE  SPACES  TO  GCA-BEN-PROV-ID.                        GA1IPGM 
00819                                                                   GA1IPGM 
00820 *    IF  FRMNUIDI  =  'GC8A'                                      GA1IPGM 
00821 ***     MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA1IPGM 
00822 *       MOVE  BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                   GA1IPGM 
00823 *       MOVE  BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO               GA1IPGM 
00824 *       MOVE  BEN-PROV-LOB  TO  GCA-L-O-B                         GA1IPGM 
00825 *       MOVE  BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                 GA1IPGM 
00826 *       MOVE  BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1IPGM 
00827 *       MOVE  BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1IPGM 
00828 *       MOVE  BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                GA1IPGM 
00829                                                                   GA1IPGM 
00830      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1IPGM 
00831      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1IPGM 
00832      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1IPGM 
00833      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1IPGM 
00834      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1IPGM 
00835      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1IPGM 
00836      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1IPGM 
00837      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1IPGM 
00838 *    MOVE  ZEROES  TO  GCA-EFF-DT.                                GA1IPGM 
00839 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00840 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA1IPGM 
00841 ** FIELDS TO DISPLAY ON THE INITIAL ADD SCREEN THEY SHOULD BE     GA1IPGM 
00842 ** PASSED HERE.                                                   GA1IPGM 
00843 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00844      MOVE INCEXCI TO GCA-I-E-INDC.                                GA1IPGM 
00845                                                                   GA1IPGM 
00846 *    SET  COMMAREA-PNTR   TO                                      GA1IPGM 
00847 *         ADDRESS OF  GCA-COMMAREA.                               GA1IPGM 
00848                                                                   GA1IPGM 
00849 *    EXEC CICS XCTL  PROGRAM('GA2IPGM') COMMAREA(COMMAREA-PNTR)   GA1IPGM 
00850 *       LENGTH(4) END-EXEC.                                       GA1IPGM 
00851      EXEC CICS XCTL  PROGRAM('GA2IPGM')                           GA1IPGM 
00852                      COMMAREA(DFHCOMMAREA)                        GA1IPGM 
00853                      LENGTH(LENGTH OF DFHCOMMAREA)                GA1IPGM 
00854      END-EXEC.                                                    GA1IPGM 
00855                                                                   GA1IPGM 
00856  3099-EXIT.   EXIT.                                               GA1IPGM 
00857      EJECT                                                        GA1IPGM 
00858 ***************************************************************** GA1IPGM 
00859 **          D I S P L A Y   F I R S T   S C R E E N               GA1IPGM 
00860 **                                                                GA1IPGM 
00861 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1IPGM 
00862 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1IPGM 
00863 ** ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1IPGM 
00864 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1IPGM 
00865 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1IPGM 
00866 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1IPGM 
00867 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1IPGM 
00868 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1IPGM 
00869 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1IPGM 
00870 ** DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1IPGM 
00871 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1IPGM 
00872 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1IPGM 
00873 ******************************************************************GA1IPGM 
00874  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1IPGM 
00875      MOVE '4000'  TO  WS-PARA-ID.                                 GA1IPGM 
00876                                                                   GA1IPGM 
00877      MOVE LOW-VALUES  TO GA1II01I.                                GA1IPGM 
00878      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1IPGM 
00879         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1IPGM 
00880            TO ERRMSGO                                             GA1IPGM 
00881         MOVE '1IC1'  TO  WS-ABEND-CODE                            GA1IPGM 
00882         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1IPGM 
00883                                                                   GA1IPGM 
00884 *    SET ADDRESS OF  GCA-COMMAREA                                 GA1IPGM 
00885 *        TO  INCOMING-COMMAREA-PNTR.                              GA1IPGM 
00886                                                                   GA1IPGM 
00887      SET ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD                  GA1IPGM 
00888          TO  GCA-RECORD-POINTER.                                  GA1IPGM 
00889                                                                   GA1IPGM 
00890      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA1IPGM 
00891      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA1IPGM 
00892      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA1IPGM 
00893      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA1IPGM 
00894      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA1IPGM 
00895      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA1IPGM 
00896      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA1IPGM 
00897      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA1IPGM 
00898 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00899 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1IPGM 
00900 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1IPGM 
00901 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00902      MOVE GX3-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               GA1IPGM 
00903      MOVE GCA-I-E-INDC TO INCEXCO.                                GA1IPGM 
00904                                                                   GA1IPGM 
00905      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1IPGM 
00906         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA1IPGM 
00907 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA1IPGM 
00908         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA1IPGM 
00909         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA1IPGM 
00910         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA1IPGM 
00911         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA1IPGM 
00912         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1IPGM 
00913         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA1IPGM 
00914         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA1IPGM 
00915         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA1IPGM 
00916         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1IPGM 
00917         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1IPGM 
00918         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1IPGM 
00919         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1IPGM 
00920                                                                   GA1IPGM 
00921      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1IPGM 
00922         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA1IPGM 
00923 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1IPGM 
00924         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA1IPGM 
00925         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA1IPGM 
00926         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA1IPGM 
00927         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA1IPGM 
00928         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1IPGM 
00929         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA1IPGM 
00930         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA1IPGM 
00931         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA1IPGM 
00932         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1IPGM 
00933         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1IPGM 
00934         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1IPGM 
00935         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1IPGM 
00936         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1IPGM 
00937         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1IPGM 
00938         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1IPGM 
00939         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1IPGM 
00940                                                                   GA1IPGM 
00941      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1IPGM 
00942         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA1IPGM 
00943         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA1IPGM 
00944         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA1IPGM 
00945         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA1IPGM 
00946         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA1IPGM 
00947         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA1IPGM 
00948         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA1IPGM 
00949         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA1IPGM 
00950         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA1IPGM 
00951         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA1IPGM 
00952         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1IPGM 
00953         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA1IPGM 
00954         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1IPGM 
00955         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA1IPGM 
00956         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1IPGM 
00957         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA1IPGM 
00958         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1IPGM 
00959         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA1IPGM 
00960         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1IPGM 
00961                                                                   GA1IPGM 
00962      SET GX3-INDEX  TO  1.                                        GA1IPGM 
00963      MOVE GX3-ENTRY (GX3-INDEX)  TO  WS-SAVED-FIELDS.             GA1IPGM 
00964                                                                   GA1IPGM 
00965      PERFORM 4500-FILL-THE-SCREEN.                                GA1IPGM 
00966      EXEC CICS SEND   MAP('GA1II01') MAPSET('GA1ISET') ERASE      GA1IPGM 
00967         FROM(GA1II01O) END-EXEC.                                  GA1IPGM 
00968                                                                   GA1IPGM 
00969  4099-EXIT.   EXIT.                                               GA1IPGM 
00970      EJECT                                                        GA1IPGM 
00971 ***************************************************************** GA1IPGM 
00972 **             F I L L   T H E   S C R E E N                      GA1IPGM 
00973 **                                                                GA1IPGM 
00974 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1IPGM 
00975 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1IPGM 
00976 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1IPGM 
00977 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1IPGM 
00978 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1IPGM 
00979 ******************************************************************GA1IPGM 
00980  4500-FILL-THE-SCREEN SECTION.                                    GA1IPGM 
00981                                                                   GA1IPGM 
00982      MOVE '4500'  TO  WS-PARA-ID.                                 GA1IPGM 
00983      MOVE  GX3-ENTRY-COUNT  TO  GX3-ENTRY-COUNT.                  GA1IPGM 
00984      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1IPGM 
00985                                                                   GA1IPGM 
00986      IF GX3-ENTRY-COUNT  NOT >  1                                 GA1IPGM 
00987         MOVE '4530'  TO  WS-PARA-ID                               GA1IPGM 
00988         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1IPGM 
00989                                                                   GA1IPGM 
00990      SET GX3-INDEX  TO  1.                                        GA1IPGM 
00991      MOVE '4510'  TO  WS-PARA-ID.                                 GA1IPGM 
00992  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1IPGM 
00993 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00994 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
00995 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00996      IF GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)  <                 GA1IPGM 
00997            WS-SAVED-PROVIDER-TYP-ARGUMENT                         GA1IPGM 
00998 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
00999         SET GX3-INDEX  UP BY  1                                   GA1IPGM 
01000         IF  GX3-INDEX  <  GX3-ENTRY-COUNT                         GA1IPGM 
01001            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1IPGM 
01002         ELSE                                                      GA1IPGM 
01003            SET GX3-INDEX  TO  1.                                  GA1IPGM 
01004                                                                   GA1IPGM 
01005      MOVE '4520'  TO  WS-PARA-ID.                                 GA1IPGM 
01006  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1IPGM 
01007      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1IPGM 
01008      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1IPGM 
01009 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
01010 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
01011 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
01012      MOVE GX3-PROVIDER-TYPE-ARGUMENT (GX3-INDEX)  TO              GA1IPGM 
01013         MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1IPGM 
01014 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
01015                                                                   GA1IPGM 
01016      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1IPGM 
01017         SET  MAP-IDX1  UP BY  1                                   GA1IPGM 
01018      ELSE                                                         GA1IPGM 
01019         IF MAP-IDX2  <  WS-MAP-COL                                GA1IPGM 
01020            SET  MAP-IDX1  TO  1                                   GA1IPGM 
01021            SET  MAP-IDX2  UP BY  1                                GA1IPGM 
01022         ELSE                                                      GA1IPGM 
01023            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1IPGM 
01024                                                                   GA1IPGM 
01025      IF GX3-INDEX  <  (GX3-ENTRY-COUNT - 1 )                      GA1IPGM 
01026         SET  GX3-INDEX  UP BY  1                                  GA1IPGM 
01027         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1IPGM 
01028                                                                   GA1IPGM 
01029      MOVE '4530'  TO  WS-PARA-ID.                                 GA1IPGM 
01030  4530-FILL-REST-WITH-NULLS.                                       GA1IPGM 
01031      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1IPGM 
01032 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
01033 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1IPGM 
01034 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
01035      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1IPGM 
01036         MAP-PROVIDER-TYP-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1IPGM 
01037 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1IPGM 
01038      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1IPGM 
01039         SET  MAP-IDX1   UP BY  1                                  GA1IPGM 
01040         GO TO 4530-FILL-REST-WITH-NULLS                           GA1IPGM 
01041      ELSE                                                         GA1IPGM 
01042         IF MAP-IDX2  <  WS-MAP-COL                                GA1IPGM 
01043            SET  MAP-IDX1  TO  1                                   GA1IPGM 
01044            SET  MAP-IDX2  UP BY 1                                 GA1IPGM 
01045            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1IPGM 
01046                                                                   GA1IPGM 
01047      MOVE '4540'  TO  WS-PARA-ID.                                 GA1IPGM 
01048  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1IPGM 
01049      IF GX3-ENTRY-COUNT  =  1                                     GA1IPGM 
01050         MOVE '*** NO ENTRIES TO DELETE ***'  TO  ERRMSGO          GA1IPGM 
01051         GO TO 4599-EXIT.                                          GA1IPGM 
01052                                                                   GA1IPGM 
01053      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1IPGM 
01054         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'  TO  ERRMSGO.   GA1IPGM 
01055                                                                   GA1IPGM 
01056  4599-EXIT.     EXIT.                                             GA1IPGM 
01057      EJECT                                                        GA1IPGM 
01058 ***************************************************************** GA1IPGM 
01059 **        X C T L   T O   P R E V I O U S   M E N U               GA1IPGM 
01060 **                                                                GA1IPGM 
01061 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1IPGM 
01062 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1IPGM 
01063 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1IPGM 
01064 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1IPGM 
01065 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1IPGM 
01066 ******************************************************************GA1IPGM 
01067  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1IPGM 
01068      MOVE '5000'  TO  WS-PARA-ID.                                 GA1IPGM 
01069                                                                   GA1IPGM 
01070                                                                   GA1IPGM 
01071 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA1IPGM 
01072 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA1IPGM 
01073 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA1IPGM 
01074                                                                   GA1IPGM 
01075      IF  ALTBFNCI  =  'GTM1'                                      GA1IPGM 
01076          EXEC CICS XCTL                                           GA1IPGM 
01077                    PROGRAM('GTM1PGM')                             GA1IPGM 
01078                    END-EXEC.                                      GA1IPGM 
01079                                                                   GA1IPGM 
01080      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN      =                   GA1IPGM 
01081               GC-GCIOPARM-LEN                 +                   GA1IPGM 
01082               GC-WORKFILE-KEY-LEN             +                   GA1IPGM 
01083               GC-GCTABULR-ABM-FIXED-LEN       +                   GA1IPGM 
01084              (GC-GCTABULR-ABM-VARY-LEN        *                   GA1IPGM 
01085               GC-GCTABULR-ABM-VARY-MAX-OCUR)                      GA1IPGM 
01086                                                                   GA1IPGM 
01087      EXEC CICS                                                    GA1IPGM 
01088         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA1IPGM 
01089         INITIMG(WS-HEX-00)                                        GA1IPGM 
01090         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA1IPGM 
01091      END-EXEC.                                                    GA1IPGM 
01092                                                                   GA1IPGM 
01093 *    EXEC CICS                                                    GA1IPGM 
01094 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1IPGM 
01095 *       INITIMG(WS-HEX-00)                                        GA1IPGM 
01096 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1IPGM 
01097 *    END-EXEC.                                                    GA1IPGM 
01098                                                                   GA1IPGM 
01099      IF  FRMNUIDI  =  'GS3A'                                      GA1IPGM 
01100         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1IPGM 
01101         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1IPGM 
01102         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA1IPGM 
01103         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1IPGM 
01104         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1IPGM 
01105         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1IPGM 
01106         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1IPGM 
01107         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1IPGM 
01108                          GCIO-WRK-PROVIDER-CONTROL                GA1IPGM 
01109         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1IPGM 
01110         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1IPGM 
01111         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1IPGM 
01112         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1IPGM 
01113                            GCA-ALL-LEVEL-TAB-ID                   GA1IPGM 
01114         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1IPGM 
01115                            GCA-ALL-LEVEL-TAB-SLOT                 GA1IPGM 
01116         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1IPGM 
01117                            GCA-INTERNAL-TAB-ID,                   GA1IPGM 
01118                            GCA-INTERNAL-TAB-SLOT                  GA1IPGM 
01119         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1IPGM 
01120                                                                   GA1IPGM 
01121      IF  FRMNUIDI  =  'GC4A'                                      GA1IPGM 
01122         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1IPGM 
01123         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1IPGM 
01124         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1IPGM 
01125         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1IPGM 
01126         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1IPGM 
01127         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1IPGM 
01128         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1IPGM 
01129         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1IPGM 
01130         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1IPGM 
01131         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1IPGM 
01132         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1IPGM 
01133         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1IPGM 
01134         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1IPGM 
01135                            GCA-ALL-LEVEL-TAB-ID                   GA1IPGM 
01136         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1IPGM 
01137                            GCA-ALL-LEVEL-TAB-SLOT                 GA1IPGM 
01138         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1IPGM 
01139                            GCA-INTERNAL-TAB-ID,                   GA1IPGM 
01140                            GCA-INTERNAL-TAB-SLOT                  GA1IPGM 
01141         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1IPGM 
01142                                                                   GA1IPGM 
01143      IF  FRMNUIDI  =  'GC8A'                                      GA1IPGM 
01144         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1IPGM 
01145         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1IPGM 
01146         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA1IPGM 
01147         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1IPGM 
01148         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1IPGM 
01149         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1IPGM 
01150         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1IPGM 
01151         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1IPGM 
01152         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1IPGM 
01153         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1IPGM 
01154         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1IPGM 
01155         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA1IPGM 
01156         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1IPGM 
01157         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA1IPGM 
01158                            GCA-ALL-LEVEL-TAB-ID                   GA1IPGM 
01159         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA1IPGM 
01160                            GCA-ALL-LEVEL-TAB-SLOT                 GA1IPGM 
01161         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA1IPGM 
01162                          GCA-INTERNAL-TAB-SLOT.                   GA1IPGM 
01163                                                                   GA1IPGM 
01164      MOVE  'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                     GA1IPGM 
01165 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA1IPGM 
01166 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA1IPGM 
01167 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA1IPGM 
01168 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA1IPGM 
01169 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA1IPGM 
01170      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1IPGM 
01171                                                                   GA1IPGM 
01172      SET  GCA-RECORD-POINTER                                      GA1IPGM 
01173           TO ADDRESS OF  IO-PARM-ALL-LEVEL-RECORD.                GA1IPGM 
01174                                                                   GA1IPGM 
01175      MOVE  GC-GCTABULR-ABM-VARY-MAX-OCUR                          GA1IPGM 
01176            TO  GAA-ENTRY-COUNT.                                   GA1IPGM 
01177                                                                   GA1IPGM 
01178      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1IPGM 
01179                                                                   GA1IPGM 
01180      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1IPGM 
01181         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA1IPGM 
01182         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA1IPGM 
01183                                                                   GA1IPGM 
01184      IF  NOT GCIO2-GOOD-RETURN                                    GA1IPGM 
01185         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1IPGM 
01186 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1IPGM 
01187         MOVE '1IF4'  TO  WS-ABEND-CODE                            GA1IPGM 
01188         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1IPGM 
01189                                                                   GA1IPGM 
01190 *    SET  COMMAREA-PNTR                                           GA1IPGM 
01191 *         TO ADDRESS OF  GCA-COMMAREA.                            GA1IPGM 
01192                                                                   GA1IPGM 
01193      IF  ALTBFNCI  =  'GA1B'                                      GA1IPGM 
01194 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA1IPGM 
01195 *          LENGTH(4) END-EXEC.                                    GA1IPGM 
01196         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA1IPGM 
01197                         COMMAREA(DFHCOMMAREA)                     GA1IPGM 
01198                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1IPGM 
01199         END-EXEC.                                                 GA1IPGM 
01200                                                                   GA1IPGM 
01201      IF  ALTBFNCI  =  'GA1C'                                      GA1IPGM 
01202 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA1IPGM 
01203 *          LENGTH(4) END-EXEC.                                    GA1IPGM 
01204         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA1IPGM 
01205                         COMMAREA(DFHCOMMAREA)                     GA1IPGM 
01206                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1IPGM 
01207         END-EXEC.                                                 GA1IPGM 
01208                                                                   GA1IPGM 
01209      IF  ALTBFNCI  =  'GA1D'                                      GA1IPGM 
01210 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA1IPGM 
01211 *          LENGTH(4) END-EXEC.                                    GA1IPGM 
01212         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA1IPGM 
01213                         COMMAREA(DFHCOMMAREA)                     GA1IPGM 
01214                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1IPGM 
01215         END-EXEC.                                                 GA1IPGM 
01216                                                                   GA1IPGM 
01217      IF  ALTBFNCI  =  'GA1E'                                      GA1IPGM 
01218 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA1IPGM 
01219 *          LENGTH(4) END-EXEC.                                    GA1IPGM 
01220         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA1IPGM 
01221                         COMMAREA(DFHCOMMAREA)                     GA1IPGM 
01222                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1IPGM 
01223         END-EXEC.                                                 GA1IPGM 
01224                                                                   GA1IPGM 
01225      IF  ALTBFNCI  =  'GA1P'                                      GA1IPGM 
01226         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA1IPGM 
01227                         COMMAREA(DFHCOMMAREA)                     GA1IPGM 
01228                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1IPGM 
01229         END-EXEC.                                                 GA1IPGM 
01230                                                                   GA1IPGM 
01231  5099-EXIT.                                                       GA1IPGM 
01232      EXIT.                                                        GA1IPGM 
01233 /**************************************************************** GA1IPGM 
01234 **           X C T L   T O   M A I N   M E N U                    GA1IPGM 
01235 **                                                                GA1IPGM 
01236 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1IPGM 
01237 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1IPGM 
01238 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1IPGM 
01239 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1IPGM 
01240 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1IPGM 
01241 ** AND PROGRESS DOWN.                                             GA1IPGM 
01242 ******************************************************************GA1IPGM 
01243  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1IPGM 
01244      MOVE '6000'  TO  WS-PARA-ID.                                 GA1IPGM 
01245      MOVE '1IP1'  TO  WS-ABEND-CODE.                              GA1IPGM 
01246                                                                   GA1IPGM 
01247      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1IPGM 
01248                                                                   GA1IPGM 
01249  6099-EXIT.     EXIT.                                             GA1IPGM 
01250      EJECT                                                        GA1IPGM 
01251 /*****************************************************************GA1IPGM 
01252 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA1IPGM 
01253 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA1IPGM 
01254 ******************************************************************GA1IPGM 
01255  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA1IPGM 
01256  9800-010.                                                        GA1IPGM 
01257                                                                   GA1IPGM 
01258      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1IPGM 
01259      MOVE 'M'   TO  HGADATE-FORM1.                                GA1IPGM 
01260      MOVE 'J'   TO  HGADATE-FORM2.                                GA1IPGM 
01261      MOVE ZEROS TO  HGADATE-RETURN                                GA1IPGM 
01262                     HGADATE-AMOUNT.                               GA1IPGM 
01263      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1IPGM 
01264                     COMMAREA(HGADATES-COMMAREA)                   GA1IPGM 
01265                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1IPGM 
01266                     END-EXEC.                                     GA1IPGM 
01267                                                                   GA1IPGM 
01268  9800-900-900-EXIT.                                               GA1IPGM 
01269      EXIT.                                                        GA1IPGM 
01270 /*****************************************************************GA1IPGM 
01271 * 9810    J U L I A N    T O    G R E G O R I A N                *GA1IPGM 
01272 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *GA1IPGM 
01273 ******************************************************************GA1IPGM 
01274  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          GA1IPGM 
01275  9810-010.                                                        GA1IPGM 
01276                                                                   GA1IPGM 
01277      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1IPGM 
01278      MOVE 'J'   TO  HGADATE-FORM1.                                GA1IPGM 
01279      MOVE 'M'   TO  HGADATE-FORM2.                                GA1IPGM 
01280      MOVE ZEROS TO  HGADATE-RETURN                                GA1IPGM 
01281                     HGADATE-AMOUNT.                               GA1IPGM 
01282      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1IPGM 
01283                     COMMAREA(HGADATES-COMMAREA)                   GA1IPGM 
01284                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1IPGM 
01285                     END-EXEC.                                     GA1IPGM 
01286                                                                   GA1IPGM 
01287  9810-900-900-EXIT.                                               GA1IPGM 
01288      EXIT.                                                        GA1IPGM 
01289      EJECT                                                        GA1IPGM 
01290  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1IPGM 
01291                                                                   GA1IPGM 
01292      SET MAP-IDX1 TO 7.                                           GA1IPGM 
01293      SET MAP-IDX2 TO 1.                                           GA1IPGM 
01294      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1IPGM 
01295                                                                   GA1IPGM 
01296      EXEC CICS SEND   MAP('GA1II01') MAPSET('GA1ISET') ERASE      GA1IPGM 
01297         FROM(GA1II01O) WAIT END-EXEC.                             GA1IPGM 
01298                                                                   GA1IPGM 
01299      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1IPGM 
01300                                                                   GA1IPGM 
01301  9999-EXIT.     EXIT.                                             GA1IPGM 
