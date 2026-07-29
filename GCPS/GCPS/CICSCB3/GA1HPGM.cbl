00001 *      LAST MAINTENANCE TIME:  8.41.27  DATE: 11/16/84            08/20/03
00002  IDENTIFICATION DIVISION.                                         GA1HPGM 
00003  PROGRAM-ID.     GA1HPGM.                                            LV001
00004 **** THIS IS A COBOL/2 PROGRAM *****                              GA1HPGM 
00005  AUTHOR.         S BUCH.                                          GA1HPGM 
00006  DATE-WRITTEN.   11/13/84.                                        GA1HPGM 
00007  DATE-COMPILED.                                                   GA1HPGM 
00008      SKIP3                                                        GA1HPGM 
00009 ******************************************************************GA1HPGM 
00010 *   GA1HPGM   ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM      GA1HPGM 
00011 *                PROVIDER-GROUP BY PROVIDER NUMBERS - GA1H        GA1HPGM 
00012 *                                                                 GA1HPGM 
00013 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1HPGM 
00014 *   CURRENTLY ON THE ALL LEVEL INTERNAL TABULAR RECORD.           GA1HPGM 
00015 *                                                                 GA1HPGM 
00016 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1HPGM 
00017 *   ALL LEVEL INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN    GA1HPGM 
00018 *   DECIDE IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN     GA1HPGM 
00019 *   ENTRY WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE    GA1HPGM 
00020 *   RECORD WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED   GA1HPGM 
00021 *   BACK INTO THE RECORD BEFORE UPDATING THE RECORD.              GA1HPGM 
00022 *                                                                 GA1HPGM 
00023 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #IPGN) GA1HPGM 
00024 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1HPGM 
00025 *   XCTL TO TRANS GA2H OR PROGRAM GA2HPGM.  THIS PROGRAM WILL     GA1HPGM 
00026 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1HPGM 
00027 *   TABLE.                                                        GA1HPGM 
00028 *                                                                 GA1HPGM 
00029 *   FUNC CODE: GA1H                                               GA1HPGM 
00030 *   MAPSET:    GA1HSETC                                           GA1HPGM 
00031 *   FILES:     GCPSWORK                                           GA1HPGM 
00032 *                                                                 GA1HPGM 
00033 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00034 *                                                                 GA1HPGM 
00035 *    TAILORING INSTRUCTIONS:                                      GA1HPGM 
00036 *                                                                 GA1HPGM 
00037 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1HPGM 
00038 *                                                                 GA1HPGM 
00039 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1H/     GA1HPGM 
00040 *              SCREEN PAGE NUMBER                 /009I/001H/     GA1HPGM 
00041 *              ADD PROGRAM FUNCTION CODE          /GCAI/GA2H/     GA1HPGM 
00042 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IPGN/    GA1HPGM 
00043 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX2/       GA1HPGM 
00044 *                                                                 GA1HPGM 
00045 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1HPGM 
00046 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1HPGM 
00047 *     OCCURANCES.                                                 GA1HPGM 
00048 *                                                                 GA1HPGM 
00049 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1HPGM 
00050 *     THAT MUST BE CHANGED.                                       GA1HPGM 
00051 *                                                                 GA1HPGM 
00052 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00053      SKIP3                                                        GA1HPGM 
00054 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1HPGM 
00055 *   DATE    PROGRAMMER  MAINTENANCE                             * GA1HPGM 
00056 * --------  ----------  --------------------------------------- * GA1HPGM 
00057 * 11-19-85      LET     REMOVED ALL HANDLE CONDITIONS EXCEPT    * GA1HPGM 
00058 *                       FOR MAPFAIL.                            * GA1HPGM 
00059 *                                                               * GA1HPGM 
00060 * 12-17-86      AHL     REVISION RELATED TO PROVIDER NUMBER     * GA1HPGM 
00061 * M050                  ARGUMENT DUE TO ITS PICTURE BEING       * GA1HPGM 
00062 *                       CHANGED FROM COMP-3 S9(10) TO X(10).    * GA1HPGM 
00063 *                       NUMBER OF OCCURS IS ALSO CHANGED TO     * GA1HPGM 
00064 *                       395 FROM 659.                           * GA1HPGM 
00065 *                                                               * GA1HPGM 
00066 *03/16/87       JLA     CHANGES FOR SINGLE TABULAR SUPPORT THAT * GA1HPGM 
00067 * (D0120)               ARE EXECUTED FROM TRANSACTION GTM1:     * GA1HPGM 
00068 *                       1. PF1/PF13 - CONSTRUCT COMMAREA AS IF  * GA1HPGM 
00069 *                          GC4A HAD CALLED, XCTL TO ADD SCREEN  * GA1HPGM 
00070 *                          PROGRAM.                             * GA1HPGM 
00071 *                       2. PF3/PF15 - XCTL TO GTM1PGM WITHOUT   * GA1HPGM 
00072 *                          PASSING ANY COMMAREA.                * GA1HPGM 
00073 *                                                               * GA1HPGM 
00074 * 8/17/87       FRY     CAPTURE OPERATOR-ID WHEN A 'C3', 'C6',  * GA1HPGM 
00075 * (D116)                OR 'G4' RECORD IS UPDATED.              * GA1HPGM 
00076 *                                                               * GA1HPGM 
00077 * 11/30/89      NGE     FIX INCL/EXCL IND MISSING FROM SCREEN.  * GA1HPGM 
00078 * (R1681)                                                       * GA1HPGM 
00079 *                       ----ACCUM TABULAR RECORD MODIFICATION--- *GA1HPGM 
00080 * 11154   10/02/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GA1HPGM 
00081 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GA1HPGM 
00082 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GA1HPGM 
00083 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GA1HPGM 
00084 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GA1HPGM 
00085 *                       6. REMOVE HARDCOPY ROUTINE.              *GA1HPGM 
00086 *                       7. >>> CONVERT TO COBOL/II <<<<          *GA1HPGM 
00087 *                                                                *GA1HPGM 
00088 * 11154   04/01/91  ENW 1. PRODUCTION FIX, PERIOD WAS OMITTED IN *GA1HPGM 
00089 *                          DATE CONVERSION ROUTINE CAUSING       *GA1HPGM 
00090 *                          DSIDERR WHEN LINKING TO GCIOPGM.      *GA1HPGM 
00091 *                                                                *GA1HPGM 
00092 *D12009 08/28/91  TPM   INCREASED THE FAMILY RELATION            *GA1HPGM 
00093 *                   FIELD   FROM ONE POSITION TO TWO POSITIONS.  *GA1HPGM 
00094 *                                                                *GA1HPGM 
00095 *14726/ 11/11/97  DAU   ADDED CODE TO SUPPORT THE YEAR 2000      *GA1HPGM 
00096 *15057                  AND THE EXPANSION OF THE GROUP SPECIFIC  *GA1HPGM 
00097 *                       AND CONTRACT KEY TO SUPPORT THE TEXAS    *GA1HPGM 
00098 *                       MERGER.                                  *GA1HPGM 
00099 *                                                                *GA1HPGM 
00100 * 14726/  05/15/98  AB   EXPANDED THE SCREEN / MAP               *GA1HPGM 
00101 * 15057                  TO INCLUDE THE ENTIRE KEY               *GA1HPGM 
00102 *                                                                *GA1HPGM 
00103 *  D341   10/07/98  GDM  1. ACTL TO NEW ACCUM TABULAR #ACP       *GA1HPGM 
00104 *                        2. ADD DELADD-OPTION = 'GAS5UPD'        *GA1HPGM 
00105 *                                                                *GA1HPGM 
00106 * P????   11/19/99  FRY  ADD LENGTH PARAMETER TO THE RETURN      *GA1HPGM 
00107 *                        COMMAND WHEN DFHCOMMAREA IS SPECIFIED.  *GA1HPGM 
00108 *                                                                *GA1HPGM 
00109 *                                                                *GA1HPGM 
00110 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA1HPGM 
SI0724*                                                                *00030141
SI0724* P56703  05/08/24  SI  RECOMPILED -PEAQ COPYBOOK EXPANSION      *00030150
SI0724*                       COPY ABM, ACP, ACL, ADL, AOL,            *00030160
SI0724*                       GCCDRLEN                                 *00030170
00111 *                                                                *GA1HPGM 
00112 *                                                                *GA1HPGM 
00113 ******************************************************************GA1HPGM 
00114 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1HPGM 
00115 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1HPGM 
00116      EJECT                                                        GA1HPGM 
00117  ENVIRONMENT DIVISION.                                            GA1HPGM 
00118      EJECT                                                        GA1HPGM 
00119  DATA DIVISION.                                                   GA1HPGM 
00120  WORKING-STORAGE SECTION.                                         GA1HPGM 
00121  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1HPGM 
00122      '***GA1HPGM WS BEGINS***'.                                   GA1HPGM 
00123  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1HPGM 
00124                                                                   GA1HPGM 
00125  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1HPGM 
00126                                                                   GA1HPGM 
00127  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1HPGM 
00128  01  COMMAREA-POINTER-AREA.                                       GA1HPGM 
00129      05  COMMAREA-PNTR-COMP          PIC S9(8)  COMP.             GA1HPGM 
00130      05  COMMAREA-PNTR  REDEFINES                                 GA1HPGM 
00131          COMMAREA-PNTR-COMP          USAGE IS  POINTER.           GA1HPGM 
00132                                                                   GA1HPGM 
00133 ** MAP COBOL SCREEN DSECTS **                                     GA1HPGM 
00134  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1HPGM 
00135      '***  I/O MAPAREA ***'.                                      GA1HPGM 
00136  COPY GA1HSETC.                                                   GA1HPGM 
00137      EJECT                                                        GA1HPGM 
00138 ******************************************************************GA1HPGM 
00139 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA1HPGM 
00140 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1HPGM 
00141 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1HPGM 
00142 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1HPGM 
00143 **  REDEFINES.                                                    GA1HPGM 
00144 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00145 **                                                                GA1HPGM 
00146 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1HPGM 
00147 **  FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1HPGM 
00148 **  MATCH THE MAP.                                                GA1HPGM 
00149 **                                                                GA1HPGM 
00150 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00151                                                                   GA1HPGM 
00152  01  FILLER     REDEFINES   GA1HI01I.                             GA1HPGM 
00153      05  FILLER                              PIC X(83).           GA1HPGM 
00154      05  GROUP-SPECIFIC-ID-LINE.                                  GA1HPGM 
00155          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA1HPGM 
00156          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA1HPGM 
00157          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA1HPGM 
00158          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA1HPGM 
00159          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA1HPGM 
00160          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA1HPGM 
00161          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA1HPGM 
00162          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA1HPGM 
00163          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA1HPGM 
00164          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA1HPGM 
00165          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA1HPGM 
00166          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA1HPGM 
00167          10  FILLER                          PIC X(16).           GA1HPGM 
00168      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA1HPGM 
00169          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1HPGM 
00170          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1HPGM 
00171          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1HPGM 
00172          10  CONTRACT-GROUP-NO               PIC X(9).            GA1HPGM 
00173          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1HPGM 
00174          10  CONTRACT-SECTION-NO             PIC X(5).            GA1HPGM 
00175          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1HPGM 
00176          10  CONTRACT-PKG-CODE               PIC X(3).            GA1HPGM 
00177          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1HPGM 
00178          10  CONTRACT-LOB                    PIC X.               GA1HPGM 
00179          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1HPGM 
00180          10  CONTRACT-PROV-CTL               PIC XX.              GA1HPGM 
00181          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1HPGM 
00182          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1HPGM 
00183          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1HPGM 
00184          10  CONTRACT-EFF-DATE               PIC X(6).            GA1HPGM 
00185          10  FILLER                          PIC X(1).            GA1HPGM 
00186      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA1HPGM 
00187                                     GROUP-SPECIFIC-ID-LINE.       GA1HPGM 
00188          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA1HPGM 
00189          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA1HPGM 
00190          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA1HPGM 
00191          10  BEN-PROV-GROUP-NO               PIC X(9).            GA1HPGM 
00192          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA1HPGM 
00193          10  BEN-PROV-SECTION-NO             PIC X(5).            GA1HPGM 
00194          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA1HPGM 
00195          10  BEN-PROV-PKG-CODE               PIC X(3).            GA1HPGM 
00196          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA1HPGM 
00197          10  BEN-PROV-LOB                    PIC X.               GA1HPGM 
00198          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA1HPGM 
00199          10  BEN-PROV-PROV-CTL               PIC XX.              GA1HPGM 
00200          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA1HPGM 
00201          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA1HPGM 
00202          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA1HPGM 
00203          10  BEN-PROV-EFF-DATE               PIC X(6).            GA1HPGM 
00204          10  BEN-PROV-ID-HEADING             PIC X(6).            GA1HPGM 
00205          10  BEN-PROV-ID-NO                  PIC X(6).            GA1HPGM 
00206          10  FILLER                          PIC X(4).            GA1HPGM 
00207      05  FILLER                              PIC X(74).           GA1HPGM 
00208      05  MAP-PROVIDER-NO-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED    GA1HPGM 
00209          BY MAP-IDX1.                                             GA1HPGM 
00210        10  MAP-PROVIDER-NO-ARGUMENT-COL  OCCURS 3 TIMES INDEXED   GA1HPGM 
00211            BY MAP-IDX2.                                           GA1HPGM 
00212          15  MAP-ACTION-CODE-LEN             PIC S9(4) COMP SYNC. GA1HPGM 
00213          15  MAP-ACTION-CODE-ATTR            PIC X.               GA1HPGM 
00214          15  MAP-ACTION-CODE                 PIC X.               GA1HPGM 
00215          15  MAP-PROVIDER-NO-ARGUMENT-LEN    PIC S9(4) COMP SYNC. GA1HPGM 
00216          15  MAP-PROVIDER-NO-ARGUMENT-ATTR   PIC X.               GA1HPGM 
00217          15  MAP-PROVIDER-NO-ARGUMENT        PIC X(10).           GA1HPGM 
00218 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00219  01  WS-MAP-OCCURS-COUNTERS.                                      GA1HPGM 
00220 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00221 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1HPGM 
00222 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00223      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +14.     GA1HPGM 
00224      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +3.      GA1HPGM 
00225 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00226      EJECT                                                        GA1HPGM 
00227 ** ALTERNATIVE WORKFILE KEYS **                                   GA1HPGM 
00228  01  FILLER                      PIC X(32)  VALUE                 GA1HPGM 
00229      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1HPGM 
00230  01  WS-ALT-WORKFILE-KEYS.                                        GA1HPGM 
00231  COPY GCWRKKEY.                                                   GA1HPGM 
00232 /                                                                 GA1HPGM 
00233                                                                   GA1HPGM 
00234 ** DATE FORMATTING AREA **                                        GA1HPGM 
00235  01  HGADATES-COMMAREA.                                           GA1HPGM 
00236  COPY HGCDAT01.                                                   GA1HPGM 
00237                                                                   GA1HPGM 
00238      EJECT                                                        GA1HPGM 
00239 ** WORKFIELDS, AND SWITCHES **                                    GA1HPGM 
00240  01  WS-WORK-FIELDS.                                              GA1HPGM 
00241      05  WS-HEX-00                     PIC X.                     GA1HPGM 
00242      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1HPGM 
00243 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00244 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00245 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00246      05  WS-SAVED-FIELDS.                                         GA1HPGM 
00247        10  WS-SAVED-PROVIDER-NO-ARGUMENT                          GA1HPGM 
00248                                        PIC X(10).                 GA1HPGM 
00249 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00250  01  WS-SWITCHES.                                                 GA1HPGM 
00251      05  WS-ERROR-SW                   PIC X.                     GA1HPGM 
00252                                                                   GA1HPGM 
00253 ** TITLE LINES **                                                 GA1HPGM 
00254  01  WS-TITLE-LINES.                                              GA1HPGM 
00255      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA1HPGM 
00256          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA1HPGM 
00257      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA1HPGM 
00258          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA1HPGM 
00259      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA1HPGM 
00260          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA1HPGM 
00261                                                                   GA1HPGM 
00262      EJECT                                                        GA1HPGM 
00263 ** ATTRIBUTES **                                                  GA1HPGM 
00264  COPY DFHBMSCA.                                                   GA1HPGM 
00265      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1HPGM 
00266      EJECT                                                        GA1HPGM 
00267 ** ATTENTION IDENTIFIERS **                                       GA1HPGM 
00268  COPY DFHAID.                                                     GA1HPGM 
00269      EJECT                                                        GA1HPGM 
00270 ** RECORD LENGTHS **                                              GA1HPGM 
00271  01  WS-RECORD-LENGTHS.                                           GA1HPGM 
00272     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA1HPGM 
00273     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA1HPGM 
00274     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1HPGM 
00275     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1HPGM 
00276 *                                                                 GA1HPGM 
00277 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA1HPGM 
00278  01  FILLER.                                                      GA1HPGM 
00279      COPY GCCDRLEN.                                               GA1HPGM 
00280                                                                   GA1HPGM 
00281                                                                   GA1HPGM 
00282  01  WS-END                      PIC X(16)  VALUE                 GA1HPGM 
00283      '*** W/S ENDS ***'.                                          GA1HPGM 
00284      EJECT                                                        GA1HPGM 
00285  LINKAGE SECTION.                                                 GA1HPGM 
00286  01  DFHCOMMAREA.                                                 GA1HPGM 
00287  COPY G2ALCKEC.                                                   GA1HPGM 
00288  COPY GACDACWA.                                                   GA1HPGM 
00289 *    05  INCOMING-COMMAREA-PNTR   USAGE IS  POINTER.              GA1HPGM 
00290      05  GAS1UPD-PASSED-AREA.                                     GA1HPGM 
00291          07  LVL2-B-SW           PIC X.                           GA1HPGM 
00292          07  LVL2-F-SW           PIC X.                           GA1HPGM 
00293          07  LVL2-G-SW           PIC X.                           GA1HPGM 
00294          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1HPGM 
00295          07  FILLER              PIC X(9).                        GA1HPGM 
00296      05  DELADD-OPTION           PIC X(7).                        GA1HPGM 
00297                                                                   GA1HPGM 
00298 *01  GCA-COMMAREA.                                                GA1HPGM 
00299 *COPY G2ALCKEC.                                                   GA1HPGM 
00300 /                                                                 GA1HPGM 
00301  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA1HPGM 
00302  COPY GCIOPRM1.                                                   GA1HPGM 
00303      EJECT                                                        GA1HPGM 
00304  COPY GCWRKDCC.                                                   GA1HPGM 
00305      SKIP3                                                        GA1HPGM 
00306  COPY GCTIPGNC.                                                   GA1HPGM 
00307      EJECT                                                        GA1HPGM 
00308 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00309 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00310 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00311  01  COPY-TABULAR-TABLE-AREA.                                     GA1HPGM 
00312      05  COPY-TABULAR-TABLE  OCCURS 395 TIMES INDEXED BY          GA1HPGM 
00313            COPY-IDX.                                              GA1HPGM 
00314        10  COPY-PROVIDER-NO-ARGUMENT             PIC  X(10).      GA1HPGM 
00315 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00316      EJECT                                                        GA1HPGM 
00317  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA1HPGM 
00318  COPY GCIOPRM2.                                                   GA1HPGM 
00319      EJECT                                                        GA1HPGM 
00320  COPY GCWRKDC2.                                                   GA1HPGM 
00321      EJECT                                                        GA1HPGM 
00322  COPY GCTABMC.                                                    GA1HPGM 
00323      EJECT                                                        GA1HPGM 
00324                                                                   GA1HPGM 
00325  PROCEDURE DIVISION.                                              GA1HPGM 
00326                                                                   GA1HPGM 
00327 ******************************************************************GA1HPGM 
00328 **               H O U S E K E E P I N G                          GA1HPGM 
00329 **                                                                GA1HPGM 
00330 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA1HPGM 
00331 **                                                                GA1HPGM 
00332 ******************************************************************GA1HPGM 
00333  0000-HOUSEKEEPING   SECTION.                                     GA1HPGM 
00334                                                                   GA1HPGM 
00335      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1HPGM 
00336                                                                   GA1HPGM 
00337      IF EIBAID  =  DFHCLEAR                                       GA1HPGM 
00338          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1HPGM 
00339                         ERASE                                     GA1HPGM 
00340          END-EXEC                                                 GA1HPGM 
00341          EXEC CICS RETURN                                         GA1HPGM 
00342          END-EXEC.                                                GA1HPGM 
00343                                                                   GA1HPGM 
00344      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1HPGM 
00345                                   END-EXEC.                       GA1HPGM 
00346                                                                   GA1HPGM 
00347  0000-EXIT.    EXIT.                                              GA1HPGM 
00348 /*****************************************************************GA1HPGM 
00349 **                     M A I N L I N E                            GA1HPGM 
00350 **                                                                GA1HPGM 
00351 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1HPGM 
00352 **  TAKEN BY THE OPERATOR.                                        GA1HPGM 
00353 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1HPGM 
00354 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1HPGM 
00355 **     ADDITIONS FROM.                                            GA1HPGM 
00356 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1HPGM 
00357 **     KEY PF12 OR PF24.                                          GA1HPGM 
00358 **  3. RECEIVE THE SCREEN.                                        GA1HPGM 
00359 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1HPGM 
00360 **     MENU.                                                      GA1HPGM 
00361 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1HPGM 
00362 **     LOGIC.                                                     GA1HPGM 
00363 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1HPGM 
00364 **     (RETURN) TO THE ADD PROGRAM (GA2HPGM).                     GA1HPGM 
00365 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1HPGM 
00366 **     (RETURN) TO THE PREVIOUS MENU.                             GA1HPGM 
00367 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1HPGM 
00368 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1HPGM 
00369 **                                                                GA1HPGM 
00370 ******************************************************************GA1HPGM 
00371  1000-MAIN-LINE.                                                  GA1HPGM 
00372                                                                   GA1HPGM 
00373      MOVE '1000'  TO  WS-PARA-ID.                                 GA1HPGM 
00374      IF EIBTRNID  NOT =  'GA1H'                                   GA1HPGM 
00375         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1HPGM 
00376         GO TO 1099-RETURN.                                        GA1HPGM 
00377                                                                   GA1HPGM 
00378      EXEC CICS RECEIVE   MAP('GA1HI01') MAPSET('GA1HSET')         GA1HPGM 
00379         INTO(GA1HI01I) END-EXEC.                                  GA1HPGM 
00380                                                                   GA1HPGM 
00381      IF SCRNIDNI  NOT =  '001H00'                                 GA1HPGM 
00382         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1HPGM 
00383                                                                   GA1HPGM 
00384      IF EIBAID  =  DFHENTER                                       GA1HPGM 
00385         PERFORM 2000-DELETE-PROCESSING                            GA1HPGM 
00386         GO TO 1099-RETURN.                                        GA1HPGM 
00387                                                                   GA1HPGM 
00388      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1HPGM 
00389         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1HPGM 
00390                                                                   GA1HPGM 
00391      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1HPGM 
00392         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1HPGM 
00393                                                                   GA1HPGM 
00394      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1HPGM 
00395      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1HPGM 
00396      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1HPGM 
00397 -    'THIS PROGRAM ***'  TO  ERRMSGO.                             GA1HPGM 
00398      EXEC CICS SEND   MAP('GA1HI01') MAPSET('GA1HSET') DATAONLY   GA1HPGM 
00399         FROM(GA1HI01O) CURSOR END-EXEC.                           GA1HPGM 
00400                                                                   GA1HPGM 
00401  1099-RETURN.                                                     GA1HPGM 
00402      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA1HPGM 
00403         (DELADD-OPTION = 'GAS1UPD') OR                            GA1HPGM 
00404         (DELADD-OPTION = 'GAS2UPD') OR                            GA1HPGM 
00405         (DELADD-OPTION = 'GAS3UPD') OR                            GA1HPGM 
00406         (DELADD-OPTION = 'GAS4UPD') OR                            GA1HPGM 
00407         (DELADD-OPTION = 'GAS5UPD')                               GA1HPGM 
00408          EXEC CICS RETURN   END-EXEC                              GA1HPGM 
00409      ELSE                                                         GA1HPGM 
00410          EXEC CICS RETURN TRANSID('GA1H')                         GA1HPGM 
00411                    COMMAREA(DFHCOMMAREA)                          GA1HPGM 
00412                    LENGTH  (EIBCALEN)                             GA1HPGM 
00413                    END-EXEC.                                      GA1HPGM 
00414                                                                   GA1HPGM 
00415      GOBACK.                                                      GA1HPGM 
00416                                                                   GA1HPGM 
00417  1999-EXIT.                                                       GA1HPGM 
00418       EXIT.                                                       GA1HPGM 
00419 /*****************************************************************GA1HPGM 
00420 **              D E L E T E   P R O C E S S I N G                 GA1HPGM 
00421 **                                                                GA1HPGM 
00422 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1HPGM 
00423 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1HPGM 
00424 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1HPGM 
00425 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1HPGM 
00426 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1HPGM 
00427 **    BACK INTO THE RECORD THAT WE READ.)                         GA1HPGM 
00428 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1HPGM 
00429 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1HPGM 
00430 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1HPGM 
00431 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1HPGM 
00432 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1HPGM 
00433 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1HPGM 
00434 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1HPGM 
00435 **    ENTRY.                                                      GA1HPGM 
00436 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1HPGM 
00437 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1HPGM 
00438 **    RECORD.                                                     GA1HPGM 
00439 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1HPGM 
00440 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1HPGM 
00441 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1HPGM 
00442 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1HPGM 
00443 **    ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1HPGM 
00444 **    SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1HPGM 
00445 **    BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1HPGM 
00446 **                                                                GA1HPGM 
00447 ******************************************************************GA1HPGM 
00448  2000-DELETE-PROCESSING SECTION.                                  GA1HPGM 
00449                                                                   GA1HPGM 
00450      MOVE '2000'  TO  WS-PARA-ID.                                 GA1HPGM 
00451      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1HPGM 
00452      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1HPGM 
00453      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1HPGM 
00454                                                                   GA1HPGM 
00455      MOVE '2010'  TO  WS-PARA-ID.                                 GA1HPGM 
00456  2010-VALIDATE-ACT-CODE.                                          GA1HPGM 
00457      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1HPGM 
00458         ADD 1  TO  WS-DELETE-COUNT.                               GA1HPGM 
00459      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D' OR           GA1HPGM 
00460         = SPACE OR =  LOW-VALUES                                  GA1HPGM 
00461         MOVE DFHBMUNF  TO                                         GA1HPGM 
00462            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1HPGM 
00463 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00464 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00465 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00466         MOVE DFHBMASF  TO                                         GA1HPGM 
00467            MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)     GA1HPGM 
00468      ELSE                                                         GA1HPGM 
00469         MOVE DFHBMUBF  TO                                         GA1HPGM 
00470            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1HPGM 
00471         MOVE DFHBMABF  TO                                         GA1HPGM 
00472            MAP-PROVIDER-NO-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)     GA1HPGM 
00473 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00474         IF WS-ERROR-SW  NOT =  'Y'                                GA1HPGM 
00475            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1HPGM 
00476            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1HPGM 
00477                                                                   GA1HPGM 
00478      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1HPGM 
00479         SET MAP-IDX1   UP BY  1                                   GA1HPGM 
00480      ELSE                                                         GA1HPGM 
00481         IF MAP-IDX2  <  WS-MAP-COL                                GA1HPGM 
00482            SET MAP-IDX1  TO  1                                    GA1HPGM 
00483            SET MAP-IDX2  UP BY  1                                 GA1HPGM 
00484         ELSE                                                      GA1HPGM 
00485            GO TO 2020-DONE-VALIDATE-A-C.                          GA1HPGM 
00486 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00487 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00488 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00489      IF MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2)           GA1HPGM 
00490         NOT =  LOW-VALUES                                         GA1HPGM 
00491 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00492         GO TO 2010-VALIDATE-ACT-CODE.                             GA1HPGM 
00493                                                                   GA1HPGM 
00494  2020-DONE-VALIDATE-A-C.                                          GA1HPGM 
00495      MOVE '2020'  TO  WS-PARA-ID.                                 GA1HPGM 
00496      SET MAP-IDX1   TO  1.                                        GA1HPGM 
00497                                                                   GA1HPGM 
00498      IF WS-ERROR-SW  =  'Y'                                       GA1HPGM 
00499         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1HPGM 
00500            ERRMSGO                                                GA1HPGM 
00501         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1HPGM 
00502            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA1HPGM 
00503            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA1HPGM 
00504 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00505 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1HPGM 
00506 **  ADD ITS MAP FIELD NAME HERE.                                  GA1HPGM 
00507 ****************************************************************  GA1HPGM 
00508            INCEXCO                                                GA1HPGM 
00509         MOVE '2100'  TO  WS-PARA-ID                               GA1HPGM 
00510         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1HPGM 
00511            VARYING MAP-IDX2 FROM  1  BY  1                        GA1HPGM 
00512                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1HPGM 
00513              AFTER MAP-IDX1 FROM  1  BY  1                        GA1HPGM 
00514                             UNTIL MAP-IDX1  >  WS-MAP-ROW         GA1HPGM 
00515         EXEC CICS SEND   MAP('GA1HI01') MAPSET('GA1HSET') DATAONLYGA1HPGM 
00516            FROM(GA1HI01O) CURSOR END-EXEC                         GA1HPGM 
00517         GO TO 2099-EXIT.                                          GA1HPGM 
00518                                                                   GA1HPGM 
00519      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1HPGM 
00520               GC-GCIOPARM-LEN                 +                   GA1HPGM 
00521               GC-WORKFILE-KEY-LEN             +                   GA1HPGM 
00522               GC-GCTABULR-IPGN-FIXED-LEN      +                   GA1HPGM 
00523              (GC-GCTABULR-IPGN-VARY-LEN       *                   GA1HPGM 
00524               GC-GCTABULR-IPGN-VARY-MAX-OCUR)                     GA1HPGM 
00525                                                                   GA1HPGM 
00526      EXEC CICS                                                    GA1HPGM 
00527           GETMAIN SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)     GA1HPGM 
00528           INITIMG(WS-HEX-00)                                      GA1HPGM 
00529           LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                  GA1HPGM 
00530      END-EXEC.                                                    GA1HPGM 
00531                                                                   GA1HPGM 
00532      IF  FRMNUIDI  =  'GS3A'                                      GA1HPGM 
00533         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1HPGM 
00534         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1HPGM 
00535         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA1HPGM 
00536         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1HPGM 
00537 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1HPGM 
00538         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1HPGM 
00539 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1HPGM 
00540         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1HPGM 
00541         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1HPGM 
00542         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1HPGM 
00543                          GCIO-WRK-PROVIDER-CONTROL                GA1HPGM 
00544         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1HPGM 
00545         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1HPGM 
00546                                                                   GA1HPGM 
00547      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1HPGM 
00548         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1HPGM 
00549         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1HPGM 
00550         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1HPGM 
00551         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1HPGM 
00552 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1HPGM 
00553         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1HPGM 
00554 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1HPGM 
00555         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1HPGM 
00556         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1HPGM 
00557         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1HPGM 
00558         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1HPGM 
00559         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1HPGM 
00560         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1HPGM 
00561                                                                   GA1HPGM 
00562      IF  FRMNUIDI  =  'GC8A'                                      GA1HPGM 
00563         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1HPGM 
00564         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1HPGM 
00565         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA1HPGM 
00566         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1HPGM 
00567 *AB*****MOVE GCA-GROUP-NO-1-3 TO GCIO-WRK-GROUP-NO-1-3            GA1HPGM 
00568         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1HPGM 
00569 *AB*****MOVE GCA-SEC-NO-1 TO GCIO-WRK-SEC-NO-1                    GA1HPGM 
00570         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1HPGM 
00571         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1HPGM 
00572         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1HPGM 
00573         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1HPGM 
00574         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1HPGM 
00575         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1HPGM 
00576                                                                   GA1HPGM 
00577      MOVE  GC-GCPSWORK-DDNAME TO  GCIO-FILE-DDNAME.               GA1HPGM 
00578      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1HPGM 
00579      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA1HPGM 
00580      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA1HPGM 
00581      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA1HPGM 
00582      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA1HPGM 
00583      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1HPGM 
00584                                                                   GA1HPGM 
00585      IF WS-DELETE-COUNT  =  ZERO                                  GA1HPGM 
00586         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1HPGM 
00587                                                                   GA1HPGM 
00588 ******************************************************************GA1HPGM 
00589 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1HPGM 
00590 *                                                                 GA1HPGM 
00591 ******************************************************************GA1HPGM 
00592                                                                   GA1HPGM 
00593      MOVE  GC-GCTABULR-IPGN-VARY-MAX-OCUR                         GA1HPGM 
00594            TO  GX2-ENTRY-COUNT.                                   GA1HPGM 
00595                                                                   GA1HPGM 
00596      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1HPGM 
00597                                                                   GA1HPGM 
00598      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1HPGM 
00599         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1HPGM 
00600         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1HPGM 
00601                                                                   GA1HPGM 
00602      IF  NOT GCIO-GOOD-RETURN                                     GA1HPGM 
00603         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1HPGM 
00604 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1HPGM 
00605         MOVE '1HF1'  TO  WS-ABEND-CODE                            GA1HPGM 
00606         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1HPGM 
00607                                                                   GA1HPGM 
00608      COMPUTE WS-COPY-LENGTH  =                                    GA1HPGM 
00609              GX2-ENTRY-COUNT  *  GC-GCTABULR-IPGN-VARY-LEN.       GA1HPGM 
00610                                                                   GA1HPGM 
00611      EXEC CICS                                                    GA1HPGM 
00612           GETMAIN SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)         GA1HPGM 
00613           INITIMG(WS-HEX-00)                                      GA1HPGM 
00614           LENGTH(WS-COPY-LENGTH)                                  GA1HPGM 
00615      END-EXEC.                                                    GA1HPGM 
00616                                                                   GA1HPGM 
00617      MOVE GX2-ENTRY-COUNT  TO  GX2-ENTRY-COUNT.                   GA1HPGM 
00618      SET COPY-IDX,  GX2-INDEX  TO  1.                             GA1HPGM 
00619                                                                   GA1HPGM 
00620      MOVE '2030'  TO  WS-PARA-ID.                                 GA1HPGM 
00621  2030-MAKE-A-COPY-OF-RECORD.                                      GA1HPGM 
00622      IF GX2-INDEX  NOT >  GX2-ENTRY-COUNT                         GA1HPGM 
00623         MOVE GX2-ENTRY (GX2-INDEX)  TO                            GA1HPGM 
00624            COPY-TABULAR-TABLE (COPY-IDX)                          GA1HPGM 
00625            SET COPY-IDX,  GX2-INDEX  UP BY  1                     GA1HPGM 
00626            GO TO 2030-MAKE-A-COPY-OF-RECORD.                      GA1HPGM 
00627      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GX2-INDEX  TO  1.         GA1HPGM 
00628                                                                   GA1HPGM 
00629      MOVE '2040'  TO  WS-PARA-ID.                                 GA1HPGM 
00630  2040-DELETE-MARKED-ENTRIES.                                      GA1HPGM 
00631 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00632 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00633 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00634      IF MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2)  =        GA1HPGM 
00635            LOW-VALUES                                             GA1HPGM 
00636         GO TO 2060-SAVE-REST-OF-COPY.                             GA1HPGM 
00637                                                                   GA1HPGM 
00638      IF MAP-PROVIDER-NO-ARGUMENT (MAP-IDX1, MAP-IDX2)  >          GA1HPGM 
00639         COPY-PROVIDER-NO-ARGUMENT (COPY-IDX)                      GA1HPGM 
00640         GO TO 2050-SAVE-COPIED-ENTRY                              GA1HPGM 
00641      ELSE                                                         GA1HPGM 
00642         IF MAP-PROVIDER-NO-ARGUMENT (MAP-IDX1, MAP-IDX2)  <       GA1HPGM 
00643            COPY-PROVIDER-NO-ARGUMENT (COPY-IDX)                   GA1HPGM 
00644            MOVE '1HL1'  TO  WS-ABEND-CODE                         GA1HPGM 
00645            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1HPGM 
00646 -    'RM SYSTEMS AREA ***'  TO  ERRMSGO                           GA1HPGM 
00647            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1HPGM 
00648                                                                   GA1HPGM 
00649 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00650                                                                   GA1HPGM 
00651      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1HPGM 
00652         IF MAP-IDX1   <  WS-MAP-ROW                               GA1HPGM 
00653            SET MAP-IDX1   UP BY  1                                GA1HPGM 
00654            GO TO 2050-SAVE-COPIED-ENTRY                           GA1HPGM 
00655         ELSE                                                      GA1HPGM 
00656            IF MAP-IDX2  <  WS-MAP-COL                             GA1HPGM 
00657               SET MAP-IDX1  TO  1                                 GA1HPGM 
00658               SET MAP-IDX2  UP BY  1                              GA1HPGM 
00659               GO TO 2050-SAVE-COPIED-ENTRY                        GA1HPGM 
00660            ELSE                                                   GA1HPGM 
00661               GO TO 2060-SAVE-REST-OF-COPY.                       GA1HPGM 
00662                                                                   GA1HPGM 
00663      SET COPY-IDX  UP BY  1.                                      GA1HPGM 
00664      IF COPY-IDX  NOT <  GX2-ENTRY-COUNT                          GA1HPGM 
00665         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    GA1HPGM 
00666            GX2-ENTRY (GX2-INDEX)                                  GA1HPGM 
00667         SET  GX2-ENTRY-COUNT  TO  GX2-INDEX                       GA1HPGM 
00668         MOVE GX2-ENTRY-COUNT  TO  GX2-ENTRY-COUNT                 GA1HPGM 
00669         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1HPGM 
00670      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1HPGM 
00671         SET MAP-IDX1   UP BY  1                                   GA1HPGM 
00672         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1HPGM 
00673      IF MAP-IDX2  <  WS-MAP-COL                                   GA1HPGM 
00674         SET MAP-IDX1  TO  1                                       GA1HPGM 
00675         SET MAP-IDX2  UP BY  1                                    GA1HPGM 
00676         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1HPGM 
00677      ELSE                                                         GA1HPGM 
00678         GO TO 2060-SAVE-REST-OF-COPY.                             GA1HPGM 
00679                                                                   GA1HPGM 
00680  2050-SAVE-COPIED-ENTRY.                                          GA1HPGM 
00681      MOVE '2050'  TO  WS-PARA-ID.                                 GA1HPGM 
00682      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1HPGM 
00683         GX2-ENTRY (GX2-INDEX).                                    GA1HPGM 
00684                                                                   GA1HPGM 
00685      SET GX2-INDEX  UP BY  1.                                     GA1HPGM 
00686      IF COPY-IDX  <  GX2-ENTRY-COUNT                              GA1HPGM 
00687         SET COPY-IDX  UP BY  1                                    GA1HPGM 
00688         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1HPGM 
00689      ELSE                                                         GA1HPGM 
00690 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1HPGM 
00691 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1HPGM 
00692 ***      THE TABLE OF ENTRIES.                                    GA1HPGM 
00693         MOVE '1HL2'  TO  WS-ABEND-CODE                            GA1HPGM 
00694         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1HPGM 
00695 -    'SYSTEMS AREA ***'  TO  ERRMSGO                              GA1HPGM 
00696         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1HPGM 
00697                                                                   GA1HPGM 
00698  2060-SAVE-REST-OF-COPY.                                          GA1HPGM 
00699      MOVE '2060'  TO  WS-PARA-ID.                                 GA1HPGM 
00700      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1HPGM 
00701         GX2-ENTRY (GX2-INDEX).                                    GA1HPGM 
00702                                                                   GA1HPGM 
00703      SET GX2-INDEX  UP BY  1.                                     GA1HPGM 
00704      IF COPY-IDX  <  GX2-ENTRY-COUNT                              GA1HPGM 
00705         SET COPY-IDX  UP BY  1                                    GA1HPGM 
00706         GO TO 2060-SAVE-REST-OF-COPY.                             GA1HPGM 
00707                                                                   GA1HPGM 
00708      SET GX2-INDEX  DOWN BY  1.                                   GA1HPGM 
00709      SET GX2-ENTRY-COUNT  TO  GX2-INDEX.                          GA1HPGM 
00710      MOVE GX2-ENTRY-COUNT  TO  GX2-ENTRY-COUNT.                   GA1HPGM 
00711                                                                   GA1HPGM 
00712  2070-UPDATE-MODIFIED-REC.                                        GA1HPGM 
00713      MOVE '2070'  TO  WS-PARA-ID.                                 GA1HPGM 
00714                                                                   GA1HPGM 
00715 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1HPGM 
00716                                                                   GA1HPGM 
00717      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1HPGM 
00718      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1HPGM 
00719                                                                   GA1HPGM 
00720      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1HPGM 
00721               GC-WORKFILE-KEY-LEN         +                       GA1HPGM 
00722               GC-GCTABULR-IPGN-FIXED-LEN  +                       GA1HPGM 
00723               (GC-GCTABULR-IPGN-VARY-LEN   * GX2-ENTRY-COUNT).    GA1HPGM 
00724                                                                   GA1HPGM 
00725      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1HPGM 
00726         GC-GCIOPARM-LEN   +  GCIO-RECORD-LENGTH.                  GA1HPGM 
00727                                                                   GA1HPGM 
00728      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1HPGM 
00729         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1HPGM 
00730         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1HPGM 
00731                                                                   GA1HPGM 
00732      IF GCIO-GOOD-RETURN                                          GA1HPGM 
00733         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1HPGM 
00734      MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR.  CONTACGA1HPGM 
00735 -    'T SYSTEMS AREA ***'  TO  ERRMSGO.                           GA1HPGM 
00736      MOVE '1HF2'  TO  WS-ABEND-CODE.                              GA1HPGM 
00737      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1HPGM 
00738                                                                   GA1HPGM 
00739  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1HPGM 
00740      MOVE  '2080'  TO  WS-PARA-ID.                                GA1HPGM 
00741      MOVE  GC-GCTABULR-IPGN-VARY-MAX-OCUR                         GA1HPGM 
00742            TO  GX2-ENTRY-COUNT.                                   GA1HPGM 
00743                                                                   GA1HPGM 
00744      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1HPGM 
00745                                                                   GA1HPGM 
00746      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1HPGM 
00747         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1HPGM 
00748         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1HPGM 
00749                                                                   GA1HPGM 
00750      IF GCIO-GOOD-RETURN                                          GA1HPGM 
00751         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1HPGM 
00752      MOVE '1HF3'  TO  WS-ABEND-CODE.                              GA1HPGM 
00753      MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTACT GA1HPGM 
00754 -    'SYSTEMS AREA ***'  TO  ERRMSGO.                             GA1HPGM 
00755      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1HPGM 
00756                                                                   GA1HPGM 
00757  2090-BUILD-NEXT-DISPLAY.                                         GA1HPGM 
00758      MOVE  '2090'  TO  WS-PARA-ID.                                GA1HPGM 
00759      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1HPGM 
00760      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1HPGM 
00761      SET GX2-INDEX  TO  1.                                        GA1HPGM 
00762 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00763 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00764 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00765      IF MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2)  =        GA1HPGM 
00766            LOW-VALUES                                             GA1HPGM 
00767         MOVE GX2-ENTRY (GX2-INDEX)  TO  WS-SAVED-FIELDS           GA1HPGM 
00768      ELSE                                                         GA1HPGM 
00769         MOVE MAP-PROVIDER-NO-ARGUMENT (MAP-IDX1, MAP-IDX2) TO     GA1HPGM 
00770            WS-SAVED-PROVIDER-NO-ARGUMENT.                         GA1HPGM 
00771 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00772                                                                   GA1HPGM 
00773      PERFORM 4500-FILL-THE-SCREEN.                                GA1HPGM 
00774      EXEC CICS SEND   MAP('GA1HI01') MAPSET('GA1HSET') ERASE      GA1HPGM 
00775         FROM(GA1HI01O) END-EXEC.                                  GA1HPGM 
00776                                                                   GA1HPGM 
00777  2099-EXIT.   EXIT.                                               GA1HPGM 
00778      EJECT                                                        GA1HPGM 
00779  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1HPGM 
00780 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00781 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
00782 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00783      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1HPGM 
00784         MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2).          GA1HPGM 
00785 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00786                                                                   GA1HPGM 
00787  2199-EXIT.   EXIT.                                               GA1HPGM 
00788      EJECT                                                        GA1HPGM 
00789 ******************************************************************GA1HPGM 
00790 **          X C T L   T O   A D D   S C R E E N                   GA1HPGM 
00791 **                                                                GA1HPGM 
00792 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1HPGM 
00793 ** ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1HPGM 
00794 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1HPGM 
00795 ** TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1HPGM 
00796 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1HPGM 
00797 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1HPGM 
00798 ******************************************************************GA1HPGM 
00799  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1HPGM 
00800      MOVE '3000'  TO  WS-PARA-ID.                                 GA1HPGM 
00801                                                                   GA1HPGM 
00802 *    EXEC CICS                                                    GA1HPGM 
00803 *         GETMAIN SET(ADDRESS OF GCA-COMMAREA)                    GA1HPGM 
00804 *         INITIMG(WS-HEX-00)                                      GA1HPGM 
00805 *         LENGTH(WS-COMMUNICATION-KEY-LEN)                        GA1HPGM 
00806 *    END-EXEC.                                                    GA1HPGM 
00807                                                                   GA1HPGM 
00808 *    IF  FRMNUIDI  =  'GS3A'                                      GA1HPGM 
00809 ***     MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA1HPGM 
00810 *       MOVE  GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                   GA1HPGM 
00811 *       MOVE  GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO               GA1HPGM 
00812 *       MOVE  GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1HPGM 
00813 *       MOVE  GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1HPGM 
00814 *       MOVE  SPACES  TO  GCA-L-O-B,                              GA1HPGM 
00815 *                         GCA-PROV-CTL,                           GA1HPGM 
00816 *                         GCA-BEN-PROV-ID.                        GA1HPGM 
00817                                                                   GA1HPGM 
00818 *    IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1HPGM 
00819 ***     MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA1HPGM 
00820 *       MOVE  CONTRACT-GROUP-NO  TO  GCA-GRP-NO                   GA1HPGM 
00821 *       MOVE  CONTRACT-SECTION-NO  TO  GCA-SECTN-NO               GA1HPGM 
00822 *       MOVE  CONTRACT-LOB  TO  GCA-L-O-B                         GA1HPGM 
00823 *       MOVE  CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                 GA1HPGM 
00824 *       MOVE  CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1HPGM 
00825 *       MOVE  CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1HPGM 
00826 *       MOVE  SPACES  TO  GCA-BEN-PROV-ID.                        GA1HPGM 
00827                                                                   GA1HPGM 
00828 *    IF  FRMNUIDI  =  'GC8A'                                      GA1HPGM 
00829 ***     MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA1HPGM 
00830 *       MOVE  BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                   GA1HPGM 
00831 *       MOVE  BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO               GA1HPGM 
00832 *       MOVE  BEN-PROV-LOB  TO  GCA-L-O-B                         GA1HPGM 
00833 *       MOVE  BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                 GA1HPGM 
00834 *       MOVE  BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1HPGM 
00835 *       MOVE  BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1HPGM 
00836 *       MOVE  BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                GA1HPGM 
00837                                                                   GA1HPGM 
00838      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1HPGM 
00839      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1HPGM 
00840      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1HPGM 
00841      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1HPGM 
00842      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1HPGM 
00843      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1HPGM 
00844      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1HPGM 
00845      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1HPGM 
00846 *    MOVE  ZEROES  TO  GCA-EFF-DT.                                GA1HPGM 
00847 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00848 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA1HPGM 
00849 ** FIELDS TO DISPLAY ON THE INITIAL ADD SCREEN THEY SHOULD BE     GA1HPGM 
00850 ** PASSED HERE.                                                   GA1HPGM 
00851 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00852      MOVE INCEXCI TO GCA-I-E-INDC.                                GA1HPGM 
00853                                                                   GA1HPGM 
00854 *    SET  COMMAREA-PNTR   TO                                      GA1HPGM 
00855 *         ADDRESS OF GCA-COMMAREA.                                GA1HPGM 
00856                                                                   GA1HPGM 
00857 *    EXEC CICS XCTL  PROGRAM('GA2HPGM') COMMAREA(COMMAREA-PNTR)   GA1HPGM 
00858 *       LENGTH(4) END-EXEC.                                       GA1HPGM 
00859      EXEC CICS XCTL PROGRAM('GA2HPGM')                            GA1HPGM 
00860                     COMMAREA(DFHCOMMAREA)                         GA1HPGM 
00861                     LENGTH(LENGTH OF DFHCOMMAREA)                 GA1HPGM 
00862      END-EXEC.                                                    GA1HPGM 
00863                                                                   GA1HPGM 
00864  3099-EXIT.   EXIT.                                               GA1HPGM 
00865      EJECT                                                        GA1HPGM 
00866 ***************************************************************** GA1HPGM 
00867 **          D I S P L A Y   F I R S T   S C R E E N               GA1HPGM 
00868 **                                                                GA1HPGM 
00869 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1HPGM 
00870 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1HPGM 
00871 ** ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1HPGM 
00872 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1HPGM 
00873 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1HPGM 
00874 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1HPGM 
00875 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1HPGM 
00876 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1HPGM 
00877 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1HPGM 
00878 ** DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1HPGM 
00879 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1HPGM 
00880 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1HPGM 
00881 ******************************************************************GA1HPGM 
00882  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1HPGM 
00883      MOVE '4000'  TO  WS-PARA-ID.                                 GA1HPGM 
00884                                                                   GA1HPGM 
00885      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1HPGM 
00886         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1HPGM 
00887            TO ERRMSGO                                             GA1HPGM 
00888         MOVE '1HC1'  TO  WS-ABEND-CODE                            GA1HPGM 
00889         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1HPGM 
00890                                                                   GA1HPGM 
00891 *    SET ADDRESS  OF  GCA-COMMAREA                                GA1HPGM 
00892 *        TO  INCOMING-COMMAREA-PNTR.                              GA1HPGM 
00893                                                                   GA1HPGM 
00894      SET ADDRESS  OF  IO-PARM-INTERNAL-TAB-RECORD                 GA1HPGM 
00895          TO  GCA-RECORD-POINTER.                                  GA1HPGM 
00896                                                                   GA1HPGM 
00897      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA1HPGM 
00898      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA1HPGM 
00899      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA1HPGM 
00900      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA1HPGM 
00901      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA1HPGM 
00902      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA1HPGM 
00903      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA1HPGM 
00904      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA1HPGM 
00905 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00906 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1HPGM 
00907 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1HPGM 
00908 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
00909      MOVE GX2-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               GA1HPGM 
00910      MOVE GCA-I-E-INDC  TO  INCEXCO.                              GA1HPGM 
00911                                                                   GA1HPGM 
00912      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1HPGM 
00913         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA1HPGM 
00914 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA1HPGM 
00915         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA1HPGM 
00916         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA1HPGM 
00917         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA1HPGM 
00918         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA1HPGM 
00919         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1HPGM 
00920         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA1HPGM 
00921         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA1HPGM 
00922         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA1HPGM 
00923         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1HPGM 
00924         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1HPGM 
00925         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1HPGM 
00926         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1HPGM 
00927                                                                   GA1HPGM 
00928      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1HPGM 
00929         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA1HPGM 
00930 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1HPGM 
00931         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA1HPGM 
00932         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA1HPGM 
00933         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA1HPGM 
00934         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA1HPGM 
00935         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1HPGM 
00936         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA1HPGM 
00937         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA1HPGM 
00938         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA1HPGM 
00939         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1HPGM 
00940         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1HPGM 
00941         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1HPGM 
00942         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1HPGM 
00943         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1HPGM 
00944         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1HPGM 
00945         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1HPGM 
00946         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1HPGM 
00947                                                                   GA1HPGM 
00948      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1HPGM 
00949         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA1HPGM 
00950         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA1HPGM 
00951         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA1HPGM 
00952         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA1HPGM 
00953         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA1HPGM 
00954         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA1HPGM 
00955         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA1HPGM 
00956         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA1HPGM 
00957         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA1HPGM 
00958         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA1HPGM 
00959         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1HPGM 
00960         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA1HPGM 
00961         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1HPGM 
00962         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA1HPGM 
00963         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1HPGM 
00964         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA1HPGM 
00965         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1HPGM 
00966         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA1HPGM 
00967         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1HPGM 
00968                                                                   GA1HPGM 
00969      SET GX2-INDEX  TO  1.                                        GA1HPGM 
00970      MOVE GX2-ENTRY (GX2-INDEX)  TO  WS-SAVED-FIELDS.             GA1HPGM 
00971                                                                   GA1HPGM 
00972      PERFORM 4500-FILL-THE-SCREEN.                                GA1HPGM 
00973      EXEC CICS SEND   MAP('GA1HI01') MAPSET('GA1HSET') ERASE      GA1HPGM 
00974         FROM(GA1HI01O) END-EXEC.                                  GA1HPGM 
00975                                                                   GA1HPGM 
00976  4099-EXIT.   EXIT.                                               GA1HPGM 
00977      EJECT                                                        GA1HPGM 
00978 ***************************************************************** GA1HPGM 
00979 **             F I L L   T H E   S C R E E N                      GA1HPGM 
00980 **                                                                GA1HPGM 
00981 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1HPGM 
00982 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1HPGM 
00983 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1HPGM 
00984 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1HPGM 
00985 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1HPGM 
00986 ******************************************************************GA1HPGM 
00987  4500-FILL-THE-SCREEN SECTION.                                    GA1HPGM 
00988                                                                   GA1HPGM 
00989      MOVE '4500'  TO  WS-PARA-ID.                                 GA1HPGM 
00990      MOVE  GX2-ENTRY-COUNT  TO  GX2-ENTRY-COUNT.                  GA1HPGM 
00991      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1HPGM 
00992                                                                   GA1HPGM 
00993      IF GX2-ENTRY-COUNT  NOT >  1                                 GA1HPGM 
00994         MOVE '4530'  TO  WS-PARA-ID                               GA1HPGM 
00995         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1HPGM 
00996                                                                   GA1HPGM 
00997      SET GX2-INDEX  TO  1.                                        GA1HPGM 
00998      MOVE '4510'  TO  WS-PARA-ID.                                 GA1HPGM 
00999  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1HPGM 
01000 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01001 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
01002 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01003      IF GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX)  <                   GA1HPGM 
01004            WS-SAVED-PROVIDER-NO-ARGUMENT                          GA1HPGM 
01005 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01006         SET GX2-INDEX  UP BY  1                                   GA1HPGM 
01007         IF  GX2-INDEX  <  GX2-ENTRY-COUNT                         GA1HPGM 
01008            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1HPGM 
01009         ELSE                                                      GA1HPGM 
01010            SET GX2-INDEX  TO  1.                                  GA1HPGM 
01011                                                                   GA1HPGM 
01012      MOVE '4520'  TO  WS-PARA-ID.                                 GA1HPGM 
01013  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1HPGM 
01014      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1HPGM 
01015      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1HPGM 
01016 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01017 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
01018 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01019      MOVE GX2-PROVIDER-NO-ARGUMENT (GX2-INDEX)  TO                GA1HPGM 
01020         MAP-PROVIDER-NO-ARGUMENT (MAP-IDX1, MAP-IDX2).            GA1HPGM 
01021 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01022                                                                   GA1HPGM 
01023      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1HPGM 
01024         SET  MAP-IDX1  UP BY  1                                   GA1HPGM 
01025      ELSE                                                         GA1HPGM 
01026         IF MAP-IDX2  <  WS-MAP-COL                                GA1HPGM 
01027            SET  MAP-IDX1  TO  1                                   GA1HPGM 
01028            SET  MAP-IDX2  UP BY  1                                GA1HPGM 
01029         ELSE                                                      GA1HPGM 
01030            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1HPGM 
01031                                                                   GA1HPGM 
01032      IF GX2-INDEX  <  (GX2-ENTRY-COUNT - 1 )                      GA1HPGM 
01033         SET  GX2-INDEX  UP BY  1                                  GA1HPGM 
01034         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1HPGM 
01035                                                                   GA1HPGM 
01036      MOVE '4530'  TO  WS-PARA-ID.                                 GA1HPGM 
01037  4530-FILL-REST-WITH-NULLS.                                       GA1HPGM 
01038      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1HPGM 
01039 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01040 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1HPGM 
01041 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01042      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1HPGM 
01043         MAP-PROVIDER-NO-ARGUMENT   (MAP-IDX1, MAP-IDX2).          GA1HPGM 
01044 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1HPGM 
01045      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1HPGM 
01046         SET  MAP-IDX1   UP BY  1                                  GA1HPGM 
01047         GO TO 4530-FILL-REST-WITH-NULLS                           GA1HPGM 
01048      ELSE                                                         GA1HPGM 
01049         IF MAP-IDX2  <  WS-MAP-COL                                GA1HPGM 
01050            SET  MAP-IDX1  TO  1                                   GA1HPGM 
01051            SET  MAP-IDX2  UP BY 1                                 GA1HPGM 
01052            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1HPGM 
01053                                                                   GA1HPGM 
01054      MOVE '4540'  TO  WS-PARA-ID.                                 GA1HPGM 
01055  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1HPGM 
01056      IF GX2-ENTRY-COUNT  =  1                                     GA1HPGM 
01057         MOVE '*** NO ENTRIES TO DELETE ***'  TO  ERRMSGO          GA1HPGM 
01058         GO TO 4599-EXIT.                                          GA1HPGM 
01059                                                                   GA1HPGM 
01060      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1HPGM 
01061         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'  TO  ERRMSGO.   GA1HPGM 
01062                                                                   GA1HPGM 
01063  4599-EXIT.     EXIT.                                             GA1HPGM 
01064      EJECT                                                        GA1HPGM 
01065 ***************************************************************** GA1HPGM 
01066 **        X C T L   T O   P R E V I O U S   M E N U               GA1HPGM 
01067 **                                                                GA1HPGM 
01068 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1HPGM 
01069 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1HPGM 
01070 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1HPGM 
01071 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1HPGM 
01072 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1HPGM 
01073 ******************************************************************GA1HPGM 
01074  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1HPGM 
01075      MOVE '5000'  TO  WS-PARA-ID.                                 GA1HPGM 
01076                                                                   GA1HPGM 
01077                                                                   GA1HPGM 
01078 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA1HPGM 
01079 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA1HPGM 
01080 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA1HPGM 
01081                                                                   GA1HPGM 
01082      IF  ALTBFNCI  =  'GTM1'                                      GA1HPGM 
01083          EXEC CICS XCTL                                           GA1HPGM 
01084                    PROGRAM('GTM1PGM')                             GA1HPGM 
01085                    END-EXEC.                                      GA1HPGM 
01086                                                                   GA1HPGM 
01087      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN      =                   GA1HPGM 
01088               GC-GCIOPARM-LEN                 +                   GA1HPGM 
01089               GC-WORKFILE-KEY-LEN             +                   GA1HPGM 
01090               GC-GCTABULR-ABM-FIXED-LEN       +                   GA1HPGM 
01091              (GC-GCTABULR-ABM-VARY-LEN        *                   GA1HPGM 
01092               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA1HPGM 
01093                                                                   GA1HPGM 
01094      EXEC CICS                                                    GA1HPGM 
01095           GETMAIN SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)        GA1HPGM 
01096           INITIMG(WS-HEX-00)                                      GA1HPGM 
01097           LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                      GA1HPGM 
01098      END-EXEC.                                                    GA1HPGM 
01099                                                                   GA1HPGM 
01100 *    EXEC CICS                                                    GA1HPGM 
01101 *         GETMAIN SET(ADDRESS OF GCA-COMMAREA)                    GA1HPGM 
01102 *         INITIMG(WS-HEX-00)                                      GA1HPGM 
01103 *         LENGTH(WS-COMMUNICATION-KEY-LEN)                        GA1HPGM 
01104 *    END-EXEC.                                                    GA1HPGM 
01105                                                                   GA1HPGM 
01106      IF  FRMNUIDI  =  'GS3A'                                      GA1HPGM 
01107         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1HPGM 
01108         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1HPGM 
01109         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA1HPGM 
01110         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1HPGM 
01111         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1HPGM 
01112         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1HPGM 
01113         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1HPGM 
01114         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1HPGM 
01115                          GCIO-WRK-PROVIDER-CONTROL                GA1HPGM 
01116         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1HPGM 
01117         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1HPGM 
01118         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1HPGM 
01119         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1HPGM 
01120                            GCA-ALL-LEVEL-TAB-ID                   GA1HPGM 
01121         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1HPGM 
01122                            GCA-ALL-LEVEL-TAB-SLOT                 GA1HPGM 
01123         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1HPGM 
01124                            GCA-INTERNAL-TAB-ID,                   GA1HPGM 
01125                            GCA-INTERNAL-TAB-SLOT                  GA1HPGM 
01126         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1HPGM 
01127                                                                   GA1HPGM 
01128      IF  FRMNUIDI  =  'GC4A'                                      GA1HPGM 
01129         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1HPGM 
01130         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1HPGM 
01131         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1HPGM 
01132         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1HPGM 
01133         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1HPGM 
01134         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1HPGM 
01135         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1HPGM 
01136         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1HPGM 
01137         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1HPGM 
01138         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1HPGM 
01139         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1HPGM 
01140         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1HPGM 
01141         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1HPGM 
01142                            GCA-ALL-LEVEL-TAB-ID                   GA1HPGM 
01143         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1HPGM 
01144                            GCA-ALL-LEVEL-TAB-SLOT                 GA1HPGM 
01145         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1HPGM 
01146                            GCA-INTERNAL-TAB-ID,                   GA1HPGM 
01147                            GCA-INTERNAL-TAB-SLOT                  GA1HPGM 
01148         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1HPGM 
01149                                                                   GA1HPGM 
01150      IF  FRMNUIDI  =  'GC8A'                                      GA1HPGM 
01151         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1HPGM 
01152         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1HPGM 
01153         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA1HPGM 
01154         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1HPGM 
01155         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1HPGM 
01156         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1HPGM 
01157         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1HPGM 
01158         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1HPGM 
01159         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1HPGM 
01160         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1HPGM 
01161         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1HPGM 
01162         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA1HPGM 
01163         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1HPGM 
01164         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA1HPGM 
01165                            GCA-ALL-LEVEL-TAB-ID                   GA1HPGM 
01166         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA1HPGM 
01167                            GCA-ALL-LEVEL-TAB-SLOT                 GA1HPGM 
01168         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA1HPGM 
01169                          GCA-INTERNAL-TAB-SLOT.                   GA1HPGM 
01170                                                                   GA1HPGM 
01171      MOVE  GC-GCPSWORK-DDNAME TO  GCIO2-FILE-DDNAME.              GA1HPGM 
01172 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA1HPGM 
01173 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA1HPGM 
01174 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA1HPGM 
01175 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA1HPGM 
01176 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA1HPGM 
01177      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1HPGM 
01178                                                                   GA1HPGM 
01179      SET  GCA-RECORD-POINTER                                      GA1HPGM 
01180           TO  ADDRESS OF  IO-PARM-ALL-LEVEL-RECORD.               GA1HPGM 
01181                                                                   GA1HPGM 
01182      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR                           GA1HPGM 
01183           TO  GAA-ENTRY-COUNT.                                    GA1HPGM 
01184                                                                   GA1HPGM 
01185      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1HPGM 
01186                                                                   GA1HPGM 
01187      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1HPGM 
01188         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA1HPGM 
01189         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA1HPGM 
01190                                                                   GA1HPGM 
01191      IF  NOT GCIO2-GOOD-RETURN                                    GA1HPGM 
01192         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1HPGM 
01193 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1HPGM 
01194         MOVE '1HF4'  TO  WS-ABEND-CODE                            GA1HPGM 
01195         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1HPGM 
01196                                                                   GA1HPGM 
01197 *    SET  COMMAREA-PNTR                                           GA1HPGM 
01198 *         TO  ADDRESS OF GCA-COMMAREA.                            GA1HPGM 
01199                                                                   GA1HPGM 
01200      IF  ALTBFNCI  =  'GA1B'                                      GA1HPGM 
01201 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA1HPGM 
01202 *          LENGTH(4) END-EXEC.                                    GA1HPGM 
01203         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA1HPGM 
01204                         COMMAREA(DFHCOMMAREA)                     GA1HPGM 
01205                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1HPGM 
01206         END-EXEC.                                                 GA1HPGM 
01207                                                                   GA1HPGM 
01208      IF  ALTBFNCI  =  'GA1C'                                      GA1HPGM 
01209 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA1HPGM 
01210 *          LENGTH(4) END-EXEC.                                    GA1HPGM 
01211         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA1HPGM 
01212                         COMMAREA(DFHCOMMAREA)                     GA1HPGM 
01213                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1HPGM 
01214         END-EXEC.                                                 GA1HPGM 
01215                                                                   GA1HPGM 
01216      IF  ALTBFNCI  =  'GA1D'                                      GA1HPGM 
01217 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA1HPGM 
01218 *          LENGTH(4) END-EXEC.                                    GA1HPGM 
01219         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA1HPGM 
01220                         COMMAREA(DFHCOMMAREA)                     GA1HPGM 
01221                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1HPGM 
01222         END-EXEC.                                                 GA1HPGM 
01223                                                                   GA1HPGM 
01224      IF  ALTBFNCI  =  'GA1E'                                      GA1HPGM 
01225 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA1HPGM 
01226 *          LENGTH(4) END-EXEC.                                    GA1HPGM 
01227         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA1HPGM 
01228                         COMMAREA(DFHCOMMAREA)                     GA1HPGM 
01229                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1HPGM 
01230         END-EXEC.                                                 GA1HPGM 
01231                                                                   GA1HPGM 
01232      IF  ALTBFNCI  =  'GA1P'                                      GA1HPGM 
01233         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA1HPGM 
01234                         COMMAREA(DFHCOMMAREA)                     GA1HPGM 
01235                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1HPGM 
01236         END-EXEC.                                                 GA1HPGM 
01237                                                                   GA1HPGM 
01238  5099-EXIT.                                                       GA1HPGM 
01239      EXIT.                                                        GA1HPGM 
01240      EJECT                                                        GA1HPGM 
01241 ***************************************************************** GA1HPGM 
01242 **           X C T L   T O   M A I N   M E N U                    GA1HPGM 
01243 **                                                                GA1HPGM 
01244 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1HPGM 
01245 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1HPGM 
01246 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1HPGM 
01247 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1HPGM 
01248 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1HPGM 
01249 ** AND PROGRESS DOWN.                                             GA1HPGM 
01250 ******************************************************************GA1HPGM 
01251  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1HPGM 
01252      MOVE '6000'  TO  WS-PARA-ID.                                 GA1HPGM 
01253      MOVE '1HP1'  TO  WS-ABEND-CODE.                              GA1HPGM 
01254                                                                   GA1HPGM 
01255      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1HPGM 
01256                                                                   GA1HPGM 
01257  6099-EXIT.     EXIT.                                             GA1HPGM 
01258 /*****************************************************************GA1HPGM 
01259 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA1HPGM 
01260 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA1HPGM 
01261 ******************************************************************GA1HPGM 
01262  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA1HPGM 
01263  9800-010.                                                        GA1HPGM 
01264                                                                   GA1HPGM 
01265      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1HPGM 
01266      MOVE 'M'   TO  HGADATE-FORM1.                                GA1HPGM 
01267      MOVE 'J'   TO  HGADATE-FORM2.                                GA1HPGM 
01268      MOVE ZEROS TO  HGADATE-RETURN                                GA1HPGM 
01269                     HGADATE-AMOUNT.                               GA1HPGM 
01270      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1HPGM 
01271                     COMMAREA(HGADATES-COMMAREA)                   GA1HPGM 
01272                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1HPGM 
01273                     END-EXEC.                                     GA1HPGM 
01274                                                                   GA1HPGM 
01275  9800-900-900-EXIT.                                               GA1HPGM 
01276      EXIT.                                                        GA1HPGM 
01277 /*****************************************************************GA1HPGM 
01278 * 9810    J U L I A N    T O    G R E G O R I A N                *GA1HPGM 
01279 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *GA1HPGM 
01280 ******************************************************************GA1HPGM 
01281  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          GA1HPGM 
01282  9810-010.                                                        GA1HPGM 
01283                                                                   GA1HPGM 
01284      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1HPGM 
01285      MOVE 'J'   TO  HGADATE-FORM1.                                GA1HPGM 
01286      MOVE 'M'   TO  HGADATE-FORM2.                                GA1HPGM 
01287      MOVE ZEROS TO  HGADATE-RETURN                                GA1HPGM 
01288                     HGADATE-AMOUNT.                               GA1HPGM 
01289      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1HPGM 
01290                     COMMAREA(HGADATES-COMMAREA)                   GA1HPGM 
01291                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1HPGM 
01292                     END-EXEC.                                     GA1HPGM 
01293                                                                   GA1HPGM 
01294  9810-900-900-EXIT.                                               GA1HPGM 
01295      EXIT.                                                        GA1HPGM 
01296  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1HPGM 
01297                                                                   GA1HPGM 
01298      SET MAP-IDX1 TO 7.                                           GA1HPGM 
01299      SET MAP-IDX2 TO 1.                                           GA1HPGM 
01300      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1HPGM 
01301                                                                   GA1HPGM 
01302      EXEC CICS SEND   MAP('GA1HI01') MAPSET('GA1HSET') ERASE      GA1HPGM 
01303         FROM(GA1HI01O) WAIT END-EXEC.                             GA1HPGM 
01304                                                                   GA1HPGM 
01305      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1HPGM 
01306                                                                   GA1HPGM 
01307  9999-EXIT.     EXIT.                                             GA1HPGM 
