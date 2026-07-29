00001 *      LAST MAINTENANCE TIME:  8.45.43  DATE: 11/16/84            08/20/03
00002  IDENTIFICATION DIVISION.                                         GA1SPGM 
00003  PROGRAM-ID.     GA1SPGM.                                            LV001
00004 **** THIS IS A COBOL/2 PROGRAM *****                              GA1SPGM 
00005  AUTHOR.         S BUCH.                                          GA1SPGM 
00006  DATE-WRITTEN.   11/13/84.                                        GA1SPGM 
00007  DATE-COMPILED.                                                   GA1SPGM 
00008      SKIP3                                                        GA1SPGM 
00009 ******************************************************************GA1SPGM 
00010 *   GA1SPGM   ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM      GA1SPGM 
00011 *                 PROVIDER-GROUP BY PROVIDER SPECIALTIES - GA1S   GA1SPGM 
00012 *                                                                 GA1SPGM 
00013 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1SPGM 
00014 *   CURRENTLY ON THE ALL LEVEL INTERNAL TABULAR RECORD.           GA1SPGM 
00015 *                                                                 GA1SPGM 
00016 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1SPGM 
00017 *   ALL LEVEL INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN    GA1SPGM 
00018 *   DECIDE IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN     GA1SPGM 
00019 *   ENTRY WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE    GA1SPGM 
00020 *   RECORD WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED   GA1SPGM 
00021 *   BACK INTO THE RECORD BEFORE UPDATING THE RECORD.              GA1SPGM 
00022 *                                                                 GA1SPGM 
00023 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID: #IPGS) GA1SPGM 
00024 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1SPGM 
00025 *   XCTL TO TRANS GA2S OR PROGRAM GA2SPGM.  THIS PROGRAM WILL     GA1SPGM 
00026 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1SPGM 
00027 *   TABLE.                                                        GA1SPGM 
00028 *                                                                 GA1SPGM 
00029 *   FUNC CODE: GA1S                                               GA1SPGM 
00030 *   MAPSET:    GA1SSETC                                           GA1SPGM 
00031 *   FILES:     GCPSWORK                                           GA1SPGM 
00032 *                                                                 GA1SPGM 
00033 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00034 *                                                                 GA1SPGM 
00035 *    TAILORING INSTRUCTIONS:                                      GA1SPGM 
00036 *                                                                 GA1SPGM 
00037 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1SPGM 
00038 *                                                                 GA1SPGM 
00039 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1S/     GA1SPGM 
00040 *              SCREEN PAGE NUMBER                 /009I/001I/     GA1SPGM 
00041 *              ADD PROGRAM FUNCTION CODE          /GCAI/GA2S/     GA1SPGM 
00042 *              BENEFIT PROVISION TABULAR ID       /#PPF/#IPGS/    GA1SPGM 
00043 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GX3/       GA1SPGM 
00044 *                                                                 GA1SPGM 
00045 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1SPGM 
00046 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1SPGM 
00047 *     OCCURANCES.                                                 GA1SPGM 
00048 *                                                                 GA1SPGM 
00049 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1SPGM 
00050 *     THAT MUST BE CHANGED.                                       GA1SPGM 
00051 *                                                                 GA1SPGM 
00052 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00053      SKIP3                                                        GA1SPGM 
00054 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1SPGM 
00055 *   DATE    PROGRAMMER  MAINTENANCE                             * GA1SPGM 
00056 * --------  ----------  --------------------------------------- * GA1SPGM 
00057 *                                                               * GA1SPGM 
00058 * P????   07/07/00  GSP  CREATED FOR NEW #IPGS INTERNAL         * GA1SPGM 
00059 *                        TABULAR BASED ON GA1IPGM.              * GA1SPGM 
00060 *                                                               * GA1SPGM 
00061 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA1SPGM 
SI0724*                                                               * 00009160
SI0724* P56703  05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION    * 00009170
SI0724*                        COPY ABM, ACP, ACL, ADL, AOL,          * 00009180
SI0724*                        GCCDRLEN                               * 00009190
00062 *                                                               * GA1SPGM 
00063 ***************************************************************** GA1SPGM 
00064 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1SPGM 
00065 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA1SPGM 
00066      EJECT                                                        GA1SPGM 
00067  ENVIRONMENT DIVISION.                                            GA1SPGM 
00068      EJECT                                                        GA1SPGM 
00069  DATA DIVISION.                                                   GA1SPGM 
00070  WORKING-STORAGE SECTION.                                         GA1SPGM 
00071  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1SPGM 
00072      '***GA1SPGM WS BEGINS***'.                                   GA1SPGM 
00073  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1SPGM 
00074                                                                   GA1SPGM 
00075  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1SPGM 
00076                                                                   GA1SPGM 
00077  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1SPGM 
00078  01  COMMAREA-POINTER-AREA.                                       GA1SPGM 
00079      05  COMMAREA-PNTR-COMP          PIC S9(8)  COMP.             GA1SPGM 
00080      05  COMMAREA-PNTR  REDEFINES                                 GA1SPGM 
00081          COMMAREA-PNTR-COMP          USAGE IS POINTER.            GA1SPGM 
00082                                                                   GA1SPGM 
00083 ** MAP COBOL SCREEN DSECTS **                                     GA1SPGM 
00084  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1SPGM 
00085      '***  I/O MAPAREA ***'.                                      GA1SPGM 
00086  COPY GA1SSETC.                                                   GA1SPGM 
00087      EJECT                                                        GA1SPGM 
00088 ******************************************************************GA1SPGM 
00089 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA1SPGM 
00090 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1SPGM 
00091 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1SPGM 
00092 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1SPGM 
00093 **  REDEFINES.                                                    GA1SPGM 
00094 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00095 **                                                                GA1SPGM 
00096 **  THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1SPGM 
00097 **  FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1SPGM 
00098 **  MATCH THE MAP.                                                GA1SPGM 
00099 **                                                                GA1SPGM 
00100 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00101                                                                   GA1SPGM 
00102  01  FILLER     REDEFINES   GA1SI01I.                             GA1SPGM 
00103      05  FILLER                              PIC X(83).           GA1SPGM 
00104      05  GROUP-SPECIFIC-ID-LINE.                                  GA1SPGM 
00105          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA1SPGM 
00106          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA1SPGM 
00107          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA1SPGM 
00108          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA1SPGM 
00109          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA1SPGM 
00110          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA1SPGM 
00111          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA1SPGM 
00112          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA1SPGM 
00113          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA1SPGM 
00114          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA1SPGM 
00115          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA1SPGM 
00116          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA1SPGM 
00117          10  FILLER                          PIC X(16).           GA1SPGM 
00118      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA1SPGM 
00119          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1SPGM 
00120          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1SPGM 
00121          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1SPGM 
00122          10  CONTRACT-GROUP-NO               PIC X(9).            GA1SPGM 
00123          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1SPGM 
00124          10  CONTRACT-SECTION-NO             PIC X(5).            GA1SPGM 
00125          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1SPGM 
00126          10  CONTRACT-PKG-CODE               PIC X(3).            GA1SPGM 
00127          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1SPGM 
00128          10  CONTRACT-LOB                    PIC X.               GA1SPGM 
00129          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1SPGM 
00130          10  CONTRACT-PROV-CTL               PIC XX.              GA1SPGM 
00131          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1SPGM 
00132          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1SPGM 
00133          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1SPGM 
00134          10  CONTRACT-EFF-DATE               PIC X(6).            GA1SPGM 
00135          10  FILLER                          PIC X(1).            GA1SPGM 
00136      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA1SPGM 
00137                                     GROUP-SPECIFIC-ID-LINE.       GA1SPGM 
00138          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA1SPGM 
00139          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA1SPGM 
00140          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA1SPGM 
00141          10  BEN-PROV-GROUP-NO               PIC X(9).            GA1SPGM 
00142          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA1SPGM 
00143          10  BEN-PROV-SECTION-NO             PIC X(5).            GA1SPGM 
00144          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA1SPGM 
00145          10  BEN-PROV-PKG-CODE               PIC X(3).            GA1SPGM 
00146          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA1SPGM 
00147          10  BEN-PROV-LOB                    PIC X.               GA1SPGM 
00148          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA1SPGM 
00149          10  BEN-PROV-PROV-CTL               PIC XX.              GA1SPGM 
00150          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA1SPGM 
00151          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA1SPGM 
00152          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA1SPGM 
00153          10  BEN-PROV-EFF-DATE               PIC X(6).            GA1SPGM 
00154          10  BEN-PROV-ID-HEADING             PIC X(6).            GA1SPGM 
00155          10  BEN-PROV-ID-NO                  PIC X(6).            GA1SPGM 
00156          10  FILLER                          PIC X(4).            GA1SPGM 
00157      05  FILLER                              PIC X(74).           GA1SPGM 
00158      05  MAP-PROVIDER-SPC-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED   GA1SPGM 
00159          BY MAP-IDX1.                                             GA1SPGM 
00160        10  MAP-PROVIDER-SPC-ARGUMENT-COL  OCCURS 3 TIMES INDEXED  GA1SPGM 
00161            BY MAP-IDX2.                                           GA1SPGM 
00162          15  MAP-ACTION-CODE-LEN             PIC S9(4) COMP SYNC. GA1SPGM 
00163          15  MAP-ACTION-CODE-ATTR            PIC X.               GA1SPGM 
00164          15  MAP-ACTION-CODE                 PIC X.               GA1SPGM 
00165          15  MAP-PROVIDER-SPC-ARGUMENT-LEN   PIC S9(4) COMP SYNC. GA1SPGM 
00166          15  MAP-PROVIDER-SPC-ARGUMENT-ATTR  PIC X.               GA1SPGM 
00167          15  MAP-PROVIDER-SPC-ARGUMENT       PIC X(3).            GA1SPGM 
00168 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00169  01  WS-MAP-OCCURS-COUNTERS.                                      GA1SPGM 
00170 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00171 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1SPGM 
00172 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00173      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +14.     GA1SPGM 
00174      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +3.      GA1SPGM 
00175 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00176      EJECT                                                        GA1SPGM 
00177 ** ALTERNATIVE WORKFILE KEYS **                                   GA1SPGM 
00178  01  FILLER                      PIC X(32)  VALUE                 GA1SPGM 
00179      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1SPGM 
00180  01  WS-ALT-WORKFILE-KEYS.                                        GA1SPGM 
00181  COPY GCWRKKEY.                                                   GA1SPGM 
00182 /                                                                 GA1SPGM 
00183 ** DATE FORMATTING AREA **                                        GA1SPGM 
00184  01  HGADATES-COMMAREA.                                           GA1SPGM 
00185  COPY HGCDAT01.                                                   GA1SPGM 
00186 /                                                                 GA1SPGM 
00187 ** WORKFIELDS, AND SWITCHES **                                    GA1SPGM 
00188  01  WS-WORK-FIELDS.                                              GA1SPGM 
00189      05  WS-HEX-00                     PIC X.                     GA1SPGM 
00190      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1SPGM 
00191 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00192 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00193 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00194      05  WS-SAVED-FIELDS.                                         GA1SPGM 
00195        10  WS-SAVED-PROVIDER-TYP-ARGUMENT                         GA1SPGM 
00196                                        PIC X(3).                  GA1SPGM 
00197 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00198  01  WS-SWITCHES.                                                 GA1SPGM 
00199      05  WS-ERROR-SW                   PIC X.                     GA1SPGM 
00200                                                                   GA1SPGM 
00201 ** TITLE LINES **                                                 GA1SPGM 
00202  01  WS-TITLE-LINES.                                              GA1SPGM 
00203      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA1SPGM 
00204          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA1SPGM 
00205      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA1SPGM 
00206          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA1SPGM 
00207      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA1SPGM 
00208          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA1SPGM 
00209                                                                   GA1SPGM 
00210      EJECT                                                        GA1SPGM 
00211 ** ATTRIBUTES **                                                  GA1SPGM 
00212  COPY DFHBMSCA.                                                   GA1SPGM 
00213      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1SPGM 
00214      EJECT                                                        GA1SPGM 
00215 ** ATTENTION IDENTIFIERS **                                       GA1SPGM 
00216  COPY DFHAID.                                                     GA1SPGM 
00217      EJECT                                                        GA1SPGM 
00218 ** RECORD LENGTHS **                                              GA1SPGM 
00219  01  WS-RECORD-LENGTHS.                                           GA1SPGM 
00220     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA1SPGM 
00221     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA1SPGM 
00222     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1SPGM 
00223     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1SPGM 
00224 *                                                                 GA1SPGM 
00225 /-------- GENERIC CONTRACT GLOBALLY DEFINED LENGTHS ...ETC ------*GA1SPGM 
00226  01  FILLER.                                                      GA1SPGM 
00227  COPY GCCDRLEN.                                                   GA1SPGM 
00228                                                                   GA1SPGM 
00229  01  WS-END                      PIC X(16)  VALUE                 GA1SPGM 
00230      '*** W/S ENDS ***'.                                          GA1SPGM 
00231 /                                                                 GA1SPGM 
00232  LINKAGE SECTION.                                                 GA1SPGM 
00233  01  DFHCOMMAREA.                                                 GA1SPGM 
00234  COPY G2ALCKEC.                                                   GA1SPGM 
00235  COPY GACDACWA.                                                   GA1SPGM 
00236 *    05  INCOMING-COMMAREA-PNTR   USAGE IS  POINTER.              GA1SPGM 
00237      05  GAS1UPD-PASSED-AREA.                                     GA1SPGM 
00238          07  LVL2-B-SW           PIC X.                           GA1SPGM 
00239          07  LVL2-F-SW           PIC X.                           GA1SPGM 
00240          07  LVL2-G-SW           PIC X.                           GA1SPGM 
00241          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1SPGM 
00242          07  FILLER              PIC X(9).                        GA1SPGM 
00243      05  DELADD-OPTION           PIC X(7).                        GA1SPGM 
00244                                                                   GA1SPGM 
00245 *01  GCA-COMMAREA.                                                GA1SPGM 
00246 *COPY G2ALCKEC.                                                   GA1SPGM 
00247 /                                                                 GA1SPGM 
00248  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA1SPGM 
00249  COPY GCIOPRM1.                                                   GA1SPGM 
00250      SKIP3                                                        GA1SPGM 
00251  COPY GCWRKDCC.                                                   GA1SPGM 
00252      SKIP3                                                        GA1SPGM 
00253  COPY GCTIPGSC.                                                   GA1SPGM 
00254      EJECT                                                        GA1SPGM 
00255 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00256 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00257 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00258  01  COPY-TABULAR-TABLE-AREA.                                     GA1SPGM 
00259      05  COPY-TABULAR-TABLE  OCCURS 1319 TIMES INDEXED BY         GA1SPGM 
00260            COPY-IDX.                                              GA1SPGM 
00261        10  COPY-PROVIDER-TYP-ARGUMENT         PIC X(3).           GA1SPGM 
00262 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00263      EJECT                                                        GA1SPGM 
00264  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA1SPGM 
00265  COPY GCIOPRM2.                                                   GA1SPGM 
00266      EJECT                                                        GA1SPGM 
00267  COPY GCWRKDC2.                                                   GA1SPGM 
00268      EJECT                                                        GA1SPGM 
00269  COPY GCTABMC.                                                    GA1SPGM 
00270      EJECT                                                        GA1SPGM 
00271                                                                   GA1SPGM 
00272  PROCEDURE DIVISION.                                              GA1SPGM 
00273                                                                   GA1SPGM 
00274 ******************************************************************GA1SPGM 
00275 **               H O U S E K E E P I N G                          GA1SPGM 
00276 **                                                                GA1SPGM 
00277 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA1SPGM 
00278 **                                                                GA1SPGM 
00279 ******************************************************************GA1SPGM 
00280  0000-HOUSEKEEPING   SECTION.                                     GA1SPGM 
00281                                                                   GA1SPGM 
00282      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1SPGM 
00283                                                                   GA1SPGM 
00284      IF EIBAID  =  DFHCLEAR                                       GA1SPGM 
00285          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1SPGM 
00286                         ERASE                                     GA1SPGM 
00287          END-EXEC                                                 GA1SPGM 
00288          EXEC CICS RETURN                                         GA1SPGM 
00289          END-EXEC.                                                GA1SPGM 
00290                                                                   GA1SPGM 
00291      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1SPGM 
00292                                   END-EXEC.                       GA1SPGM 
00293  0000-EXIT.     EXIT.                                             GA1SPGM 
00294 ******************************************************************GA1SPGM 
00295 **                     M A I N L I N E                            GA1SPGM 
00296 **                                                                GA1SPGM 
00297 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1SPGM 
00298 **  TAKEN BY THE OPERATOR.                                        GA1SPGM 
00299 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1SPGM 
00300 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1SPGM 
00301 **     ADDITIONS FROM.                                            GA1SPGM 
00302 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1SPGM 
00303 **     KEY PF12 OR PF24.                                          GA1SPGM 
00304 **  3. RECEIVE THE SCREEN.                                        GA1SPGM 
00305 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1SPGM 
00306 **     MENU.                                                      GA1SPGM 
00307 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1SPGM 
00308 **     LOGIC.                                                     GA1SPGM 
00309 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1SPGM 
00310 **     (RETURN) TO THE ADD PROGRAM (GA2SPGM).                     GA1SPGM 
00311 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1SPGM 
00312 **     (RETURN) TO THE PREVIOUS MENU.                             GA1SPGM 
00313 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1SPGM 
00314 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1SPGM 
00315 **                                                                GA1SPGM 
00316 ******************************************************************GA1SPGM 
00317  1000-MAIN-LINE.                                                  GA1SPGM 
00318                                                                   GA1SPGM 
00319      MOVE '1000'  TO  WS-PARA-ID.                                 GA1SPGM 
00320      IF EIBTRNID  NOT =  'GA1S'                                   GA1SPGM 
00321         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1SPGM 
00322         GO TO 1099-RETURN.                                        GA1SPGM 
00323                                                                   GA1SPGM 
00324      EXEC CICS RECEIVE   MAP('GA1SI01') MAPSET('GA1SSET')         GA1SPGM 
00325         INTO(GA1SI01I) END-EXEC.                                  GA1SPGM 
00326                                                                   GA1SPGM 
00327      IF SCRNIDNI  NOT =  '001I00'                                 GA1SPGM 
00328         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1SPGM 
00329                                                                   GA1SPGM 
00330      IF EIBAID  =  DFHENTER                                       GA1SPGM 
00331         PERFORM 2000-DELETE-PROCESSING                            GA1SPGM 
00332         GO TO 1099-RETURN.                                        GA1SPGM 
00333                                                                   GA1SPGM 
00334      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1SPGM 
00335         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1SPGM 
00336                                                                   GA1SPGM 
00337      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1SPGM 
00338         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1SPGM 
00339                                                                   GA1SPGM 
00340      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1SPGM 
00341      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1SPGM 
00342      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1SPGM 
00343 -    'THIS PROGRAM ***'  TO  ERRMSGO.                             GA1SPGM 
00344      EXEC CICS SEND   MAP('GA1SI01') MAPSET('GA1SSET') DATAONLY   GA1SPGM 
00345         FROM(GA1SI01O) CURSOR END-EXEC.                           GA1SPGM 
00346                                                                   GA1SPGM 
00347  1099-RETURN.                                                     GA1SPGM 
00348      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA1SPGM 
00349         (DELADD-OPTION = 'GAS1UPD') OR                            GA1SPGM 
00350         (DELADD-OPTION = 'GAS2UPD') OR                            GA1SPGM 
00351         (DELADD-OPTION = 'GAS3UPD') OR                            GA1SPGM 
00352         (DELADD-OPTION = 'GAS4UPD') OR                            GA1SPGM 
00353         (DELADD-OPTION = 'GAS5UPD')                               GA1SPGM 
00354          EXEC CICS RETURN   END-EXEC                              GA1SPGM 
00355      ELSE                                                         GA1SPGM 
00356          EXEC CICS RETURN TRANSID('GA1S')                         GA1SPGM 
00357                    COMMAREA(DFHCOMMAREA)                          GA1SPGM 
00358                    LENGTH  (EIBCALEN)                             GA1SPGM 
00359                    END-EXEC.                                      GA1SPGM 
00360      GOBACK.                                                      GA1SPGM 
00361                                                                   GA1SPGM 
00362  1999-EXIT.     EXIT.                                             GA1SPGM 
00363 /*****************************************************************GA1SPGM 
00364 **              D E L E T E   P R O C E S S I N G                 GA1SPGM 
00365 **                                                                GA1SPGM 
00366 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1SPGM 
00367 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1SPGM 
00368 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1SPGM 
00369 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1SPGM 
00370 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1SPGM 
00371 **    BACK INTO THE RECORD THAT WE READ.)                         GA1SPGM 
00372 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1SPGM 
00373 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1SPGM 
00374 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1SPGM 
00375 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1SPGM 
00376 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1SPGM 
00377 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1SPGM 
00378 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1SPGM 
00379 **    ENTRY.                                                      GA1SPGM 
00380 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1SPGM 
00381 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1SPGM 
00382 **    RECORD.                                                     GA1SPGM 
00383 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1SPGM 
00384 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1SPGM 
00385 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1SPGM 
00386 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1SPGM 
00387 **    ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1SPGM 
00388 **    SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1SPGM 
00389 **    BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1SPGM 
00390 **                                                                GA1SPGM 
00391 ******************************************************************GA1SPGM 
00392  2000-DELETE-PROCESSING SECTION.                                  GA1SPGM 
00393                                                                   GA1SPGM 
00394      MOVE '2000'  TO  WS-PARA-ID.                                 GA1SPGM 
00395      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1SPGM 
00396      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1SPGM 
00397      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1SPGM 
00398                                                                   GA1SPGM 
00399      MOVE '2010'  TO  WS-PARA-ID.                                 GA1SPGM 
00400  2010-VALIDATE-ACT-CODE.                                          GA1SPGM 
00401      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1SPGM 
00402         ADD 1  TO  WS-DELETE-COUNT.                               GA1SPGM 
00403      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D' OR           GA1SPGM 
00404         = SPACE OR =  LOW-VALUES                                  GA1SPGM 
00405         MOVE DFHBMUNF  TO                                         GA1SPGM 
00406            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1SPGM 
00407 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00408 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00409 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00410         MOVE DFHBMASF  TO                                         GA1SPGM 
00411            MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA1SPGM 
00412      ELSE                                                         GA1SPGM 
00413         MOVE DFHBMUBF  TO                                         GA1SPGM 
00414            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1SPGM 
00415         MOVE DFHBMABF  TO                                         GA1SPGM 
00416            MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA1SPGM 
00417 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00418         IF WS-ERROR-SW  NOT =  'Y'                                GA1SPGM 
00419            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1SPGM 
00420            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1SPGM 
00421                                                                   GA1SPGM 
00422      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1SPGM 
00423         SET MAP-IDX1   UP BY  1                                   GA1SPGM 
00424      ELSE                                                         GA1SPGM 
00425         IF MAP-IDX2  <  WS-MAP-COL                                GA1SPGM 
00426            SET MAP-IDX1  TO  1                                    GA1SPGM 
00427            SET MAP-IDX2  UP BY  1                                 GA1SPGM 
00428         ELSE                                                      GA1SPGM 
00429            GO TO 2020-DONE-VALIDATE-A-C.                          GA1SPGM 
00430 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00431 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00432 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00433      IF MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)            GA1SPGM 
00434         NOT =  LOW-VALUES                                         GA1SPGM 
00435 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00436         GO TO 2010-VALIDATE-ACT-CODE.                             GA1SPGM 
00437                                                                   GA1SPGM 
00438  2020-DONE-VALIDATE-A-C.                                          GA1SPGM 
00439      MOVE '2020'  TO  WS-PARA-ID.                                 GA1SPGM 
00440      SET MAP-IDX1   TO  1.                                        GA1SPGM 
00441                                                                   GA1SPGM 
00442      IF WS-ERROR-SW  =  'Y'                                       GA1SPGM 
00443         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1SPGM 
00444            ERRMSGO                                                GA1SPGM 
00445         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1SPGM 
00446            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA1SPGM 
00447            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA1SPGM 
00448 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00449 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1SPGM 
00450 **  ADD ITS MAP FIELD NAME HERE.                                  GA1SPGM 
00451 ****************************************************************  GA1SPGM 
00452            INCEXCO                                                GA1SPGM 
00453         MOVE '2100'  TO  WS-PARA-ID                               GA1SPGM 
00454         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1SPGM 
00455            VARYING MAP-IDX2 FROM  1  BY  1                        GA1SPGM 
00456                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1SPGM 
00457              AFTER MAP-IDX1 FROM  1  BY  1                        GA1SPGM 
00458                             UNTIL MAP-IDX1  >  WS-MAP-ROW         GA1SPGM 
00459         EXEC CICS SEND   MAP('GA1SI01') MAPSET('GA1SSET') DATAONLYGA1SPGM 
00460            FROM(GA1SI01O) CURSOR END-EXEC                         GA1SPGM 
00461         GO TO 2099-EXIT.                                          GA1SPGM 
00462                                                                   GA1SPGM 
00463      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1SPGM 
00464               GC-GCIOPARM-LEN                 +                   GA1SPGM 
00465               GC-WORKFILE-KEY-LEN             +                   GA1SPGM 
00466               GC-GCTABULR-IPGS-FIXED-LEN      +                   GA1SPGM 
00467              (GC-GCTABULR-IPGS-VARY-LEN       *                   GA1SPGM 
00468               GC-GCTABULR-IPGS-VARY-MAX-OCUR).                    GA1SPGM 
00469                                                                   GA1SPGM 
00470      EXEC CICS                                                    GA1SPGM 
00471         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA1SPGM 
00472         INITIMG(WS-HEX-00)                                        GA1SPGM 
00473         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA1SPGM 
00474      END-EXEC.                                                    GA1SPGM 
00475                                                                   GA1SPGM 
00476      IF  FRMNUIDI  =  'GS3A'                                      GA1SPGM 
00477         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1SPGM 
00478         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1SPGM 
00479         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA1SPGM 
00480         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1SPGM 
00481         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1SPGM 
00482         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1SPGM 
00483         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1SPGM 
00484         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1SPGM 
00485                          GCIO-WRK-PROVIDER-CONTROL                GA1SPGM 
00486         MOVE GRP-SPEC-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1SPGM 
00487         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1SPGM 
00488                                                                   GA1SPGM 
00489      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1SPGM 
00490         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1SPGM 
00491         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1SPGM 
00492         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1SPGM 
00493         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1SPGM 
00494         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1SPGM 
00495         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1SPGM 
00496         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1SPGM 
00497         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1SPGM 
00498         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1SPGM 
00499         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1SPGM 
00500         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1SPGM 
00501                                                                   GA1SPGM 
00502      IF  FRMNUIDI  =  'GC8A'                                      GA1SPGM 
00503         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1SPGM 
00504         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1SPGM 
00505         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA1SPGM 
00506         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1SPGM 
00507         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA1SPGM 
00508         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA1SPGM 
00509         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1SPGM 
00510         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1SPGM 
00511         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1SPGM 
00512         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA1SPGM 
00513         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA1SPGM 
00514                                                                   GA1SPGM 
00515      MOVE  GC-GCPSWORK-DDNAME TO  GCIO-FILE-DDNAME.               GA1SPGM 
00516      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1SPGM 
00517      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA1SPGM 
00518      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA1SPGM 
00519      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA1SPGM 
00520      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA1SPGM 
00521      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1SPGM 
00522                                                                   GA1SPGM 
00523      IF WS-DELETE-COUNT  =  ZERO                                  GA1SPGM 
00524         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1SPGM 
00525                                                                   GA1SPGM 
00526 ******************************************************************GA1SPGM 
00527 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1SPGM 
00528 *                                                                 GA1SPGM 
00529 ******************************************************************GA1SPGM 
00530                                                                   GA1SPGM 
00531      MOVE  GC-GCTABULR-IPGS-VARY-MAX-OCUR                         GA1SPGM 
00532            TO  GXS-ENTRY-COUNT.                                   GA1SPGM 
00533                                                                   GA1SPGM 
00534      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1SPGM 
00535                                                                   GA1SPGM 
00536      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1SPGM 
00537         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1SPGM 
00538         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1SPGM 
00539                                                                   GA1SPGM 
00540      IF  NOT GCIO-GOOD-RETURN                                     GA1SPGM 
00541         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1SPGM 
00542 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1SPGM 
00543         MOVE '1IF1'  TO  WS-ABEND-CODE                            GA1SPGM 
00544         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1SPGM 
00545                                                                   GA1SPGM 
00546      COMPUTE WS-COPY-LENGTH  =                                    GA1SPGM 
00547              GXS-ENTRY-COUNT  *  GC-GCTABULR-IPGS-VARY-LEN.       GA1SPGM 
00548                                                                   GA1SPGM 
00549      EXEC CICS                                                    GA1SPGM 
00550         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA1SPGM 
00551         INITIMG(WS-HEX-00)                                        GA1SPGM 
00552         LENGTH(WS-COPY-LENGTH)                                    GA1SPGM 
00553      END-EXEC.                                                    GA1SPGM 
00554                                                                   GA1SPGM 
00555      MOVE GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT.                   GA1SPGM 
00556      SET COPY-IDX,  GXS-INDEX  TO  1.                             GA1SPGM 
00557                                                                   GA1SPGM 
00558      MOVE '2030'  TO  WS-PARA-ID.                                 GA1SPGM 
00559  2030-MAKE-A-COPY-OF-RECORD.                                      GA1SPGM 
00560      IF GXS-INDEX  NOT >  GXS-ENTRY-COUNT                         GA1SPGM 
00561         MOVE GXS-ENTRY (GXS-INDEX)  TO                            GA1SPGM 
00562            COPY-TABULAR-TABLE (COPY-IDX)                          GA1SPGM 
00563            SET COPY-IDX,  GXS-INDEX  UP BY  1                     GA1SPGM 
00564            GO TO 2030-MAKE-A-COPY-OF-RECORD.                      GA1SPGM 
00565      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GXS-INDEX  TO  1.         GA1SPGM 
00566                                                                   GA1SPGM 
00567      MOVE '2040'  TO  WS-PARA-ID.                                 GA1SPGM 
00568  2040-DELETE-MARKED-ENTRIES.                                      GA1SPGM 
00569 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00570 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00571 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00572      IF MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)  =         GA1SPGM 
00573            LOW-VALUES                                             GA1SPGM 
00574         GO TO 2060-SAVE-REST-OF-COPY.                             GA1SPGM 
00575                                                                   GA1SPGM 
00576      IF MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)  >         GA1SPGM 
00577         COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)                     GA1SPGM 
00578         GO TO 2050-SAVE-COPIED-ENTRY                              GA1SPGM 
00579      ELSE                                                         GA1SPGM 
00580         IF MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)  <      GA1SPGM 
00581            COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)                  GA1SPGM 
00582            MOVE '1IL1'  TO  WS-ABEND-CODE                         GA1SPGM 
00583            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1SPGM 
00584 -    'RM SYSTEMS AREA ***'  TO  ERRMSGO                           GA1SPGM 
00585            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1SPGM 
00586                                                                   GA1SPGM 
00587 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00588                                                                   GA1SPGM 
00589      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1SPGM 
00590         IF MAP-IDX1   <  WS-MAP-ROW                               GA1SPGM 
00591            SET MAP-IDX1   UP BY  1                                GA1SPGM 
00592            GO TO 2050-SAVE-COPIED-ENTRY                           GA1SPGM 
00593         ELSE                                                      GA1SPGM 
00594            IF MAP-IDX2  <  WS-MAP-COL                             GA1SPGM 
00595               SET MAP-IDX1  TO  1                                 GA1SPGM 
00596               SET MAP-IDX2  UP BY  1                              GA1SPGM 
00597               GO TO 2050-SAVE-COPIED-ENTRY                        GA1SPGM 
00598            ELSE                                                   GA1SPGM 
00599               GO TO 2060-SAVE-REST-OF-COPY.                       GA1SPGM 
00600                                                                   GA1SPGM 
00601      SET COPY-IDX  UP BY  1.                                      GA1SPGM 
00602      IF COPY-IDX  NOT <  GXS-ENTRY-COUNT                          GA1SPGM 
00603         MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                    GA1SPGM 
00604            GXS-ENTRY (GXS-INDEX)                                  GA1SPGM 
00605         SET  GXS-ENTRY-COUNT  TO  GXS-INDEX                       GA1SPGM 
00606         MOVE GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT                 GA1SPGM 
00607         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1SPGM 
00608      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1SPGM 
00609         SET MAP-IDX1   UP BY  1                                   GA1SPGM 
00610         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1SPGM 
00611      IF MAP-IDX2  <  WS-MAP-COL                                   GA1SPGM 
00612         SET MAP-IDX1  TO  1                                       GA1SPGM 
00613         SET MAP-IDX2  UP BY  1                                    GA1SPGM 
00614         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1SPGM 
00615      ELSE                                                         GA1SPGM 
00616         GO TO 2060-SAVE-REST-OF-COPY.                             GA1SPGM 
00617                                                                   GA1SPGM 
00618  2050-SAVE-COPIED-ENTRY.                                          GA1SPGM 
00619      MOVE '2050'  TO  WS-PARA-ID.                                 GA1SPGM 
00620      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1SPGM 
00621         GXS-ENTRY (GXS-INDEX).                                    GA1SPGM 
00622                                                                   GA1SPGM 
00623      SET GXS-INDEX  UP BY  1.                                     GA1SPGM 
00624      IF COPY-IDX  <  GXS-ENTRY-COUNT                              GA1SPGM 
00625         SET COPY-IDX  UP BY  1                                    GA1SPGM 
00626         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1SPGM 
00627      ELSE                                                         GA1SPGM 
00628 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1SPGM 
00629 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1SPGM 
00630 ***      THE TABLE OF ENTRIES.                                    GA1SPGM 
00631         MOVE '1IL2'  TO  WS-ABEND-CODE                            GA1SPGM 
00632         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1SPGM 
00633 -    'SYSTEMS AREA ***'  TO  ERRMSGO                              GA1SPGM 
00634         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1SPGM 
00635                                                                   GA1SPGM 
00636  2060-SAVE-REST-OF-COPY.                                          GA1SPGM 
00637      MOVE '2060'  TO  WS-PARA-ID.                                 GA1SPGM 
00638      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA1SPGM 
00639         GXS-ENTRY (GXS-INDEX).                                    GA1SPGM 
00640                                                                   GA1SPGM 
00641      SET GXS-INDEX  UP BY  1.                                     GA1SPGM 
00642      IF COPY-IDX  <  GXS-ENTRY-COUNT                              GA1SPGM 
00643         SET COPY-IDX  UP BY  1                                    GA1SPGM 
00644         GO TO 2060-SAVE-REST-OF-COPY.                             GA1SPGM 
00645                                                                   GA1SPGM 
00646      SET GXS-INDEX  DOWN BY  1.                                   GA1SPGM 
00647      SET GXS-ENTRY-COUNT  TO  GXS-INDEX.                          GA1SPGM 
00648      MOVE GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT.                   GA1SPGM 
00649                                                                   GA1SPGM 
00650  2070-UPDATE-MODIFIED-REC.                                        GA1SPGM 
00651      MOVE '2070'  TO  WS-PARA-ID.                                 GA1SPGM 
00652                                                                   GA1SPGM 
00653 *-- SET INDICATOR TO CAPTURE OPERAOR-ID.                          GA1SPGM 
00654                                                                   GA1SPGM 
00655      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1SPGM 
00656      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1SPGM 
00657                                                                   GA1SPGM 
00658      COMPUTE  GCIO-RECORD-LENGTH              =                   GA1SPGM 
00659               GC-WORKFILE-KEY-LEN             +                   GA1SPGM 
00660               GC-GCTABULR-IPGS-FIXED-LEN      +                   GA1SPGM 
00661              (GC-GCTABULR-IPGS-VARY-LEN       *                   GA1SPGM 
00662               GXS-ENTRY-COUNT).                                   GA1SPGM 
00663                                                                   GA1SPGM 
00664      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA1SPGM 
00665         GC-GCIOPARM-LEN     +  GCIO-RECORD-LENGTH.                GA1SPGM 
00666                                                                   GA1SPGM 
00667      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1SPGM 
00668         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1SPGM 
00669         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1SPGM 
00670                                                                   GA1SPGM 
00671      IF GCIO-GOOD-RETURN                                          GA1SPGM 
00672         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1SPGM 
00673      MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR.  CONTACGA1SPGM 
00674 -    'T SYSTEMS AREA ***'  TO  ERRMSGO.                           GA1SPGM 
00675      MOVE '1IF2'  TO  WS-ABEND-CODE.                              GA1SPGM 
00676      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1SPGM 
00677                                                                   GA1SPGM 
00678  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1SPGM 
00679      MOVE  '2080'  TO  WS-PARA-ID.                                GA1SPGM 
00680                                                                   GA1SPGM 
00681      MOVE  GC-GCTABULR-IPGS-VARY-MAX-OCUR                         GA1SPGM 
00682            TO  GXS-ENTRY-COUNT.                                   GA1SPGM 
00683                                                                   GA1SPGM 
00684      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1SPGM 
00685                                                                   GA1SPGM 
00686      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1SPGM 
00687         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA1SPGM 
00688         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA1SPGM 
00689                                                                   GA1SPGM 
00690      IF GCIO-GOOD-RETURN                                          GA1SPGM 
00691         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1SPGM 
00692      MOVE '1IF3'  TO  WS-ABEND-CODE.                              GA1SPGM 
00693      MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTACT GA1SPGM 
00694 -    'SYSTEMS AREA ***'  TO  ERRMSGO.                             GA1SPGM 
00695      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1SPGM 
00696                                                                   GA1SPGM 
00697  2090-BUILD-NEXT-DISPLAY.                                         GA1SPGM 
00698      MOVE  '2090'  TO  WS-PARA-ID.                                GA1SPGM 
00699      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1SPGM 
00700      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1SPGM 
00701      SET GXS-INDEX  TO  1.                                        GA1SPGM 
00702 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00703 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00704 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00705      IF MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)  =         GA1SPGM 
00706            LOW-VALUES                                             GA1SPGM 
00707         MOVE GXS-ENTRY (GXS-INDEX)  TO  WS-SAVED-FIELDS           GA1SPGM 
00708      ELSE                                                         GA1SPGM 
00709         MOVE MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2) TO    GA1SPGM 
00710            WS-SAVED-PROVIDER-TYP-ARGUMENT.                        GA1SPGM 
00711 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00712                                                                   GA1SPGM 
00713      PERFORM 4500-FILL-THE-SCREEN.                                GA1SPGM 
00714      EXEC CICS SEND   MAP('GA1SI01') MAPSET('GA1SSET') ERASE      GA1SPGM 
00715         FROM(GA1SI01O) END-EXEC.                                  GA1SPGM 
00716                                                                   GA1SPGM 
00717  2099-EXIT.   EXIT.                                               GA1SPGM 
00718      EJECT                                                        GA1SPGM 
00719  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1SPGM 
00720 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00721 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00722 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00723      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1SPGM 
00724         MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1SPGM 
00725 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00726                                                                   GA1SPGM 
00727  2199-EXIT.   EXIT.                                               GA1SPGM 
00728      EJECT                                                        GA1SPGM 
00729 ******************************************************************GA1SPGM 
00730 **          X C T L   T O   A D D   S C R E E N                   GA1SPGM 
00731 **                                                                GA1SPGM 
00732 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1SPGM 
00733 ** ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1SPGM 
00734 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1SPGM 
00735 ** TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1SPGM 
00736 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1SPGM 
00737 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1SPGM 
00738 ******************************************************************GA1SPGM 
00739  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1SPGM 
00740      MOVE '3000'  TO  WS-PARA-ID.                                 GA1SPGM 
00741                                                                   GA1SPGM 
00742 *    EXEC CICS                                                    GA1SPGM 
00743 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1SPGM 
00744 *       INITIMG(WS-HEX-00)                                        GA1SPGM 
00745 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1SPGM 
00746 *    END-EXEC.                                                    GA1SPGM 
00747                                                                   GA1SPGM 
00748 *    IF  FRMNUIDI  =  'GS3A'                                      GA1SPGM 
00749 ***     MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GA1SPGM 
00750 *       MOVE  GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                   GA1SPGM 
00751 *       MOVE  GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO               GA1SPGM 
00752 *       MOVE  GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1SPGM 
00753 *       MOVE  GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1SPGM 
00754 *       MOVE  SPACES  TO  GCA-L-O-B,                              GA1SPGM 
00755 *                         GCA-PROV-CTL,                           GA1SPGM 
00756 *                         GCA-BEN-PROV-ID.                        GA1SPGM 
00757                                                                   GA1SPGM 
00758 *    IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA1SPGM 
00759 ***     MOVE  IDLINEI  TO  CONTRACT-ID-LINE                       GA1SPGM 
00760 *       MOVE  CONTRACT-GROUP-NO  TO  GCA-GRP-NO                   GA1SPGM 
00761 *       MOVE  CONTRACT-SECTION-NO  TO  GCA-SECTN-NO               GA1SPGM 
00762 *       MOVE  CONTRACT-LOB  TO  GCA-L-O-B                         GA1SPGM 
00763 *       MOVE  CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                 GA1SPGM 
00764 *       MOVE  CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1SPGM 
00765 *       MOVE  CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1SPGM 
00766 *       MOVE  SPACES  TO  GCA-BEN-PROV-ID.                        GA1SPGM 
00767                                                                   GA1SPGM 
00768 *    IF  FRMNUIDI  =  'GC8A'                                      GA1SPGM 
00769 ***     MOVE  IDLINEI  TO  BENEFIT-PROVISION-ID-LINE              GA1SPGM 
00770 *       MOVE  BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                   GA1SPGM 
00771 *       MOVE  BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO               GA1SPGM 
00772 *       MOVE  BEN-PROV-LOB  TO  GCA-L-O-B                         GA1SPGM 
00773 *       MOVE  BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                 GA1SPGM 
00774 *       MOVE  BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL           GA1SPGM 
00775 *       MOVE  BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE           GA1SPGM 
00776 *       MOVE  BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                GA1SPGM 
00777                                                                   GA1SPGM 
00778      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1SPGM 
00779      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1SPGM 
00780      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1SPGM 
00781      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1SPGM 
00782      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1SPGM 
00783      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1SPGM 
00784      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1SPGM 
00785      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1SPGM 
00786 *    MOVE  ZEROES  TO  GCA-EFF-DT.                                GA1SPGM 
00787 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00788 ** IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD OR OTHER    GA1SPGM 
00789 ** FIELDS TO DISPLAY ON THE INITIAL ADD SCREEN THEY SHOULD BE     GA1SPGM 
00790 ** PASSED HERE.                                                   GA1SPGM 
00791 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00792      MOVE INCEXCI TO GCA-I-E-INDC.                                GA1SPGM 
00793                                                                   GA1SPGM 
00794 *    SET  COMMAREA-PNTR   TO                                      GA1SPGM 
00795 *         ADDRESS OF  GCA-COMMAREA.                               GA1SPGM 
00796                                                                   GA1SPGM 
00797 *    EXEC CICS XCTL  PROGRAM('GA2SPGM') COMMAREA(COMMAREA-PNTR)   GA1SPGM 
00798 *       LENGTH(4) END-EXEC.                                       GA1SPGM 
00799      EXEC CICS XCTL  PROGRAM('GA2SPGM')                           GA1SPGM 
00800                      COMMAREA(DFHCOMMAREA)                        GA1SPGM 
00801                      LENGTH(LENGTH OF DFHCOMMAREA)                GA1SPGM 
00802      END-EXEC.                                                    GA1SPGM 
00803                                                                   GA1SPGM 
00804  3099-EXIT.   EXIT.                                               GA1SPGM 
00805      EJECT                                                        GA1SPGM 
00806 ***************************************************************** GA1SPGM 
00807 **          D I S P L A Y   F I R S T   S C R E E N               GA1SPGM 
00808 **                                                                GA1SPGM 
00809 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1SPGM 
00810 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1SPGM 
00811 ** ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1SPGM 
00812 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1SPGM 
00813 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1SPGM 
00814 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1SPGM 
00815 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1SPGM 
00816 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1SPGM 
00817 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1SPGM 
00818 ** DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1SPGM 
00819 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1SPGM 
00820 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1SPGM 
00821 ******************************************************************GA1SPGM 
00822  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1SPGM 
00823      MOVE '4000'  TO  WS-PARA-ID.                                 GA1SPGM 
00824                                                                   GA1SPGM 
00825      MOVE LOW-VALUES  TO GA1SI01I.                                GA1SPGM 
00826      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1SPGM 
00827         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1SPGM 
00828            TO ERRMSGO                                             GA1SPGM 
00829         MOVE '1IC1'  TO  WS-ABEND-CODE                            GA1SPGM 
00830         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1SPGM 
00831                                                                   GA1SPGM 
00832 *    SET ADDRESS OF  GCA-COMMAREA                                 GA1SPGM 
00833 *        TO  INCOMING-COMMAREA-PNTR.                              GA1SPGM 
00834                                                                   GA1SPGM 
00835      SET ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD                  GA1SPGM 
00836          TO  GCA-RECORD-POINTER.                                  GA1SPGM 
00837                                                                   GA1SPGM 
00838      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA1SPGM 
00839      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA1SPGM 
00840      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA1SPGM 
00841      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA1SPGM 
00842      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA1SPGM 
00843      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA1SPGM 
00844      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA1SPGM 
00845      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA1SPGM 
00846 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00847 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1SPGM 
00848 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1SPGM 
00849 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00850      MOVE GXS-INCLUDE-EXCLUDE-IND  TO GCA-I-E-INDC.               GA1SPGM 
00851      MOVE GCA-I-E-INDC TO INCEXCO.                                GA1SPGM 
00852                                                                   GA1SPGM 
00853      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1SPGM 
00854         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA1SPGM 
00855         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA1SPGM 
00856         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA1SPGM 
00857         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA1SPGM 
00858         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA1SPGM 
00859         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1SPGM 
00860         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA1SPGM 
00861         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA1SPGM 
00862         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA1SPGM 
00863         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1SPGM 
00864         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1SPGM 
00865         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1SPGM 
00866         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1SPGM 
00867                                                                   GA1SPGM 
00868      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1SPGM 
00869         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA1SPGM 
00870         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA1SPGM 
00871         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA1SPGM 
00872         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA1SPGM 
00873         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA1SPGM 
00874         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1SPGM 
00875         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA1SPGM 
00876         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA1SPGM 
00877         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA1SPGM 
00878         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1SPGM 
00879         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1SPGM 
00880         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1SPGM 
00881         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1SPGM 
00882         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1SPGM 
00883         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1SPGM 
00884         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1SPGM 
00885         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1SPGM 
00886                                                                   GA1SPGM 
00887      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1SPGM 
00888         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA1SPGM 
00889         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA1SPGM 
00890         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA1SPGM 
00891         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA1SPGM 
00892         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA1SPGM 
00893         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA1SPGM 
00894         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA1SPGM 
00895         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA1SPGM 
00896         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA1SPGM 
00897         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA1SPGM 
00898         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1SPGM 
00899         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA1SPGM 
00900         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1SPGM 
00901         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA1SPGM 
00902         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1SPGM 
00903         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA1SPGM 
00904         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1SPGM 
00905         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA1SPGM 
00906         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1SPGM 
00907                                                                   GA1SPGM 
00908      SET GXS-INDEX  TO  1.                                        GA1SPGM 
00909      MOVE GXS-ENTRY (GXS-INDEX)  TO  WS-SAVED-FIELDS.             GA1SPGM 
00910                                                                   GA1SPGM 
00911      PERFORM 4500-FILL-THE-SCREEN.                                GA1SPGM 
00912      EXEC CICS SEND   MAP('GA1SI01') MAPSET('GA1SSET') ERASE      GA1SPGM 
00913         FROM(GA1SI01O) END-EXEC.                                  GA1SPGM 
00914                                                                   GA1SPGM 
00915  4099-EXIT.   EXIT.                                               GA1SPGM 
00916      EJECT                                                        GA1SPGM 
00917 ***************************************************************** GA1SPGM 
00918 **             F I L L   T H E   S C R E E N                      GA1SPGM 
00919 **                                                                GA1SPGM 
00920 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1SPGM 
00921 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1SPGM 
00922 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1SPGM 
00923 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1SPGM 
00924 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1SPGM 
00925 ******************************************************************GA1SPGM 
00926  4500-FILL-THE-SCREEN SECTION.                                    GA1SPGM 
00927                                                                   GA1SPGM 
00928      MOVE '4500'  TO  WS-PARA-ID.                                 GA1SPGM 
00929      MOVE  GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT.                  GA1SPGM 
00930      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1SPGM 
00931                                                                   GA1SPGM 
00932      IF GXS-ENTRY-COUNT  NOT >  1                                 GA1SPGM 
00933         MOVE '4530'  TO  WS-PARA-ID                               GA1SPGM 
00934         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1SPGM 
00935                                                                   GA1SPGM 
00936      SET GXS-INDEX  TO  1.                                        GA1SPGM 
00937      MOVE '4510'  TO  WS-PARA-ID.                                 GA1SPGM 
00938  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1SPGM 
00939 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00940 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00941 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00942      IF GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)  <                 GA1SPGM 
00943            WS-SAVED-PROVIDER-TYP-ARGUMENT                         GA1SPGM 
00944 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00945         SET GXS-INDEX  UP BY  1                                   GA1SPGM 
00946         IF  GXS-INDEX  <  GXS-ENTRY-COUNT                         GA1SPGM 
00947            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1SPGM 
00948         ELSE                                                      GA1SPGM 
00949            SET GXS-INDEX  TO  1.                                  GA1SPGM 
00950                                                                   GA1SPGM 
00951      MOVE '4520'  TO  WS-PARA-ID.                                 GA1SPGM 
00952  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1SPGM 
00953      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1SPGM 
00954      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1SPGM 
00955 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00956 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00957 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00958      MOVE GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX)  TO              GA1SPGM 
00959         MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1SPGM 
00960 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00961                                                                   GA1SPGM 
00962      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1SPGM 
00963         SET  MAP-IDX1  UP BY  1                                   GA1SPGM 
00964      ELSE                                                         GA1SPGM 
00965         IF MAP-IDX2  <  WS-MAP-COL                                GA1SPGM 
00966            SET  MAP-IDX1  TO  1                                   GA1SPGM 
00967            SET  MAP-IDX2  UP BY  1                                GA1SPGM 
00968         ELSE                                                      GA1SPGM 
00969            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1SPGM 
00970                                                                   GA1SPGM 
00971      IF GXS-INDEX  <  (GXS-ENTRY-COUNT - 1 )                      GA1SPGM 
00972         SET  GXS-INDEX  UP BY  1                                  GA1SPGM 
00973         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1SPGM 
00974                                                                   GA1SPGM 
00975      MOVE '4530'  TO  WS-PARA-ID.                                 GA1SPGM 
00976  4530-FILL-REST-WITH-NULLS.                                       GA1SPGM 
00977      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1SPGM 
00978 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00979 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1SPGM 
00980 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00981      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1SPGM 
00982         MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2).           GA1SPGM 
00983 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1SPGM 
00984      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1SPGM 
00985         SET  MAP-IDX1   UP BY  1                                  GA1SPGM 
00986         GO TO 4530-FILL-REST-WITH-NULLS                           GA1SPGM 
00987      ELSE                                                         GA1SPGM 
00988         IF MAP-IDX2  <  WS-MAP-COL                                GA1SPGM 
00989            SET  MAP-IDX1  TO  1                                   GA1SPGM 
00990            SET  MAP-IDX2  UP BY 1                                 GA1SPGM 
00991            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1SPGM 
00992                                                                   GA1SPGM 
00993      MOVE '4540'  TO  WS-PARA-ID.                                 GA1SPGM 
00994  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1SPGM 
00995      IF GXS-ENTRY-COUNT  =  1                                     GA1SPGM 
00996         MOVE '*** NO ENTRIES TO DELETE ***'  TO  ERRMSGO          GA1SPGM 
00997         GO TO 4599-EXIT.                                          GA1SPGM 
00998                                                                   GA1SPGM 
00999      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1SPGM 
01000         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'  TO  ERRMSGO.   GA1SPGM 
01001                                                                   GA1SPGM 
01002  4599-EXIT.     EXIT.                                             GA1SPGM 
01003      EJECT                                                        GA1SPGM 
01004 ***************************************************************** GA1SPGM 
01005 **        X C T L   T O   P R E V I O U S   M E N U               GA1SPGM 
01006 **                                                                GA1SPGM 
01007 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1SPGM 
01008 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1SPGM 
01009 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1SPGM 
01010 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1SPGM 
01011 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1SPGM 
01012 ******************************************************************GA1SPGM 
01013  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1SPGM 
01014      MOVE '5000'  TO  WS-PARA-ID.                                 GA1SPGM 
01015                                                                   GA1SPGM 
01016                                                                   GA1SPGM 
01017 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA1SPGM 
01018 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA1SPGM 
01019 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA1SPGM 
01020                                                                   GA1SPGM 
01021      IF  ALTBFNCI  =  'GTM1'                                      GA1SPGM 
01022          EXEC CICS XCTL                                           GA1SPGM 
01023                    PROGRAM('GTM1PGM')                             GA1SPGM 
01024                    END-EXEC.                                      GA1SPGM 
01025                                                                   GA1SPGM 
01026      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN      =                   GA1SPGM 
01027               GC-GCIOPARM-LEN                 +                   GA1SPGM 
01028               GC-WORKFILE-KEY-LEN             +                   GA1SPGM 
01029               GC-GCTABULR-ABM-FIXED-LEN       +                   GA1SPGM 
01030              (GC-GCTABULR-ABM-VARY-LEN        *                   GA1SPGM 
01031               GC-GCTABULR-ABM-VARY-MAX-OCUR)                      GA1SPGM 
01032                                                                   GA1SPGM 
01033      EXEC CICS                                                    GA1SPGM 
01034         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA1SPGM 
01035         INITIMG(WS-HEX-00)                                        GA1SPGM 
01036         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA1SPGM 
01037      END-EXEC.                                                    GA1SPGM 
01038                                                                   GA1SPGM 
01039 *    EXEC CICS                                                    GA1SPGM 
01040 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA1SPGM 
01041 *       INITIMG(WS-HEX-00)                                        GA1SPGM 
01042 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA1SPGM 
01043 *    END-EXEC.                                                    GA1SPGM 
01044                                                                   GA1SPGM 
01045      IF  FRMNUIDI  =  'GS3A'                                      GA1SPGM 
01046         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1SPGM 
01047         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA1SPGM 
01048         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA1SPGM 
01049         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1SPGM 
01050         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1SPGM 
01051         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1SPGM 
01052         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1SPGM 
01053         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1SPGM 
01054                          GCIO-WRK-PROVIDER-CONTROL                GA1SPGM 
01055         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1SPGM 
01056         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1SPGM 
01057         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1SPGM 
01058         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1SPGM 
01059                            GCA-ALL-LEVEL-TAB-ID                   GA1SPGM 
01060         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1SPGM 
01061                            GCA-ALL-LEVEL-TAB-SLOT                 GA1SPGM 
01062         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1SPGM 
01063                            GCA-INTERNAL-TAB-ID,                   GA1SPGM 
01064                            GCA-INTERNAL-TAB-SLOT                  GA1SPGM 
01065         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1SPGM 
01066                                                                   GA1SPGM 
01067      IF  FRMNUIDI  =  'GC4A'                                      GA1SPGM 
01068         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1SPGM 
01069         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1SPGM 
01070         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA1SPGM 
01071         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1SPGM 
01072         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1SPGM 
01073         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1SPGM 
01074         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1SPGM 
01075         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1SPGM 
01076         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1SPGM 
01077         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1SPGM 
01078         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1SPGM 
01079         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA1SPGM 
01080         MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID,                 GA1SPGM 
01081                            GCA-ALL-LEVEL-TAB-ID                   GA1SPGM 
01082         MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO,            GA1SPGM 
01083                            GCA-ALL-LEVEL-TAB-SLOT                 GA1SPGM 
01084         MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,             GA1SPGM 
01085                            GCA-INTERNAL-TAB-ID,                   GA1SPGM 
01086                            GCA-INTERNAL-TAB-SLOT                  GA1SPGM 
01087         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1SPGM 
01088                                                                   GA1SPGM 
01089      IF  FRMNUIDI  =  'GC8A'                                      GA1SPGM 
01090         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1SPGM 
01091         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA1SPGM 
01092         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA1SPGM 
01093         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA1SPGM 
01094         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA1SPGM 
01095         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA1SPGM 
01096         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA1SPGM 
01097         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA1SPGM 
01098         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA1SPGM 
01099         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA1SPGM 
01100         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA1SPGM 
01101         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA1SPGM 
01102         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1SPGM 
01103         MOVE ALTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID,             GA1SPGM 
01104                            GCA-ALL-LEVEL-TAB-ID                   GA1SPGM 
01105         MOVE ALTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO,             GA1SPGM 
01106                            GCA-ALL-LEVEL-TAB-SLOT                 GA1SPGM 
01107         MOVE SPACES  TO  GCA-INTERNAL-TAB-ID,                     GA1SPGM 
01108                          GCA-INTERNAL-TAB-SLOT.                   GA1SPGM 
01109                                                                   GA1SPGM 
01110      MOVE  'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                     GA1SPGM 
01111 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA1SPGM 
01112 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA1SPGM 
01113 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA1SPGM 
01114 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA1SPGM 
01115 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA1SPGM 
01116      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1SPGM 
01117                                                                   GA1SPGM 
01118      SET  GCA-RECORD-POINTER                                      GA1SPGM 
01119           TO ADDRESS OF  IO-PARM-ALL-LEVEL-RECORD.                GA1SPGM 
01120                                                                   GA1SPGM 
01121      MOVE  GC-GCTABULR-ABM-VARY-MAX-OCUR                          GA1SPGM 
01122            TO  GAA-ENTRY-COUNT.                                   GA1SPGM 
01123                                                                   GA1SPGM 
01124      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1SPGM 
01125                                                                   GA1SPGM 
01126      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1SPGM 
01127         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA1SPGM 
01128         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA1SPGM 
01129                                                                   GA1SPGM 
01130      IF  NOT GCIO2-GOOD-RETURN                                    GA1SPGM 
01131         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA1SPGM 
01132 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA1SPGM 
01133         MOVE '1IF4'  TO  WS-ABEND-CODE                            GA1SPGM 
01134         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1SPGM 
01135                                                                   GA1SPGM 
01136 *    SET  COMMAREA-PNTR                                           GA1SPGM 
01137 *         TO ADDRESS OF  GCA-COMMAREA.                            GA1SPGM 
01138                                                                   GA1SPGM 
01139      IF  ALTBFNCI  =  'GA1B'                                      GA1SPGM 
01140 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA1SPGM 
01141 *          LENGTH(4) END-EXEC.                                    GA1SPGM 
01142         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA1SPGM 
01143                         COMMAREA(DFHCOMMAREA)                     GA1SPGM 
01144                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1SPGM 
01145         END-EXEC.                                                 GA1SPGM 
01146                                                                   GA1SPGM 
01147      IF  ALTBFNCI  =  'GA1C'                                      GA1SPGM 
01148 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA1SPGM 
01149 *          LENGTH(4) END-EXEC.                                    GA1SPGM 
01150         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA1SPGM 
01151                         COMMAREA(DFHCOMMAREA)                     GA1SPGM 
01152                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1SPGM 
01153         END-EXEC.                                                 GA1SPGM 
01154                                                                   GA1SPGM 
01155      IF  ALTBFNCI  =  'GA1D'                                      GA1SPGM 
01156 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA1SPGM 
01157 *          LENGTH(4) END-EXEC.                                    GA1SPGM 
01158         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA1SPGM 
01159                         COMMAREA(DFHCOMMAREA)                     GA1SPGM 
01160                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1SPGM 
01161         END-EXEC.                                                 GA1SPGM 
01162                                                                   GA1SPGM 
01163      IF  ALTBFNCI  =  'GA1E'                                      GA1SPGM 
01164 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA1SPGM 
01165 *          LENGTH(4) END-EXEC.                                    GA1SPGM 
01166         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA1SPGM 
01167                         COMMAREA(DFHCOMMAREA)                     GA1SPGM 
01168                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1SPGM 
01169         END-EXEC.                                                 GA1SPGM 
01170                                                                   GA1SPGM 
01171      IF  ALTBFNCI  =  'GA1P'                                      GA1SPGM 
01172         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA1SPGM 
01173                         COMMAREA(DFHCOMMAREA)                     GA1SPGM 
01174                         LENGTH(LENGTH OF DFHCOMMAREA)             GA1SPGM 
01175         END-EXEC.                                                 GA1SPGM 
01176                                                                   GA1SPGM 
01177  5099-EXIT.                                                       GA1SPGM 
01178      EXIT.                                                        GA1SPGM 
01179 /**************************************************************** GA1SPGM 
01180 **           X C T L   T O   M A I N   M E N U                    GA1SPGM 
01181 **                                                                GA1SPGM 
01182 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1SPGM 
01183 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1SPGM 
01184 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1SPGM 
01185 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1SPGM 
01186 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1SPGM 
01187 ** AND PROGRESS DOWN.                                             GA1SPGM 
01188 ******************************************************************GA1SPGM 
01189  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1SPGM 
01190      MOVE '6000'  TO  WS-PARA-ID.                                 GA1SPGM 
01191      MOVE '1IP1'  TO  WS-ABEND-CODE.                              GA1SPGM 
01192                                                                   GA1SPGM 
01193      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1SPGM 
01194                                                                   GA1SPGM 
01195  6099-EXIT.     EXIT.                                             GA1SPGM 
01196      EJECT                                                        GA1SPGM 
01197 /*****************************************************************GA1SPGM 
01198 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA1SPGM 
01199 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA1SPGM 
01200 ******************************************************************GA1SPGM 
01201  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA1SPGM 
01202  9800-010.                                                        GA1SPGM 
01203                                                                   GA1SPGM 
01204      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1SPGM 
01205      MOVE 'M'   TO  HGADATE-FORM1.                                GA1SPGM 
01206      MOVE 'J'   TO  HGADATE-FORM2.                                GA1SPGM 
01207      MOVE ZEROS TO  HGADATE-RETURN                                GA1SPGM 
01208                     HGADATE-AMOUNT.                               GA1SPGM 
01209      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1SPGM 
01210                     COMMAREA(HGADATES-COMMAREA)                   GA1SPGM 
01211                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1SPGM 
01212                     END-EXEC.                                     GA1SPGM 
01213                                                                   GA1SPGM 
01214  9800-900-900-EXIT.                                               GA1SPGM 
01215      EXIT.                                                        GA1SPGM 
01216 /*****************************************************************GA1SPGM 
01217 * 9810    J U L I A N    T O    G R E G O R I A N                *GA1SPGM 
01218 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *GA1SPGM 
01219 ******************************************************************GA1SPGM 
01220  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          GA1SPGM 
01221  9810-010.                                                        GA1SPGM 
01222                                                                   GA1SPGM 
01223      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA1SPGM 
01224      MOVE 'J'   TO  HGADATE-FORM1.                                GA1SPGM 
01225      MOVE 'M'   TO  HGADATE-FORM2.                                GA1SPGM 
01226      MOVE ZEROS TO  HGADATE-RETURN                                GA1SPGM 
01227                     HGADATE-AMOUNT.                               GA1SPGM 
01228      EXEC CICS LINK PROGRAM ('HGADATES')                          GA1SPGM 
01229                     COMMAREA(HGADATES-COMMAREA)                   GA1SPGM 
01230                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA1SPGM 
01231                     END-EXEC.                                     GA1SPGM 
01232                                                                   GA1SPGM 
01233  9810-900-900-EXIT.                                               GA1SPGM 
01234      EXIT.                                                        GA1SPGM 
01235      EJECT                                                        GA1SPGM 
01236  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1SPGM 
01237                                                                   GA1SPGM 
01238      SET MAP-IDX1 TO 7.                                           GA1SPGM 
01239      SET MAP-IDX2 TO 1.                                           GA1SPGM 
01240      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1SPGM 
01241                                                                   GA1SPGM 
01242      EXEC CICS SEND   MAP('GA1SI01') MAPSET('GA1SSET') ERASE      GA1SPGM 
01243         FROM(GA1SI01O) WAIT END-EXEC.                             GA1SPGM 
01244                                                                   GA1SPGM 
01245      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1SPGM 
01246                                                                   GA1SPGM 
01247  9999-EXIT.     EXIT.                                             GA1SPGM 
