00001  ID DIVISION.                                                     08/20/03
00002  PROGRAM-ID.     GA2SPGM.                                         GA2SPGM 
00003 **** THIS IS A COBOL/2 PROGRAM *****                                 LV001
00004  AUTHOR.         S BUCH.                                          GA2SPGM 
00005  DATE-WRITTEN.   11/13/84.                                        GA2SPGM 
00006  DATE-COMPILED.                                                   GA2SPGM 
00007      SKIP3                                                        GA2SPGM 
00008 *** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2SPGM 
00009 ****** P R O G R A M   M O D I F I C A T I O N   L O G       *****GA2SPGM 
00010 *** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2SPGM 
00011 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------GA2SPGM 
00012 *                                                                 GA2SPGM 
00013 *                                                               * GA2SPGM 
00014 * P????   07/07/00  GSP  CREATED FOR NEW #IPGS INTERNAL         * GA2SPGM 
00015 *                        TABULAR. BASED ON GA2IPGM.             * GA2SPGM 
00016 *                                                               * GA2SPGM 
00017 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA2SPGM 
SI0724*                                                                *00009160
SI0724* P56703 05/08/24   SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *00009170
SI0724*                           COPY ABM, ACP, ACL, ADL, AOL,        *00009180
SI0724*                           GCCDRLEN                             *00009190
00018 *                                                               * GA2SPGM 
00019 ***************************************************************** GA2SPGM 
00020 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2SPGM 
00021 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2SPGM 
00022 /                                                                 GA2SPGM 
00023 ******************************************************************GA2SPGM 
00024 *   GA2SPGM      ALL LEVEL INTERNAL TABULAR MAINTENANCE PROGRAM   GA2SPGM 
00025 *                    PROVIDER-GROUP BY PROVIDER-SPECIALTIES - GA2SGA2SPGM 
00026 *                                                                 GA2SPGM 
00027 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2SPGM 
00028 *   TABULAR PROVISION ID ARGUMENTS.                               GA2SPGM 
00029 *                                                                 GA2SPGM 
00030 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2SPGM 
00031 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2SPGM 
00032 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2SPGM 
00033 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2SPGM 
00034 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2SPGM 
00035 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2SPGM 
00036 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2SPGM 
00037 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2SPGM 
00038 *                                                                 GA2SPGM 
00039 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IPGS)GA2SPGM 
00040 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2SPGM 
00041 *   XCTL TO TRANS GA1S OR PROGRAM GA1SPGM.  THIS PROGRAM WILL     GA2SPGM 
00042 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2SPGM 
00043 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2SPGM 
00044 *   CODE.                                                         GA2SPGM 
00045 *                                                                 GA2SPGM 
00046 *   FUNC CODE: GA2S                                               GA2SPGM 
00047 *   MAPSET:    GA2SSETC                                           GA2SPGM 
00048 *   FILES:     GCPSWORK                                           GA2SPGM 
00049 ******************************************************************GA2SPGM 
00050      SKIP3                                                        GA2SPGM 
00051  ENVIRONMENT DIVISION.                                            GA2SPGM 
00052 /                                                                 GA2SPGM 
00053  DATA DIVISION.                                                   GA2SPGM 
00054  WORKING-STORAGE SECTION.                                         GA2SPGM 
00055  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2SPGM 
00056      '***GA2SPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2SPGM 
00057  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2SPGM 
00058                                                                   GA2SPGM 
00059  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2SPGM 
00060                                                                   GA2SPGM 
00061  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2SPGM 
00062                                                                   GA2SPGM 
00063  01  COMMAREA-POINTER-AREA.                                       GA2SPGM 
00064      05  COMMAREA-PNTR-COMP      PIC S9(8)  COMP.                 GA2SPGM 
00065      05  COMMAREA-PNTR  REDEFINES                                 GA2SPGM 
00066          COMMAREA-PNTR-COMP      USAGE IS POINTER.                GA2SPGM 
00067                                                                   GA2SPGM 
00068 ** MAP COBOL SCREEN DSECTS **                                     GA2SPGM 
00069  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2SPGM 
00070      '***  I/O MAPAREA ***'.                                      GA2SPGM 
00071  COPY GA2SSETC.                                                   GA2SPGM 
00072 /                                                                 GA2SPGM 
00073 ******************************************************************GA2SPGM 
00074 **    THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2SPGM 
00075 **  ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2SPGM 
00076 **  HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2SPGM 
00077 **  FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2SPGM 
00078 **  REDEFINES.                                                    GA2SPGM 
00079 ******************************************************************GA2SPGM 
00080      SKIP3                                                        GA2SPGM 
00081  01  FILLER     REDEFINES   GA2SI01I.                             GA2SPGM 
00082      05  FILLER                              PIC X(89).           GA2SPGM 
00083      05  GROUP-SPECIFIC-ID-LINE.                                  GA2SPGM 
00084          10  GRP-SPEC-PLAN-HEADING           PIC X(5).            GA2SPGM 
00085          10  GRP-SPEC-PLAN-CODE              PIC X(3).            GA2SPGM 
00086          10  GRP-SPEC-GROUP-HEADING          PIC X(6).            GA2SPGM 
00087          10  GRP-SPEC-GROUP-NO               PIC X(9).            GA2SPGM 
00088          10  GRP-SPEC-SECTION-HEADING        PIC X(6).            GA2SPGM 
00089          10  GRP-SPEC-SECTION-NO             PIC X(5).            GA2SPGM 
00090          10  GRP-SPEC-PKG-HEADING            PIC X(6).            GA2SPGM 
00091          10  GRP-SPEC-PKG-CODE               PIC X(3).            GA2SPGM 
00092          10  GRP-SPEC-FAM-REL-HEADING        PIC X(5).            GA2SPGM 
00093          10  GRP-SPEC-FAM-REL-LVL            PIC XX.              GA2SPGM 
00094          10  GRP-SPEC-EFF-DT-HEADING         PIC X(7).            GA2SPGM 
00095          10  GRP-SPEC-EFF-DATE               PIC X(6).            GA2SPGM 
00096          10  FILLER                          PIC X(16).           GA2SPGM 
00097      05  CONTRACT-ID-LINE  REDEFINES  GROUP-SPECIFIC-ID-LINE.     GA2SPGM 
00098          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2SPGM 
00099          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2SPGM 
00100          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2SPGM 
00101          10  CONTRACT-GROUP-NO               PIC X(9).            GA2SPGM 
00102          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2SPGM 
00103          10  CONTRACT-SECTION-NO             PIC X(5).            GA2SPGM 
00104          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2SPGM 
00105          10  CONTRACT-PKG-CODE               PIC X(3).            GA2SPGM 
00106          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2SPGM 
00107          10  CONTRACT-LOB                    PIC X.               GA2SPGM 
00108          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2SPGM 
00109          10  CONTRACT-PROV-CTL               PIC XX.              GA2SPGM 
00110          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2SPGM 
00111          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2SPGM 
00112          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2SPGM 
00113          10  CONTRACT-EFF-DATE               PIC X(6).            GA2SPGM 
00114          10  FILLER                          PIC X(1).            GA2SPGM 
00115      05  BENEFIT-PROVISION-ID-LINE  REDEFINES                     GA2SPGM 
00116                                     GROUP-SPECIFIC-ID-LINE.       GA2SPGM 
00117          10  BEN-PROV-PLAN-HEADING           PIC X(4).            GA2SPGM 
00118          10  BEN-PROV-PLAN-CODE              PIC X(3).            GA2SPGM 
00119          10  BEN-PROV-GROUP-HEADING          PIC X(4).            GA2SPGM 
00120          10  BEN-PROV-GROUP-NO               PIC X(9).            GA2SPGM 
00121          10  BEN-PROV-SECTION-HEADING        PIC X(4).            GA2SPGM 
00122          10  BEN-PROV-SECTION-NO             PIC X(5).            GA2SPGM 
00123          10  BEN-PROV-PKG-HEADING            PIC X(4).            GA2SPGM 
00124          10  BEN-PROV-PKG-CODE               PIC X(3).            GA2SPGM 
00125          10  BEN-PROV-LOB-HEADING            PIC X(4).            GA2SPGM 
00126          10  BEN-PROV-LOB                    PIC X.               GA2SPGM 
00127          10  BEN-PROV-PROV-CTL-HEADING       PIC X(4).            GA2SPGM 
00128          10  BEN-PROV-PROV-CTL               PIC XX.              GA2SPGM 
00129          10  BEN-PROV-FAM-REL-HEADING        PIC X(3).            GA2SPGM 
00130          10  BEN-PROV-FAM-REL-LVL            PIC XX.              GA2SPGM 
00131          10  BEN-PROV-EFF-DT-HEADING         PIC X(5).            GA2SPGM 
00132          10  BEN-PROV-EFF-DATE               PIC X(6).            GA2SPGM 
00133          10  BEN-PROV-ID-HEADING             PIC X(6).            GA2SPGM 
00134          10  BEN-PROV-ID-NO                  PIC X(6).            GA2SPGM 
00135          10  FILLER                          PIC X(4).            GA2SPGM 
00136      05  FILLER                              PIC X(78).           GA2SPGM 
00137      05  MAP-PROVIDER-SPC-ARGUMENT-ROW  OCCURS 14 TIMES INDEXED   GA2SPGM 
00138          BY MAP-IDX1.                                             GA2SPGM 
00139        10  MAP-PROVIDER-SPC-ARGUMENT-COL  OCCURS 3 TIMES INDEXED  GA2SPGM 
00140            BY MAP-IDX2.                                           GA2SPGM 
00141          15  MAP-PROVIDER-SPC-ARGUMENT-LEN   PIC S9(4) COMP SYNC. GA2SPGM 
00142          15  MAP-PROVIDER-SPC-ARGUMENT-ATTR  PIC X.               GA2SPGM 
00143          15  MAP-PROVIDER-SPC-ARGUMENT       PIC X(3).            GA2SPGM 
00144      SKIP3                                                        GA2SPGM 
00145  01  FILLER.                                                      GA2SPGM 
00146 ****************************************************************  GA2SPGM 
00147 **   THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.      GA2SPGM 
00148 ****************************************************************  GA2SPGM 
00149      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +14.  GA2SPGM 
00150      05  WS-MAP-COL                  PIC S999 COMP-3  VALUE +3.   GA2SPGM 
00151 /                                                                 GA2SPGM 
00152 ** ALTERNATIVE WORKFILE KEYS **                                   GA2SPGM 
00153  01  FILLER                      PIC X(32)  VALUE                 GA2SPGM 
00154      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2SPGM 
00155  01  WS-ALT-WORKFILE-KEYS.                                        GA2SPGM 
00156  COPY GCWRKKEY.                                                   GA2SPGM 
00157 /                                                                 GA2SPGM 
00158                                                                   GA2SPGM 
00159 ** DATE FORMATTING AREA **                                        GA2SPGM 
00160  01  HGADATES-COMMAREA.                                           GA2SPGM 
00161  COPY HGCDAT01.                                                   GA2SPGM 
00162                                                                   GA2SPGM 
00163 ** WORKFIELDS **                                                  GA2SPGM 
00164  01  FILLER                           PIC X(16)                   GA2SPGM 
00165              VALUE '** WORKFIELDS **'.                            GA2SPGM 
00166  01  WS-WORK-FIELDS.                                              GA2SPGM 
00167      05  WS-HEX-00                    PIC X.                      GA2SPGM 
00168      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2SPGM 
00169      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2SPGM 
00170        VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2SPGM 
00171 ****************************************************************  GA2SPGM 
00172 ** THIS MUST BE CHANGED TO MATCH ONE OCCURENCE OF ENTRY IN TABULARGA2SPGM 
00173 ** RECORD.                                                        GA2SPGM 
00174 ****************************************************************  GA2SPGM 
00175      05  WS-QUOTE-COMP            PIC X   VALUE QUOTE.            GA2SPGM 
00176      05  WS-SAVED-FIELDS.                                         GA2SPGM 
00177        10  WS-SAVED-PROVIDER-TYP-ARGUMENT  PIC X(3).              GA2SPGM 
00178 ****************************************************************  GA2SPGM 
00179 ****************************************************************  GA2SPGM 
00180 ** THIS IS THE AREA IN WHICH THE SORTING OF NEW ENTRIES HAPPENS.  GA2SPGM 
00181 ** NAMES MUST CHANGE ACCORDINGLY, AND ONE MORE OCCURENCE IS       GA2SPGM 
00182 ** PROVIDED THAN IS FOUND ON THE SCREEN, THIS IS FOR THE TRAILER. GA2SPGM 
00183 ****************************************************************  GA2SPGM 
00184      05  WS-PROV-TYP-ARGUMENT-ENTRY  OCCURS 43 TIMES INDEXED BY   GA2SPGM 
00185          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2SPGM 
00186        10  WS-PROVIDER-TYP-ARGUMENT       PIC X(3).               GA2SPGM 
00187 /                                                                 GA2SPGM 
00188 *** SWITCHES ***                                                  GA2SPGM 
00189  01  FILLER                           PIC X(14)                   GA2SPGM 
00190              VALUE '** SWITCHES **'.                              GA2SPGM 
00191  01  WS-SWITCHES.                                                 GA2SPGM 
00192      05  WS-ERROR-SW                  PIC X.                      GA2SPGM 
00193                                                                   GA2SPGM 
00194 ** TITLE LINES **                                                 GA2SPGM 
00195  01  WS-TITLE-LINES.                                              GA2SPGM 
00196      05  GROUP-SPECIFIC-TITLE-LINE       PIC X(46)  VALUE         GA2SPGM 
00197          '  GROUP SPECIFIC INTERNAL TABULAR MAINTENANCE '.        GA2SPGM 
00198      05  CONTRACT-TITLE-LINE             PIC X(46)  VALUE         GA2SPGM 
00199          '     CONTRACT INTERNAL TABULAR MAINTENANCE    '.        GA2SPGM 
00200      05  BENEFIT-PROVISION-TITLE-LINE    PIC X(46)  VALUE         GA2SPGM 
00201          'BENEFIT PROVISION INTERNAL TABULAR MAINTENANCE'.        GA2SPGM 
00202                                                                   GA2SPGM 
00203 *** RECORD LENGTHS ***                                            GA2SPGM 
00204  01  FILLER                           PIC X(20)                   GA2SPGM 
00205              VALUE '** RECORD LENGTHS **'.                        GA2SPGM 
00206  01  WS-RECORD-LENGTHS.                                           GA2SPGM 
00207     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP.             GA2SPGM 
00208     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP.             GA2SPGM 
00209     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2SPGM 
00210     05 WS-GCVI-COMMAREA-LEN           PIC S9(4) COMP   VALUE +19. GA2SPGM 
00211     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2SPGM 
00212 /-------------- GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*GA2SPGM 
00213  01  FILLER.                                                      GA2SPGM 
00214      COPY GCCDRLEN.                                               GA2SPGM 
00215                                                                   GA2SPGM 
00216 ** ATTRIBUTES **                                                  GA2SPGM 
00217  COPY DFHBMSCA.                                                   GA2SPGM 
00218      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2SPGM 
00219 /                                                                 GA2SPGM 
00220 ** ATTENTION IDENTIFIERS **                                       GA2SPGM 
00221  COPY DFHAID.                                                     GA2SPGM 
00222 /                                                                 GA2SPGM 
00223  01  GCVIOPGMS-PARM.                                              GA2SPGM 
00224  COPY GCVINTR2.                                                   GA2SPGM 
00225      SKIP3                                                        GA2SPGM 
00226      SKIP3                                                        GA2SPGM 
00227  01  WS-END                          PIC X(16)  VALUE             GA2SPGM 
00228      '*** W/S ENDS ***'.                                          GA2SPGM 
00229 /                                                                 GA2SPGM 
00230  LINKAGE SECTION.                                                 GA2SPGM 
00231  01  DFHCOMMAREA.                                                 GA2SPGM 
00232  COPY G2ALCKEC.                                                   GA2SPGM 
00233  COPY GACDACWA.                                                   GA2SPGM 
00234 *    05  INCOMING-COMMAREA-PNTR   USAGE IS POINTER.               GA2SPGM 
00235      05  GAS1UPD-PASSED-AREA.                                     GA2SPGM 
00236          07  LVL2-B-SW           PIC X.                           GA2SPGM 
00237          07  LVL2-F-SW           PIC X.                           GA2SPGM 
00238          07  LVL2-G-SW           PIC X.                           GA2SPGM 
00239          07  INTR-TAB-PGM-ID     PIC X(8).                        GA2SPGM 
00240          07  FILLER              PIC X(9).                        GA2SPGM 
00241      05  DELADD-OPTION           PIC X(7).                        GA2SPGM 
00242                                                                   GA2SPGM 
00243                                                                   GA2SPGM 
00244 *01  GCA-COMMAREA.                                                GA2SPGM 
00245 *COPY G2ALCKEC.                                                   GA2SPGM 
00246 /                                                                 GA2SPGM 
00247 ** I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD **         GA2SPGM 
00248  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2SPGM 
00249  COPY GCIOPRM1.                                                   GA2SPGM 
00250 /                                                                 GA2SPGM 
00251  COPY GCWRKDCC.                                                   GA2SPGM 
00252 /                                                                 GA2SPGM 
00253  COPY GCTIPGSC.                                                   GA2SPGM 
00254 /                                                                 GA2SPGM 
00255 ****************************************************************  GA2SPGM 
00256 ** THIS AREA MUST BE CHANGED TO MATCH THE TABLE FROM THE TABULAR  GA2SPGM 
00257 ** RECORD; FIELD NAMES, TYPES, AND THE NUMBER OF OCCURENCES.      GA2SPGM 
00258 ****************************************************************  GA2SPGM 
00259  01  COPY-TABULAR-TABLE-AREA.                                     GA2SPGM 
00260      05  COPY-TABULAR-TABLE  OCCURS 1319 TIMES INDEXED BY         GA2SPGM 
00261            COPY-IDX.                                              GA2SPGM 
00262        10  COPY-PROVIDER-TYP-ARGUMENT   PIC X(3).                 GA2SPGM 
00263 /                                                                 GA2SPGM 
00264 ** IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD **             GA2SPGM 
00265  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2SPGM 
00266  COPY GCIOPRM2.                                                   GA2SPGM 
00267 /                                                                 GA2SPGM 
00268  COPY GCWRKDC2.                                                   GA2SPGM 
00269 /                                                                 GA2SPGM 
00270  COPY GCTABMC.                                                    GA2SPGM 
00271 /                                                                 GA2SPGM 
00272                                                                   GA2SPGM 
00273  PROCEDURE DIVISION.                                              GA2SPGM 
00274                                                                   GA2SPGM 
00275 ******************************************************************GA2SPGM 
00276 **                H O U S E K E E P I N G                         GA2SPGM 
00277 **                                                                GA2SPGM 
00278 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2SPGM 
00279 **                                                                GA2SPGM 
00280 ******************************************************************GA2SPGM 
00281  0000-HOUSEKEEPING  SECTION.                                      GA2SPGM 
00282                                                                   GA2SPGM 
00283      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2SPGM 
00284                                                                   GA2SPGM 
00285      IF EIBAID  =  DFHCLEAR                                       GA2SPGM 
00286          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA2SPGM 
00287                         ERASE                                     GA2SPGM 
00288          END-EXEC                                                 GA2SPGM 
00289          EXEC CICS RETURN                                         GA2SPGM 
00290          END-EXEC.                                                GA2SPGM 
00291                                                                   GA2SPGM 
00292      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2SPGM 
00293                END-EXEC.                                          GA2SPGM 
00294  0000-EXIT.                                                       GA2SPGM 
00295        EXIT.                                                      GA2SPGM 
00296 /*****************************************************************GA2SPGM 
00297 **                     M A I N L I N E                            GA2SPGM 
00298 **                                                                GA2SPGM 
00299 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2SPGM 
00300 **  TAKEN BY THE OPERATOR.                                        GA2SPGM 
00301 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2SPGM 
00302 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2SPGM 
00303 **     ADDITIONS FROM.                                            GA2SPGM 
00304 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2SPGM 
00305 **     KEY PF12 OR PF24.                                          GA2SPGM 
00306 **  3. RECEIVE THE SCREEN.                                        GA2SPGM 
00307 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2SPGM 
00308 **     MENU.                                                      GA2SPGM 
00309 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2SPGM 
00310 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2SPGM 
00311 **     (RETURN) TO THE DELETE PROGRAM (GA1SPGM).                  GA2SPGM 
00312 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2SPGM 
00313 **     (RETURN) TO THE PREVIOUS MENU.                             GA2SPGM 
00314 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2SPGM 
00315 **     NORMAL ADD LOGIC, EXCEPT BYPASS EMPTY VALIDATION TABLE     GA2SPGM 
00316 **     CONDITION FOR PROVIDER TYPE ARGUMENT FIELD.                GA2SPGM 
00317 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2SPGM 
00318 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2SPGM 
00319 **                                                                GA2SPGM 
00320 ******************************************************************GA2SPGM 
00321  1000-MAIN-LINE  SECTION.                                         GA2SPGM 
00322                                                                   GA2SPGM 
00323      MOVE '1000'  TO  WS-PARA-ID.                                 GA2SPGM 
00324      IF EIBTRNID  NOT =  'GA2S'                                   GA2SPGM 
00325         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2SPGM 
00326         GO TO 1099-RETURN.                                        GA2SPGM 
00327                                                                   GA2SPGM 
00328      EXEC CICS RECEIVE   MAP('GA2SI01') MAPSET('GA2SSET')         GA2SPGM 
00329         INTO(GA2SI01I) END-EXEC.                                  GA2SPGM 
00330                                                                   GA2SPGM 
00331      IF SCRNIDNI  NOT =  '002I00'                                 GA2SPGM 
00332         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2SPGM 
00333                                                                   GA2SPGM 
00334      IF EIBAID  =  DFHENTER                                       GA2SPGM 
00335         PERFORM 2000-ADD-PROCESSING                               GA2SPGM 
00336         GO TO 1099-RETURN.                                        GA2SPGM 
00337                                                                   GA2SPGM 
00338      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2SPGM 
00339         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2SPGM 
00340                                                                   GA2SPGM 
00341      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2SPGM 
00342         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2SPGM 
00343                                                                   GA2SPGM 
00344      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2SPGM 
00345         PERFORM 2000-ADD-PROCESSING                               GA2SPGM 
00346         GO TO 1099-RETURN.                                        GA2SPGM 
00347                                                                   GA2SPGM 
00348      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2SPGM 
00349      MOVE -1  TO                                                  GA2SPGM 
00350         MAP-PROVIDER-SPC-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).       GA2SPGM 
00351      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2SPGM 
00352 -    ' THIS PROGRAM ***'  TO  ERRMSGO.                            GA2SPGM 
00353      EXEC CICS SEND   MAP('GA2SI01') MAPSET('GA2SSET') DATAONLY   GA2SPGM 
00354         FROM(GA2SI01O) CURSOR END-EXEC.                           GA2SPGM 
00355                                                                   GA2SPGM 
00356  1099-RETURN.                                                     GA2SPGM 
00357      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2SPGM 
00358         (DELADD-OPTION = 'GAS1UPD') OR                            GA2SPGM 
00359         (DELADD-OPTION = 'GAS2UPD') OR                            GA2SPGM 
00360         (DELADD-OPTION = 'GAS3UPD') OR                            GA2SPGM 
00361         (DELADD-OPTION = 'GAS4UPD') OR                            GA2SPGM 
00362         (DELADD-OPTION = 'GAS5UPD')                               GA2SPGM 
00363          EXEC CICS RETURN END-EXEC                                GA2SPGM 
00364      ELSE                                                         GA2SPGM 
00365          EXEC CICS RETURN TRANSID('GA2S')                         GA2SPGM 
00366                    COMMAREA(DFHCOMMAREA)                          GA2SPGM 
00367                    LENGTH  (EIBCALEN)                             GA2SPGM 
00368                    END-EXEC.                                      GA2SPGM 
00369                                                                   GA2SPGM 
00370      GOBACK.                                                      GA2SPGM 
00371  1099-EXIT.                                                       GA2SPGM 
00372        EXIT.                                                      GA2SPGM 
00373 /*****************************************************************GA2SPGM 
00374 **               A D D   P R O C E S S I N G                      GA2SPGM 
00375 **                                                                GA2SPGM 
00376 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2SPGM 
00377 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2SPGM.            GA2SPGM 
00378 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2SPGM 
00379 **  2. DETERMINE IF ANY VALUE WERE ENTERED FOR THIS LINE.   IF NOTGA2SPGM 
00380 **     SKIP TO THE NEXT LINE.                                     GA2SPGM 
00381 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2SPGM 
00382 **     WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2SPGM 
00383 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2SPGM 
00384 **     DATA.                                                      GA2SPGM 
00385 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2SPGM 
00386 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2SPGM 
00387 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2SPGM 
00388 **     ORDER, FIELD BY FIELD.                                     GA2SPGM 
00389 **  6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2SPGM 
00390 **     MADE.                                                      GA2SPGM 
00391 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2SPGM 
00392 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2SPGM 
00393 **     BACK INTO THE RECORD.                                      GA2SPGM 
00394 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2SPGM 
00395 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2SPGM 
00396 **                                                                GA2SPGM 
00397 ******************************************************************GA2SPGM 
00398  2000-ADD-PROCESSING SECTION.                                     GA2SPGM 
00399                                                                   GA2SPGM 
00400      MOVE '2000'  TO  WS-PARA-ID.                                 GA2SPGM 
00401      MOVE 'N'     TO  WS-ERROR-SW.                                GA2SPGM 
00402      MOVE 'Y'     TO  GCVI2-TABLE-SW.                             GA2SPGM 
00403      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2SPGM 
00404      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2SPGM 
00405                                                                   GA2SPGM 
00406      MOVE '2005'  TO  WS-PARA-ID.                                 GA2SPGM 
00407  2005-RESET-ALL-ATTRIBUTES.                                       GA2SPGM 
00408      MOVE DFHBMUNF  TO                                            GA2SPGM 
00409         MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2).      GA2SPGM 
00410      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2SPGM 
00411         SET MAP-IDX1  UP BY  1                                    GA2SPGM 
00412         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2SPGM 
00413      IF MAP-IDX2  <  WS-MAP-COL                                   GA2SPGM 
00414         SET MAP-IDX1  TO  1                                       GA2SPGM 
00415         SET MAP-IDX2  UP BY  1                                    GA2SPGM 
00416         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2SPGM 
00417                                                                   GA2SPGM 
00418      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2SPGM 
00419      MOVE '2010'  TO  WS-PARA-ID.                                 GA2SPGM 
00420  2010-VALIDATE-ADD-ENTRIES.                                       GA2SPGM 
00421      IF MAP-PROVIDER-SPC-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)        GA2SPGM 
00422            =  ZERO                                                GA2SPGM 
00423         IF MAP-IDX1  <  WS-MAP-ROW                                GA2SPGM 
00424            SET MAP-IDX1  UP BY  1                                 GA2SPGM 
00425            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2SPGM 
00426         ELSE                                                      GA2SPGM 
00427            IF MAP-IDX2  <  WS-MAP-COL                             GA2SPGM 
00428               SET MAP-IDX1  TO  1                                 GA2SPGM 
00429               SET MAP-IDX2  UP BY  1                              GA2SPGM 
00430               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2SPGM 
00431            ELSE                                                   GA2SPGM 
00432               GO TO 2020-CHECK-FOR-ERRORS.                        GA2SPGM 
00433                                                                   GA2SPGM 
00434      IF MAP-PROVIDER-SPC-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2) = ZERO GA2SPGM 
00435         MOVE DFHBMUBF  TO                                         GA2SPGM 
00436            MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)    GA2SPGM 
00437         MOVE '??????' TO                                          GA2SPGM 
00438            MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)         GA2SPGM 
00439         IF WS-ERROR-SW  NOT =  'Y'                                GA2SPGM 
00440            MOVE 'Y'  TO  WS-ERROR-SW                              GA2SPGM 
00441            MOVE -1   TO                                           GA2SPGM 
00442               MAP-PROVIDER-SPC-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2)  GA2SPGM 
00443            MOVE ' *** PROVIDER SPEC ARGUMENT IS INVALID ***'      GA2SPGM 
00444               TO  ERRMSGO                                         GA2SPGM 
00445         ELSE                                                      GA2SPGM 
00446            NEXT SENTENCE                                          GA2SPGM 
00447      ELSE                                                         GA2SPGM 
00448         MOVE MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)  TO   GA2SPGM 
00449            WS-SAVED-PROVIDER-TYP-ARGUMENT                         GA2SPGM 
00450         INSPECT WS-SAVED-PROVIDER-TYP-ARGUMENT                    GA2SPGM 
00451                 REPLACING  ALL  WS-QUOTE-COMP BY '\
00452         INSPECT WS-SAVED-PROVIDER-TYP-ARGUMENT                    GA2SPGM 
00453               REPLACING   CHARACTERS BY  WS-QUOTE-COMP            GA2SPGM 
00454         IF WS-SAVED-PROVIDER-TYP-ARGUMENT NOT = QUOTES            GA2SPGM 
00455            MOVE DFHBMUBF  TO                                      GA2SPGM 
00456               MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2) GA2SPGM 
00457            IF WS-ERROR-SW NOT = 'Y'                               GA2SPGM 
00458                MOVE 'Y' TO WS-ERROR-SW                            GA2SPGM 
00459                MOVE -1 TO                                         GA2SPGM 
00460                   MAP-PROVIDER-SPC-ARGUMENT-LEN                   GA2SPGM 
00461                      (MAP-IDX1, MAP-IDX2)                         GA2SPGM 
00462                MOVE ' *** PROVIDER SPEC ARGUMENT IS INVALID ***'  GA2SPGM 
00463                   TO ERRMSGO.                                     GA2SPGM 
00464                                                                   GA2SPGM 
00465      IF MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA2SPGM 
00466            NOT  =  DFHBMUBF                                       GA2SPGM 
00467         ADD 1  TO  WS-ADD-COUNT                                   GA2SPGM 
00468         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2SPGM 
00469         MOVE MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2) TO    GA2SPGM 
00470            WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX).                GA2SPGM 
00471                                                                   GA2SPGM 
00472      IF MAP-PROVIDER-SPC-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA2SPGM 
00473          NOT = DFHBMUBF                                           GA2SPGM 
00474         MOVE  'BPRV02' TO GCVI2-FIELDS-KEY-ID                     GA2SPGM 
00475         MOVE  ZEROES   TO GCVI2-RETURN-CODE                       GA2SPGM 
00476         MOVE MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2)       GA2SPGM 
00477                  TO GCVI2-VALUE-LEN-3                             GA2SPGM 
00478         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2SPGM 
00479                        COMMAREA(GCVIOPGMS-PARM)                   GA2SPGM 
00480                        LENGTH(WS-GCVI-COMMAREA-LEN) END-EXEC      GA2SPGM 
00481         IF GCVI2-VALUE-NOT-FOUND                                  GA2SPGM 
00482            IF WS-ERROR-SW NOT = 'Y'                               GA2SPGM 
00483               MOVE DFHBMUBF  TO                                   GA2SPGM 
00484                 MAP-PROVIDER-SPC-ARGUMENT-ATTR(MAP-IDX1, MAP-IDX2)GA2SPGM 
00485               MOVE -1  TO                                         GA2SPGM 
00486                  MAP-PROVIDER-SPC-ARGUMENT-LEN(MAP-IDX1, MAP-IDX2)GA2SPGM 
00487               MOVE '*** PROVIDER SPEC CODE INVALID ***'  TO       GA2SPGM 
00488                                                         ERRMSGO   GA2SPGM 
00489               MOVE 'Y' TO WS-ERROR-SW                             GA2SPGM 
00490            ELSE                                                   GA2SPGM 
00491               MOVE DFHBMUBF  TO                                   GA2SPGM 
00492                 MAP-PROVIDER-SPC-ARGUMENT-ATTR(MAP-IDX1, MAP-IDX2)GA2SPGM 
00493         ELSE                                                      GA2SPGM 
00494            IF GCVI2-VALUE-NOT-LOADED                              GA2SPGM 
00495               IF EIBAID  =  DFHPF4 OR  =  DFHPF16                 GA2SPGM 
00496                  NEXT SENTENCE                                    GA2SPGM 
00497               ELSE                                                GA2SPGM 
00498                  MOVE DFHBMUBF  TO                                GA2SPGM 
00499                 MAP-PROVIDER-SPC-ARGUMENT-ATTR(MAP-IDX1, MAP-IDX2)GA2SPGM 
00500                  IF WS-ERROR-SW  NOT =  'Y'                       GA2SPGM 
00501                     MOVE 'Y'  TO  WS-ERROR-SW                     GA2SPGM 
00502                     MOVE 'N' TO GCVI2-TABLE-SW                    GA2SPGM 
00503                     MOVE -1  TO                                   GA2SPGM 
00504                  MAP-PROVIDER-SPC-ARGUMENT-LEN(MAP-IDX1, MAP-IDX2)GA2SPGM 
00505               MOVE 'EDIT TABLE EMPTY - DATA NOT VALIDATED -  PRESSGA2SPGM 
00506 -              ' PF4 / PF16 TO CONTINUE' TO  ERRMSGO.             GA2SPGM 
00507                                                                   GA2SPGM 
00508      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2SPGM 
00509         SET MAP-IDX1  UP BY  1                                    GA2SPGM 
00510         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2SPGM 
00511      IF MAP-IDX2  <  WS-MAP-COL                                   GA2SPGM 
00512         SET MAP-IDX1  TO  1                                       GA2SPGM 
00513         SET MAP-IDX2  UP BY  1                                    GA2SPGM 
00514         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2SPGM 
00515                                                                   GA2SPGM 
00516  2020-CHECK-FOR-ERRORS.                                           GA2SPGM 
00517      MOVE '2020'  TO  WS-PARA-ID.                                 GA2SPGM 
00518                                                                   GA2SPGM 
00519      IF INCEXCI  NOT =  'I' AND  NOT =  'E'                       GA2SPGM 
00520         MOVE -1  TO  INCEXCL                                      GA2SPGM 
00521         MOVE 'Y'  TO  WS-ERROR-SW                                 GA2SPGM 
00522         MOVE '*** INCLUDE/EXCLUDE FIELD VALUE NOT VALID ***'  TO  GA2SPGM 
00523            ERRMSGO.                                               GA2SPGM 
00524                                                                   GA2SPGM 
00525      IF WS-ERROR-SW  =  'Y' OR GCVI2-TABLE-SW = 'N'               GA2SPGM 
00526         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2SPGM 
00527            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2SPGM 
00528            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2SPGM 
00529            INCEXCO                                                GA2SPGM 
00530         MOVE '2100'  TO  WS-PARA-ID                               GA2SPGM 
00531         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2SPGM 
00532            VARYING MAP-IDX2 FROM  1  BY  1                        GA2SPGM 
00533               UNTIL MAP-IDX2  >  WS-MAP-COL                       GA2SPGM 
00534            AFTER MAP-IDX1 FROM  1  BY  1                          GA2SPGM 
00535               UNTIL MAP-IDX1  >  WS-MAP-ROW                       GA2SPGM 
00536         EXEC CICS SEND   MAP('GA2SI01') MAPSET('GA2SSET')         GA2SPGM 
00537            DATAONLY FROM(GA2SI01O) CURSOR END-EXEC                GA2SPGM 
00538         GO TO 2099-EXIT.                                          GA2SPGM 
00539                                                                   GA2SPGM 
00540      IF WS-ADD-COUNT  NOT >  ZERO AND                             GA2SPGM 
00541         INCEXCI  =  INEXDRKI                                      GA2SPGM 
00542         MOVE '*** NO ADD ENTRY FOUND OR INC/EXC FIELD CHANGE ***' GA2SPGM 
00543            TO  ERRMSGO                                            GA2SPGM 
00544         MOVE -1  TO  INCEXCL                                      GA2SPGM 
00545         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA2SPGM 
00546            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO,  IDLINEO,   GA2SPGM 
00547            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO,             GA2SPGM 
00548            INCEXCO                                                GA2SPGM 
00549         EXEC CICS SEND   MAP('GA2SI01') MAPSET('GA2SSET')         GA2SPGM 
00550            DATAONLY FROM(GA2SI01O) CURSOR END-EXEC                GA2SPGM 
00551         GO TO 2099-EXIT.                                          GA2SPGM 
00552                                                                   GA2SPGM 
00553  2025-CONTINUE-PROCESSING.                                        GA2SPGM 
00554                                                                   GA2SPGM 
00555      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2SPGM 
00556               GC-GCIOPARM-LEN                 +                   GA2SPGM 
00557               GC-WORKFILE-KEY-LEN             +                   GA2SPGM 
00558               GC-GCTABULR-IPGS-FIXED-LEN      +                   GA2SPGM 
00559              (GC-GCTABULR-IPGS-VARY-LEN       *                   GA2SPGM 
00560               GC-GCTABULR-IPGS-VARY-MAX-OCUR)                     GA2SPGM 
00561                                                                   GA2SPGM 
00562                                                                   GA2SPGM 
00563      EXEC CICS                                                    GA2SPGM 
00564         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2SPGM 
00565         INITIMG(WS-HEX-00)                                        GA2SPGM 
00566         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2SPGM 
00567      END-EXEC.                                                    GA2SPGM 
00568                                                                   GA2SPGM 
00569      IF  FRMNUIDI  =  'GS3A'                                      GA2SPGM 
00570         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2SPGM 
00571         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2SPGM 
00572         MOVE  'G4' TO GCIO-WRK-RECORD-TYPE                        GA2SPGM 
00573         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2SPGM 
00574         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2SPGM 
00575         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2SPGM 
00576         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2SPGM 
00577         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2SPGM 
00578                          GCIO-WRK-PROVIDER-CONTROL                GA2SPGM 
00579         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2SPGM 
00580         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2SPGM 
00581                                                                   GA2SPGM 
00582      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2SPGM 
00583         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2SPGM 
00584         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2SPGM 
00585         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2SPGM 
00586         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2SPGM 
00587         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2SPGM 
00588         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2SPGM 
00589         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2SPGM 
00590         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2SPGM 
00591         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2SPGM 
00592         MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2SPGM 
00593         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2SPGM 
00594                                                                   GA2SPGM 
00595      IF  FRMNUIDI  =  'GC8A'                                      GA2SPGM 
00596         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2SPGM 
00597         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2SPGM 
00598         MOVE  'C6' TO GCIO-WRK-RECORD-TYPE                        GA2SPGM 
00599         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2SPGM 
00600         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NUM            GA2SPGM 
00601         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NUM        GA2SPGM 
00602         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2SPGM 
00603         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2SPGM 
00604         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2SPGM 
00605         MOVE BEN-PROV-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVLGA2SPGM 
00606         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                 GA2SPGM 
00607                                                                   GA2SPGM 
00608      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2SPGM 
00609      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2SPGM 
00610      MOVE ALTABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2SPGM 
00611      MOVE ALTBSLTI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2SPGM 
00612      MOVE INTABIDI  TO  GCIO-WRK-TAB-PROVISION-ID.                GA2SPGM 
00613      MOVE INTBSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2SPGM 
00614      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2SPGM 
00615                                                                   GA2SPGM 
00616      MOVE  GC-GCTABULR-IPGS-VARY-MAX-OCUR                         GA2SPGM 
00617            TO  GXS-ENTRY-COUNT.                                   GA2SPGM 
00618      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2SPGM 
00619                                                                   GA2SPGM 
00620      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2SPGM 
00621         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2SPGM 
00622         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2SPGM 
00623                                                                   GA2SPGM 
00624      IF  NOT GCIO-GOOD-RETURN                                     GA2SPGM 
00625         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2SPGM 
00626 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2SPGM 
00627         MOVE '2IF1'  TO  WS-ABEND-CODE                            GA2SPGM 
00628         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00629                                                                   GA2SPGM 
00630      MOVE INCEXCI  TO  INEXDRKO,  GXS-INCLUDE-EXCLUDE-IND.        GA2SPGM 
00631      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2SPGM 
00632         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2SPGM 
00633                                                                   GA2SPGM 
00634       SET WS-SORT-IDX  TO  1.                                     GA2SPGM 
00635       SET WS-SORT-IDX2  TO  2.                                    GA2SPGM 
00636       MOVE '2030'  TO  WS-PARA-ID.                                GA2SPGM 
00637                                                                   GA2SPGM 
00638  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2SPGM 
00639      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2SPGM 
00640         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2SPGM 
00641                                                                   GA2SPGM 
00642      IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) <                  GA2SPGM 
00643         WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2)                   GA2SPGM 
00644         SET WS-SORT-IDX2  UP BY  1                                GA2SPGM 
00645         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2SPGM 
00646      ELSE                                                         GA2SPGM 
00647         IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) >               GA2SPGM 
00648            WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2)                GA2SPGM 
00649            MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) TO         GA2SPGM 
00650               WS-SAVED-PROVIDER-TYP-ARGUMENT                      GA2SPGM 
00651            MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2) TO        GA2SPGM 
00652               WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX)              GA2SPGM 
00653            MOVE WS-SAVED-PROVIDER-TYP-ARGUMENT  TO                GA2SPGM 
00654               WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2)             GA2SPGM 
00655            SET WS-SORT-IDX2  UP BY  1                             GA2SPGM 
00656            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2SPGM 
00657                                                                   GA2SPGM 
00658      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2SPGM 
00659      MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX3) TO              GA2SPGM 
00660         WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX2).                  GA2SPGM 
00661      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2SPGM 
00662      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2SPGM 
00663                                                                   GA2SPGM 
00664  2040-ARE-WE-DONE-WITH-SORT.                                      GA2SPGM 
00665      MOVE '2040'  TO  WS-PARA-ID.                                 GA2SPGM 
00666      SET WS-SORT-IDX  UP BY  1.                                   GA2SPGM 
00667      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2SPGM 
00668         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2SPGM 
00669         SET WS-SORT-IDX2  UP BY  1                                GA2SPGM 
00670         MOVE '2030'  TO  WS-PARA-ID                               GA2SPGM 
00671         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2SPGM 
00672      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2SPGM 
00673      MOVE HIGH-VALUES TO WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX).GA2SPGM 
00674      MOVE GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT.                   GA2SPGM 
00675                                                                   GA2SPGM 
00676      COMPUTE  WS-COPY-LENGTH  =                                   GA2SPGM 
00677                GXS-ENTRY-COUNT  *  GC-GCTABULR-IPGS-VARY-LEN.     GA2SPGM 
00678                                                                   GA2SPGM 
00679      EXEC CICS                                                    GA2SPGM 
00680         GETMAIN  SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)          GA2SPGM 
00681         LENGTH      (WS-COPY-LENGTH)                              GA2SPGM 
00682         INITIMG     (WS-HEX-00)                                   GA2SPGM 
00683      END-EXEC.                                                    GA2SPGM 
00684                                                                   GA2SPGM 
00685      MOVE GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT.                   GA2SPGM 
00686      SET COPY-IDX,  GXS-INDEX  TO  1.                             GA2SPGM 
00687                                                                   GA2SPGM 
00688      MOVE '2050'  TO  WS-PARA-ID.                                 GA2SPGM 
00689  2050-MAKE-A-COPY-OF-RECORD.                                      GA2SPGM 
00690      IF GXS-INDEX  NOT >  GXS-ENTRY-COUNT                         GA2SPGM 
00691         MOVE GXS-ENTRY (GXS-INDEX)  TO                            GA2SPGM 
00692            COPY-TABULAR-TABLE (COPY-IDX)                          GA2SPGM 
00693         SET COPY-IDX, GXS-INDEX  UP BY  1                         GA2SPGM 
00694         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2SPGM 
00695                                                                   GA2SPGM 
00696      IF WS-ADD-COUNT  +  GXS-ENTRY-COUNT  >                       GA2SPGM 
00697                          GC-GCTABULR-IPGS-VARY-MAX-OCUR           GA2SPGM 
00698         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2SPGM 
00699 -    'EASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                 GA2SPGM 
00700         MOVE '2IL1'  TO  WS-ABEND-CODE                            GA2SPGM 
00701         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00702                                                                   GA2SPGM 
00703      SET WS-SORT-IDX,  COPY-IDX,  GXS-INDEX  TO  1.               GA2SPGM 
00704                                                                   GA2SPGM 
00705      MOVE '2060'  TO  WS-PARA-ID.                                 GA2SPGM 
00706  2060-MERGE-IN-NEW-ENTRIES.                                       GA2SPGM 
00707      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2SPGM 
00708         SET GXS-INDEX  DOWN BY  1                                 GA2SPGM 
00709         SET GXS-ENTRY-COUNT  TO  GXS-INDEX                        GA2SPGM 
00710         MOVE GXS-ENTRY-COUNT  TO  GXS-ENTRY-COUNT                 GA2SPGM 
00711         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2SPGM 
00712                                                                   GA2SPGM 
00713      IF WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX)                  GA2SPGM 
00714               =  HIGH-VALUES  AND                                 GA2SPGM 
00715         COPY-TABULAR-TABLE (COPY-IDX)  NOT =  HIGH-VALUES         GA2SPGM 
00716         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2SPGM 
00717                                                                   GA2SPGM 
00718      IF WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX)                  GA2SPGM 
00719               NOT =  HIGH-VALUES AND                              GA2SPGM 
00720         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2SPGM 
00721         GO TO 2080-INSERT-NEW-ENTRY.                              GA2SPGM 
00722                                                                   GA2SPGM 
00723      IF WS-PROV-TYP-ARGUMENT-ENTRY (WS-SORT-IDX)                  GA2SPGM 
00724               =  HIGH-VALUES AND                                  GA2SPGM 
00725         COPY-TABULAR-TABLE (COPY-IDX)  =  HIGH-VALUES             GA2SPGM 
00726         NEXT SENTENCE                                             GA2SPGM 
00727      ELSE                                                         GA2SPGM 
00728         IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) >               GA2SPGM 
00729            COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)                  GA2SPGM 
00730            GO TO 2070-SAVE-COPIED-ENTRY                           GA2SPGM 
00731         ELSE                                                      GA2SPGM 
00732            IF WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) <            GA2SPGM 
00733               COPY-PROVIDER-TYP-ARGUMENT (COPY-IDX)               GA2SPGM 
00734               GO TO 2080-INSERT-NEW-ENTRY.                        GA2SPGM 
00735                                                                   GA2SPGM 
00736 **   AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2SPGM 
00737 **   OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2SPGM 
00738 **   INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2SPGM 
00739 **   FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2SPGM 
00740                                                                   GA2SPGM 
00741      SET WS-SORT-IDX  UP BY  1.                                   GA2SPGM 
00742                                                                   GA2SPGM 
00743  2070-SAVE-COPIED-ENTRY.                                          GA2SPGM 
00744      MOVE '2070'  TO  WS-PARA-ID.                                 GA2SPGM 
00745      MOVE COPY-TABULAR-TABLE (COPY-IDX)  TO                       GA2SPGM 
00746         GXS-ENTRY (GXS-INDEX).                                    GA2SPGM 
00747      IF COPY-IDX  NOT >  GXS-ENTRY-COUNT                          GA2SPGM 
00748         SET COPY-IDX  UP BY  1                                    GA2SPGM 
00749         SET GXS-INDEX  UP BY  1                                   GA2SPGM 
00750         MOVE '2060'  TO  WS-PARA-ID                               GA2SPGM 
00751         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2SPGM 
00752      ELSE                                                         GA2SPGM 
00753         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2SPGM 
00754 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2SPGM 
00755         MOVE '2IL2'  TO  WS-ABEND-CODE                            GA2SPGM 
00756         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00757                                                                   GA2SPGM 
00758  2080-INSERT-NEW-ENTRY.                                           GA2SPGM 
00759      MOVE '2080'  TO  WS-PARA-ID.                                 GA2SPGM 
00760      MOVE WS-PROVIDER-TYP-ARGUMENT (WS-SORT-IDX) TO               GA2SPGM 
00761         GXS-PROVIDER-SPEC-ARGUMENT (GXS-INDEX).                   GA2SPGM 
00762                                                                   GA2SPGM 
00763      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2SPGM 
00764         SET WS-SORT-IDX  UP BY  1                                 GA2SPGM 
00765         SET GXS-INDEX  UP BY  1                                   GA2SPGM 
00766         MOVE '2060'  TO  WS-PARA-ID                               GA2SPGM 
00767         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2SPGM 
00768      ELSE                                                         GA2SPGM 
00769         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2SPGM 
00770 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  ERRMSGO               GA2SPGM 
00771         MOVE '2IL3'  TO  WS-ABEND-CODE                            GA2SPGM 
00772         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00773                                                                   GA2SPGM 
00774  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2SPGM 
00775      MOVE '2090'  TO  WS-PARA-ID.                                 GA2SPGM 
00776                                                                   GA2SPGM 
00777 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2SPGM 
00778                                                                   GA2SPGM 
00779      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2SPGM 
00780      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2SPGM 
00781                                                                   GA2SPGM 
00782      COMPUTE  GCIO-RECORD-LENGTH  =                               GA2SPGM 
00783               GC-WORKFILE-KEY-LEN             +                   GA2SPGM 
00784               GC-GCTABULR-IPGS-FIXED-LEN      +                   GA2SPGM 
00785              (GC-GCTABULR-IPGS-VARY-LEN       *  GXS-ENTRY-COUNT).GA2SPGM 
00786                                                                   GA2SPGM 
00787      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2SPGM 
00788            GC-GCIOPARM-LEN   +  GCIO-RECORD-LENGTH.               GA2SPGM 
00789                                                                   GA2SPGM 
00790      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2SPGM 
00791         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2SPGM 
00792         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2SPGM 
00793                                                                   GA2SPGM 
00794      IF NOT GCIO-GOOD-RETURN                                      GA2SPGM 
00795         MOVE '*** ERROR REWRITING ALL LEVEL INTERNAL TABULAR RECORGA2SPGM 
00796 -    'D.  CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                  GA2SPGM 
00797         MOVE '2IF2'  TO  WS-ABEND-CODE                            GA2SPGM 
00798         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00799                                                                   GA2SPGM 
00800      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2SPGM 
00801         VARYING MAP-IDX2 FROM 1  BY  1                            GA2SPGM 
00802            UNTIL  MAP-IDX2  >  WS-MAP-COL                         GA2SPGM 
00803         AFTER MAP-IDX1 FROM 1  BY  1                              GA2SPGM 
00804            UNTIL  MAP-IDX1  >  WS-MAP-ROW.                        GA2SPGM 
00805                                                                   GA2SPGM 
00806      EXEC CICS SEND   MAP('GA2SI01') MAPSET('GA2SSET') ERASE      GA2SPGM 
00807         FROM(GA2SI01O) END-EXEC.                                  GA2SPGM 
00808                                                                   GA2SPGM 
00809  2099-EXIT.   EXIT.                                               GA2SPGM 
00810 /                                                                 GA2SPGM 
00811 ******************************************************************GA2SPGM 
00812 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2SPGM 
00813 **                                                                GA2SPGM 
00814 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2SPGM 
00815 **  ALREADY ON THE OPERATORS SCREEN.                              GA2SPGM 
00816 **                                                                GA2SPGM 
00817 ******************************************************************GA2SPGM 
00818  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2SPGM 
00819                                                                   GA2SPGM 
00820      MOVE LOW-VALUES  TO                                          GA2SPGM 
00821           MAP-PROVIDER-SPC-ARGUMENT (MAP-IDX1, MAP-IDX2).         GA2SPGM 
00822                                                                   GA2SPGM 
00823  2199-EXIT.   EXIT.                                               GA2SPGM 
00824 /                                                                 GA2SPGM 
00825 ******************************************************************GA2SPGM 
00826 **          X C T L   T O   D E L   S C R E E N                   GA2SPGM 
00827 **                                                                GA2SPGM 
00828 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2SPGM 
00829 ** DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2SPGM 
00830 ** PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2SPGM 
00831 ** TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2SPGM 
00832 ** THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2SPGM 
00833 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2SPGM 
00834 ******************************************************************GA2SPGM 
00835  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2SPGM 
00836      MOVE '3000'  TO  WS-PARA-ID.                                 GA2SPGM 
00837                                                                   GA2SPGM 
00838      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2SPGM 
00839               GC-GCIOPARM-LEN                 +                   GA2SPGM 
00840               GC-WORKFILE-KEY-LEN             +                   GA2SPGM 
00841               GC-GCTABULR-IPGS-FIXED-LEN      +                   GA2SPGM 
00842              (GC-GCTABULR-IPGS-VARY-LEN       *                   GA2SPGM 
00843               GC-GCTABULR-IPGS-VARY-MAX-OCUR)                     GA2SPGM 
00844                                                                   GA2SPGM 
00845                                                                   GA2SPGM 
00846      EXEC CICS                                                    GA2SPGM 
00847         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2SPGM 
00848         INITIMG(WS-HEX-00)                                        GA2SPGM 
00849         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)                    GA2SPGM 
00850      END-EXEC.                                                    GA2SPGM 
00851                                                                   GA2SPGM 
00852 *    EXEC CICS                                                    GA2SPGM 
00853 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2SPGM 
00854 *       LENGTH      (WS-COMMUNICATION-KEY-LEN)                    GA2SPGM 
00855 *       INITIMG     (WS-HEX-00)                                   GA2SPGM 
00856 *    END-EXEC.                                                    GA2SPGM 
00857                                                                   GA2SPGM 
00858      IF  FRMNUIDI  =  'GS3A'                                      GA2SPGM 
00859         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2SPGM 
00860         MOVE   'G'   TO  GCIO-WRK-STATUS-CODE                     GA2SPGM 
00861         MOVE   'G4'  TO  GCIO-WRK-RECORD-TYPE                     GA2SPGM 
00862         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2SPGM 
00863         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2SPGM 
00864         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2SPGM 
00865         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2SPGM 
00866         MOVE  SPACES  TO  GCIO-WRK-LINE-OF-BUS                    GA2SPGM 
00867                           GCIO-WRK-PROVIDER-CONTROL               GA2SPGM 
00868         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2SPGM 
00869         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2SPGM 
00870                                                                   GA2SPGM 
00871      IF  FRMNUIDI  =  'GC4A' OR 'GTM1'                            GA2SPGM 
00872         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2SPGM 
00873         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2SPGM 
00874         MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE                     GA2SPGM 
00875         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2SPGM 
00876         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2SPGM 
00877         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2SPGM 
00878         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2SPGM 
00879         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2SPGM 
00880         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2SPGM 
00881         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2SPGM 
00882         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                GA2SPGM 
00883                                                                   GA2SPGM 
00884      IF  FRMNUIDI  =  'GC8A'                                      GA2SPGM 
00885         MOVE  SPACES TO  GCIO-WORKFILE-KEY                        GA2SPGM 
00886         MOVE   'C'   TO  GCIO-WRK-STATUS-CODE                     GA2SPGM 
00887         MOVE   'C6'  TO  GCIO-WRK-RECORD-TYPE                     GA2SPGM 
00888         MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                 GA2SPGM 
00889         MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                 GA2SPGM 
00890         MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM             GA2SPGM 
00891         MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                   GA2SPGM 
00892         MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                   GA2SPGM 
00893         MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL           GA2SPGM 
00894         MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL     GA2SPGM 
00895         MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                 GA2SPGM 
00896         MOVE  GCA-BEN-PROV-ID TO  GCIO-WRK-PROVISION-ID.          GA2SPGM 
00897                                                                   GA2SPGM 
00898      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2SPGM 
00899      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2SPGM 
00900      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2SPGM 
00901      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2SPGM 
00902      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA2SPGM 
00903      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2SPGM 
00904      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2SPGM 
00905      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA2SPGM 
00906                                                                   GA2SPGM 
00907      MOVE INCEXCI TO GCA-I-E-INDC.                                GA2SPGM 
00908                                                                   GA2SPGM 
00909      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2SPGM 
00910 *    MOVE SPACES  TO  GCA-EFFECTIVE-DATE.                         GA2SPGM 
00911      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2SPGM 
00912                                                                   GA2SPGM 
00913      SET GCA-RECORD-POINTER                                       GA2SPGM 
00914          TO ADDRESS OF  IO-PARM-INTERNAL-TAB-RECORD.              GA2SPGM 
00915                                                                   GA2SPGM 
00916      MOVE  GC-GCTABULR-IPGS-VARY-MAX-OCUR                         GA2SPGM 
00917            TO  GXS-ENTRY-COUNT.                                   GA2SPGM 
00918      MOVE 'RD '  TO  GCIO-FILE-ACCESS-CODE.                       GA2SPGM 
00919                                                                   GA2SPGM 
00920      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2SPGM 
00921         COMMAREA(IO-PARM-INTERNAL-TAB-RECORD)                     GA2SPGM 
00922         LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN) END-EXEC.          GA2SPGM 
00923                                                                   GA2SPGM 
00924      IF  NOT GCIO-GOOD-RETURN                                     GA2SPGM 
00925         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR RECORD.GA2SPGM 
00926 -    ' CONTACT SYSTEMS AREA ***'  TO  ERRMSGO                     GA2SPGM 
00927         MOVE '2IF3'  TO  WS-ABEND-CODE                            GA2SPGM 
00928         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00929                                                                   GA2SPGM 
00930 *    SET  COMMAREA-PNTR   TO                                      GA2SPGM 
00931 *         ADDRESS  OF GCA-COMMAREA.                               GA2SPGM 
00932                                                                   GA2SPGM 
00933 *    EXEC CICS XCTL  PROGRAM('GA1SPGM') COMMAREA(COMMAREA-PNTR)   GA2SPGM 
00934 *       LENGTH(4)  END-EXEC.                                      GA2SPGM 
00935      EXEC CICS XCTL  PROGRAM('GA1SPGM')                           GA2SPGM 
00936                      COMMAREA(DFHCOMMAREA)                        GA2SPGM 
00937                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2SPGM 
00938      END-EXEC.                                                    GA2SPGM 
00939                                                                   GA2SPGM 
00940  3099-EXIT.   EXIT.                                               GA2SPGM 
00941 /                                                                 GA2SPGM 
00942 ***************************************************************** GA2SPGM 
00943 **          D I S P L A Y   F I R S T   S C R E E N               GA2SPGM 
00944 **                                                                GA2SPGM 
00945 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2SPGM 
00946 ** OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2SPGM 
00947 ** THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2SPGM 
00948 ** (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2SPGM 
00949 ** THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2SPGM 
00950 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2SPGM 
00951 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2SPGM 
00952 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2SPGM 
00953 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2SPGM 
00954 ******************************************************************GA2SPGM 
00955  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2SPGM 
00956      MOVE '4000'  TO  WS-PARA-ID.                                 GA2SPGM 
00957                                                                   GA2SPGM 
00958      MOVE LOW-VALUES  TO  GA2SI01O.                               GA2SPGM 
00959                                                                   GA2SPGM 
00960      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA2SPGM 
00961         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2SPGM 
00962            TO  ERRMSGO                                            GA2SPGM 
00963         MOVE '2IC1'  TO  WS-ABEND-CODE                            GA2SPGM 
00964         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
00965                                                                   GA2SPGM 
00966 *    SET  ADDRESS OF  GCA-COMMAREA  TO                            GA2SPGM 
00967 *         INCOMING-COMMAREA-PNTR.                                 GA2SPGM 
00968                                                                   GA2SPGM 
00969      MOVE GCA-ALL-LEVEL-TAB-ID  TO  ALTABIDO.                     GA2SPGM 
00970      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  ALTBSLTO.                   GA2SPGM 
00971      MOVE GCA-INTERNAL-TAB-ID  TO  INTABIDO.                      GA2SPGM 
00972      MOVE GCA-INTERNAL-TAB-SLOT  TO  INTBSLTO.                    GA2SPGM 
00973      MOVE GCA-ADD-DEL-IND  TO  ADDELINO.                          GA2SPGM 
00974      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO  ALTBFNCO.              GA2SPGM 
00975      MOVE GCA-OCCURS-ENTRY-COUNTER  TO  OENTCTRO.                 GA2SPGM 
00976      MOVE GCA-FROM-MENU-ID  TO  FRMNUIDO.                         GA2SPGM 
00977                                                                   GA2SPGM 
00978      MOVE GCA-I-E-INDC TO INCEXCO,                                GA2SPGM 
00979                        INEXDRKO.                                  GA2SPGM 
00980                                                                   GA2SPGM 
00981      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2SPGM 
00982         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  TTLELNEO              GA2SPGM 
00983 *AB*****MOVE 'GROUP SPECIFIC ID = '  TO  GRP-SPEC-ID-HEADING      GA2SPGM 
00984         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2SPGM 
00985         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2SPGM 
00986         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2SPGM 
00987         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2SPGM 
00988         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2SPGM 
00989         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2SPGM 
00990         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2SPGM 
00991         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2SPGM 
00992         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2SPGM 
00993         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2SPGM 
00994         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2SPGM 
00995         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2SPGM 
00996                                                                   GA2SPGM 
00997      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2SPGM 
00998         MOVE CONTRACT-TITLE-LINE  TO  TTLELNEO                    GA2SPGM 
00999 *AB*****MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA2SPGM 
01000         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2SPGM 
01001         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2SPGM 
01002         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2SPGM 
01003         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2SPGM 
01004         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2SPGM 
01005         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2SPGM 
01006         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2SPGM 
01007         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2SPGM 
01008         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2SPGM 
01009         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2SPGM 
01010         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2SPGM 
01011         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2SPGM 
01012         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2SPGM 
01013         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2SPGM 
01014         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2SPGM 
01015         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2SPGM 
01016                                                                   GA2SPGM 
01017      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2SPGM 
01018         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  TTLELNEO           GA2SPGM 
01019         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2SPGM 
01020         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2SPGM 
01021         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2SPGM 
01022         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2SPGM 
01023         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2SPGM 
01024         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2SPGM 
01025         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2SPGM 
01026         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2SPGM 
01027         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2SPGM 
01028         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2SPGM 
01029         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2SPGM 
01030         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2SPGM 
01031         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2SPGM 
01032         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2SPGM 
01033         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2SPGM 
01034         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2SPGM 
01035         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2SPGM 
01036         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2SPGM 
01037                                                                   GA2SPGM 
01038      EXEC CICS SEND   MAP('GA2SI01') MAPSET('GA2SSET') ERASE      GA2SPGM 
01039         FROM(GA2SI01O) END-EXEC.                                  GA2SPGM 
01040                                                                   GA2SPGM 
01041  4099-EXIT.   EXIT.                                               GA2SPGM 
01042 /                                                                 GA2SPGM 
01043 ***************************************************************** GA2SPGM 
01044 **        X C T L   T O   P R E V I O U S   M E N U               GA2SPGM 
01045 **                                                                GA2SPGM 
01046 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2SPGM 
01047 ** ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2SPGM 
01048 ** RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2SPGM 
01049 ** THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2SPGM 
01050 ** IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2SPGM 
01051 ******************************************************************GA2SPGM 
01052  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2SPGM 
01053      MOVE '5000'  TO  WS-PARA-ID.                                 GA2SPGM 
01054                                                                   GA2SPGM 
01055                                                                   GA2SPGM 
01056 *******   IF ALL LEVEL FUNCTION CODE ON SCREEN = 'GTM1',          GA2SPGM 
01057 * STS *   WE ARE IN SINGLE TABULAR MAINTENANCE SUPPORT AND        GA2SPGM 
01058 *******   MUST RETURN TO THE SINGLE TABULAR MAINTENANCE MENU.     GA2SPGM 
01059                                                                   GA2SPGM 
01060      IF  ALTBFNCI  =  'GTM1'                                      GA2SPGM 
01061          EXEC CICS XCTL                                           GA2SPGM 
01062                    PROGRAM('GTM1PGM')                             GA2SPGM 
01063                    END-EXEC.                                      GA2SPGM 
01064                                                                   GA2SPGM 
01065                                                                   GA2SPGM 
01066      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2SPGM 
01067               GC-GCIOPARM-LEN             +                       GA2SPGM 
01068               GC-WORKFILE-KEY-LEN         +                       GA2SPGM 
01069               GC-GCTABULR-ABM-FIXED-LEN   +                       GA2SPGM 
01070              (GC-GCTABULR-ABM-VARY-LEN    *                       GA2SPGM 
01071               GC-GCTABULR-ABM-VARY-MAX-OCUR).                     GA2SPGM 
01072                                                                   GA2SPGM 
01073      EXEC CICS                                                    GA2SPGM 
01074         GETMAIN  SET(ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)         GA2SPGM 
01075         INITIMG(WS-HEX-00)                                        GA2SPGM 
01076         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                        GA2SPGM 
01077      END-EXEC.                                                    GA2SPGM 
01078                                                                   GA2SPGM 
01079 *    EXEC CICS                                                    GA2SPGM 
01080 *       GETMAIN  SET(ADDRESS OF GCA-COMMAREA)                     GA2SPGM 
01081 *       INITIMG(WS-HEX-00)                                        GA2SPGM 
01082 *       LENGTH(WS-COMMUNICATION-KEY-LEN)                          GA2SPGM 
01083 *    END-EXEC.                                                    GA2SPGM 
01084                                                                   GA2SPGM 
01085      IF  FRMNUIDI  =  'GS3A'                                      GA2SPGM 
01086         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2SPGM 
01087         MOVE  'G'  TO GCIO-WRK-STATUS-CODE                        GA2SPGM 
01088         MOVE  'G3' TO GCIO-WRK-RECORD-TYPE                        GA2SPGM 
01089         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2SPGM 
01090         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2SPGM 
01091         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2SPGM 
01092         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2SPGM 
01093         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS                     GA2SPGM 
01094                          GCIO-WRK-PROVIDER-CONTROL                GA2SPGM 
01095         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2SPGM 
01096         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2SPGM 
01097         MOVE SPACES TO GCA-BEN-PROV-ID                            GA2SPGM 
01098         MOVE ALTABIDI TO  GCIO-WRK-PROVISION-ID                   GA2SPGM 
01099                           GCA-ALL-LEVEL-TAB-ID                    GA2SPGM 
01100         MOVE ALTBSLTI TO  GCIO-WRK-PROVISION-SLOT-NO              GA2SPGM 
01101                           GCA-ALL-LEVEL-TAB-SLOT                  GA2SPGM 
01102         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA2SPGM 
01103                        GCA-INTERNAL-TAB-ID                        GA2SPGM 
01104                        GCA-INTERNAL-TAB-SLOT                      GA2SPGM 
01105         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA2SPGM 
01106                                                                   GA2SPGM 
01107      IF  FRMNUIDI  =  'GC4A'                                      GA2SPGM 
01108         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2SPGM 
01109         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2SPGM 
01110         MOVE  'C3' TO GCIO-WRK-RECORD-TYPE                        GA2SPGM 
01111         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2SPGM 
01112         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2SPGM 
01113         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2SPGM 
01114         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2SPGM 
01115         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2SPGM 
01116         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2SPGM 
01117         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2SPGM 
01118         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2SPGM 
01119         MOVE SPACES TO GCA-BEN-PROV-ID                            GA2SPGM 
01120         MOVE ALTABIDI TO GCIO-WRK-PROVISION-ID                    GA2SPGM 
01121                          GCA-ALL-LEVEL-TAB-ID                     GA2SPGM 
01122         MOVE ALTBSLTI TO GCIO-WRK-PROVISION-SLOT-NO               GA2SPGM 
01123                          GCA-ALL-LEVEL-TAB-SLOT                   GA2SPGM 
01124         MOVE SPACES TO GCIO-WRK-TAB-PROVISION-ID                  GA2SPGM 
01125                        GCA-INTERNAL-TAB-ID                        GA2SPGM 
01126                        GCA-INTERNAL-TAB-SLOT                      GA2SPGM 
01127         MOVE ZEROES TO GCIO-WRK-TAB-PROV-SLOT-NO.                 GA2SPGM 
01128                                                                   GA2SPGM 
01129      IF  FRMNUIDI  =  'GC8A'                                      GA2SPGM 
01130         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2SPGM 
01131         MOVE  'C'  TO GCIO-WRK-STATUS-CODE                        GA2SPGM 
01132         MOVE  'C5' TO GCIO-WRK-RECORD-TYPE                        GA2SPGM 
01133         MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE                  GA2SPGM 
01134         MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM                  GA2SPGM 
01135         MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM              GA2SPGM 
01136         MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE                    GA2SPGM 
01137         MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS                    GA2SPGM 
01138         MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL            GA2SPGM 
01139         MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL      GA2SPGM 
01140         MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN                  GA2SPGM 
01141         MOVE GCA-BEN-PROV-ID TO GCIO-WRK-PROVISION-ID             GA2SPGM 
01142         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2SPGM 
01143         MOVE ALTABIDI TO GCIO-WRK-TAB-PROVISION-ID                GA2SPGM 
01144                          GCA-ALL-LEVEL-TAB-ID                     GA2SPGM 
01145         MOVE ALTBSLTI TO GCIO-WRK-TAB-PROV-SLOT-NO                GA2SPGM 
01146                          GCA-ALL-LEVEL-TAB-SLOT                   GA2SPGM 
01147         MOVE SPACES TO GCA-INTERNAL-TAB-ID                        GA2SPGM 
01148                        GCA-INTERNAL-TAB-SLOT.                     GA2SPGM 
01149                                                                   GA2SPGM 
01150      MOVE GC-GCPSWORK-DDNAME  TO  GCIO2-FILE-DDNAME.              GA2SPGM 
01151 *    MOVE SPACES  TO  GCA-I-E-INDC.                               GA2SPGM 
01152 *    MOVE ADDELINI  TO  GCA-ADD-DEL-IND.                          GA2SPGM 
01153 *    MOVE ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.              GA2SPGM 
01154 *    MOVE OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                 GA2SPGM 
01155 *    MOVE FRMNUIDI  TO  GCA-FROM-MENU-ID.                         GA2SPGM 
01156      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2SPGM 
01157      SET GCA-RECORD-POINTER                                       GA2SPGM 
01158          TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                  GA2SPGM 
01159                                                                   GA2SPGM 
01160      MOVE GC-GCTABULR-ABM-VARY-MAX-OCUR  TO                       GA2SPGM 
01161           GAA-ENTRY-COUNT.                                        GA2SPGM 
01162      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2SPGM 
01163                                                                   GA2SPGM 
01164      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2SPGM 
01165         COMMAREA(IO-PARM-ALL-LEVEL-RECORD)                        GA2SPGM 
01166         LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN) END-EXEC.              GA2SPGM 
01167                                                                   GA2SPGM 
01168      IF  NOT GCIO2-GOOD-RETURN                                    GA2SPGM 
01169         MOVE '*** ERROR READING ALL LEVEL INTERNAL TABULAR.  CONTAGA2SPGM 
01170 -    'CT SYSTEMS AREA ***'  TO  ERRMSGO                           GA2SPGM 
01171         MOVE '2IF4'  TO  WS-ABEND-CODE                            GA2SPGM 
01172         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2SPGM 
01173                                                                   GA2SPGM 
01174 *    SET  COMMAREA-PNTR                                           GA2SPGM 
01175 *         TO   ADDRESS  OF  GCA-COMMAREA.                         GA2SPGM 
01176                                                                   GA2SPGM 
01177      IF  ALTBFNCI  =  'GA1B'                                      GA2SPGM 
01178 *       EXEC CICS XCTL  PROGRAM('GA1BPGM') COMMAREA(COMMAREA-PNTR)GA2SPGM 
01179 *          LENGTH(4) END-EXEC.                                    GA2SPGM 
01180         EXEC CICS XCTL  PROGRAM('GA1BPGM')                        GA2SPGM 
01181                         COMMAREA(DFHCOMMAREA)                     GA2SPGM 
01182                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2SPGM 
01183         END-EXEC.                                                 GA2SPGM 
01184                                                                   GA2SPGM 
01185      IF  ALTBFNCI  =  'GA1C'                                      GA2SPGM 
01186 *       EXEC CICS XCTL  PROGRAM('GA1CPGM') COMMAREA(COMMAREA-PNTR)GA2SPGM 
01187 *          LENGTH(4) END-EXEC.                                    GA2SPGM 
01188         EXEC CICS XCTL  PROGRAM('GA1CPGM')                        GA2SPGM 
01189                         COMMAREA(DFHCOMMAREA)                     GA2SPGM 
01190                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2SPGM 
01191         END-EXEC.                                                 GA2SPGM 
01192                                                                   GA2SPGM 
01193      IF  ALTBFNCI  =  'GA1D'                                      GA2SPGM 
01194 *       EXEC CICS XCTL  PROGRAM('GA1DPGM') COMMAREA(COMMAREA-PNTR)GA2SPGM 
01195 *          LENGTH(4) END-EXEC.                                    GA2SPGM 
01196         EXEC CICS XCTL  PROGRAM('GA1DPGM')                        GA2SPGM 
01197                         COMMAREA(DFHCOMMAREA)                     GA2SPGM 
01198                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2SPGM 
01199         END-EXEC.                                                 GA2SPGM 
01200                                                                   GA2SPGM 
01201      IF  ALTBFNCI  =  'GA1E'                                      GA2SPGM 
01202 *       EXEC CICS XCTL  PROGRAM('GA1EPGM') COMMAREA(COMMAREA-PNTR)GA2SPGM 
01203 *          LENGTH(4) END-EXEC.                                    GA2SPGM 
01204         EXEC CICS XCTL  PROGRAM('GA1EPGM')                        GA2SPGM 
01205                         COMMAREA(DFHCOMMAREA)                     GA2SPGM 
01206                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2SPGM 
01207         END-EXEC.                                                 GA2SPGM 
01208                                                                   GA2SPGM 
01209      IF  ALTBFNCI  =  'GA1P'                                      GA2SPGM 
01210         EXEC CICS XCTL  PROGRAM('GA1PPGM')                        GA2SPGM 
01211                         COMMAREA(DFHCOMMAREA)                     GA2SPGM 
01212                         LENGTH (LENGTH OF DFHCOMMAREA)            GA2SPGM 
01213         END-EXEC.                                                 GA2SPGM 
01214                                                                   GA2SPGM 
01215  5099-EXIT.                                                       GA2SPGM 
01216      EXIT.                                                        GA2SPGM 
01217 /                                                                 GA2SPGM 
01218 ***************************************************************** GA2SPGM 
01219 **           X C T L   T O   M A I N   M E N U                    GA2SPGM 
01220 **                                                                GA2SPGM 
01221 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2SPGM 
01222 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2SPGM 
01223 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2SPGM 
01224 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2SPGM 
01225 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2SPGM 
01226 ** AND PROGRESS DOWN.                                             GA2SPGM 
01227 ******************************************************************GA2SPGM 
01228  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2SPGM 
01229      MOVE '6000'  TO  WS-PARA-ID.                                 GA2SPGM 
01230      MOVE '2IP1'  TO  WS-ABEND-CODE.                              GA2SPGM 
01231                                                                   GA2SPGM 
01232      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2SPGM 
01233                                                                   GA2SPGM 
01234  6099-EXIT.     EXIT.                                             GA2SPGM 
01235 /*****************************************************************GA2SPGM 
01236 * 9800    G R E G O R I A N   T O   J U L I A N                  *GA2SPGM 
01237 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *GA2SPGM 
01238 ******************************************************************GA2SPGM 
01239  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          GA2SPGM 
01240  9800-010.                                                        GA2SPGM 
01241                                                                   GA2SPGM 
01242      MOVE 'CNV' TO  HGADATE-FUNC.                                 GA2SPGM 
01243      MOVE 'M'   TO  HGADATE-FORM1.                                GA2SPGM 
01244      MOVE 'J'   TO  HGADATE-FORM2.                                GA2SPGM 
01245      MOVE ZEROS TO  HGADATE-RETURN                                GA2SPGM 
01246                     HGADATE-AMOUNT.                               GA2SPGM 
01247      EXEC CICS LINK PROGRAM ('HGADATES')                          GA2SPGM 
01248                     COMMAREA(HGADATES-COMMAREA)                   GA2SPGM 
01249                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GA2SPGM 
01250                     END-EXEC.                                     GA2SPGM 
01251                                                                   GA2SPGM 
01252  9800-900-900-EXIT.                                               GA2SPGM 
01253      EXIT.                                                        GA2SPGM 
01254 /*****************************************************************GA2SPGM 
01255  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2SPGM 
01256                                                                   GA2SPGM 
01257      SET MAP-IDX1  TO  7.                                         GA2SPGM 
01258      SET MAP-IDX2  TO  1.                                         GA2SPGM 
01259      MOVE -1  TO                                                  GA2SPGM 
01260         MAP-PROVIDER-SPC-ARGUMENT-LEN (MAP-IDX1, MAP-IDX2).       GA2SPGM 
01261      EXEC CICS SEND   MAP('GA2SI01') MAPSET('GA2SSET') ERASE      GA2SPGM 
01262         FROM(GA2SI01O) CURSOR WAIT END-EXEC.                      GA2SPGM 
01263                                                                   GA2SPGM 
01264      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2SPGM 
01265                                                                   GA2SPGM 
01266  9999-EXIT.     EXIT.                                             GA2SPGM 
