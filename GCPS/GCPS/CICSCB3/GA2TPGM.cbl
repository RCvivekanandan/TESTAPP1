00001  ID DIVISION.                                                     01/12/06
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA2TPGM 
00003  PROGRAM-ID.     GA2TPGM.                                            LV005
00004  AUTHOR.         DICK MOHILL.                                     GA2TPGM 
00005  DATE-WRITTEN.   JUNE 2001.                                       GA2TPGM 
00006  DATE-COMPILED.                                                   GA2TPGM 
00007                                                                   GA2TPGM 
00008 ******************************************************************GA2TPGM 
00009 *   GA2TPGM - INTERNAL TABULAR MAINTENANCE PROGRAM                GA2TPGM 
00010 *             INTERNAL DIAGNOSIS RELATIONSHIP RANGES              GA2TPGM 
00011 *                                                                 GA2TPGM 
00012 *     THIS PROGRAM WILL ADD ENTRIES TO THE ALL LEVEL INTERNAL     GA2TPGM 
00013 *   TABULAR INTERNAL DIAGNOSIS RELATIONSHIP RANGES.               GA2TPGM 
00014 *                                                                 GA2TPGM 
00015 *                                                                 GA2TPGM 
00016 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2TPGM 
00017 *   TO ADD ENTRIES TO THIS PARTICULAR TABULAR RECORD.  THE PROGRAMGA2TPGM 
00018 *   THEN READS THE ENTRIES, AND VALIDATES THE FORMAT OF EACH FIELDGA2TPGM 
00019 *   IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN ERROR). GA2TPGM 
00020 *   IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES IN       GA2TPGM 
00021 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER    GA2TPGM 
00022 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2TPGM 
00023 *   ENTRIES FOR THIS TABULAR RECORD.                              GA2TPGM 
00024 *                                                                 GA2TPGM 
00025 *   TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID: #IRDX) GA2TPGM 
00026 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2TPGM 
00027 *   XCTL TO TRANS GA1T OR PROGRAM GA1TPGM.                        GA2TPGM 
00028 *                                                                 GA2TPGM 
00029 *   FUNC CODE: GA2T                                               GA2TPGM 
00030 *   MAPSET:    GA2TSETC                                           GA2TPGM 
00031 *   FILES:     GCPSWORK                                           GA2TPGM 
00032 *                                                                 GA2TPGM 
00033 *----------------------------------------------------------------*GA2TPGM 
00034 *                   PROGRAM MODIFICATION STATUS                  *GA2TPGM 
00035 *                                                                *GA2TPGM 
00036 * *-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION---------------*GA2TPGM 
00037 *----------------------------------------------------------------*GA2TPGM 
00038 *                                                                *GA2TPGM 
00039 *  P0015    06/13/01    DM   CREATES BASIC SKELETON RECORD FOR   *GA2TPGM 
00040 *                            THE INTERNAL TABULAR RECORD #IRDX.  *GA2TPGM 
00041 *                                                                *GA2TPGM 
00042 *  D-358    10/02/2001  GSP  ADDED CODING TO SET GTE-INDEX TO    *GA2TPGM 
00043 *                            DESIRED SEQUENCE NUMBER FOR         *GA2TPGM 
00044 *                            PROCESSING IN GC4HPGM.              *GA2TPGM 
00045 *                                                                *GA2TPGM 
00046 *  NO LOG # 10/29/2002  KIKI CORRECTED SEARCH FOR APPROPRIATE    *GA2TPGM 
00047 *                            #IRPV SEQUENCE, CARRIED OVER TO     *GA2TPGM 
00048 *                            'GC4H' SCREEN                       *GA2TPGM 
00049 *                                                                *GA2TPGM 
00050 *  D-356A   04/29/03   GTF   EXPANDED DIAGNOSIS CODE FROM 6 TO   *GA2TPGM 
00051 *                            10 BYTES. CHANGED # OF OCCURS FROM  *GA2TPGM 
00052 *                            647 TO 388 FOR #IRDX TABULAR.       *GA2TPGM 
00053 *                                                                *GA2TPGM 
00054 *  NO LOG # 10/01/2003  KIKI ADDED INFORMATIONAL MESSAGES        *GA2TPGM 
00055 *                                                                *GA2TPGM 
00056 *  NO LOG # 11/05/2003  KIKI FIXED ENTRANCE TO 'GC4H' WHEN       *GA2TPGM 
00057 *                            GCA-FROM-MENU-ID = 'GTM1' AND       *GA2TPGM 
00058 *                            PF3 IS PRESSED                      *GA2TPGM 
00059 *                                                                *GA2TPGM 
00059 * ICD-10  07/06/11    BA   CHANGE LOGIC FOR ICD-10 REQUIREMENTS. *GA2TPGM 
00060 ******************************************************************GA2TPGM 
00061                                                                   GA2TPGM 
00062  ENVIRONMENT DIVISION.                                            GA2TPGM 
00063                                                                   GA2TPGM 
00064  DATA DIVISION.                                                   GA2TPGM 
00065  WORKING-STORAGE SECTION.                                         GA2TPGM 
00066                                                                   GA2TPGM 
00067  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2TPGM 
00068      '** GA2TPGM WS BEGINS **    ** PARAGRAPH NUMBER FOLLOWS **'. GA2TPGM 
00069  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2TPGM 
00070  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2TPGM 
00071  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2TPGM 
00072                                                                   GA2TPGM 
00073  01  COMMAREA-POINTER-AREA.                                       GA2TPGM 
00074      05  COMMAREA-PNTR-COMP          PIC S9(08)  COMP.            GA2TPGM 
00075      05  COMMAREA-PNTR  REDEFINES                                 GA2TPGM 
00076                   COMMAREA-PNTR-COMP USAGE IS POINTER.            GA2TPGM 
00077                                                                   GA2TPGM 
00078  01  INTERNAL-POINTER-AREA.                                       GA2TPGM 
00079      05  INTERNAL-TAB-PNTR-COMP          PIC S9(08)  COMP.        GA2TPGM 
00080      05  INTERNAL-TAB-PNTR  REDEFINES                             GA2TPGM 
00081                   INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.        GA2TPGM 
00082                                                                   GA2TPGM 
00083 ******************************************************************GA2TPGM 
00084 *    MAP COBOL SCREEN DSECTS                                      GA2TPGM 
00085 ******************************************************************GA2TPGM 
00086  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2TPGM 
00087                                           '*** I/O MAP AREA ***'. GA2TPGM 
00088  COPY GA2TSETC.                                                   GA2TPGM 
00089                                                                   GA2TPGM 
00090 ******************************************************************GA2TPGM 
00091 *     THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2TPGM 
00092 *   ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2TPGM 
00093 *   HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2TPGM 
00094 *   FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2TPGM 
00095 *   REDEFINES.                                                    GA2TPGM 
00096 ****************************************************************  GA2TPGM 
00097                                                                   GA2TPGM 
00098  01  FILLER     REDEFINES   GA2TI01I.                             GA2TPGM 
00099      05  FILLER                              PIC X(89).           GA2TPGM 
00100      05  CONTRACT-ID-LINE.                                        GA2TPGM 
00101          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA2TPGM 
00102          10  CONTRACT-PLAN-CODE              PIC X(3).            GA2TPGM 
00103          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA2TPGM 
00104          10  CONTRACT-GROUP-NO               PIC X(9).            GA2TPGM 
00105          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA2TPGM 
00106          10  CONTRACT-SECTION-NO             PIC X(5).            GA2TPGM 
00107          10  CONTRACT-PKG-HEADING            PIC X(6).            GA2TPGM 
00108          10  CONTRACT-PKG-CODE               PIC X(3).            GA2TPGM 
00109          10  CONTRACT-LOB-HEADING            PIC X(6).            GA2TPGM 
00110          10  CONTRACT-LOB                    PIC X.               GA2TPGM 
00111          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA2TPGM 
00112          10  CONTRACT-PROV-CTL               PIC XX.              GA2TPGM 
00113          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA2TPGM 
00114          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA2TPGM 
00115          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA2TPGM 
00116          10  CONTRACT-EFF-DATE               PIC X(6).            GA2TPGM 
00117          10  FILLER                          PIC X(1).            GA2TPGM 
00118      05  FILLER                              PIC X(78).           GA2TPGM 
00119      05  MAP-DIAGNOSIS-RANGE-ROW     OCCURS 12 TIMES INDEXED      GA2TPGM 
00120                                       BY MAP-IDX1.                GA2TPGM 
00121        10  MAP-DIAGNOSIS-RANGE-COL   OCCURS 3 TIMES INDEXED       GA2TPGM 
00122                                       BY MAP-IDX2.                GA2TPGM 
00123          15  MAP-DIAGNOSIS-FROM-LEN     PIC S9(4) COMP SYNC.      GA2TPGM 
00124          15  MAP-DIAGNOSIS-FROM-ATTR    PIC X.                    GA2TPGM 
00125          15  MAP-DIAGNOSIS-FROM         PIC X(10).                GA2TPGM 
00126          15  MAP-DIAGNOSIS-TO-LEN       PIC S9(4) COMP SYNC.      GA2TPGM 
00127          15  MAP-DIAGNOSIS-TO-ATTR      PIC X.                    GA2TPGM 
00128          15  MAP-DIAGNOSIS-TO           PIC X(10).                GA2TPGM 
00129          15  FILLER                     PIC X.                    GA2TPGM 
00130                                                                   GA2TPGM 
00131                                                                   GA2TPGM 
00132  01  WS-INTERNAL-ID-SLOT.                                         GA2TPGM 
00133      05  WS-INTERNAL-ID                 PIC X(6).                 GA2TPGM 
00134      05  WS-INTERNAL-SLOT-NO            PIC S9(7)  COMP-3.        GA2TPGM 
00135                                                                   GA2TPGM 
00136 ****************************************************************  GA2TPGM 
00137 *    FIELDS DESCRIBING NUMBER OF OCCURS FOR MAP.                  GA2TPGM 
00138 ****************************************************************  GA2TPGM 
00139  01  FILLER.                                                      GA2TPGM 
00140      05  WS-MAP-ROW          PIC S999 COMP    VALUE +12.          GA2TPGM 
00141      05  WS-MAP-COL          PIC S999 COMP    VALUE +3.           GA2TPGM 
00142                                                                   GA2TPGM 
00143 ****************************************************************  GA2TPGM 
00144 *    ALTERNATIVE WORKFILE KEYS                                    GA2TPGM 
00145 ****************************************************************  GA2TPGM 
00146  01  FILLER                      PIC X(32)  VALUE                 GA2TPGM 
00147                              '*** ALTERNATIVE WORKFILE KEY ***'.  GA2TPGM 
00148  01  WS-ALT-WORKFILE-KEYS.                                        GA2TPGM 
00149  COPY GCWRKKEY.                                                   GA2TPGM 
00150                                                                   GA2TPGM 
00151 ****************************************************************  GA2TPGM 
00152 *    WORK FIELDS                                                  GA2TPGM 
00153 ****************************************************************  GA2TPGM 
00154  01  FILLER                           PIC X(17)                   GA2TPGM 
00155                                      VALUE '** WORK FIELDS **'.   GA2TPGM 
00156  01  WS-WORK-FIELDS.                                              GA2TPGM 
00157      05  WS-HEX-00                    PIC X VALUE LOW-VALUES.     GA2TPGM 
00158      05  WS-ADD-COUNT                 PIC 999 COMP VALUE ZEROES.  GA2TPGM 
00159      05  WS-ERROR-SW                  PIC X.                      GA2TPGM 
00160      05  WS-ADD                       PIC X(3) VALUE 'ADD'.       GA2TPGM 
00161      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2TPGM 
00162          VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.           GA2TPGM 
00163      05  WS-TITLE-LINE                PIC X(46)  VALUE            GA2TPGM 
00164          '       CONTRACT INTERNAL TABULAR MAINTENANCE  '.        GA2TPGM 
00165      05  WS-SAVED-FIELDS.                                         GA2TPGM 
00166          10  WS-SAVED-DIAGNOSIS           PIC X(20).              GA2TPGM 
00167      05  WS-SORT-DIAGNOSIS-ENTRY  OCCURS 37 TIMES INDEXED BY      GA2TPGM 
00168                        WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.   GA2TPGM 
00169             10  WS-DIAGNOSIS-SORT.                                GA2TPGM 
00170                 15  WS-FROM-DIAGNOSIS-SORT     PIC X(10).         GA2TPGM 
00171                 15  WS-TO-DIAGNOSIS-SORT       PIC X(10).         GA2TPGM 
00172                                                                   GA2TPGM 
00173 ****************************************************************  GA2TPGM 
00174 *   RECORD LENGTHS                                                GA2TPGM 
00175 ****************************************************************  GA2TPGM 
00176  01  FILLER                           PIC X(20)                   GA2TPGM 
00177                                    VALUE '** RECORD LENGTHS **'.  GA2TPGM 
00178  01  WS-RECORD-LENGTHS.                                           GA2TPGM 
00179     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP VALUE ZEROES.GA2TPGM 
00180     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP VALUE ZEROES.GA2TPGM 
00181     05 WS-COPY-LENGTH                 PIC S9(4) COMP VALUE ZEROES.GA2TPGM 
00182     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP VALUE +100.  GA2TPGM 
00183                                                                   GA2TPGM 
00184 ******************************************************************GA2TPGM 
00185 *    WT-01   MESSAGE TABLE                                       *GA2TPGM 
00186 ******************************************************************GA2TPGM 
00187  01  WT-01-TABLE.                                                 GA2TPGM 
00188      05  FILLER                  PIC X(16) VALUE                  GA2TPGM 
00189                                          '* WT-01-TABLE  *'.      GA2TPGM 
00190  01  FILLER.                                                      GA2TPGM 
00191      05  WT-01-MESSAGE-VALUES.                                    GA2TPGM 
00192          10  WT-01-ENTRY-001.                                     GA2TPGM 
00193              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00194              15  WT-01-MESSAGE-TEXT-001.                          GA2TPGM 
00195                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00196                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00197                  20  FILLER          PIC X(3)  VALUE  '001'.      GA2TPGM 
00198                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00199                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00200                           '** INVALID REQUEST. THE PF KEY USED HASGA2TPGM 
00201 -                   ' NO MEANING TO THIS PROGRAM **'.             GA2TPGM 
00202              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00203                                                                   GA2TPGM 
00204 *----------------------------------------------------------------*GA2TPGM 
00205          10  WT-01-ENTRY-002.                                     GA2TPGM 
00206              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00207              15  WT-01-MESSAGE-TEXT-002.                          GA2TPGM 
00208                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00209                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00210                  20  FILLER          PIC X(3)  VALUE  '002'.      GA2TPGM 
00211                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00212                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00213                  '**  \
00214              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00215                                                                   GA2TPGM 
00216 *----------------------------------------------------------------*GA2TPGM 
00217          10  WT-01-ENTRY-003.                                     GA2TPGM 
00218              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00219              15  WT-01-MESSAGE-TEXT-003.                          GA2TPGM 
00220                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00221                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00222                  20  FILLER          PIC X(3)  VALUE  '003'.      GA2TPGM 
00223                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00224                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00225                  '**  \
00226              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00227 *----------------------------------------------------------------*GA2TPGM 
00228          10  WT-01-ENTRY-004.                                     GA2TPGM 
00229              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00230              15  WT-01-MESSAGE-TEXT-004.                          GA2TPGM 
00231                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00232                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00233                  20  FILLER          PIC X(3)  VALUE  '004'.      GA2TPGM 
00234                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00235                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00236                  '** INCLUDE/EXCLUDE FIELD VALUE NOT VALID **'.   GA2TPGM 
00237              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00238 *----------------------------------------------------------------*GA2TPGM 
00239          10  WT-01-ENTRY-005.                                     GA2TPGM 
00240              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00241              15  WT-01-MESSAGE-TEXT-005.                          GA2TPGM 
00242                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00243                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00244                  20  FILLER          PIC X(3)  VALUE  '005'.      GA2TPGM 
00245                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00246                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00247                            '** ADD ENTRY NOT FOUND  **            GA2TPGM 
00248 -                    '         '.                                 GA2TPGM 
00249              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00250 *----------------------------------------------------------------*GA2TPGM 
00251          10  WT-01-ENTRY-006.                                     GA2TPGM 
00252              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00253              15  WT-01-MESSAGE-TEXT-006.                          GA2TPGM 
00254                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00255                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00256                  20  FILLER          PIC X(3)  VALUE  '006'.      GA2TPGM 
00257                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00258                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00259                               '** ERROR READING ALL LEVEL INTERNALGA2TPGM 
00260 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2TPGM 
00261              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00262                                                                   GA2TPGM 
00263 *----------------------------------------------------------------*GA2TPGM 
00264          10  WT-01-ENTRY-007.                                     GA2TPGM 
00265              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00266              15  WT-01-MESSAGE-TEXT-007.                          GA2TPGM 
00267                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00268                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00269                  20  FILLER          PIC X(3)  VALUE  '007'.      GA2TPGM 
00270                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00271                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00272                              '** PROGRAM ABOUT TO EXCEED MAX RECORGA2TPGM 
00273 -                    ' SIZE. CONTACT SYSTEMS AREA **'.            GA2TPGM 
00274              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00275 *----------------------------------------------------------------*GA2TPGM 
00276          10  WT-01-ENTRY-008.                                     GA2TPGM 
00277              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00278              15  WT-01-MESSAGE-TEXT-008.                          GA2TPGM 
00279                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00280                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00281                  20  FILLER          PIC X(3)  VALUE  '008'.      GA2TPGM 
00282                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00283                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00284                              '** PROGRAM SUBSCRIPT ABOUT TO EXCEEDGA2TPGM 
00285 -                    ' ITS MAX. CONTACT SYSTEMS AREA **'.         GA2TPGM 
00286              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00287                                                                   GA2TPGM 
00288 *----------------------------------------------------------------*GA2TPGM 
00289          10  WT-01-ENTRY-009.                                     GA2TPGM 
00290              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00291              15  WT-01-MESSAGE-TEXT-009.                          GA2TPGM 
00292                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00293                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00294                  20  FILLER          PIC X(3)  VALUE  '009'.      GA2TPGM 
00295                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00296                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00297                             '** ERROR REWRITING ALL LEVEL INTERNALGA2TPGM 
00298 -                    ' TABULAR RECORD **'.                        GA2TPGM 
00299              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00300                                                                   GA2TPGM 
00301 *----------------------------------------------------------------*GA2TPGM 
00302          10  WT-01-ENTRY-010.                                     GA2TPGM 
00303              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00304              15  WT-01-MESSAGE-TEXT-010.                          GA2TPGM 
00305                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00306                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00307                  20  FILLER          PIC X(3)  VALUE  '010'.      GA2TPGM 
00308                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00309                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00310                               '** ERROR READING ALL LEVEL INTERNALGA2TPGM 
00311 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2TPGM 
00312              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00313                                                                   GA2TPGM 
00314 *----------------------------------------------------------------*GA2TPGM 
00315          10  WT-01-ENTRY-011.                                     GA2TPGM 
00316              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00317              15  WT-01-MESSAGE-TEXT-011.                          GA2TPGM 
00318                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00319                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00320                  20  FILLER          PIC X(3)  VALUE  '011'.      GA2TPGM 
00321                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00322                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00323                      '** COMMAREA LENGTH IS INVALID **'.          GA2TPGM 
00324              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00325                                                                   GA2TPGM 
00326 *----------------------------------------------------------------*GA2TPGM 
00327          10  WT-01-ENTRY-012.                                     GA2TPGM 
00328              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00329              15  WT-01-MESSAGE-TEXT-012.                          GA2TPGM 
00330                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00331                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00332                  20  FILLER          PIC X(3)  VALUE  '012'.      GA2TPGM 
00333                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00334                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00335         '** REQUESTED PCG BIT IS NOT ALLOWED FOR THIS TABULAR **'.GA2TPGM 
00336              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00337                                                                   GA2TPGM 
00338 *----------------------------------------------------------------*GA2TPGM 
00339          10  WT-01-ENTRY-013.                                     GA2TPGM 
00340              15  FILLER              PIC X(2)  VALUE '¬>'.        GA2TPGM 
00341              15  WT-01-MESSAGE-TEXT-013.                          GA2TPGM 
00342                  20  FILLER          PIC X(4)  VALUE  'GA2T'.     GA2TPGM 
00343                  20  FILLER          PIC X(1)  VALUE  '-'.        GA2TPGM 
00344                  20  FILLER          PIC X(3)  VALUE  '013'.      GA2TPGM 
00345                  20  FILLER          PIC X(1)  VALUE  ' '.        GA2TPGM 
00346                  20  FILLER          PIC X(70) VALUE              GA2TPGM 
00347                      '**** FUTURE USE ****'.                      GA2TPGM 
00348              15  FILLER              PIC X(2)  VALUE '<¬'.        GA2TPGM 
00349                                                                   GA2TPGM 
00350 *----------------------------------------------------------------*GA2TPGM 
00351                                                                   GA2TPGM 
00352      05  WT-01-MESSAGE-TABLE         REDEFINES                    GA2TPGM 
00353          WT-01-MESSAGE-VALUES        OCCURS 013 TIMES             GA2TPGM 
00354                                      INDEXED BY WT-01-INDEX.      GA2TPGM 
00355          10  WT-01-ENTRY.                                         GA2TPGM 
00356              15  FILLER              PIC X(02).                   GA2TPGM 
00357              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GA2TPGM 
00358              15  FILLER              PIC X(02).                   GA2TPGM 
00359                                                                   GA2TPGM 
00360 ******************************************************************GA2TPGM 
00361 *    ATTRIBUTES                                                   GA2TPGM 
00362 ******************************************************************GA2TPGM 
00363  COPY DFHBMSCA.                                                   GA2TPGM 
00364      02  DFHBMABF                   PIC X VALUE 'Z'.              GA2TPGM 
00365                                                                   GA2TPGM 
00366 ******************************************************************GA2TPGM 
00367 *    GENERIC CONTRACT GLOBALLY DEFINED LENGHTH..ETC.--*           GA2TPGM 
00368 ******************************************************************GA2TPGM 
00369  01  FILLER.                                                      GA2TPGM 
00370      COPY GCCDRLEN.                                               GA2TPGM 
00371                                                                   GA2TPGM 
00372 ******************************************************************GA2TPGM 
00373 *    IO PARM AREA                                                 GA2TPGM 
00374 ******************************************************************GA2TPGM 
00375  01  GCPPDIO-PARM-AREA.                                           GA2TPGM 
00376  COPY GCPPDIOC.                                                   GA2TPGM 
00377                                                                   GA2TPGM 
00378 ******************************************************************GA2TPGM 
00379 *    ATTENTION IDENTIFIERS                                        GA2TPGM 
00380 ******************************************************************GA2TPGM 
00381  COPY DFHAID.                                                     GA2TPGM 
00382                                                                   GA2TPGM 
00383 ******************************************************************GA2TPGM 
00384 *    END OF WORKING STORAGE SECTION                               GA2TPGM 
00385 ******************************************************************GA2TPGM 
00386  01  WS-END                PIC X(16) VALUE                        GA2TPGM 
00387                                      '*** W/S ENDS ***'.          GA2TPGM 
00388                                                                   GA2TPGM 
00389  LINKAGE SECTION.                                                 GA2TPGM 
00390                                                                   GA2TPGM 
00391  01  DFHCOMMAREA.                                                 GA2TPGM 
00392  COPY G2ALCKEC.                                                   GA2TPGM 
00393      05  WS-COMMAREA-CDRS-REC                                     GA2TPGM 
00394          REDEFINES COMMAREA-ALL-LEV-TAB-RECORD.                   GA2TPGM 
00395          10  FILLER                      PIC X(97).               GA2TPGM 
00396          10  WS-PRIM-IND                 PIC X(01).               GA2TPGM 
00397          10  WS-PRIM-SEQ-NO-X            PIC X(02).               GA2TPGM 
00398          10  WS-PRIM-SEQ-NO                                       GA2TPGM 
00399              REDEFINES WS-PRIM-SEQ-NO-X  PIC 9(02).               GA2TPGM 
00400          10  WS-ADDL-IND                 PIC X(01).               GA2TPGM 
00401          10  WS-ADDL-SEQ-NO-X            PIC X(02).               GA2TPGM 
00402          10  WS-ADDL-SEQ-NO                                       GA2TPGM 
00403              REDEFINES WS-ADDL-SEQ-NO-X  PIC 9(02).               GA2TPGM 
00404          10  FILLER                      PIC X(47).               GA2TPGM 
00405                                                                   GA2TPGM 
00406  COPY GACDACWA.                                                   GA2TPGM 
00407 ***  05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA2TPGM 
00408      05  GAS1UPD-PASSED-AREA.                                     GA2TPGM 
00409          07  LVL2-B-SW          PIC X.                            GA2TPGM 
00410          07  LVL2-F-SW          PIC X.                            GA2TPGM 
00411          07  LVL2-G-SW          PIC X.                            GA2TPGM 
00412          07  INTR-TAB-PGM-ID    PIC X(8).                         GA2TPGM 
00413          07  OCCURS-COUNTER     PIC 9(2).                         GA2TPGM 
00414          07  FILLER             PIC X(7).                         GA2TPGM 
00415      05  DELADD-OPTION          PIC X(7).                         GA2TPGM 
00416                                                                   GA2TPGM 
00417 /                                                                 GA2TPGM 
00418 ******************************************************************GA2TPGM 
00419 *    I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD          GA2TPGM 
00420 ******************************************************************GA2TPGM 
00421  01  IO-PARM-INTERNAL-TAB-RECORD.                                 GA2TPGM 
00422  COPY GCIOPRM1.                                                   GA2TPGM 
00423                                                                   GA2TPGM 
00424  COPY GCWRKDCC.                                                   GA2TPGM 
00425                                                                   GA2TPGM 
00426  COPY GCTIRDXC.                                                   GA2TPGM 
00427                                                                   GA2TPGM 
00428 ****************************************************************  GA2TPGM 
00429 *    COPY OF THE TABULAR PORTION OF IRDX RECORD.                  GA2TPGM 
00430 *    THIS AREA IS USED IN SORTING PROCESS.                        GA2TPGM 
00431 ****************************************************************  GA2TPGM 
00432  01  LK-COPY-TABULAR-TABLE-AREA.                                  GA2TPGM 
00433      05  LK-COPY-TABULAR-TABLE           OCCURS 388 TIMES         GA2TPGM 
00434                                          INDEXED BY COPY-IDX.     GA2TPGM 
00435          10  LK-COPY-DIAGNOSIS-RANGE.                             GA2TPGM 
00436              15  LK-COPY-DIAGNOSIS-FROM    PIC X(10).             GA2TPGM 
00437              15  LK-COPY-DIAGNOSIS-TO      PIC X(10).             GA2TPGM 
00438                                                                   GA2TPGM 
00439 ****************************************************************  GA2TPGM 
00440 *    IO PARM, WITH WORKFILE KEY, AND CONTRACT RECORD              GA2TPGM 
00441 ****************************************************************  GA2TPGM 
00442  01  IO-PARM-ALL-LEVEL-RECORD.                                    GA2TPGM 
00443  COPY GCIOPRM2.                                                   GA2TPGM 
00444                                                                   GA2TPGM 
00445  COPY GCWRKDC2.                                                   GA2TPGM 
00446                                                                   GA2TPGM 
00447  COPY GCTCDRSC.                                                   GA2TPGM 
00448                                                                   GA2TPGM 
00449                                                                   GA2TPGM 
00450  PROCEDURE DIVISION.                                              GA2TPGM 
00451                                                                   GA2TPGM 
00452 ******************************************************************GA2TPGM 
00453 *                      M A I N L I N E                            GA2TPGM 
00454 *                                                                 GA2TPGM 
00455 *    THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2TPGM 
00456 *   TAKEN BY THE OPERATOR.                                        GA2TPGM 
00457 *   1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2TPGM 
00458 *      WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2TPGM 
00459 *      ADDITIONS FROM.                                            GA2TPGM 
00460 *   2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2TPGM 
00461 *      KEY PF12 OR PF24.                                          GA2TPGM 
00462 *   3. RECEIVE THE SCREEN.                                        GA2TPGM 
00463 *   4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2TPGM 
00464 *      MENU.                                                      GA2TPGM 
00465 *   5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2TPGM 
00466 *   6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2TPGM 
00467 *      (RETURN) TO THE DELETE PROGRAM (GA1NPGM).                  GA2TPGM 
00468 *   7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2TPGM 
00469 *      (RETURN) TO THE PREVIOUS MENU.                             GA2TPGM 
00470 *   8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2TPGM 
00471 *      NORMAL ADD PROCESSING, EXCEPT BYPASS EMPTY VALIDATION TABLEGA2TPGM 
00472 *      CONDITION FOR THE DIAGNOSIS CODE.                          GA2TPGM 
00473 *   9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2TPGM 
00474 *      KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2TPGM 
00475 *                                                                 GA2TPGM 
00476 ******************************************************************GA2TPGM 
00477  1000-MAIN-LINE SECTION.                                          GA2TPGM 
                                                                                
00478      MOVE '1000'  TO  WS-PARA-ID.                                 GA2TPGM 
                                                                                
00479      IF EIBAID = DFHCLEAR                                         GA2TPGM 
00480         EXEC CICS SEND                                            GA2TPGM 
00481              FROM (WS-ONE-LOW)                                    GA2TPGM 
00482              ERASE                                                GA2TPGM 
00483         END-EXEC                                                  GA2TPGM 
00484         EXEC CICS                                                 GA2TPGM 
00485              RETURN                                               GA2TPGM 
00486         END-EXEC.                                                 GA2TPGM 
00487                                                                   GA2TPGM 
00488      IF EIBTRNID  NOT =  'GA2T'                                   GA2TPGM 
00489         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2TPGM 
00490         GO TO 1099-RETURN.                                        GA2TPGM 
00491                                                                   GA2TPGM 
00492      EXEC CICS RECEIVE                                            GA2TPGM 
00493           MAP    ('GA2TI01')                                      GA2TPGM 
00494           MAPSET ('GA2TSET')                                      GA2TPGM 
00495           INTO   (GA2TI01I)                                       GA2TPGM 
00496      END-EXEC.                                                    GA2TPGM 
00497                                                                   GA2TPGM 
00498      IF A2TSCRNI  NOT =  '0A2T00'                                 GA2TPGM 
00499         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2TPGM 
00500                                                                   GA2TPGM 
00501      IF EIBAID  =  DFHENTER                                       GA2TPGM 
00502         PERFORM 2000-ADD-PROCESSING                               GA2TPGM 
00503         GO TO 1099-RETURN.                                        GA2TPGM 
00504                                                                   GA2TPGM 
00505      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2TPGM 
00506         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2TPGM 
00507                                                                   GA2TPGM 
00508      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2TPGM 
00509         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2TPGM 
00510                                                                   GA2TPGM 
00511      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2TPGM 
00512         PERFORM 2000-ADD-PROCESSING                               GA2TPGM 
00513         GO TO 1099-RETURN.                                        GA2TPGM 
00514                                                                   GA2TPGM 
00515      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2TPGM 
00516      MOVE -1  TO                                                  GA2TPGM 
00517         MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2).              GA2TPGM 
00518         SET WT-01-INDEX TO +01.                                   GA2TPGM 
00519         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2TPGM 
00520                                                                   GA2TPGM 
00521      EXEC CICS SEND                                               GA2TPGM 
00522           MAP    ('GA2TI01')                                      GA2TPGM 
00523           MAPSET ('GA2TSET') DATAONLY                             GA2TPGM 
00524           FROM   (GA2TI01O) CURSOR                                GA2TPGM 
00525      END-EXEC.                                                    GA2TPGM 
00526                                                                   GA2TPGM 
00527      GO TO 1099-RETURN.                                           GA2TPGM 
00528                                                                   GA2TPGM 
00529  1000-EXIT.                                                       GA2TPGM 
00530      EXIT.                                                        GA2TPGM 
00531                                                                   GA2TPGM 
00532  1099-RETURN.                                                     GA2TPGM 
                                                                                
00533      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2TPGM 
00534         (DELADD-OPTION = 'GAS1UPD') OR                            GA2TPGM 
00535         (DELADD-OPTION = 'GAS2UPD') OR                            GA2TPGM 
00536         (DELADD-OPTION = 'GAS3UPD') OR                            GA2TPGM 
00537         (DELADD-OPTION = 'GAS4UPD') OR                            GA2TPGM 
00538         (DELADD-OPTION = 'GAS5UPD')                               GA2TPGM 
00539          EXEC CICS                                                GA2TPGM 
00540               RETURN                                              GA2TPGM 
00541          END-EXEC                                                 GA2TPGM 
00542                                                                   GA2TPGM 
00543      ELSE                                                         GA2TPGM 
00544          EXEC CICS                                                GA2TPGM 
00545               RETURN                                              GA2TPGM 
00546               TRANSID  ('GA2T')                                   GA2TPGM 
00547               COMMAREA (DFHCOMMAREA)                              GA2TPGM 
00548               LENGTH   (EIBCALEN)                                 GA2TPGM 
00549          END-EXEC.                                                GA2TPGM 
00550                                                                   GA2TPGM 
00551      GOBACK.                                                      GA2TPGM 
                                                                                
00552  1099-EXIT.                                                       GA2TPGM 
00553      EXIT.                                                        GA2TPGM 
00554                                                                   GA2TPGM 
00555 ******************************************************************GA2TPGM 
00556 *                A D D   P R O C E S S I N G                      GA2TPGM 
00557 *                                                                 GA2TPGM 
00558 *    THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2TPGM 
00559 *   MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2TPGM.            GA2TPGM 
00560 *   1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2TPGM 
00561 *   2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2TPGM 
00562 *      SKIP TO THE NEXT LINE.                                     GA2TPGM 
00563 *   3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2TPGM 
00564 *      WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2TPGM 
00565 *      VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2TPGM 
00566 *      DATA.                                                      GA2TPGM 
00567 *   4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2TPGM 
00568 *      APPROPRIATE MESSAGE IS DISPLAYED.                          GA2TPGM 
00569 *   5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2TPGM 
00570 *      ORDER, FIELD BY FIELD.                                     GA2TPGM 
00571 *   6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2TPGM 
00572 *      MADE.                                                      GA2TPGM 
00573 *   7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2TPGM 
00574 *      COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2TPGM 
00575 *      BACK INTO THE RECORD.                                      GA2TPGM 
00576 *   8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2TPGM 
00577 *      SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2TPGM 
00578 *                                                                 GA2TPGM 
00579 ******************************************************************GA2TPGM 
00580  2000-ADD-PROCESSING SECTION.                                     GA2TPGM 
                                                                                
00581      MOVE '2000'  TO  WS-PARA-ID.                                 GA2TPGM 
00582      MOVE 'N'     TO  WS-ERROR-SW.                                GA2TPGM 
00583      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2TPGM 
00584      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2TPGM 
00585                                                                   GA2TPGM 
00586      MOVE '2005'  TO  WS-PARA-ID.                                 GA2TPGM 
                                                                                
00587  2005-RESET-ALL-ATTRIBUTES.                                       GA2TPGM 
                                                                                
00588      PERFORM WITH TEST BEFORE                                     GA2TPGM 
00589       VARYING MAP-IDX1 FROM 1 BY 1 UNTIL MAP-IDX2 > WS-MAP-COL    GA2TPGM 
00590          MOVE DFHBMUNF TO                                         GA2TPGM 
00591               MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)        GA2TPGM 
00592               MAP-DIAGNOSIS-TO-ATTR   (MAP-IDX1, MAP-IDX2)        GA2TPGM 
00593       IF MAP-IDX1 = WS-MAP-ROW                                    GA2TPGM 
00594          SET MAP-IDX2 UP BY 1                                     GA2TPGM 
00595          SET MAP-IDX1 TO 1                                        GA2TPGM 
00596          SET MAP-IDX1 DOWN BY 1                                   GA2TPGM 
00597       END-IF                                                      GA2TPGM 
00598      END-PERFORM.                                                 GA2TPGM 
00599                                                                   GA2TPGM 
00600      MOVE '2010'  TO  WS-PARA-ID.                                 GA2TPGM 
00601      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2TPGM 
                                                                                
00602  2010-VALIDATE-ADD-ENTRIES.                                       GA2TPGM 
                                                                                
00603      IF MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2) = ZERO        GA2TPGM 
00604         IF MAP-IDX1  <  WS-MAP-ROW                                GA2TPGM 
00605            SET MAP-IDX1  UP BY  1                                 GA2TPGM 
00606            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2TPGM 
00607         ELSE                                                      GA2TPGM 
00608            IF MAP-IDX2  <  WS-MAP-COL                             GA2TPGM 
00609               SET MAP-IDX1  TO  1                                 GA2TPGM 
00610               SET MAP-IDX2  UP BY  1                              GA2TPGM 
00611               GO TO 2010-VALIDATE-ADD-ENTRIES                     GA2TPGM 
00612            ELSE                                                   GA2TPGM 
00613               GO TO 2020-CHECK-FOR-ERRORS.                        GA2TPGM 
00614                                                                   GA2TPGM 
00615 *--                                                               GA2TPGM 
00616 *--  VALIDATE \
00617 *--            ====                                               GA2TPGM 
00618                                                                   GA2TPGM 
00619      IF MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2) = ZERO        GA2TPGM 
00620         MOVE DFHBMUBF                                             GA2TPGM 
00621           TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00622         MOVE '??????'                                             GA2TPGM 
00623           TO MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)              GA2TPGM 
00624         IF WS-ERROR-SW  NOT = 'Y'                                 GA2TPGM 
00625            MOVE 'Y'  TO  WS-ERROR-SW                              GA2TPGM 
00626            MOVE -1                                                GA2TPGM 
00627              TO MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2)       GA2TPGM 
00628            SET WT-01-INDEX TO +02                                 GA2TPGM 
00629            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                   GA2TPGM 
00630                                                                   GA2TPGM 
      *** ICD-10 START                                                          
           MOVE MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)                         
                                              TO WS-SAVED-DIAGNOSIS             
                                                 GCPPDIO-SVC-CD.        GA2TPGM 
                                                                                
           MOVE 'PRCDR03 '      TO  GCPPDIO-REQUEST-TYPE                GA2TPGM 
                                                                                
           IF GCPPDIO-SVC-CD (1:1)  IS NUMERIC                                  
              MOVE '9'          TO  GCPPDIO-SVC-CD-SYS-ID                       
           ELSE                                                                 
              IF  GCPPDIO-SVC-CD (1:1) = 'V'                                    
              AND GCPPDIO-SVC-CD (6:1) = SPACE                                  
                 MOVE '9'       TO GCPPDIO-SVC-CD-SYS-ID                        
              ELSE                                                              
                 MOVE '1'       TO GCPPDIO-SVC-CD-SYS-ID                GA2TPGM 
              END-IF                                                            
           END-IF                                                               
      *** ICD-10 END                                                            
                                                                                
00636      EXEC CICS LINK                                               GA2TPGM 
00637           PROGRAM  ('GCPPDIO')                                    GA2TPGM 
00638           COMMAREA (GCPPDIO-PARM-AREA)                            GA2TPGM 
00639           LENGTH   (GCPPDIO-CA-LEN)                               GA2TPGM 
00640      END-EXEC.                                                    GA2TPGM 
00641                                                                   GA2TPGM 
00642      IF GCPPDIO-SUCCESSFUL                                        GA2TPGM 
00643          NEXT SENTENCE                                            GA2TPGM 
00644      ELSE                                                         GA2TPGM 
00645      IF GCPPDIO-REC-NOT-FOUND                                     GA2TPGM 
00646          MOVE DFHBMUBF                                            GA2TPGM 
00647             TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)       GA2TPGM 
00648          IF WS-ERROR-SW NOT = 'Y'                                 GA2TPGM 
00649             MOVE 'Y' TO WS-ERROR-SW                               GA2TPGM 
00650             MOVE -1 TO MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2)GA2TPGM 
      *** ICD-10 START                                                          
                  IF WS-SAVED-DIAGNOSIS (1:3) = 'BIT'                           
00651                SET WT-01-INDEX TO +12                             GA2TPGM 
                  ELSE                                                          
      *** ICD-10 END                                                            
00651                SET WT-01-INDEX TO +02                             GA2TPGM 
                  END-IF                                                        
00652             PERFORM 9000-000-MOVE-MSG-TO-SCREEN                   GA2TPGM 
00653          ELSE                                                     GA2TPGM 
00654             MOVE DFHBMUBF                                         GA2TPGM 
00655               TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)     GA2TPGM 
00656                  MAP-DIAGNOSIS-TO-ATTR   (MAP-IDX1, MAP-IDX2)     GA2TPGM 
00657      ELSE                                                         GA2TPGM 
00658         MOVE 'DEW1'  TO  WS-ABEND-CODE                            GA2TPGM 
00659         MOVE GCPPDIO-RETURN-MESSAGE TO ERRMSGO                    GA2TPGM 
00660         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
00661                                                                   GA2TPGM 
00663 *--                                                               GA2TPGM 
00664 *--  VALIDATE \
00665 *--            ==                                                 GA2TPGM 
00666                                                                   GA2TPGM 
00667      IF MAP-DIAGNOSIS-TO-LEN (MAP-IDX1, MAP-IDX2) = ZERO          GA2TPGM 
00668         MOVE DFHBMUBF                                             GA2TPGM 
00669           TO MAP-DIAGNOSIS-TO-ATTR (MAP-IDX1, MAP-IDX2)           GA2TPGM 
00670         MOVE '??????'                                             GA2TPGM 
00671           TO MAP-DIAGNOSIS-TO (MAP-IDX1, MAP-IDX2)                GA2TPGM 
00672         IF WS-ERROR-SW  NOT = 'Y'                                 GA2TPGM 
00673            MOVE 'Y'  TO  WS-ERROR-SW                              GA2TPGM 
00674            MOVE -1                                                GA2TPGM 
00675              TO MAP-DIAGNOSIS-TO-LEN (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00676            SET WT-01-INDEX TO +03                                 GA2TPGM 
00677            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                   GA2TPGM 
00678                                                                   GA2TPGM 
      *** ICD-10 START                                                          
           MOVE MAP-DIAGNOSIS-TO (MAP-IDX1, MAP-IDX2)                           
                                            TO WS-SAVED-DIAGNOSIS               
                                               GCPPDIO-SVC-CD.          GA2TPGM 
                                                                                
           MOVE 'PRCDR03 '     TO  GCPPDIO-REQUEST-TYPE                 GA2TPGM 
                                                                                
           IF GCPPDIO-SVC-CD (1:1)  IS NUMERIC                                  
              MOVE '9'         TO  GCPPDIO-SVC-CD-SYS-ID                        
           ELSE                                                                 
              IF  GCPPDIO-SVC-CD (1:1) = 'V'                                    
              AND GCPPDIO-SVC-CD (6:1) = SPACE                                  
                 MOVE '9' TO  GCPPDIO-SVC-CD-SYS-ID                             
              ELSE                                                              
                 MOVE '1' TO  GCPPDIO-SVC-CD-SYS-ID                             
              END-IF                                                            
           END-IF                                                               
      *** ICD-10 END                                                            
                                                                                
00684      EXEC CICS LINK                                               GA2TPGM 
00685           PROGRAM  ('GCPPDIO')                                    GA2TPGM 
00686           COMMAREA (GCPPDIO-PARM-AREA)                            GA2TPGM 
00687           LENGTH   (GCPPDIO-CA-LEN)                               GA2TPGM 
00688      END-EXEC.                                                    GA2TPGM 
00689                                                                   GA2TPGM 
00690      IF GCPPDIO-SUCCESSFUL                                        GA2TPGM 
00691          NEXT SENTENCE                                            GA2TPGM 
00692      ELSE                                                         GA2TPGM 
00693      IF GCPPDIO-REC-NOT-FOUND                                     GA2TPGM 
00694          MOVE DFHBMUBF                                            GA2TPGM 
00695             TO MAP-DIAGNOSIS-TO-ATTR (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00696          IF WS-ERROR-SW NOT = 'Y'                                 GA2TPGM 
00697             MOVE 'Y' TO WS-ERROR-SW                               GA2TPGM 
00698             MOVE -1 TO MAP-DIAGNOSIS-TO-LEN (MAP-IDX1, MAP-IDX2)  GA2TPGM 
      *** ICD-10 START                                                          
                  IF WS-SAVED-DIAGNOSIS (1:3) = 'BIT'                           
00651                SET WT-01-INDEX TO +12                             GA2TPGM 
                  ELSE                                                          
      *** ICD-10 END                                                            
00699                SET WT-01-INDEX TO +03                             GA2TPGM 
                  END-IF                                                        
00700             PERFORM 9000-000-MOVE-MSG-TO-SCREEN                   GA2TPGM 
00701          ELSE                                                     GA2TPGM 
00702             MOVE DFHBMUBF                                         GA2TPGM 
00703               TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)     GA2TPGM 
00704                  MAP-DIAGNOSIS-TO-ATTR   (MAP-IDX1, MAP-IDX2)     GA2TPGM 
00705      ELSE                                                         GA2TPGM 
00706         MOVE 'DEW1'  TO  WS-ABEND-CODE                            GA2TPGM 
00707         MOVE GCPPDIO-RETURN-MESSAGE TO ERRMSGO                    GA2TPGM 
00708         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
00709                                                                   GA2TPGM 
00712      IF MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)              GA2TPGM 
00713                                                   NOT = DFHBMUBF  GA2TPGM 
00714       AND MAP-DIAGNOSIS-TO-ATTR (MAP-IDX1, MAP-IDX2)              GA2TPGM 
00715                                                   NOT = DFHBMUBF  GA2TPGM 
00716         ADD 1            TO  WS-ADD-COUNT                         GA2TPGM 
00717         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2TPGM 
00718         MOVE MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)              GA2TPGM 
00719           TO WS-FROM-DIAGNOSIS-SORT (WS-SORT-IDX)                 GA2TPGM 
00720         MOVE MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2)              GA2TPGM 
00721           TO WS-TO-DIAGNOSIS-SORT (WS-SORT-IDX).                  GA2TPGM 
00722                                                                   GA2TPGM 
00724      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2TPGM 
00725         SET MAP-IDX1  UP BY  1                                    GA2TPGM 
00726         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2TPGM 
00727                                                                   GA2TPGM 
00729      IF MAP-IDX2  <  WS-MAP-COL                                   GA2TPGM 
00730         SET MAP-IDX1  TO  1                                       GA2TPGM 
00731         SET MAP-IDX2  UP BY  1                                    GA2TPGM 
00732         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2TPGM 
00733                                                                   GA2TPGM 
00735                                                                   GA2TPGM 
00736  2020-CHECK-FOR-ERRORS.                                           GA2TPGM 
00737                                                                   GA2TPGM 
00738      MOVE '2020'  TO  WS-PARA-ID.                                 GA2TPGM 
00739                                                                   GA2TPGM 
00740      IF A2TINEXI   =  'I' OR 'E'                                  GA2TPGM 
00741         CONTINUE                                                  GA2TPGM 
00742      ELSE                                                         GA2TPGM 
00743         MOVE DFHBMUBF   TO  A2TINEXO                              GA2TPGM 
00744         MOVE -1         TO  A2TINEXL                              GA2TPGM 
00745         MOVE 'Y'        TO  WS-ERROR-SW                           GA2TPGM 
00746         SET WT-01-INDEX TO +04                                    GA2TPGM 
00747         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2TPGM 
00748                                                                   GA2TPGM 
00749      IF WS-ERROR-SW  =  'Y'                                       GA2TPGM 
00750         MOVE LOW-VALUES  TO  A2TFUNCO,  A2TITLEO,  A2TSCRNO,      GA2TPGM 
00751                              A2TABIDO,  A2TABSLO,  A2TINIDO,      GA2TPGM 
00752                              A2TINSLO,  A2TINEXO,  A2TFRIDO,      GA2TPGM 
00753                              A2TADDEO,  A2TMENUO,  A2TECTRO,      GA2TPGM 
00754         MOVE '2100'  TO  WS-PARA-ID                               GA2TPGM 
00755         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2TPGM 
00756            VARYING MAP-IDX2 FROM  1  BY  1                        GA2TPGM 
00757              UNTIL MAP-IDX2  >  WS-MAP-COL                        GA2TPGM 
00758            AFTER   MAP-IDX1 FROM  1  BY  1                        GA2TPGM 
00759              UNTIL MAP-IDX1  >  WS-MAP-ROW                        GA2TPGM 
00760         MOVE '2020'  TO  WS-PARA-ID                               GA2TPGM 
00761         EXEC CICS SEND                                            GA2TPGM 
00762              MAP      ('GA2TI01')                                 GA2TPGM 
00763              MAPSET   ('GA2TSET') DATAONLY                        GA2TPGM 
00764              FROM     (GA2TI01O)  CURSOR                          GA2TPGM 
00765         END-EXEC                                                  GA2TPGM 
00766         GO TO 2099-EXIT.                                          GA2TPGM 
00767                                                                   GA2TPGM 
00768      IF WS-ADD-COUNT   NOT >  ZERO                                GA2TPGM 
00769       IF A2TINEXI = A2TXDRKI                                      GA2TPGM 
00770         SET MAP-IDX1, MAP-IDX2  TO  1                             GA2TPGM 
00771         MOVE DFHBMUBF                                             GA2TPGM 
00772           TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00773         MOVE '??????'                                             GA2TPGM 
00774           TO MAP-DIAGNOSIS-FROM      (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00775         MOVE -1  TO  MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2)  GA2TPGM 
00776         MOVE DFHBMUBF                                             GA2TPGM 
00777           TO MAP-DIAGNOSIS-TO-ATTR (MAP-IDX1, MAP-IDX2)           GA2TPGM 
00778         MOVE '??????'                                             GA2TPGM 
00779           TO MAP-DIAGNOSIS-TO      (MAP-IDX1, MAP-IDX2)           GA2TPGM 
00780         MOVE LOW-VALUES  TO  A2TFUNCO,  A2TITLEO,  A2TSCRNO,      GA2TPGM 
00781                              A2TABIDO,  A2TABSLO,  A2TINIDO,      GA2TPGM 
00782                              A2TINSLO,  A2TINEXO,  A2TFRIDO,      GA2TPGM 
00783                              A2TADDEO,  A2TMENUO,  A2TECTRO,      GA2TPGM 
00784         SET WT-01-INDEX  TO +05                                   GA2TPGM 
00785         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
00786         EXEC CICS SEND                                            GA2TPGM 
00787              MAP    ('GA2TI01')                                   GA2TPGM 
00788              MAPSET ('GA2TSET') DATAONLY                          GA2TPGM 
00789              FROM   (GA2TI01O)  CURSOR                            GA2TPGM 
00790         END-EXEC                                                  GA2TPGM 
00791         GO TO 2099-EXIT                                           GA2TPGM 
00792       ELSE                                                        GA2TPGM 
00793         SET MAP-IDX1, MAP-IDX2  TO  1                             GA2TPGM 
00794         MOVE DFHBMUBF                                             GA2TPGM 
00795           TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00796         MOVE '??????'                                             GA2TPGM 
00797           TO MAP-DIAGNOSIS-FROM      (MAP-IDX1, MAP-IDX2)         GA2TPGM 
00798         MOVE -1  TO  MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2)  GA2TPGM 
00799         MOVE DFHBMUBF                                             GA2TPGM 
00800           TO MAP-DIAGNOSIS-TO-ATTR (MAP-IDX1, MAP-IDX2)           GA2TPGM 
00801         MOVE '??????'                                             GA2TPGM 
00802           TO MAP-DIAGNOSIS-TO      (MAP-IDX1, MAP-IDX2)           GA2TPGM 
00803         MOVE LOW-VALUES  TO  A2TFUNCO,  A2TITLEO,  A2TSCRNO,      GA2TPGM 
00804                              A2TABIDO,  A2TABSLO,  A2TINIDO,      GA2TPGM 
00805                              A2TINSLO,  A2TINEXO,  A2TFRIDO,      GA2TPGM 
00806                              A2TADDEO,  A2TMENUO,  A2TECTRO,      GA2TPGM 
00807         SET WT-01-INDEX  TO +05                                   GA2TPGM 
00808         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
00809         EXEC CICS SEND                                            GA2TPGM 
00810              MAP    ('GA2TI01')                                   GA2TPGM 
00811              MAPSET ('GA2TSET') DATAONLY                          GA2TPGM 
00812              FROM   (GA2TI01O)  CURSOR                            GA2TPGM 
00813         END-EXEC                                                  GA2TPGM 
00814         GO TO 2099-EXIT.                                          GA2TPGM 
00815                                                                   GA2TPGM 
00817                                                                   GA2TPGM 
00818  2025-CONTINUE-PROCESSING.                                        GA2TPGM 
00819                                                                   GA2TPGM 
00820      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN   =                  GA2TPGM 
00821               GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +            GA2TPGM 
00822                         GC-GCTABULR-IRDX-FIXED-LEN +              GA2TPGM 
00823      (GC-GCTABULR-IRDX-VARY-LEN * GC-GCTABULR-IRDX-VARY-MAX-OCUR).GA2TPGM 
00824                                                                   GA2TPGM 
00825      EXEC CICS                                                    GA2TPGM 
00826         GETMAIN  SET (ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)     GA2TPGM 
00827         INITIMG  (WS-HEX-00)                                      GA2TPGM 
00828         LENGTH   (WS-IO-PARM-WRK-INTERNL-TAB-LEN)                 GA2TPGM 
00829      END-EXEC.                                                    GA2TPGM 
00830                                                                   GA2TPGM 
00831      MOVE SPACES TO GCIO-WORKFILE-KEY.                            GA2TPGM 
00832      MOVE  'C'  TO GCIO-WRK-STATUS-CODE.                          GA2TPGM 
00833      MOVE  'C3' TO GCIO-WRK-RECORD-TYPE.                          GA2TPGM 
00834      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA2TPGM 
00835      MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NUM.              GA2TPGM 
00836      MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NUM.          GA2TPGM 
00837      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA2TPGM 
00838      MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2TPGM 
00839      MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2TPGM 
00840      MOVE CONTRACT-FAM-REL-LVL TO  GCIO-WRK-FAMILY-RELATION-LVL.  GA2TPGM 
00841      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA2TPGM 
00842                                                                   GA2TPGM 
00843      MOVE GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.               GA2TPGM 
00844      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2TPGM 
00845      MOVE A2TABIDI  TO  GCIO-WRK-PROVISION-ID.                    GA2TPGM 
00846      MOVE A2TABSLI  TO  GCIO-WRK-PROVISION-SLOT-NO.               GA2TPGM 
00847      MOVE '#IRDX'   TO  GCIO-WRK-TAB-PROVISION-ID.                GA2TPGM 
00848      MOVE A2TINSLI  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2TPGM 
00849      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2TPGM 
00850                                                                   GA2TPGM 
00851      MOVE GC-GCTABULR-IRDX-VARY-MAX-OCUR                          GA2TPGM 
00852        TO GXG-ENTRY-COUNT.                                        GA2TPGM 
00853                                                                   GA2TPGM 
00854      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2TPGM 
00855                                                                   GA2TPGM 
00856      EXEC CICS LINK                                               GA2TPGM 
00857           PROGRAM  ('GCIOPGM')                                    GA2TPGM 
00858           COMMAREA (IO-PARM-INTERNAL-TAB-RECORD)                  GA2TPGM 
00859           LENGTH   (WS-IO-PARM-WRK-INTERNL-TAB-LEN)               GA2TPGM 
00860      END-EXEC.                                                    GA2TPGM 
00861                                                                   GA2TPGM 
00862      IF NOT GCIO-GOOD-RETURN                                      GA2TPGM 
00863         SET WT-01-INDEX TO +06                                    GA2TPGM 
00864         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
00865         MOVE '2T01'  TO  WS-ABEND-CODE                            GA2TPGM 
00866         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
00867                                                                   GA2TPGM 
00868      MOVE A2TINEXI  TO  A2TXDRKO                                  GA2TPGM 
00869                         GXG-INCLUDE-EXCLUDE-IND.                  GA2TPGM 
00870                                                                   GA2TPGM 
00871      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2TPGM 
00872         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2TPGM 
00873                                                                   GA2TPGM 
00874      SET WS-SORT-IDX  TO  1.                                      GA2TPGM 
00875      SET WS-SORT-IDX2  TO  2.                                     GA2TPGM 
00876      MOVE '2030'  TO  WS-PARA-ID.                                 GA2TPGM 
00877                                                                   GA2TPGM 
00878  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2TPGM 
00879                                                                   GA2TPGM 
00880      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2TPGM 
00881         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2TPGM 
00882                                                                   GA2TPGM 
00883                                                                   GA2TPGM 
00884      IF WS-DIAGNOSIS-SORT (WS-SORT-IDX)     <                     GA2TPGM 
00885         WS-DIAGNOSIS-SORT (WS-SORT-IDX2)                          GA2TPGM 
00886         SET WS-SORT-IDX2  UP BY  1                                GA2TPGM 
00887         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2TPGM 
00888      ELSE                                                         GA2TPGM 
00889         IF WS-DIAGNOSIS-SORT (WS-SORT-IDX)  >                     GA2TPGM 
00890            WS-DIAGNOSIS-SORT (WS-SORT-IDX2)                       GA2TPGM 
00891            MOVE WS-DIAGNOSIS-SORT (WS-SORT-IDX)                   GA2TPGM 
00892              TO WS-SAVED-DIAGNOSIS                                GA2TPGM 
00893            MOVE WS-DIAGNOSIS-SORT (WS-SORT-IDX2)                  GA2TPGM 
00894              TO WS-DIAGNOSIS-SORT (WS-SORT-IDX)                   GA2TPGM 
00895            MOVE WS-SAVED-DIAGNOSIS                                GA2TPGM 
00896              TO WS-DIAGNOSIS-SORT (WS-SORT-IDX2)                  GA2TPGM 
00897            SET WS-SORT-IDX2  UP BY  1                             GA2TPGM 
00898            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2TPGM 
00899                                                                   GA2TPGM 
00900      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2TPGM 
00901      MOVE WS-DIAGNOSIS-SORT (WS-SORT-IDX3)                        GA2TPGM 
00902        TO WS-DIAGNOSIS-SORT (WS-SORT-IDX2).                       GA2TPGM 
00903      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2TPGM 
00904      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2TPGM 
00905                                                                   GA2TPGM 
00906  2040-ARE-WE-DONE-WITH-SORT.                                      GA2TPGM 
00907                                                                   GA2TPGM 
00908      MOVE '2040'  TO  WS-PARA-ID.                                 GA2TPGM 
00909      SET WS-SORT-IDX  UP BY  1.                                   GA2TPGM 
00910                                                                   GA2TPGM 
00911      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2TPGM 
00912         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2TPGM 
00913         SET WS-SORT-IDX2  UP BY  1                                GA2TPGM 
00914         MOVE '2030'  TO  WS-PARA-ID                               GA2TPGM 
00915         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2TPGM 
00916                                                                   GA2TPGM 
00917      SET WS-ADD-COUNT TO WS-SORT-IDX.                             GA2TPGM 
00918      MOVE HIGH-VALUES TO WS-SORT-DIAGNOSIS-ENTRY (WS-SORT-IDX).   GA2TPGM 
00919      MOVE GXG-ENTRY-COUNT  TO  GXG-ENTRY-COUNT.                   GA2TPGM 
00920                                                                   GA2TPGM 
00921      COMPUTE  WS-COPY-LENGTH                  =                   GA2TPGM 
00922               GC-GCTABULR-IRDX-VARY-MAX-OCUR  *                   GA2TPGM 
00923               GC-GCTABULR-IRDX-VARY-LEN.                          GA2TPGM 
00924                                                                   GA2TPGM 
00925      EXEC CICS                                                    GA2TPGM 
00926         GETMAIN  SET(ADDRESS OF LK-COPY-TABULAR-TABLE-AREA)       GA2TPGM 
00927         LENGTH   (WS-COPY-LENGTH)                                 GA2TPGM 
00928         INITIMG  (WS-HEX-00)                                      GA2TPGM 
00929      END-EXEC.                                                    GA2TPGM 
00932                                                                   GA2TPGM 
00933      SET COPY-IDX,  GXG-INDEX  TO  1.                             GA2TPGM 
00934      MOVE '2050'  TO  WS-PARA-ID.                                 GA2TPGM 
                                                                                
00935  2050-MAKE-A-COPY-OF-RECORD.                                      GA2TPGM 
00936                                                                   GA2TPGM 
00937      MOVE GXG-ENTRIES TO LK-COPY-TABULAR-TABLE-AREA.              GA2TPGM 
00938                                                                   GA2TPGM 
00939      IF WS-ADD-COUNT  +  GXG-ENTRY-COUNT  >                       GA2TPGM 
00940            GC-GCTABULR-IRDX-VARY-MAX-OCUR                         GA2TPGM 
00941         SET WT-01-INDEX TO +07                                    GA2TPGM 
00942         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
00943         MOVE '2T02'  TO  WS-ABEND-CODE                            GA2TPGM 
00944         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
00945                                                                   GA2TPGM 
00946      SET WS-SORT-IDX,  COPY-IDX,  GXG-INDEX  TO  1.               GA2TPGM 
00948                                                                   GA2TPGM 
00949      MOVE '2060'  TO  WS-PARA-ID.                                 GA2TPGM 
                                                                                
00950  2060-MERGE-IN-NEW-ENTRIES.                                       GA2TPGM 
00951                                                                   GA2TPGM 
00952      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2TPGM 
00953         SET GXG-INDEX  DOWN BY  1                                 GA2TPGM 
00954         SET GXG-ENTRY-COUNT   TO  GXG-INDEX                       GA2TPGM 
00955         MOVE GXG-ENTRY-COUNT  TO  GXG-ENTRY-COUNT                 GA2TPGM 
00956         GO TO 2090-UPDATE-ALL-LVL-IN-TAB-REC.                     GA2TPGM 
00957                                                                   GA2TPGM 
00958      IF WS-SORT-DIAGNOSIS-ENTRY (WS-SORT-IDX)    = HIGH-VALUES    GA2TPGM 
00959        AND LK-COPY-TABULAR-TABLE (COPY-IDX)  NOT = HIGH-VALUES    GA2TPGM 
00960         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2TPGM 
00961                                                                   GA2TPGM 
00962      IF WS-SORT-DIAGNOSIS-ENTRY (WS-SORT-IDX)  NOT =  HIGH-VALUES GA2TPGM 
00963       AND  LK-COPY-TABULAR-TABLE (COPY-IDX)        = HIGH-VALUES  GA2TPGM 
00964         GO TO 2080-INSERT-NEW-ENTRY.                              GA2TPGM 
00965                                                                   GA2TPGM 
00966      IF WS-SORT-DIAGNOSIS-ENTRY (WS-SORT-IDX) = HIGH-VALUES       GA2TPGM 
00967       AND  LK-COPY-TABULAR-TABLE (COPY-IDX)   = HIGH-VALUES       GA2TPGM 
00968            NEXT SENTENCE                                          GA2TPGM 
00969      ELSE                                                         GA2TPGM 
00970      IF WS-DIAGNOSIS-SORT (WS-SORT-IDX)  >                        GA2TPGM 
00971                              LK-COPY-DIAGNOSIS-RANGE (COPY-IDX)   GA2TPGM 
00972         GO TO 2070-SAVE-COPIED-ENTRY                              GA2TPGM 
00973      ELSE                                                         GA2TPGM 
00974      IF WS-DIAGNOSIS-SORT (WS-SORT-IDX)  <                        GA2TPGM 
00975                              LK-COPY-DIAGNOSIS-RANGE (COPY-IDX)   GA2TPGM 
00976         GO TO 2080-INSERT-NEW-ENTRY.                              GA2TPGM 
00977                                                                   GA2TPGM 
00978 ******************************************************************GA2TPGM 
00979 *    AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2TPGM 
00980 *    OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2TPGM 
00981 *    INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2TPGM 
00982 *    FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2TPGM 
00983 ******************************************************************GA2TPGM 
00984                                                                   GA2TPGM 
00985      SET WS-SORT-IDX  UP BY  1.                                   GA2TPGM 
00986                                                                   GA2TPGM 
00987  2070-SAVE-COPIED-ENTRY.                                          GA2TPGM 
00988                                                                   GA2TPGM 
00989      MOVE '2070'  TO  WS-PARA-ID.                                 GA2TPGM 
00990      MOVE LK-COPY-TABULAR-TABLE (COPY-IDX)                        GA2TPGM 
00991        TO GXG-ENTRY (GXG-INDEX).                                  GA2TPGM 
00992                                                                   GA2TPGM 
00993      IF COPY-IDX  NOT >  GXG-ENTRY-COUNT                          GA2TPGM 
00994         SET COPY-IDX   UP BY  1                                   GA2TPGM 
00995         SET GXG-INDEX  UP BY  1                                   GA2TPGM 
00996         MOVE '2060'  TO  WS-PARA-ID                               GA2TPGM 
00997         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2TPGM 
00998      ELSE                                                         GA2TPGM 
00999         SET WT-01-INDEX TO +08                                    GA2TPGM 
01000         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
01001         MOVE '2T03'  TO  WS-ABEND-CODE                            GA2TPGM 
01002         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
01003                                                                   GA2TPGM 
01004  2080-INSERT-NEW-ENTRY.                                           GA2TPGM 
                                                                                
01005      MOVE '2080'  TO  WS-PARA-ID.                                 GA2TPGM 
01006                                                                   GA2TPGM 
01007      MOVE WS-FROM-DIAGNOSIS-SORT (WS-SORT-IDX)                    GA2TPGM 
01008        TO GXG-DIAGNOSIS-FROM (GXG-INDEX).                         GA2TPGM 
01009      MOVE WS-TO-DIAGNOSIS-SORT (WS-SORT-IDX)                      GA2TPGM 
01010        TO GXG-DIAGNOSIS-TO   (GXG-INDEX).                         GA2TPGM 
01011                                                                   GA2TPGM 
01012      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2TPGM 
01013         SET WS-SORT-IDX  UP BY  1                                 GA2TPGM 
01014         SET GXG-INDEX    UP BY  1                                 GA2TPGM 
01015         MOVE '2060'  TO  WS-PARA-ID                               GA2TPGM 
01016         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2TPGM 
01017      ELSE                                                         GA2TPGM 
01018         SET WT-01-INDEX TO +08                                    GA2TPGM 
01019         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
01020         MOVE '2T04'  TO  WS-ABEND-CODE                            GA2TPGM 
01021         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
01022                                                                   GA2TPGM 
01024                                                                   GA2TPGM 
01025  2090-UPDATE-ALL-LVL-IN-TAB-REC.                                  GA2TPGM 
01026                                                                   GA2TPGM 
01027      MOVE '2090'  TO  WS-PARA-ID.                                 GA2TPGM 
01028      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2TPGM 
01029      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2TPGM 
01030                                                                   GA2TPGM 
01031      COMPUTE  GCIO-RECORD-LENGTH = GC-WORKFILE-KEY-LEN +          GA2TPGM 
01032                     GC-GCTABULR-IRDX-FIXED-LEN +                  GA2TPGM 
01033              (GXG-ENTRY-COUNT  *  GC-GCTABULR-IRDX-VARY-LEN).     GA2TPGM 
01034                                                                   GA2TPGM 
01035      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN   =                  GA2TPGM 
01036               GC-GCIOPARM-LEN  +  GCIO-RECORD-LENGTH.             GA2TPGM 
01037                                                                   GA2TPGM 
01038      EXEC CICS LINK                                               GA2TPGM 
01039           PROGRAM  ('GCIOPGM')                                    GA2TPGM 
01040           COMMAREA (IO-PARM-INTERNAL-TAB-RECORD)                  GA2TPGM 
01041           LENGTH   (WS-IO-PARM-WRK-INTERNL-TAB-LEN)               GA2TPGM 
01042      END-EXEC.                                                    GA2TPGM 
01043                                                                   GA2TPGM 
01044      IF NOT GCIO-GOOD-RETURN                                      GA2TPGM 
01045         SET WT-01-INDEX TO +09                                    GA2TPGM 
01046         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
01047         MOVE '2T05'  TO  WS-ABEND-CODE                            GA2TPGM 
01048         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
01049                                                                   GA2TPGM 
01050      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2TPGM 
01051         VARYING MAP-IDX2 FROM 1  BY  1                            GA2TPGM 
01052           UNTIL MAP-IDX2  >  WS-MAP-COL                           GA2TPGM 
01053         AFTER   MAP-IDX1 FROM 1  BY  1                            GA2TPGM 
01054           UNTIL MAP-IDX1  >  WS-MAP-ROW.                          GA2TPGM 
01055                                                                   GA2TPGM 
01056      EXEC CICS SEND                                               GA2TPGM 
01057           MAP    ('GA2TI01')                                      GA2TPGM 
01058           MAPSET ('GA2TSET') ERASE                                GA2TPGM 
01059           FROM   (GA2TI01O)                                       GA2TPGM 
01060      END-EXEC.                                                    GA2TPGM 
01061                                                                   GA2TPGM 
01062  2099-EXIT.                                                       GA2TPGM 
01063      EXIT.                                                        GA2TPGM 
01064 /*****************************************************************GA2TPGM 
01065 *      D O N ' T   R E T R A N S M I T   F I E L D S              GA2TPGM 
01066 *                                                                 GA2TPGM 
01067 *    WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2TPGM 
01068 *   ALREADY ON THE OPERATORS SCREEN.                              GA2TPGM 
01069 *                                                                 GA2TPGM 
01070 ******************************************************************GA2TPGM 
01071  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2TPGM 
01072                                                                   GA2TPGM 
01073      MOVE LOW-VALUES                                              GA2TPGM 
01074        TO MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)                 GA2TPGM 
01075           MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2).                GA2TPGM 
01076                                                                   GA2TPGM 
01077  2199-EXIT.                                                       GA2TPGM 
01078      EXIT.                                                        GA2TPGM 
01079 /*****************************************************************GA2TPGM 
01080 *   X C T L   T O   D E L E T E   S C R E E N                     GA2TPGM 
01081 *                                                                 GA2TPGM 
01082 *   THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2TPGM 
01083 *  DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2TPGM 
01084 *  PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2TPGM 
01085 *  TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2TPGM 
01086 *  THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2TPGM 
01087 *  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2TPGM 
01088 ******************************************************************GA2TPGM 
01089  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2TPGM 
01090                                                                   GA2TPGM 
01091      MOVE '3000'  TO  WS-PARA-ID.                                 GA2TPGM 
01092                                                                   GA2TPGM 
01093      COMPUTE  WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                   GA2TPGM 
01094            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA2TPGM 
01095            GC-GCTABULR-IRDX-FIXED-LEN +                           GA2TPGM 
01096      (GC-GCTABULR-IRDX-VARY-LEN * GC-GCTABULR-IRDX-VARY-MAX-OCUR).GA2TPGM 
01097                                                                   GA2TPGM 
01098      EXEC CICS                                                    GA2TPGM 
01099         GETMAIN  SET(ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD)      GA2TPGM 
01100         INITIMG  (WS-HEX-00)                                      GA2TPGM 
01101         LENGTH   (WS-IO-PARM-WRK-INTERNL-TAB-LEN)                 GA2TPGM 
01102      END-EXEC.                                                    GA2TPGM 
01103                                                                   GA2TPGM 
01104      MOVE SPACES  TO  GCIO-WORKFILE-KEY.                          GA2TPGM 
01105      MOVE   'C'   TO  GCIO-WRK-STATUS-CODE.                       GA2TPGM 
01106      MOVE   'C3'  TO  GCIO-WRK-RECORD-TYPE.                       GA2TPGM 
01107      MOVE  GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                   GA2TPGM 
01108      MOVE  GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                   GA2TPGM 
01109      MOVE  GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.               GA2TPGM 
01110      MOVE  GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                     GA2TPGM 
01111      MOVE  GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS.                     GA2TPGM 
01112      MOVE  GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL.             GA2TPGM 
01113      MOVE  GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.       GA2TPGM 
01114      MOVE  GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                   GA2TPGM 
01115                                                                   GA2TPGM 
01116      MOVE  GCA-ALL-LEVEL-TAB-ID TO GCIO-WRK-PROVISION-ID.         GA2TPGM 
01117      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2TPGM 
01118      MOVE  GCA-INTERNAL-TAB-ID TO GCIO-WRK-TAB-PROVISION-ID.      GA2TPGM 
01119      MOVE  GCA-INTERNAL-TAB-SLOT TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2TPGM 
01120      MOVE  A2TADDEI  TO  GCA-ADD-DEL-IND.                         GA2TPGM 
01121      MOVE  A2TMENUI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA2TPGM 
01122      MOVE  A2TECTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA2TPGM 
01123      MOVE  A2TFRIDI  TO GCA-FROM-MENU-ID.                         GA2TPGM 
01124      MOVE  A2TINEXI  TO GCA-I-E-INDC.                             GA2TPGM 
01125                                                                   GA2TPGM 
01126      MOVE  GC-GCPSWORK-DDNAME  TO  GCIO-FILE-DDNAME.              GA2TPGM 
01127      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2TPGM 
01128                                                                   GA2TPGM 
01129      SET GCA-RECORD-POINTER                                       GA2TPGM 
01130       TO ADDRESS OF IO-PARM-INTERNAL-TAB-RECORD.                  GA2TPGM 
01131                                                                   GA2TPGM 
01132      MOVE GC-GCTABULR-IRDX-VARY-MAX-OCUR                          GA2TPGM 
01133        TO GXG-ENTRY-COUNT.                                        GA2TPGM 
01134                                                                   GA2TPGM 
01135      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2TPGM 
01136                                                                   GA2TPGM 
01137      EXEC CICS LINK                                               GA2TPGM 
01138           PROGRAM  ('GCIOPGM')                                    GA2TPGM 
01139           COMMAREA (IO-PARM-INTERNAL-TAB-RECORD)                  GA2TPGM 
01140           LENGTH   (WS-IO-PARM-WRK-INTERNL-TAB-LEN)               GA2TPGM 
01141      END-EXEC.                                                    GA2TPGM 
01142                                                                   GA2TPGM 
01143      IF NOT GCIO-GOOD-RETURN                                      GA2TPGM 
01144         SET WT-01-INDEX TO +10                                    GA2TPGM 
01145         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
01146         MOVE '2T06'  TO  WS-ABEND-CODE                            GA2TPGM 
01147         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
01148                                                                   GA2TPGM 
01149      EXEC CICS XCTL                                               GA2TPGM 
01150           PROGRAM  ('GA1TPGM')                                    GA2TPGM 
01151           COMMAREA (DFHCOMMAREA)                                  GA2TPGM 
01152           LENGTH   (LENGTH OF DFHCOMMAREA)                        GA2TPGM 
01153      END-EXEC.                                                    GA2TPGM 
01154                                                                   GA2TPGM 
01155  3099-EXIT.                                                       GA2TPGM 
01156      EXIT.                                                        GA2TPGM 
01157 /**************************************************************** GA2TPGM 
01158 *           D I S P L A Y   F I R S T   S C R E E N               GA2TPGM 
01159 *                                                                 GA2TPGM 
01160 *    THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU   GA2TPGM 
01161 *  OR THE DELETE PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ  GA2TPGM 
01162 *  THE ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD     GA2TPGM 
01163 *  (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA2TPGM 
01164 *  THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA2TPGM 
01165 *    THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2TPGM 
01166 *  (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA2TPGM 
01167 *  AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2TPGM 
01168 *  DETERMINATION OF APPROPRIATE ACTION.                           GA2TPGM 
01169 ******************************************************************GA2TPGM 
01170  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2TPGM 
01171                                                                   GA2TPGM 
01172      MOVE '4000'  TO  WS-PARA-ID.                                 GA2TPGM 
01173      MOVE LOW-VALUES TO GA2TI01I.                                 GA2TPGM 
01174                                                                   GA2TPGM 
01175      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2TPGM 
01176         SET WT-01-INDEX TO +11                                    GA2TPGM 
01177         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
01178         MOVE '2T07'  TO  WS-ABEND-CODE                            GA2TPGM 
01179         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
01180                                                                   GA2TPGM 
01181      MOVE GCA-ALL-LEVEL-TAB-ID        TO A2TABIDO.                GA2TPGM 
01182      MOVE GCA-ALL-LEVEL-TAB-SLOT      TO A2TABSLO.                GA2TPGM 
01183      MOVE GCA-INTERNAL-TAB-ID         TO A2TINIDO.                GA2TPGM 
01184      MOVE GCA-INTERNAL-TAB-SLOT       TO A2TINSLO.                GA2TPGM 
01185      MOVE GCA-ADD-DEL-IND             TO A2TADDEO.                GA2TPGM 
01186      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE TO A2TMENUO.                GA2TPGM 
01187      MOVE GCA-OCCURS-ENTRY-COUNTER    TO A2TECTRO.                GA2TPGM 
01188      MOVE GCA-FROM-MENU-ID            TO A2TFRIDO.                GA2TPGM 
01189      MOVE GCA-I-E-INDC                TO A2TINEXO                 GA2TPGM 
01190                                          A2TXDRKO.                GA2TPGM 
01191      MOVE WS-ADD                      TO A2TFLITO.                GA2TPGM 
01192      MOVE WS-TITLE-LINE               TO A2TITLEO.                GA2TPGM 
01193                                                                   GA2TPGM 
01194      MOVE 'PLN: '  TO  CONTRACT-PLAN-HEADING.                     GA2TPGM 
01195      MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE.                    GA2TPGM 
01196      MOVE ' GRP: '  TO  CONTRACT-GROUP-HEADING.                   GA2TPGM 
01197      MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO.                    GA2TPGM 
01198      MOVE ' SEC: '  TO  CONTRACT-SECTION-HEADING.                 GA2TPGM 
01199      MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO.                GA2TPGM 
01200      MOVE ' PKG: '  TO  CONTRACT-PKG-HEADING.                     GA2TPGM 
01201      MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE.                      GA2TPGM 
01202      MOVE ' LOB: '  TO  CONTRACT-LOB-HEADING.                     GA2TPGM 
01203      MOVE GCA-L-O-B  TO  CONTRACT-LOB.                            GA2TPGM 
01204      MOVE ' PRV: '  TO  CONTRACT-PROV-CTL-HEADING.                GA2TPGM 
01205      MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL.                    GA2TPGM 
01206      MOVE ' FR: '  TO  CONTRACT-FAM-REL-HEADING.                  GA2TPGM 
01207      MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL.              GA2TPGM 
01208      MOVE ' EFDT: '  TO  CONTRACT-EFF-DT-HEADING.                 GA2TPGM 
01209      MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.              GA2TPGM 
01210                                                                   GA2TPGM 
01211      EXEC CICS SEND                                               GA2TPGM 
01212           MAP    ('GA2TI01')                                      GA2TPGM 
01213           MAPSET ('GA2TSET') ERASE                                GA2TPGM 
01214           FROM   (GA2TI01O)                                       GA2TPGM 
01215      END-EXEC.                                                    GA2TPGM 
01216                                                                   GA2TPGM 
01217  4099-EXIT.                                                       GA2TPGM 
01218      EXIT.                                                        GA2TPGM 
01219 /**************************************************************** GA2TPGM 
01220 *         X C T L   T O   P R E V I O U S   M E N U               GA2TPGM 
01221 *                                                                 GA2TPGM 
01222 *   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2TPGM 
01223 *  ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2TPGM 
01224 *  RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2TPGM 
01225 *  THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2TPGM 
01226 *  IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2TPGM 
01227 ******************************************************************GA2TPGM 
01228  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2TPGM 
01229                                                                   GA2TPGM 
01230      MOVE '5000'  TO  WS-PARA-ID.                                 GA2TPGM 
01231                                                                   GA2TPGM 
01232 *******                                                           GA2TPGM 
01233 * STS *  RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA       GA2TPGM 
01234 *******                                                           GA2TPGM 
01235                                                                   GA2TPGM 
01236 *    IF  A2TMENUI  =  'GTM1'                                      GA2TPGM 
01237 *        EXEC CICS XCTL                                           GA2TPGM 
01238 *             PROGRAM ('GTM1PGM')                                 GA2TPGM 
01239 *        END-EXEC.                                                GA2TPGM 
01240                                                                   GA2TPGM 
01241      IF  A2TMENUI  =  'GTM1'    AND                               GA2TPGM 
01242          A2TABIDI  =  'STS000'                                    GA2TPGM 
01243          EXEC CICS XCTL  PROGRAM ('GTM1PGM')  END-EXEC.           GA2TPGM 
01244                                                                   GA2TPGM 
01245                                                                   GA2TPGM 
01246      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =                       GA2TPGM 
01247            GC-GCIOPARM-LEN +  GC-WORKFILE-KEY-LEN +               GA2TPGM 
01248            GC-GCTABULR-CDRS-FIXED-LEN     +                       GA2TPGM 
01249           (GC-GCTABULR-CDRS-VARY-LEN  *                           GA2TPGM 
01250               GC-GCTABULR-CDRS-VARY-MAX-OCUR).                    GA2TPGM 
01251                                                                   GA2TPGM 
01252      EXEC CICS GETMAIN                                            GA2TPGM 
01253         SET     (ADDRESS OF IO-PARM-ALL-LEVEL-RECORD)             GA2TPGM 
01254         INITIMG (WS-HEX-00)                                       GA2TPGM 
01255         LENGTH  (WS-IO-PARM-WRK-ALL-LVL-LEN)                      GA2TPGM 
01256      END-EXEC.                                                    GA2TPGM 
01257                                                                   GA2TPGM 
01258      MOVE SPACES TO GCIO-WORKFILE-KEY.                            GA2TPGM 
01259      MOVE  'C'  TO GCIO-WRK-STATUS-CODE.                          GA2TPGM 
01260      MOVE  'C3' TO GCIO-WRK-RECORD-TYPE.                          GA2TPGM 
01261      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA2TPGM 
01262      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA2TPGM 
01263      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA2TPGM 
01264      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA2TPGM 
01265      MOVE GCA-L-O-B TO GCIO-WRK-LINE-OF-BUS.                      GA2TPGM 
01266      MOVE GCA-PROV-CTL TO GCIO-WRK-PROVIDER-CONTROL.              GA2TPGM 
01267      MOVE GCA-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.        GA2TPGM 
01268      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA2TPGM 
01269                                                                   GA2TPGM 
01270      MOVE SPACES    TO  GCA-BEN-PROV-ID.                          GA2TPGM 
01271      MOVE A2TABIDI  TO  GCIO-WRK-PROVISION-ID,                    GA2TPGM 
01272                         GCA-ALL-LEVEL-TAB-ID.                     GA2TPGM 
01273      MOVE A2TABSLI  TO  GCIO-WRK-PROVISION-SLOT-NO,               GA2TPGM 
01274                         GCA-ALL-LEVEL-TAB-SLOT.                   GA2TPGM 
01275                                                                   GA2TPGM 
01276      MOVE SPACES    TO  WS-INTERNAL-ID                            GA2TPGM 
01277      MOVE ZEROES    TO  WS-INTERNAL-SLOT-NO                       GA2TPGM 
01278      MOVE A2TINIDI  TO  WS-INTERNAL-ID                            GA2TPGM 
01279      MOVE A2TINSLI  TO  WS-INTERNAL-SLOT-NO                       GA2TPGM 
01280                                                                   GA2TPGM 
01281      MOVE SPACES    TO  GCIO-WRK-TAB-PROVISION-ID,                GA2TPGM 
01282                         GCA-INTERNAL-TAB-ID,                      GA2TPGM 
01283                         GCA-INTERNAL-TAB-SLOT.                    GA2TPGM 
01284      MOVE ZEROES    TO  GCIO-WRK-TAB-PROV-SLOT-NO.                GA2TPGM 
01285                                                                   GA2TPGM 
01286      MOVE GC-GCPSWORK-DDNAME TO  GCIO2-FILE-DDNAME.               GA2TPGM 
01287      MOVE SPACES             TO  GCA-I-E-INDC.                    GA2TPGM 
01288      MOVE A2TADDEI           TO  GCA-ADD-DEL-IND.                 GA2TPGM 
01289      MOVE A2TMENUI           TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.     GA2TPGM 
01290      MOVE A2TECTRI           TO  GCA-OCCURS-ENTRY-COUNTER.        GA2TPGM 
01291      MOVE A2TFRIDI           TO  GCA-FROM-MENU-ID.                GA2TPGM 
01292      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2TPGM 
01293                                                                   GA2TPGM 
01294      SET GCA-RECORD-POINTER                                       GA2TPGM 
01295       TO ADDRESS OF IO-PARM-ALL-LEVEL-RECORD.                     GA2TPGM 
01296                                                                   GA2TPGM 
01297      MOVE GC-GCTABULR-CDRS-VARY-MAX-OCUR                          GA2TPGM 
01298        TO GTE-ENTRY-COUNT.                                        GA2TPGM 
01299                                                                   GA2TPGM 
01300      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA2TPGM 
01301                                                                   GA2TPGM 
01302      EXEC CICS LINK                                               GA2TPGM 
01303           PROGRAM  ('GCIOPGM')                                    GA2TPGM 
01304           COMMAREA (IO-PARM-ALL-LEVEL-RECORD)                     GA2TPGM 
01305           LENGTH   (WS-IO-PARM-WRK-ALL-LVL-LEN)                   GA2TPGM 
01306      END-EXEC.                                                    GA2TPGM 
01307                                                                   GA2TPGM 
01308      IF NOT GCIO2-GOOD-RETURN                                     GA2TPGM 
01309         SET WT-01-INDEX TO +06                                    GA2TPGM 
01310         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2TPGM 
01311         MOVE '2T08'  TO  WS-ABEND-CODE                            GA2TPGM 
01312         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2TPGM 
01313                                                                   GA2TPGM 
01314 *** FIND SEQUENCE NUMBER WITH #IRDX PRIMARY DRIVER       ***      GA2TPGM 
01315                                                                   GA2TPGM 
01316      IF  GCA-FROM-MENU-ID  =  'GC4G'                              GA2TPGM 
01317          SEARCH GTE-PRIMARY-ENTRY                                 GA2TPGM 
01318              AT END                                               GA2TPGM 
01319                  SET GTE-INDEX TO 1                               GA2TPGM 
01320              WHEN ((GTE-PRIMARY-DRIVER (GTE-INDEX) = '#IRDX ')    GA2TPGM 
01321                   AND                                             GA2TPGM 
01322                    (GTE-PRIMARY-SLOT-NO (GTE-INDEX) =             GA2TPGM 
01323                     WS-INTERNAL-SLOT-NO))                         GA2TPGM 
01324                   MOVE 'Y'   TO WS-PRIM-IND                       GA2TPGM 
01325                   MOVE SPACE TO WS-ADDL-IND                       GA2TPGM 
01326                   MOVE GTE-PRIM-SEQ-NO (GTE-INDEX)                GA2TPGM 
01327                     TO WS-PRIM-SEQ-NO.                            GA2TPGM 
01328                                                                   GA2TPGM 
01329      EXEC CICS XCTL                                               GA2TPGM 
01330           PROGRAM  ('GC4HPGM')                                    GA2TPGM 
01331           COMMAREA (DFHCOMMAREA)                                  GA2TPGM 
01332           LENGTH   (LENGTH OF DFHCOMMAREA)                        GA2TPGM 
01333      END-EXEC.                                                    GA2TPGM 
01334                                                                   GA2TPGM 
01335  5099-EXIT.                                                       GA2TPGM 
01336      EXIT.                                                        GA2TPGM 
01337 /**************************************************************** GA2TPGM 
01338 *       X C T L   T O   M A I N   M E N U                         GA2TPGM 
01339 *                                                                 GA2TPGM 
01340 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2TPGM 
01341 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2TPGM 
01342 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2TPGM 
01343 *  PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2TPGM 
01344 *  PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2TPGM 
01345 *  AND PROGRESS DOWN.                                             GA2TPGM 
01346 ******************************************************************GA2TPGM 
01347  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2TPGM 
01348                                                                   GA2TPGM 
01349      MOVE '6000'  TO  WS-PARA-ID.                                 GA2TPGM 
01350      MOVE '2T09'  TO  WS-ABEND-CODE.                              GA2TPGM 
01351                                                                   GA2TPGM 
01352      EXEC CICS XCTL                                               GA2TPGM 
01353           PROGRAM ('GCPSPGM')                                     GA2TPGM 
01354      END-EXEC.                                                    GA2TPGM 
01355                                                                   GA2TPGM 
01356  6099-EXIT.                                                       GA2TPGM 
01357      EXIT.                                                        GA2TPGM 
01358 /***************************************************************  GA2TPGM 
01359 *                                                              *  GA2TPGM 
01360 * 9000   MOVE MESSAGE TO SCREEN                                *  GA2TPGM 
01361 *                                                              *  GA2TPGM 
01362 ****************************************************************  GA2TPGM 
01363  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA2TPGM 
01364  9000-010.                                                        GA2TPGM 
01365                                                                   GA2TPGM 
01366      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)                         GA2TPGM 
01367        TO ERRMSGO.                                                GA2TPGM 
01368                                                                   GA2TPGM 
01369  9000-900-EXIT.                                                   GA2TPGM 
01370      EXIT.                                                        GA2TPGM 
01371                                                                   GA2TPGM 
01372  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2TPGM 
01373                                                                   GA2TPGM 
01374      SET MAP-IDX1  TO  7.                                         GA2TPGM 
01375      SET MAP-IDX2  TO  1.                                         GA2TPGM 
01376      MOVE -1                                                      GA2TPGM 
01377        TO  MAP-DIAGNOSIS-FROM-LEN (MAP-IDX1, MAP-IDX2).           GA2TPGM 
01378      EXEC CICS SEND                                               GA2TPGM 
01379           MAP    ('GA2TI01')                                      GA2TPGM 
01380           MAPSET ('GA2TSET') ERASE                                GA2TPGM 
01381           FROM   (GA2TI01O)  CURSOR WAIT                          GA2TPGM 
01382      END-EXEC.                                                    GA2TPGM 
01383                                                                   GA2TPGM 
01384      EXEC CICS ABEND                                              GA2TPGM 
01385           ABCODE (WS-ABEND-CODE)                                  GA2TPGM 
01386      END-EXEC.                                                    GA2TPGM 
01387  9999-EXIT.                                                       GA2TPGM 
01388      EXIT.                                                        GA2TPGM 
