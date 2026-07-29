00001  IDENTIFICATION DIVISION.                                         04/07/05
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA1WPGM 
00003  PROGRAM-ID.     GA1WPGM.                                            LV002
00004  AUTHOR.         DELORES FRY.                                     GA1WPGM 
00005  DATE-WRITTEN.   AUGUST 2001.                                     GA1WPGM 
00006  DATE-COMPILED.                                                   GA1WPGM 
00007                                                                   GA1WPGM 
00008 *----------------------------------------------------------------*GA1WPGM 
00009 *                                                                *GA1WPGM 
00010 *            GENERIC CONTRACT PROCESSING SYSTEM (GCPS)           *GA1WPGM 
00011 *            =========================================           *GA1WPGM 
00012 *                                                                *GA1WPGM 
00013 *    #IRPV      RELATED PROVISIONS / ALTERNATE PROVISIONS        *GA1WPGM 
00014 *                                                                *GA1WPGM 
00015 *    INTERNAL TABULAR PROVISION MAINTENANCE \
00016 *                                                                *GA1WPGM 
00017 *   THIS PROGRAM WILL  \
00018 *   PROVISIONS ENTRIES FROM THE #IRPV INTERNAL TABULAR RECORD.   *GA1WPGM 
00019 *                                                                *GA1WPGM 
00020 *  THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE   *GA1WPGM 
00021 *  INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN DETERMINE    *GA1WPGM 
00022 *  IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN ENTRY      *GA1WPGM 
00023 *  WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD   *GA1WPGM 
00024 *  WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK     *GA1WPGM 
00025 *  INTO THE RECORD BEFORE UPDATING THE RECORD.                   *GA1WPGM 
00026 *                                                                *GA1WPGM 
00027 *  TO EXECUTE THE ADD PROGRAM FOR THIS SET OF DATA (ID: #IRPV)   *GA1WPGM 
00028 *  THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM     *GA1WPGM 
00029 *  TO XCTL TO TRANS-ID GA2W OR PROGRAM GA2WPGM.  THIS PROGRAM    *GA1WPGM 
00030 *  WILL VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN     *GA1WPGM 
00031 *  THE TABLE.                                                    *GA1WPGM 
00032 *                                                                *GA1WPGM 
00033 *   FUNC CODE: GA1W                                              *GA1WPGM 
00034 *   MAPSET:    GA1WSETC                                          *GA1WPGM 
00035 *   FILES:     GCPSWORK                                          *GA1WPGM 
00036 *                                                                *GA1WPGM 
00037 *----------------------------------------------------------------*GA1WPGM 
00038 *                                                                *GA1WPGM 
00039 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GA1WPGM 
00040 *      *-*         U P D A T E   H I S T O R Y         *-*       *GA1WPGM 
00041 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GA1WPGM 
00042 *                                                                *GA1WPGM 
00043 *----------------------------------------------------------------*GA1WPGM 
00044 *                                                                *GA1WPGM 
00045 **-CHG-NUM-* *--DATE--* *WHO* *---------DESCRIPTION--------------*GA1WPGM 
00046 *                                                                *GA1WPGM 
00047 *  P00072    00/00/0000 FRY   INITIAL PROGRAM CODING IN SUPPORT  *GA1WPGM 
00048 *                             OF THE NATIONAL CARRIER INITIATIVE.*GA1WPGM 
00049 *                                                                *GA1WPGM 
00050 *  D-358     10/02/2001 GSP   - ADDED CODING TO SET GTE-INDEX TO *GA1WPGM 
00051 *                               DESIRED SEQUENCE NUMBER FOR      *GA1WPGM 
00052 *                               PROCESSING IN GC4HPGM.           *GA1WPGM 
00053 *                                                                *GA1WPGM 
00054 *                             - REMOVED LOGIC TO XCTL TO 'GTM1'. *GA1WPGM 
00055 *                               WHEN PF3 IS ENTERED, THIS PGM    *GA1WPGM 
00056 *                               WILL NOW ALWAYS GO TO 'GC4H'.    *GA1WPGM 
00057 *                                                                *GA1WPGM 
00058 *  NO LOG #  10/08/2002 KIKI  - CORRECTED SEARCH FOR APPROPRIATE *GA1WPGM 
00059 *                               #IRPV SEQUENCE, CARRIED OVER TO  *GA1WPGM 
00060 *                               'GC4H' SCREEN                    *GA1WPGM 
00061 *                                                                *GA1WPGM 
00062 *  NO LOG #  10/01/2003 KIKI  ADDED INFORMATIONAL MESSAGES       *GA1WPGM 
00063 *                                                                *GA1WPGM 
00064 *  NO LOG #  10/29/2003 KIKI  FIX ENTRANCE TO  'GC4H'  WHEN      *GA1WPGM 
00065 *                             GCA-FROM-MENU-ID  =  'GTM1'        *GA1WPGM 
00066 *                             AND  'PF3'  IS PRESSED             *GA1WPGM 
00067 *                                                                *GA1WPGM 
00068 *----------------------------------------------------------------*GA1WPGM 
00069                                                                   GA1WPGM 
00070  ENVIRONMENT DIVISION.                                            GA1WPGM 
00071                                                                   GA1WPGM 
00072  DATA DIVISION.                                                   GA1WPGM 
00073  WORKING-STORAGE SECTION.                                         GA1WPGM 
00074                                                                   GA1WPGM 
00075  01  WS-BEGIN                    PIC X(23) VALUE                  GA1WPGM 
00076                                      '** GA1WPGM WS BEGINS **'.   GA1WPGM 
00077                                                                   GA1WPGM 
00078  01  FILLER                      PIC X(15) VALUE                  GA1WPGM 
00079                                      '**  PARA-ID  **'.           GA1WPGM 
00080  01  WS-PARA-ID                        PIC X(04) VALUE 'XXXX'.    GA1WPGM 
00081                                                                   GA1WPGM 
00082  01  FILLER                            PIC X(25) VALUE            GA1WPGM 
00083                                      '** GA1WPGM ABEND CODE **'.  GA1WPGM 
00084  01  WS-ABEND-CODE                     PIC X(04) VALUE 'XXXX'.    GA1WPGM 
00085                                                                   GA1WPGM 
00086  01  WS-ONE-LOW                        PIC X(01) VALUE LOW-VALUES.GA1WPGM 
00087                                                                   GA1WPGM 
00088  01  FILLER                            PIC X(17) VALUE            GA1WPGM 
00089                                             '*** WORK AREA ***'.  GA1WPGM 
00090  01  WS-WORK-FIELDS.                                              GA1WPGM 
00091      05  WS-ERROR-SW                   PIC X(01).                 GA1WPGM 
00092      05  WS-HEX-00                     PIC X(01).                 GA1WPGM 
00093      05  WS-QUOTIENT                   PIC 9(03) COMP-3.          GA1WPGM 
00094      05  WS-REMAINDER                  PIC 9(03) COMP-3.          GA1WPGM 
00095      05  WS-DELETE-COUNT               PIC 9(03) COMP-3.          GA1WPGM 
00096      05  WS-DEL-REQUEST                PIC X(03) VALUE 'DEL'.     GA1WPGM 
00097      05  WS-CONTRACT-TITLE-LINE        PIC X(46) VALUE            GA1WPGM 
00098          'CONTRACT INTERNAL TABULAR WORKFILE MAINTENANCE'.        GA1WPGM 
00099      05  WS-SAVED-PROVISIONS           PIC X(12).                 GA1WPGM 
00100      05  FILLER     REDEFINES    WS-SAVED-PROVISIONS.             GA1WPGM 
00101          10  WS-SAVED-RELATED          PIC X(06).                 GA1WPGM 
00102          10  WS-SAVED-ALTERNATE        PIC X(06).                 GA1WPGM 
00103                                                                   GA1WPGM 
00104                                                                   GA1WPGM 
00105      05  WS-CARRY-OVER-SLOT-NO         PIC S9(7)  COMP-3.         GA1WPGM 
00106                                                                   GA1WPGM 
00107                                                                   GA1WPGM 
00108  01  FILLER                      PIC X(22)  VALUE                 GA1WPGM 
00109                                       '*** RECORD LENGTHS ***'.   GA1WPGM 
00110  01  WS-RECORD-LENGTHS.                                           GA1WPGM 
00111      05 WS-IO-PARM-WRK-IRPV-TAB-LEN    PIC S9(4) COMP.            GA1WPGM 
00112      05 WS-IO-PARM-WRK-CDRS-LEN        PIC S9(4) COMP.            GA1WPGM 
00113      05 WS-COPY-LENGTH                 PIC S9(4) COMP.            GA1WPGM 
00114                                                                   GA1WPGM 
00115  01  COMMAREA-POINTER-AREA.                                       GA1WPGM 
00116      05  COMMAREA-PNTR-COMP            PIC S9(08)  COMP.          GA1WPGM 
00117      05  COMMAREA-PNTR  REDEFINES                                 GA1WPGM 
00118                COMMAREA-PNTR-COMP USAGE IS POINTER.               GA1WPGM 
00119                                                                   GA1WPGM 
00120  01  INTERNAL-POINTER-AREA.                                       GA1WPGM 
00121      05  INTERNAL-TAB-PNTR-COMP         PIC S9(08)  COMP.         GA1WPGM 
00122      05  INTERNAL-TAB-PNTR       REDEFINES                        GA1WPGM 
00123                INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.           GA1WPGM 
00124                                                                   GA1WPGM 
00125  01  FILLER                             PIC X(32)  VALUE          GA1WPGM 
00126                              '*** ALTERNATIVE WORKFILE KEY ***'.  GA1WPGM 
00127  01  WS-ALT-WORKFILE-KEYS.                                        GA1WPGM 
00128  COPY GCWRKKEY.                                                   GA1WPGM 
00129                                                                   GA1WPGM 
00130                                                                   GA1WPGM 
00131 ******************************************************************GA1WPGM 
00132 *    MAP COBOL SCREEN DSECTS                                      GA1WPGM 
00133 ******************************************************************GA1WPGM 
00134  01  WS-I-O-MAP-AREA                    PIC X(18)  VALUE          GA1WPGM 
00135                                          '***  MAP AREA  ***'.    GA1WPGM 
00136  COPY GA1WSETC.                                                   GA1WPGM 
00137                                                                   GA1WPGM 
00138 ******************************************************************GA1WPGM 
00139 *   THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR          GA1WPGM 
00140 *   ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1WPGM 
00141 *   HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1WPGM 
00142 *   FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1WPGM 
00143 *   REDEFINES.                                                    GA1WPGM 
00144 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1WPGM 
00145 *                                                                 GA1WPGM 
00146 *   THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1WPGM 
00147 *   FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1WPGM 
00148 *   MATCH THE MAP.                                                GA1WPGM 
00149 *                                                                 GA1WPGM 
00150 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1WPGM 
00151                                                                   GA1WPGM 
00152  01  FILLER     REDEFINES   GA1WI01I.                             GA1WPGM 
00153      05  FILLER                              PIC X(89).           GA1WPGM 
00154      05  CONTRACT-ID-LINE.                                        GA1WPGM 
00155          10  CONTRACT-PLAN-HEADING           PIC X(05).           GA1WPGM 
00156          10  CONTRACT-PLAN-CODE              PIC X(03).           GA1WPGM 
00157          10  CONTRACT-GROUP-HEADING          PIC X(06).           GA1WPGM 
00158          10  CONTRACT-GROUP-NO               PIC X(09).           GA1WPGM 
00159          10  CONTRACT-SECTION-HEADING        PIC X(06).           GA1WPGM 
00160          10  CONTRACT-SECTION-NO             PIC X(05).           GA1WPGM 
00161          10  CONTRACT-PKG-HEADING            PIC X(06).           GA1WPGM 
00162          10  CONTRACT-PKG-CODE               PIC X(03).           GA1WPGM 
00163          10  CONTRACT-LOB-HEADING            PIC X(06).           GA1WPGM 
00164          10  CONTRACT-LOB                    PIC X(01).           GA1WPGM 
00165          10  CONTRACT-PROV-CTL-HEADING       PIC X(06).           GA1WPGM 
00166          10  CONTRACT-PROV-CTL               PIC X(02).           GA1WPGM 
00167          10  CONTRACT-FAM-REL-HEADING        PIC X(05).           GA1WPGM 
00168          10  CONTRACT-FAM-REL-LVL            PIC X(02).           GA1WPGM 
00169          10  CONTRACT-EFF-DT-HEADING         PIC X(07).           GA1WPGM 
00170          10  CONTRACT-EFF-DATE               PIC X(06).           GA1WPGM 
00171          10  FILLER                          PIC X(01).           GA1WPGM 
00172      05  FILLER                              PIC X(74).           GA1WPGM 
00173      05  MAP-PROVISIONS-ROW           OCCURS 12 TIMES             GA1WPGM 
00174                                       INDEXED BY MAP-IDX1.        GA1WPGM 
00175          10  MAP-PROVISION-COL        OCCURS  3 TIMES             GA1WPGM 
00176                                       INDEXED BY MAP-IDX2.        GA1WPGM 
00177              15  MAP-ACTION-CODE-LEN       PIC S9(04) COMP SYNC.  GA1WPGM 
00178              15  MAP-ACTION-CODE-ATTR      PIC X(01).             GA1WPGM 
00179              15  MAP-ACTION-CODE           PIC X(01).             GA1WPGM 
00180              15  MAP-RELATED-LEN           PIC S9(04) COMP SYNC.  GA1WPGM 
00181              15  MAP-RELATED-ATTR          PIC X(01).             GA1WPGM 
00182              15  MAP-RELATED               PIC X(06).             GA1WPGM 
00183              15  MAP-ALTERNATE-LEN         PIC S9(04) COMP SYNC.  GA1WPGM 
00184              15  MAP-ALTERNATE-ATTR        PIC X(01).             GA1WPGM 
00185              15  MAP-ALTERNATE             PIC X(06).             GA1WPGM 
00186              15  FILLER                    PIC X(01).             GA1WPGM 
00187                                                                   GA1WPGM 
00188 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1WPGM 
00189 *   THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1WPGM 
00190 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1WPGM 
00191  01  WS-MAP-OCCURS-COUNTERS.                                      GA1WPGM 
00192      05  WS-MAP-ROW              PIC S9(03)  COMP-3 VALUE +12.    GA1WPGM 
00193      05  WS-MAP-COL              PIC S9(03)  COMP-3 VALUE +3.     GA1WPGM 
00194                                                                   GA1WPGM 
00195 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1WPGM 
00196                                                                   GA1WPGM 
00197  01  FILLER                      PIC X(21) VALUE                  GA1WPGM 
00198                                       '*** MESSAGE TABLE ***'.    GA1WPGM 
00199  01  FILLER.                                                      GA1WPGM 
00200      05  WS-MESSAGE-VALUES.                                       GA1WPGM 
00201                                                                   GA1WPGM 
00202 *----------------------------------------------------------------*GA1WPGM 
00203          10  WS-MESSAGE-ENTRY-001.                                GA1WPGM 
00204              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00205              15  WS-MESSAGE-TEXT-001.                             GA1WPGM 
00206                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00207                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00208                  20  FILLER          PIC X(3)  VALUE  '001'.      GA1WPGM 
00209                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00210                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00211                           '** INVALID REQUEST. THE PF KEY USED HASGA1WPGM 
00212 -                   ' NO MEANING TO THIS PROGRAM **'.             GA1WPGM 
00213              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00214                                                                   GA1WPGM 
00215 *----------------------------------------------------------------*GA1WPGM 
00216          10  WS-MESSAGE-ENTRY-002.                                GA1WPGM 
00217              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00218              15  WS-MESSAGE-TEXT-002.                             GA1WPGM 
00219                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00220                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00221                  20  FILLER          PIC X(3)  VALUE  '002'.      GA1WPGM 
00222                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00223                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00224                      '** INVALID ACTION CODE FOUND **'.           GA1WPGM 
00225              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00226                                                                   GA1WPGM 
00227 *----------------------------------------------------------------*GA1WPGM 
00228          10  WS-MESSAGE-ENTRY-003.                                GA1WPGM 
00229              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00230              15  WS-MESSAGE-TEXT-003.                             GA1WPGM 
00231                  20  FILLER          PIC X(4)  VALUE  'GA10'.     GA1WPGM 
00232                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00233                  20  FILLER          PIC X(3)  VALUE  '003'.      GA1WPGM 
00234                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00235                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00236                           '** ERROR READING ALL LEVEL TABULARS CONGA1WPGM 
00237 -                   'TACT SYSTEMS AREA **          '.             GA1WPGM 
00238              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00239 *----------------------------------------------------------------*GA1WPGM 
00240          10  WS-MESSAGE-ENTRY-004.                                GA1WPGM 
00241              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00242              15  WS-MESSAGE-TEXT-004.                             GA1WPGM 
00243                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00244                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00245                  20  FILLER          PIC X(3)  VALUE  '004'.      GA1WPGM 
00246                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00247                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00248                           '** PROGRAM ERROR IN 2040-DELETE, CONTACGA1WPGM 
00249 -                   'T SYSTEM AREA **              '.             GA1WPGM 
00250              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00251 *----------------------------------------------------------------*GA1WPGM 
00252          10  WS-MESSAGE-ENTRY-005.                                GA1WPGM 
00253              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00254              15  WS-MESSAGE-TEXT-005.                             GA1WPGM 
00255                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00256                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00257                  20  FILLER          PIC X(3)  VALUE  '005'.      GA1WPGM 
00258                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00259                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00260                           '** PROGRAM ERROR IN 2050-SAVE, CONTACT GA1WPGM 
00261 -                   'SYSTEM AREA **                '.             GA1WPGM 
00262              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00263 *----------------------------------------------------------------*GA1WPGM 
00264          10  WS-MESSAGE-ENTRY-006.                                GA1WPGM 
00265              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00266              15  WS-MESSAGE-TEXT-006.                             GA1WPGM 
00267                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00268                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00269                  20  FILLER          PIC X(3)  VALUE  '006'.      GA1WPGM 
00270                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00271                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00272                           '** REWRITE ERROR, INTERNAL TABULAR FILEGA1WPGM 
00273 -                   ', CONTACT SYSTEM AREA **      '.             GA1WPGM 
00274              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00275                                                                   GA1WPGM 
00276 *----------------------------------------------------------------*GA1WPGM 
00277          10  WS-MESSAGE-ENTRY-007.                                GA1WPGM 
00278              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00279              15  WS-MESSAGE-TEXT-007.                             GA1WPGM 
00280                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00281                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00282                  20  FILLER          PIC X(3)  VALUE  '007'.      GA1WPGM 
00283                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00284                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00285                           '** READ ERROR, INTERNAL TABULAR FILE, CGA1WPGM 
00286 -                   'ONTACT SYSTEM AREA **         '.             GA1WPGM 
00287              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00288 *----------------------------------------------------------------*GA1WPGM 
00289          10  WS-MESSAGE-ENTRY-008.                                GA1WPGM 
00290              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00291              15  WS-MESSAGE-TEXT-008.                             GA1WPGM 
00292                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00293                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00294                  20  FILLER          PIC X(3)  VALUE  '008'.      GA1WPGM 
00295                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00296                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00297               ' ** COMMAREA LENGTH IS INVALID **'.                GA1WPGM 
00298              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00299                                                                   GA1WPGM 
00300 *----------------------------------------------------------------*GA1WPGM 
00301          10  WS-MESSAGE-ENTRY-009.                                GA1WPGM 
00302              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00303              15  WS-MESSAGE-TEXT-009.                             GA1WPGM 
00304                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00305                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00306                  20  FILLER          PIC X(3)  VALUE  '009'.      GA1WPGM 
00307                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00308                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00309            '** INVALID EFFECTIVE DATE DISCOVERED **'.             GA1WPGM 
00310              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00311                                                                   GA1WPGM 
00312 *----------------------------------------------------------------*GA1WPGM 
00313          10  WS-MESSAGE-ENTRY-010.                                GA1WPGM 
00314              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00315              15  WS-MESSAGE-TEXT-010.                             GA1WPGM 
00316                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00317                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00318                  20  FILLER          PIC X(3)  VALUE  '010'.      GA1WPGM 
00319                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00320                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00321                           '** NO MORE ENTRIES TO DELETE **       *GA1WPGM 
00322 -                   '* PRESS PF3 TO EXIT **        '.             GA1WPGM 
00323              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00324                                                                   GA1WPGM 
00325 *----------------------------------------------------------------*GA1WPGM 
00326          10  WS-MESSAGE-ENTRY-011.                                GA1WPGM 
00327              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1WPGM 
00328              15  WS-MESSAGE-TEXT-011.                             GA1WPGM 
00329                  20  FILLER          PIC X(4)  VALUE  'GA1W'.     GA1WPGM 
00330                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1WPGM 
00331                  20  FILLER          PIC X(3)  VALUE  '011'.      GA1WPGM 
00332                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1WPGM 
00333                  20  FILLER          PIC X(70) VALUE              GA1WPGM 
00334                           '** NO MORE ENTRIES TO DISPLAY **      *GA1WPGM 
00335 -                   '* PRESS PF3 TO EXIT **        '.             GA1WPGM 
00336              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1WPGM 
00337                                                                   GA1WPGM 
00338 *----------------------------------------------------------------*GA1WPGM 
00339                                                                   GA1WPGM 
00340      05  WS-MESSAGE-TABLE            REDEFINES                    GA1WPGM 
00341          WS-MESSAGE-VALUES           OCCURS 011 TIMES             GA1WPGM 
00342                                      INDEXED BY WS-MESSAGE-INDEX. GA1WPGM 
00343          10  WS-MESSAGE-ENTRY.                                    GA1WPGM 
00344              15  FILLER              PIC X(02).                   GA1WPGM 
00345              15  WS-MESSAGE-TEXT     PIC X(79).                   GA1WPGM 
00346              15  FILLER              PIC X(02).                   GA1WPGM 
00347                                                                   GA1WPGM 
00348                                                                   GA1WPGM 
00349  01  FILLER                          PIC X(27)  VALUE             GA1WPGM 
00350                                    '*** GCPS RECORD LENGTHS ***'. GA1WPGM 
00351  01  FILLER.                                                      GA1WPGM 
00352      COPY GCCDRLEN.                                               GA1WPGM 
00353                                                                   GA1WPGM 
00354  01  FILLER                          PIC X(18)  VALUE             GA1WPGM 
00355                                            '*** ATTRIBUTES ***'.  GA1WPGM 
00356  COPY DFHBMSCA.                                                   GA1WPGM 
00357      02  DFHBMABF                    PIC X VALUE 'Z'.             GA1WPGM 
00358                                                                   GA1WPGM 
00359  01  FILLER                          PIC X(29)  VALUE             GA1WPGM 
00360                                 '*** ATTENTION IDENTIFIERS ***'.  GA1WPGM 
00361  COPY DFHAID.                                                     GA1WPGM 
00362                                                                   GA1WPGM 
00363  01  WS-END                          PIC X(58)  VALUE             GA1WPGM 
00364      '***  GA1WPGM WORKING-STORAGE ENDS HERE  ***'.               GA1WPGM 
00365                                                                   GA1WPGM 
00366 /                   L I N K A G E    S E C T I O N                GA1WPGM 
00367  LINKAGE SECTION.                                                 GA1WPGM 
00368                                                                   GA1WPGM 
00369  01  DFHCOMMAREA.                                                 GA1WPGM 
00370  COPY G2ALCKEC.                                                   GA1WPGM 
00371      05  WS-COMMAREA-CDRS-REC                                     GA1WPGM 
00372          REDEFINES COMMAREA-ALL-LEV-TAB-RECORD.                   GA1WPGM 
00373          10  FILLER                      PIC X(97).               GA1WPGM 
00374          10  WS-PRIM-IND                 PIC X(01).               GA1WPGM 
00375          10  WS-PRIM-SEQ-NO-X            PIC X(02).               GA1WPGM 
00376          10  WS-PRIM-SEQ-NO                                       GA1WPGM 
00377              REDEFINES WS-PRIM-SEQ-NO-X  PIC 9(02).               GA1WPGM 
00378          10  WS-ADDL-IND                 PIC X(01).               GA1WPGM 
00379          10  WS-ADDL-SEQ-NO-X            PIC X(02).               GA1WPGM 
00380          10  WS-ADDL-SEQ-NO                                       GA1WPGM 
00381              REDEFINES WS-ADDL-SEQ-NO-X  PIC 9(02).               GA1WPGM 
00382          10  FILLER                      PIC X(47).               GA1WPGM 
00383                                                                   GA1WPGM 
00384  COPY GACDACWA.                                                   GA1WPGM 
00385      05  GAS1UPD-PASSED-AREA.                                     GA1WPGM 
00386          07  LVL2-B-SW           PIC X.                           GA1WPGM 
00387          07  LVL2-F-SW           PIC X.                           GA1WPGM 
00388          07  LVL2-G-SW           PIC X.                           GA1WPGM 
00389          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1WPGM 
00390          07  FILLER              PIC X(9).                        GA1WPGM 
00391      05  DELADD-OPTION           PIC X(7).                        GA1WPGM 
00392                                                                   GA1WPGM 
00393 ******************************************************************GA1WPGM 
00394 *    I/O PARM, WORKFILE KEY, AND INTERNAL #IRPV TABULAR RECORD    GA1WPGM 
00395 ******************************************************************GA1WPGM 
00396  01  IO-PARM-INTERNAL-IRPV-RECORD.                                GA1WPGM 
00397  COPY GCIOPRM1.                                                   GA1WPGM 
00398  COPY GCWRKDCC.                                                   GA1WPGM 
00399  COPY GCTIRPVC.                                                   GA1WPGM 
00400                                                                   GA1WPGM 
00401 ****************************************************************  GA1WPGM 
00402 *    COPY OF THE #IRPV TABULAR VARIABLE AREA                      GA1WPGM 
00403 *    THIS AREA IS USED IN SORTING PROCESS.                        GA1WPGM 
00404 ****************************************************************  GA1WPGM 
00405  01  LK-COPY-IRPV-TAB-TABLE-AREA.                                 GA1WPGM 
00406      05  LK-COPY-IRPV-TAB-TABLE        OCCURS 647 TIMES           GA1WPGM 
00407                                        INDEXED BY COPY-IDX.       GA1WPGM 
00408          10  LK-COPY-RELATED-PROV            PIC X(06).           GA1WPGM 
00409          10  LK-COPY-ALTERNATE-PROV          PIC X(06).           GA1WPGM 
00410                                                                   GA1WPGM 
00411 ****************************************************************  GA1WPGM 
00412 *  IO PARM, WITH WORKFILE KEY, AND #CDRS TABULAR RECORD           GA1WPGM 
00413 ****************************************************************  GA1WPGM 
00414  01  IO-PARM-CDRS-TABULAR-RECORD.                                 GA1WPGM 
00415  COPY GCIOPRM2.                                                   GA1WPGM 
00416  COPY GCWRKDC2.                                                   GA1WPGM 
00417  COPY GCTCDRSC.                                                   GA1WPGM 
00418                                                                   GA1WPGM 
00419 /                    P R O C E D U R E    D I V I S I O N         GA1WPGM 
00420                                                                   GA1WPGM 
00421  PROCEDURE DIVISION.                                              GA1WPGM 
00422                                                                   GA1WPGM 
00423 ******************************************************************GA1WPGM 
00424 *                      M A I N L I N E                            GA1WPGM 
00425 *                                                                 GA1WPGM 
00426 *    THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1WPGM 
00427 *   TAKEN BY THE OPERATOR.                                        GA1WPGM 
00428 *   1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1WPGM 
00429 *      WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1WPGM 
00430 *      ADDITIONS FROM.                                            GA1WPGM 
00431 *   2. RECEIVE THE SCREEN.                                        GA1WPGM 
00432 *   3. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1WPGM 
00433 *      MENU.                                                      GA1WPGM 
00434 *   4. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1WPGM 
00435 *      LOGIC.                                                     GA1WPGM 
00436 *   5. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1WPGM 
00437 *      (RETURN) TO THE ADD PROGRAM (GA2WPGM).                     GA1WPGM 
00438 *   6. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1WPGM 
00439 *      (RETURN) TO THE PREVIOUS MENU.                             GA1WPGM 
00440 *   7. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1WPGM 
00441 *      KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1WPGM 
00442 *                                                                 GA1WPGM 
00443 ******************************************************************GA1WPGM 
00444  1000-MAIN-LINE SECTION.                                          GA1WPGM 
00445                                                                   GA1WPGM 
00446      MOVE '1000'  TO  WS-PARA-ID.                                 GA1WPGM 
00447                                                                   GA1WPGM 
00448      IF EIBAID  =  DFHCLEAR                                       GA1WPGM 
00449         EXEC CICS SEND                                            GA1WPGM 
00450              FROM (WS-ONE-LOW) ERASE                              GA1WPGM 
00451         END-EXEC                                                  GA1WPGM 
00452         EXEC CICS                                                 GA1WPGM 
00453              RETURN                                               GA1WPGM 
00454         END-EXEC.                                                 GA1WPGM 
00455                                                                   GA1WPGM 
00456      IF EIBTRNID  NOT =  'GA1W'                                   GA1WPGM 
00457         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1WPGM 
00458         GO TO 1099-RETURN.                                        GA1WPGM 
00459                                                                   GA1WPGM 
00460      EXEC CICS RECEIVE                                            GA1WPGM 
00461           MAP    ('GA1WI01')                                      GA1WPGM 
00462           MAPSET ('GA1WSET')                                      GA1WPGM 
00463           INTO   (GA1WI01I)                                       GA1WPGM 
00464      END-EXEC.                                                    GA1WPGM 
00465                                                                   GA1WPGM 
00466      IF SCRNIDNI  NOT =  '0A1W00'                                 GA1WPGM 
00467         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1WPGM 
00468                                                                   GA1WPGM 
00469      IF EIBAID  =  DFHENTER                                       GA1WPGM 
00470         PERFORM 2000-DELETE-PROCESSING                            GA1WPGM 
00471         GO TO 1099-RETURN.                                        GA1WPGM 
00472                                                                   GA1WPGM 
00473      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1WPGM 
00474         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1WPGM 
00475                                                                   GA1WPGM 
00476      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1WPGM 
00477         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1WPGM 
00478                                                                   GA1WPGM 
00479      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1WPGM 
00480      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1WPGM 
00481      SET WS-MESSAGE-INDEX TO +01.                                 GA1WPGM 
00482      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1WPGM 
00483      EXEC CICS SEND                                               GA1WPGM 
00484           MAP    ('GA1WI01')                                      GA1WPGM 
00485           MAPSET ('GA1WSET') DATAONLY                             GA1WPGM 
00486           FROM   (GA1WI01O)  CURSOR                               GA1WPGM 
00487      END-EXEC.                                                    GA1WPGM 
00488      GO TO 1099-RETURN.                                           GA1WPGM 
00489                                                                   GA1WPGM 
00490  1000-EXIT.                                                       GA1WPGM 
00491      EXIT.                                                        GA1WPGM 
00492                                                                   GA1WPGM 
00493                                                                   GA1WPGM 
00494  1099-RETURN.                                                     GA1WPGM 
00495                                                                   GA1WPGM 
00496      EXEC CICS RETURN                                             GA1WPGM 
00497                TRANSID  ('GA1W')                                  GA1WPGM 
00498                COMMAREA (DFHCOMMAREA)                             GA1WPGM 
00499                LENGTH   (EIBCALEN)                                GA1WPGM 
00500                END-EXEC.                                          GA1WPGM 
00501                                                                   GA1WPGM 
00502      GOBACK.                                                      GA1WPGM 
00503                                                                   GA1WPGM 
00504  1099-EXIT.                                                       GA1WPGM 
00505      EXIT.                                                        GA1WPGM 
00506                                                                   GA1WPGM 
00507 ******************************************************************GA1WPGM 
00508 *               D E L E T E   P R O C E S S I N G                 GA1WPGM 
00509 *                                                                 GA1WPGM 
00510 *   WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1WPGM 
00511 *  1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1WPGM 
00512 *     VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1WPGM 
00513 *  2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1WPGM 
00514 *     (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1WPGM 
00515 *     BACK INTO THE RECORD THAT WE READ.)                         GA1WPGM 
00516 *  3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1WPGM 
00517 *     THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1WPGM 
00518 *     ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1WPGM 
00519 *  4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1WPGM 
00520 *     ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1WPGM 
00521 *  5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1WPGM 
00522 *     POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1WPGM 
00523 *     ENTRY.                                                      GA1WPGM 
00524 *  6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1WPGM 
00525 *     MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1WPGM 
00526 *     RECORD.                                                     GA1WPGM 
00527 *  7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1WPGM 
00528 *     NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1WPGM 
00529 *     DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1WPGM 
00530 *  8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1WPGM 
00531 *     ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1WPGM 
00532 *     SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1WPGM 
00533 *     BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1WPGM 
00534 *                                                                 GA1WPGM 
00535 ******************************************************************GA1WPGM 
00536  2000-DELETE-PROCESSING SECTION.                                  GA1WPGM 
00537                                                                   GA1WPGM 
00538      MOVE '2000'  TO  WS-PARA-ID.                                 GA1WPGM 
00539      MOVE 'N'     TO  WS-ERROR-SW.                                GA1WPGM 
00540      MOVE ZERO    TO  WS-DELETE-COUNT.                            GA1WPGM 
00541      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1WPGM 
00542                                                                   GA1WPGM 
00543                                                                   GA1WPGM 
00544  2010-VALIDATE-ACT-CODE.                                          GA1WPGM 
00545                                                                   GA1WPGM 
00546      MOVE '2010'  TO  WS-PARA-ID.                                 GA1WPGM 
00547                                                                   GA1WPGM 
00548      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1WPGM 
00549         ADD 1  TO  WS-DELETE-COUNT.                               GA1WPGM 
00550      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1WPGM 
00551          OR = SPACE OR =  LOW-VALUES                              GA1WPGM 
00552         MOVE DFHBMUNF                                             GA1WPGM 
00553           TO MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)            GA1WPGM 
00554         MOVE DFHBMASF                                             GA1WPGM 
00555           TO MAP-RELATED-ATTR     (MAP-IDX1, MAP-IDX2)            GA1WPGM 
00556              MAP-ALTERNATE-ATTR   (MAP-IDX1, MAP-IDX2)            GA1WPGM 
00557      ELSE                                                         GA1WPGM 
00558         MOVE DFHBMUBF                                             GA1WPGM 
00559           TO MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)            GA1WPGM 
00560         MOVE DFHBMABF                                             GA1WPGM 
00561           TO MAP-RELATED-ATTR     (MAP-IDX1, MAP-IDX2)            GA1WPGM 
00562              MAP-ALTERNATE-ATTR  (MAP-IDX1, MAP-IDX2)             GA1WPGM 
00563         IF WS-ERROR-SW  NOT =  'Y'                                GA1WPGM 
00564            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1WPGM 
00565            MOVE 'Y' TO  WS-ERROR-SW.                              GA1WPGM 
00566                                                                   GA1WPGM 
00567      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1WPGM 
00568         SET MAP-IDX1   UP BY  1                                   GA1WPGM 
00569      ELSE                                                         GA1WPGM 
00570         IF MAP-IDX2  <  WS-MAP-COL                                GA1WPGM 
00571            SET MAP-IDX1  TO  1                                    GA1WPGM 
00572            SET MAP-IDX2  UP BY  1                                 GA1WPGM 
00573         ELSE                                                      GA1WPGM 
00574            GO TO 2020-DONE-VALIDATE-A-C.                          GA1WPGM 
00575                                                                   GA1WPGM 
00576      IF MAP-RELATED (MAP-IDX1, MAP-IDX2)  NOT =  LOW-VALUES       GA1WPGM 
00577         GO TO 2010-VALIDATE-ACT-CODE.                             GA1WPGM 
00578                                                                   GA1WPGM 
00579                                                                   GA1WPGM 
00580                                                                   GA1WPGM 
00581  2020-DONE-VALIDATE-A-C.                                          GA1WPGM 
00582                                                                   GA1WPGM 
00583      MOVE '2020'  TO  WS-PARA-ID.                                 GA1WPGM 
00584      SET MAP-IDX1 TO  1.                                          GA1WPGM 
00585                                                                   GA1WPGM 
00586      IF WS-ERROR-SW  =  'Y'                                       GA1WPGM 
00587         SET WS-MESSAGE-INDEX TO +02                               GA1WPGM 
00588         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
00589         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1WPGM 
00590            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO, IDLINEO,    GA1WPGM 
00591            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO, INCEXCO     GA1WPGM 
00592         MOVE '2100'  TO  WS-PARA-ID                               GA1WPGM 
00593         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1WPGM 
00594            VARYING MAP-IDX2 FROM  1  BY  1                        GA1WPGM 
00595              UNTIL MAP-IDX2  >  WS-MAP-COL                        GA1WPGM 
00596            AFTER   MAP-IDX1 FROM  1  BY  1                        GA1WPGM 
00597              UNTIL MAP-IDX1  >  WS-MAP-ROW                        GA1WPGM 
00598         EXEC CICS SEND                                            GA1WPGM 
00599              MAP    ('GA1WI01')                                   GA1WPGM 
00600              MAPSET ('GA1WSET') DATAONLY                          GA1WPGM 
00601              FROM   (GA1WI01O)  CURSOR                            GA1WPGM 
00602         END-EXEC                                                  GA1WPGM 
00603         GO TO 2099-EXIT.                                          GA1WPGM 
00604                                                                   GA1WPGM 
00605      COMPUTE WS-IO-PARM-WRK-IRPV-TAB-LEN                          GA1WPGM 
00606        EQUAL GC-GCIOPARM-LEN                                      GA1WPGM 
00607              + GC-WORKFILE-KEY-LEN                                GA1WPGM 
00608              + GC-GCTABULR-IRPV-FIXED-LEN                         GA1WPGM 
00609              + (GC-GCTABULR-IRPV-VARY-MAX-OCUR                    GA1WPGM 
00610                 * GC-GCTABULR-IRPV-VARY-LEN).                     GA1WPGM 
00611                                                                   GA1WPGM 
00612      EXEC CICS GETMAIN                                            GA1WPGM 
00613           SET     (ADDRESS OF IO-PARM-INTERNAL-IRPV-RECORD)       GA1WPGM 
00614           INITIMG (WS-HEX-00)                                     GA1WPGM 
00615           LENGTH  (WS-IO-PARM-WRK-IRPV-TAB-LEN)                   GA1WPGM 
00616      END-EXEC.                                                    GA1WPGM 
00617                                                                   GA1WPGM 
00618      MOVE SPACES                TO GCIO-WORKFILE-KEY.             GA1WPGM 
00619      MOVE  'C'                  TO GCIO-WRK-STATUS-CODE.          GA1WPGM 
00620      MOVE  'C3'                 TO GCIO-WRK-RECORD-TYPE.          GA1WPGM 
00621      MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE.            GA1WPGM 
00622      MOVE CONTRACT-GROUP-NO     TO GCIO-WRK-GROUP-NUM.            GA1WPGM 
00623      MOVE CONTRACT-SECTION-NO   TO GCIO-WRK-SECTION-NUM.          GA1WPGM 
00624      MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE.             GA1WPGM 
00625      MOVE CONTRACT-LOB          TO GCIO-WRK-LINE-OF-BUS.          GA1WPGM 
00626      MOVE CONTRACT-PROV-CTL     TO GCIO-WRK-PROVIDER-CONTROL.     GA1WPGM 
00627      MOVE CONTRACT-FAM-REL-LVL  TO GCIO-WRK-FAMILY-RELATION-LVL.  GA1WPGM 
00628      MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN.            GA1WPGM 
00629                                                                   GA1WPGM 
00630      MOVE 'GCPSWORK'         TO  GCIO-FILE-DDNAME.                GA1WPGM 
00631      MOVE 1                  TO  GCIO-IO-AREA-TO-USE.             GA1WPGM 
00632      MOVE ALTABIDI           TO  GCIO-WRK-PROVISION-ID.           GA1WPGM 
00633      MOVE ALTBSLTI           TO  GCIO-WRK-PROVISION-SLOT-NO.      GA1WPGM 
00634      MOVE INTABIDI           TO  GCIO-WRK-TAB-PROVISION-ID.       GA1WPGM 
00635      MOVE INTBSLTI           TO  GCIO-WRK-TAB-PROV-SLOT-NO.       GA1WPGM 
00636      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1WPGM 
00637                                                                   GA1WPGM 
00638      IF WS-DELETE-COUNT  =  ZERO                                  GA1WPGM 
00639         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1WPGM 
00640                                                                   GA1WPGM 
00641 ******************************************************************GA1WPGM 
00642 *    WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.         GA1WPGM 
00643 ******************************************************************GA1WPGM 
00644                                                                   GA1WPGM 
00645      MOVE GC-GCTABULR-IRPV-VARY-MAX-OCUR                          GA1WPGM 
00646        TO GXJ-ENTRY-COUNT.                                        GA1WPGM 
00647                                                                   GA1WPGM 
00648      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1WPGM 
00649                                                                   GA1WPGM 
00650      EXEC CICS LINK                                               GA1WPGM 
00651           PROGRAM  ('GCIOPGM')                                    GA1WPGM 
00652           COMMAREA (IO-PARM-INTERNAL-IRPV-RECORD)                 GA1WPGM 
00653           LENGTH   (WS-IO-PARM-WRK-IRPV-TAB-LEN)                  GA1WPGM 
00654      END-EXEC.                                                    GA1WPGM 
00655                                                                   GA1WPGM 
00656      IF NOT GCIO-GOOD-RETURN                                      GA1WPGM 
00657         SET WS-MESSAGE-INDEX TO +03                               GA1WPGM 
00658         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
00659         MOVE '1W01'          TO WS-ABEND-CODE                     GA1WPGM 
00660         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1WPGM 
00661                                                                   GA1WPGM 
00662      COMPUTE WS-COPY-LENGTH  =                                    GA1WPGM 
00663              GXJ-ENTRY-COUNT  *  GC-GCTABULR-IRPV-VARY-LEN.       GA1WPGM 
00664                                                                   GA1WPGM 
00665      EXEC CICS GETMAIN                                            GA1WPGM 
00666         SET      (ADDRESS OF LK-COPY-IRPV-TAB-TABLE-AREA)         GA1WPGM 
00667         LENGTH   (WS-COPY-LENGTH)                                 GA1WPGM 
00668         INITIMG  (WS-HEX-00)                                      GA1WPGM 
00669      END-EXEC.                                                    GA1WPGM 
00670                                                                   GA1WPGM 
00671      MOVE GXJ-ENTRY-COUNT  TO  GXJ-ENTRY-COUNT.                   GA1WPGM 
00672      SET COPY-IDX,  GXJ-INDEX  TO  1.                             GA1WPGM 
00673                                                                   GA1WPGM 
00674                                                                   GA1WPGM 
00675  2030-MAKE-A-COPY-OF-RECORD.                                      GA1WPGM 
00676                                                                   GA1WPGM 
00677      MOVE '2030'  TO  WS-PARA-ID.                                 GA1WPGM 
00678      IF GXJ-INDEX  NOT >  GXJ-ENTRY-COUNT                         GA1WPGM 
00679         MOVE GXJ-ENTRY (GXJ-INDEX)                                GA1WPGM 
00680           TO LK-COPY-IRPV-TAB-TABLE (COPY-IDX)                    GA1WPGM 
00681         SET COPY-IDX,  GXJ-INDEX  UP BY  1                        GA1WPGM 
00682         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1WPGM 
00683                                                                   GA1WPGM 
00684      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GXJ-INDEX  TO  1.         GA1WPGM 
00685      MOVE '2040'  TO  WS-PARA-ID.                                 GA1WPGM 
00686                                                                   GA1WPGM 
00687                                                                   GA1WPGM 
00688  2040-DELETE-MARKED-ENTRIES.                                      GA1WPGM 
00689      IF  MAP-RELATED    (MAP-IDX1, MAP-IDX2)   =  LOW-VALUES      GA1WPGM 
00690       OR MAP-ALTERNATE  (MAP-IDX1, MAP-IDX2)   =  LOW-VALUES      GA1WPGM 
00691          GO TO 2060-SAVE-REST-OF-COPY.                            GA1WPGM 
00692                                                                   GA1WPGM 
00693                                                                   GA1WPGM 
00694                                                                   GA1WPGM 
00695      IF MAP-RELATED    (MAP-IDX1, MAP-IDX2)   >                   GA1WPGM 
00696                                LK-COPY-RELATED-PROV   (COPY-IDX)  GA1WPGM 
00697       OR MAP-ALTERNATE (MAP-IDX1, MAP-IDX2)   >                   GA1WPGM 
00698                                LK-COPY-ALTERNATE-PROV (COPY-IDX)  GA1WPGM 
00699          GO TO 2050-SAVE-COPIED-ENTRY                             GA1WPGM 
00700      ELSE                                                         GA1WPGM 
00701      IF  MAP-RELATED   (MAP-IDX1, MAP-IDX2)  <                    GA1WPGM 
00702                                LK-COPY-RELATED-PROV    (COPY-IDX) GA1WPGM 
00703       OR MAP-ALTERNATE (MAP-IDX1, MAP-IDX2)  <                    GA1WPGM 
00704                                 LK-COPY-ALTERNATE-PROV (COPY-IDX) GA1WPGM 
00705         SET WS-MESSAGE-INDEX TO +04                               GA1WPGM 
00706         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
00707         MOVE '1W02'          TO WS-ABEND-CODE                     GA1WPGM 
00708         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1WPGM 
00709                                                                   GA1WPGM 
00710                                                                   GA1WPGM 
00711                                                                   GA1WPGM 
00712      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1WPGM 
00713         IF MAP-IDX1   <  WS-MAP-ROW                               GA1WPGM 
00714            SET MAP-IDX1   UP BY  1                                GA1WPGM 
00715            GO TO 2050-SAVE-COPIED-ENTRY                           GA1WPGM 
00716         ELSE                                                      GA1WPGM 
00717            IF MAP-IDX2  <  WS-MAP-COL                             GA1WPGM 
00718               SET MAP-IDX1  TO  1                                 GA1WPGM 
00719               SET MAP-IDX2  UP BY  1                              GA1WPGM 
00720               GO TO 2050-SAVE-COPIED-ENTRY                        GA1WPGM 
00721            ELSE                                                   GA1WPGM 
00722               GO TO 2060-SAVE-REST-OF-COPY.                       GA1WPGM 
00723                                                                   GA1WPGM 
00724      SET COPY-IDX  UP BY  1.                                      GA1WPGM 
00725      IF COPY-IDX  NOT <  GXJ-ENTRY-COUNT                          GA1WPGM 
00726         MOVE LK-COPY-IRPV-TAB-TABLE (COPY-IDX)                    GA1WPGM 
00727           TO GXJ-ENTRY (GXJ-INDEX)                                GA1WPGM 
00728         SET  GXJ-ENTRY-COUNT  TO  GXJ-INDEX                       GA1WPGM 
00729         MOVE GXJ-ENTRY-COUNT  TO  GXJ-ENTRY-COUNT                 GA1WPGM 
00730         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1WPGM 
00731                                                                   GA1WPGM 
00732      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1WPGM 
00733         SET MAP-IDX1   UP BY  1                                   GA1WPGM 
00734         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1WPGM 
00735                                                                   GA1WPGM 
00736      IF MAP-IDX2  <  WS-MAP-COL                                   GA1WPGM 
00737         SET MAP-IDX1  TO  1                                       GA1WPGM 
00738         SET MAP-IDX2  UP BY  1                                    GA1WPGM 
00739         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1WPGM 
00740      ELSE                                                         GA1WPGM 
00741         GO TO 2060-SAVE-REST-OF-COPY.                             GA1WPGM 
00742                                                                   GA1WPGM 
00743                                                                   GA1WPGM 
00744                                                                   GA1WPGM 
00745  2050-SAVE-COPIED-ENTRY.                                          GA1WPGM 
00746                                                                   GA1WPGM 
00747      MOVE '2050'  TO  WS-PARA-ID.                                 GA1WPGM 
00748      MOVE LK-COPY-IRPV-TAB-TABLE (COPY-IDX)                       GA1WPGM 
00749        TO  GXJ-ENTRY (GXJ-INDEX).                                 GA1WPGM 
00750                                                                   GA1WPGM 
00751      SET GXJ-INDEX  UP BY  1.                                     GA1WPGM 
00752      IF COPY-IDX  <  GXJ-ENTRY-COUNT                              GA1WPGM 
00753         SET COPY-IDX  UP BY  1                                    GA1WPGM 
00754         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1WPGM 
00755      ELSE                                                         GA1WPGM 
00756 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1WPGM 
00757 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1WPGM 
00758 ***      THE TABLE OF ENTRIES.                                    GA1WPGM 
00759         SET WS-MESSAGE-INDEX TO +05                               GA1WPGM 
00760         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
00761         MOVE '1W03'  TO  WS-ABEND-CODE                            GA1WPGM 
00762         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1WPGM 
00763                                                                   GA1WPGM 
00764                                                                   GA1WPGM 
00765                                                                   GA1WPGM 
00766  2060-SAVE-REST-OF-COPY.                                          GA1WPGM 
00767                                                                   GA1WPGM 
00768      MOVE '2060'  TO  WS-PARA-ID.                                 GA1WPGM 
00769      MOVE LK-COPY-IRPV-TAB-TABLE (COPY-IDX)                       GA1WPGM 
00770        TO GXJ-ENTRY (GXJ-INDEX).                                  GA1WPGM 
00771                                                                   GA1WPGM 
00772      SET GXJ-INDEX  UP BY  1.                                     GA1WPGM 
00773      IF COPY-IDX  <  GXJ-ENTRY-COUNT                              GA1WPGM 
00774         SET COPY-IDX  UP BY  1                                    GA1WPGM 
00775         GO TO 2060-SAVE-REST-OF-COPY.                             GA1WPGM 
00776                                                                   GA1WPGM 
00777      SET GXJ-INDEX  DOWN BY  1.                                   GA1WPGM 
00778      SET GXJ-ENTRY-COUNT  TO  GXJ-INDEX.                          GA1WPGM 
00779      MOVE GXJ-ENTRY-COUNT TO  GXJ-ENTRY-COUNT.                    GA1WPGM 
00780                                                                   GA1WPGM 
00781                                                                   GA1WPGM 
00782                                                                   GA1WPGM 
00783  2070-UPDATE-MODIFIED-REC.                                        GA1WPGM 
00784                                                                   GA1WPGM 
00785      MOVE '2070'  TO  WS-PARA-ID.                                 GA1WPGM 
00786      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1WPGM 
00787      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1WPGM 
00788                                                                   GA1WPGM 
00789      COMPUTE  GCIO-RECORD-LENGTH           =                      GA1WPGM 
00790               GC-WORKFILE-KEY-LEN          +                      GA1WPGM 
00791               GC-GCTABULR-IRPV-FIXED-LEN   +                      GA1WPGM 
00792              (GXJ-ENTRY-COUNT  *  GC-GCTABULR-IRPV-VARY-LEN).     GA1WPGM 
00793                                                                   GA1WPGM 
00794      COMPUTE  WS-IO-PARM-WRK-IRPV-TAB-LEN     =                   GA1WPGM 
00795               GC-GCIOPARM-LEN                 +                   GA1WPGM 
00796               GCIO-RECORD-LENGTH.                                 GA1WPGM 
00797                                                                   GA1WPGM 
00798      EXEC CICS LINK                                               GA1WPGM 
00799           PROGRAM  ('GCIOPGM')                                    GA1WPGM 
00800           COMMAREA (IO-PARM-INTERNAL-IRPV-RECORD)                 GA1WPGM 
00801           LENGTH   (WS-IO-PARM-WRK-IRPV-TAB-LEN)                  GA1WPGM 
00802      END-EXEC.                                                    GA1WPGM 
00803                                                                   GA1WPGM 
00804      IF GCIO-GOOD-RETURN                                          GA1WPGM 
00805         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1WPGM 
00806                                                                   GA1WPGM 
00807      SET WS-MESSAGE-INDEX TO +06.                                 GA1WPGM 
00808      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1WPGM 
00809      MOVE '1W04'          TO WS-ABEND-CODE.                       GA1WPGM 
00810      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1WPGM 
00811                                                                   GA1WPGM 
00812                                                                   GA1WPGM 
00813                                                                   GA1WPGM 
00814  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1WPGM 
00815                                                                   GA1WPGM 
00816      MOVE  '2080'  TO  WS-PARA-ID.                                GA1WPGM 
00817      MOVE GC-GCTABULR-IRPV-VARY-MAX-OCUR                          GA1WPGM 
00818        TO GXJ-ENTRY-COUNT.                                        GA1WPGM 
00819                                                                   GA1WPGM 
00820      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1WPGM 
00821                                                                   GA1WPGM 
00822      EXEC CICS LINK                                               GA1WPGM 
00823           PROGRAM  ('GCIOPGM')                                    GA1WPGM 
00824           COMMAREA (IO-PARM-INTERNAL-IRPV-RECORD)                 GA1WPGM 
00825           LENGTH   (WS-IO-PARM-WRK-IRPV-TAB-LEN)                  GA1WPGM 
00826      END-EXEC.                                                    GA1WPGM 
00827                                                                   GA1WPGM 
00828      IF GCIO-GOOD-RETURN                                          GA1WPGM 
00829         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1WPGM 
00830                                                                   GA1WPGM 
00831      SET WS-MESSAGE-INDEX TO +07.                                 GA1WPGM 
00832      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1WPGM 
00833      MOVE '1W05'          TO WS-ABEND-CODE.                       GA1WPGM 
00834      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1WPGM 
00835                                                                   GA1WPGM 
00836                                                                   GA1WPGM 
00837                                                                   GA1WPGM 
00838  2090-BUILD-NEXT-DISPLAY.                                         GA1WPGM 
00839                                                                   GA1WPGM 
00840      MOVE  '2090'   TO  WS-PARA-ID.                               GA1WPGM 
00841      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1WPGM 
00842      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1WPGM 
00843      SET GXJ-INDEX  TO  1.                                        GA1WPGM 
00844                                                                   GA1WPGM 
00845      IF MAP-RELATED (MAP-IDX1, MAP-IDX2)   =   LOW-VALUES         GA1WPGM 
00846         MOVE GXJ-ENTRY (GXJ-INDEX)  TO  WS-SAVED-PROVISIONS       GA1WPGM 
00847      ELSE                                                         GA1WPGM 
00848      MOVE MAP-RELATED   (MAP-IDX1, MAP-IDX2)                      GA1WPGM 
00849        TO WS-SAVED-RELATED                                        GA1WPGM 
00850      MOVE MAP-ALTERNATE (MAP-IDX1, MAP-IDX2)                      GA1WPGM 
00851        TO WS-SAVED-ALTERNATE.                                     GA1WPGM 
00852                                                                   GA1WPGM 
00853      PERFORM 4500-FILL-THE-SCREEN.                                GA1WPGM 
00854      EXEC CICS SEND                                               GA1WPGM 
00855           MAP    ('GA1WI01')                                      GA1WPGM 
00856           MAPSET ('GA1WSET') ERASE                                GA1WPGM 
00857           FROM   (GA1WI01O)                                       GA1WPGM 
00858      END-EXEC.                                                    GA1WPGM 
00859                                                                   GA1WPGM 
00860  2099-EXIT.                                                       GA1WPGM 
00861      EXIT.                                                        GA1WPGM 
00862                                                                   GA1WPGM 
00863  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1WPGM 
00864                                                                   GA1WPGM 
00865      MOVE '2100'     TO  WS-PARA-ID.                              GA1WPGM 
00866      MOVE LOW-VALUES TO MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),     GA1WPGM 
00867                         MAP-RELATED   (MAP-IDX1, MAP-IDX2),       GA1WPGM 
00868                         MAP-ALTERNATE (MAP-IDX1, MAP-IDX2).       GA1WPGM 
00869                                                                   GA1WPGM 
00870  2199-EXIT.                                                       GA1WPGM 
00871      EXIT.                                                        GA1WPGM 
00872 /*****************************************************************GA1WPGM 
00873 *           X C T L   T O   A D D   S C R E E N                   GA1WPGM 
00874 *                                                                 GA1WPGM 
00875 *  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO   GA1WPGM 
00876 *  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1WPGM 
00877 *  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1WPGM 
00878 *  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1WPGM 
00879 *  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1WPGM 
00880 *  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1WPGM 
00881 ******************************************************************GA1WPGM 
00882  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1WPGM 
00883                                                                   GA1WPGM 
00884      MOVE '3000'     TO  WS-PARA-ID.                              GA1WPGM 
00885      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1WPGM 
00886      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1WPGM 
00887      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1WPGM 
00888      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1WPGM 
00889      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1WPGM 
00890      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1WPGM 
00891      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1WPGM 
00892      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1WPGM 
00893      MOVE INCEXCI    TO GCA-I-E-INDC.                             GA1WPGM 
00894                                                                   GA1WPGM 
00895      EXEC CICS XCTL                                               GA1WPGM 
00896           PROGRAM  ('GA2WPGM')                                    GA1WPGM 
00897           COMMAREA (DFHCOMMAREA)                                  GA1WPGM 
00898           LENGTH   (LENGTH OF DFHCOMMAREA)                        GA1WPGM 
00899      END-EXEC.                                                    GA1WPGM 
00900                                                                   GA1WPGM 
00901  3099-EXIT.                                                       GA1WPGM 
00902      EXIT.                                                        GA1WPGM 
00903 /**************************************************************** GA1WPGM 
00904 *           D I S P L A Y   F I R S T   S C R E E N               GA1WPGM 
00905 *                                                                 GA1WPGM 
00906 *  THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU OR  GA1WPGM 
00907 *  THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1WPGM 
00908 *  ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1WPGM 
00909 *  (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1WPGM 
00910 *  THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1WPGM 
00911 *  THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN,   GA1WPGM 
00912 *  (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1WPGM 
00913 *  SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1WPGM 
00914 *  WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1WPGM 
00915 *  DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1WPGM 
00916 *  AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1WPGM 
00917 *  DETERMINATION OF APPROPRIATE ACTION.                           GA1WPGM 
00918 ******************************************************************GA1WPGM 
00919  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1WPGM 
00920                                                                   GA1WPGM 
00921      MOVE '4000'     TO WS-PARA-ID.                               GA1WPGM 
00922      MOVE LOW-VALUES TO GA1WI01I.                                 GA1WPGM 
00923                                                                   GA1WPGM 
00924      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA1WPGM 
00925         SET WS-MESSAGE-INDEX TO +08                               GA1WPGM 
00926         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
00927         MOVE '1W06'          TO WS-ABEND-CODE                     GA1WPGM 
00928         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1WPGM 
00929                                                                   GA1WPGM 
00930      SET ADDRESS OF IO-PARM-INTERNAL-IRPV-RECORD                  GA1WPGM 
00931       TO GCA-RECORD-POINTER.                                      GA1WPGM 
00932                                                                   GA1WPGM 
00933      MOVE GCA-ALL-LEVEL-TAB-ID         TO ALTABIDO.               GA1WPGM 
00934      MOVE GCA-ALL-LEVEL-TAB-SLOT       TO ALTBSLTO.               GA1WPGM 
00935      MOVE GCA-INTERNAL-TAB-ID          TO INTABIDO.               GA1WPGM 
00936      MOVE GCA-INTERNAL-TAB-SLOT        TO INTBSLTO.               GA1WPGM 
00937      MOVE GCA-ADD-DEL-IND              TO ADDELINO.               GA1WPGM 
00938      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO ALTBFNCO.               GA1WPGM 
00939      MOVE GCA-OCCURS-ENTRY-COUNTER     TO OENTCTRO.               GA1WPGM 
00940      MOVE GCA-FROM-MENU-ID             TO FRMNUIDO.               GA1WPGM 
00941      MOVE GXJ-INCLUDE-EXCLUDE-IND      TO GCA-I-E-INDC.           GA1WPGM 
00942      MOVE GCA-I-E-INDC                 TO INCEXCO.                GA1WPGM 
00943      MOVE WS-DEL-REQUEST               TO FUNCLITO.               GA1WPGM 
00944                                                                   GA1WPGM 
00945 *-- MOVE TITLE LINE                                               GA1WPGM 
00946 *--                                                               GA1WPGM 
00947      MOVE WS-CONTRACT-TITLE-LINE     TO TTLELNEO.                 GA1WPGM 
00948      MOVE 'PLN: '                    TO CONTRACT-PLAN-HEADING.    GA1WPGM 
00949      MOVE GCA-PLAN-CODE              TO CONTRACT-PLAN-CODE.       GA1WPGM 
00950      MOVE ' GRP: '                   TO CONTRACT-GROUP-HEADING.   GA1WPGM 
00951      MOVE GCA-GROUP-NUM              TO CONTRACT-GROUP-NO.        GA1WPGM 
00952      MOVE ' SEC: '                   TO CONTRACT-SECTION-HEADING. GA1WPGM 
00953      MOVE GCA-SECTION-NUM            TO CONTRACT-SECTION-NO.      GA1WPGM 
00954      MOVE ' PKG: '                   TO CONTRACT-PKG-HEADING.     GA1WPGM 
00955      MOVE GCA-PKG-CODE               TO CONTRACT-PKG-CODE.        GA1WPGM 
00956      MOVE ' LOB: '                   TO CONTRACT-LOB-HEADING.     GA1WPGM 
00957      MOVE GCA-L-O-B                  TO CONTRACT-LOB.             GA1WPGM 
00958      MOVE ' PRV: '                   TO CONTRACT-PROV-CTL-HEADING.GA1WPGM 
00959      MOVE GCA-PROV-CTL               TO CONTRACT-PROV-CTL.        GA1WPGM 
00960      MOVE ' FR: '                    TO CONTRACT-FAM-REL-HEADING. GA1WPGM 
00961      MOVE GCA-FAM-REL-LVL            TO CONTRACT-FAM-REL-LVL.     GA1WPGM 
00962      MOVE ' EFDT: '                  TO CONTRACT-EFF-DT-HEADING.  GA1WPGM 
00963      MOVE GCA-EFFECTIVE-DATE         TO CONTRACT-EFF-DATE.        GA1WPGM 
00964                                                                   GA1WPGM 
00965      SET  GXJ-INDEX                  TO  1.                       GA1WPGM 
00966      MOVE GXJ-ENTRY (GXJ-INDEX)      TO WS-SAVED-PROVISIONS.      GA1WPGM 
00967                                                                   GA1WPGM 
00968      PERFORM 4500-FILL-THE-SCREEN.                                GA1WPGM 
00969                                                                   GA1WPGM 
00970      EXEC CICS SEND                                               GA1WPGM 
00971           MAP    ('GA1WI01')                                      GA1WPGM 
00972           MAPSET ('GA1WSET') ERASE                                GA1WPGM 
00973           FROM   (GA1WI01O)                                       GA1WPGM 
00974      END-EXEC.                                                    GA1WPGM 
00975                                                                   GA1WPGM 
00976  4099-EXIT.                                                       GA1WPGM 
00977      EXIT.                                                        GA1WPGM 
00978 /**************************************************************** GA1WPGM 
00979 *              F I L L   T H E   S C R E E N                      GA1WPGM 
00980 *                                                                 GA1WPGM 
00981 *  THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO   GA1WPGM 
00982 *  BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1WPGM 
00983 *  ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1WPGM 
00984 *  FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1WPGM 
00985 *  THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1WPGM 
00986 ******************************************************************GA1WPGM 
00987  4500-FILL-THE-SCREEN SECTION.                                    GA1WPGM 
00988                                                                   GA1WPGM 
00989      MOVE '4500'              TO  WS-PARA-ID.                     GA1WPGM 
00990      MOVE  GXJ-ENTRY-COUNT    TO  GXJ-ENTRY-COUNT.                GA1WPGM 
00991      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1WPGM 
00992                                                                   GA1WPGM 
00993      IF GXJ-ENTRY-COUNT  NOT >  1                                 GA1WPGM 
00994         MOVE '4530'  TO  WS-PARA-ID                               GA1WPGM 
00995         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1WPGM 
00996                                                                   GA1WPGM 
00997                                                                   GA1WPGM 
00998                                                                   GA1WPGM 
00999      SET GXJ-INDEX  TO  1.                                        GA1WPGM 
01000      MOVE '4510'    TO  WS-PARA-ID.                               GA1WPGM 
01001                                                                   GA1WPGM 
01002  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1WPGM 
01003                                                                   GA1WPGM 
01004      IF GXJ-RELATED-PROVISION (GXJ-INDEX) < WS-SAVED-RELATED      GA1WPGM 
01005         SET GXJ-INDEX  UP BY  1                                   GA1WPGM 
01006         IF GXJ-INDEX  <  GXJ-ENTRY-COUNT                          GA1WPGM 
01007            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1WPGM 
01008         ELSE                                                      GA1WPGM 
01009            SET GXJ-INDEX  TO  1.                                  GA1WPGM 
01010                                                                   GA1WPGM 
01011                                                                   GA1WPGM 
01012                                                                   GA1WPGM 
01013      MOVE '4520'  TO  WS-PARA-ID.                                 GA1WPGM 
01014                                                                   GA1WPGM 
01015  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1WPGM 
01016                                                                   GA1WPGM 
01017      MOVE DFHBMUNF                                                GA1WPGM 
01018        TO MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).              GA1WPGM 
01019      MOVE LOW-VALUES                                              GA1WPGM 
01020        TO MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).                   GA1WPGM 
01021      MOVE GXJ-RELATED-PROVISION (GXJ-INDEX)                       GA1WPGM 
01022        TO MAP-RELATED (MAP-IDX1, MAP-IDX2)                        GA1WPGM 
01023      MOVE GXJ-ALTERNATE-PROVISION (GXJ-INDEX)                     GA1WPGM 
01024        TO MAP-ALTERNATE (MAP-IDX1, MAP-IDX2).                     GA1WPGM 
01025                                                                   GA1WPGM 
01026      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1WPGM 
01027         SET  MAP-IDX1  UP BY  1                                   GA1WPGM 
01028      ELSE                                                         GA1WPGM 
01029         IF MAP-IDX2  <  WS-MAP-COL                                GA1WPGM 
01030            SET  MAP-IDX1  TO  1                                   GA1WPGM 
01031            SET  MAP-IDX2  UP BY  1                                GA1WPGM 
01032         ELSE                                                      GA1WPGM 
01033            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1WPGM 
01034                                                                   GA1WPGM 
01035      IF GXJ-INDEX  <  (GXJ-ENTRY-COUNT - 1 )                      GA1WPGM 
01036         SET  GXJ-INDEX  UP BY  1                                  GA1WPGM 
01037         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1WPGM 
01038                                                                   GA1WPGM 
01039                                                                   GA1WPGM 
01040                                                                   GA1WPGM 
01041  4530-FILL-REST-WITH-NULLS.                                       GA1WPGM 
01042                                                                   GA1WPGM 
01043      MOVE '4530'     TO  WS-PARA-ID.                              GA1WPGM 
01044      MOVE DFHBMASK                                                GA1WPGM 
01045        TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).             GA1WPGM 
01046      MOVE LOW-VALUES TO MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),     GA1WPGM 
01047                         MAP-RELATED     (MAP-IDX1, MAP-IDX2),     GA1WPGM 
01048                         MAP-ALTERNATE   (MAP-IDX1, MAP-IDX2).     GA1WPGM 
01049                                                                   GA1WPGM 
01050      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1WPGM 
01051         SET  MAP-IDX1   UP BY  1                                  GA1WPGM 
01052         GO TO 4530-FILL-REST-WITH-NULLS                           GA1WPGM 
01053      ELSE                                                         GA1WPGM 
01054         IF MAP-IDX2  <  WS-MAP-COL                                GA1WPGM 
01055            SET  MAP-IDX1  TO  1                                   GA1WPGM 
01056            SET  MAP-IDX2  UP BY 1                                 GA1WPGM 
01057            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1WPGM 
01058                                                                   GA1WPGM 
01059                                                                   GA1WPGM 
01060                                                                   GA1WPGM 
01061  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1WPGM 
01062                                                                   GA1WPGM 
01063      MOVE '4540'  TO  WS-PARA-ID.                                 GA1WPGM 
01064      IF GXJ-ENTRY-COUNT  =  1                                     GA1WPGM 
01065         SET WS-MESSAGE-INDEX TO +10                               GA1WPGM 
01066         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
01067         GO TO 4599-EXIT.                                          GA1WPGM 
01068                                                                   GA1WPGM 
01069      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1WPGM 
01070         SET WS-MESSAGE-INDEX TO +11                               GA1WPGM 
01071         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA1WPGM 
01072                                                                   GA1WPGM 
01073  4599-EXIT.                                                       GA1WPGM 
01074      EXIT.                                                        GA1WPGM 
01075 /**************************************************************** GA1WPGM 
01076 *         X C T L   T O   P R E V I O U S   M E N U               GA1WPGM 
01077 *                                                                 GA1WPGM 
01078 *   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1WPGM 
01079 *  ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1WPGM 
01080 *  RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1WPGM 
01081 *  THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1WPGM 
01082 *  IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1WPGM 
01083 ******************************************************************GA1WPGM 
01084  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1WPGM 
01085                                                                   GA1WPGM 
01086      MOVE '5000'  TO  WS-PARA-ID.                                 GA1WPGM 
01087                                                                   GA1WPGM 
01088 *******                                                           GA1WPGM 
01089 * STS *  RETURN TO SINGLE TABULAR MENU,  NO  COMMAREA             GA1WPGM 
01090 *******                                                           GA1WPGM 
01091                                                                   GA1WPGM 
01092      IF  ALTBFNCI  =  'GTM1'    AND                               GA1WPGM 
01093          ALTABIDI  =  'STS000'                                    GA1WPGM 
01094          EXEC CICS XCTL  PROGRAM ('GTM1PGM')  END-EXEC.           GA1WPGM 
01095                                                                   GA1WPGM 
01096                                                                   GA1WPGM 
01097      COMPUTE  WS-IO-PARM-WRK-CDRS-LEN        =                    GA1WPGM 
01098          GC-GCIOPARM-LEN                     +                    GA1WPGM 
01099          GC-WORKFILE-KEY-LEN                 +                    GA1WPGM 
01100          GC-GCTABULR-CDRS-FIXED-LEN          +                    GA1WPGM 
01101         (GC-GCTABULR-CDRS-VARY-MAX-OCUR      *                    GA1WPGM 
01102                 GC-GCTABULR-CDRS-VARY-LEN).                       GA1WPGM 
01103                                                                   GA1WPGM 
01104      EXEC CICS GETMAIN                                            GA1WPGM 
01105           SET     (ADDRESS OF IO-PARM-CDRS-TABULAR-RECORD)        GA1WPGM 
01106           INITIMG (WS-HEX-00)                                     GA1WPGM 
01107           LENGTH  (WS-IO-PARM-WRK-CDRS-LEN)                       GA1WPGM 
01108      END-EXEC.                                                    GA1WPGM 
01109                                                                   GA1WPGM 
01110      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1WPGM 
01111      MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           GA1WPGM 
01112      MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           GA1WPGM 
01113      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1WPGM 
01114      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1WPGM 
01115      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA1WPGM 
01116      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1WPGM 
01117      MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           GA1WPGM 
01118      MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      GA1WPGM 
01119      MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1WPGM 
01120      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1WPGM 
01121                                                                   GA1WPGM 
01122 ***  ALTABIDI/ALTBSLTI ARE THE PRIMARY DRIVER             ***     GA1WPGM 
01123      MOVE SPACES               TO GCA-BEN-PROV-ID.                GA1WPGM 
01124      MOVE ALTABIDI             TO GCIO-WRK-PROVISION-ID           GA1WPGM 
01125                                   GCA-ALL-LEVEL-TAB-ID.           GA1WPGM 
01126      MOVE ALTBSLTI             TO GCIO-WRK-PROVISION-SLOT-NO      GA1WPGM 
01127                                   GCA-ALL-LEVEL-TAB-SLOT.         GA1WPGM 
01128                                                                   GA1WPGM 
01129                                                                   GA1WPGM 
01130      MOVE ZEROES               TO WS-CARRY-OVER-SLOT-NO           GA1WPGM 
01131      MOVE INTBSLTI             TO WS-CARRY-OVER-SLOT-NO           GA1WPGM 
01132                                                                   GA1WPGM 
01133      MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID       GA1WPGM 
01134                                   GCA-INTERNAL-TAB-ID             GA1WPGM 
01135                                   GCA-INTERNAL-TAB-SLOT.          GA1WPGM 
01136      MOVE ZEROES               TO GCIO-WRK-TAB-PROV-SLOT-NO.      GA1WPGM 
01137                                                                   GA1WPGM 
01138      MOVE GC-GCPSWORK-DDNAME   TO  GCIO2-FILE-DDNAME.             GA1WPGM 
01139      MOVE GCIO-WORKFILE-KEY    TO  GCIO2-FILE-KEY.                GA1WPGM 
01140                                                                   GA1WPGM 
01141      SET GCA-RECORD-POINTER                                       GA1WPGM 
01142       TO ADDRESS OF IO-PARM-CDRS-TABULAR-RECORD.                  GA1WPGM 
01143                                                                   GA1WPGM 
01144      MOVE GC-GCTABULR-CDRS-VARY-MAX-OCUR                          GA1WPGM 
01145        TO GTE-ENTRY-COUNT.                                        GA1WPGM 
01146      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1WPGM 
01147                                                                   GA1WPGM 
01148      EXEC CICS LINK                                               GA1WPGM 
01149           PROGRAM  ('GCIOPGM')                                    GA1WPGM 
01150           COMMAREA (IO-PARM-CDRS-TABULAR-RECORD)                  GA1WPGM 
01151           LENGTH   (WS-IO-PARM-WRK-CDRS-LEN)                      GA1WPGM 
01152      END-EXEC.                                                    GA1WPGM 
01153                                                                   GA1WPGM 
01154      IF NOT GCIO2-GOOD-RETURN                                     GA1WPGM 
01155         SET WS-MESSAGE-INDEX TO +03                               GA1WPGM 
01156         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1WPGM 
01157         MOVE '1T08'          TO WS-ABEND-CODE                     GA1WPGM 
01158         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1WPGM 
01159                                                                   GA1WPGM 
01160                                                                   GA1WPGM 
01161      IF  GCA-FROM-MENU-ID  =  'GC4G'                              GA1WPGM 
01162          SEARCH GTE-PRIMARY-ENTRY                                 GA1WPGM 
01163              AT END                                               GA1WPGM 
01164                 SET GTE-INDEX TO 1                                GA1WPGM 
01165            WHEN ((GTE-PRIMARY-DRIVER (GTE-INDEX) = '#IRPV ')      GA1WPGM 
01166                   AND                                             GA1WPGM 
01167                  (GTE-PRIMARY-SLOT-NO (GTE-INDEX)  =              GA1WPGM 
01168                   WS-CARRY-OVER-SLOT-NO ))                        GA1WPGM 
01169                   MOVE 'Y'    TO WS-PRIM-IND                      GA1WPGM 
01170                   MOVE SPACE  TO WS-ADDL-IND                      GA1WPGM 
01171                   MOVE GTE-PRIM-SEQ-NO (GTE-INDEX)                GA1WPGM 
01172                               TO WS-PRIM-SEQ-NO.                  GA1WPGM 
01173                                                                   GA1WPGM 
01174      EXEC CICS XCTL                                               GA1WPGM 
01175           PROGRAM  ('GC4HPGM')                                    GA1WPGM 
01176           COMMAREA (DFHCOMMAREA)                                  GA1WPGM 
01177           LENGTH   (LENGTH OF DFHCOMMAREA)                        GA1WPGM 
01178      END-EXEC.                                                    GA1WPGM 
01179                                                                   GA1WPGM 
01180  5099-EXIT.                                                       GA1WPGM 
01181      EXIT.                                                        GA1WPGM 
01182 /**************************************************************** GA1WPGM 
01183 *            X C T L   T O   M A I N   M E N U                    GA1WPGM 
01184 *                                                                 GA1WPGM 
01185 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1WPGM 
01186 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1WPGM 
01187 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1WPGM 
01188 *  PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1WPGM 
01189 *  PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1WPGM 
01190 *  AND PROGRESS DOWN.                                             GA1WPGM 
01191 ******************************************************************GA1WPGM 
01192  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1WPGM 
01193                                                                   GA1WPGM 
01194      MOVE '6000'  TO  WS-PARA-ID.                                 GA1WPGM 
01195      MOVE '1T09'  TO  WS-ABEND-CODE.                              GA1WPGM 
01196                                                                   GA1WPGM 
01197      EXEC CICS XCTL                                               GA1WPGM 
01198           PROGRAM ('GCPSPGM')                                     GA1WPGM 
01199      END-EXEC.                                                    GA1WPGM 
01200                                                                   GA1WPGM 
01201  6099-EXIT.                                                       GA1WPGM 
01202      EXIT.                                                        GA1WPGM 
01203 /***************************************************************  GA1WPGM 
01204 *                                                              *  GA1WPGM 
01205 * 9000   MOVE MESSAGE TO SCREEN                                *  GA1WPGM 
01206 *                                                              *  GA1WPGM 
01207 ****************************************************************  GA1WPGM 
01208  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA1WPGM 
01209  9000-010.                                                        GA1WPGM 
01210                                                                   GA1WPGM 
01211      MOVE WS-MESSAGE-TEXT (WS-MESSAGE-INDEX)                      GA1WPGM 
01212        TO ERRMSGO.                                                GA1WPGM 
01213                                                                   GA1WPGM 
01214  9000-900-EXIT.                                                   GA1WPGM 
01215      EXIT.                                                        GA1WPGM 
01216 /                                                                 GA1WPGM 
01217  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1WPGM 
01218                                                                   GA1WPGM 
01219      SET MAP-IDX1 TO 7.                                           GA1WPGM 
01220      SET MAP-IDX2 TO 1.                                           GA1WPGM 
01221      MOVE -1 TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).         GA1WPGM 
01222                                                                   GA1WPGM 
01223      EXEC CICS SEND                                               GA1WPGM 
01224           MAP    ('GA1WI01')                                      GA1WPGM 
01225           MAPSET ('GA1WSET') ERASE                                GA1WPGM 
01226           FROM   (GA1WI01O)  WAIT                                 GA1WPGM 
01227      END-EXEC.                                                    GA1WPGM 
01228                                                                   GA1WPGM 
01229      EXEC CICS ABEND                                              GA1WPGM 
01230           ABCODE (WS-ABEND-CODE)                                  GA1WPGM 
01231      END-EXEC.                                                    GA1WPGM 
01232                                                                   GA1WPGM 
01233  9999-EXIT.                                                       GA1WPGM 
01234      EXIT.                                                        GA1WPGM 
01235 /                                                                 GA1WPGM 
