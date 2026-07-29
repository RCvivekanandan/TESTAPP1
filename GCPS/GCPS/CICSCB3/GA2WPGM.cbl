00001  ID DIVISION.                                                     04/07/05
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA2WPGM 
00003  PROGRAM-ID.     GA2WPGM.                                            LV002
00004  AUTHOR.         DELORES FRY.                                     GA2WPGM 
00005  DATE-WRITTEN.   AUGUST 2001.                                     GA2WPGM 
00006  DATE-COMPILED.                                                   GA2WPGM 
00007                                                                   GA2WPGM 
00008 *----------------------------------------------------------------*GA2WPGM 
00009 *                                                                *GA2WPGM 
00010 *            GENERIC CONTRACT PROCESSING SYSTEM (GCPS)           *GA2WPGM 
00011 *            =========================================           *GA2WPGM 
00012 *                                                                *GA2WPGM 
00013 *    #IRPV      RELATED PROVISIONS / ALTERNATE PROVISIONS        *GA2WPGM 
00014 *    =====                                                       *GA2WPGM 
00015 *    INTERNAL TABULAR PROVISION MAINTENANCE \
00016 *                                            ===                 *GA2WPGM 
00017 *                                                                *GA2WPGM 
00018 *   THIS PROGRAM WILL  \
00019 *   PROVISIONS ENTRIES TO THE #IRPV INTERNAL TABULAR RECORD.     *GA2WPGM 
00020 *                                                                *GA2WPGM 
00021 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE        *GA2WPGM 
00022 *   OPERATOR TO ADD ENTRIES TO THE #IRPV INTERNAL TABULAR        *GA2WPGM 
00023 *   RECORD.  THE PROGRAM THEN PROCESSES THE SCREEN ENTRIES,      *GA2WPGM 
00024 *   VALIDATING THE FORMAT OF EACH SCREEN FIELD IN AN ENTRY;      *GA2WPGM 
00025 *   IF ERROR(S), REQUEST A CORRECTION FOR ANY SCREEN FIELD       *GA2WPGM 
00026 *   IN ERROR.                                                    *GA2WPGM 
00027 *                                                                *GA2WPGM 
00028 *     IF NO ERRORS HAVE BEEN FOUND, SET ALL ENTRIES IN           *GA2WPGM 
00029 *   ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER   *GA2WPGM 
00030 *   POSITION IN THE RECORD.  FINALLY UPDATE THE FILE WITH THE    *GA2WPGM 
00031 *   ADDITIONAL ENTRIES FOR THIS INTERNAL TABULAR RECORD.         *GA2WPGM 
00032 *                                                                *GA2WPGM 
00033 *     TO \
00034 *   ENTRIES FROM THE #IRPV INTERNAL TABULAR RECORD, THE          *GA2WPGM 
00035 *   \
00036 *   PROGRAM, GA1WPGM, WITH TRANS-ID, \
00037 *                                                                *GA2WPGM 
00038 *                                                                *GA2WPGM 
00039 *                                                                *GA2WPGM 
00040 *                                                                *GA2WPGM 
00041 *   FUNC CODE:   GA2W                                            *GA2WPGM 
00042 *   MAPSET:      GA2WSETC                                        *GA2WPGM 
00043 *   FILES:       GCPSWORK                                        *GA2WPGM 
00044 *                FIELD VALIDATION                                *GA2WPGM 
00045 *                                                                *GA2WPGM 
00046 *----------------------------------------------------------------*GA2WPGM 
00047 *                                                                *GA2WPGM 
00048 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GA2WPGM 
00049 *      *-*         U P D A T E   H I S T O R Y         *-*       *GA2WPGM 
00050 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GA2WPGM 
00051 *                                                                *GA2WPGM 
00052 **-CHG-NUM-* *--DATE--* *WHO* *---------DESCRIPTION--------------*GA2WPGM 
00053 *                                                                *GA2WPGM 
00054 *  P00072    00/00/0000  FRY  INITIAL PROGRAM CODING IN SUPPORT  *GA2WPGM 
00055 *                             OF THE NATIONAL CARRIER INITIATIVE.*GA2WPGM 
00056 *                                                                *GA2WPGM 
00057 *  D-358     10/02/2001  GSP  ADDED CODING TO SET GTE-INDEX TO   *GA2WPGM 
00058 *                             DESIRED SEQUENCE NUMBER FOR        *GA2WPGM 
00059 *                             PROCESSING IN GC4HPGM.             *GA2WPGM 
00060 *                                                                *GA2WPGM 
00061 *  NO LOG #  10/08/2002  KIKI CORRECTED SEARCH FOR APPROPRIATE   *GA2WPGM 
00062 *                             #IRPV SEQUENCE, CARRIED OVER TO    *GA2WPGM 
00063 *                             'GC4H' SCREEN                      *GA2WPGM 
00064 *                                                                *GA2WPGM 
00065 *  NO LOG #  10/03/2003  KIKI ADDED INFORMATIONAL MESSAGES       *GA2WPGM 
00066 *                                                                *GA2WPGM 
00067 *  NO LOG #  11/05/2003  KIKI FIXED ENTRANCE TO 'GC4H' WHEN      *GA2WPGM 
00068 *                             GCA-FROM-MENU-ID  =  'GTM1'        *GA2WPGM 
00069 *                             AND 'PF3' IS PRESSED               *GA2WPGM 
00070 *                                                                *GA2WPGM 
00071 *----------------------------------------------------------------*GA2WPGM 
00072 *                                                                 GA2WPGM 
00073  ENVIRONMENT DIVISION.                                            GA2WPGM 
00074                                                                   GA2WPGM 
00075  DATA DIVISION.                                                   GA2WPGM 
00076  WORKING-STORAGE SECTION.                                         GA2WPGM 
00077                                                                   GA2WPGM 
00078  01  FILLER                      PIC X(23)  VALUE                 GA2WPGM 
00079                                      '** GA2WPGM WS BEGINS **'.   GA2WPGM 
00080                                                                   GA2WPGM 
00081  01  FILLER                      PIC X(15) VALUE                  GA2WPGM 
00082                                      '**  PARA-ID  **'.           GA2WPGM 
00083  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2WPGM 
00084                                                                   GA2WPGM 
00085  01  FILLER                      PIC X(25) VALUE                  GA2WPGM 
00086                                      '** GA2WPGM ABEND CODE **'.  GA2WPGM 
00087                                                                   GA2WPGM 
00088  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2WPGM 
00089                                                                   GA2WPGM 
00090  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA2WPGM 
00091                                                                   GA2WPGM 
00092                                                                   GA2WPGM 
00093  01  WS-CARRY-OVER-SLOT-NO               PIC S9(7)  COMP-3.       GA2WPGM 
00094                                                                   GA2WPGM 
00095  01  COMMAREA-POINTER-AREA.                                       GA2WPGM 
00096      05  COMMAREA-PNTR-COMP              PIC S9(08)  COMP.        GA2WPGM 
00097      05  COMMAREA-PNTR  REDEFINES                                 GA2WPGM 
00098                   COMMAREA-PNTR-COMP USAGE IS POINTER.            GA2WPGM 
00099                                                                   GA2WPGM 
00100  01  INTERNAL-POINTER-AREA.                                       GA2WPGM 
00101      05  INTERNAL-TAB-PNTR-COMP          PIC S9(08)  COMP.        GA2WPGM 
00102      05  INTERNAL-TAB-PNTR  REDEFINES                             GA2WPGM 
00103                   INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.        GA2WPGM 
00104                                                                   GA2WPGM 
00105 ******************************************************************GA2WPGM 
00106 *    MAP COBOL SCREEN DSECTS                                      GA2WPGM 
00107 ******************************************************************GA2WPGM 
00108  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2WPGM 
00109                                           '*** I/O MAP AREA ***'. GA2WPGM 
00110  COPY GA2WSETC.                                                   GA2WPGM 
00111                                                                   GA2WPGM 
00112 ******************************************************************GA2WPGM 
00113 *     THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR        GA2WPGM 
00114 *   ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA2WPGM 
00115 *   HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA2WPGM 
00116 *   FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA2WPGM 
00117 *   REDEFINES.                                                    GA2WPGM 
00118 ****************************************************************  GA2WPGM 
00119                                                                   GA2WPGM 
00120  01  FILLER     REDEFINES   GA2WI01I.                             GA2WPGM 
00121      05  FILLER                              PIC X(89).           GA2WPGM 
00122      05  CONTRACT-ID-LINE.                                        GA2WPGM 
00123          10  CONTRACT-PLAN-HEADING           PIC X(05).           GA2WPGM 
00124          10  CONTRACT-PLAN-CODE              PIC X(03).           GA2WPGM 
00125          10  CONTRACT-GROUP-HEADING          PIC X(06).           GA2WPGM 
00126          10  CONTRACT-GROUP-NO               PIC X(09).           GA2WPGM 
00127          10  CONTRACT-SECTION-HEADING        PIC X(06).           GA2WPGM 
00128          10  CONTRACT-SECTION-NO             PIC X(05).           GA2WPGM 
00129          10  CONTRACT-PKG-HEADING            PIC X(06).           GA2WPGM 
00130          10  CONTRACT-PKG-CODE               PIC X(03).           GA2WPGM 
00131          10  CONTRACT-LOB-HEADING            PIC X(06).           GA2WPGM 
00132          10  CONTRACT-LOB                    PIC X(01).           GA2WPGM 
00133          10  CONTRACT-PROV-CTL-HEADING       PIC X(06).           GA2WPGM 
00134          10  CONTRACT-PROV-CTL               PIC X(02).           GA2WPGM 
00135          10  CONTRACT-FAM-REL-HEADING        PIC X(05).           GA2WPGM 
00136          10  CONTRACT-FAM-REL-LVL            PIC X(02).           GA2WPGM 
00137          10  CONTRACT-EFF-DT-HEADING         PIC X(07).           GA2WPGM 
00138          10  CONTRACT-EFF-DATE               PIC X(06).           GA2WPGM 
00139          10  FILLER                          PIC X(01).           GA2WPGM 
00140      05  FILLER                              PIC X(78).           GA2WPGM 
00141      05  MAP-PROVISIONS-ROW          OCCURS 12 TIMES              GA2WPGM 
00142                                      INDEXED BY MAP-IDX1.         GA2WPGM 
00143        10  MAP-PROVISIONS-COL        OCCURS 3 TIMES               GA2WPGM 
00144                                      INDEXED BY MAP-IDX2.         GA2WPGM 
00145          15  MAP-RELATED-PROV-LEN       PIC S9(04) COMP SYNC.     GA2WPGM 
00146          15  MAP-RELATED-PROV-ATTR      PIC  X(01).               GA2WPGM 
00147          15  MAP-RELATED-PROV           PIC  X(06).               GA2WPGM 
00148          15  MAP-ALTERNATE-PROV-LEN     PIC S9(04) COMP SYNC.     GA2WPGM 
00149          15  MAP-ALTERNATE-PROV-ATTR    PIC  X(01).               GA2WPGM 
00150          15  MAP-ALTERNATE-PROV         PIC  X(06).               GA2WPGM 
00151          15  FILLER                     PIC  X(01).               GA2WPGM 
00152                                                                   GA2WPGM 
00153 ****************************************************************  GA2WPGM 
00154 *    FIELDS DESCRIBING NUMBER OF OCCURS FOR MAP.                  GA2WPGM 
00155 ****************************************************************  GA2WPGM 
00156  01  FILLER.                                                      GA2WPGM 
00157      05  WS-MAP-ROW                     PIC S9(03) COMP VALUE +12.GA2WPGM 
00158      05  WS-MAP-COL                     PIC S9(03) COMP VALUE +3. GA2WPGM 
00159                                                                   GA2WPGM 
00160  01  FILLER                      PIC X(32)  VALUE                 GA2WPGM 
00161                              '*** ALTERNATIVE WORKFILE KEY ***'.  GA2WPGM 
00162  01  WS-ALT-WORKFILE-KEYS.                                        GA2WPGM 
00163  COPY GCWRKKEY.                                                   GA2WPGM 
00164                                                                   GA2WPGM 
00165 ****************************************************************  GA2WPGM 
00166 *    WORK FIELDS                                                  GA2WPGM 
00167 ****************************************************************  GA2WPGM 
00168  01  FILLER                           PIC X(17)                   GA2WPGM 
00169                                      VALUE '** WORK FIELDS **'.   GA2WPGM 
00170  01  WS-WORK-FIELDS.                                              GA2WPGM 
00171      05  WS-HEX-00                    PIC X(01) VALUE LOW-VALUES. GA2WPGM 
00172                                                                   GA2WPGM 
00173      05  WS-ADD-COUNT          COMP   PIC 9(03) VALUE ZEROES.     GA2WPGM 
00174                                                                   GA2WPGM 
00175      05  WS-ERROR-SW                  PIC X(01).                  GA2WPGM 
00176                                                                   GA2WPGM 
00177      05  WS-ADD-REQUEST               PIC X(03) VALUE 'ADD'.      GA2WPGM 
00178                                                                   GA2WPGM 
00179      05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2WPGM 
00180          VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.           GA2WPGM 
00181                                                                   GA2WPGM 
00182      05  WS-CONTRACT-TITLE-LINE       PIC X(46)  VALUE            GA2WPGM 
00183          '       CONTRACT INTERNAL TABULAR MAINTENANCE  '.        GA2WPGM 
00184                                                                   GA2WPGM 
00185      05  WS-SAVED-FIELDS.                                         GA2WPGM 
00186          10  WS-SAVED-PROVISIONS      PIC X(12).                  GA2WPGM 
00187      05  WS-SORT-PROVISION-ENTRY      OCCURS 37 TIMES INDEXED BY  GA2WPGM 
00188                        WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.   GA2WPGM 
00189             10  WS-REL-ALT-PROV-SORT.                             GA2WPGM 
00190                 15  WS-RELATED-PROVISION-SORT      PIC X(6).      GA2WPGM 
00191                 15  WS-ALTERNATE-PROVISION-SORT    PIC X(6).      GA2WPGM 
00192                                                                   GA2WPGM 
00193 ****************************************************************  GA2WPGM 
00194 *   RECORD LENGTHS                                                GA2WPGM 
00195 ****************************************************************  GA2WPGM 
00196  01  FILLER                           PIC X(20)                   GA2WPGM 
00197                                    VALUE '** RECORD LENGTHS **'.  GA2WPGM 
00198  01  WS-RECORD-LENGTHS.                                           GA2WPGM 
00199     05 WS-IO-PARM-WRK-IRPV-TAB-LEN    PIC S9(4) COMP VALUE ZEROES.GA2WPGM 
00200     05 WS-IO-PARM-WRK-CDRS-TAB-LEN    PIC S9(4) COMP VALUE ZEROES.GA2WPGM 
00201     05 WS-COPY-LENGTH                 PIC S9(4) COMP VALUE ZEROES.GA2WPGM 
00202     05 WS-GCVI2-PARM-AREA-LEN         PIC S9(4) COMP VALUE +19.   GA2WPGM 
00203                                                                   GA2WPGM 
00204                                                                   GA2WPGM 
00205 ******************************************************************GA2WPGM 
00206 *            MESSAGE TABLE                                       *GA2WPGM 
00207 ******************************************************************GA2WPGM 
00208  01  FILLER                           PIC X(22) VALUE             GA2WPGM 
00209                                      '** WS-MESSAGE-TABLE **'.    GA2WPGM 
00210  01  FILLER.                                                      GA2WPGM 
00211      05  WS-MESSAGE-VALUES.                                       GA2WPGM 
00212          10  WS-MESSAGE-ENTRY-001.                                GA2WPGM 
00213              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00214              15  WS-MESSAGE-TEXT-001.                             GA2WPGM 
00215                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00216                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00217                  20  FILLER          PIC X(03)  VALUE  '001'.     GA2WPGM 
00218                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00219                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00220                           '** INVALID REQUEST. THE PF KEY USED HASGA2WPGM 
00221 -                   ' NO MEANING TO THIS PROGRAM **'.             GA2WPGM 
00222              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00223                                                                   GA2WPGM 
00224 *----------------------------------------------------------------*GA2WPGM 
00225          10  WS-MESSAGE-ENTRY-002.                                GA2WPGM 
00226              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00227              15  WS-MESSAGE-TEXT-002.                             GA2WPGM 
00228                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00229                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00230                  20  FILLER          PIC X(03)  VALUE  '002'.     GA2WPGM 
00231                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00232                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00233                  '** INCLUDE/EXCLUDE FIELD VALUE NOT VALID **'.   GA2WPGM 
00234              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00235                                                                   GA2WPGM 
00236 *----------------------------------------------------------------*GA2WPGM 
00237          10  WS-MESSAGE-ENTRY-003.                                GA2WPGM 
00238              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00239              15  WS-MESSAGE-TEXT-003.                             GA2WPGM 
00240                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00241                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00242                  20  FILLER          PIC X(03)  VALUE  '003'.     GA2WPGM 
00243                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00244                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00245                            '**  ADD ENTRY NOT FOUND               GA2WPGM 
00246 -                    '         '.                                 GA2WPGM 
00247              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00248 *----------------------------------------------------------------*GA2WPGM 
00249          10  WS-MESSAGE-ENTRY-004.                                GA2WPGM 
00250              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00251              15  WS-MESSAGE-TEXT-004.                             GA2WPGM 
00252                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00253                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00254                  20  FILLER          PIC X(03)  VALUE  '004'.     GA2WPGM 
00255                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00256                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00257                              '**  RELATED BENEFIT PROVISION CODE IGA2WPGM 
00258 -                    'S INVALID                        '.         GA2WPGM 
00259              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00260 *----------------------------------------------------------------*GA2WPGM 
00261          10  WS-MESSAGE-ENTRY-005.                                GA2WPGM 
00262              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00263              15  WS-MESSAGE-TEXT-005.                             GA2WPGM 
00264                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00265                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00266                  20  FILLER          PIC X(03)  VALUE  '005'.     GA2WPGM 
00267                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00268                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00269                              '**  ALTERNATE BENEFIT PROVISION CODEGA2WPGM 
00270 -                    ' IS INVALID                      '.         GA2WPGM 
00271              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00272 *----------------------------------------------------------------*GA2WPGM 
00273          10  WS-MESSAGE-ENTRY-006.                                GA2WPGM 
00274              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00275              15  WS-MESSAGE-TEXT-006.                             GA2WPGM 
00276                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00277                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00278                  20  FILLER          PIC X(03)  VALUE  '006'.     GA2WPGM 
00279                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00280                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00281                               '** ERROR READING ALL LEVEL INTERNALGA2WPGM 
00282 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2WPGM 
00283              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00284                                                                   GA2WPGM 
00285 *----------------------------------------------------------------*GA2WPGM 
00286          10  WS-MESSAGE-ENTRY-007.                                GA2WPGM 
00287              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00288              15  WS-MESSAGE-TEXT-007.                             GA2WPGM 
00289                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00290                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00291                  20  FILLER          PIC X(03)  VALUE  '007'.     GA2WPGM 
00292                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00293                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00294                              '** PROGRAM ABOUT TO EXCEED MAX RECORGA2WPGM 
00295 -                    ' SIZE. CONTACT SYSTEMS AREA **'.            GA2WPGM 
00296              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00297 *----------------------------------------------------------------*GA2WPGM 
00298          10  WS-MESSAGE-ENTRY-008.                                GA2WPGM 
00299              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00300              15  WS-MESSAGE-TEXT-008.                             GA2WPGM 
00301                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00302                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00303                  20  FILLER          PIC X(03)  VALUE  '008'.     GA2WPGM 
00304                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00305                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00306                              '** PROGRAM SUBSCRIPT ABOUT TO EXCEEDGA2WPGM 
00307 -                    ' ITS MAX. CONTACT SYSTEMS AREA **'.         GA2WPGM 
00308              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00309                                                                   GA2WPGM 
00310 *----------------------------------------------------------------*GA2WPGM 
00311          10  WS-MESSAGE-ENTRY-009.                                GA2WPGM 
00312              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00313              15  WS-MESSAGE-TEXT-009.                             GA2WPGM 
00314                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00315                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00316                  20  FILLER          PIC X(03)  VALUE  '009'.     GA2WPGM 
00317                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00318                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00319                             '** ERROR REWRITING ALL LEVEL INTERNALGA2WPGM 
00320 -                    ' TABULAR RECORD **'.                        GA2WPGM 
00321              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00322                                                                   GA2WPGM 
00323 *----------------------------------------------------------------*GA2WPGM 
00324          10  WS-MESSAGE-ENTRY-010.                                GA2WPGM 
00325              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00326              15  WS-MESSAGE-TEXT-010.                             GA2WPGM 
00327                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00328                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00329                  20  FILLER          PIC X(03)  VALUE  '010'.     GA2WPGM 
00330                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00331                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00332                               '** ERROR READING ALL LEVEL INTERNALGA2WPGM 
00333 -                    ' TABULAR. CONTACT SYSTEMS AREA **'.         GA2WPGM 
00334              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00335                                                                   GA2WPGM 
00336 *----------------------------------------------------------------*GA2WPGM 
00337          10  WS-MESSAGE-ENTRY-011.                                GA2WPGM 
00338              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00339              15  WS-MESSAGE-TEXT-011.                             GA2WPGM 
00340                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00341                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00342                  20  FILLER          PIC X(03)  VALUE  '011'.     GA2WPGM 
00343                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00344                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00345                      '** COMMAREA LENGTH IS INVALID **'.          GA2WPGM 
00346              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00347                                                                   GA2WPGM 
00348 *----------------------------------------------------------------*GA2WPGM 
00349          10  WS-MESSAGE-ENTRY-012.                                GA2WPGM 
00350              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00351              15  WS-MESSAGE-TEXT-012.                             GA2WPGM 
00352                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00353                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00354                  20  FILLER          PIC X(03)  VALUE  '012'.     GA2WPGM 
00355                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00356                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00357                             'EDIT TABLE EMPTY \
00358 -                    '  PRESS PF14 OR PF16 TO CONTINUE '.         GA2WPGM 
00359              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00360                                                                   GA2WPGM 
00361 *----------------------------------------------------------------*GA2WPGM 
00362          10  WS-MESSAGE-ENTRY-013.                                GA2WPGM 
00363              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00364              15  WS-MESSAGE-TEXT-013.                             GA2WPGM 
00365                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00366                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00367                  20  FILLER          PIC X(03)  VALUE  '013'.     GA2WPGM 
00368                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00369                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00370                               '** RELATED AND ALTERNATE PROVISIONSGA2WPGM 
00371 -                    ' CANNOT BE THE SAME **           '.         GA2WPGM 
00372              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00373                                                                   GA2WPGM 
00374 *----------------------------------------------------------------*GA2WPGM 
00375          10  WS-MESSAGE-ENTRY-014.                                GA2WPGM 
00376              15  FILLER              PIC X(02)  VALUE '¬>'.       GA2WPGM 
00377              15  WS-MESSAGE-TEXT-014.                             GA2WPGM 
00378                  20  FILLER          PIC X(04)  VALUE  'GA2W'.    GA2WPGM 
00379                  20  FILLER          PIC X(01)  VALUE  '-'.       GA2WPGM 
00380                  20  FILLER          PIC X(03)  VALUE  '014'.     GA2WPGM 
00381                  20  FILLER          PIC X(01)  VALUE  ' '.       GA2WPGM 
00382                  20  FILLER          PIC X(70) VALUE              GA2WPGM 
00383                      '**** FUTURE USE ****'.                      GA2WPGM 
00384              15  FILLER              PIC X(02)  VALUE '<¬'.       GA2WPGM 
00385                                                                   GA2WPGM 
00386 *----------------------------------------------------------------*GA2WPGM 
00387                                                                   GA2WPGM 
00388      05  WS-MESSAGE-TABLE            REDEFINES                    GA2WPGM 
00389          WS-MESSAGE-VALUES           OCCURS 014 TIMES             GA2WPGM 
00390                                      INDEXED BY WS-MESSAGE-INDEX. GA2WPGM 
00391          10  WS-MESSAGE-ENTRY.                                    GA2WPGM 
00392              15  FILLER              PIC X(02).                   GA2WPGM 
00393              15  WS-MESSAGE-TEXT     PIC X(79).                   GA2WPGM 
00394              15  FILLER              PIC X(02).                   GA2WPGM 
00395                                                                   GA2WPGM 
00396                                                                   GA2WPGM 
00397 **** FIELD VALIDATION PARM LIST                                   GA2WPGM 
00398  01  GCVIOPGM2-PARM-LIST.                                         GA2WPGM 
00399  COPY GCVINTR2.                                                   GA2WPGM 
00400                                                                   GA2WPGM 
00401 /*** MAP FIELD ATTRIBUTES                                         GA2WPGM 
00402  COPY DFHBMSCA.                                                   GA2WPGM 
00403      02  DFHBMABF                   PIC X VALUE 'Z'.              GA2WPGM 
00404                                                                   GA2WPGM 
00405 /*** ATTENTION IDENTIFIERS                                        GA2WPGM 
00406  COPY DFHAID.                                                     GA2WPGM 
00407                                                                   GA2WPGM 
00408 /*** GENERIC CONTRACT GLOBALLY DEFINES LENGTHS                    GA2WPGM 
00409  01  FILLER.                                                      GA2WPGM 
00410      COPY GCCDRLEN.                                               GA2WPGM 
00411                                                                   GA2WPGM 
00412  01  WS-END                               PIC X(58)  VALUE        GA2WPGM 
00413      '***  GA2WPGM WORKING-STORAGE ENDS HERE  ***'.               GA2WPGM 
00414                                                                   GA2WPGM 
00415  LINKAGE SECTION.                                                 GA2WPGM 
00416                                                                   GA2WPGM 
00417  01  DFHCOMMAREA.                                                 GA2WPGM 
00418  COPY G2ALCKEC.                                                   GA2WPGM 
00419      05  WS-COMMAREA-CDRS-REC                                     GA2WPGM 
00420          REDEFINES COMMAREA-ALL-LEV-TAB-RECORD.                   GA2WPGM 
00421          10  FILLER                      PIC X(97).               GA2WPGM 
00422          10  WS-PRIM-IND                 PIC X(01).               GA2WPGM 
00423          10  WS-PRIM-SEQ-NO-X            PIC X(02).               GA2WPGM 
00424          10  WS-PRIM-SEQ-NO                                       GA2WPGM 
00425              REDEFINES WS-PRIM-SEQ-NO-X  PIC 9(02).               GA2WPGM 
00426          10  WS-ADDL-IND                 PIC X(01).               GA2WPGM 
00427          10  WS-ADDL-SEQ-NO-X            PIC X(02).               GA2WPGM 
00428          10  WS-ADDL-SEQ-NO                                       GA2WPGM 
00429              REDEFINES WS-ADDL-SEQ-NO-X  PIC 9(02).               GA2WPGM 
00430          10  FILLER                      PIC X(47).               GA2WPGM 
00431                                                                   GA2WPGM 
00432  COPY GACDACWA.                                                   GA2WPGM 
00433 ***  05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA2WPGM 
00434      05  GAS1UPD-PASSED-AREA.                                     GA2WPGM 
00435          07  LVL2-B-SW          PIC X.                            GA2WPGM 
00436          07  LVL2-F-SW          PIC X.                            GA2WPGM 
00437          07  LVL2-G-SW          PIC X.                            GA2WPGM 
00438          07  INTR-TAB-PGM-ID    PIC X(8).                         GA2WPGM 
00439          07  FILLER             PIC X(9).                         GA2WPGM 
00440      05  DELADD-OPTION          PIC X(7).                         GA2WPGM 
00441                                                                   GA2WPGM 
00442 ******************************************************************GA2WPGM 
00443 *    I/O PARM, WORKFILE KEY, AND ALL LVL INT. TAB RECORD          GA2WPGM 
00444 ******************************************************************GA2WPGM 
00445  01  IO-PARM-INTERNAL-IRPV-TAB-REC.                               GA2WPGM 
00446  COPY GCIOPRM1.                                                   GA2WPGM 
00447  COPY GCWRKDCC.                                                   GA2WPGM 
00448  COPY GCTIRPVC.                                                   GA2WPGM 
00449                                                                   GA2WPGM 
00450 ****************************************************************  GA2WPGM 
00451 *    COPY OF THE TABULAR PORTION OF IRPV RECORD.                  GA2WPGM 
00452 *    THIS AREA IS USED IN SORTING PROCESS.                        GA2WPGM 
00453 ****************************************************************  GA2WPGM 
00454  01  LK-COPY-TABULAR-TABLE-AREA.                                  GA2WPGM 
00455      05  LK-COPY-TABULAR-TABLE         OCCURS 647 TIMES           GA2WPGM 
00456                                        INDEXED BY COPY-IDX.       GA2WPGM 
00457          10  LK-COPY-REL-ALT-PROVISIONS.                          GA2WPGM 
00458              15  LK-COPY-RELATED-PROVISION      PIC X(6).         GA2WPGM 
00459              15  LK-COPY-ALTERNATE-PROVISION    PIC X(6).         GA2WPGM 
00460                                                                   GA2WPGM 
00461 ****************************************************************  GA2WPGM 
00462 *    IO PARM, WITH WORKFILE KEY, AND #CDRS TABULAR RECORD         GA2WPGM 
00463 ****************************************************************  GA2WPGM 
00464  01  IO-PARM-ALL-LEVEL-CDRS-RECORD.                               GA2WPGM 
00465  COPY GCIOPRM2.                                                   GA2WPGM 
00466  COPY GCWRKDC2.                                                   GA2WPGM 
00467  COPY GCTCDRSC.                                                   GA2WPGM 
00468                                                                   GA2WPGM 
00469                                                                   GA2WPGM 
00470  PROCEDURE DIVISION.                                              GA2WPGM 
00471                                                                   GA2WPGM 
00472 ******************************************************************GA2WPGM 
00473 *                      M A I N L I N E                            GA2WPGM 
00474 *                                                                 GA2WPGM 
00475 *   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONS GA2WPGM 
00476 *   TAKEN BY THE OPERATOR.                                        GA2WPGM 
00477 *   1. IF THIS PROGRAM RECEIVED CONTROL FROM ANOTHER TRANSACTION  GA2WPGM 
00478 *      THEN WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO       GA2WPGM 
00479 *      ENTER ADDITIONS.                                           GA2WPGM 
00480 *   2. RECEIVE THE SCREEN.                                        GA2WPGM 
00481 *   3. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2WPGM 
00482 *      MENU.                                                      GA2WPGM 
00483 *   4. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2WPGM 
00484 *   5. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2WPGM 
00485 *      (RETURN) TO THE DELETE PROGRAM (GA2WPGM).                  GA2WPGM 
00486 *   6. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2WPGM 
00487 *      (RETURN) TO THE PREVIOUS MENU.                             GA2WPGM 
00488 *   7. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN PERFORM  GA2WPGM 
00489 *      NORMAL ADD PROCESSING, EXCEPT BYPASS EMPTY VALIDATION TABLEGA2WPGM 
00490 *      CONDITION FOR THE RELATED PROVISION / ALTERNATE PROVISION  GA2WPGM 
00491 *      ENTRY.                                                     GA2WPGM 
00492 *   8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2WPGM 
00493 *      KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2WPGM 
00494 *                                                                 GA2WPGM 
00495 ******************************************************************GA2WPGM 
00496  1000-MAIN-LINE SECTION.                                          GA2WPGM 
00497                                                                   GA2WPGM 
00498      MOVE '1000'  TO  WS-PARA-ID.                                 GA2WPGM 
00499                                                                   GA2WPGM 
00500      IF EIBAID = DFHCLEAR                                         GA2WPGM 
00501         EXEC CICS SEND                                            GA2WPGM 
00502              FROM (WS-ONE-LOW)                                    GA2WPGM 
00503              ERASE                                                GA2WPGM 
00504         END-EXEC                                                  GA2WPGM 
00505         EXEC CICS                                                 GA2WPGM 
00506              RETURN                                               GA2WPGM 
00507         END-EXEC.                                                 GA2WPGM 
00508                                                                   GA2WPGM 
00509      IF EIBTRNID  NOT =  'GA2W'                                   GA2WPGM 
00510         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2WPGM 
00511         GO TO 1099-RETURN.                                        GA2WPGM 
00512                                                                   GA2WPGM 
00513      EXEC CICS RECEIVE                                            GA2WPGM 
00514           MAP    ('GA2WI01')                                      GA2WPGM 
00515           MAPSET ('GA2WSET')                                      GA2WPGM 
00516           INTO   (GA2WI01I)                                       GA2WPGM 
00517      END-EXEC.                                                    GA2WPGM 
00518                                                                   GA2WPGM 
00519      IF A2WSCRNI  NOT =  '0A2W00'                                 GA2WPGM 
00520         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2WPGM 
00521                                                                   GA2WPGM 
00522      IF EIBAID  =  DFHENTER                                       GA2WPGM 
00523         PERFORM 2000-ADD-PROCESSING                               GA2WPGM 
00524         GO TO 1099-RETURN.                                        GA2WPGM 
00525                                                                   GA2WPGM 
00526      IF EIBAID  =  DFHPF1 OR  DFHPF13                             GA2WPGM 
00527         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2WPGM 
00528                                                                   GA2WPGM 
00529      IF EIBAID  =  DFHPF3 OR  DFHPF15                             GA2WPGM 
00530         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2WPGM 
00531                                                                   GA2WPGM 
00532      IF EIBAID  =  DFHPF4 OR  DFHPF16                             GA2WPGM 
00533         PERFORM 2000-ADD-PROCESSING                               GA2WPGM 
00534         GO TO 1099-RETURN.                                        GA2WPGM 
00535                                                                   GA2WPGM 
00536      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2WPGM 
00537      MOVE -1  TO MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2).       GA2WPGM 
00538      SET WS-MESSAGE-INDEX    TO +01.                              GA2WPGM 
00539      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA2WPGM 
00540                                                                   GA2WPGM 
00541      EXEC CICS SEND                                               GA2WPGM 
00542           MAP    ('GA2WI01')                                      GA2WPGM 
00543           MAPSET ('GA2WSET') DATAONLY                             GA2WPGM 
00544           FROM   (GA2WI01O) CURSOR                                GA2WPGM 
00545      END-EXEC.                                                    GA2WPGM 
00546                                                                   GA2WPGM 
00547      GO TO 1099-RETURN.                                           GA2WPGM 
00548                                                                   GA2WPGM 
00549  1000-EXIT.                                                       GA2WPGM 
00550      EXIT.                                                        GA2WPGM 
00551                                                                   GA2WPGM 
00552  1099-RETURN.                                                     GA2WPGM 
00553      IF (DELADD-OPTION = 'CHG/DEL') OR                            GA2WPGM 
00554         (DELADD-OPTION = 'GAS1UPD') OR                            GA2WPGM 
00555         (DELADD-OPTION = 'GAS2UPD') OR                            GA2WPGM 
00556         (DELADD-OPTION = 'GAS3UPD') OR                            GA2WPGM 
00557         (DELADD-OPTION = 'GAS4UPD') OR                            GA2WPGM 
00558         (DELADD-OPTION = 'GAS5UPD')                               GA2WPGM 
00559          EXEC CICS                                                GA2WPGM 
00560               RETURN                                              GA2WPGM 
00561          END-EXEC                                                 GA2WPGM 
00562      ELSE                                                         GA2WPGM 
00563          EXEC CICS                                                GA2WPGM 
00564               RETURN                                              GA2WPGM 
00565               TRANSID  ('GA2W')                                   GA2WPGM 
00566               COMMAREA (DFHCOMMAREA)                              GA2WPGM 
00567               LENGTH   (EIBCALEN)                                 GA2WPGM 
00568          END-EXEC.                                                GA2WPGM 
00569                                                                   GA2WPGM 
00570      GOBACK.                                                      GA2WPGM 
00571  1099-EXIT.                                                       GA2WPGM 
00572      EXIT.                                                        GA2WPGM 
00573                                                                   GA2WPGM 
00574 ******************************************************************GA2WPGM 
00575 *                A D D   P R O C E S S I N G                      GA2WPGM 
00576 *                                                                 GA2WPGM 
00577 *    THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2WPGM 
00578 *   MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2WPGM.            GA2WPGM 
00579 *   1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2WPGM 
00580 *   2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2WPGM 
00581 *      SKIP TO THE NEXT LINE.                                     GA2WPGM 
00582 *   3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2WPGM 
00583 *      WITH SPECIAL CHARACTERS.  THE OPERATOR MUST ENTER SOME     GA2WPGM 
00584 *      VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2WPGM 
00585 *      DATA.                                                      GA2WPGM 
00586 *   4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2WPGM 
00587 *      APPROPRIATE MESSAGE IS DISPLAYED.                          GA2WPGM 
00588 *   5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2WPGM 
00589 *      ORDER, FIELD BY FIELD.                                     GA2WPGM 
00590 *   6. THE TABULAR RECORD IS READ, AND A COPY OF THE TABLE IS     GA2WPGM 
00591 *      MADE.                                                      GA2WPGM 
00592 *   7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2WPGM 
00593 *      COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2WPGM 
00594 *      BACK INTO THE RECORD.                                      GA2WPGM 
00595 *   8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2WPGM 
00596 *      SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2WPGM 
00597 *                                                                 GA2WPGM 
00598 ******************************************************************GA2WPGM 
00599  2000-ADD-PROCESSING SECTION.                                     GA2WPGM 
00600                                                                   GA2WPGM 
00601      MOVE '2000'  TO  WS-PARA-ID.                                 GA2WPGM 
00602      MOVE 'N'     TO  WS-ERROR-SW.                                GA2WPGM 
00603      MOVE ZERO    TO  WS-ADD-COUNT.                               GA2WPGM 
00604      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2WPGM 
00605                                                                   GA2WPGM 
00606                                                                   GA2WPGM 
00607                                                                   GA2WPGM 
00608      MOVE '2005'  TO  WS-PARA-ID.                                 GA2WPGM 
00609  2005-RESET-ALL-ATTRIBUTES.                                       GA2WPGM 
00610                                                                   GA2WPGM 
00611      PERFORM WITH TEST BEFORE                                     GA2WPGM 
00612       VARYING MAP-IDX1 FROM 1 BY 1                                GA2WPGM 
00613         UNTIL MAP-IDX2 > WS-MAP-COL                               GA2WPGM 
00614            MOVE  DFHBMUNF                                         GA2WPGM 
00615              TO  MAP-RELATED-PROV-ATTR   (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00616                  MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00617            IF MAP-IDX1 = WS-MAP-ROW                               GA2WPGM 
00618               SET MAP-IDX2 UP BY 1                                GA2WPGM 
00619               SET MAP-IDX1 TO 1                                   GA2WPGM 
00620               SET MAP-IDX1 DOWN BY 1                              GA2WPGM 
00621            END-IF                                                 GA2WPGM 
00622      END-PERFORM.                                                 GA2WPGM 
00623                                                                   GA2WPGM 
00624                                                                   GA2WPGM 
00625                                                                   GA2WPGM 
00626      MOVE '2010'  TO WS-PARA-ID.                                  GA2WPGM 
00627      MOVE SPACES  TO ERRMSGO.                                     GA2WPGM 
00628      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA2WPGM 
00629                                                                   GA2WPGM 
00630  2010-VALIDATE-ADD-ENTRIES.                                       GA2WPGM 
00631                                                                   GA2WPGM 
00632 *--  EDIT SCREEN FIELD, \
00633 *--                                                               GA2WPGM 
00634      IF MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2) = ZERO          GA2WPGM 
00635         IF MAP-IDX1  <  WS-MAP-ROW                                GA2WPGM 
00636            SET MAP-IDX1  UP BY  1                                 GA2WPGM 
00637            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2WPGM 
00638         ELSE                                                      GA2WPGM 
00639         IF MAP-IDX2  <  WS-MAP-COL                                GA2WPGM 
00640            SET MAP-IDX1  TO  1                                    GA2WPGM 
00641            SET MAP-IDX2  UP BY  1                                 GA2WPGM 
00642            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2WPGM 
00643         ELSE                                                      GA2WPGM 
00644            GO TO 2020-CHECK-FOR-ERRORS.                           GA2WPGM 
00645                                                                   GA2WPGM 
00646                                                                   GA2WPGM 
00647                                                                   GA2WPGM 
00648 *--   DID THE OPERATOR ENTER A \
00649 *--                                                               GA2WPGM 
00650      IF MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2)  =   ZERO       GA2WPGM 
00651         MOVE DFHBMUBF                                             GA2WPGM 
00652           TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)           GA2WPGM 
00653         MOVE '??????'                                             GA2WPGM 
00654           TO MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)                GA2WPGM 
00655         IF WS-ERROR-SW  NOT = 'Y'                                 GA2WPGM 
00656            MOVE 'Y'  TO  WS-ERROR-SW                              GA2WPGM 
00657            MOVE -1                                                GA2WPGM 
00658              TO MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2)         GA2WPGM 
00659            SET WS-MESSAGE-INDEX TO +04                            GA2WPGM 
00660            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                   GA2WPGM 
00661                                                                   GA2WPGM 
00662                                                                   GA2WPGM 
00663                                                                   GA2WPGM 
00664 *--   VALIDATE THE \
00665 *--                                                               GA2WPGM 
00666      IF MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2) NOT = DFHBMUBF GA2WPGM 
00667         MOVE  'MULT01'  TO  GCVI2-FIELDS-KEY-ID                   GA2WPGM 
00668         MOVE MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)                GA2WPGM 
00669           TO GCVI2-VALUE-LEN-6                                    GA2WPGM 
00670         PERFORM 5200-000-LINK-TO-GCVIOPGM                         GA2WPGM 
00671         IF GCVI2-VALUE-NOT-FOUND                                  GA2WPGM 
00672            MOVE  'MULT06'  TO  GCVI2-FIELDS-KEY-ID                GA2WPGM 
00673            MOVE MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)             GA2WPGM 
00674              TO GCVI2-VALUE-LEN-6                                 GA2WPGM 
00675            PERFORM 5200-000-LINK-TO-GCVIOPGM                      GA2WPGM 
00676            IF GCVI2-VALUE-NOT-FOUND                               GA2WPGM 
00677               MOVE DFHBMUBF                                       GA2WPGM 
00678                 TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00679               IF WS-ERROR-SW  NOT =  'Y'                          GA2WPGM 
00680                  MOVE 'Y'             TO WS-ERROR-SW              GA2WPGM 
00681                  MOVE -1                                          GA2WPGM 
00682                    TO MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2)   GA2WPGM 
00683                  SET WS-MESSAGE-INDEX TO +04                      GA2WPGM 
00684                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              GA2WPGM 
00685               ELSE                                                GA2WPGM 
00686                  MOVE DFHBMUBF                                    GA2WPGM 
00687                    TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00688         ELSE                                                      GA2WPGM 
00689         IF GCVI2-VALUE-NOT-LOADED                                 GA2WPGM 
00690            IF EIBAID  =  DFHPF4  OR  DFHPF16                      GA2WPGM 
00691               NEXT SENTENCE                                       GA2WPGM 
00692            ELSE                                                   GA2WPGM 
00693               MOVE DFHBMUBF                                       GA2WPGM 
00694                 TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00695               IF WS-ERROR-SW  NOT =  'Y'                          GA2WPGM 
00696                  MOVE 'Y'            TO  WS-ERROR-SW              GA2WPGM 
00697                  MOVE -1                                          GA2WPGM 
00698                    TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00699                  SET WS-MESSAGE-INDEX TO +12                      GA2WPGM 
00700                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             GA2WPGM 
00701                                                                   GA2WPGM 
00702                                                                   GA2WPGM 
00703                                                                   GA2WPGM 
00704                                                                   GA2WPGM 
00705                                                                   GA2WPGM 
00706                                                                   GA2WPGM 
00707 *--  EDIT SCREEN FIELD, \
00708 *--                                                               GA2WPGM 
00709                                                                   GA2WPGM 
00710                                                                   GA2WPGM 
00711 *--   DID THE ENTER AN \
00712 *--                                                               GA2WPGM 
00713      IF MAP-ALTERNATE-PROV-LEN (MAP-IDX1, MAP-IDX2)  =   ZERO     GA2WPGM 
00714         MOVE DFHBMUBF                                             GA2WPGM 
00715           TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)         GA2WPGM 
00716         MOVE '??????'                                             GA2WPGM 
00717           TO MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2)              GA2WPGM 
00718         IF WS-ERROR-SW  NOT = 'Y'                                 GA2WPGM 
00719            MOVE 'Y'  TO  WS-ERROR-SW                              GA2WPGM 
00720            MOVE -1                                                GA2WPGM 
00721              TO MAP-ALTERNATE-PROV-LEN (MAP-IDX1, MAP-IDX2)       GA2WPGM 
00722            SET WS-MESSAGE-INDEX TO +05                            GA2WPGM 
00723            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                   GA2WPGM 
00724                                                                   GA2WPGM 
00725                                                                   GA2WPGM 
00726                                                                   GA2WPGM 
00727 *--   VALIDATE THE \
00728 *--                                                               GA2WPGM 
00729      IF MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)              GA2WPGM 
00730                                               NOT =  DFHBMUBF     GA2WPGM 
00731        IF MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)   =               GA2WPGM 
00732                      MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2)      GA2WPGM 
00733           MOVE DFHBMUBF                                           GA2WPGM 
00734             TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)       GA2WPGM 
00735           IF WS-ERROR-SW  NOT =  'Y'                              GA2WPGM 
00736              MOVE 'Y'             TO WS-ERROR-SW                  GA2WPGM 
00737              MOVE -1                                              GA2WPGM 
00738                TO MAP-ALTERNATE-PROV-LEN (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00739              SET WS-MESSAGE-INDEX TO +13                          GA2WPGM 
00740              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  GA2WPGM 
00741           ELSE                                                    GA2WPGM 
00742              MOVE DFHBMUBF                                        GA2WPGM 
00743                TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)    GA2WPGM 
00744        ELSE                                                       GA2WPGM 
00745          MOVE  'MULT01'  TO  GCVI2-FIELDS-KEY-ID                  GA2WPGM 
00746          MOVE MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2)             GA2WPGM 
00747            TO GCVI2-VALUE-LEN-6                                   GA2WPGM 
00748          PERFORM 5200-000-LINK-TO-GCVIOPGM                        GA2WPGM 
00749          IF GCVI2-VALUE-NOT-FOUND                                 GA2WPGM 
00750             MOVE  'MULT06'  TO  GCVI2-FIELDS-KEY-ID               GA2WPGM 
00751             MOVE MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2)          GA2WPGM 
00752               TO GCVI2-VALUE-LEN-6                                GA2WPGM 
00753             PERFORM 5200-000-LINK-TO-GCVIOPGM                     GA2WPGM 
00754             IF GCVI2-VALUE-NOT-FOUND                              GA2WPGM 
00755                MOVE DFHBMUBF                                      GA2WPGM 
00756                  TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00757                IF WS-ERROR-SW  NOT =  'Y'                         GA2WPGM 
00758                   MOVE 'Y'             TO WS-ERROR-SW             GA2WPGM 
00759                   MOVE -1                                         GA2WPGM 
00760                   TO MAP-ALTERNATE-PROV-LEN (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00761                   SET WS-MESSAGE-INDEX TO +05                     GA2WPGM 
00762                   PERFORM 9000-000-MOVE-MSG-TO-SCREEN             GA2WPGM 
00763                ELSE                                               GA2WPGM 
00764                   MOVE DFHBMUBF                                   GA2WPGM 
00765                   TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2) GA2WPGM 
00766          ELSE                                                     GA2WPGM 
00767          IF GCVI2-VALUE-NOT-LOADED                                GA2WPGM 
00768             IF EIBAID  =  DFHPF4  OR  DFHPF16                     GA2WPGM 
00769                NEXT SENTENCE                                      GA2WPGM 
00770             ELSE                                                  GA2WPGM 
00771                MOVE DFHBMUBF                                      GA2WPGM 
00772                  TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00773                IF WS-ERROR-SW  NOT =  'Y'                         GA2WPGM 
00774                   MOVE 'Y'   TO  WS-ERROR-SW                      GA2WPGM 
00775                   MOVE -1                                         GA2WPGM 
00776                   TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2) GA2WPGM 
00777                   SET WS-MESSAGE-INDEX TO +12                     GA2WPGM 
00778                   PERFORM 9000-000-MOVE-MSG-TO-SCREEN.            GA2WPGM 
00779                                                                   GA2WPGM 
00780                                                                   GA2WPGM 
00781                                                                   GA2WPGM 
00782 *--   IF THIS SCEEN OCCURANCE HAS VALID BENEFIT PROVISION CODES,  GA2WPGM 
00783 *--     UPDATE THE ADD COUNT AND MOVE THE OCCURANCE TO THE SORT   GA2WPGM 
00784 *--     AREA.                                                     GA2WPGM 
00785 *--                                                               GA2WPGM 
00786      IF MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)                GA2WPGM 
00787                                                NOT = DFHBMUBF     GA2WPGM 
00788       AND  MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)           GA2WPGM 
00789                                                NOT = DFHBMUBF     GA2WPGM 
00790            ADD 1            TO  WS-ADD-COUNT                      GA2WPGM 
00791            SET WS-SORT-IDX  TO  WS-ADD-COUNT                      GA2WPGM 
00792            MOVE MAP-RELATED-PROV            (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00793              TO WS-RELATED-PROVISION-SORT   (WS-SORT-IDX)         GA2WPGM 
00794            MOVE MAP-ALTERNATE-PROV          (MAP-IDX1, MAP-IDX2)  GA2WPGM 
00795              TO WS-ALTERNATE-PROVISION-SORT (WS-SORT-IDX).        GA2WPGM 
00796                                                                   GA2WPGM 
00797                                                                   GA2WPGM 
00798      IF MAP-IDX1  <  WS-MAP-ROW                                   GA2WPGM 
00799         SET MAP-IDX1  UP BY  1                                    GA2WPGM 
00800         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2WPGM 
00801                                                                   GA2WPGM 
00802                                                                   GA2WPGM 
00803      IF MAP-IDX2  <  WS-MAP-COL                                   GA2WPGM 
00804         SET MAP-IDX1  TO  1                                       GA2WPGM 
00805         SET MAP-IDX2  UP BY  1                                    GA2WPGM 
00806         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2WPGM 
00807                                                                   GA2WPGM 
00808                                                                   GA2WPGM 
00809                                                                   GA2WPGM 
00810  2020-CHECK-FOR-ERRORS.                                           GA2WPGM 
00811                                                                   GA2WPGM 
00812      MOVE '2020'  TO  WS-PARA-ID.                                 GA2WPGM 
00813                                                                   GA2WPGM 
00814      IF A2WINEXI   =  'I' OR  'E'                                 GA2WPGM 
00815         CONTINUE                                                  GA2WPGM 
00816      ELSE                                                         GA2WPGM 
00817         MOVE DFHBMUBF        TO  A2WINEXO                         GA2WPGM 
00818         MOVE -1              TO  A2WINEXL                         GA2WPGM 
00819         MOVE 'Y'             TO  WS-ERROR-SW                      GA2WPGM 
00820         SET WS-MESSAGE-INDEX TO  +02                              GA2WPGM 
00821         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA2WPGM 
00822                                                                   GA2WPGM 
00823      IF WS-ERROR-SW  =  'Y'                                       GA2WPGM 
00824         MOVE LOW-VALUES  TO  A2WFUNCO,  A2WTITLO,  A2WSCRNO,      GA2WPGM 
00825                              A2WTBIDO,  A2WTBSLO,  A2WINIDO,      GA2WPGM 
00826                              A2WINSLO,  A2WINEXO,  A2WFRIDO,      GA2WPGM 
00827                              A2WADDEO,  A2WTBFCO,  A2WECTRO       GA2WPGM 
00828         MOVE '2100'      TO  WS-PARA-ID                           GA2WPGM 
00829         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2WPGM 
00830            VARYING MAP-IDX2 FROM  1  BY  1                        GA2WPGM 
00831              UNTIL MAP-IDX2  >  WS-MAP-COL                        GA2WPGM 
00832            AFTER   MAP-IDX1 FROM  1  BY  1                        GA2WPGM 
00833              UNTIL MAP-IDX1  >  WS-MAP-ROW                        GA2WPGM 
00834         MOVE '2020'  TO  WS-PARA-ID                               GA2WPGM 
00835         EXEC CICS SEND                                            GA2WPGM 
00836                   MAP     ('GA2WI01')                             GA2WPGM 
00837                   MAPSET  ('GA2WSET') DATAONLY                    GA2WPGM 
00838                   FROM    (GA2WI01O)  CURSOR                      GA2WPGM 
00839                   END-EXEC                                        GA2WPGM 
00840         GO TO 2099-EXIT.                                          GA2WPGM 
00841                                                                   GA2WPGM 
00842                                                                   GA2WPGM 
00843      IF WS-ADD-COUNT   NOT >  ZERO                                GA2WPGM 
00844       IF A2WINEXI  = A2WXDRKI                                     GA2WPGM 
00845          SET MAP-IDX1, MAP-IDX2  TO  1                            GA2WPGM 
00846          MOVE DFHBMUBF                                            GA2WPGM 
00847            TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)          GA2WPGM 
00848          MOVE '??????'                                            GA2WPGM 
00849            TO MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)               GA2WPGM 
00850          MOVE -1 TO MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00851          MOVE DFHBMUBF                                            GA2WPGM 
00852            TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)        GA2WPGM 
00853          MOVE '??????'                                            GA2WPGM 
00854            TO MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2)             GA2WPGM 
00855          MOVE LOW-VALUES  TO  A2WFUNCO,  A2WTITLO,  A2WSCRNO,     GA2WPGM 
00856                               A2WTBIDO,  A2WTBSLO,  A2WINIDO,     GA2WPGM 
00857                               A2WINSLO,  A2WINEXO,  A2WFRIDO      GA2WPGM 
00858          SET WS-MESSAGE-INDEX TO +03                              GA2WPGM 
00859          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      GA2WPGM 
00860          EXEC CICS SEND                                           GA2WPGM 
00861               MAP    ('GA2WI01')                                  GA2WPGM 
00862               MAPSET ('GA2WSET') DATAONLY                         GA2WPGM 
00863               FROM   (GA2WI01O)  CURSOR                           GA2WPGM 
00864          END-EXEC                                                 GA2WPGM 
00865          GO TO 2099-EXIT                                          GA2WPGM 
00866       ELSE                                                        GA2WPGM 
00867          SET MAP-IDX1, MAP-IDX2  TO  1                            GA2WPGM 
00868          MOVE DFHBMUBF                                            GA2WPGM 
00869            TO MAP-RELATED-PROV-ATTR (MAP-IDX1, MAP-IDX2)          GA2WPGM 
00870          MOVE '??????'                                            GA2WPGM 
00871            TO MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)               GA2WPGM 
00872          MOVE -1 TO MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2)     GA2WPGM 
00873          MOVE DFHBMUBF                                            GA2WPGM 
00874            TO MAP-ALTERNATE-PROV-ATTR (MAP-IDX1, MAP-IDX2)        GA2WPGM 
00875          MOVE '??????'                                            GA2WPGM 
00876            TO MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2)             GA2WPGM 
00877          MOVE LOW-VALUES  TO  A2WFUNCO,  A2WTITLO,  A2WSCRNO,     GA2WPGM 
00878                               A2WTBIDO,  A2WTBSLO,  A2WINIDO,     GA2WPGM 
00879                               A2WINSLO,  A2WTBFCO,  A2WECTRO,     GA2WPGM 
00880                               A2WADDEO                            GA2WPGM 
00881          SET WS-MESSAGE-INDEX TO +03                              GA2WPGM 
00882          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      GA2WPGM 
00883          EXEC CICS SEND                                           GA2WPGM 
00884               MAP    ('GA2WI01')                                  GA2WPGM 
00885               MAPSET ('GA2WSET') DATAONLY                         GA2WPGM 
00886               FROM   (GA2WI01O)  CURSOR                           GA2WPGM 
00887          END-EXEC                                                 GA2WPGM 
00888          GO TO 2099-EXIT.                                         GA2WPGM 
00889                                                                   GA2WPGM 
00890                                                                   GA2WPGM 
00891                                                                   GA2WPGM 
00892  2025-CONTINUE-PROCESSING.                                        GA2WPGM 
00893                                                                   GA2WPGM 
00894      COMPUTE  WS-IO-PARM-WRK-IRPV-TAB-LEN      =                  GA2WPGM 
00895               GC-GCIOPARM-LEN                  +                  GA2WPGM 
00896               GC-WORKFILE-KEY-LEN              +                  GA2WPGM 
00897               GC-GCTABULR-IRPV-FIXED-LEN       +                  GA2WPGM 
00898              (GC-GCTABULR-IRPV-VARY-LEN        *                  GA2WPGM 
00899               GC-GCTABULR-IRPV-VARY-MAX-OCUR).                    GA2WPGM 
00900                                                                   GA2WPGM 
00901      EXEC CICS GETMAIN                                            GA2WPGM 
00902                SET     (ADDRESS OF IO-PARM-INTERNAL-IRPV-TAB-REC) GA2WPGM 
00903                INITIMG (WS-HEX-00)                                GA2WPGM 
00904                LENGTH  (WS-IO-PARM-WRK-IRPV-TAB-LEN)              GA2WPGM 
00905                END-EXEC.                                          GA2WPGM 
00906                                                                   GA2WPGM 
00907      MOVE SPACES                 TO GCIO-WORKFILE-KEY.            GA2WPGM 
00908      MOVE  'C'                   TO GCIO-WRK-STATUS-CODE.         GA2WPGM 
00909      MOVE  'C3'                  TO GCIO-WRK-RECORD-TYPE.         GA2WPGM 
00910      MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           GA2WPGM 
00911      MOVE CONTRACT-GROUP-NO      TO GCIO-WRK-GROUP-NUM.           GA2WPGM 
00912      MOVE CONTRACT-SECTION-NO    TO GCIO-WRK-SECTION-NUM.         GA2WPGM 
00913      MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE.            GA2WPGM 
00914      MOVE CONTRACT-LOB           TO GCIO-WRK-LINE-OF-BUS.         GA2WPGM 
00915      MOVE CONTRACT-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL.    GA2WPGM 
00916      MOVE CONTRACT-FAM-REL-LVL   TO GCIO-WRK-FAMILY-RELATION-LVL. GA2WPGM 
00917      MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN.           GA2WPGM 
00918      MOVE A2WTBIDI               TO GCIO-WRK-PROVISION-ID.        GA2WPGM 
00919      MOVE A2WTBSLI               TO GCIO-WRK-PROVISION-SLOT-NO.   GA2WPGM 
00920      MOVE '#IRPV '               TO GCIO-WRK-TAB-PROVISION-ID.    GA2WPGM 
00921      MOVE A2WINSLI               TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA2WPGM 
00922      MOVE GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.             GA2WPGM 
00923      MOVE GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.                GA2WPGM 
00924      MOVE 1                      TO GCIO-IO-AREA-TO-USE.          GA2WPGM 
00925      MOVE  'RU '                 TO GCIO-FILE-ACCESS-CODE.        GA2WPGM 
00926      MOVE GC-GCTABULR-IRPV-VARY-MAX-OCUR                          GA2WPGM 
00927        TO GXJ-ENTRY-COUNT.                                        GA2WPGM 
00928                                                                   GA2WPGM 
00929      EXEC CICS LINK                                               GA2WPGM 
00930                PROGRAM  ('GCIOPGM')                               GA2WPGM 
00931                COMMAREA (IO-PARM-INTERNAL-IRPV-TAB-REC)           GA2WPGM 
00932                LENGTH   (WS-IO-PARM-WRK-IRPV-TAB-LEN)             GA2WPGM 
00933                END-EXEC.                                          GA2WPGM 
00934                                                                   GA2WPGM 
00935      IF NOT GCIO-GOOD-RETURN                                      GA2WPGM 
00936         SET WS-MESSAGE-INDEX TO +06                               GA2WPGM 
00937         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
00938         MOVE '2W01'  TO  WS-ABEND-CODE                            GA2WPGM 
00939         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
00940                                                                   GA2WPGM 
00941      MOVE A2WINEXI  TO  A2WXDRKO                                  GA2WPGM 
00942                         GXJ-INCLUDE-EXCLUDE-IND.                  GA2WPGM 
00943                                                                   GA2WPGM 
00944      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2WPGM 
00945         GO TO 2090-UPDATE-IRPV-TAB-WRK-REC.                       GA2WPGM 
00946                                                                   GA2WPGM 
00947                                                                   GA2WPGM 
00948      SET WS-SORT-IDX   TO  1.                                     GA2WPGM 
00949      SET WS-SORT-IDX2  TO  2.                                     GA2WPGM 
00950      MOVE '2030'       TO  WS-PARA-ID.                            GA2WPGM 
00951                                                                   GA2WPGM 
00952  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2WPGM 
00953                                                                   GA2WPGM 
00954      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2WPGM 
00955         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2WPGM 
00956                                                                   GA2WPGM 
00957      IF WS-REL-ALT-PROV-SORT (WS-SORT-IDX)     <                  GA2WPGM 
00958         WS-REL-ALT-PROV-SORT (WS-SORT-IDX2)                       GA2WPGM 
00959         SET WS-SORT-IDX2  UP BY  1                                GA2WPGM 
00960         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2WPGM 
00961      ELSE                                                         GA2WPGM 
00962         IF WS-REL-ALT-PROV-SORT (WS-SORT-IDX)  >                  GA2WPGM 
00963            WS-REL-ALT-PROV-SORT (WS-SORT-IDX2)                    GA2WPGM 
00964            MOVE WS-REL-ALT-PROV-SORT (WS-SORT-IDX)                GA2WPGM 
00965              TO WS-SAVED-PROVISIONS                               GA2WPGM 
00966            MOVE WS-REL-ALT-PROV-SORT (WS-SORT-IDX2)               GA2WPGM 
00967              TO WS-REL-ALT-PROV-SORT (WS-SORT-IDX)                GA2WPGM 
00968            MOVE WS-SAVED-PROVISIONS                               GA2WPGM 
00969              TO WS-REL-ALT-PROV-SORT (WS-SORT-IDX2)               GA2WPGM 
00970            SET WS-SORT-IDX2  UP BY  1                             GA2WPGM 
00971            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2WPGM 
00972                                                                   GA2WPGM 
00973      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2WPGM 
00974      MOVE WS-REL-ALT-PROV-SORT (WS-SORT-IDX3)                     GA2WPGM 
00975        TO WS-REL-ALT-PROV-SORT (WS-SORT-IDX2).                    GA2WPGM 
00976      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2WPGM 
00977      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2WPGM 
00978                                                                   GA2WPGM 
00979                                                                   GA2WPGM 
00980                                                                   GA2WPGM 
00981                                                                   GA2WPGM 
00982  2040-ARE-WE-DONE-WITH-SORT.                                      GA2WPGM 
00983                                                                   GA2WPGM 
00984      MOVE '2040'  TO  WS-PARA-ID.                                 GA2WPGM 
00985      SET WS-SORT-IDX  UP BY  1.                                   GA2WPGM 
00986                                                                   GA2WPGM 
00987      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2WPGM 
00988         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2WPGM 
00989         SET WS-SORT-IDX2  UP BY  1                                GA2WPGM 
00990         MOVE '2030'  TO  WS-PARA-ID                               GA2WPGM 
00991         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2WPGM 
00992                                                                   GA2WPGM 
00993      SET WS-ADD-COUNT      TO WS-SORT-IDX.                        GA2WPGM 
00994      MOVE HIGH-VALUES                                             GA2WPGM 
00995        TO WS-SORT-PROVISION-ENTRY (WS-SORT-IDX).                  GA2WPGM 
00996      MOVE GXJ-ENTRY-COUNT  TO GXJ-ENTRY-COUNT.                    GA2WPGM 
00997                                                                   GA2WPGM 
00998      COMPUTE  WS-COPY-LENGTH                  =                   GA2WPGM 
00999               GC-GCTABULR-IRPV-VARY-MAX-OCUR  *                   GA2WPGM 
01000               GC-GCTABULR-IRPV-VARY-LEN.                          GA2WPGM 
01001                                                                   GA2WPGM 
01002      EXEC CICS GETMAIN                                            GA2WPGM 
01003                SET     (ADDRESS OF LK-COPY-TABULAR-TABLE-AREA)    GA2WPGM 
01004                LENGTH  (WS-COPY-LENGTH)                           GA2WPGM 
01005                INITIMG (WS-HEX-00)                                GA2WPGM 
01006                END-EXEC.                                          GA2WPGM 
01007                                                                   GA2WPGM 
01008                                                                   GA2WPGM 
01009                                                                   GA2WPGM 
01010      SET COPY-IDX,  GXJ-INDEX  TO  1.                             GA2WPGM 
01011      MOVE '2050'  TO  WS-PARA-ID.                                 GA2WPGM 
01012                                                                   GA2WPGM 
01013  2050-MAKE-A-COPY-OF-RECORD.                                      GA2WPGM 
01014                                                                   GA2WPGM 
01015      MOVE GXJ-ENTRIES TO LK-COPY-TABULAR-TABLE-AREA.              GA2WPGM 
01016                                                                   GA2WPGM 
01017      IF WS-ADD-COUNT  +  GXJ-ENTRY-COUNT    >                     GA2WPGM 
01018                                   GC-GCTABULR-IRPV-VARY-MAX-OCUR  GA2WPGM 
01019         SET WS-MESSAGE-INDEX TO +07                               GA2WPGM 
01020         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01021         MOVE '2W02'          TO WS-ABEND-CODE                     GA2WPGM 
01022         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01023                                                                   GA2WPGM 
01024                                                                   GA2WPGM 
01025                                                                   GA2WPGM 
01026      SET WS-SORT-IDX,  COPY-IDX,  GXJ-INDEX  TO  1.               GA2WPGM 
01027      MOVE '2060'  TO  WS-PARA-ID.                                 GA2WPGM 
01028                                                                   GA2WPGM 
01029  2060-MERGE-IN-NEW-ENTRIES.                                       GA2WPGM 
01030                                                                   GA2WPGM 
01031      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2WPGM 
01032         SET GXJ-INDEX  DOWN BY  1                                 GA2WPGM 
01033         SET GXJ-ENTRY-COUNT  TO  GXJ-INDEX                        GA2WPGM 
01034         MOVE GXJ-ENTRY-COUNT TO  GXJ-ENTRY-COUNT                  GA2WPGM 
01035         GO TO 2090-UPDATE-IRPV-TAB-WRK-REC.                       GA2WPGM 
01036                                                                   GA2WPGM 
01037                                                                   GA2WPGM 
01038      IF WS-SORT-PROVISION-ENTRY (WS-SORT-IDX)   = HIGH-VALUES     GA2WPGM 
01039       AND  LK-COPY-TABULAR-TABLE (COPY-IDX) NOT = HIGH-VALUES     GA2WPGM 
01040            GO TO 2070-SAVE-COPIED-ENTRY.                          GA2WPGM 
01041                                                                   GA2WPGM 
01042                                                                   GA2WPGM 
01043      IF WS-SORT-PROVISION-ENTRY (WS-SORT-IDX) NOT = HIGH-VALUES   GA2WPGM 
01044       AND  LK-COPY-TABULAR-TABLE (COPY-IDX)       = HIGH-VALUES   GA2WPGM 
01045            GO TO 2080-INSERT-NEW-ENTRY.                           GA2WPGM 
01046                                                                   GA2WPGM 
01047                                                                   GA2WPGM 
01048      IF WS-SORT-PROVISION-ENTRY (WS-SORT-IDX) =  HIGH-VALUES      GA2WPGM 
01049       AND LK-COPY-TABULAR-TABLE (COPY-IDX)    =  HIGH-VALUES      GA2WPGM 
01050           NEXT SENTENCE                                           GA2WPGM 
01051      ELSE                                                         GA2WPGM 
01052      IF WS-REL-ALT-PROV-SORT (WS-SORT-IDX)    >                   GA2WPGM 
01053                          LK-COPY-REL-ALT-PROVISIONS (COPY-IDX)    GA2WPGM 
01054         GO TO 2070-SAVE-COPIED-ENTRY                              GA2WPGM 
01055      ELSE                                                         GA2WPGM 
01056      IF WS-REL-ALT-PROV-SORT (WS-SORT-IDX)    <                   GA2WPGM 
01057                          LK-COPY-REL-ALT-PROVISIONS (COPY-IDX)    GA2WPGM 
01058         GO TO 2080-INSERT-NEW-ENTRY.                              GA2WPGM 
01059                                                                   GA2WPGM 
01060 ******************************************************************GA2WPGM 
01061 *    AT THIS POINT THE NEW ENTRY'S FIELD MUST BE EQUAL TO THE     GA2WPGM 
01062 *    OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING THE  GA2WPGM 
01063 *    INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE ENTRY GA2WPGM 
01064 *    FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.        GA2WPGM 
01065 ******************************************************************GA2WPGM 
01066                                                                   GA2WPGM 
01067      SET WS-SORT-IDX  UP BY  1.                                   GA2WPGM 
01068                                                                   GA2WPGM 
01069  2070-SAVE-COPIED-ENTRY.                                          GA2WPGM 
01070                                                                   GA2WPGM 
01071      MOVE '2070'  TO  WS-PARA-ID.                                 GA2WPGM 
01072      MOVE LK-COPY-TABULAR-TABLE (COPY-IDX)                        GA2WPGM 
01073        TO GXJ-ENTRY (GXJ-INDEX).                                  GA2WPGM 
01074                                                                   GA2WPGM 
01075      IF COPY-IDX  NOT >  GXJ-ENTRY-COUNT                          GA2WPGM 
01076         SET COPY-IDX   UP BY  1                                   GA2WPGM 
01077         SET GXJ-INDEX  UP BY  1                                   GA2WPGM 
01078         MOVE '2060'  TO  WS-PARA-ID                               GA2WPGM 
01079         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2WPGM 
01080      ELSE                                                         GA2WPGM 
01081         SET WS-MESSAGE-INDEX TO +08                               GA2WPGM 
01082         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01083         MOVE '2W03'  TO  WS-ABEND-CODE                            GA2WPGM 
01084         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01085                                                                   GA2WPGM 
01086                                                                   GA2WPGM 
01087                                                                   GA2WPGM 
01088  2080-INSERT-NEW-ENTRY.                                           GA2WPGM 
01089                                                                   GA2WPGM 
01090      MOVE '2080'  TO  WS-PARA-ID.                                 GA2WPGM 
01091      MOVE WS-RELATED-PROVISION-SORT (WS-SORT-IDX)                 GA2WPGM 
01092        TO GXJ-RELATED-PROVISION (GXJ-INDEX).                      GA2WPGM 
01093      MOVE WS-ALTERNATE-PROVISION-SORT (WS-SORT-IDX)               GA2WPGM 
01094        TO GXJ-ALTERNATE-PROVISION (GXJ-INDEX).                    GA2WPGM 
01095                                                                   GA2WPGM 
01096      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2WPGM 
01097         SET WS-SORT-IDX  UP BY  1                                 GA2WPGM 
01098         SET GXJ-INDEX    UP BY  1                                 GA2WPGM 
01099         MOVE '2060'  TO  WS-PARA-ID                               GA2WPGM 
01100         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2WPGM 
01101      ELSE                                                         GA2WPGM 
01102         SET WS-MESSAGE-INDEX TO +08                               GA2WPGM 
01103         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01104         MOVE '2W04'          TO WS-ABEND-CODE                     GA2WPGM 
01105         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01106                                                                   GA2WPGM 
01107                                                                   GA2WPGM 
01108                                                                   GA2WPGM 
01109  2090-UPDATE-IRPV-TAB-WRK-REC.                                    GA2WPGM 
01110                                                                   GA2WPGM 
01111      MOVE '2090'  TO  WS-PARA-ID.                                 GA2WPGM 
01112      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2WPGM 
01113      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2WPGM 
01114                                                                   GA2WPGM 
01115      COMPUTE  GCIO-RECORD-LENGTH = GC-WORKFILE-KEY-LEN   +        GA2WPGM 
01116               GC-GCTABULR-IRPV-FIXED-LEN                 +        GA2WPGM 
01117              (GXJ-ENTRY-COUNT  *  GC-GCTABULR-IRPV-VARY-LEN).     GA2WPGM 
01118                                                                   GA2WPGM 
01119      COMPUTE  WS-IO-PARM-WRK-IRPV-TAB-LEN      =                  GA2WPGM 
01120               GC-GCIOPARM-LEN  +  GCIO-RECORD-LENGTH.             GA2WPGM 
01121                                                                   GA2WPGM 
01122      EXEC CICS LINK                                               GA2WPGM 
01123                PROGRAM  ('GCIOPGM')                               GA2WPGM 
01124                COMMAREA (IO-PARM-INTERNAL-IRPV-TAB-REC)           GA2WPGM 
01125                LENGTH   (WS-IO-PARM-WRK-IRPV-TAB-LEN)             GA2WPGM 
01126                END-EXEC.                                          GA2WPGM 
01127                                                                   GA2WPGM 
01128      IF NOT GCIO-GOOD-RETURN                                      GA2WPGM 
01129         SET WS-MESSAGE-INDEX TO +09                               GA2WPGM 
01130         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01131         MOVE '2W05'  TO  WS-ABEND-CODE                            GA2WPGM 
01132         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01133                                                                   GA2WPGM 
01134      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2WPGM 
01135         VARYING MAP-IDX2 FROM 1  BY  1                            GA2WPGM 
01136           UNTIL MAP-IDX2  >  WS-MAP-COL                           GA2WPGM 
01137         AFTER   MAP-IDX1 FROM 1  BY  1                            GA2WPGM 
01138           UNTIL MAP-IDX1  >  WS-MAP-ROW.                          GA2WPGM 
01139                                                                   GA2WPGM 
01140      EXEC CICS SEND                                               GA2WPGM 
01141                MAP    ('GA2WI01')                                 GA2WPGM 
01142                MAPSET ('GA2WSET') ERASE                           GA2WPGM 
01143                FROM   (GA2WI01O)                                  GA2WPGM 
01144                END-EXEC.                                          GA2WPGM 
01145                                                                   GA2WPGM 
01146  2099-EXIT.                                                       GA2WPGM 
01147      EXIT.                                                        GA2WPGM 
01148 /*****************************************************************GA2WPGM 
01149 *      D O N ' T    R E T R A N S M I T    F I E L D S            GA2WPGM 
01150 *                                                                 GA2WPGM 
01151 *    WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2WPGM 
01152 *   ALREADY ON THE OPERATORS SCREEN.                              GA2WPGM 
01153 *                                                                 GA2WPGM 
01154 ******************************************************************GA2WPGM 
01155  2100-DONT-RETRANSMIT-FIELDS   SECTION.                           GA2WPGM 
01156  2100-010.                                                        GA2WPGM 
01157                                                                   GA2WPGM 
01158      MOVE LOW-VALUES                                              GA2WPGM 
01159        TO MAP-RELATED-PROV (MAP-IDX1, MAP-IDX2)                   GA2WPGM 
01160           MAP-ALTERNATE-PROV (MAP-IDX1, MAP-IDX2).                GA2WPGM 
01161                                                                   GA2WPGM 
01162  2100-900-EXIT.                                                   GA2WPGM 
01163      EXIT.                                                        GA2WPGM 
01164 /*****************************************************************GA2WPGM 
01165 *          X C T L    T O    D E L E T E    S C R E E N           GA2WPGM 
01166 *                                                                 GA2WPGM 
01167 *   THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2WPGM 
01168 *  DELETING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR &    GA2WPGM 
01169 *  PASS THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL GA2WPGM 
01170 *  TABULAR RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU  GA2WPGM 
01171 *  THE PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THEGA2WPGM 
01172 *  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2WPGM 
01173 ******************************************************************GA2WPGM 
01174  3000-XCTL-TO-DEL-SCREEN    SECTION.                              GA2WPGM 
01175  3000-010.                                                        GA2WPGM 
01176                                                                   GA2WPGM 
01177      MOVE '3000'  TO  WS-PARA-ID.                                 GA2WPGM 
01178                                                                   GA2WPGM 
01179      COMPUTE  WS-IO-PARM-WRK-IRPV-TAB-LEN     =                   GA2WPGM 
01180            GC-GCIOPARM-LEN                    +                   GA2WPGM 
01181            GC-WORKFILE-KEY-LEN                +                   GA2WPGM 
01182            GC-GCTABULR-IRPV-FIXED-LEN         +                   GA2WPGM 
01183      (GC-GCTABULR-IRPV-VARY-LEN * GC-GCTABULR-IRPV-VARY-MAX-OCUR).GA2WPGM 
01184                                                                   GA2WPGM 
01185      EXEC CICS GETMAIN                                            GA2WPGM 
01186                SET     (ADDRESS OF IO-PARM-INTERNAL-IRPV-TAB-REC) GA2WPGM 
01187                INITIMG (WS-HEX-00)                                GA2WPGM 
01188                LENGTH  (WS-IO-PARM-WRK-IRPV-TAB-LEN)              GA2WPGM 
01189                END-EXEC.                                          GA2WPGM 
01190                                                                   GA2WPGM 
01191                                                                   GA2WPGM 
01192                                                                   GA2WPGM 
01193      MOVE SPACES                  TO GCIO-WORKFILE-KEY.           GA2WPGM 
01194      MOVE   'C'                   TO GCIO-WRK-STATUS-CODE.        GA2WPGM 
01195      MOVE   'C3'                  TO GCIO-WRK-RECORD-TYPE.        GA2WPGM 
01196      MOVE  GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.          GA2WPGM 
01197      MOVE  GCA-GROUP-NUM          TO GCIO-WRK-GROUP-NUM.          GA2WPGM 
01198      MOVE  GCA-SECTION-NUM        TO GCIO-WRK-SECTION-NUM.        GA2WPGM 
01199      MOVE  GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE.           GA2WPGM 
01200      MOVE  GCA-L-O-B              TO GCIO-WRK-LINE-OF-BUS.        GA2WPGM 
01201      MOVE  GCA-PROV-CTL           TO GCIO-WRK-PROVIDER-CONTROL.   GA2WPGM 
01202      MOVE  GCA-FAM-REL-LVL        TO GCIO-WRK-FAMILY-RELATION-LVL.GA2WPGM 
01203      MOVE  GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN.          GA2WPGM 
01204      MOVE  GCA-ALL-LEVEL-TAB-ID   TO GCIO-WRK-PROVISION-ID.       GA2WPGM 
01205      MOVE  GCA-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO.  GA2WPGM 
01206      MOVE  GCA-INTERNAL-TAB-ID    TO GCIO-WRK-TAB-PROVISION-ID.   GA2WPGM 
01207      MOVE  GCA-INTERNAL-TAB-SLOT  TO GCIO-WRK-TAB-PROV-SLOT-NO.   GA2WPGM 
01208      MOVE  A2WADDEI               TO GCA-ADD-DEL-IND.             GA2WPGM 
01209      MOVE  A2WTBFCI               TO GCA-ALL-LEVEL-TAB-FUNC-CODE. GA2WPGM 
01210      MOVE  A2WECTRI               TO GCA-OCCURS-ENTRY-COUNTER.    GA2WPGM 
01211      MOVE  A2WFRIDI               TO GCA-FROM-MENU-ID.            GA2WPGM 
01212      MOVE  A2WINEXI               TO GCA-I-E-INDC.                GA2WPGM 
01213                                                                   GA2WPGM 
01214      MOVE  GC-GCPSWORK-DDNAME     TO GCIO-FILE-DDNAME.            GA2WPGM 
01215      MOVE  GCIO-WORKFILE-KEY      TO GCIO-FILE-KEY.               GA2WPGM 
01216                                                                   GA2WPGM 
01217      SET GCA-RECORD-POINTER                                       GA2WPGM 
01218       TO ADDRESS OF IO-PARM-INTERNAL-IRPV-TAB-REC.                GA2WPGM 
01219                                                                   GA2WPGM 
01220      MOVE GC-GCTABULR-IRPV-VARY-MAX-OCUR                          GA2WPGM 
01221        TO GXJ-ENTRY-COUNT.                                        GA2WPGM 
01222      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2WPGM 
01223                                                                   GA2WPGM 
01224      EXEC CICS LINK                                               GA2WPGM 
01225                PROGRAM  ('GCIOPGM')                               GA2WPGM 
01226                COMMAREA (IO-PARM-INTERNAL-IRPV-TAB-REC)           GA2WPGM 
01227                LENGTH   (WS-IO-PARM-WRK-IRPV-TAB-LEN)             GA2WPGM 
01228                END-EXEC.                                          GA2WPGM 
01229                                                                   GA2WPGM 
01230      IF NOT GCIO-GOOD-RETURN                                      GA2WPGM 
01231         SET WS-MESSAGE-INDEX TO +10                               GA2WPGM 
01232         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01233         MOVE '2W06'  TO  WS-ABEND-CODE                            GA2WPGM 
01234         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01235                                                                   GA2WPGM 
01236      EXEC CICS XCTL                                               GA2WPGM 
01237                PROGRAM  ('GA1WPGM')                               GA2WPGM 
01238                COMMAREA (DFHCOMMAREA)                             GA2WPGM 
01239                LENGTH   (LENGTH OF DFHCOMMAREA)                   GA2WPGM 
01240                END-EXEC.                                          GA2WPGM 
01241                                                                   GA2WPGM 
01242  3000-900-EXIT.                                                   GA2WPGM 
01243      EXIT.                                                        GA2WPGM 
01244 /*****************************************************************GA2WPGM 
01245 *          D I S P L A Y     F I R S T     S C R E E N            GA2WPGM 
01246 *          ===========================================            GA2WPGM 
01247 *                                                                 GA2WPGM 
01248 *      THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE      GA2WPGM 
01249 *    CONTRACT TABULAR #CDRS MAINTENANCE MENU OR THE CONTRACT      GA2WPGM 
01250 *    TABULAR #CDRS MAINTENANCE ADDITONAL MENU, OR THE #IRPV       GA2WPGM 
01251 *    INTERNAL TABULAR DELETE MENU.                                GA2WPGM 
01252 *                                                                 GA2WPGM 
01253 *      THESE PROGRAMS WILL READ THE #CDRS CONTRACT INTERNAL       GA2WPGM 
01254 *    TABULAR, #IRPV, AND PASS THIS PROGRAM THE RECORD             GA2WPGM 
01255 *    PRECEEDED BY I/O PARMS AND WORKFILE KEY.  THE RECORD         GA2WPGM 
01256 *    WILL BUILD THE SCREEN IMAGE.                                 GA2WPGM 
01257 *                                                                 GA2WPGM 
01258 *      THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE       GA2WPGM 
01259 *    SCREEN AND SENDS THE SCREEN IMAGE TO THE OPERATOR.           GA2WPGM 
01260 *                                                                 GA2WPGM 
01261 ******************************************************************GA2WPGM 
01262  4000-DISPLAY-FIRST-SCREEN   SECTION.                             GA2WPGM 
01263  4000-010.                                                        GA2WPGM 
01264                                                                   GA2WPGM 
01265      MOVE '4000'     TO  WS-PARA-ID.                              GA2WPGM 
01266      MOVE LOW-VALUES TO  GA2WI01I.                                GA2WPGM 
01267                                                                   GA2WPGM 
01268      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2WPGM 
01269         SET WS-MESSAGE-INDEX TO +11                               GA2WPGM 
01270         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01271         MOVE '2W07'          TO WS-ABEND-CODE                     GA2WPGM 
01272         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01273                                                                   GA2WPGM 
01274                                                                   GA2WPGM 
01275      MOVE GCA-ALL-LEVEL-TAB-ID        TO A2WTBIDO.                GA2WPGM 
01276      MOVE GCA-ALL-LEVEL-TAB-SLOT      TO A2WTBSLO.                GA2WPGM 
01277      MOVE GCA-INTERNAL-TAB-ID         TO A2WINIDO.                GA2WPGM 
01278      MOVE GCA-INTERNAL-TAB-SLOT       TO A2WINSLO.                GA2WPGM 
01279      MOVE GCA-ADD-DEL-IND             TO A2WADDEO.                GA2WPGM 
01280      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE TO A2WTBFCO.                GA2WPGM 
01281      MOVE GCA-OCCURS-ENTRY-COUNTER    TO A2WECTRO.                GA2WPGM 
01282      MOVE GCA-FROM-MENU-ID            TO A2WFRIDO.                GA2WPGM 
01283      MOVE GCA-I-E-INDC                TO A2WINEXO                 GA2WPGM 
01284                                          A2WXDRKO.                GA2WPGM 
01285      MOVE WS-ADD-REQUEST              TO A2WFLITO.                GA2WPGM 
01286                                                                   GA2WPGM 
01287 *--                                                               GA2WPGM 
01288 *-- BUILD SCREEN TITLE LINE                                       GA2WPGM 
01289 *--                                                               GA2WPGM 
01290      MOVE WS-CONTRACT-TITLE-LINE    TO A2WTITLO.                  GA2WPGM 
01291      MOVE 'PLN: '                   TO CONTRACT-PLAN-HEADING.     GA2WPGM 
01292      MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE.        GA2WPGM 
01293      MOVE ' GRP: '                  TO CONTRACT-GROUP-HEADING.    GA2WPGM 
01294      MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NO.         GA2WPGM 
01295      MOVE ' SEC: '                  TO CONTRACT-SECTION-HEADING.  GA2WPGM 
01296      MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NO.       GA2WPGM 
01297      MOVE ' PKG: '                  TO CONTRACT-PKG-HEADING.      GA2WPGM 
01298      MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE.         GA2WPGM 
01299      MOVE ' LOB: '                  TO CONTRACT-LOB-HEADING.      GA2WPGM 
01300      MOVE GCA-L-O-B                 TO CONTRACT-LOB.              GA2WPGM 
01301      MOVE ' PRV: '                  TO CONTRACT-PROV-CTL-HEADING. GA2WPGM 
01302      MOVE GCA-PROV-CTL              TO CONTRACT-PROV-CTL.         GA2WPGM 
01303      MOVE ' FR: '                   TO CONTRACT-FAM-REL-HEADING.  GA2WPGM 
01304      MOVE GCA-FAM-REL-LVL           TO CONTRACT-FAM-REL-LVL.      GA2WPGM 
01305      MOVE ' EFDT: '                 TO CONTRACT-EFF-DT-HEADING.   GA2WPGM 
01306      MOVE GCA-EFFECTIVE-DATE        TO CONTRACT-EFF-DATE.         GA2WPGM 
01307                                                                   GA2WPGM 
01308      EXEC CICS SEND                                               GA2WPGM 
01309                MAP    ('GA2WI01')                                 GA2WPGM 
01310                MAPSET ('GA2WSET') ERASE                           GA2WPGM 
01311                FROM   (GA2WI01O)                                  GA2WPGM 
01312                END-EXEC.                                          GA2WPGM 
01313                                                                   GA2WPGM 
01314  4000-900-EXIT.                                                   GA2WPGM 
01315      EXIT.                                                        GA2WPGM 
01316 /*****************************************************************GA2WPGM 
01317 *         X C T L    T O    P R E V I O U S    M E N U            GA2WPGM 
01318 *                                                                 GA2WPGM 
01319 *   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2WPGM 
01320 *  ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA2WPGM 
01321 *  RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA2WPGM 
01322 *  THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA2WPGM 
01323 *  IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA2WPGM 
01324 *                                                                 GA2WPGM 
01325 ******************************************************************GA2WPGM 
01326  5000-XCTL-TO-PREVIOUS-MENU   SECTION.                            GA2WPGM 
01327  5000-010.                                                        GA2WPGM 
01328                                                                   GA2WPGM 
01329      MOVE '5000'  TO  WS-PARA-ID.                                 GA2WPGM 
01330                                                                   GA2WPGM 
01331 *******                                                           GA2WPGM 
01332 * STS *  RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA       GA2WPGM 
01333 *******                                                           GA2WPGM 
01334                                                                   GA2WPGM 
01335      IF  A2WTBFCI  =  'GTM1'  AND                                 GA2WPGM 
01336          A2WTBIDI  =  'STS000'                                    GA2WPGM 
01337          EXEC CICS XCTL  PROGRAM ('GTM1PGM')  END-EXEC.           GA2WPGM 
01338                                                                   GA2WPGM 
01339                                                                   GA2WPGM 
01340      COMPUTE  WS-IO-PARM-WRK-CDRS-TAB-LEN      =                  GA2WPGM 
01341               GC-GCIOPARM-LEN                  +                  GA2WPGM 
01342               GC-WORKFILE-KEY-LEN              +                  GA2WPGM 
01343               GC-GCTABULR-CDRS-FIXED-LEN       +                  GA2WPGM 
01344              (GC-GCTABULR-CDRS-VARY-LEN        *                  GA2WPGM 
01345               GC-GCTABULR-CDRS-VARY-MAX-OCUR).                    GA2WPGM 
01346                                                                   GA2WPGM 
01347      EXEC CICS GETMAIN                                            GA2WPGM 
01348                SET     (ADDRESS OF IO-PARM-ALL-LEVEL-CDRS-RECORD) GA2WPGM 
01349                INITIMG (WS-HEX-00)                                GA2WPGM 
01350                LENGTH  (WS-IO-PARM-WRK-CDRS-TAB-LEN)              GA2WPGM 
01351                END-EXEC.                                          GA2WPGM 
01352                                                                   GA2WPGM 
01353      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA2WPGM 
01354      MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           GA2WPGM 
01355      MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           GA2WPGM 
01356      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2WPGM 
01357      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2WPGM 
01358      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2WPGM 
01359      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2WPGM 
01360      MOVE GCA-L-O-B            TO GCIO-WRK-LINE-OF-BUS.           GA2WPGM 
01361      MOVE GCA-PROV-CTL         TO GCIO-WRK-PROVIDER-CONTROL.      GA2WPGM 
01362      MOVE GCA-FAM-REL-LVL      TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2WPGM 
01363      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2WPGM 
01364                                                                   GA2WPGM 
01365 ***  THIS IS THE PRIMARY ID/SLOT                            ***   GA2WPGM 
01366      MOVE SPACES               TO GCA-BEN-PROV-ID.                GA2WPGM 
01367      MOVE A2WTBIDI             TO GCIO-WRK-PROVISION-ID,          GA2WPGM 
01368                                   GCA-ALL-LEVEL-TAB-ID.           GA2WPGM 
01369      MOVE A2WTBSLI             TO GCIO-WRK-PROVISION-SLOT-NO,     GA2WPGM 
01370                                   GCA-ALL-LEVEL-TAB-SLOT.         GA2WPGM 
01371                                                                   GA2WPGM 
01372      MOVE ZEROES               TO WS-CARRY-OVER-SLOT-NO.          GA2WPGM 
01373      MOVE A2WINSLI             TO WS-CARRY-OVER-SLOT-NO.          GA2WPGM 
01374                                                                   GA2WPGM 
01375      MOVE SPACES               TO GCIO-WRK-TAB-PROVISION-ID,      GA2WPGM 
01376                                   GCA-INTERNAL-TAB-ID,            GA2WPGM 
01377                                   GCA-INTERNAL-TAB-SLOT.          GA2WPGM 
01378      MOVE ZEROES               TO GCIO-WRK-TAB-PROV-SLOT-NO.      GA2WPGM 
01379      MOVE SPACES               TO GCA-I-E-INDC.                   GA2WPGM 
01380      MOVE A2WADDEI             TO GCA-ADD-DEL-IND.                GA2WPGM 
01381      MOVE A2WTBFCI             TO GCA-ALL-LEVEL-TAB-FUNC-CODE.    GA2WPGM 
01382      MOVE A2WECTRI             TO GCA-OCCURS-ENTRY-COUNTER.       GA2WPGM 
01383      MOVE A2WFRIDI             TO GCA-FROM-MENU-ID.               GA2WPGM 
01384      MOVE GC-GCPSWORK-DDNAME   TO GCIO2-FILE-DDNAME.              GA2WPGM 
01385      MOVE GCIO-WORKFILE-KEY    TO GCIO2-FILE-KEY.                 GA2WPGM 
01386      MOVE  'RD '               TO GCIO2-FILE-ACCESS-CODE.         GA2WPGM 
01387                                                                   GA2WPGM 
01388      SET GCA-RECORD-POINTER                                       GA2WPGM 
01389       TO ADDRESS OF IO-PARM-ALL-LEVEL-CDRS-RECORD.                GA2WPGM 
01390                                                                   GA2WPGM 
01391      MOVE GC-GCTABULR-CDRS-VARY-MAX-OCUR                          GA2WPGM 
01392        TO GTE-ENTRY-COUNT.                                        GA2WPGM 
01393                                                                   GA2WPGM 
01394      EXEC CICS LINK                                               GA2WPGM 
01395                PROGRAM  ('GCIOPGM')                               GA2WPGM 
01396                COMMAREA (IO-PARM-ALL-LEVEL-CDRS-RECORD)           GA2WPGM 
01397                LENGTH   (WS-IO-PARM-WRK-CDRS-TAB-LEN)             GA2WPGM 
01398                END-EXEC.                                          GA2WPGM 
01399                                                                   GA2WPGM 
01400      IF NOT  GCIO2-GOOD-RETURN                                    GA2WPGM 
01401         SET WS-MESSAGE-INDEX TO +06                               GA2WPGM 
01402         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA2WPGM 
01403         MOVE '2W08'          TO WS-ABEND-CODE                     GA2WPGM 
01404         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2WPGM 
01405                                                                   GA2WPGM 
01406                                                                   GA2WPGM 
01407      IF  GCA-FROM-MENU-ID  =  'GC4G'                              GA2WPGM 
01408          SEARCH GTE-PRIMARY-ENTRY                                 GA2WPGM 
01409              AT END                                               GA2WPGM 
01410                 SET GTE-INDEX TO 1                                GA2WPGM 
01411            WHEN ((GTE-PRIMARY-DRIVER (GTE-INDEX) = '#IRPV ')      GA2WPGM 
01412                   AND                                             GA2WPGM 
01413                  (GTE-PRIMARY-SLOT-NO (GTE-INDEX) =               GA2WPGM 
01414                   WS-CARRY-OVER-SLOT-NO))                         GA2WPGM 
01415                   MOVE 'Y'    TO WS-PRIM-IND                      GA2WPGM 
01416                   MOVE SPACE  TO WS-ADDL-IND                      GA2WPGM 
01417                   MOVE GTE-PRIM-SEQ-NO (GTE-INDEX)                GA2WPGM 
01418                               TO WS-PRIM-SEQ-NO.                  GA2WPGM 
01419                                                                   GA2WPGM 
01420                                                                   GA2WPGM 
01421      EXEC CICS XCTL                                               GA2WPGM 
01422                PROGRAM  ('GC4HPGM')                               GA2WPGM 
01423                COMMAREA (DFHCOMMAREA)                             GA2WPGM 
01424                LENGTH   (LENGTH OF DFHCOMMAREA)                   GA2WPGM 
01425                END-EXEC.                                          GA2WPGM 
01426                                                                   GA2WPGM 
01427  5000-900-EXIT.                                                   GA2WPGM 
01428      EXIT.                                                        GA2WPGM 
01429 ******************************************************************GA2WPGM 
01430  5200-000-LINK-TO-GCVIOPGM    SECTION.                            GA2WPGM 
01431  5200-010.                                                        GA2WPGM 
01432                                                                   GA2WPGM 
01433      MOVE '5200'  TO  WS-PARA-ID.                                 GA2WPGM 
01434      MOVE ZEROES  TO  GCVI2-RETURN-CODE.                          GA2WPGM 
01435                                                                   GA2WPGM 
01436      EXEC CICS  LINK                                              GA2WPGM 
01437                 PROGRAM   ('GCVIOPGM')                            GA2WPGM 
01438                 COMMAREA  (GCVIOPGM2-PARM-LIST)                   GA2WPGM 
01439                 LENGTH    (WS-GCVI2-PARM-AREA-LEN)                GA2WPGM 
01440                 END-EXEC.                                         GA2WPGM 
01441                                                                   GA2WPGM 
01442  5200-900-EXIT.                                                   GA2WPGM 
01443      EXIT.                                                        GA2WPGM 
01444 /*****************************************************************GA2WPGM 
01445 *              X C T L    T O    M A I N    M E N U               GA2WPGM 
01446 *                                                                 GA2WPGM 
01447 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2WPGM 
01448 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2WPGM 
01449 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2WPGM 
01450 *  PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2WPGM 
01451 *  PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2WPGM 
01452 *  AND PROGRESS DOWN.                                             GA2WPGM 
01453 ******************************************************************GA2WPGM 
01454  6000-XCTL-TO-MAIN-MENU   SECTION.                                GA2WPGM 
01455  6000-010.                                                        GA2WPGM 
01456                                                                   GA2WPGM 
01457      MOVE '6000'  TO  WS-PARA-ID.                                 GA2WPGM 
01458      MOVE '2W09'  TO  WS-ABEND-CODE.                              GA2WPGM 
01459                                                                   GA2WPGM 
01460      EXEC CICS XCTL                                               GA2WPGM 
01461                PROGRAM ('GCPSPGM')                                GA2WPGM 
01462                END-EXEC.                                          GA2WPGM 
01463                                                                   GA2WPGM 
01464  6000-900-EXIT.                                                   GA2WPGM 
01465      EXIT.                                                        GA2WPGM 
01466 /*****************************************************************GA2WPGM 
01467 *          MOVE MESSAGE TO SCREEN                                *GA2WPGM 
01468 ******************************************************************GA2WPGM 
01469  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA2WPGM 
01470  9000-010.                                                        GA2WPGM 
01471                                                                   GA2WPGM 
01472      MOVE WS-MESSAGE-TEXT (WS-MESSAGE-INDEX)                      GA2WPGM 
01473        TO ERRMSGO.                                                GA2WPGM 
01474                                                                   GA2WPGM 
01475  9000-900-EXIT.                                                   GA2WPGM 
01476      EXIT.                                                        GA2WPGM 
01477 /*****************************************************************GA2WPGM 
01478 *          MOVE MESSAGE TO SCREEN                                *GA2WPGM 
01479 ******************************************************************GA2WPGM 
01480  9999-ERROR-MSG-THEN-ABEND    SECTION.                            GA2WPGM 
01481  9999-010.                                                        GA2WPGM 
01482                                                                   GA2WPGM 
01483      SET MAP-IDX1  TO  7.                                         GA2WPGM 
01484      SET MAP-IDX2  TO  1.                                         GA2WPGM 
01485      MOVE -1       TO  MAP-RELATED-PROV-LEN (MAP-IDX1, MAP-IDX2). GA2WPGM 
01486                                                                   GA2WPGM 
01487      EXEC CICS SEND                                               GA2WPGM 
01488                MAP    ('GA2WI01')                                 GA2WPGM 
01489                MAPSET ('GA2WSET') ERASE                           GA2WPGM 
01490                FROM   (GA2WI01O)  CURSOR WAIT                     GA2WPGM 
01491                END-EXEC.                                          GA2WPGM 
01492                                                                   GA2WPGM 
01493      EXEC CICS ABEND                                              GA2WPGM 
01494                ABCODE (WS-ABEND-CODE)                             GA2WPGM 
01495                END-EXEC.                                          GA2WPGM 
01496                                                                   GA2WPGM 
01497  9999-900-EXIT.                                                   GA2WPGM 
01498      EXIT.                                                        GA2WPGM 
