00001  IDENTIFICATION DIVISION.                                         01/12/06
00002 *** THIS IS A COBOL/2 PROGRAM                                     GA1TPGM 
00003  PROGRAM-ID.     GA1TPGM.                                            LV005
00004  AUTHOR.         DELORES FRY.                                     GA1TPGM 
00005  DATE-WRITTEN.   AUGUST 2001.                                     GA1TPGM 
00006  DATE-COMPILED.                                                   GA1TPGM 
00007                                                                   GA1TPGM 
00008 ******************************************************************GA1TPGM 
00009 *                                                                *GA1TPGM 
00010 *            GENERIC CONTRACT PROCESSING SYSTEM (GCPS)           *GA1TPGM 
00011 *            =========================================           *GA1TPGM 
00012 *                                                                *GA1TPGM 
00013 *    #IRDX      RELATED DIAGNOSIS RANGE(S) AND/OR LIST(S)        *GA1TPGM 
00014 *                                                                *GA1TPGM 
00015 *    INTERNAL TABULAR PROVISION MAINTENANCE \
00016 *                                                                *GA1TPGM 
00017 *   THIS PROGRAM WILL  \
00018 *   PROVISIONS ENTRIES FROM THE #IRDX INTERNAL TABULAR RECORD.   *GA1TPGM 
00019 *                                                                *GA1TPGM 
00020 *  THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE   *GA1TPGM 
00021 *  INTERNAL TABULAR RECORD.  THE OPERATOR WILL THEN DETERMINE    *GA1TPGM 
00022 *  IF ANY OF THE ENTRIES WILL BE DELETED.  THE SCREEN ENTRY      *GA1TPGM 
00023 *  WILL BE VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD   *GA1TPGM 
00024 *  WILL BE MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK     *GA1TPGM 
00025 *  INTO THE RECORD BEFORE UPDATING THE RECORD.                   *GA1TPGM 
00026 *                                                                *GA1TPGM 
00027 *  TO EXECUTE THE ADD PROGRAM FOR THIS SET OF DATA (ID: #IRDX)   *GA1TPGM 
00028 *  THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM     *GA1TPGM 
00029 *  TO XCTL TO TRANS-ID GA2T OR PROGRAM GA2TPGM.  THIS PROGRAM    *GA1TPGM 
00030 *  WILL VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN     *GA1TPGM 
00031 *  THE TABLE.                                                    *GA1TPGM 
00032 *                                                                *GA1TPGM 
00033 *   FUNC CODE: GA1T                                              *GA1TPGM 
00034 *   MAPSET:    GA1TSETC                                          *GA1TPGM 
00035 *   FILES:     GCPSWORK                                          *GA1TPGM 
00036 *                                                                *GA1TPGM 
00037 *----------------------------------------------------------------*GA1TPGM 
00038 *                                                                *GA1TPGM 
00039 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GA1TPGM 
00040 *      *-*         U P D A T E   H I S T O R Y         *-*       *GA1TPGM 
00041 *      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       *GA1TPGM 
00042 *                                                                *GA1TPGM 
00043 **-CHG-NUM-* *--DATE--* *WHO* *---------DESCRIPTION--------------*GA1TPGM 
00044 *                                                                *GA1TPGM 
00045 *                                                                *GA1TPGM 
00046 *  P00072    00/00/0000 FRY   INITIAL PROGRAM CODING IN SUPPORT  *GA1TPGM 
00047 *                             OF THE NATIONAL CARRIER INITIATIVE.*GA1TPGM 
00048 *                                                                *GA1TPGM 
00049 *  D-358     10/02/2001 GSP   ADDED CODING TO SET GTE-INDEX TO   *GA1TPGM 
00050 *                             DESIRED SEQUENCE NUMBER FOR        *GA1TPGM 
00051 *                             PROCESSING IN GC4HPGM.             *GA1TPGM 
00052 *                                                                *GA1TPGM 
00053 *                             REMOVED LOGIC TO XCTL TO 'GTM1'.   *GA1TPGM 
00054 *                             WHEN PF3 IS ENTERED, THIS PGM      *GA1TPGM 
00055 *                             WILL NOW ALWAYS GO TO 'GC4H'.      *GA1TPGM 
00056 *                                                                *GA1TPGM 
00057 *  NO LOG #  10/29/2002 KIKI  CORRECTED SEARCH FOR APPROPRIATE   *GA1TPGM 
00058 *                             #IRPV SEQUENCE, CARRIED OVER TO    *GA1TPGM 
00059 *                             'GC4H' SCREEN                      *GA1TPGM 
00060 *                                                                *GA1TPGM 
00061 *  D-356A    04/29/03   GTF   EXPANDED DIAGNOSIS CODE FROM 6 TO  *GA1TPGM 
00062 *                             10 BYTES. CHANGED # OF OCCURS FROM *GA1TPGM 
00063 *                             647 TO 388 FOR #IRDX TABULAR. RE-  *GA1TPGM 
00064 *                             DUCED # OF MAP COLUMNS FROM 3 TO 2.*GA1TPGM 
00065 *                                                                *GA1TPGM 
00066 *  NO LOG #  10/01/2003 KIKI  ADDED INFORMATIONAL MESSAGES       *GA1TPGM 
00067 *                                                                *GA1TPGM 
00068 *  NO LOG #  10/28/2003 KIKI  FIXED ENTRANCE TO 'GC4H' WHEN      *GA1TPGM 
00069 *                             GCA-FROM-MENU-ID = 'GTM1' AND      *GA1TPGM 
00070 *                             PF3 IS PRESSED                     *GA1TPGM 
00071 *                                                                *GA1TPGM 
00072 *            01/11/06   NB    RECOMPILED FOR GCPPDIOC CHANGES    *GA1TPGM 
00071 *                                                                *GA1TPGM 
00071 *            07/26/11   BA    RECOMPILE FOR GCPPDIOC CHANGES     *GA1TPGM 
00073 ******************************************************************GA1TPGM 
00074 *                                                                *GA1TPGM 
00075                                                                   GA1TPGM 
00076  ENVIRONMENT DIVISION.                                            GA1TPGM 
00077                                                                   GA1TPGM 
00078  DATA DIVISION.                                                   GA1TPGM 
00079  WORKING-STORAGE SECTION.                                         GA1TPGM 
00080                                                                   GA1TPGM 
00081  01  WS-BEGIN                      PIC X(22)  VALUE               GA1TPGM 
00082                                 '*** WS BEGINS HERE ***'.         GA1TPGM 
00083                                                                   GA1TPGM 
00084  01  FILLER                        PIC X(15) VALUE                GA1TPGM 
00085                                      '**  PARA-ID  **'.           GA1TPGM 
00086  01  WS-PARA-ID                    PIC X(04) VALUE 'XXXX'.        GA1TPGM 
00087                                                                   GA1TPGM 
00088  01  FILLER                        PIC X(25) VALUE                GA1TPGM 
00089                                      '** GA1WPGM ABEND CODE **'.  GA1TPGM 
00090  01  WS-ABEND-CODE                 PIC X(04) VALUE 'XXXX'.        GA1TPGM 
00091                                                                   GA1TPGM 
00092  01  FILLER                        PIC X(17)  VALUE               GA1TPGM 
00093                                             '*** WORK AREA ***'.  GA1TPGM 
00094                                                                   GA1TPGM 
00095  01  WS-WORK-FIELDS.                                              GA1TPGM 
00096      05  WS-ERROR-SW               PIC X(01).                     GA1TPGM 
00097      05  WS-ONE-LOW                PIC X(01) VALUE LOW-VALUES.    GA1TPGM 
00098      05  WS-HEX-00                 PIC X(01).                     GA1TPGM 
00099      05  WS-QUOTIENT               PIC 9(03) COMP-3.              GA1TPGM 
00100      05  WS-REMAINDER              PIC 9(03) COMP-3.              GA1TPGM 
00101      05  WS-DELETE-COUNT           PIC 9(03) COMP-3.              GA1TPGM 
00102      05  WS-DEL-LITERAL            PIC X(03) VALUE 'DEL'.         GA1TPGM 
00103      05  CONTRACT-TITLE-LINE       PIC X(46) VALUE                GA1TPGM 
00104          'CONTRACT INTERNAL TABULAR WORKFILE MAINTENANCE'.        GA1TPGM 
00105                                                                   GA1TPGM 
00106      05  WS-SAVED-RANGE            PIC X(20).                     GA1TPGM 
00107      05  FILLER    REDEFINES    WS-SAVED-RANGE.                   GA1TPGM 
00108          10  WS-SAVED-FROM         PIC X(10).                     GA1TPGM 
00109          10  WS-SAVED-TO           PIC X(10).                     GA1TPGM 
00110                                                                   GA1TPGM 
00111      05  WS-INTERNAL-ID-SLOT.                                     GA1TPGM 
00112          10  WS-INTERNAL-ID        PIC X(6).                      GA1TPGM 
00113          10  WS-INTERNAL-SLOT-NO   PIC S9(7)  COMP-3.             GA1TPGM 
00114                                                                   GA1TPGM 
00115                                                                   GA1TPGM 
00116  01  FILLER                        PIC X(22)  VALUE               GA1TPGM 
00117                                       '*** RECORD LENGTHS ***'.   GA1TPGM 
00118  01  WS-RECORD-LENGTHS.                                           GA1TPGM 
00119      05 WS-IO-PARM-WRK-IRDX-TAB-LEN    PIC S9(4) COMP.            GA1TPGM 
00120      05 WS-IO-PARM-WRK-CDRS-LEN        PIC S9(4) COMP.            GA1TPGM 
00121      05 WS-COPY-LENGTH                 PIC S9(4) COMP.            GA1TPGM 
00122                                                                   GA1TPGM 
00123  01  COMMAREA-POINTER-AREA.                                       GA1TPGM 
00124      05  COMMAREA-PNTR-COMP            PIC S9(08)  COMP.          GA1TPGM 
00125      05  COMMAREA-PNTR  REDEFINES                                 GA1TPGM 
00126                COMMAREA-PNTR-COMP USAGE IS POINTER.               GA1TPGM 
00127                                                                   GA1TPGM 
00128  01  INTERNAL-POINTER-AREA.                                       GA1TPGM 
00129      05  INTERNAL-TAB-PNTR-COMP         PIC S9(08)  COMP.         GA1TPGM 
00130      05  INTERNAL-TAB-PNTR       REDEFINES                        GA1TPGM 
00131                INTERNAL-TAB-PNTR-COMP USAGE IS POINTER.           GA1TPGM 
00132                                                                   GA1TPGM 
00133  01  FILLER                             PIC X(32)  VALUE          GA1TPGM 
00134                              '*** ALTERNATIVE WORKFILE KEY ***'.  GA1TPGM 
00135  01  WS-ALT-WORKFILE-KEYS.                                        GA1TPGM 
00136  COPY GCWRKKEY.                                                   GA1TPGM 
00137                                                                   GA1TPGM 
00138                                                                   GA1TPGM 
00139 ******************************************************************GA1TPGM 
00140 *    MAP COBOL SCREEN DSECTS                                      GA1TPGM 
00141 ******************************************************************GA1TPGM 
00142  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1TPGM 
00143                                          '***  I/O MAPAREA ***'.  GA1TPGM 
00144  COPY GA1TSETC.                                                   GA1TPGM 
00145                                                                   GA1TPGM 
00146 ******************************************************************GA1TPGM 
00147 *   THIS IS OUR VERSION OF THE RE-OCCURING FIELDS, THEIR          GA1TPGM 
00148 *   ATTRIBUTES, AND LENGTHS; THIS IS DONE BECAUSE BMS CAN'T       GA1TPGM 
00149 *   HANDLE MULTIPLE FIELDS IN A RE-OCCURING GROUP. THE LENGTH     GA1TPGM 
00150 *   FIELDS ARE COMP SYNC TO TAKE CARE OF THE FILLER BYTE BETWEEN  GA1TPGM 
00151 *   REDEFINES.                                                    GA1TPGM 
00152 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1TPGM 
00153 *                                                                 GA1TPGM 
00154 *   THIS AREA MUST BE CHANGED TO MATCH ONE ENTRY IN THE MAP. THE  GA1TPGM 
00155 *   FILLER AREA MUST BE CALCULATED, AND OCCURS COUNT CHANGED TO   GA1TPGM 
00156 *   MATCH THE MAP.                                                GA1TPGM 
00157 *                                                                 GA1TPGM 
00158 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1TPGM 
00159                                                                   GA1TPGM 
00160  01  FILLER     REDEFINES   GA1TI01I.                             GA1TPGM 
00161      05  FILLER                              PIC X(89).           GA1TPGM 
00162      05  CONTRACT-ID-LINE.                                        GA1TPGM 
00163          10  CONTRACT-PLAN-HEADING           PIC X(5).            GA1TPGM 
00164          10  CONTRACT-PLAN-CODE              PIC X(3).            GA1TPGM 
00165          10  CONTRACT-GROUP-HEADING          PIC X(6).            GA1TPGM 
00166          10  CONTRACT-GROUP-NO               PIC X(9).            GA1TPGM 
00167          10  CONTRACT-SECTION-HEADING        PIC X(6).            GA1TPGM 
00168          10  CONTRACT-SECTION-NO             PIC X(5).            GA1TPGM 
00169          10  CONTRACT-PKG-HEADING            PIC X(6).            GA1TPGM 
00170          10  CONTRACT-PKG-CODE               PIC X(3).            GA1TPGM 
00171          10  CONTRACT-LOB-HEADING            PIC X(6).            GA1TPGM 
00172          10  CONTRACT-LOB                    PIC X.               GA1TPGM 
00173          10  CONTRACT-PROV-CTL-HEADING       PIC X(6).            GA1TPGM 
00174          10  CONTRACT-PROV-CTL               PIC XX.              GA1TPGM 
00175          10  CONTRACT-FAM-REL-HEADING        PIC X(5).            GA1TPGM 
00176          10  CONTRACT-FAM-REL-LVL            PIC XX.              GA1TPGM 
00177          10  CONTRACT-EFF-DT-HEADING         PIC X(7).            GA1TPGM 
00178          10  CONTRACT-EFF-DATE               PIC X(6).            GA1TPGM 
00179          10  FILLER                          PIC X(1).            GA1TPGM 
00180      05  FILLER                              PIC X(74).           GA1TPGM 
00181      05  MAP-DIAGNOSIS-RANGE-ROW      OCCURS 12 TIMES             GA1TPGM 
00182                                       INDEXED BY MAP-IDX1.        GA1TPGM 
00183          10  MAP-DIAGNOSIS-RANGE-COL  OCCURS  2 TIMES             GA1TPGM 
00184                                       INDEXED BY MAP-IDX2.        GA1TPGM 
00185              15  MAP-ACTION-CODE-LEN         PIC S9(4) COMP SYNC. GA1TPGM 
00186              15  MAP-ACTION-CODE-ATTR        PIC X.               GA1TPGM 
00187              15  MAP-ACTION-CODE             PIC X.               GA1TPGM 
00188              15  MAP-DIAGNOSIS-FROM-LEN      PIC S9(4) COMP SYNC. GA1TPGM 
00189              15  MAP-DIAGNOSIS-FROM-ATTR     PIC X.               GA1TPGM 
00190              15  MAP-DIAGNOSIS-FROM          PIC X(10).           GA1TPGM 
00191              15  MAP-DIAGNOSIS-TO-LEN        PIC S9(4) COMP SYNC. GA1TPGM 
00192              15  MAP-DIAGNOSIS-TO-ATTR       PIC X.               GA1TPGM 
00193              15  MAP-DIAGNOSIS-TO            PIC X(10).           GA1TPGM 
00194              15  FILLER                      PIC X.               GA1TPGM 
00195                                                                   GA1TPGM 
00196  01  WS-MAP-OCCURS-COUNTERS.                                      GA1TPGM 
00197      05  WS-MAP-ROW              PIC S9(3)  COMP-3 VALUE +12.     GA1TPGM 
00198      05  WS-MAP-COL              PIC S9(3)  COMP-3 VALUE +2.      GA1TPGM 
00199                                                                   GA1TPGM 
00200                                                                   GA1TPGM 
00201  01  FILLER                      PIC X(18)  VALUE                 GA1TPGM 
00202                                            '*** ATTRIBUTES ***'.  GA1TPGM 
00203  COPY DFHBMSCA.                                                   GA1TPGM 
00204      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1TPGM 
00205                                                                   GA1TPGM 
00206  01  FILLER                             PIC X(21) VALUE           GA1TPGM 
00207                                       '*** MESSAGE TABLE ***'.    GA1TPGM 
00208                                                                   GA1TPGM 
00209  01  WT-01-TABLE.                                                 GA1TPGM 
00210      05  FILLER                         PIC X(16) VALUE           GA1TPGM 
00211                                      '* WT 01 TABLE  *'.          GA1TPGM 
00212  01  FILLER.                                                      GA1TPGM 
00213      05  WT-01-MESSAGE-VALUES.                                    GA1TPGM 
00214                                                                   GA1TPGM 
00215 *----------------------------------------------------------------*GA1TPGM 
00216          10  WT-01-ENTRY-001.                                     GA1TPGM 
00217              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00218              15  WT-01-MESSAGE-TEXT-001.                          GA1TPGM 
00219                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00220                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00221                  20  FILLER          PIC X(3)  VALUE  '001'.      GA1TPGM 
00222                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00223                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00224                           '** INVALID REQUEST. THE PF KEY USED HASGA1TPGM 
00225 -                   ' NO MEANING TO THIS PROGRAM **'.             GA1TPGM 
00226              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00227                                                                   GA1TPGM 
00228 *----------------------------------------------------------------*GA1TPGM 
00229          10  WT-01-ENTRY-002.                                     GA1TPGM 
00230              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00231              15  WT-01-MESSAGE-TEXT-002.                          GA1TPGM 
00232                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00233                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00234                  20  FILLER          PIC X(3)  VALUE  '002'.      GA1TPGM 
00235                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00236                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00237                      '** INVALID ACTION CODE FOUND **'.           GA1TPGM 
00238              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00239                                                                   GA1TPGM 
00240 *----------------------------------------------------------------*GA1TPGM 
00241          10  WT-01-ENTRY-003.                                     GA1TPGM 
00242              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00243              15  WT-01-MESSAGE-TEXT-003.                          GA1TPGM 
00244                  20  FILLER          PIC X(4)  VALUE  'GA10'.     GA1TPGM 
00245                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00246                  20  FILLER          PIC X(3)  VALUE  '003'.      GA1TPGM 
00247                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00248                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00249                           '** ERROR READING ALL LEVEL TABULARS CONGA1TPGM 
00250 -                   'TACT SYSTEMS AREA **          '.             GA1TPGM 
00251              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00252 *----------------------------------------------------------------*GA1TPGM 
00253          10  WT-01-ENTRY-004.                                     GA1TPGM 
00254              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00255              15  WT-01-MESSAGE-TEXT-004.                          GA1TPGM 
00256                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00257                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00258                  20  FILLER          PIC X(3)  VALUE  '004'.      GA1TPGM 
00259                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00260                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00261                           '** PROGRAM ERROR IN 2040-DELETE, CONTACGA1TPGM 
00262 -                   'T SYSTEM AREA **              '.             GA1TPGM 
00263              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00264 *----------------------------------------------------------------*GA1TPGM 
00265          10  WT-01-ENTRY-005.                                     GA1TPGM 
00266              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00267              15  WT-01-MESSAGE-TEXT-005.                          GA1TPGM 
00268                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00269                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00270                  20  FILLER          PIC X(3)  VALUE  '005'.      GA1TPGM 
00271                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00272                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00273                           '** PROGRAM ERROR IN 2050-SAVE, CONTACT GA1TPGM 
00274 -                   'SYSTEM AREA **                '.             GA1TPGM 
00275              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00276 *----------------------------------------------------------------*GA1TPGM 
00277          10  WT-01-ENTRY-006.                                     GA1TPGM 
00278              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00279              15  WT-01-MESSAGE-TEXT-006.                          GA1TPGM 
00280                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00281                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00282                  20  FILLER          PIC X(3)  VALUE  '006'.      GA1TPGM 
00283                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00284                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00285                           '** REWRITE ERROR, INTERNAL TABULAR FILEGA1TPGM 
00286 -                   ', CONTACT SYSTEM AREA **      '.             GA1TPGM 
00287              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00288                                                                   GA1TPGM 
00289 *----------------------------------------------------------------*GA1TPGM 
00290          10  WT-01-ENTRY-007.                                     GA1TPGM 
00291              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00292              15  WT-01-MESSAGE-TEXT-007.                          GA1TPGM 
00293                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00294                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00295                  20  FILLER          PIC X(3)  VALUE  '007'.      GA1TPGM 
00296                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00297                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00298                           '** READ ERROR, INTERNAL TABULAR FILE, CGA1TPGM 
00299 -                   'ONTACT SYSTEM AREA **         '.             GA1TPGM 
00300              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00301 *----------------------------------------------------------------*GA1TPGM 
00302          10  WT-01-ENTRY-008.                                     GA1TPGM 
00303              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00304              15  WT-01-MESSAGE-TEXT-008.                          GA1TPGM 
00305                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00306                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00307                  20  FILLER          PIC X(3)  VALUE  '008'.      GA1TPGM 
00308                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00309                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00310               ' ** COMMAREA LENGTH IS INVALID **'.                GA1TPGM 
00311              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00312                                                                   GA1TPGM 
00313 *----------------------------------------------------------------*GA1TPGM 
00314          10  WT-01-ENTRY-009.                                     GA1TPGM 
00315              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00316              15  WT-01-MESSAGE-TEXT-009.                          GA1TPGM 
00317                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00318                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00319                  20  FILLER          PIC X(3)  VALUE  '009'.      GA1TPGM 
00320                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00321                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00322            '**       FUTURE USE                  **'.             GA1TPGM 
00323              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00324                                                                   GA1TPGM 
00325 *----------------------------------------------------------------*GA1TPGM 
00326          10  WT-01-ENTRY-010.                                     GA1TPGM 
00327              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00328              15  WT-01-MESSAGE-TEXT-010.                          GA1TPGM 
00329                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00330                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00331                  20  FILLER          PIC X(3)  VALUE  '010'.      GA1TPGM 
00332                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00333                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00334                           '** NO MORE ENTRIES TO DELETE **    ** PGA1TPGM 
00335 -                   'RESS PF3 FIR ADDTL. DRIVER ** '.             GA1TPGM 
00336              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00337                                                                   GA1TPGM 
00338 *----------------------------------------------------------------*GA1TPGM 
00339          10  WT-01-ENTRY-011.                                     GA1TPGM 
00340              15  FILLER              PIC X(2)  VALUE '¬>'.        GA1TPGM 
00341              15  WT-01-MESSAGE-TEXT-011.                          GA1TPGM 
00342                  20  FILLER          PIC X(4)  VALUE  'GA1T'.     GA1TPGM 
00343                  20  FILLER          PIC X(1)  VALUE  '-'.        GA1TPGM 
00344                  20  FILLER          PIC X(3)  VALUE  '011'.      GA1TPGM 
00345                  20  FILLER          PIC X(1)  VALUE  ' '.        GA1TPGM 
00346                  20  FILLER          PIC X(70) VALUE              GA1TPGM 
00347                          '** NO MORE ENTRIES TO DISPLAY **    ** PGA1TPGM 
00348 -                   'RESS PF3 FOR ADDTL. DRIVER ** '.             GA1TPGM 
00349              15  FILLER              PIC X(2)  VALUE '<¬'.        GA1TPGM 
00350                                                                   GA1TPGM 
00351 *----------------------------------------------------------------*GA1TPGM 
00352                                                                   GA1TPGM 
00353      05  WT-01-MESSAGE-TABLE         REDEFINES                    GA1TPGM 
00354          WT-01-MESSAGE-VALUES        OCCURS 011 TIMES             GA1TPGM 
00355                                      INDEXED BY WT-01-INDEX.      GA1TPGM 
00356          10  WT-01-ENTRY.                                         GA1TPGM 
00357              15  FILLER              PIC X(02).                   GA1TPGM 
00358              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GA1TPGM 
00359              15  FILLER              PIC X(02).                   GA1TPGM 
00360                                                                   GA1TPGM 
00361 *----------------------------------------------------------------*GA1TPGM 
00362                                                                   GA1TPGM 
00363  01  FILLER                      PIC X(27)  VALUE                 GA1TPGM 
00364                                    '*** GCPS RECORD LENGTHS ***'. GA1TPGM 
00365  01  FILLER.                                                      GA1TPGM 
00366      COPY GCCDRLEN.                                               GA1TPGM 
00367                                                                   GA1TPGM 
00368 ******************************************************************GA1TPGM 
00369 *    AREA TO VALIDATE THE DIAGNOSIS CODES                         GA1TPGM 
00370 ******************************************************************GA1TPGM 
00371  01  GCPPDIO-PARM-AREA.                                           GA1TPGM 
00372  COPY GCPPDIOC.                                                   GA1TPGM 
00373                                                                   GA1TPGM 
00374  01  FILLER                      PIC X(29)  VALUE                 GA1TPGM 
00375                                 '*** ATTENTION IDENTIFIERS ***'.  GA1TPGM 
00376  COPY DFHAID.                                                     GA1TPGM 
00377                                                                   GA1TPGM 
00378  01  WS-END                               PIC X(58) VALUE         GA1TPGM 
00379      '***  GA1TPGM WORKING-STORAGE ENDS HERE  ***'.               GA1TPGM 
00380                                                                   GA1TPGM 
00381 /                   L I N K A G E    S E C T I O N                GA1TPGM 
00382  LINKAGE SECTION.                                                 GA1TPGM 
00383                                                                   GA1TPGM 
00384  01  DFHCOMMAREA.                                                 GA1TPGM 
00385  COPY G2ALCKEC.                                                   GA1TPGM 
00386      05  WS-COMMAREA-CDRS-REC                                     GA1TPGM 
00387          REDEFINES COMMAREA-ALL-LEV-TAB-RECORD.                   GA1TPGM 
00388          10  FILLER                      PIC X(97).               GA1TPGM 
00389          10  WS-PRIM-IND                 PIC X(01).               GA1TPGM 
00390          10  WS-PRIM-SEQ-NO-X            PIC X(02).               GA1TPGM 
00391          10  WS-PRIM-SEQ-NO                                       GA1TPGM 
00392              REDEFINES WS-PRIM-SEQ-NO-X  PIC 9(02).               GA1TPGM 
00393          10  WS-ADDL-IND                 PIC X(01).               GA1TPGM 
00394          10  WS-ADDL-SEQ-NO-X            PIC X(02).               GA1TPGM 
00395          10  WS-ADDL-SEQ-NO                                       GA1TPGM 
00396              REDEFINES WS-ADDL-SEQ-NO-X  PIC 9(02).               GA1TPGM 
00397          10  FILLER                      PIC X(47).               GA1TPGM 
00398                                                                   GA1TPGM 
00399  COPY GACDACWA.                                                   GA1TPGM 
00400                                                                   GA1TPGM 
00401 ***  05  INCOMING-COMMAREA-PNTR    USAGE IS POINTER.              GA1TPGM 
00402      05  GAS1UPD-PASSED-AREA.                                     GA1TPGM 
00403          07  LVL2-B-SW           PIC X.                           GA1TPGM 
00404          07  LVL2-F-SW           PIC X.                           GA1TPGM 
00405          07  LVL2-G-SW           PIC X.                           GA1TPGM 
00406          07  INTR-TAB-PGM-ID     PIC X(8).                        GA1TPGM 
00407          07  OCCURS-COUNTER      PIC 9(2).                        GA1TPGM 
00408          07  FILLER              PIC X(7).                        GA1TPGM 
00409      05  DELADD-OPTION           PIC X(7).                        GA1TPGM 
00410                                                                   GA1TPGM 
00411 ******************************************************************GA1TPGM 
00412 *    I/O PARM, WORKFILE KEY, AND #IRDX TABULAR RECORD             GA1TPGM 
00413 ******************************************************************GA1TPGM 
00414  01  IO-PARM-INTERNAL-IRDX-RECORD.                                GA1TPGM 
00415  COPY GCIOPRM1.                                                   GA1TPGM 
00416  COPY GCWRKDCC.                                                   GA1TPGM 
00417  COPY GCTIRDXC.                                                   GA1TPGM 
00418                                                                   GA1TPGM 
00419 ****************************************************************  GA1TPGM 
00420 *    COPY OF THE #IRDX TABULAR VARIABLE AREA                      GA1TPGM 
00421 *    THIS AREA IS USED IN SORTING PROCESS.                        GA1TPGM 
00422 ****************************************************************  GA1TPGM 
00423  01  COPY-TABULAR-TABLE-AREA.                                     GA1TPGM 
00424      05  COPY-TABULAR-TABLE         OCCURS 388 TIMES              GA1TPGM 
00425                                     INDEXED BY COPY-IDX.          GA1TPGM 
00426          10  COPY-DIAGNOSIS-FROM         PIC X(10).               GA1TPGM 
00427          10  COPY-DIAGNOSIS-TO           PIC X(10).               GA1TPGM 
00428                                                                   GA1TPGM 
00429 ****************************************************************  GA1TPGM 
00430 *    IO PARM, WITH WORKFILE KEY, AND #CDRS TABULAR RECORD         GA1TPGM 
00431 ****************************************************************  GA1TPGM 
00432  01  IO-PARM-CDRS-TABULAR-RECORD.                                 GA1TPGM 
00433  COPY GCIOPRM2.                                                   GA1TPGM 
00434  COPY GCWRKDC2.                                                   GA1TPGM 
00435  COPY GCTCDRSC.                                                   GA1TPGM 
00436 /                                                                 GA1TPGM 
00437  PROCEDURE DIVISION.                                              GA1TPGM 
00438                                                                   GA1TPGM 
00439 ******************************************************************GA1TPGM 
00440 *                      M A I N L I N E                            GA1TPGM 
00441 *                                                                 GA1TPGM 
00442 *   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONS GA1TPGM 
00443 *   TAKEN BY THE OPERATOR.                                        GA1TPGM 
00444 *   1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1TPGM 
00445 *      WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA1TPGM 
00446 *      ADDITIONS FROM.                                            GA1TPGM 
00447 *   2. RECEIVE THE SCREEN.                                        GA1TPGM 
00448 *   3. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1TPGM 
00449 *      MENU.                                                      GA1TPGM 
00450 *   4. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1TPGM 
00451 *      LOGIC.                                                     GA1TPGM 
00452 *   5. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1TPGM 
00453 *      (RETURN) TO THE ADD PROGRAM (GA2NPGM).                     GA1TPGM 
00454 *   6. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1TPGM 
00455 *      (RETURN) TO THE PREVIOUS MENU.                             GA1TPGM 
00456 *   7. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1TPGM 
00457 *      KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1TPGM 
00458 *                                                                 GA1TPGM 
00459 ******************************************************************GA1TPGM 
00460  1000-MAIN-LINE SECTION.                                          GA1TPGM 
00461      MOVE '1000'  TO  WS-PARA-ID.                                 GA1TPGM 
00462                                                                   GA1TPGM 
00463      IF EIBAID  =  DFHCLEAR                                       GA1TPGM 
00464         EXEC CICS SEND                                            GA1TPGM 
00465              FROM (WS-ONE-LOW) ERASE                              GA1TPGM 
00466         END-EXEC                                                  GA1TPGM 
00467         EXEC CICS                                                 GA1TPGM 
00468              RETURN                                               GA1TPGM 
00469         END-EXEC.                                                 GA1TPGM 
00470                                                                   GA1TPGM 
00471      IF EIBTRNID  NOT =  'GA1T'                                   GA1TPGM 
00472         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1TPGM 
00473         GO TO 1099-RETURN.                                        GA1TPGM 
00474                                                                   GA1TPGM 
00475      EXEC CICS RECEIVE                                            GA1TPGM 
00476           MAP    ('GA1TI01')                                      GA1TPGM 
00477           MAPSET ('GA1TSET')                                      GA1TPGM 
00478           INTO   (GA1TI01I)                                       GA1TPGM 
00479      END-EXEC.                                                    GA1TPGM 
00480                                                                   GA1TPGM 
00481      IF SCRNIDNI  NOT =  '0A1T00'                                 GA1TPGM 
00482         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1TPGM 
00483                                                                   GA1TPGM 
00484      IF EIBAID  =  DFHENTER                                       GA1TPGM 
00485         PERFORM 2000-DELETE-PROCESSING                            GA1TPGM 
00486         GO TO 1099-RETURN.                                        GA1TPGM 
00487                                                                   GA1TPGM 
00488      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1TPGM 
00489         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1TPGM 
00490                                                                   GA1TPGM 
00491      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1TPGM 
00492         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1TPGM 
00493                                                                   GA1TPGM 
00494      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1TPGM 
00495      MOVE -1                                                      GA1TPGM 
00496        TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).              GA1TPGM 
00497      SET WT-01-INDEX         TO +01.                              GA1TPGM 
00498      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1TPGM 
00499      EXEC CICS SEND                                               GA1TPGM 
00500           MAP    ('GA1TI01')                                      GA1TPGM 
00501           MAPSET ('GA1TSET') DATAONLY                             GA1TPGM 
00502           FROM   (GA1TI01O)  CURSOR                               GA1TPGM 
00503      END-EXEC.                                                    GA1TPGM 
00504      GO TO 1099-RETURN.                                           GA1TPGM 
00505                                                                   GA1TPGM 
00506  1000-EXIT.                                                       GA1TPGM 
00507      EXIT.                                                        GA1TPGM 
00508                                                                   GA1TPGM 
00509  1099-RETURN.                                                     GA1TPGM 
00510                                                                   GA1TPGM 
00511      EXEC CICS RETURN                                             GA1TPGM 
00512                TRANSID  ('GA1T')                                  GA1TPGM 
00513                COMMAREA (DFHCOMMAREA)                             GA1TPGM 
00514                LENGTH   (EIBCALEN)                                GA1TPGM 
00515                END-EXEC.                                          GA1TPGM 
00516                                                                   GA1TPGM 
00517      GOBACK.                                                      GA1TPGM 
00518                                                                   GA1TPGM 
00519  1099-EXIT.                                                       GA1TPGM 
00520      EXIT.                                                        GA1TPGM 
00521                                                                   GA1TPGM 
00522 ******************************************************************GA1TPGM 
00523 *               D E L E T E   P R O C E S S I N G                 GA1TPGM 
00524 *                                                                 GA1TPGM 
00525 *   WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1TPGM 
00526 *  1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1TPGM 
00527 *     VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1TPGM 
00528 *  2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1TPGM 
00529 *     (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1TPGM 
00530 *     BACK INTO THE RECORD THAT WE READ.)                         GA1TPGM 
00531 *  3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1TPGM 
00532 *     THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1TPGM 
00533 *     ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1TPGM 
00534 *  4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1TPGM 
00535 *     ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1TPGM 
00536 *  5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1TPGM 
00537 *     POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1TPGM 
00538 *     ENTRY.                                                      GA1TPGM 
00539 *  6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1TPGM 
00540 *     MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1TPGM 
00541 *     RECORD.                                                     GA1TPGM 
00542 *  7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1TPGM 
00543 *     NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1TPGM 
00544 *     DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1TPGM 
00545 *  8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1TPGM 
00546 *     ARE BYPASSED; WE READ THE ALL LEVEL INTERNAL TABULAR RECORD,GA1TPGM 
00547 *     SAVE THE NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO GA1TPGM 
00548 *     BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.     GA1TPGM 
00549 *                                                                 GA1TPGM 
00550 ******************************************************************GA1TPGM 
00551  2000-DELETE-PROCESSING SECTION.                                  GA1TPGM 
00552                                                                   GA1TPGM 
00553      MOVE '2000'  TO  WS-PARA-ID.                                 GA1TPGM 
00554      MOVE 'N'     TO  WS-ERROR-SW.                                GA1TPGM 
00555      MOVE ZERO    TO  WS-DELETE-COUNT.                            GA1TPGM 
00556      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1TPGM 
00557                                                                   GA1TPGM 
00558                                                                   GA1TPGM 
00559  2010-VALIDATE-ACT-CODE.                                          GA1TPGM 
00560                                                                   GA1TPGM 
00561      MOVE '2010'  TO  WS-PARA-ID.                                 GA1TPGM 
00562                                                                   GA1TPGM 
00563      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1TPGM 
00564         ADD 1  TO  WS-DELETE-COUNT.                               GA1TPGM 
00565                                                                   GA1TPGM 
00566      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  =  'D'              GA1TPGM 
00567                                        OR  SPACE  OR  LOW-VALUES  GA1TPGM 
00568         MOVE DFHBMUNF                                             GA1TPGM 
00569           TO MAP-ACTION-CODE-ATTR    (MAP-IDX1, MAP-IDX2)         GA1TPGM 
00570         MOVE DFHBMASF                                             GA1TPGM 
00571           TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)         GA1TPGM 
00572              MAP-DIAGNOSIS-TO-ATTR   (MAP-IDX1, MAP-IDX2)         GA1TPGM 
00573      ELSE                                                         GA1TPGM 
00574         MOVE DFHBMUBF                                             GA1TPGM 
00575           TO MAP-ACTION-CODE-ATTR    (MAP-IDX1, MAP-IDX2)         GA1TPGM 
00576         MOVE DFHBMABF                                             GA1TPGM 
00577           TO MAP-DIAGNOSIS-FROM-ATTR (MAP-IDX1, MAP-IDX2)         GA1TPGM 
00578              MAP-DIAGNOSIS-TO-ATTR   (MAP-IDX1, MAP-IDX2)         GA1TPGM 
00579         IF WS-ERROR-SW  NOT =  'Y'                                GA1TPGM 
00580            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1TPGM 
00581            MOVE 'Y' TO  WS-ERROR-SW.                              GA1TPGM 
00582                                                                   GA1TPGM 
00583      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1TPGM 
00584         SET MAP-IDX1   UP BY  1                                   GA1TPGM 
00585      ELSE                                                         GA1TPGM 
00586         IF MAP-IDX2  <  WS-MAP-COL                                GA1TPGM 
00587            SET MAP-IDX1  TO  1                                    GA1TPGM 
00588            SET MAP-IDX2  UP BY  1                                 GA1TPGM 
00589         ELSE                                                      GA1TPGM 
00590            GO TO 2020-DONE-VALIDATE-A-C.                          GA1TPGM 
00591                                                                   GA1TPGM 
00592      IF MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2) NOT =  LOW-VALUES GA1TPGM 
00593         GO TO 2010-VALIDATE-ACT-CODE.                             GA1TPGM 
00594                                                                   GA1TPGM 
00595                                                                   GA1TPGM 
00596                                                                   GA1TPGM 
00597  2020-DONE-VALIDATE-A-C.                                          GA1TPGM 
00598                                                                   GA1TPGM 
00599      MOVE '2020'  TO  WS-PARA-ID.                                 GA1TPGM 
00600      SET MAP-IDX1 TO  1.                                          GA1TPGM 
00601                                                                   GA1TPGM 
00602      IF WS-ERROR-SW  =  'Y'                                       GA1TPGM 
00603         SET WT-01-INDEX  TO +02                                   GA1TPGM 
00604         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1TPGM 
00605         MOVE LOW-VALUES  TO  FUNCTONO,  TTLELNEO,  SCRNIDNO,      GA1TPGM 
00606            ALTABIDO,  ALTBSLTO,  INTABIDO,  INTBSLTO, IDLINEO,    GA1TPGM 
00607            ADDELINO,  ALTBFNCO,  OENTCTRO,  FRMNUIDO, INCEXCO     GA1TPGM 
00608         MOVE '2100'  TO  WS-PARA-ID                               GA1TPGM 
00609         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1TPGM 
00610            VARYING MAP-IDX2 FROM  1  BY  1                        GA1TPGM 
00611              UNTIL MAP-IDX2  >  WS-MAP-COL                        GA1TPGM 
00612            AFTER   MAP-IDX1 FROM  1  BY  1                        GA1TPGM 
00613              UNTIL MAP-IDX1  >  WS-MAP-ROW                        GA1TPGM 
00614         EXEC CICS SEND                                            GA1TPGM 
00615                  MAP    ('GA1TI01')                               GA1TPGM 
00616                  MAPSET ('GA1TSET') DATAONLY                      GA1TPGM 
00617                  FROM   (GA1TI01O)  CURSOR                        GA1TPGM 
00618                  END-EXEC                                         GA1TPGM 
00619         GO TO 2099-EXIT.                                          GA1TPGM 
00620                                                                   GA1TPGM 
00621      COMPUTE WS-IO-PARM-WRK-IRDX-TAB-LEN         =                GA1TPGM 
00622              GC-GCIOPARM-LEN                     +                GA1TPGM 
00623                GC-WORKFILE-KEY-LEN               +                GA1TPGM 
00624                GC-GCTABULR-IRDX-FIXED-LEN        +                GA1TPGM 
00625                (GC-GCTABULR-IRDX-VARY-MAX-OCUR   *                GA1TPGM 
00626                       GC-GCTABULR-IRDX-VARY-LEN).                 GA1TPGM 
00627                                                                   GA1TPGM 
00628      EXEC CICS GETMAIN                                            GA1TPGM 
00629                SET     (ADDRESS OF IO-PARM-INTERNAL-IRDX-RECORD)  GA1TPGM 
00630                INITIMG (WS-HEX-00)                                GA1TPGM 
00631                LENGTH  (WS-IO-PARM-WRK-IRDX-TAB-LEN)              GA1TPGM 
00632                END-EXEC.                                          GA1TPGM 
00633                                                                   GA1TPGM 
00634      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1TPGM 
00635      MOVE  'C'                 TO GCIO-WRK-STATUS-CODE.           GA1TPGM 
00636      MOVE  'C3'                TO GCIO-WRK-RECORD-TYPE.           GA1TPGM 
00637      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1TPGM 
00638      MOVE CONTRACT-GROUP-NO    TO GCIO-WRK-GROUP-NUM.             GA1TPGM 
00639      MOVE CONTRACT-SECTION-NO  TO GCIO-WRK-SECTION-NUM.           GA1TPGM 
00640      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1TPGM 
00641      MOVE CONTRACT-LOB         TO GCIO-WRK-LINE-OF-BUS.           GA1TPGM 
00642      MOVE CONTRACT-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL.      GA1TPGM 
00643      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1TPGM 
00644      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1TPGM 
00645                                                                   GA1TPGM 
00646      MOVE  'GCPSWORK'          TO GCIO-FILE-DDNAME.               GA1TPGM 
00647      MOVE 1                    TO GCIO-IO-AREA-TO-USE.            GA1TPGM 
00648      MOVE ALTABIDI             TO GCIO-WRK-PROVISION-ID.          GA1TPGM 
00649      MOVE ALTBSLTI             TO GCIO-WRK-PROVISION-SLOT-NO.     GA1TPGM 
00650      MOVE INTABIDI             TO GCIO-WRK-TAB-PROVISION-ID.      GA1TPGM 
00651      MOVE INTBSLTI             TO GCIO-WRK-TAB-PROV-SLOT-NO.      GA1TPGM 
00652      MOVE GCIO-WORKFILE-KEY    TO GCIO-FILE-KEY.                  GA1TPGM 
00653                                                                   GA1TPGM 
00654      IF WS-DELETE-COUNT  =  ZERO                                  GA1TPGM 
00655         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1TPGM 
00656                                                                   GA1TPGM 
00657 *--                                                               GA1TPGM 
00658 *- WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.           GA1TPGM 
00659 *--                                                               GA1TPGM 
00660                                                                   GA1TPGM 
00661      MOVE GC-GCTABULR-IRDX-VARY-MAX-OCUR                          GA1TPGM 
00662        TO GXG-ENTRY-COUNT.                                        GA1TPGM 
00663      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1TPGM 
00664                                                                   GA1TPGM 
00665      EXEC CICS LINK                                               GA1TPGM 
00666                PROGRAM  ('GCIOPGM')                               GA1TPGM 
00667                COMMAREA (IO-PARM-INTERNAL-IRDX-RECORD)            GA1TPGM 
00668                LENGTH   (WS-IO-PARM-WRK-IRDX-TAB-LEN)             GA1TPGM 
00669                END-EXEC.                                          GA1TPGM 
00670                                                                   GA1TPGM 
00671      IF NOT  GCIO-GOOD-RETURN                                     GA1TPGM 
00672         SET WT-01-INDEX TO +03                                    GA1TPGM 
00673         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1TPGM 
00674         MOVE '1T01'     TO WS-ABEND-CODE                          GA1TPGM 
00675         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1TPGM 
00676                                                                   GA1TPGM 
00677      COMPUTE WS-COPY-LENGTH           =                           GA1TPGM 
00678              GXG-ENTRY-COUNT          *                           GA1TPGM 
00679              GC-GCTABULR-IRDX-VARY-LEN.                           GA1TPGM 
00680                                                                   GA1TPGM 
00681      EXEC CICS GETMAIN                                            GA1TPGM 
00682                SET     (ADDRESS OF COPY-TABULAR-TABLE-AREA)       GA1TPGM 
00683                LENGTH  (WS-COPY-LENGTH)                           GA1TPGM 
00684                INITIMG (WS-HEX-00)                                GA1TPGM 
00685                END-EXEC.                                          GA1TPGM 
00686                                                                   GA1TPGM 
00687      MOVE GXG-ENTRY-COUNT      TO  GXG-ENTRY-COUNT.               GA1TPGM 
00688      SET COPY-IDX,  GXG-INDEX  TO  1.                             GA1TPGM 
00689                                                                   GA1TPGM 
00690                                                                   GA1TPGM 
00691                                                                   GA1TPGM 
00692  2030-MAKE-A-COPY-OF-RECORD.                                      GA1TPGM 
00693                                                                   GA1TPGM 
00694      MOVE '2030'  TO  WS-PARA-ID.                                 GA1TPGM 
00695                                                                   GA1TPGM 
00696      IF GXG-INDEX  NOT >  GXG-ENTRY-COUNT                         GA1TPGM 
00697         MOVE GXG-ENTRY (GXG-INDEX)                                GA1TPGM 
00698           TO COPY-TABULAR-TABLE (COPY-IDX)                        GA1TPGM 
00699         SET COPY-IDX,  GXG-INDEX  UP BY  1                        GA1TPGM 
00700         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1TPGM 
00701                                                                   GA1TPGM 
00702                                                                   GA1TPGM 
00703                                                                   GA1TPGM 
00704      SET MAP-IDX1, MAP-IDX2, COPY-IDX,  GXG-INDEX  TO  1.         GA1TPGM 
00705      MOVE '2040'  TO  WS-PARA-ID.                                 GA1TPGM 
00706                                                                   GA1TPGM 
00707  2040-DELETE-MARKED-ENTRIES.                                      GA1TPGM 
00708                                                                   GA1TPGM 
00709      IF  MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)   =  LOW-VALUES  GA1TPGM 
00710       OR MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2)   =  LOW-VALUES  GA1TPGM 
00711          GO TO 2060-SAVE-REST-OF-COPY.                            GA1TPGM 
00712                                                                   GA1TPGM 
00713      IF  MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)   >              GA1TPGM 
00714                                COPY-DIAGNOSIS-FROM  (COPY-IDX)    GA1TPGM 
00715       OR MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2)   >              GA1TPGM 
00716                                COPY-DIAGNOSIS-TO    (COPY-IDX)    GA1TPGM 
00717          GO TO 2050-SAVE-COPIED-ENTRY                             GA1TPGM 
00718      ELSE                                                         GA1TPGM 
00719      IF MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)    <              GA1TPGM 
00720                                 COPY-DIAGNOSIS-FROM  (COPY-IDX)   GA1TPGM 
00721       OR MAP-DIAGNOSIS-TO  (MAP-IDX1, MAP-IDX2)    <              GA1TPGM 
00722                                 COPY-DIAGNOSIS-TO    (COPY-IDX)   GA1TPGM 
00723          SET WT-01-INDEX TO +04                                   GA1TPGM 
00724          MOVE '1T02'     TO  WS-ABEND-CODE                        GA1TPGM 
00725          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      GA1TPGM 
00726          PERFORM 9999-ERROR-MSG-THEN-ABEND.                       GA1TPGM 
00727                                                                   GA1TPGM 
00728      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1TPGM 
00729         IF MAP-IDX1   <  WS-MAP-ROW                               GA1TPGM 
00730            SET MAP-IDX1   UP BY  1                                GA1TPGM 
00731            GO TO 2050-SAVE-COPIED-ENTRY                           GA1TPGM 
00732         ELSE                                                      GA1TPGM 
00733         IF MAP-IDX2  <  WS-MAP-COL                                GA1TPGM 
00734            SET MAP-IDX1  TO  1                                    GA1TPGM 
00735            SET MAP-IDX2  UP BY  1                                 GA1TPGM 
00736            GO TO 2050-SAVE-COPIED-ENTRY                           GA1TPGM 
00737         ELSE                                                      GA1TPGM 
00738            GO TO 2060-SAVE-REST-OF-COPY.                          GA1TPGM 
00739                                                                   GA1TPGM 
00740      SET COPY-IDX  UP BY  1.                                      GA1TPGM 
00741                                                                   GA1TPGM 
00742      IF COPY-IDX  NOT <  GXG-ENTRY-COUNT                          GA1TPGM 
00743         MOVE COPY-TABULAR-TABLE (COPY-IDX)                        GA1TPGM 
00744           TO GXG-ENTRY (GXG-INDEX)                                GA1TPGM 
00745         SET  GXG-ENTRY-COUNT  TO  GXG-INDEX                       GA1TPGM 
00746         MOVE GXG-ENTRY-COUNT  TO  GXG-ENTRY-COUNT                 GA1TPGM 
00747         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1TPGM 
00748                                                                   GA1TPGM 
00749      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1TPGM 
00750         SET MAP-IDX1   UP BY  1                                   GA1TPGM 
00751         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1TPGM 
00752                                                                   GA1TPGM 
00753      IF MAP-IDX2  <  WS-MAP-COL                                   GA1TPGM 
00754         SET MAP-IDX1  TO  1                                       GA1TPGM 
00755         SET MAP-IDX2  UP BY  1                                    GA1TPGM 
00756         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1TPGM 
00757      ELSE                                                         GA1TPGM 
00758         GO TO 2060-SAVE-REST-OF-COPY.                             GA1TPGM 
00759                                                                   GA1TPGM 
00760                                                                   GA1TPGM 
00761                                                                   GA1TPGM 
00762  2050-SAVE-COPIED-ENTRY.                                          GA1TPGM 
00763                                                                   GA1TPGM 
00764      MOVE '2050'  TO  WS-PARA-ID.                                 GA1TPGM 
00765                                                                   GA1TPGM 
00766      MOVE COPY-TABULAR-TABLE (COPY-IDX)                           GA1TPGM 
00767        TO  GXG-ENTRY (GXG-INDEX).                                 GA1TPGM 
00768                                                                   GA1TPGM 
00769      SET GXG-INDEX  UP BY  1.                                     GA1TPGM 
00770      IF COPY-IDX  <  GXG-ENTRY-COUNT                              GA1TPGM 
00771         SET COPY-IDX  UP BY  1                                    GA1TPGM 
00772         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1TPGM 
00773      ELSE                                                         GA1TPGM 
00774 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1TPGM 
00775 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1TPGM 
00776 ***      THE TABLE OF ENTRIES.                                    GA1TPGM 
00777      SET WT-01-INDEX TO +05                                       GA1TPGM 
00778      MOVE '1T03'     TO WS-ABEND-CODE                             GA1TPGM 
00779      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                          GA1TPGM 
00780      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1TPGM 
00781                                                                   GA1TPGM 
00782                                                                   GA1TPGM 
00783                                                                   GA1TPGM 
00784  2060-SAVE-REST-OF-COPY.                                          GA1TPGM 
00785                                                                   GA1TPGM 
00786      MOVE '2060'  TO  WS-PARA-ID.                                 GA1TPGM 
00787                                                                   GA1TPGM 
00788      MOVE COPY-TABULAR-TABLE (COPY-IDX)                           GA1TPGM 
00789        TO GXG-ENTRY (GXG-INDEX).                                  GA1TPGM 
00790                                                                   GA1TPGM 
00791      SET GXG-INDEX  UP BY  1.                                     GA1TPGM 
00792      IF COPY-IDX  <  GXG-ENTRY-COUNT                              GA1TPGM 
00793         SET COPY-IDX  UP BY  1                                    GA1TPGM 
00794         GO TO 2060-SAVE-REST-OF-COPY.                             GA1TPGM 
00795                                                                   GA1TPGM 
00796      SET GXG-INDEX  DOWN BY  1.                                   GA1TPGM 
00797      SET GXG-ENTRY-COUNT  TO  GXG-INDEX.                          GA1TPGM 
00798      MOVE GXG-ENTRY-COUNT TO  GXG-ENTRY-COUNT.                    GA1TPGM 
00799                                                                   GA1TPGM 
00800                                                                   GA1TPGM 
00801                                                                   GA1TPGM 
00802  2070-UPDATE-MODIFIED-REC.                                        GA1TPGM 
00803                                                                   GA1TPGM 
00804      MOVE '2070'  TO  WS-PARA-ID.                                 GA1TPGM 
00805      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1TPGM 
00806      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1TPGM 
00807                                                                   GA1TPGM 
00808      COMPUTE  GCIO-RECORD-LENGTH      =                           GA1TPGM 
00809         GC-WORKFILE-KEY-LEN           +                           GA1TPGM 
00810         GC-GCTABULR-IRDX-FIXED-LEN    +                           GA1TPGM 
00811         (GXG-ENTRY-COUNT  *  GC-GCTABULR-IRDX-VARY-LEN).          GA1TPGM 
00812                                                                   GA1TPGM 
00813      COMPUTE  WS-IO-PARM-WRK-IRDX-TAB-LEN     =                   GA1TPGM 
00814               GC-GCIOPARM-LEN                 +                   GA1TPGM 
00815               GCIO-RECORD-LENGTH.                                 GA1TPGM 
00816                                                                   GA1TPGM 
00817      EXEC CICS LINK                                               GA1TPGM 
00818                PROGRAM  ('GCIOPGM')                               GA1TPGM 
00819                COMMAREA (IO-PARM-INTERNAL-IRDX-RECORD)            GA1TPGM 
00820                LENGTH   (WS-IO-PARM-WRK-IRDX-TAB-LEN)             GA1TPGM 
00821                END-EXEC.                                          GA1TPGM 
00822                                                                   GA1TPGM 
00823      IF GCIO-GOOD-RETURN                                          GA1TPGM 
00824         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1TPGM 
00825                                                                   GA1TPGM 
00826      SET WT-01-INDEX TO +06.                                      GA1TPGM 
00827      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1TPGM 
00828      MOVE '1T04'     TO WS-ABEND-CODE.                            GA1TPGM 
00829      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1TPGM 
00830                                                                   GA1TPGM 
00831                                                                   GA1TPGM 
00832                                                                   GA1TPGM 
00833  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1TPGM 
00834                                                                   GA1TPGM 
00835      MOVE  '2080'  TO  WS-PARA-ID.                                GA1TPGM 
00836      MOVE  'RD '   TO  GCIO-FILE-ACCESS-CODE.                     GA1TPGM 
00837      MOVE GC-GCTABULR-IRDX-VARY-MAX-OCUR                          GA1TPGM 
00838        TO GXG-ENTRY-COUNT.                                        GA1TPGM 
00839                                                                   GA1TPGM 
00840      EXEC CICS LINK                                               GA1TPGM 
00841                PROGRAM  ('GCIOPGM')                               GA1TPGM 
00842                COMMAREA (IO-PARM-INTERNAL-IRDX-RECORD)            GA1TPGM 
00843                LENGTH   (WS-IO-PARM-WRK-IRDX-TAB-LEN)             GA1TPGM 
00844                END-EXEC.                                          GA1TPGM 
00845                                                                   GA1TPGM 
00846      IF GCIO-GOOD-RETURN                                          GA1TPGM 
00847         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1TPGM 
00848                                                                   GA1TPGM 
00849      SET WT-01-INDEX TO +07.                                      GA1TPGM 
00850      MOVE '1T05'     TO WS-ABEND-CODE.                            GA1TPGM 
00851      PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                         GA1TPGM 
00852      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1TPGM 
00853                                                                   GA1TPGM 
00854                                                                   GA1TPGM 
00855                                                                   GA1TPGM 
00856  2090-BUILD-NEXT-DISPLAY.                                         GA1TPGM 
00857                                                                   GA1TPGM 
00858      MOVE  '2090'   TO  WS-PARA-ID.                               GA1TPGM 
00859      SET MAP-IDX1   TO  WS-MAP-ROW.                               GA1TPGM 
00860      SET MAP-IDX2   TO  WS-MAP-COL.                               GA1TPGM 
00861      SET GXG-INDEX  TO  1.                                        GA1TPGM 
00862                                                                   GA1TPGM 
00863      IF MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2) = LOW-VALUES      GA1TPGM 
00864         MOVE GXG-ENTRY (GXG-INDEX)  TO  WS-SAVED-RANGE            GA1TPGM 
00865      ELSE                                                         GA1TPGM 
00866      MOVE MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)                 GA1TPGM 
00867        TO WS-SAVED-FROM                                           GA1TPGM 
00868      MOVE MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2)                 GA1TPGM 
00869        TO WS-SAVED-TO.                                            GA1TPGM 
00870                                                                   GA1TPGM 
00871      PERFORM 4500-FILL-THE-SCREEN.                                GA1TPGM 
00872                                                                   GA1TPGM 
00873      EXEC CICS SEND                                               GA1TPGM 
00874                MAP    ('GA1TI01')                                 GA1TPGM 
00875                MAPSET ('GA1TSET') ERASE                           GA1TPGM 
00876                FROM   (GA1TI01O)                                  GA1TPGM 
00877                END-EXEC.                                          GA1TPGM 
00878                                                                   GA1TPGM 
00879  2099-EXIT.                                                       GA1TPGM 
00880      EXIT.                                                        GA1TPGM 
00881 /*****************************************************************GA1TPGM 
00882 *                                                                *GA1TPGM 
00883 ******************************************************************GA1TPGM 
00884  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1TPGM 
00885                                                                   GA1TPGM 
00886      MOVE  '2100'    TO WS-PARA-ID.                               GA1TPGM 
00887      MOVE LOW-VALUES TO MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),     GA1TPGM 
00888                         MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2),  GA1TPGM 
00889                         MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2).  GA1TPGM 
00890  2199-EXIT.                                                       GA1TPGM 
00891      EXIT.                                                        GA1TPGM 
00892 /*****************************************************************GA1TPGM 
00893 *           X C T L   T O   A D D   S C R E E N                   GA1TPGM 
00894 *                                                                 GA1TPGM 
00895 *  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO   GA1TPGM 
00896 *  ADDING ENTRIES.  WE READ THE ALL LEVEL INTERNAL TABULAR & PASS GA1TPGM 
00897 *  THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL      GA1TPGM 
00898 *  TABULAR RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE GA1TPGM 
00899 *  PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA1TPGM 
00900 *  IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1TPGM 
00901 ******************************************************************GA1TPGM 
00902  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1TPGM 
00903                                                                   GA1TPGM 
00904      MOVE '3000'     TO  WS-PARA-ID.                              GA1TPGM 
00905      MOVE  ALTABIDI  TO  GCA-ALL-LEVEL-TAB-ID.                    GA1TPGM 
00906      MOVE  ALTBSLTI  TO  GCA-ALL-LEVEL-TAB-SLOT.                  GA1TPGM 
00907      MOVE  INTABIDI  TO  GCA-INTERNAL-TAB-ID.                     GA1TPGM 
00908      MOVE  INTBSLTI  TO  GCA-INTERNAL-TAB-SLOT.                   GA1TPGM 
00909      MOVE  ADDELINI  TO  GCA-ADD-DEL-IND.                         GA1TPGM 
00910      MOVE  ALTBFNCI  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE.             GA1TPGM 
00911      MOVE  OENTCTRI  TO  GCA-OCCURS-ENTRY-COUNTER.                GA1TPGM 
00912      MOVE  FRMNUIDI  TO  GCA-FROM-MENU-ID.                        GA1TPGM 
00913      MOVE  INCEXCI   TO  GCA-I-E-INDC.                            GA1TPGM 
00914                                                                   GA1TPGM 
00915      EXEC CICS XCTL                                               GA1TPGM 
00916                PROGRAM  ('GA2TPGM')                               GA1TPGM 
00917                COMMAREA (DFHCOMMAREA)                             GA1TPGM 
00918                LENGTH   (LENGTH OF DFHCOMMAREA)                   GA1TPGM 
00919                END-EXEC.                                          GA1TPGM 
00920                                                                   GA1TPGM 
00921  3099-EXIT.                                                       GA1TPGM 
00922      EXIT.                                                        GA1TPGM 
00923 /*****************************************************************GA1TPGM 
00924 *           D I S P L A Y   F I R S T   S C R E E N               GA1TPGM 
00925 *                                                                 GA1TPGM 
00926 *  THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU OR  GA1TPGM 
00927 *  THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1TPGM 
00928 *  ALL LEVEL INTERNAL TABULAR RECORD & PASS US THE RECORD         GA1TPGM 
00929 *  (PRECEEDED BY I/O PARMS AND WORKFILE KEY).  WE WILL THEN USE   GA1TPGM 
00930 *  THAT RECORD TO BUILD THE SCREEN IMAGE.                         GA1TPGM 
00931 *  THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN,   GA1TPGM 
00932 *  (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1TPGM 
00933 *  SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1TPGM 
00934 *  WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1TPGM 
00935 *  DISPLAYED THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,   GA1TPGM 
00936 *  AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1TPGM 
00937 *  DETERMINATION OF APPROPRIATE ACTION.                           GA1TPGM 
00938 ******************************************************************GA1TPGM 
00939  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1TPGM 
00940                                                                   GA1TPGM 
00941      MOVE '4000'     TO  WS-PARA-ID.                              GA1TPGM 
00942      MOVE LOW-VALUES TO  GA1TI01I.                                GA1TPGM 
00943                                                                   GA1TPGM 
00944      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA1TPGM 
00945         SET WT-01-INDEX TO +08                                    GA1TPGM 
00946         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1TPGM 
00947         MOVE '1T06'     TO WS-ABEND-CODE                          GA1TPGM 
00948         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1TPGM 
00949                                                                   GA1TPGM 
00950      SET ADDRESS OF IO-PARM-INTERNAL-IRDX-RECORD                  GA1TPGM 
00951       TO GCA-RECORD-POINTER.                                      GA1TPGM 
00952                                                                   GA1TPGM 
00953      MOVE GCA-ALL-LEVEL-TAB-ID         TO ALTABIDO.               GA1TPGM 
00954      MOVE GCA-ALL-LEVEL-TAB-SLOT       TO ALTBSLTO.               GA1TPGM 
00955      MOVE GCA-INTERNAL-TAB-ID          TO INTABIDO.               GA1TPGM 
00956      MOVE GCA-INTERNAL-TAB-SLOT        TO INTBSLTO.               GA1TPGM 
00957      MOVE GCA-ADD-DEL-IND              TO ADDELINO.               GA1TPGM 
00958      MOVE GCA-ALL-LEVEL-TAB-FUNC-CODE  TO ALTBFNCO.               GA1TPGM 
00959      MOVE GCA-OCCURS-ENTRY-COUNTER     TO OENTCTRO.               GA1TPGM 
00960      MOVE GCA-FROM-MENU-ID             TO FRMNUIDO.               GA1TPGM 
00961      MOVE GXG-INCLUDE-EXCLUDE-IND      TO GCA-I-E-INDC.           GA1TPGM 
00962      MOVE GCA-I-E-INDC                 TO INCEXCO.                GA1TPGM 
00963      MOVE WS-DEL-LITERAL               TO FUNCLITO.               GA1TPGM 
00964                                                                   GA1TPGM 
00965      MOVE CONTRACT-TITLE-LINE       TO TTLELNEO.                  GA1TPGM 
00966      MOVE 'PLN: '                   TO CONTRACT-PLAN-HEADING.     GA1TPGM 
00967      MOVE GCA-PLAN-CODE             TO CONTRACT-PLAN-CODE.        GA1TPGM 
00968      MOVE ' GRP: '                  TO CONTRACT-GROUP-HEADING.    GA1TPGM 
00969      MOVE GCA-GROUP-NUM             TO CONTRACT-GROUP-NO.         GA1TPGM 
00970      MOVE ' SEC: '                  TO CONTRACT-SECTION-HEADING.  GA1TPGM 
00971      MOVE GCA-SECTION-NUM           TO CONTRACT-SECTION-NO.       GA1TPGM 
00972      MOVE ' PKG: '                  TO CONTRACT-PKG-HEADING.      GA1TPGM 
00973      MOVE GCA-PKG-CODE              TO CONTRACT-PKG-CODE.         GA1TPGM 
00974      MOVE ' LOB: '                  TO CONTRACT-LOB-HEADING.      GA1TPGM 
00975      MOVE GCA-L-O-B                 TO CONTRACT-LOB.              GA1TPGM 
00976      MOVE ' PRV: '                  TO CONTRACT-PROV-CTL-HEADING. GA1TPGM 
00977      MOVE GCA-PROV-CTL              TO CONTRACT-PROV-CTL.         GA1TPGM 
00978      MOVE ' FR: '                   TO CONTRACT-FAM-REL-HEADING.  GA1TPGM 
00979      MOVE GCA-FAM-REL-LVL           TO CONTRACT-FAM-REL-LVL.      GA1TPGM 
00980      MOVE ' EFDT: '                 TO CONTRACT-EFF-DT-HEADING.   GA1TPGM 
00981      MOVE GCA-EFFECTIVE-DATE        TO CONTRACT-EFF-DATE.         GA1TPGM 
00982                                                                   GA1TPGM 
00983                                                                   GA1TPGM 
00984      SET GXG-INDEX              TO 1.                             GA1TPGM 
00985      MOVE GXG-ENTRY (GXG-INDEX) TO WS-SAVED-RANGE.                GA1TPGM 
00986                                                                   GA1TPGM 
00987      PERFORM 4500-FILL-THE-SCREEN.                                GA1TPGM 
00988                                                                   GA1TPGM 
00989      EXEC CICS SEND                                               GA1TPGM 
00990                MAP    ('GA1TI01')                                 GA1TPGM 
00991                MAPSET ('GA1TSET') ERASE                           GA1TPGM 
00992                FROM   (GA1TI01O)                                  GA1TPGM 
00993                END-EXEC.                                          GA1TPGM 
00994                                                                   GA1TPGM 
00995  4099-EXIT.                                                       GA1TPGM 
00996      EXIT.                                                        GA1TPGM 
00997 /**************************************************************** GA1TPGM 
00998 *              F I L L   T H E   S C R E E N                      GA1TPGM 
00999 *                                                                 GA1TPGM 
01000 *  THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO   GA1TPGM 
01001 *  BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1TPGM 
01002 *  ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1TPGM 
01003 *  FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1TPGM 
01004 *  THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1TPGM 
01005 ******************************************************************GA1TPGM 
01006  4500-FILL-THE-SCREEN SECTION.                                    GA1TPGM 
01007                                                                   GA1TPGM 
01008      MOVE '4500'              TO  WS-PARA-ID.                     GA1TPGM 
01009      MOVE  GXG-ENTRY-COUNT    TO  GXG-ENTRY-COUNT.                GA1TPGM 
01010      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1TPGM 
01011                                                                   GA1TPGM 
01012      IF GXG-ENTRY-COUNT  NOT >  1                                 GA1TPGM 
01013         MOVE '4530'  TO  WS-PARA-ID                               GA1TPGM 
01014         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1TPGM 
01015                                                                   GA1TPGM 
01016                                                                   GA1TPGM 
01017      SET GXG-INDEX  TO  1.                                        GA1TPGM 
01018      MOVE '4510'  TO  WS-PARA-ID.                                 GA1TPGM 
01019                                                                   GA1TPGM 
01020  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1TPGM 
01021                                                                   GA1TPGM 
01022      IF GXG-ENTRY (GXG-INDEX) < WS-SAVED-RANGE                    GA1TPGM 
01023         SET GXG-INDEX  UP BY  1                                   GA1TPGM 
01024         IF GXG-INDEX  <  GXG-ENTRY-COUNT                          GA1TPGM 
01025            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1TPGM 
01026         ELSE                                                      GA1TPGM 
01027            SET GXG-INDEX  TO  1.                                  GA1TPGM 
01028                                                                   GA1TPGM 
01029                                                                   GA1TPGM 
01030                                                                   GA1TPGM 
01031      MOVE '4520'  TO  WS-PARA-ID.                                 GA1TPGM 
01032                                                                   GA1TPGM 
01033  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1TPGM 
01034                                                                   GA1TPGM 
01035      MOVE DFHBMUNF                                                GA1TPGM 
01036        TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).             GA1TPGM 
01037      MOVE LOW-VALUES                                              GA1TPGM 
01038        TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).                  GA1TPGM 
01039      MOVE GXG-DIAGNOSIS-FROM (GXG-INDEX)                          GA1TPGM 
01040        TO MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2)                 GA1TPGM 
01041      MOVE GXG-DIAGNOSIS-TO   (GXG-INDEX)                          GA1TPGM 
01042        TO MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2).                GA1TPGM 
01043                                                                   GA1TPGM 
01044      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1TPGM 
01045         SET  MAP-IDX1  UP BY  1                                   GA1TPGM 
01046      ELSE                                                         GA1TPGM 
01047      IF MAP-IDX2  <  WS-MAP-COL                                   GA1TPGM 
01048         SET  MAP-IDX1  TO  1                                      GA1TPGM 
01049         SET  MAP-IDX2  UP BY  1                                   GA1TPGM 
01050      ELSE                                                         GA1TPGM 
01051      GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                         GA1TPGM 
01052                                                                   GA1TPGM 
01053      IF GXG-INDEX  <  (GXG-ENTRY-COUNT - 1 )                      GA1TPGM 
01054         SET  GXG-INDEX  UP BY  1                                  GA1TPGM 
01055         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1TPGM 
01056                                                                   GA1TPGM 
01057                                                                   GA1TPGM 
01058                                                                   GA1TPGM 
01059  4530-FILL-REST-WITH-NULLS.                                       GA1TPGM 
01060                                                                   GA1TPGM 
01061      MOVE '4530'  TO  WS-PARA-ID.                                 GA1TPGM 
01062      MOVE DFHBMASK                                                GA1TPGM 
01063        TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).             GA1TPGM 
01064      MOVE LOW-VALUES TO MAP-ACTION-CODE    (MAP-IDX1, MAP-IDX2),  GA1TPGM 
01065                         MAP-DIAGNOSIS-FROM (MAP-IDX1, MAP-IDX2),  GA1TPGM 
01066                         MAP-DIAGNOSIS-TO   (MAP-IDX1, MAP-IDX2).  GA1TPGM 
01067                                                                   GA1TPGM 
01068      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1TPGM 
01069         SET  MAP-IDX1   UP BY  1                                  GA1TPGM 
01070         GO TO 4530-FILL-REST-WITH-NULLS                           GA1TPGM 
01071      ELSE                                                         GA1TPGM 
01072      IF MAP-IDX2  <  WS-MAP-COL                                   GA1TPGM 
01073         SET  MAP-IDX1  TO  1                                      GA1TPGM 
01074         SET  MAP-IDX2  UP BY 1                                    GA1TPGM 
01075         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1TPGM 
01076                                                                   GA1TPGM 
01077                                                                   GA1TPGM 
01078                                                                   GA1TPGM 
01079  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1TPGM 
01080                                                                   GA1TPGM 
01081      MOVE '4540'  TO  WS-PARA-ID.                                 GA1TPGM 
01082                                                                   GA1TPGM 
01083      IF GXG-ENTRY-COUNT  =  1                                     GA1TPGM 
01084         SET WT-01-INDEX TO +10                                    GA1TPGM 
01085         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1TPGM 
01086         GO TO 4599-EXIT.                                          GA1TPGM 
01087                                                                   GA1TPGM 
01088      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1TPGM 
01089         SET WT-01-INDEX TO +11                                    GA1TPGM 
01090         PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                      GA1TPGM 
01091                                                                   GA1TPGM 
01092  4599-EXIT.                                                       GA1TPGM 
01093      EXIT.                                                        GA1TPGM 
01094                                                                   GA1TPGM 
01095 /**************************************************************** GA1TPGM 
01096 *         X C T L   T O   P R E V I O U S   M E N U               GA1TPGM 
01097 *                                                                 GA1TPGM 
01098 *   THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA1TPGM 
01099 *  ORIGINATED FROM.  WE READ THE ALL LEVEL INTERNAL TABULAR       GA1TPGM 
01100 *  RECORD AND PASS IT PRECEEDED BY THE WORKFILE KEY TO            GA1TPGM 
01101 *  THE CORRECT ORIGINATING PROGRAM (DETERMINED BY THE CODE        GA1TPGM 
01102 *  IN THE 'ALL LEVEL TABULAR FUNCTION CODE' FIELD).               GA1TPGM 
01103 *                                                                 GA1TPGM 
01104 ******************************************************************GA1TPGM 
01105  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1TPGM 
01106                                                                   GA1TPGM 
01107      MOVE '5000'  TO  WS-PARA-ID.                                 GA1TPGM 
01108                                                                   GA1TPGM 
01109 *******                                                           GA1TPGM 
01110 * STS *  RETURN TO SINGLE TABULAR SUPPORT MENU, NO COMMAREA       GA1TPGM 
01111 *******                                                           GA1TPGM 
01112                                                                   GA1TPGM 
01113      IF  ALTBFNCI  =  'GTM1'    AND                               GA1TPGM 
01114          ALTABIDI  =  'STS000'                                    GA1TPGM 
01115          EXEC CICS XCTL  PROGRAM ('GTM1PGM')  END-EXEC.           GA1TPGM 
01116                                                                   GA1TPGM 
01117                                                                   GA1TPGM 
01118       COMPUTE  WS-IO-PARM-WRK-CDRS-LEN           =                GA1TPGM 
01119                GC-GCIOPARM-LEN                   +                GA1TPGM 
01120                GC-WORKFILE-KEY-LEN               +                GA1TPGM 
01121                GC-GCTABULR-CDRS-FIXED-LEN        +                GA1TPGM 
01122                ( GC-GCTABULR-CDRS-VARY-MAX-OCUR  *                GA1TPGM 
01123                  GC-GCTABULR-CDRS-VARY-LEN ).                     GA1TPGM 
01124                                                                   GA1TPGM 
01125      EXEC CICS GETMAIN                                            GA1TPGM 
01126                SET     (ADDRESS OF IO-PARM-CDRS-TABULAR-RECORD)   GA1TPGM 
01127                INITIMG (WS-HEX-00)                                GA1TPGM 
01128                LENGTH  (WS-IO-PARM-WRK-CDRS-LEN)                  GA1TPGM 
01129                END-EXEC.                                          GA1TPGM 
01130                                                                   GA1TPGM 
01131      MOVE SPACES                 TO GCIO-WORKFILE-KEY.            GA1TPGM 
01132      MOVE  'C'                   TO GCIO-WRK-STATUS-CODE.         GA1TPGM 
01133      MOVE  'C3'                  TO GCIO-WRK-RECORD-TYPE.         GA1TPGM 
01134      MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           GA1TPGM 
01135      MOVE GCA-GROUP-NUM          TO GCIO-WRK-GROUP-NUM.           GA1TPGM 
01136      MOVE GCA-SECTION-NUM        TO GCIO-WRK-SECTION-NUM.         GA1TPGM 
01137      MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE.            GA1TPGM 
01138      MOVE GCA-L-O-B              TO GCIO-WRK-LINE-OF-BUS.         GA1TPGM 
01139      MOVE GCA-PROV-CTL           TO GCIO-WRK-PROVIDER-CONTROL.    GA1TPGM 
01140      MOVE GCA-FAM-REL-LVL        TO GCIO-WRK-FAMILY-RELATION-LVL. GA1TPGM 
01141      MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN.           GA1TPGM 
01142                                                                   GA1TPGM 
01143      MOVE SPACES                 TO GCA-BEN-PROV-ID.              GA1TPGM 
01144      MOVE ALTABIDI               TO GCIO-WRK-PROVISION-ID         GA1TPGM 
01145                                     GCA-ALL-LEVEL-TAB-ID.         GA1TPGM 
01146      MOVE ALTBSLTI               TO GCIO-WRK-PROVISION-SLOT-NO    GA1TPGM 
01147                                     GCA-ALL-LEVEL-TAB-SLOT.       GA1TPGM 
01148                                                                   GA1TPGM 
01149                                                                   GA1TPGM 
01150      MOVE SPACES                 TO  WS-INTERNAL-ID               GA1TPGM 
01151      MOVE ZEROES                 TO  WS-INTERNAL-SLOT-NO          GA1TPGM 
01152      MOVE INTABIDI               TO  WS-INTERNAL-ID               GA1TPGM 
01153      MOVE INTBSLTI               TO  WS-INTERNAL-SLOT-NO          GA1TPGM 
01154                                                                   GA1TPGM 
01155                                                                   GA1TPGM 
01156      MOVE SPACES                 TO GCIO-WRK-TAB-PROVISION-ID     GA1TPGM 
01157                                     GCA-INTERNAL-TAB-ID           GA1TPGM 
01158                                     GCA-INTERNAL-TAB-SLOT.        GA1TPGM 
01159      MOVE ZEROES                 TO GCIO-WRK-TAB-PROV-SLOT-NO.    GA1TPGM 
01160                                                                   GA1TPGM 
01161      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GA1TPGM 
01162      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GA1TPGM 
01163                                                                   GA1TPGM 
01164      SET  GCA-RECORD-POINTER                                      GA1TPGM 
01165       TO  ADDRESS OF IO-PARM-CDRS-TABULAR-RECORD.                 GA1TPGM 
01166                                                                   GA1TPGM 
01167      MOVE GC-GCTABULR-CDRS-VARY-MAX-OCUR  TO GTE-ENTRY-COUNT.     GA1TPGM 
01168                                                                   GA1TPGM 
01169      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1TPGM 
01170                                                                   GA1TPGM 
01171      EXEC CICS LINK                                               GA1TPGM 
01172                PROGRAM  ('GCIOPGM')                               GA1TPGM 
01173                COMMAREA (IO-PARM-CDRS-TABULAR-RECORD)             GA1TPGM 
01174                LENGTH   (WS-IO-PARM-WRK-CDRS-LEN)                 GA1TPGM 
01175      END-EXEC.                                                    GA1TPGM 
01176                                                                   GA1TPGM 
01177      IF NOT GCIO2-GOOD-RETURN                                     GA1TPGM 
01178         SET WT-01-INDEX TO +03                                    GA1TPGM 
01179         PERFORM 9000-000-MOVE-MSG-TO-SCREEN                       GA1TPGM 
01180         MOVE '1T08'     TO WS-ABEND-CODE                          GA1TPGM 
01181         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1TPGM 
01182                                                                   GA1TPGM 
01183                                                                   GA1TPGM 
01184 *** FIND SEQUENCE NUMBER WITH #IRDX PRIMARY DRIVER       ***      GA1TPGM 
01185                                                                   GA1TPGM 
01186 *    IF  GCA-FROM-MENU-ID  =  'GC4G'                              GA1TPGM 
01187 *        SEARCH GTE-PRIMARY-ENTRY                                 GA1TPGM 
01188 *            AT END                                               GA1TPGM 
01189 *                SET GTE-INDEX TO 1                               GA1TPGM 
01190 *            WHEN GTE-PRIM-SEQ-NO (GTE-INDEX)    = WS-PRIM-SEQ-NO GA1TPGM 
01191 *                 MOVE 'Y'   TO WS-PRIM-IND                       GA1TPGM 
01192 *                 MOVE SPACE TO WS-ADDL-IND.                      GA1TPGM 
01193                                                                   GA1TPGM 
01194      IF  GCA-FROM-MENU-ID  =  'GC4G'                              GA1TPGM 
01195          SEARCH GTE-PRIMARY-ENTRY                                 GA1TPGM 
01196              AT END                                               GA1TPGM 
01197                  SET GTE-INDEX TO 1                               GA1TPGM 
01198              WHEN ((GTE-PRIMARY-DRIVER (GTE-INDEX) = '#IRDX ')    GA1TPGM 
01199                     AND                                           GA1TPGM 
01200                    (GTE-PRIMARY-SLOT-NO (GTE-INDEX) =             GA1TPGM 
01201                     WS-INTERNAL-SLOT-NO))                         GA1TPGM 
01202                   MOVE 'Y'   TO WS-PRIM-IND                       GA1TPGM 
01203                   MOVE SPACE TO WS-ADDL-IND                       GA1TPGM 
01204                   MOVE GTE-PRIM-SEQ-NO (GTE-INDEX)                GA1TPGM 
01205                     TO WS-PRIM-SEQ-NO.                            GA1TPGM 
01206                                                                   GA1TPGM 
01207      EXEC CICS XCTL                                               GA1TPGM 
01208                PROGRAM  ('GC4HPGM')                               GA1TPGM 
01209                COMMAREA (DFHCOMMAREA)                             GA1TPGM 
01210                LENGTH   (LENGTH OF DFHCOMMAREA)                   GA1TPGM 
01211                END-EXEC.                                          GA1TPGM 
01212                                                                   GA1TPGM 
01213  5099-EXIT.                                                       GA1TPGM 
01214      EXIT.                                                        GA1TPGM 
01215 /**************************************************************** GA1TPGM 
01216 *            X C T L   T O   M A I N   M E N U                    GA1TPGM 
01217 *                                                                 GA1TPGM 
01218 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1TPGM 
01219 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1TPGM 
01220 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1TPGM 
01221 *  PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA1TPGM 
01222 *  PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1TPGM 
01223 *  AND PROGRESS DOWN.                                             GA1TPGM 
01224 ******************************************************************GA1TPGM 
01225  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1TPGM 
01226                                                                   GA1TPGM 
01227      MOVE '6000'  TO  WS-PARA-ID.                                 GA1TPGM 
01228      MOVE '1T09'  TO  WS-ABEND-CODE.                              GA1TPGM 
01229                                                                   GA1TPGM 
01230      EXEC CICS XCTL                                               GA1TPGM 
01231                PROGRAM ('GCPSPGM')                                GA1TPGM 
01232                END-EXEC.                                          GA1TPGM 
01233                                                                   GA1TPGM 
01234  6099-EXIT.                                                       GA1TPGM 
01235      EXIT.                                                        GA1TPGM 
01236 /***************************************************************  GA1TPGM 
01237 *                                                              *  GA1TPGM 
01238 * 9000   MOVE MESSAGE TO SCREEN                                *  GA1TPGM 
01239 *                                                              *  GA1TPGM 
01240 ****************************************************************  GA1TPGM 
01241  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          GA1TPGM 
01242  9000-010.                                                        GA1TPGM 
01243                                                                   GA1TPGM 
01244      MOVE '9000'  TO  WS-PARA-ID.                                 GA1TPGM 
01245      MOVE WT-01-MESSAGE-TEXT (WT-01-INDEX)                        GA1TPGM 
01246        TO ERRMSGO.                                                GA1TPGM 
01247                                                                   GA1TPGM 
01248  9000-900-EXIT.                                                   GA1TPGM 
01249      EXIT.                                                        GA1TPGM 
01250 /*****************************************************************GA1TPGM 
01251 *                                                                *GA1TPGM 
01252 ******************************************************************GA1TPGM 
01253  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1TPGM 
01254                                                                   GA1TPGM 
01255      MOVE '9999'    TO  WS-PARA-ID.                               GA1TPGM 
01256      SET MAP-IDX1   TO 7.                                         GA1TPGM 
01257      SET MAP-IDX2   TO 1.                                         GA1TPGM 
01258      MOVE -1        TO MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).  GA1TPGM 
01259                                                                   GA1TPGM 
01260      EXEC CICS SEND                                               GA1TPGM 
01261                MAP    ('GA1TI01')                                 GA1TPGM 
01262                MAPSET ('GA1TSET') ERASE                           GA1TPGM 
01263                FROM   (GA1TI01O)  WAIT                            GA1TPGM 
01264                END-EXEC.                                          GA1TPGM 
01265                                                                   GA1TPGM 
01266      EXEC CICS ABEND                                              GA1TPGM 
01267                ABCODE (WS-ABEND-CODE)                             GA1TPGM 
01268                END-EXEC.                                          GA1TPGM 
01269  9999-EXIT.                                                       GA1TPGM 
01270      EXIT.                                                        GA1TPGM 
