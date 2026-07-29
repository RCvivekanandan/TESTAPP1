00001  ID DIVISION.                                                     09/03/03
00002  PROGRAM-ID.     GA1KPGM.                                         GA1KPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV003
00004  AUTHOR.         S BUCH.                                          GA1KPGM 
00005  DATE-WRITTEN.   02/14/85.                                        GA1KPGM 
00006  DATE-COMPILED.                                                   GA1KPGM 
00007      SKIP3                                                        GA1KPGM 
00008 ******************************************************************GA1KPGM 
00009 *                      REVISIONS                                  GA1KPGM 
00010 ******************************************************************GA1KPGM 
00011 *   DATE    BY   DESCRIPTION                                      GA1KPGM 
00012 * --------  ---  -------------------------------------------------GA1KPGM 
00013 * 01/16/86  ENW  CHANGED WS-CONTRACT-FIXED-PORTION FROM 483 TO 563GA1KPGM 
00014 *                        WS-CONTRACT-KEY-LENGTH    FROM  12 TO  10GA1KPGM 
00015 *                        WS-CONTRACT-MAX-OCCURS    FROM 450 TO 520GA1KPGM 
00016 * 08/27/86  JLA  1. ADD SUPPORT FOR PF7,8,10 AND 11.              GA1KPGM 
00017 *  (D136)        2. ADD  SELECT FIELD.                            GA1KPGM 
00018 *                3. ADD LOCATION COUNTERS(I.E 1 TO 36             GA1KPGM 
00019 *                   OF 54 PROCEDURES DISPLAYED) TO                GA1KPGM 
00020 *                   SCREEN.                                       GA1KPGM 
00021 *                4. USE USER DEFINED LOGICAL MAP FOR              GA1KPGM 
00022 *                   SCREEN.  THIS REPLACES THE PARTIAL            GA1KPGM 
00023 *                   USE OF BMS MAP AND USER DEFINED.              GA1KPGM 
00024 *                                                                 GA1KPGM 
00025 * 12/18/86  DES  FIX THE DELETE SO THAT IF THERE ARE TWO ENTRIES  GA1KPGM 
00026 *  (D136)        WITH THE SAME PROCEDURE AND THE SECOND ONE IS    GA1KPGM 
00027 *                DELETED THE PROGRAM WILL FIND THE RIGHT ONE      GA1KPGM 
00028 *                                                                 GA1KPGM 
00029 *  01/30/87 JLA  CHANGES FOR SINGLE TABULAR SUPPORT               GA1KPGM 
00030 *  (D0120)       EXECUTED FROM TRANSACTION GTM1:                  GA1KPGM 
00031 *                1. PF1/PF13 - CONSTRUCT COMMAREA AS              GA1KPGM 
00032 *                   IF GC4A HAD CALLED, XCTL TO ADD               GA1KPGM 
00033 *                   SCREEN PROGRAM.                               GA1KPGM 
00034 *                2. PF3/PF15 - CONSTRUCT COMMAREA AS              GA1KPGM 
00035 *                   IF GC4A HAD CALLED, XCTL TO                   GA1KPGM 
00036 *                   GTM1PGM.                                      GA1KPGM 
00037 *                                                                *GA1KPGM 
00038 *   8/17/87 FRY  CAPTURE OPERATOR-ID WHEN A 'C3', 'C5', OR 'G3'  *GA1KPGM 
00039 *   (D116)       RECORD IS UPDATED.                              *GA1KPGM 
00040 *                                                                *GA1KPGM 
00041 *  11161  11/17/90  PFH   CHANGED  PROGRAM TO BRING IN COPYBOOK  *GA1KPGM 
00042 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY    *GA1KPGM 
00043 *                         ROUTINES.                              *GA1KPGM 
00044 *                                                                *GA1KPGM 
00045 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD      *GA1KPGM 
00046 *                           FROM ONE POSITION TO TWO POSITIONS.  *GA1KPGM 
00047 *                                                                *GA1KPGM 
00048 *D12009 09/30/91  GDM   CONVERT TO COBOL II                      *GA1KPGM 
00049 *                                                                *GA1KPGM 
00050 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GA1KPGM 
00051 *                                                                *GA1KPGM 
00052 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO*GA1KPGM 
00053 *                             7 BYTES. CHANGE # OF OCCURS TO 396 *GA1KPGM 
00054 *                             ON #ADOP TABULAR.                  *GA1KPGM 
      *                                                                *        
      * ICD-10   07/06/11     BA   EXPAND MAP-SELECT FIELD FROM 6 TO 7.*        
00055 ******************************************************************GA1KPGM 
00056 ******************************************************************GA1KPGM 
00057 *   GA1KPGM     ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM  GA1KPGM 
00058 *                              DENTAL OUTPATIENT         GA1K     GA1KPGM 
00059 *                                                                 GA1KPGM 
00060 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1KPGM 
00061 *   CURRENTLY ON THE ALL LEVEL TABULAR RECORD.                    GA1KPGM 
00062 *                                                                 GA1KPGM 
00063 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1KPGM 
00064 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN DECIDE IF   GA1KPGM 
00065 *   ANY ENTRIES WILL BE DELETED.  THE SCREEN ENTRY WILL BE        GA1KPGM 
00066 *   VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD WILL BE   GA1KPGM 
00067 *   MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK INTO THE    GA1KPGM 
00068 *   RECORD BEFORE UPDATING THE RECORD.                            GA1KPGM 
00069 *                                                                 GA1KPGM 
00070 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID:#ADOP)  GA1KPGM 
00071 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1KPGM 
00072 *   XCTL TO TRANS GA2K OR PROGRAM GA2KPGM.  THIS PROGRAM WILL     GA1KPGM 
00073 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1KPGM 
00074 *   TABLE.                                                        GA1KPGM 
00075 *                                                                 GA1KPGM 
00076 *   FUNC CODE: GA1K                                               GA1KPGM 
00077 *   MAPSET:    GA1KSETC  <<<< REDEFINED BY USER DEFINED MAP >>>>  GA1KPGM 
00078 *   FILES:     GCPSWORK                                           GA1KPGM 
00079 *                                                                 GA1KPGM 
00080 *   PF7/PF19  PAGE BACKWARD.                                      GA1KPGM 
00081 *   PF8/PF20  PAGE FORWARD.                                       GA1KPGM 
00082 *   PF10/PF22 PAGE TO BOTTOM.                                     GA1KPGM 
00083 *   PF11/PF23 PAGE TO TOP.                                        GA1KPGM 
00084 *                                                                 GA1KPGM 
00085 *                                                                 GA1KPGM 
00086 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00087 *                                                                 GA1KPGM 
00088 *    TAILORING INSTRUCTIONS:                                      GA1KPGM 
00089 *                                                                 GA1KPGM 
00090 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1KPGM 
00091 *                                                                 GA1KPGM 
00092 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1K/     GA1KPGM 
00093 *              SCREEN PAGE NUMBER                 /009I/001K/     GA1KPGM 
00094 *              ADD PROGRAM FUNCTION CODE          /GA9I/GA2K/     GA1KPGM 
00095 *              BENEFIT PROVISION TABULAR ID       /#PPF/#ADOP/    GA1KPGM 
00096 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GAH/       GA1KPGM 
00097 *                                                                 GA1KPGM 
00098 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1KPGM 
00099 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1KPGM 
00100 *     OCCURANCES.                                                 GA1KPGM 
00101 *                                                                 GA1KPGM 
00102 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1KPGM 
00103 *     THAT MUST BE CHANGED.                                       GA1KPGM 
00104 *                                                                 GA1KPGM 
00105 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00106      EJECT                                                        GA1KPGM 
00107  ENVIRONMENT DIVISION.                                            GA1KPGM 
00108      EJECT                                                        GA1KPGM 
00109  DATA DIVISION.                                                   GA1KPGM 
00110  WORKING-STORAGE SECTION.                                         GA1KPGM 
00111  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1KPGM 
00112      '***GA1KPGM WS BEGINS***'.                                   GA1KPGM 
00113  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1KPGM 
00114                                                                   GA1KPGM 
00115  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1KPGM 
00116                                                                   GA1KPGM 
00117  01  WS-MISC.                                                     GA1KPGM 
00118      05  WS-SELECT-FROM          PIC S9(4) COMP VALUE +0.         GA1KPGM 
00119      05  WS-SELECT-TO            PIC S9(4) COMP VALUE +0.         GA1KPGM 
00120      05  WS-SELECT-OF            PIC S9(4) COMP VALUE +0.         GA1KPGM 
00121      05  WS-SELECT-FROM-MASK     PIC ZZ9.                         GA1KPGM 
00122      05  WS-SELECT-TO-MASK       PIC ZZ9.                         GA1KPGM 
00123      05  WS-SELECT-OF-MASK       PIC ZZ9.                         GA1KPGM 
00124      05  WS-GAH-INDEX            PIC S9(4) COMP VALUE +0.         GA1KPGM 
00125                                                                   GA1KPGM 
00126                                                                   GA1KPGM 
00127 ** MAP COBOL SCREEN DSECTS **                                     GA1KPGM 
00128  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1KPGM 
00129      '***  I/O MAPAREA ***'.                                      GA1KPGM 
00130  COPY GA1KSETC.                                                   GA1KPGM 
00131 /*****************************************************************GA1KPGM 
00132 ******************************************************************GA1KPGM 
00133 ******************************************************************GA1KPGM 
00134 **                                                              **GA1KPGM 
00135 **    THIS IS A USER DEFINED LOGICAL MAP.  ANY CHANGES TO       **GA1KPGM 
00136 **     MAPSET GA1KSETC AFFECTING IT\
00137 **     FOR HERE.                                                **GA1KPGM 
00138 **                                                              **GA1KPGM 
00139 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA1KPGM 
00140 **                                                              **GA1KPGM 
00141 **                                            JLA 8/27/86       **GA1KPGM 
00142 **                                                              **GA1KPGM 
00143 ******************************************************************GA1KPGM 
00144 ******************************************************************GA1KPGM 
00145 ******************************************************************GA1KPGM 
00146                                                                   GA1KPGM 
00147  01  MAP-USER-DEFINED     REDEFINES   GA1KI01I.                   GA1KPGM 
00148                                                                   GA1KPGM 
00149      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA1KPGM 
00150                                                                   GA1KPGM 
00151      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA1KPGM 
00152      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA1KPGM 
00153      05  MAP-FUNCTION-CODE                PIC X(04).              GA1KPGM 
00154                                                                   GA1KPGM 
00155      05  MAP-TITLE-LINE-LEN               PIC S9(4) COMP SYNC.    GA1KPGM 
00156      05  MAP-TITLE-LINE-ATTR              PIC X.                  GA1KPGM 
00157      05  MAP-TITLE-LINE                   PIC X(40).              GA1KPGM 
00158                                                                   GA1KPGM 
00159      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA1KPGM 
00160      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA1KPGM 
00161      05  MAP-SCREEN-ID                    PIC X(06).              GA1KPGM 
00162                                                                   GA1KPGM 
00163      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA1KPGM 
00164      05  MAP-ID-LINE-ATTR                 PIC X.                  GA1KPGM 
00165      05  MAP-ID-LINE                      PIC X(79).              GA1KPGM 
00166      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA1KPGM 
00167          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA1KPGM 
00168          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA1KPGM 
00169          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA1KPGM 
00170          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA1KPGM 
00171          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA1KPGM 
00172          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA1KPGM 
00173          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA1KPGM 
00174          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA1KPGM 
00175          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA1KPGM 
00176          10  FILLER                           PIC X(18).          GA1KPGM 
00177      05  CONTRACT-ID-LINE  REDEFINES  MAP-ID-LINE.                GA1KPGM 
00178          10  CONTRACT-ID-HEADING              PIC X(14).          GA1KPGM 
00179          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA1KPGM 
00180          10  CONTRACT-GROUP-NO                PIC X(6).           GA1KPGM 
00181          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA1KPGM 
00182          10  CONTRACT-SECTION-NO              PIC X(4).           GA1KPGM 
00183          10  CONTRACT-LOB-HEADING             PIC X(6).           GA1KPGM 
00184          10  CONTRACT-LOB                     PIC X.              GA1KPGM 
00185          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA1KPGM 
00186          10  CONTRACT-PROV-CTL                PIC XX.             GA1KPGM 
00187          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA1KPGM 
00188          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA1KPGM 
00189          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA1KPGM 
00190          10  CONTRACT-EFF-DATE                PIC X(6).           GA1KPGM 
00191          10  FILLER                           PIC X(09).          GA1KPGM 
00192      05  BENEFIT-PROVISION-ID-LINE  REDEFINES  MAP-ID-LINE.       GA1KPGM 
00193          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA1KPGM 
00194          10  BEN-PROV-GROUP-NO                PIC X(6).           GA1KPGM 
00195          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA1KPGM 
00196          10  BEN-PROV-SECTION-NO              PIC X(4).           GA1KPGM 
00197          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA1KPGM 
00198          10  BEN-PROV-LOB                     PIC X.              GA1KPGM 
00199          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA1KPGM 
00200          10  BEN-PROV-PROV-CTL                PIC XX.             GA1KPGM 
00201          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA1KPGM 
00202          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA1KPGM 
00203          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA1KPGM 
00204          10  BEN-PROV-EFF-DATE                PIC X(6).           GA1KPGM 
00205          10  BEN-PROV-ID-HEADING              PIC X(8).           GA1KPGM 
00206          10  BEN-PROV-ID-NO                   PIC X(6).           GA1KPGM 
00207          10  FILLER                           PIC X(09).          GA1KPGM 
00208                                                                   GA1KPGM 
00209      05  MAP-ALL-LEVEL-TAB-ID-LEN         PIC S9(4) COMP SYNC.    GA1KPGM 
00210      05  MAP-ALL-LEVEL-TAB-ID-ATTR        PIC X.                  GA1KPGM 
00211      05  MAP-ALL-LEVEL-TAB-ID             PIC X(06).              GA1KPGM 
00212                                                                   GA1KPGM 
00213      05  MAP-ALL-LEVEL-TAB-SLOT-LEN       PIC S9(4) COMP SYNC.    GA1KPGM 
00214      05  MAP-ALL-LEVEL-TAB-SLOT-ATTR      PIC X.                  GA1KPGM 
00215      05  MAP-ALL-LEVEL-TAB-SLOT           PIC X(07).              GA1KPGM 
00216                                                                   GA1KPGM 
00217      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA1KPGM 
00218      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA1KPGM 
00219      05  MAP-FROM-MENU-ID                 PIC X(04).              GA1KPGM 
00220                                                                   GA1KPGM 
00221      05  MAP-SELECT-LABEL-LEN             PIC S9(4) COMP SYNC.    GA1KPGM 
00222      05  MAP-SELECT-LABEL-ATTR            PIC X.                  GA1KPGM 
00223      05  MAP-SELECT-LABEL                 PIC X(07).              GA1KPGM 
00224                                                                   GA1KPGM 
00225      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA1KPGM 
00226      05  MAP-SELECT-ATTR                  PIC X.                  GA1KPGM 
00227      05  MAP-SELECT                       PIC X(07).              GA1KPGM 
00228                                                                   GA1KPGM 
00229      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA1KPGM 
00230      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA1KPGM 
00231      05  MAP-SELECT-FROM                  PIC X(03).              GA1KPGM 
00232                                                                   GA1KPGM 
00233      05  MAP-SELECT-TO-LABEL-LEN          PIC S9(4) COMP SYNC.    GA1KPGM 
00234      05  MAP-SELECT-TO-LABEL-ATTR         PIC X.                  GA1KPGM 
00235      05  MAP-SELECT-TO-LABEL              PIC X(02).              GA1KPGM 
00236                                                                   GA1KPGM 
00237      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA1KPGM 
00238      05  MAP-SELECT-TO-ATTR               PIC X.                  GA1KPGM 
00239      05  MAP-SELECT-TO                    PIC X(03).              GA1KPGM 
00240                                                                   GA1KPGM 
00241      05  MAP-SELECT-OF-LABEL-LEN          PIC S9(4) COMP SYNC.    GA1KPGM 
00242      05  MAP-SELECT-OF-LABEL-ATTR         PIC X.                  GA1KPGM 
00243      05  MAP-SELECT-OF-LABEL              PIC X(02).              GA1KPGM 
00244                                                                   GA1KPGM 
00245      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA1KPGM 
00246      05  MAP-SELECT-OF-ATTR               PIC X.                  GA1KPGM 
00247      05  MAP-SELECT-OF                    PIC X(03).              GA1KPGM 
00248                                                                   GA1KPGM 
00249      05  MAP-SELECT-DISPLAY-LABEL-LEN     PIC S9(4) COMP SYNC.    GA1KPGM 
00250      05  MAP-SELECT-DISPLAY-LABEL-ATTR    PIC X.                  GA1KPGM 
00251      05  MAP-SELECT-DISPLAY-LABEL         PIC X(24).              GA1KPGM 
00252                                                                   GA1KPGM 
00253                                                                   GA1KPGM 
00254      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA1KPGM 
00255          INDEXED BY MAP-IDX1.                                     GA1KPGM 
00256        10  MAP-PROCEDURE-ARGUMENT-COL  OCCURS 2 TIMES             GA1KPGM 
00257            INDEXED BY MAP-IDX2.                                   GA1KPGM 
00258          15  MAP-ACTION-CODE-LEN          PIC S9(4) COMP SYNC.    GA1KPGM 
00259          15  MAP-ACTION-CODE-ATTR         PIC X.                  GA1KPGM 
00260          15  MAP-ACTION-CODE              PIC X.                  GA1KPGM 
00261          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA1KPGM 
00262          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA1KPGM 
00263          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA1KPGM 
00264          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA1KPGM 
00265          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA1KPGM 
00266          15  MAP-CODE-FUNCTION            PIC X(3).               GA1KPGM 
00267                                                                   GA1KPGM 
00268      05  MAP-PAGING-LABEL-LEN             PIC S9(4) COMP SYNC.    GA1KPGM 
00269      05  MAP-PAGING-LABEL-ATTR            PIC X.                  GA1KPGM 
00270      05  MAP-PAGING-LABEL                 PIC X(79).              GA1KPGM 
00271                                                                   GA1KPGM 
00272      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA1KPGM 
00273      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA1KPGM 
00274      05  MAP-ERROR-MESSAGE                PIC X(79).              GA1KPGM 
00275                                                                   GA1KPGM 
00276 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00277  01  WS-MAP-OCCURS-COUNTERS.                                      GA1KPGM 
00278 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00279 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1KPGM 
00280 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00281      05  WS-MAP-ROW              PIC S9(3)  COMP-3  VALUE +15.    GA1KPGM 
00282      05  WS-MAP-COL              PIC S9(3)  COMP-3  VALUE +2.     GA1KPGM 
00283 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00284      EJECT                                                        GA1KPGM 
00285 ** ALTERNATIVE WORKFILE KEYS **                                   GA1KPGM 
00286  01  FILLER                      PIC X(32)  VALUE                 GA1KPGM 
00287      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1KPGM 
00288  01  WS-ALT-WORKFILE-KEYS.                                        GA1KPGM 
00289  COPY GCWRKKEY.                                                   GA1KPGM 
00290      EJECT                                                        GA1KPGM 
00291 ** HARDCOPY WORK AREA **                                          GA1KPGM 
00292 *01  WS-HARDCOPY-COMMAREA.                                        GA1KPGM 
00293 *COPY PRNCOBOL.                                                   GA1KPGM 
00294                                                                   GA1KPGM 
00295 ** DATE FORMATTING AREA **                                        GA1KPGM 
00296  01  WS-DATE-AREA.                                                GA1KPGM 
00297      05  WS-MDY.                                                  GA1KPGM 
00298        10  WS-M                  PIC 99.                          GA1KPGM 
00299        10  WS-D                  PIC 99.                          GA1KPGM 
00300        10  WS-Y                  PIC 99.                          GA1KPGM 
00301      05  WS-YYDDD                PIC 9(5).                        GA1KPGM 
00302      05  FILLER          REDEFINES   WS-YYDDD.                    GA1KPGM 
00303        10  WS-YY                 PIC 99.                          GA1KPGM 
00304        10  WS-DDD                PIC 999.                         GA1KPGM 
00305                                                                   GA1KPGM 
00306 ******************************************************            GA1KPGM 
00307 **    MONTH TABLE FOR DATE CONVERSION                             GA1KPGM 
00308 **    WILL BE GENERATED ONLY ONCE                                 GA1KPGM 
00309 ******************************************************            GA1KPGM 
00310  01   WS-JUL-GREG-DATE-CONV-TAB.                                  GA1KPGM 
00311      05  WS-MONTH-TABLE   OCCURS 13  INDEXED BY  WS-M-IDX         GA1KPGM 
00312          PIC 999 COMP-3.                                          GA1KPGM 
00313      EJECT                                                        GA1KPGM 
00314 ** WORKFIELDS, AND SWITCHES **                                    GA1KPGM 
00315  01  WS-WORK-FIELDS.                                              GA1KPGM 
00316      05  WS-HEX-00                     PIC X.                     GA1KPGM 
00317      05  WS-QUOTIENT                   PIC 999  COMP-3.           GA1KPGM 
00318      05  WS-REMAINDER                  PIC 999  COMP-3.           GA1KPGM 
00319      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1KPGM 
00320 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00321 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00322 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00323      05  WS-SAVED-FIELDS.                                         GA1KPGM 
00324        10  WS-SAVED-PROCED-ARGUMENT    PIC X(7).                  GA1KPGM 
00325        10  WS-SAVED-CODE-FUNCTION      PIC X(3).                  GA1KPGM 
00326 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00327  01  WS-SWITCHES.                                                 GA1KPGM 
00328      05  WS-ERROR-SW                   PIC X.                     GA1KPGM 
00329                                                                   GA1KPGM 
00330 ** TITLE LINES **                                                 GA1KPGM 
00331  01  WS-TITLE-LINES.                                              GA1KPGM 
00332      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(40)  VALUE    GA1KPGM 
00333          ' GROUP SPECIFIC CONDITIONAL PROCEDURES  '.              GA1KPGM 
00334      05  CONTRACT-TITLE-LINE                  PIC X(40)  VALUE    GA1KPGM 
00335          '    CONTRACT CONDITIONAL PROCEDURES     '.              GA1KPGM 
00336      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(40)  VALUE    GA1KPGM 
00337          'BENEFIT PROVISION CONDITIONAL PROCEDURES'.              GA1KPGM 
00338                                                                   GA1KPGM 
00339      EJECT                                                        GA1KPGM 
00340 ** ATTRIBUTES **                                                  GA1KPGM 
00341  COPY DFHBMSCA.                                                   GA1KPGM 
00342      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1KPGM 
00343      EJECT                                                        GA1KPGM 
00344 ** ATTENTION IDENTIFIERS **                                       GA1KPGM 
00345  COPY DFHAID.                                                     GA1KPGM 
00346      EJECT                                                        GA1KPGM 
00347 ** RECORD LENGTHS **                                              GA1KPGM 
00348  01  WS-RECORD-LENGTHS.                                           GA1KPGM 
00349     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA1KPGM 
00350     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA1KPGM 
00351     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1KPGM 
00352     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1KPGM 
00353      COPY GCCDRLEN.                                               GA1KPGM 
00354 *   05 WS-GCIO-PARM-LENGTH            PIC S9(5) COMP-3 VALUE +228.GA1KPGM 
00355 *   05 WS-WORK-RECORD-KEY-LENGTH      PIC S9(5) COMP-3 VALUE +64. GA1KPGM 
00356 *   05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA1KPGM 
00357 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1KPGM 
00358 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA1KPGM 
00359 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA1KPGM 
00360 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1KPGM 
00361 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA1KPGM 
00362 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA1KPGM 
00363 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1KPGM 
00364 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA1KPGM 
00365 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00366 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00367 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00368 *   05 WS-TABULAR-FIXED-PORTION       PIC S9(5) COMP-3 VALUE +40. GA1KPGM 
00369 *   05 WS-TABULAR-VARIABLE-PORTION    PIC S9(5) COMP-3 VALUE +9.  GA1KPGM 
00370 *   05 WS-TABULAR-MAX-OCCURS          PIC S9(5) COMP-3 VALUE +440.GA1KPGM 
00371 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00372 /                                                                 GA1KPGM 
00373  01  COMMAREA-POINTER-AREA.                                       GA1KPGM 
00374      05  COMMAREA-PNTR-COMP  PIC S9(8)  COMP.                     GA1KPGM 
00375      05  COMMAREA-PNTR       REDEFINES                            GA1KPGM 
00376          COMMAREA-PNTR-COMP  USAGE IS POINTER.                    GA1KPGM 
00377 /                                                                 GA1KPGM 
00378                                                                   GA1KPGM 
00379  01  WS-END                      PIC X(16)  VALUE                 GA1KPGM 
00380      '*** W/S ENDS ***'.                                          GA1KPGM 
00381      EJECT                                                        GA1KPGM 
00382  LINKAGE SECTION.                                                 GA1KPGM 
00383  01  DFHCOMMAREA.                                                 GA1KPGM 
00384      COPY G2ALCKEC.                                               GA1KPGM 
00385                                                                   GA1KPGM 
00386 *    05  INCOMING-COMMAREA-PNTR-COMP  PIC S9(8)  COMP.            GA1KPGM 
00387 *    05  INCOMING-COMMAREA-PNTR       REDEFINES                   GA1KPGM 
00388 *        INCOMING-COMMAREA-PNTR-COMP  USAGE IS POINTER.           GA1KPGM 
00389 *                                                                 GA1KPGM 
00390 *01  BLL-CELLS.                                                   GA1KPGM 
00391 *    02  FILLER                  PIC S9(8)  COMP.                 GA1KPGM 
00392 *    02  COMMAREA-PNTR           PIC S9(8)  COMP.                 GA1KPGM 
00393 *    02  ALL-LEVEL-TAB-PNTR      PIC S9(8)  COMP.                 GA1KPGM 
00394 *    02  ALL-LEVEL-TAB-PNTR2     PIC S9(8)  COMP.                 GA1KPGM 
00395 *    02  COPY-AREA-PNTR          PIC S9(8)  COMP.                 GA1KPGM 
00396 *    02  GRP-SPEC-PNTR           PIC S9(8)  COMP.                 GA1KPGM 
00397 *    02  CONTRACT-PNTR           PIC S9(8)  COMP.                 GA1KPGM 
00398 *    02  CONTRACT-PNTR2          PIC S9(8)  COMP.                 GA1KPGM 
00399 *    02  BEN-PROV-PNTR           PIC S9(8)  COMP.                 GA1KPGM 
00400 *                                                                 GA1KPGM 
00401 *01  GCA-COMMAREA.                                                GA1KPGM 
00402 *COPY G2ALCKEC.                                                   GA1KPGM 
00403      EJECT                                                        GA1KPGM 
00404  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA1KPGM 
00405  COPY GCIOPRM1.                                                   GA1KPGM 
00406      EJECT                                                        GA1KPGM 
00407  COPY GCWRKDCC.                                                   GA1KPGM 
00408      SKIP3                                                        GA1KPGM 
00409      SKIP3                                                        GA1KPGM 
00410      SKIP3                                                        GA1KPGM 
00411  COPY GCTADOPC.                                                   GA1KPGM 
00412      EJECT                                                        GA1KPGM 
00413 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00414 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00415 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00416  01  COPY-OF-TABLE-AREA.                                          GA1KPGM 
00417      05  COPY-OF-TABLE   OCCURS 396 TIMES   INDEXED BY  COPY-IDX. GA1KPGM 
00418        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA1KPGM 
00419        10  COPY-CODE-FUNCTION          PIC X(3).                  GA1KPGM 
00420 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00421      EJECT                                                        GA1KPGM 
00422  01  IO-PARM-GRP-SPEC-RECORD.                                     GA1KPGM 
00423  COPY GCIOPRM2.                                                   GA1KPGM 
00424      EJECT                                                        GA1KPGM 
00425  COPY GCWRKDC2.                                                   GA1KPGM 
00426      EJECT                                                        GA1KPGM 
00427  COPY GCGROUPC.                                                   GA1KPGM 
00428      EJECT                                                        GA1KPGM 
00429                                                                   GA1KPGM 
00430  01  IO-PARM-CONTRACT-RECORD.                                     GA1KPGM 
00431  COPY GCIOPRM3.                                                   GA1KPGM 
00432      EJECT                                                        GA1KPGM 
00433  COPY GCWRKDC3.                                                   GA1KPGM 
00434      EJECT                                                        GA1KPGM 
00435  COPY GCCONTRC.                                                   GA1KPGM 
00436      EJECT                                                        GA1KPGM 
00437                                                                   GA1KPGM 
00438  01  IO-PARM-BEN-PROV-RECORD.                                     GA1KPGM 
00439  COPY GCIOPRM4.                                                   GA1KPGM 
00440      EJECT                                                        GA1KPGM 
00441  COPY GCWRKDC4.                                                   GA1KPGM 
00442      EJECT                                                        GA1KPGM 
00443  COPY GCBENPVC.                                                   GA1KPGM 
00444      EJECT                                                        GA1KPGM 
00445  PROCEDURE DIVISION.                                              GA1KPGM 
00446                                                                   GA1KPGM 
00447 ******************************************************************GA1KPGM 
00448 **                H O U S E K E E P I N G                         GA1KPGM 
00449 **                                                                GA1KPGM 
00450 ** DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM. GA1KPGM 
00451 **                                                                GA1KPGM 
00452 ******************************************************************GA1KPGM 
00453  0000-HOUSEKEEPING SECTION.                                       GA1KPGM 
00454                                                                   GA1KPGM 
00455      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1KPGM 
00456      MOVE  ZERO  TO  WS-MONTH-TABLE (1),  WS-YYDDD.               GA1KPGM 
00457      MOVE  31  TO  WS-MONTH-TABLE (2).                            GA1KPGM 
00458      MOVE  59  TO  WS-MONTH-TABLE (3).                            GA1KPGM 
00459      MOVE  90  TO  WS-MONTH-TABLE (4).                            GA1KPGM 
00460      MOVE  120  TO  WS-MONTH-TABLE (5).                           GA1KPGM 
00461      MOVE  151  TO  WS-MONTH-TABLE (6).                           GA1KPGM 
00462      MOVE  181  TO  WS-MONTH-TABLE (7).                           GA1KPGM 
00463      MOVE  212  TO  WS-MONTH-TABLE (8).                           GA1KPGM 
00464      MOVE  243  TO  WS-MONTH-TABLE (9).                           GA1KPGM 
00465      MOVE  273  TO  WS-MONTH-TABLE (10).                          GA1KPGM 
00466      MOVE  304  TO  WS-MONTH-TABLE (11).                          GA1KPGM 
00467      MOVE  334  TO  WS-MONTH-TABLE (12).                          GA1KPGM 
00468      MOVE  400  TO  WS-MONTH-TABLE (13).                          GA1KPGM 
00469                                                                   GA1KPGM 
00470      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1KPGM 
00471         NOSTG(9010-NO-STORAGE)                                    GA1KPGM 
00472         PGMIDERR(9020-PGM-ID-ERROR)   END-EXEC.                   GA1KPGM 
00473      EJECT                                                        GA1KPGM 
00474 ******************************************************************GA1KPGM 
00475 **                     M A I N L I N E                            GA1KPGM 
00476 **                                                                GA1KPGM 
00477 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1KPGM 
00478 **  TAKEN BY THE OPERATOR.                                        GA1KPGM 
00479 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1KPGM 
00480 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO DETERMINE  GA1KPGM 
00481 **     WHICH ENTRIES, IF ANY, THEY MIGHT WANT TO DELETE.          GA1KPGM 
00482 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1KPGM 
00483 **     KEY PF12 OR PF24.                                          GA1KPGM 
00484 **  3. RECEIVE THE SCREEN.                                        GA1KPGM 
00485 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1KPGM 
00486 **     MENU.                                                      GA1KPGM 
00487 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL DELETE      GA1KPGM 
00488 **     LOGIC.                                                     GA1KPGM 
00489 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1KPGM 
00490 **     (RETURN) TO THE ADD PROGRAM (GA2KPGM).                     GA1KPGM 
00491 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1KPGM 
00492 **     (RETURN) TO THE PREVIOUS MENU.                             GA1KPGM 
00493 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1KPGM 
00494 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1KPGM 
00495 **                                                                GA1KPGM 
00496 ******************************************************************GA1KPGM 
00497  1000-MAIN-LINE SECTION.                                          GA1KPGM 
00498                                                                   GA1KPGM 
00499      MOVE '1000'  TO  WS-PARA-ID.                                 GA1KPGM 
00500      IF EIBTRNID  NOT =  'GA1K'                                   GA1KPGM 
00501         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1KPGM 
00502         GO TO 1099-RETURN.                                        GA1KPGM 
00503                                                                   GA1KPGM 
00504 ******************************************************************GA1KPGM 
00505 ***  IF EIBAID  =  DFHPF12 OR  =  DFHPF24                      ***GA1KPGM 
00506 ***     PERFORM 7000-PRINT-HARDCOPY                            ***GA1KPGM 
00507 ***     GO TO 1099-RETURN.                                     ***GA1KPGM 
00508 ******************************************************************GA1KPGM 
00509                                                                   GA1KPGM 
00510      EXEC CICS RECEIVE   MAP('GA1KI01') MAPSET('GA1KSET')         GA1KPGM 
00511         INTO(GA1KI01I) END-EXEC.                                  GA1KPGM 
00512                                                                   GA1KPGM 
00513      IF MAP-SCREEN-ID  NOT = '001K00'                             GA1KPGM 
00514         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1KPGM 
00515                                                                   GA1KPGM 
00516      IF EIBAID   =  DFHENTER OR                                   GA1KPGM 
00517                     DFHPF7   OR DFHPF19 OR                        GA1KPGM 
00518                     DFHPF8   OR DFHPF20 OR                        GA1KPGM 
00519                     DFHPF10  OR DFHPF22 OR                        GA1KPGM 
00520                     DFHPF11  OR DFHPF23                           GA1KPGM 
00521         PERFORM 2000-DELETE-PROCESSING                            GA1KPGM 
00522         GO TO 1099-RETURN.                                        GA1KPGM 
00523                                                                   GA1KPGM 
00524      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1KPGM 
00525         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1KPGM 
00526                                                                   GA1KPGM 
00527      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1KPGM 
00528         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1KPGM 
00529                                                                   GA1KPGM 
00530      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1KPGM 
00531      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1KPGM 
00532      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1KPGM 
00533 -    'THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                   GA1KPGM 
00534      EXEC CICS SEND   MAP('GA1KI01') MAPSET('GA1KSET') DATAONLY   GA1KPGM 
00535         FROM(GA1KI01O) CURSOR END-EXEC.                           GA1KPGM 
00536                                                                   GA1KPGM 
00537  1099-RETURN.                                                     GA1KPGM 
00538 *    EXEC CICS RETURN   END-EXEC.                                 GA1KPGM 
00539      EXEC CICS RETURN TRANSID ('GA1K')                            GA1KPGM 
00540                COMMAREA (DFHCOMMAREA)                             GA1KPGM 
00541                END-EXEC.                                          GA1KPGM 
00542                                                                   GA1KPGM 
00543      GOBACK.                                                      GA1KPGM 
00544      EJECT                                                        GA1KPGM 
00545 ******************************************************************GA1KPGM 
00546 **              D E L E T E   P R O C E S S I N G                 GA1KPGM 
00547 **                                                                GA1KPGM 
00548 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1KPGM 
00549 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1KPGM 
00550 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1KPGM 
00551 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1KPGM 
00552 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1KPGM 
00553 **    BACK INTO THE RECORD THAT WE READ.)                         GA1KPGM 
00554 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1KPGM 
00555 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1KPGM 
00556 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1KPGM 
00557 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1KPGM 
00558 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1KPGM 
00559 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1KPGM 
00560 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1KPGM 
00561 **    ENTRY.                                                      GA1KPGM 
00562 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1KPGM 
00563 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1KPGM 
00564 **    RECORD.                                                     GA1KPGM 
00565 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1KPGM 
00566 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1KPGM 
00567 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1KPGM 
00568 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1KPGM 
00569 **    ARE BYPASSED; WE READ THE ALL LEVEL TABULAR RECORD, SAVE THEGA1KPGM 
00570 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1KPGM 
00571 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1KPGM 
00572 **                                                                GA1KPGM 
00573 ******************************************************************GA1KPGM 
00574  2000-DELETE-PROCESSING SECTION.                                  GA1KPGM 
00575                                                                   GA1KPGM 
00576      MOVE '2000'  TO  WS-PARA-ID.                                 GA1KPGM 
00577      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1KPGM 
00578      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1KPGM 
00579      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1KPGM 
00580                                                                   GA1KPGM 
00581      MOVE '2010'  TO  WS-PARA-ID.                                 GA1KPGM 
00582  2010-VALIDATE-ACT-CODE.                                          GA1KPGM 
00583      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D'               GA1KPGM 
00584         ADD 1  TO  WS-DELETE-COUNT.                               GA1KPGM 
00585      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D' OR            GA1KPGM 
00586         =  SPACE OR  =  LOW-VALUES                                GA1KPGM 
00587         MOVE DFHBMUNF  TO                                         GA1KPGM 
00588            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1KPGM 
00589         MOVE DFHBMASF  TO                                         GA1KPGM 
00590 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00591 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00592 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00593            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1KPGM 
00594            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1KPGM 
00595      ELSE                                                         GA1KPGM 
00596         MOVE DFHBMUBF  TO                                         GA1KPGM 
00597            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1KPGM 
00598         MOVE DFHBMABF  TO                                         GA1KPGM 
00599            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1KPGM 
00600            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1KPGM 
00601 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00602         IF WS-ERROR-SW  NOT =  'Y'                                GA1KPGM 
00603            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1KPGM 
00604            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1KPGM 
00605                                                                   GA1KPGM 
00606      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1KPGM 
00607         SET MAP-IDX1  UP BY  1                                    GA1KPGM 
00608      ELSE                                                         GA1KPGM 
00609         IF MAP-IDX2  <  WS-MAP-COL                                GA1KPGM 
00610            SET MAP-IDX1  TO  1                                    GA1KPGM 
00611            SET MAP-IDX2  UP BY  1                                 GA1KPGM 
00612         ELSE                                                      GA1KPGM 
00613            GO TO 2020-DONE-VALIDATE-A-C.                          GA1KPGM 
00614 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00615 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00616 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00617      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)               GA1KPGM 
00618             NOT =  LOW-VALUES  AND                                GA1KPGM 
00619         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2)                    GA1KPGM 
00620             NOT =  LOW-VALUES                                     GA1KPGM 
00621 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00622         GO TO 2010-VALIDATE-ACT-CODE.                             GA1KPGM 
00623                                                                   GA1KPGM 
00624  2020-DONE-VALIDATE-A-C.                                          GA1KPGM 
00625      MOVE '2020'  TO  WS-PARA-ID.                                 GA1KPGM 
00626      SET MAP-IDX1  TO  1.                                         GA1KPGM 
00627      IF WS-ERROR-SW  =  'Y'                                       GA1KPGM 
00628         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1KPGM 
00629            MAP-ERROR-MESSAGE                                      GA1KPGM 
00630         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA1KPGM 
00631                              MAP-TITLE-LINE                       GA1KPGM 
00632                              MAP-SCREEN-ID                        GA1KPGM 
00633                              MAP-ALL-LEVEL-TAB-ID                 GA1KPGM 
00634                              MAP-ALL-LEVEL-TAB-SLOT               GA1KPGM 
00635                              MAP-ID-LINE                          GA1KPGM 
00636                              MAP-FROM-MENU-ID                     GA1KPGM 
00637 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00638 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1KPGM 
00639 **  ADD ITS MAP FIELD NAME HERE.                                  GA1KPGM 
00640 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00641         MOVE '2100'  TO  WS-PARA-ID                               GA1KPGM 
00642         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1KPGM 
00643            VARYING MAP-IDX2 FROM  1  BY  1                        GA1KPGM 
00644                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1KPGM 
00645              AFTER MAP-IDX1 FROM  1  BY  1                        GA1KPGM 
00646                             UNTIL MAP-IDX1  > WS-MAP-ROW          GA1KPGM 
00647         EXEC CICS SEND   MAP('GA1KI01') MAPSET('GA1KSET') DATAONLYGA1KPGM 
00648            FROM(GA1KI01O) CURSOR END-EXEC                         GA1KPGM 
00649         GO TO 2099-EXIT.                                          GA1KPGM 
00650                                                                   GA1KPGM 
00651      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1KPGM 
00652              GC-GCIOPARM-LEN +                                    GA1KPGM 
00653              GC-WORKFILE-KEY-LEN +                                GA1KPGM 
00654              GC-GCTABULR-ADOP-FIXED-LEN +                         GA1KPGM 
00655           (GC-GCTABULR-ADOP-VARY-MAX-OCUR                         GA1KPGM 
00656               * GC-GCTABULR-ADOP-VARY-LEN)                        GA1KPGM 
00657                                                                   GA1KPGM 
00658 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA1KPGM 
00659      EXEC CICS GETMAIN                                            GA1KPGM 
00660         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA1KPGM 
00661         INITIMG(WS-HEX-00)                                        GA1KPGM 
00662         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1KPGM 
00663 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1KPGM 
00664 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1KPGM 
00665                                                                   GA1KPGM 
00666      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1KPGM 
00667         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1KPGM 
00668         MOVE 'G'   TO  GCIO-WRK-STATUS-CODE                       GA1KPGM 
00669         MOVE 'G3'  TO  GCIO-WRK-RECORD-TYPE                       GA1KPGM 
00670         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1KPGM 
00671         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1KPGM 
00672         MOVE GRP-SPEC-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA1KPGM 
00673         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1KPGM 
00674         MOVE GRP-SPEC-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1KPGM 
00675         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1KPGM 
00676         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA1KPGM 
00677                          GCIO-WRK-PROVIDER-CONTROL                GA1KPGM 
00678         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1KPGM 
00679         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1KPGM 
00680         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-PROVISION-ID       GA1KPGM 
00681         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-PROVISION-SLOT-NOGA1KPGM 
00682         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA1KPGM 
00683         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1KPGM 
00684                                                                   GA1KPGM 
00685      IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA1KPGM 
00686         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1KPGM 
00687         MOVE 'C'   TO  GCIO-WRK-STATUS-CODE                       GA1KPGM 
00688         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE                       GA1KPGM 
00689         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1KPGM 
00690         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1KPGM 
00691         MOVE CONTRACT-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA1KPGM 
00692         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1KPGM 
00693         MOVE CONTRACT-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1KPGM 
00694         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1KPGM 
00695         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1KPGM 
00696         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1KPGM 
00697         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1KPGM 
00698         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1KPGM 
00699         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-PROVISION-ID       GA1KPGM 
00700         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-PROVISION-SLOT-NOGA1KPGM 
00701         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA1KPGM 
00702         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA1KPGM 
00703                                                                   GA1KPGM 
00704      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1KPGM 
00705         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA1KPGM 
00706         MOVE 'C'   TO  GCIO-WRK-STATUS-CODE                       GA1KPGM 
00707         MOVE 'C5'  TO  GCIO-WRK-RECORD-TYPE                       GA1KPGM 
00708         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA1KPGM 
00709         MOVE GCA-GROUP-NO-1-3     TO  GCIO-WRK-GROUP-NO-1-3       GA1KPGM 
00710         MOVE BEN-PROV-GROUP-NO  TO  GCIO-WRK-GROUP-NO             GA1KPGM 
00711         MOVE GCA-SEC-NO-1         TO  GCIO-WRK-SEC-NO-1           GA1KPGM 
00712         MOVE BEN-PROV-SECTION-NO  TO  GCIO-WRK-SECTION-NO         GA1KPGM 
00713         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA1KPGM 
00714         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA1KPGM 
00715         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA1KPGM 
00716         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1KPGM 
00717         MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN          GA1KPGM 
00718         MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID            GA1KPGM 
00719         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA1KPGM 
00720         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-TAB-PROVISION-ID   GA1KPGM 
00721         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-TAB-PROV-SLOT-NO.GA1KPGM 
00722                                                                   GA1KPGM 
00723      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA1KPGM 
00724      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1KPGM 
00725      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1KPGM 
00726                                                                   GA1KPGM 
00727      IF WS-DELETE-COUNT  =  ZERO                                  GA1KPGM 
00728         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1KPGM 
00729                                                                   GA1KPGM 
00730                                                                   GA1KPGM 
00731      IF  EIBAID  =  DFHPF7   OR DFHPF19 OR                        GA1KPGM 
00732                     DFHPF8   OR DFHPF20 OR                        GA1KPGM 
00733                     DFHPF10  OR DFHPF22 OR                        GA1KPGM 
00734                     DFHPF11  OR DFHPF23                           GA1KPGM 
00735      THEN                                                         GA1KPGM 
00736          MOVE '*** ACTION CODE ENTRY INVALID WHEN PAGING ***'     GA1KPGM 
00737                           TO MAP-ERROR-MESSAGE                    GA1KPGM 
00738          MOVE -1          TO  MAP-SELECT-LEN                      GA1KPGM 
00739          MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                   GA1KPGM 
00740                               MAP-SCREEN-ID                       GA1KPGM 
00741 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00742 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1KPGM 
00743 **  ADD ITS MAP FIELD NAME HERE.                                  GA1KPGM 
00744 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00745          MOVE '2100'  TO  WS-PARA-ID                              GA1KPGM 
00746          PERFORM 2100-DONT-RETRANSMIT-FIELDS                      GA1KPGM 
00747             VARYING MAP-IDX2 FROM  1  BY  1                       GA1KPGM 
00748                              UNTIL MAP-IDX2  >  WS-MAP-COL        GA1KPGM 
00749               AFTER MAP-IDX1 FROM  1  BY  1                       GA1KPGM 
00750                              UNTIL MAP-IDX1  >  WS-MAP-ROW        GA1KPGM 
00751          EXEC CICS SEND   MAP('GA1KI01')                          GA1KPGM 
00752                           MAPSET('GA1KSET')                       GA1KPGM 
00753                           DATAONLY                                GA1KPGM 
00754                           FROM(GA1KI01O)                          GA1KPGM 
00755                           CURSOR                                  GA1KPGM 
00756                           END-EXEC                                GA1KPGM 
00757          GO TO 2099-EXIT                                          GA1KPGM 
00758      ELSE                                                         GA1KPGM 
00759          NEXT SENTENCE.                                           GA1KPGM 
00760                                                                   GA1KPGM 
00761 ******************************************************************GA1KPGM 
00762 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1KPGM 
00763 *                                                                 GA1KPGM 
00764 ******************************************************************GA1KPGM 
00765                                                                   GA1KPGM 
00766      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1KPGM 
00767      TO   GAH-ENTRY-COUNT.                                        GA1KPGM 
00768                                                                   GA1KPGM 
00769      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1KPGM 
00770                                                                   GA1KPGM 
00771      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1KPGM 
00772         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1KPGM 
00773         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1KPGM 
00774                                                                   GA1KPGM 
00775      IF  NOT GCIO-GOOD-RETURN                                     GA1KPGM 
00776         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1KPGM 
00777 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1KPGM 
00778         MOVE '1KF1'  TO  WS-ABEND-CODE                            GA1KPGM 
00779         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1KPGM 
00780                                                                   GA1KPGM 
00781      COMPUTE  WS-COPY-LENGTH  =                                   GA1KPGM 
00782            GAH-ENTRY-COUNT  *  GC-GCTABULR-ADOP-VARY-LEN.         GA1KPGM 
00783                                                                   GA1KPGM 
00784 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA1KPGM 
00785      EXEC CICS GETMAIN                                            GA1KPGM 
00786         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA1KPGM 
00787         LENGTH(WS-COPY-LENGTH)                                    GA1KPGM 
00788         INITIMG(WS-HEX-00) END-EXEC.                              GA1KPGM 
00789 ***  SERVICE RELOAD COPY-OF-TABLE-AREA.                           GA1KPGM 
00790                                                                   GA1KPGM 
00791      MOVE GAH-ENTRY-COUNT  TO  GAH-ENTRY-COUNT.                   GA1KPGM 
00792      SET COPY-IDX, GAH-INDEX  TO  1.                              GA1KPGM 
00793                                                                   GA1KPGM 
00794      MOVE '2030'  TO  WS-PARA-ID.                                 GA1KPGM 
00795  2030-MAKE-A-COPY-OF-RECORD.                                      GA1KPGM 
00796      IF GAH-INDEX  NOT >  GAH-ENTRY-COUNT                         GA1KPGM 
00797         MOVE GAH-ENTRY (GAH-INDEX)  TO  COPY-OF-TABLE (COPY-IDX)  GA1KPGM 
00798         SET COPY-IDX, GAH-INDEX  UP BY 1                          GA1KPGM 
00799         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1KPGM 
00800                                                                   GA1KPGM 
00801      SET MAP-IDX1, MAP-IDX2, COPY-IDX, GAH-INDEX  TO  1.          GA1KPGM 
00802      MOVE '2040'  TO  WS-PARA-ID.                                 GA1KPGM 
00803  2040-DELETE-MARKED-ENTRIES.                                      GA1KPGM 
00804 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00805 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00806 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00807      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1KPGM 
00808             AND                                                   GA1KPGM 
00809         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1KPGM 
00810         GO TO 2060-SAVE-REST-OF-COPY.                             GA1KPGM 
00811                                                                   GA1KPGM 
00812      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)  >            GA1KPGM 
00813                            COPY-PROCEDURE-ARGUMENT (COPY-IDX) OR  GA1KPGM 
00814         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2)  >                 GA1KPGM 
00815                                     COPY-CODE-FUNCTION(COPY-IDX)  GA1KPGM 
00816         GO TO 2050-SAVE-COPIED-ENTRY                              GA1KPGM 
00817      ELSE                                                         GA1KPGM 
00818         IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) <          GA1KPGM 
00819            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA1KPGM 
00820            MOVE '1KL1'  TO  WS-ABEND-CODE                         GA1KPGM 
00821            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1KPGM 
00822 -    'RM SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                 GA1KPGM 
00823            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1KPGM 
00824                                                                   GA1KPGM 
00825 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00826                                                                   GA1KPGM 
00827      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1KPGM 
00828         IF MAP-IDX1  <  WS-MAP-ROW                                GA1KPGM 
00829            SET MAP-IDX1  UP BY  1                                 GA1KPGM 
00830            GO TO 2050-SAVE-COPIED-ENTRY                           GA1KPGM 
00831         ELSE                                                      GA1KPGM 
00832            IF MAP-IDX2  <  WS-MAP-COL                             GA1KPGM 
00833               SET MAP-IDX1  TO  1                                 GA1KPGM 
00834               SET MAP-IDX2  UP BY  1                              GA1KPGM 
00835               GO TO 2050-SAVE-COPIED-ENTRY                        GA1KPGM 
00836            ELSE                                                   GA1KPGM 
00837               GO TO 2060-SAVE-REST-OF-COPY.                       GA1KPGM 
00838                                                                   GA1KPGM 
00839      SET COPY-IDX  UP BY  1.                                      GA1KPGM 
00840      IF COPY-IDX  NOT <  GAH-ENTRY-COUNT                          GA1KPGM 
00841         MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAH-ENTRY (GAH-INDEX)  GA1KPGM 
00842         SET  GAH-ENTRY-COUNT  TO  GAH-INDEX                       GA1KPGM 
00843         MOVE GAH-ENTRY-COUNT  TO  GAH-ENTRY-COUNT                 GA1KPGM 
00844         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1KPGM 
00845                                                                   GA1KPGM 
00846      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1KPGM 
00847         SET MAP-IDX1  UP BY  1                                    GA1KPGM 
00848         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1KPGM 
00849                                                                   GA1KPGM 
00850      IF MAP-IDX2  <  WS-MAP-COL                                   GA1KPGM 
00851         SET MAP-IDX1  TO  1                                       GA1KPGM 
00852         SET MAP-IDX2  UP BY  1                                    GA1KPGM 
00853         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1KPGM 
00854      ELSE                                                         GA1KPGM 
00855         GO TO 2060-SAVE-REST-OF-COPY.                             GA1KPGM 
00856                                                                   GA1KPGM 
00857  2050-SAVE-COPIED-ENTRY.                                          GA1KPGM 
00858      MOVE '2050'  TO  WS-PARA-ID.                                 GA1KPGM 
00859      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAH-ENTRY (GAH-INDEX).    GA1KPGM 
00860                                                                   GA1KPGM 
00861      SET GAH-INDEX  UP BY  1.                                     GA1KPGM 
00862      IF COPY-IDX  <  GAH-ENTRY-COUNT                              GA1KPGM 
00863         SET COPY-IDX  UP BY  1                                    GA1KPGM 
00864         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1KPGM 
00865      ELSE                                                         GA1KPGM 
00866 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1KPGM 
00867 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1KPGM 
00868 ***      THE TABLE OF ENTRIES.                                    GA1KPGM 
00869         MOVE '1KL2'  TO  WS-ABEND-CODE                            GA1KPGM 
00870         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1KPGM 
00871 -    'SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                    GA1KPGM 
00872         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1KPGM 
00873                                                                   GA1KPGM 
00874  2060-SAVE-REST-OF-COPY.                                          GA1KPGM 
00875      MOVE '2060'  TO  WS-PARA-ID.                                 GA1KPGM 
00876      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAH-ENTRY (GAH-INDEX).    GA1KPGM 
00877                                                                   GA1KPGM 
00878      SET GAH-INDEX  UP BY  1.                                     GA1KPGM 
00879      IF COPY-IDX  <  GAH-ENTRY-COUNT                              GA1KPGM 
00880         SET COPY-IDX  UP BY  1                                    GA1KPGM 
00881         GO TO 2060-SAVE-REST-OF-COPY.                             GA1KPGM 
00882                                                                   GA1KPGM 
00883      SET GAH-INDEX  DOWN BY  1.                                   GA1KPGM 
00884      SET GAH-ENTRY-COUNT  TO  GAH-INDEX.                          GA1KPGM 
00885      MOVE GAH-ENTRY-COUNT  TO  GAH-ENTRY-COUNT.                   GA1KPGM 
00886                                                                   GA1KPGM 
00887  2070-UPDATE-MODIFIED-REC.                                        GA1KPGM 
00888      MOVE '2070'  TO  WS-PARA-ID.                                 GA1KPGM 
00889                                                                   GA1KPGM 
00890 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1KPGM 
00891                                                                   GA1KPGM 
00892      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1KPGM 
00893      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1KPGM 
00894                                                                   GA1KPGM 
00895      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1KPGM 
00896              GC-WORKFILE-KEY-LEN        +                         GA1KPGM 
00897              GC-GCTABULR-ADOP-FIXED-LEN +                         GA1KPGM 
00898             (GAH-ENTRY-COUNT  *  GC-GCTABULR-ADOP-VARY-LEN).      GA1KPGM 
00899                                                                   GA1KPGM 
00900      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1KPGM 
00901              GC-GCIOPARM-LEN      +                               GA1KPGM 
00902              GCIO-RECORD-LENGTH.                                  GA1KPGM 
00903                                                                   GA1KPGM 
00904      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1KPGM 
00905         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1KPGM 
00906         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1KPGM 
00907                                                                   GA1KPGM 
00908      IF GCIO-GOOD-RETURN                                          GA1KPGM 
00909         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1KPGM 
00910      MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASE CGA1KPGM 
00911 -    'ONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.            GA1KPGM 
00912      MOVE '1KF2'  TO  WS-ABEND-CODE.                              GA1KPGM 
00913      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1KPGM 
00914                                                                   GA1KPGM 
00915  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1KPGM 
00916      MOVE  '2080'  TO  WS-PARA-ID.                                GA1KPGM 
00917      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1KPGM 
00918      TO   GAH-ENTRY-COUNT.                                        GA1KPGM 
00919      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1KPGM 
00920      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1KPGM 
00921         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1KPGM 
00922         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1KPGM 
00923                                                                   GA1KPGM 
00924      IF GCIO-GOOD-RETURN                                          GA1KPGM 
00925         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1KPGM 
00926      MOVE '1KF3'  TO  WS-ABEND-CODE.                              GA1KPGM 
00927      MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE CONGA1KPGM 
00928 -    'TACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.              GA1KPGM 
00929      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1KPGM 
00930                                                                   GA1KPGM 
00931  2090-BUILD-NEXT-DISPLAY.                                         GA1KPGM 
00932      MOVE  '2090'  TO  WS-PARA-ID.                                GA1KPGM 
00933      SET MAP-IDX1  TO  WS-MAP-ROW.                                GA1KPGM 
00934      SET MAP-IDX2  TO  WS-MAP-COL.                                GA1KPGM 
00935      SET GAH-INDEX  TO  1.                                        GA1KPGM 
00936 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00937 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
00938 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00939      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1KPGM 
00940             AND                                                   GA1KPGM 
00941         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1KPGM 
00942         MOVE GAH-ENTRY (GAH-INDEX)  TO  WS-SAVED-FIELDS           GA1KPGM 
00943      ELSE                                                         GA1KPGM 
00944         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) TO       GA1KPGM 
00945            WS-SAVED-PROCED-ARGUMENT                               GA1KPGM 
00946         MOVE MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) TO            GA1KPGM 
00947            WS-SAVED-CODE-FUNCTION.                                GA1KPGM 
00948 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
00949                                                                   GA1KPGM 
00950                                                                   GA1KPGM 
00951      IF  MAP-SELECT-LEN > 0        AND                            GA1KPGM 
00952          MAP-SELECT     > SPACES                                  GA1KPGM 
00953          MOVE MAP-SELECT TO WS-SAVED-PROCED-ARGUMENT.             GA1KPGM 
00954                                                                   GA1KPGM 
00955      IF  EIBAID  =  DFHPF7   OR DFHPF19 OR                        GA1KPGM 
00956                     DFHPF8   OR DFHPF20 OR                        GA1KPGM 
00957                     DFHPF10  OR DFHPF22 OR                        GA1KPGM 
00958                     DFHPF11  OR DFHPF23                           GA1KPGM 
00959      THEN                                                         GA1KPGM 
00960          MOVE SPACES TO MAP-SELECT                                GA1KPGM 
00961      ELSE                                                         GA1KPGM 
00962          GO TO 2090-FILL-THE-SCREEN.                              GA1KPGM 
00963                                                                   GA1KPGM 
00964      IF  EIBAID  =  DFHPF7   OR DFHPF19                           GA1KPGM 
00965          GO TO 2090-PAGE-BACKWARD.                                GA1KPGM 
00966      IF  EIBAID  =  DFHPF8   OR DFHPF20                           GA1KPGM 
00967          GO TO 2090-PAGE-FORWARD.                                 GA1KPGM 
00968      IF  EIBAID  =  DFHPF10  OR DFHPF22                           GA1KPGM 
00969          GO TO 2090-PAGE-TO-BOTTOM.                               GA1KPGM 
00970      IF  EIBAID  =  DFHPF11  OR DFHPF23                           GA1KPGM 
00971          GO TO 2090-PAGE-TO-TOP.                                  GA1KPGM 
00972                                                                   GA1KPGM 
00973  2090-PAGE-BACKWARD.                                              GA1KPGM 
00974                                                                   GA1KPGM 
00975      MOVE MAP-PROCEDURE-ARGUMENT(1 1) TO WS-SAVED-PROCED-ARGUMENT.GA1KPGM 
00976      MOVE MAP-CODE-FUNCTION(1 1)  TO  WS-SAVED-CODE-FUNCTION.     GA1KPGM 
00977                                                                   GA1KPGM 
00978      SEARCH GAH-ENTRY                                             GA1KPGM 
00979          AT END                                                   GA1KPGM 
00980                MOVE GAH-ENTRY(1)  TO  WS-SAVED-FIELDS             GA1KPGM 
00981                GO TO 2090-FILL-THE-SCREEN                         GA1KPGM 
00982          WHEN                                                     GA1KPGM 
00983                WS-SAVED-FIELDS  =  GAH-ENTRY(GAH-INDEX)           GA1KPGM 
00984                SET WS-GAH-INDEX  TO  GAH-INDEX.                   GA1KPGM 
00985                                                                   GA1KPGM 
00986      COMPUTE WS-GAH-INDEX = WS-GAH-INDEX                          GA1KPGM 
00987                           - (WS-MAP-ROW * WS-MAP-COL)             GA1KPGM 
00988                           + 1.                                    GA1KPGM 
00989                                                                   GA1KPGM 
00990      IF WS-GAH-INDEX  <  +0                                       GA1KPGM 
00991      THEN                                                         GA1KPGM 
00992          MOVE GAH-ENTRY(1)  TO  WS-SAVED-FIELDS                   GA1KPGM 
00993      ELSE                                                         GA1KPGM 
00994          SET GAH-INDEX  TO  WS-GAH-INDEX                          GA1KPGM 
00995          MOVE GAH-ENTRY(GAH-INDEX)  TO  WS-SAVED-FIELDS.          GA1KPGM 
00996                                                                   GA1KPGM 
00997      GO TO 2090-FILL-THE-SCREEN.                                  GA1KPGM 
00998                                                                   GA1KPGM 
00999  2090-PAGE-FORWARD.                                               GA1KPGM 
01000                                                                   GA1KPGM 
01001      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)  =  LOW-VALUESGA1KPGM 
01002      THEN                                                         GA1KPGM 
01003          MOVE GAH-ENTRY(GAH-INDEX)  TO  WS-SAVED-FIELDS           GA1KPGM 
01004      ELSE                                                         GA1KPGM 
01005          MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)  TO     GA1KPGM 
01006                                           WS-SAVED-PROCED-ARGUMENTGA1KPGM 
01007          MOVE MAP-CODE-FUNCTION(MAP-IDX1, MAP-IDX2)  TO           GA1KPGM 
01008                                            WS-SAVED-CODE-FUNCTION.GA1KPGM 
01009                                                                   GA1KPGM 
01010      GO TO 2090-FILL-THE-SCREEN.                                  GA1KPGM 
01011                                                                   GA1KPGM 
01012  2090-PAGE-TO-BOTTOM.                                             GA1KPGM 
01013                                                                   GA1KPGM 
01014      COMPUTE WS-GAH-INDEX = GAH-ENTRY-COUNT                       GA1KPGM 
01015                           - (WS-MAP-ROW * WS-MAP-COL).            GA1KPGM 
01016                                                                   GA1KPGM 
01017      IF WS-GAH-INDEX  <  +0                                       GA1KPGM 
01018      THEN                                                         GA1KPGM 
01019          MOVE GAH-ENTRY(1)  TO  WS-SAVED-FIELDS                   GA1KPGM 
01020      ELSE                                                         GA1KPGM 
01021          SET GAH-INDEX  TO  WS-GAH-INDEX                          GA1KPGM 
01022          MOVE GAH-ENTRY(GAH-INDEX)  TO  WS-SAVED-FIELDS.          GA1KPGM 
01023                                                                   GA1KPGM 
01024      GO TO 2090-FILL-THE-SCREEN.                                  GA1KPGM 
01025                                                                   GA1KPGM 
01026  2090-PAGE-TO-TOP.                                                GA1KPGM 
01027                                                                   GA1KPGM 
01028      SET GAH-INDEX  TO  1.                                        GA1KPGM 
01029      MOVE GAH-ENTRY(GAH-INDEX)  TO  WS-SAVED-FIELDS.              GA1KPGM 
01030                                                                   GA1KPGM 
01031      GO TO 2090-FILL-THE-SCREEN.                                  GA1KPGM 
01032                                                                   GA1KPGM 
01033  2090-FILL-THE-SCREEN.                                            GA1KPGM 
01034                                                                   GA1KPGM 
01035      PERFORM 4500-FILL-THE-SCREEN.                                GA1KPGM 
01036      EXEC CICS SEND   MAP   ('GA1KI01')                           GA1KPGM 
01037                       MAPSET('GA1KSET')                           GA1KPGM 
01038                       ERASE                                       GA1KPGM 
01039                       FROM  (GA1KI01O)                            GA1KPGM 
01040                       END-EXEC.                                   GA1KPGM 
01041                                                                   GA1KPGM 
01042                                                                   GA1KPGM 
01043  2099-EXIT.   EXIT.                                               GA1KPGM 
01044      EJECT                                                        GA1KPGM 
01045  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1KPGM 
01046 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01047 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
01048 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01049      MOVE LOW-VALUES  TO                                          GA1KPGM 
01050         MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),                     GA1KPGM 
01051         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1KPGM 
01052         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1KPGM 
01053 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01054                                                                   GA1KPGM 
01055  2199-EXIT.   EXIT.                                               GA1KPGM 
01056      EJECT                                                        GA1KPGM 
01057 ******************************************************************GA1KPGM 
01058 **          X C T L   T O   A D D   S C R E E N                   GA1KPGM 
01059 **                                                                GA1KPGM 
01060 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1KPGM 
01061 ** ADDING ENTRIES.  WE READ THE ALL LEVEL TABULAR & PASS THE      GA1KPGM 
01062 ** ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL TABULAR  GA1KPGM 
01063 ** RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE PROGRAM GA1KPGM 
01064 ** ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE            GA1KPGM 
01065 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1KPGM 
01066 ******************************************************************GA1KPGM 
01067  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1KPGM 
01068      MOVE '3000'  TO  WS-PARA-ID.                                 GA1KPGM 
01069                                                                   GA1KPGM 
01070 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA1KPGM 
01071 *    EXEC CICS GETMAIN                                            GA1KPGM 
01072 *       SET(ADDRESS OF GCA-COMMAREA)                              GA1KPGM 
01073 *       INITIMG(WS-HEX-00)                                        GA1KPGM 
01074 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA1KPGM 
01075 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1KPGM 
01076                                                                   GA1KPGM 
01077 *    IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1KPGM 
01078 *       MOVE MAP-ID-LINE  TO GROUP-SPECIFIC-ID-LINE               GA1KPGM 
01079 *       MOVE GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                    GA1KPGM 
01080 *       MOVE GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO                GA1KPGM 
01081 *       MOVE GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1KPGM 
01082 *       MOVE GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1KPGM 
01083 *       MOVE SPACES  TO  GCA-L-O-B,                               GA1KPGM 
01084 *                        GCA-PROV-CTL,                            GA1KPGM 
01085 *                        GCA-BEN-PROV-ID.                         GA1KPGM 
01086                                                                   GA1KPGM 
01087 *    IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA1KPGM 
01088 *       MOVE MAP-ID-LINE  TO CONTRACT-ID-LINE                     GA1KPGM 
01089 *       MOVE CONTRACT-GROUP-NO  TO  GCA-GRP-NO                    GA1KPGM 
01090 *       MOVE CONTRACT-SECTION-NO  TO  GCA-SECTN-NO                GA1KPGM 
01091 *       MOVE CONTRACT-LOB  TO  GCA-L-O-B                          GA1KPGM 
01092 *       MOVE CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                  GA1KPGM 
01093 *       MOVE CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1KPGM 
01094 *       MOVE CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1KPGM 
01095 *       MOVE SPACES  TO  GCA-BEN-PROV-ID.                         GA1KPGM 
01096                                                                   GA1KPGM 
01097 *    IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1KPGM 
01098 *       MOVE MAP-ID-LINE  TO BENEFIT-PROVISION-ID-LINE            GA1KPGM 
01099 *       MOVE BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                    GA1KPGM 
01100 *       MOVE BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO                GA1KPGM 
01101 *       MOVE BEN-PROV-LOB  TO  GCA-L-O-B                          GA1KPGM 
01102 *       MOVE BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                  GA1KPGM 
01103 *       MOVE BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1KPGM 
01104 *       MOVE BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1KPGM 
01105 *       MOVE BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                 GA1KPGM 
01106                                                                   GA1KPGM 
01107 *    MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID.          GA1KPGM 
01108 *    MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT.      GA1KPGM 
01109 *    MOVE SPACES  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE,                GA1KPGM 
01110 *                     GCA-INTERNAL-TAB-ID,                        GA1KPGM 
01111 *                     GCA-INTERNAL-TAB-SLOT,                      GA1KPGM 
01112 *                     GCA-OCCURS-ENTRY-COUNTER,                   GA1KPGM 
01113 *                     GCA-ADD-DEL-IND.                            GA1KPGM 
01114 *    MOVE MAP-FROM-MENU-ID  TO GCA-FROM-MENU-ID.                  GA1KPGM 
01115 *    MOVE ZEROES  TO  GCA-EFF-DT.                                 GA1KPGM 
01116                                                                   GA1KPGM 
01117 *    SET COMMAREA-PNTR TO ADDRESS                                 GA1KPGM 
01118 *    OF GCA-COMMAREA.                                             GA1KPGM 
01119                                                                   GA1KPGM 
01120 *    EXEC CICS XCTL  PROGRAM('GA2KPGM') COMMAREA(COMMAREA-PNTR)   GA1KPGM 
01121 *       LENGTH(4) END-EXEC.                                       GA1KPGM 
01122      EXEC CICS XCTL  PROGRAM('GA2KPGM')                           GA1KPGM 
01123                      COMMAREA(DFHCOMMAREA)                        GA1KPGM 
01124                      LENGTH (LENGTH OF DFHCOMMAREA)               GA1KPGM 
01125      END-EXEC.                                                    GA1KPGM 
01126                                                                   GA1KPGM 
01127  3099-EXIT.   EXIT.                                               GA1KPGM 
01128      EJECT                                                        GA1KPGM 
01129 ***************************************************************** GA1KPGM 
01130 **          D I S P L A Y   F I R S T   S C R E E N               GA1KPGM 
01131 **                                                                GA1KPGM 
01132 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1KPGM 
01133 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1KPGM 
01134 ** ALL LEVEL TABULAR RECORD & PASS US THE RECORD (PRECEEDED BY I/OGA1KPGM 
01135 ** PARMS AND WORKFILE KEY).  WE WILL THEN USE THAT RECORD TO BUILDGA1KPGM 
01136 ** THE SCREEN IMAGE.                                              GA1KPGM 
01137 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1KPGM 
01138 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1KPGM 
01139 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1KPGM 
01140 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1KPGM 
01141 ** DISPLAYED, THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,  GA1KPGM 
01142 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1KPGM 
01143 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1KPGM 
01144 ******************************************************************GA1KPGM 
01145  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1KPGM 
01146      MOVE '4000'  TO  WS-PARA-ID.                                 GA1KPGM 
01147                                                                   GA1KPGM 
01148 ***  MOVE LOW-VALUES TO SCREEN                                    GA1KPGM 
01149      MOVE LOW-VALUES TO GA1KI01I.                                 GA1KPGM 
01150                                                                   GA1KPGM 
01151      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1KPGM 
01152         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1KPGM 
01153            TO MAP-ERROR-MESSAGE                                   GA1KPGM 
01154         MOVE '1KC1'  TO  WS-ABEND-CODE                            GA1KPGM 
01155         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1KPGM 
01156                                                                   GA1KPGM 
01157 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA1KPGM 
01158 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1KPGM 
01159 ***  MOVE GCA-RECORD-POINTER  TO  ALL-LEVEL-TAB-PNTR.             GA1KPGM 
01160 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1KPGM 
01161 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1KPGM 
01162                                                                   GA1KPGM 
01163 *    SET ADDRESS OF GCA-COMMAREA                                  GA1KPGM 
01164 *    TO  INCOMING-COMMAREA-PNTR.                                  GA1KPGM 
01165                                                                   GA1KPGM 
01166      SET ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD                    GA1KPGM 
01167      TO  GCA-RECORD-POINTER.                                      GA1KPGM 
01168                                                                   GA1KPGM 
01169      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-ALL-LEVEL-TAB-ID.         GA1KPGM 
01170      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-ALL-LEVEL-TAB-SLOT.     GA1KPGM 
01171      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA1KPGM 
01172 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01173 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1KPGM 
01174 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1KPGM 
01175 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01176                                                                   GA1KPGM 
01177 *    MOVE WRK-EFF-DATE  TO  WS-YYDDD.                             GA1KPGM 
01178                                                                   GA1KPGM 
01179 *    SET WS-M-IDX  TO  1.                                         GA1KPGM 
01180 *    MOVE WS-YY  TO  WS-Y.                                        GA1KPGM 
01181 *    DIVIDE  WS-YY  BY  4  GIVING  WS-QUOTIENT                    GA1KPGM 
01182 *       REMAINDER  WS-REMAINDER.                                  GA1KPGM 
01183 *                                                                 GA1KPGM 
01184 *    MOVE '4010'  TO  WS-PARA-ID.                                 GA1KPGM 
01185 *4010-DETERMINE-DATE.                                             GA1KPGM 
01186 *    IF  WS-REMAINDER  =  ZERO  AND  WS-M-IDX  >  2               GA1KPGM 
01187 *       COMPUTE  WS-MONTH-TABLE (WS-M-IDX)  =                     GA1KPGM 
01188 *          WS-MONTH-TABLE (WS-M-IDX)  +  1.                       GA1KPGM 
01189 *    IF  WS-MONTH-TABLE (WS-M-IDX)  =  WS-DDD  OR  >  WS-DDD      GA1KPGM 
01190 *       SET WS-M-IDX  DOWN BY  1                                  GA1KPGM 
01191 *       SET WS-M  TO  WS-M-IDX                                    GA1KPGM 
01192 *       COMPUTE  WS-D  =  WS-DDD  -  WS-MONTH-TABLE (WS-M-IDX)    GA1KPGM 
01193 *    ELSE                                                         GA1KPGM 
01194 *       IF  WS-M-IDX  <  13                                       GA1KPGM 
01195 *          SET WS-M-IDX  UP BY  1                                 GA1KPGM 
01196 *          GO TO  4010-DETERMINE-DATE                             GA1KPGM 
01197 *       ELSE                                                      GA1KPGM 
01198 *          MOVE '*** INVALID EFFECTIVE DATE DISCOVERED, WE CAN NOTGA1KPGM 
01199 *    ' PROCESS THIS REQUEST ***'  TO  MAP-ERROR-MESSAGE           GA1KPGM 
01200 *          MOVE '1KC2'  TO  WS-ABEND-CODE                         GA1KPGM 
01201 *          PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1KPGM 
01202                                                                   GA1KPGM 
01203 *    MOVE WS-MDY  TO  GCA-EFFECTIVE-DATE.                         GA1KPGM 
01204                                                                   GA1KPGM 
01205      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1KPGM 
01206         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-TITLE-LINE        GA1KPGM 
01207         MOVE 'GROUP SPECIFIC ID = '  TO GRP-SPEC-ID-HEADING       GA1KPGM 
01208         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA1KPGM 
01209         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA1KPGM 
01210         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1KPGM 
01211         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA1KPGM 
01212         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1KPGM 
01213         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1KPGM 
01214         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1KPGM 
01215         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1KPGM 
01216                                                                   GA1KPGM 
01217      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1KPGM 
01218         MOVE CONTRACT-TITLE-LINE  TO  MAP-TITLE-LINE              GA1KPGM 
01219         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1KPGM 
01220         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA1KPGM 
01221         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA1KPGM 
01222         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1KPGM 
01223         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA1KPGM 
01224         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1KPGM 
01225         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1KPGM 
01226         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1KPGM 
01227         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1KPGM 
01228         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1KPGM 
01229         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1KPGM 
01230         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1KPGM 
01231         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1KPGM 
01232                                                                   GA1KPGM 
01233      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1KPGM 
01234         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-TITLE-LINE     GA1KPGM 
01235         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA1KPGM 
01236         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA1KPGM 
01237         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA1KPGM 
01238         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA1KPGM 
01239         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA1KPGM 
01240         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1KPGM 
01241         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA1KPGM 
01242         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1KPGM 
01243         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA1KPGM 
01244         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1KPGM 
01245         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA1KPGM 
01246         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1KPGM 
01247         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA1KPGM 
01248         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1KPGM 
01249                                                                   GA1KPGM 
01250      SET GAH-INDEX  TO  1.                                        GA1KPGM 
01251      MOVE GAH-ENTRY (GAH-INDEX)  TO  WS-SAVED-FIELDS.             GA1KPGM 
01252                                                                   GA1KPGM 
01253      PERFORM 4500-FILL-THE-SCREEN.                                GA1KPGM 
01254      EXEC CICS SEND   MAP('GA1KI01') MAPSET('GA1KSET') ERASE      GA1KPGM 
01255         FROM(GA1KI01O) END-EXEC.                                  GA1KPGM 
01256                                                                   GA1KPGM 
01257  4099-EXIT.   EXIT.                                               GA1KPGM 
01258      EJECT                                                        GA1KPGM 
01259 ***************************************************************** GA1KPGM 
01260 **             F I L L   T H E   S C R E E N                      GA1KPGM 
01261 **                                                                GA1KPGM 
01262 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1KPGM 
01263 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1KPGM 
01264 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1KPGM 
01265 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1KPGM 
01266 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1KPGM 
01267 ******************************************************************GA1KPGM 
01268  4500-FILL-THE-SCREEN SECTION.                                    GA1KPGM 
01269                                                                   GA1KPGM 
01270      MOVE '4500'  TO  WS-PARA-ID.                                 GA1KPGM 
01271      MOVE  GAH-ENTRY-COUNT  TO  GAH-ENTRY-COUNT.                  GA1KPGM 
01272      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1KPGM 
01273                                                                   GA1KPGM 
01274      COMPUTE WS-SELECT-OF      =   GAH-ENTRY-COUNT - 1.           GA1KPGM 
01275      MOVE    WS-SELECT-OF      TO  WS-SELECT-OF-MASK.             GA1KPGM 
01276      MOVE    WS-SELECT-OF-MASK TO  MAP-SELECT-FROM                GA1KPGM 
01277                                    MAP-SELECT-TO                  GA1KPGM 
01278                                    MAP-SELECT-OF.                 GA1KPGM 
01279                                                                   GA1KPGM 
01280      IF GAH-ENTRY-COUNT  NOT >  1                                 GA1KPGM 
01281         MOVE '4530'  TO  WS-PARA-ID                               GA1KPGM 
01282         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1KPGM 
01283                                                                   GA1KPGM 
01284      SET GAH-INDEX  TO  1.                                        GA1KPGM 
01285      MOVE '4510'  TO  WS-PARA-ID.                                 GA1KPGM 
01286  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1KPGM 
01287 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01288 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
01289 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01290      IF GAH-ENTRY(GAH-INDEX)  <  WS-SAVED-FIELDS                  GA1KPGM 
01291 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01292         SET GAH-INDEX  UP BY  1                                   GA1KPGM 
01293         IF  GAH-INDEX  <  GAH-ENTRY-COUNT                         GA1KPGM 
01294            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1KPGM 
01295         ELSE                                                      GA1KPGM 
01296            SET GAH-INDEX  TO  1.                                  GA1KPGM 
01297                                                                   GA1KPGM 
01298      SET  WS-SELECT-FROM       TO GAH-INDEX.                      GA1KPGM 
01299      MOVE WS-SELECT-FROM       TO WS-SELECT-FROM-MASK.            GA1KPGM 
01300      MOVE WS-SELECT-FROM-MASK  TO MAP-SELECT-FROM.                GA1KPGM 
01301                                                                   GA1KPGM 
01302      MOVE '4520'  TO  WS-PARA-ID.                                 GA1KPGM 
01303  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1KPGM 
01304      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1KPGM 
01305      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1KPGM 
01306 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01307 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
01308 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01309      MOVE GAH-PROCEDURE-ARGUMENT (GAH-INDEX) TO                   GA1KPGM 
01310         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2).              GA1KPGM 
01311      MOVE GAH-COMBINATION-CODE-FUNCTION (GAH-INDEX) TO            GA1KPGM 
01312         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1KPGM 
01313 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01314                                                                   GA1KPGM 
01315      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1KPGM 
01316         SET  MAP-IDX1  UP BY  1                                   GA1KPGM 
01317      ELSE                                                         GA1KPGM 
01318         IF MAP-IDX2  <  WS-MAP-COL                                GA1KPGM 
01319            SET  MAP-IDX1  TO  1                                   GA1KPGM 
01320            SET  MAP-IDX2  UP BY  1                                GA1KPGM 
01321         ELSE                                                      GA1KPGM 
01322            SET  WS-SELECT-TO         TO GAH-INDEX                 GA1KPGM 
01323            MOVE WS-SELECT-TO         TO WS-SELECT-TO-MASK         GA1KPGM 
01324            MOVE WS-SELECT-TO-MASK    TO MAP-SELECT-TO             GA1KPGM 
01325            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1KPGM 
01326                                                                   GA1KPGM 
01327      IF GAH-INDEX  <  (GAH-ENTRY-COUNT - 1 )                      GA1KPGM 
01328         SET  GAH-INDEX  UP BY  1                                  GA1KPGM 
01329         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1KPGM 
01330                                                                   GA1KPGM 
01331      SET  WS-SELECT-TO         TO GAH-INDEX.                      GA1KPGM 
01332      MOVE WS-SELECT-TO         TO WS-SELECT-TO-MASK.              GA1KPGM 
01333      MOVE WS-SELECT-TO-MASK    TO MAP-SELECT-TO.                  GA1KPGM 
01334                                                                   GA1KPGM 
01335                                                                   GA1KPGM 
01336      MOVE '4530'  TO  WS-PARA-ID.                                 GA1KPGM 
01337  4530-FILL-REST-WITH-NULLS.                                       GA1KPGM 
01338      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1KPGM 
01339 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01340 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1KPGM 
01341 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01342      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1KPGM 
01343         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1KPGM 
01344         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1KPGM 
01345 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1KPGM 
01346      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1KPGM 
01347         SET  MAP-IDX1  UP BY  1                                   GA1KPGM 
01348         GO TO 4530-FILL-REST-WITH-NULLS                           GA1KPGM 
01349      ELSE                                                         GA1KPGM 
01350         IF MAP-IDX2  <  WS-MAP-COL                                GA1KPGM 
01351            SET  MAP-IDX1  TO  1                                   GA1KPGM 
01352            SET  MAP-IDX2  UP BY  1                                GA1KPGM 
01353            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1KPGM 
01354                                                                   GA1KPGM 
01355      MOVE '4540'  TO  WS-PARA-ID.                                 GA1KPGM 
01356  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1KPGM 
01357      IF GAH-ENTRY-COUNT  =  1                                     GA1KPGM 
01358         MOVE '*** NO ENTRIES TO DELETE ***' TO MAP-ERROR-MESSAGE  GA1KPGM 
01359         GO TO 4599-EXIT.                                          GA1KPGM 
01360                                                                   GA1KPGM 
01361      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1KPGM 
01362         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'                 GA1KPGM 
01363           TO  MAP-ERROR-MESSAGE.                                  GA1KPGM 
01364                                                                   GA1KPGM 
01365  4599-EXIT.     EXIT.                                             GA1KPGM 
01366      EJECT                                                        GA1KPGM 
01367 ***************************************************************** GA1KPGM 
01368 **        X C T L   T O   P R E V I O U S   M E N U               GA1KPGM 
01369 **                                                                GA1KPGM 
01370 **  THE OPERATOR WANTS TO RETURN TO THE MENU THIS PROGRAM         GA1KPGM 
01371 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND     GA1KPGM 
01372 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA1KPGM 
01373 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM MENU  GA1KPGM 
01374 ** ID' FIELD).                                                    GA1KPGM 
01375 ******************************************************************GA1KPGM 
01376  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1KPGM 
01377      MOVE '5000'  TO  WS-PARA-ID.                                 GA1KPGM 
01378                                                                   GA1KPGM 
01379      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1KPGM 
01380         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA1KPGM 
01381                                                                   GA1KPGM 
01382      IF  MAP-FROM-MENU-ID  = 'GC4A'                               GA1KPGM 
01383         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA1KPGM 
01384                                                                   GA1KPGM 
01385      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1KPGM 
01386         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA1KPGM 
01387                                                                   GA1KPGM 
01388      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA1KPGM 
01389         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA1KPGM 
01390                                                                   GA1KPGM 
01391                                                                   GA1KPGM 
01392  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA1KPGM 
01393      MOVE '5010'  TO  WS-PARA-ID.                                 GA1KPGM 
01394                                                                   GA1KPGM 
01395      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1KPGM 
01396          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1KPGM 
01397                 GC-GCGRPSPC-FIXED-LEN      +                      GA1KPGM 
01398         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1KPGM 
01399                                                                   GA1KPGM 
01400 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA1KPGM 
01401      EXEC CICS GETMAIN                                            GA1KPGM 
01402         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA1KPGM 
01403         INITIMG(WS-HEX-00)                                        GA1KPGM 
01404         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01405 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA1KPGM 
01406                                                                   GA1KPGM 
01407      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA1KPGM 
01408                                                                   GA1KPGM 
01409      MOVE 'G'   TO  GCIO-WRK-STATUS-CODE.                         GA1KPGM 
01410      MOVE 'G2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1KPGM 
01411      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1KPGM 
01412      MOVE GCA-GROUP-NUM TO  GCIO-WRK-GROUP-NUM.                   GA1KPGM 
01413      MOVE GCA-SECTION-NUM TO  GCIO-WRK-SECTION-NUM.               GA1KPGM 
01414      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1KPGM 
01415      MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                       GA1KPGM 
01416                       GCIO-WRK-PROVIDER-CONTROL.                  GA1KPGM 
01417      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1KPGM 
01418      MOVE GCA-EFFDT-CEN   TO GCIO-WRK-EFFDT-CEN.                  GA1KPGM 
01419                                                                   GA1KPGM 
01420 *    MOVE  WS-Y  TO  WS-YY.                                       GA1KPGM 
01421 *    IF WS-M  >  2                                                GA1KPGM 
01422 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1KPGM 
01423 *          REMAINDER  WS-REMAINDER                                GA1KPGM 
01424 *    ELSE                                                         GA1KPGM 
01425 *       MOVE 1  TO  WS-REMAINDER.                                 GA1KPGM 
01426 *    SET WS-M-IDX  TO  WS-M.                                      GA1KPGM 
01427 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1KPGM 
01428 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1KPGM 
01429 *    IF WS-REMAINDER  =  ZERO                                     GA1KPGM 
01430 *       ADD 1  TO  WS-DDD.                                        GA1KPGM 
01431                                                                   GA1KPGM 
01432 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1KPGM 
01433      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA1KPGM 
01434      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1KPGM 
01435                       GCIO-WRK-TAB-PROVISION-ID.                  GA1KPGM 
01436      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1KPGM 
01437                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1KPGM 
01438      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1KPGM 
01439                                                                   GA1KPGM 
01440      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA1KPGM 
01441      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA1KPGM 
01442                                                                   GA1KPGM 
01443      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1KPGM 
01444                                                                   GA1KPGM 
01445      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1KPGM 
01446         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA1KPGM 
01447         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01448                                                                   GA1KPGM 
01449      IF  NOT GCIO2-GOOD-RETURN                                    GA1KPGM 
01450         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1KPGM 
01451 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1KPGM 
01452         MOVE '1KF4'  TO  WS-ABEND-CODE                            GA1KPGM 
01453         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1KPGM 
01454                                                                   GA1KPGM 
01455      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1KPGM 
01456          GC-WORKFILE-KEY-LEN        +                             GA1KPGM 
01457                 GC-GCGRPSPC-FIXED-LEN      +                      GA1KPGM 
01458         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1KPGM 
01459                                                                   GA1KPGM 
01460      EXEC CICS XCTL  PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)   GA1KPGM 
01461         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01462                                                                   GA1KPGM 
01463      GO  TO  5099-EXIT.                                           GA1KPGM 
01464                                                                   GA1KPGM 
01465  5020-XCTL-TO-CONTRACT-MENU.                                      GA1KPGM 
01466      MOVE '5020'  TO  WS-PARA-ID.                                 GA1KPGM 
01467                                                                   GA1KPGM 
01468      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1KPGM 
01469          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1KPGM 
01470                 GC-GCCONTR-FIXED-LEN       +                      GA1KPGM 
01471         (GC-GCCONTR-VARY-LEN     *  GC-GCCONTR-VARY-MAX-OCUR).    GA1KPGM 
01472                                                                   GA1KPGM 
01473 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA1KPGM 
01474      EXEC CICS GETMAIN                                            GA1KPGM 
01475         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA1KPGM 
01476         INITIMG(WS-HEX-00)                                        GA1KPGM 
01477         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01478 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA1KPGM 
01479 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA1KPGM 
01480                                                                   GA1KPGM 
01481      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA1KPGM 
01482                                                                   GA1KPGM 
01483      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1KPGM 
01484      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA1KPGM 
01485      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1KPGM 
01486      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1KPGM 
01487      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1KPGM 
01488      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1KPGM 
01489      MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA1KPGM 
01490      MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA1KPGM 
01491      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1KPGM 
01492      MOVE GCA-EFFDT-CEN TO GCIO-WRK-EFFDT-CEN.                    GA1KPGM 
01493                                                                   GA1KPGM 
01494 *    MOVE  WS-Y  TO  WS-YY.                                       GA1KPGM 
01495 *    IF WS-M  >  2                                                GA1KPGM 
01496 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1KPGM 
01497 *          REMAINDER  WS-REMAINDER                                GA1KPGM 
01498 *    ELSE                                                         GA1KPGM 
01499 *       MOVE 1  TO  WS-REMAINDER.                                 GA1KPGM 
01500 *    SET WS-M-IDX  TO  WS-M.                                      GA1KPGM 
01501 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1KPGM 
01502 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1KPGM 
01503 *    IF WS-REMAINDER  =  ZERO                                     GA1KPGM 
01504 *       ADD 1  TO  WS-DDD.                                        GA1KPGM 
01505                                                                   GA1KPGM 
01506 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1KPGM 
01507      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA1KPGM 
01508      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1KPGM 
01509                       GCIO-WRK-TAB-PROVISION-ID.                  GA1KPGM 
01510      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1KPGM 
01511                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1KPGM 
01512      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA1KPGM 
01513                                                                   GA1KPGM 
01514      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA1KPGM 
01515      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA1KPGM 
01516                                                                   GA1KPGM 
01517      MOVE  'RD '  TO  GCIO3-FILE-ACCESS-CODE.                     GA1KPGM 
01518                                                                   GA1KPGM 
01519      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1KPGM 
01520         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA1KPGM 
01521         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01522                                                                   GA1KPGM 
01523      IF  NOT GCIO3-GOOD-RETURN                                    GA1KPGM 
01524         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1KPGM 
01525 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1KPGM 
01526         MOVE '1KF5'  TO  WS-ABEND-CODE                            GA1KPGM 
01527         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1KPGM 
01528                                                                   GA1KPGM 
01529      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1KPGM 
01530          GC-WORKFILE-KEY-LEN        +                             GA1KPGM 
01531                 GC-GCCONTR-FIXED-LEN       +                      GA1KPGM 
01532         (GC-GCCONTR-VARY-LEN     *  GC-GCCONTR-VARY-MAX-OCUR).    GA1KPGM 
01533                                                                   GA1KPGM 
01534      EXEC CICS XCTL  PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)   GA1KPGM 
01535         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01536                                                                   GA1KPGM 
01537      GO  TO  5099-EXIT.                                           GA1KPGM 
01538                                                                   GA1KPGM 
01539  5030-XCTL-TO-BEN-PROV-MENU.                                      GA1KPGM 
01540      MOVE '5030'  TO  WS-PARA-ID.                                 GA1KPGM 
01541                                                                   GA1KPGM 
01542      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1KPGM 
01543          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1KPGM 
01544                 GC-GCBENPRV-FIXED-LEN      +                      GA1KPGM 
01545         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1KPGM 
01546                                                                   GA1KPGM 
01547 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA1KPGM 
01548      EXEC CICS GETMAIN                                            GA1KPGM 
01549         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA1KPGM 
01550         INITIMG(WS-HEX-00)                                        GA1KPGM 
01551         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01552 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA1KPGM 
01553                                                                   GA1KPGM 
01554      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA1KPGM 
01555                                                                   GA1KPGM 
01556      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA1KPGM 
01557      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA1KPGM 
01558      MOVE GCA-PLAN-CODE TO GCIO-WRK-PLAN-CODE.                    GA1KPGM 
01559      MOVE GCA-GROUP-NUM TO GCIO-WRK-GROUP-NUM.                    GA1KPGM 
01560      MOVE GCA-SECTION-NUM TO GCIO-WRK-SECTION-NUM.                GA1KPGM 
01561      MOVE GCA-PKG-CODE TO GCIO-WRK-PKG-CODE.                      GA1KPGM 
01562      MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA1KPGM 
01563      MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA1KPGM 
01564      MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1KPGM 
01565      MOVE GCA-EFFDT-CEN   TO GCIO-WRK-EFFDT-CEN.                  GA1KPGM 
01566 *    MOVE BEN-PROV-EFF-DATE  TO  WS-MDY.                          GA1KPGM 
01567      MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID.              GA1KPGM 
01568                                                                   GA1KPGM 
01569 *    MOVE  WS-Y  TO  WS-YY.                                       GA1KPGM 
01570 *    IF WS-M  >  2                                                GA1KPGM 
01571 *       DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1KPGM 
01572 *          REMAINDER  WS-REMAINDER                                GA1KPGM 
01573 *    ELSE                                                         GA1KPGM 
01574 *       MOVE 1  TO  WS-REMAINDER.                                 GA1KPGM 
01575 *    SET WS-M-IDX  TO  WS-M.                                      GA1KPGM 
01576 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1KPGM 
01577 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1KPGM 
01578 *    IF WS-REMAINDER  =  ZERO                                     GA1KPGM 
01579 *       ADD 1  TO  WS-DDD.                                        GA1KPGM 
01580                                                                   GA1KPGM 
01581 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1KPGM 
01582      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA1KPGM 
01583      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA1KPGM 
01584      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA1KPGM 
01585      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1KPGM 
01586      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA1KPGM 
01587                                                                   GA1KPGM 
01588      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA1KPGM 
01589      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA1KPGM 
01590                                                                   GA1KPGM 
01591      MOVE  'RD '  TO  GCIO4-FILE-ACCESS-CODE.                     GA1KPGM 
01592                                                                   GA1KPGM 
01593      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1KPGM 
01594         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA1KPGM 
01595         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01596                                                                   GA1KPGM 
01597      IF  NOT GCIO4-GOOD-RETURN                                    GA1KPGM 
01598         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1KPGM 
01599 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1KPGM 
01600         MOVE '1KF6'  TO  WS-ABEND-CODE                            GA1KPGM 
01601         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1KPGM 
01602                                                                   GA1KPGM 
01603      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1KPGM 
01604          GC-WORKFILE-KEY-LEN        +                             GA1KPGM 
01605                 GC-GCBENPRV-FIXED-LEN      +                      GA1KPGM 
01606         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1KPGM 
01607                                                                   GA1KPGM 
01608      EXEC CICS XCTL  PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)   GA1KPGM 
01609         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1KPGM 
01610                                                                   GA1KPGM 
01611      GO  TO  5099-EXIT.                                           GA1KPGM 
01612                                                                   GA1KPGM 
01613                                                                   GA1KPGM 
01614  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA1KPGM 
01615      MOVE '5040'  TO  WS-PARA-ID.                                 GA1KPGM 
01616                                                                   GA1KPGM 
01617      EXEC CICS XCTL                                               GA1KPGM 
01618                PROGRAM('GTM1PGM')                                 GA1KPGM 
01619                END-EXEC.                                          GA1KPGM 
01620                                                                   GA1KPGM 
01621      GO  TO  5099-EXIT.                                           GA1KPGM 
01622                                                                   GA1KPGM 
01623  5099-EXIT.                                                       GA1KPGM 
01624      EXIT.                                                        GA1KPGM 
01625      EJECT                                                        GA1KPGM 
01626 ***************************************************************** GA1KPGM 
01627 **           X C T L   T O   M A I N   M E N U                    GA1KPGM 
01628 **                                                                GA1KPGM 
01629 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1KPGM 
01630 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1KPGM 
01631 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1KPGM 
01632 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOME OF AGA1KPGM 
01633 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1KPGM 
01634 ** AND PROGRESS DOWN.                                             GA1KPGM 
01635 ******************************************************************GA1KPGM 
01636  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1KPGM 
01637      MOVE '6000'  TO  WS-PARA-ID.                                 GA1KPGM 
01638      MOVE '1KP1'  TO  WS-ABEND-CODE.                              GA1KPGM 
01639                                                                   GA1KPGM 
01640      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1KPGM 
01641                                                                   GA1KPGM 
01642  6099-EXIT.     EXIT.                                             GA1KPGM 
01643      EJECT                                                        GA1KPGM 
01644 /**************************************************************** GA1KPGM 
01645 **              P R I N T   H A R D C O P Y                       GA1KPGM 
01646 **                                                                GA1KPGM 
01647 **   THE OPERATOR HAS KEYED THE PF12 OF PF24 KEY INDICATING THEY  GA1KPGM 
01648 ** WANT A HARDCOPY IMAGE OF THE CURRENT SCREEN.  WE LINK TO THE   GA1KPGM 
01649 ** 'CSCRTCPY' PROGRAM WHICH WILL PRINT THE SCREEN, AND RETURN US AGA1KPGM 
01650 ** RETURN CODE INDICATING SUCCESS OR THE TYPE OF ERROR.           GA1KPGM 
01651 ******************************************************************GA1KPGM 
01652 *7000-PRINT-HARDCOPY SECTION.                                     GA1KPGM 
01653 **   MOVE '7000'  TO  WS-PARA-ID.                                 GA1KPGM 
01654 **                                                                GA1KPGM 
01655 **   MOVE 'CSCRTCPY'  TO  PRINT-PROGRAM-ID.                       GA1KPGM 
01656 **   MOVE 'PRN'  TO  PRINT-REQUEST-TYPE.                          GA1KPGM 
01657 **   SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1KPGM 
01658 **   MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1KPGM 
01659 **                                                                GA1KPGM 
01660 **   EXEC CICS LINK   PROGRAM('CSCRTCPY')                         GA1KPGM 
01661 **      LENGTH(PRINT-COMMAREA-LEN)                                GA1KPGM 
01662 **      COMMAREA(CSCRTCPY-COMMAREA-DEFINITION) END-EXEC.          GA1KPGM 
01663 **                                                                GA1KPGM 
01664 **   IF PRINT-OK                                                  GA1KPGM 
01665 **      MOVE '    *** HARDCOPY REQUEST COMPLETED ***'             GA1KPGM 
01666 **        TO  MAP-ERROR-MESSAGE                                   GA1KPGM 
01667 **      GO TO 7010-SEND-SCREEN.                                   GA1KPGM 
01668 **                                                                GA1KPGM 
01669 **   IF PRINT-OUT-OF-SERVICE                                      GA1KPGM 
01670 **      MOVE '                *** OUT OF SERVICE ***'             GA1KPGM 
01671 **        TO  MAP-ERROR-MESSAGE                                   GA1KPGM 
01672 **      GO TO 7010-SEND-SCREEN.                                   GA1KPGM 
01673 **                                                                GA1KPGM 
01674 **   IF PRINT-NO-TEMP-STORAGE                                     GA1KPGM 
01675 **      MOVE '               *** NO TEMP STORAGE ***'             GA1KPGM 
01676 **        TO  MAP-ERROR-MESSAGE                                   GA1KPGM 
01677 **      GO TO 7010-SEND-SCREEN.                                   GA1KPGM 
01678 **                                                                GA1KPGM 
01679 **   IF PRINT-NO-PRINTER                                          GA1KPGM 
01680 **      MOVE '           *** NO PRINTER ATTACHED ***'             GA1KPGM 
01681 **        TO  MAP-ERROR-MESSAGE                                   GA1KPGM 
01682 **      GO TO 7010-SEND-SCREEN.                                   GA1KPGM 
01683 **                                                                GA1KPGM 
01684 **   MOVE '                  *** TS INT CTL ERR ***'              GA1KPGM 
01685 **     TO  MAP-ERROR-MESSAGE.                                     GA1KPGM 
01686 **                                                                GA1KPGM 
01687 *7010-SEND-SCREEN.                                                GA1KPGM 
01688 **   MOVE '7010'  TO  WS-PARA-ID.                                 GA1KPGM 
01689 **                                                                GA1KPGM 
01690 **   EXEC CICS SEND   MAP('GA1KI01') MAPSET('GA1KSET') DATAONLY   GA1KPGM 
01691 **      FROM(GA1KI01O) CURSOR END-EXEC.                           GA1KPGM 
01692 **                                                                GA1KPGM 
01693 *7099-EXIT.     EXIT.                                             GA1KPGM 
01694      EJECT                                                        GA1KPGM 
01695 ******************************************************************GA1KPGM 
01696  9010-NO-STORAGE SECTION.                                         GA1KPGM 
01697                                                                   GA1KPGM 
01698      MOVE '1KS1'  TO  WS-ABEND-CODE.                              GA1KPGM 
01699      MOVE '*** CICS IS UNABLE TO FIND STORAGE REQUESTED BY THIS PGGA1KPGM 
01700 -    'M, NOTIFY SYSTEMS ***'  TO  MAP-ERROR-MESSAGE.              GA1KPGM 
01701                                                                   GA1KPGM 
01702      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1KPGM 
01703                                                                   GA1KPGM 
01704  9019-EXIT.     EXIT.                                             GA1KPGM 
01705      SKIP3                                                        GA1KPGM 
01706      SKIP3                                                        GA1KPGM 
01707 ******************************************************************GA1KPGM 
01708  9020-PGM-ID-ERROR SECTION.                                       GA1KPGM 
01709                                                                   GA1KPGM 
01710 **     THIS ERROR CAN BE INVOKED BY A NUMBER OF DIFFERENT REQUESTSGA1KPGM 
01711 **     THE PROGRAMMER SHOULD CHECK THE WS-PARA-ID FIELD IN THE    GA1KPGM 
01712 **     DUMP TO DETERMINE WHAT CODE CAUSED THIS ABEND.             GA1KPGM 
01713                                                                   GA1KPGM 
01714      MOVE '1KP2'  TO  WS-ABEND-CODE.                              GA1KPGM 
01715      MOVE '*** CICS CAN T ACCESS A PGM, TABLE, OR MAP FOR THIS PGMGA1KPGM 
01716 -    '. CALL SYSTEMS ***'  TO  MAP-ERROR-MESSAGE.                 GA1KPGM 
01717                                                                   GA1KPGM 
01718      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1KPGM 
01719                                                                   GA1KPGM 
01720  9029-EXIT.     EXIT.                                             GA1KPGM 
01721      SKIP3                                                        GA1KPGM 
01722      SKIP3                                                        GA1KPGM 
01723 ******************************************************************GA1KPGM 
01724  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1KPGM 
01725                                                                   GA1KPGM 
01726      SET MAP-IDX1  TO  7.                                         GA1KPGM 
01727      SET MAP-IDX2  TO  1.                                         GA1KPGM 
01728      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1KPGM 
01729                                                                   GA1KPGM 
01730      EXEC CICS SEND   MAP('GA1KI01') MAPSET('GA1KSET') ERASE      GA1KPGM 
01731         FROM(GA1KI01O) WAIT END-EXEC.                             GA1KPGM 
01732                                                                   GA1KPGM 
01733      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1KPGM 
01734                                                                   GA1KPGM 
01735  9999-EXIT.     EXIT.                                             GA1KPGM 
