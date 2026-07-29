00001  ID DIVISION.                                                     09/03/03
00002  PROGRAM-ID.     GA1JPGM.                                         GA1JPGM 
00003 ***  THIS IS A COBOL II PROGRAM                                      LV003
00004  AUTHOR.         S BUCH.                                          GA1JPGM 
00005  DATE-WRITTEN.   02/07/85.                                        GA1JPGM 
00006  DATE-COMPILED.                                                   GA1JPGM 
00007      SKIP3                                                        GA1JPGM 
00008 ******************************************************************GA1JPGM 
00009 *                        REVISIONS                                GA1JPGM 
00010 ******************************************************************GA1JPGM 
00011 *   DATE    BY   DESCRIPTION                                      GA1JPGM 
00012 * --------  ---  -------------------------------------------------GA1JPGM 
00013 * 01/16/86  ENW  CHANGED WS-CONTRACT-FIXED-PORITION FROM 483 TO 56GA1JPGM 
00014 *                        WS-CONTRACT-KEY-LEN       FROM  12 TO  10GA1JPGM 
00015 *                        WS-CONTRACT-MAX-OCCURS    FROM 450 TO 520GA1JPGM 
00016 *                                                                 GA1JPGM 
00017 * 08/26/86  JLA  1. ADD SUPPORT FOR PF7,8,10 AND 11.              GA1JPGM 
00018 *   (D136)       2. ADD  SELECT FIELD.                            GA1JPGM 
00019 *                3. ADD LOCATION COUNTERS(I.E 1 TO 36             GA1JPGM 
00020 *                   OF 54 PROCEDURES DISPLAYED) TO SCREEN.        GA1JPGM 
00021 *                4. USE USER DEFINED LOGICAL MAP FOR              GA1JPGM 
00022 *                   SCREEN.  THIS REPLACES THE PARTIAL            GA1JPGM 
00023 *                   USE OF BMS MAP AND USER DEFINED.              GA1JPGM 
00024 *                                                                 GA1JPGM 
00025 * 01/30/87  JLA  CHANGES FOR SINGLE TABULAR SUPPORT               GA1JPGM 
00026 *   D0120        EXECUTED FROM TRANSACTION GTM1:                  GA1JPGM 
00027 *                1. PF1/PF13 - CONSTRUCT COMMAREA AS              GA1JPGM 
00028 *                   IF GC4A HAD CALLED, XCTL TO ADD               GA1JPGM 
00029 *                   SCREEN PROGRAM.                               GA1JPGM 
00030 *                2. PF3/PF15 - CONSTRUCT COMMAREA AS              GA1JPGM 
00031 *                   IF GC4A HAD CALLED, XCTL TO                   GA1JPGM 
00032 *                   GTM1PGM.                                      GA1JPGM 
00033 *                                                                 GA1JPGM 
00034 *                                                                 GA1JPGM 
00035 *  8/17/87  FRY  CAPTURE OPERATOR-ID WHEN A 'C3', 'C5', OR 'G3'   GA1JPGM 
00036 *   D116         RECORD IS UPDATED.                               GA1JPGM 
00037 *                                                                 GA1JPGM 
00038 *  11161  11/17/90  PFH   CHANGED  PROGRAM TO BRING IN COPYBOOK   GA1JPGM 
00039 *                         GCCDRLEN.  REMOVED PF12/24 HARDCOPY     GA1JPGM 
00040 *                         ROUTINES.                               GA1JPGM 
00041 *                                                                 GA1JPGM 
00042 *                                                                 GA1JPGM 
00043 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD       GA1JPGM 
00044 *                           FROM ONE POSITION TO TWO POSITIONS.   GA1JPGM 
00045 *                                                                 GA1JPGM 
00046 *D12009 09/27/91  GDM   CONVERT TO COBOL II                       GA1JPGM 
00047 *                                                                 GA1JPGM 
00048 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION        GA1JPGM 
00049 *                                                                 GA1JPGM 
00050 *   D365A   05/06/03    GTF   EXPAND PROCEDURE ARGUMENT FROM 6 TO GA1JPGM 
00051 *                             7 BYTES. CHANGE # OF OCCURS TO 396  GA1JPGM 
00052 *                             ON #ADIP TABULAR.                   GA1JPGM 
      *                                                                         
00025 * ICD-10  07/06/11      BA    EXPAND MAP-SELECT FIELD FROM 6 TO 7.GA1JPGM 
00053 ***************************************************************** GA1JPGM 
00054 ******************************************************************GA1JPGM 
00055 ******************************************************************GA1JPGM 
00056 *   GA1JPGM     ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM  GA1JPGM 
00057 *                               DENTAL INPATIENT            GA1J  GA1JPGM 
00058 *                                                                 GA1JPGM 
00059 *     THIS PROGRAM WILL PERFORM DELETE MAINTENANCE ON ALL ENTRIES GA1JPGM 
00060 *   CURRENTLY ON THE ALL LEVEL TABULAR RECORD.                    GA1JPGM 
00061 *                                                                 GA1JPGM 
00062 *     THE DELETE SCREEN WILL DISPLAY ALL ENTRIES CURRENTLY ON THE GA1JPGM 
00063 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN DECIDE IF   GA1JPGM 
00064 *   ANY ENTRIES WILL BE DELETED.  THE SCREEN ENTRY WILL BE        GA1JPGM 
00065 *   VALIDATED AND A COPY OF THE ENTRIES FROM THE RECORD WILL BE   GA1JPGM 
00066 *   MADE.  ANY MATCHED ENTRIES WILL NOT BE MOVED BACK INTO THE    GA1JPGM 
00067 *   RECORD BEFORE UPDATING THE RECORD.                            GA1JPGM 
00068 *                                                                 GA1JPGM 
00069 *     TO EXECUTE THE ADD PORTION FOR THIS SET OF DATA (ID:#ADIP)  GA1JPGM 
00070 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA1JPGM 
00071 *   XCTL TO TRANS GA2J OR PROGRAM GA2JPGM.  THIS PROGRAM WILL     GA1JPGM 
00072 *   VALIDATE ALL FIELDS AND THEN SEQUENCE ALL ENTRIES IN THE      GA1JPGM 
00073 *   TABLE.                                                        GA1JPGM 
00074 *                                                                 GA1JPGM 
00075 *   FUNC CODE: GA1J                                               GA1JPGM 
00076 *   MAPSET:    GA1JSETC  <<<< REDEFINED BY USER DEFINED MAP >>>>  GA1JPGM 
00077 *   FILES:     GCPSWORK                                           GA1JPGM 
00078 *                                                                 GA1JPGM 
00079 *   PF7/PF19  PAGE BACKWARD.                                      GA1JPGM 
00080 *   PF8/PF20  PAGE FORWARD.                                       GA1JPGM 
00081 *   PF10/PF22 PAGE TO BOTTOM.                                     GA1JPGM 
00082 *   PF11/PF23 PAGE TO TOP.                                        GA1JPGM 
00083 *                                                                 GA1JPGM 
00084 *                                                                 GA1JPGM 
00085 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00086 *                                                                 GA1JPGM 
00087 *    TAILORING INSTRUCTIONS:                                      GA1JPGM 
00088 *                                                                 GA1JPGM 
00089 *              EDIT THE FOLLOWING CHARACTER STRINGS               GA1JPGM 
00090 *                                                                 GA1JPGM 
00091 *              PROGRAM FUNCTION CODE          EX. /GC9I/GA1J/     GA1JPGM 
00092 *              SCREEN PAGE NUMBER                 /009I/001J/     GA1JPGM 
00093 *              ADD PROGRAM FUNCTION CODE          /GA9I/GA2J/     GA1JPGM 
00094 *              BENEFIT PROVISION TABULAR ID       /#PPF/#ADIP/    GA1JPGM 
00095 *              RDW PREFIX FOR TABULAR RECORD      /GBB/GAG/       GA1JPGM 
00096 *                                                                 GA1JPGM 
00097 *     ALL AREAS BETWEEN LINES OF +++++++ MUST BE CHANGED TO       GA1JPGM 
00098 *     MATCH THE ACTUAL TABULAR RECORD FIELDS OR COUNT OF SCREEN   GA1JPGM 
00099 *     OCCURANCES.                                                 GA1JPGM 
00100 *                                                                 GA1JPGM 
00101 *     YOU CAN SCAN FOR /**+**/ TO FIND ALL AREAS IN THIS PROGRAM  GA1JPGM 
00102 *     THAT MUST BE CHANGED.                                       GA1JPGM 
00103 *                                                                 GA1JPGM 
00104 *                                                                 GA1JPGM 
00105 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00106      EJECT                                                        GA1JPGM 
00107  ENVIRONMENT DIVISION.                                            GA1JPGM 
00108      EJECT                                                        GA1JPGM 
00109  DATA DIVISION.                                                   GA1JPGM 
00110  WORKING-STORAGE SECTION.                                         GA1JPGM 
00111  01  WS-BEGIN                    PIC X(24)  VALUE                 GA1JPGM 
00112      '***GA1JPGM WS BEGINS***'.                                   GA1JPGM 
00113  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA1JPGM 
00114                                                                   GA1JPGM 
00115  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA1JPGM 
00116                                                                   GA1JPGM 
00117  01  WS-MISC.                                                     GA1JPGM 
00118      05  WS-SELECT-FROM          PIC S9(4) COMP VALUE +0.         GA1JPGM 
00119      05  WS-SELECT-TO            PIC S9(4) COMP VALUE +0.         GA1JPGM 
00120      05  WS-SELECT-OF            PIC S9(4) COMP VALUE +0.         GA1JPGM 
00121      05  WS-SELECT-FROM-MASK     PIC ZZ9.                         GA1JPGM 
00122      05  WS-SELECT-TO-MASK       PIC ZZ9.                         GA1JPGM 
00123      05  WS-SELECT-OF-MASK       PIC ZZ9.                         GA1JPGM 
00124      05  WS-GAG-INDEX            PIC S9(4) COMP VALUE +0.         GA1JPGM 
00125                                                                   GA1JPGM 
00126 ** MAP COBOL SCREEN DSECTS **                                     GA1JPGM 
00127  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA1JPGM 
00128      '***  I/O MAPAREA ***'.                                      GA1JPGM 
00129  COPY GA1JSETC.                                                   GA1JPGM 
00130 /*****************************************************************GA1JPGM 
00131 ******************************************************************GA1JPGM 
00132 ******************************************************************GA1JPGM 
00133 **                                                              **GA1JPGM 
00134 **    THIS IS A USER DEFINED LOGICAL MAP.  ANY CHANGES TO       **GA1JPGM 
00135 **     MAPSET GA1JSETC AFFECTING IT\
00136 **     FOR HERE.                                                **GA1JPGM 
00137 **                                                              **GA1JPGM 
00138 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA1JPGM 
00139 **                                                              **GA1JPGM 
00140 **                                            JLA 8/26/86       **GA1JPGM 
00141 **                                                              **GA1JPGM 
00142 ******************************************************************GA1JPGM 
00143 ******************************************************************GA1JPGM 
00144 ******************************************************************GA1JPGM 
00145                                                                   GA1JPGM 
00146  01  MAP-USER-DEFINED     REDEFINES   GA1JI01I.                   GA1JPGM 
00147                                                                   GA1JPGM 
00148      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA1JPGM 
00149                                                                   GA1JPGM 
00150      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA1JPGM 
00151      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA1JPGM 
00152      05  MAP-FUNCTION-CODE                PIC X(04).              GA1JPGM 
00153                                                                   GA1JPGM 
00154      05  MAP-TITLE-LINE-LEN               PIC S9(4) COMP SYNC.    GA1JPGM 
00155      05  MAP-TITLE-LINE-ATTR              PIC X.                  GA1JPGM 
00156      05  MAP-TITLE-LINE                   PIC X(40).              GA1JPGM 
00157                                                                   GA1JPGM 
00158      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA1JPGM 
00159      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA1JPGM 
00160      05  MAP-SCREEN-ID                    PIC X(06).              GA1JPGM 
00161                                                                   GA1JPGM 
00162      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA1JPGM 
00163      05  MAP-ID-LINE-ATTR                 PIC X.                  GA1JPGM 
00164      05  MAP-ID-LINE                      PIC X(79).              GA1JPGM 
00165      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA1JPGM 
00166          10  GRP-SPEC-ID-HEADING              PIC X(20).          GA1JPGM 
00167          10  GRP-SPEC-GROUP-HEADING           PIC X(5).           GA1JPGM 
00168          10  GRP-SPEC-GROUP-NO                PIC X(6).           GA1JPGM 
00169          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA1JPGM 
00170          10  GRP-SPEC-SECTION-NO              PIC X(4).           GA1JPGM 
00171          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA1JPGM 
00172          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA1JPGM 
00173          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA1JPGM 
00174          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA1JPGM 
00175          10  FILLER                           PIC X(18).          GA1JPGM 
00176      05  CONTRACT-ID-LINE  REDEFINES  MAP-ID-LINE.                GA1JPGM 
00177          10  CONTRACT-ID-HEADING              PIC X(14).          GA1JPGM 
00178          10  CONTRACT-GROUP-HEADING           PIC X(5).           GA1JPGM 
00179          10  CONTRACT-GROUP-NO                PIC X(6).           GA1JPGM 
00180          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA1JPGM 
00181          10  CONTRACT-SECTION-NO              PIC X(4).           GA1JPGM 
00182          10  CONTRACT-LOB-HEADING             PIC X(6).           GA1JPGM 
00183          10  CONTRACT-LOB                     PIC X.              GA1JPGM 
00184          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA1JPGM 
00185          10  CONTRACT-PROV-CTL                PIC XX.             GA1JPGM 
00186          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA1JPGM 
00187          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA1JPGM 
00188          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA1JPGM 
00189          10  CONTRACT-EFF-DATE                PIC X(6).           GA1JPGM 
00190          10  FILLER                           PIC X(09).          GA1JPGM 
00191      05  BENEFIT-PROVISION-ID-LINE  REDEFINES  MAP-ID-LINE.       GA1JPGM 
00192          10  BEN-PROV-GROUP-HEADING           PIC X(5).           GA1JPGM 
00193          10  BEN-PROV-GROUP-NO                PIC X(6).           GA1JPGM 
00194          10  BEN-PROV-SECTION-HEADING         PIC X(6).           GA1JPGM 
00195          10  BEN-PROV-SECTION-NO              PIC X(4).           GA1JPGM 
00196          10  BEN-PROV-LOB-HEADING             PIC X(6).           GA1JPGM 
00197          10  BEN-PROV-LOB                     PIC X.              GA1JPGM 
00198          10  BEN-PROV-PROV-CTL-HEADING        PIC X(6).           GA1JPGM 
00199          10  BEN-PROV-PROV-CTL                PIC XX.             GA1JPGM 
00200          10  BEN-PROV-FAM-REL-HEADING         PIC X(5).           GA1JPGM 
00201          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA1JPGM 
00202          10  BEN-PROV-EFF-DT-HEADING          PIC X(7).           GA1JPGM 
00203          10  BEN-PROV-EFF-DATE                PIC X(6).           GA1JPGM 
00204          10  BEN-PROV-ID-HEADING              PIC X(8).           GA1JPGM 
00205          10  BEN-PROV-ID-NO                   PIC X(6).           GA1JPGM 
00206          10  FILLER                           PIC X(09).          GA1JPGM 
00207                                                                   GA1JPGM 
00208      05  MAP-ALL-LEVEL-TAB-ID-LEN         PIC S9(4) COMP SYNC.    GA1JPGM 
00209      05  MAP-ALL-LEVEL-TAB-ID-ATTR        PIC X.                  GA1JPGM 
00210      05  MAP-ALL-LEVEL-TAB-ID             PIC X(06).              GA1JPGM 
00211                                                                   GA1JPGM 
00212      05  MAP-ALL-LEVEL-TAB-SLOT-LEN       PIC S9(4) COMP SYNC.    GA1JPGM 
00213      05  MAP-ALL-LEVEL-TAB-SLOT-ATTR      PIC X.                  GA1JPGM 
00214      05  MAP-ALL-LEVEL-TAB-SLOT           PIC X(07).              GA1JPGM 
00215                                                                   GA1JPGM 
00216      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA1JPGM 
00217      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA1JPGM 
00218      05  MAP-FROM-MENU-ID                 PIC X(04).              GA1JPGM 
00219                                                                   GA1JPGM 
00220      05  MAP-SELECT-LABEL-LEN             PIC S9(4) COMP SYNC.    GA1JPGM 
00221      05  MAP-SELECT-LABEL-ATTR            PIC X.                  GA1JPGM 
00222      05  MAP-SELECT-LABEL                 PIC X(07).              GA1JPGM 
00223                                                                   GA1JPGM 
00224      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA1JPGM 
00225      05  MAP-SELECT-ATTR                  PIC X.                  GA1JPGM 
00226      05  MAP-SELECT                       PIC X(07).              GA1JPGM 
00227                                                                   GA1JPGM 
00228      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA1JPGM 
00229      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA1JPGM 
00230      05  MAP-SELECT-FROM                  PIC X(03).              GA1JPGM 
00231                                                                   GA1JPGM 
00232      05  MAP-SELECT-TO-LABEL-LEN          PIC S9(4) COMP SYNC.    GA1JPGM 
00233      05  MAP-SELECT-TO-LABEL-ATTR         PIC X.                  GA1JPGM 
00234      05  MAP-SELECT-TO-LABEL              PIC X(02).              GA1JPGM 
00235                                                                   GA1JPGM 
00236      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA1JPGM 
00237      05  MAP-SELECT-TO-ATTR               PIC X.                  GA1JPGM 
00238      05  MAP-SELECT-TO                    PIC X(03).              GA1JPGM 
00239                                                                   GA1JPGM 
00240      05  MAP-SELECT-OF-LABEL-LEN          PIC S9(4) COMP SYNC.    GA1JPGM 
00241      05  MAP-SELECT-OF-LABEL-ATTR         PIC X.                  GA1JPGM 
00242      05  MAP-SELECT-OF-LABEL              PIC X(02).              GA1JPGM 
00243                                                                   GA1JPGM 
00244      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA1JPGM 
00245      05  MAP-SELECT-OF-ATTR               PIC X.                  GA1JPGM 
00246      05  MAP-SELECT-OF                    PIC X(03).              GA1JPGM 
00247                                                                   GA1JPGM 
00248      05  MAP-SELECT-DISPLAY-LABEL-LEN     PIC S9(4) COMP SYNC.    GA1JPGM 
00249      05  MAP-SELECT-DISPLAY-LABEL-ATTR    PIC X.                  GA1JPGM 
00250      05  MAP-SELECT-DISPLAY-LABEL         PIC X(24).              GA1JPGM 
00251                                                                   GA1JPGM 
00252      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA1JPGM 
00253          INDEXED BY MAP-IDX1.                                     GA1JPGM 
00254        10  MAP-PROCEDURE-ARGUMENT-COL  OCCURS 2 TIMES             GA1JPGM 
00255            INDEXED BY  MAP-IDX2.                                  GA1JPGM 
00256          15  MAP-ACTION-CODE-LEN          PIC S9(4) COMP SYNC.    GA1JPGM 
00257          15  MAP-ACTION-CODE-ATTR         PIC X.                  GA1JPGM 
00258          15  MAP-ACTION-CODE              PIC X.                  GA1JPGM 
00259          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA1JPGM 
00260          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA1JPGM 
00261          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA1JPGM 
00262          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA1JPGM 
00263          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA1JPGM 
00264          15  MAP-CODE-FUNCTION            PIC X(3).               GA1JPGM 
00265                                                                   GA1JPGM 
00266      05  MAP-PAGING-LABEL-LEN             PIC S9(4) COMP SYNC.    GA1JPGM 
00267      05  MAP-PAGING-LABEL-ATTR            PIC X.                  GA1JPGM 
00268      05  MAP-PAGING-LABEL                 PIC X(79).              GA1JPGM 
00269                                                                   GA1JPGM 
00270      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA1JPGM 
00271      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA1JPGM 
00272      05  MAP-ERROR-MESSAGE                PIC X(79).              GA1JPGM 
00273                                                                   GA1JPGM 
00274 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00275  01  WS-MAP-OCCURS-COUNTERS.                                      GA1JPGM 
00276 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00277 **  THIS AREA MUST BE CHANGED TO NUMBER OF OCCURS FROM MAP.       GA1JPGM 
00278 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00279      05  WS-MAP-ROW              PIC S9(3)  COMP-3  VALUE +15.    GA1JPGM 
00280      05  WS-MAP-COL              PIC S9(3)  COMP-3  VALUE +2.     GA1JPGM 
00281 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00282      EJECT                                                        GA1JPGM 
00283 ** ALTERNATIVE WORKFILE KEYS **                                   GA1JPGM 
00284  01  FILLER                      PIC X(32)  VALUE                 GA1JPGM 
00285      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA1JPGM 
00286  01  WS-ALT-WORKFILE-KEYS.                                        GA1JPGM 
00287  COPY GCWRKKEY.                                                   GA1JPGM 
00288      EJECT                                                        GA1JPGM 
00289 ** HARDCOPY WORK AREA **                                          GA1JPGM 
00290 *01  WS-HARDCOPY-COMMAREA.                                        GA1JPGM 
00291 *COPY PRNCOBOL.                                                   GA1JPGM 
00292                                                                   GA1JPGM 
00293      EJECT                                                        GA1JPGM 
00294 ** WORKFIELDS, AND SWITCHES **                                    GA1JPGM 
00295  01  WS-WORK-FIELDS.                                              GA1JPGM 
00296      05  WS-HEX-00                     PIC X.                     GA1JPGM 
00297      05  WS-DELETE-COUNT               PIC 999  COMP-3.           GA1JPGM 
00298 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00299 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00300 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00301      05  WS-SAVED-FIELDS.                                         GA1JPGM 
00302        10  WS-SAVED-PROCED-ARGUMENT    PIC X(7).                  GA1JPGM 
00303        10  WS-SAVED-CODE-FUNCTION      PIC X(3).                  GA1JPGM 
00304 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00305  01  WS-SWITCHES.                                                 GA1JPGM 
00306      05  WS-ERROR-SW                   PIC X.                     GA1JPGM 
00307                                                                   GA1JPGM 
00308 ** TITLE LINES **                                                 GA1JPGM 
00309  01  WS-TITLE-LINES.                                              GA1JPGM 
00310      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(40)  VALUE    GA1JPGM 
00311          ' GROUP SPECIFIC CONDITIONAL PROCEDURES  '.              GA1JPGM 
00312      05  CONTRACT-TITLE-LINE                  PIC X(40)  VALUE    GA1JPGM 
00313          '    CONTRACT CONDITIONAL PROCEDURES     '.              GA1JPGM 
00314      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(40)  VALUE    GA1JPGM 
00315          'BENEFIT PROVISION CONDITIONAL PROCEDURES'.              GA1JPGM 
00316                                                                   GA1JPGM 
00317      EJECT                                                        GA1JPGM 
00318 ** ATTRIBUTES **                                                  GA1JPGM 
00319  COPY DFHBMSCA.                                                   GA1JPGM 
00320      02  DFHBMABF                PIC X VALUE 'Z'.                 GA1JPGM 
00321      EJECT                                                        GA1JPGM 
00322 ** ATTENTION IDENTIFIERS **                                       GA1JPGM 
00323  COPY DFHAID.                                                     GA1JPGM 
00324      EJECT                                                        GA1JPGM 
00325 ** RECORD LENGTHS **                                              GA1JPGM 
00326  01  WS-RECORD-LENGTHS.                                           GA1JPGM 
00327      COPY GCCDRLEN.                                               GA1JPGM 
00328                                                                   GA1JPGM 
00329     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA1JPGM 
00330     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA1JPGM 
00331     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA1JPGM 
00332     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA1JPGM 
00333 *   05 WS-GCIO-PARM-LENGTH            PIC S9(5) COMP-3 VALUE +228.GA1JPGM 
00334 *   05 WS-WORK-RECORD-KEY-LENGTH      PIC S9(5) COMP-3 VALUE +64. GA1JPGM 
00335 *   05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA1JPGM 
00336 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1JPGM 
00337 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA1JPGM 
00338 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA1JPGM 
00339 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1JPGM 
00340 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA1JPGM 
00341 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA1JPGM 
00342 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA1JPGM 
00343 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA1JPGM 
00344 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00345 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00346 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00347 *   05 WS-TABULAR-FIXED-PORTION       PIC S9(5) COMP-3 VALUE +40. GA1JPGM 
00348 *   05 WS-TABULAR-VARIABLE-PORTION    PIC S9(5) COMP-3 VALUE +9.  GA1JPGM 
00349 *   05 WS-TABULAR-MAX-OCCURS          PIC S9(5) COMP-3 VALUE +440.GA1JPGM 
00350 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00351  01  WS-ONE-LOW                  PIC X(01) VALUE LOW-VALUES.      GA1JPGM 
00352 /                                                                 GA1JPGM 
00353  01  COMMAREA-POINTER-AREA.                                       GA1JPGM 
00354      05  COMMAREA-PNTR-COMP  PIC S9(8)  COMP.                     GA1JPGM 
00355      05  COMMAREA-PNTR       REDEFINES                            GA1JPGM 
00356          COMMAREA-PNTR-COMP  USAGE IS POINTER.                    GA1JPGM 
00357 /                                                                 GA1JPGM 
00358                                                                   GA1JPGM 
00359  01  WS-END                      PIC X(16)  VALUE                 GA1JPGM 
00360      '*** W/S ENDS ***'.                                          GA1JPGM 
00361      EJECT                                                        GA1JPGM 
00362  LINKAGE SECTION.                                                 GA1JPGM 
00363  01  DFHCOMMAREA.                                                 GA1JPGM 
00364      COPY G2ALCKEC.                                               GA1JPGM 
00365 *    05  INCOMING-COMMAREA-PNTR-COMP  PIC S9(8)  COMP.            GA1JPGM 
00366 *    05  INCOMING-COMMAREA-PNTR       REDEFINES                   GA1JPGM 
00367 *        INCOMING-COMMAREA-PNTR-COMP  USAGE IS POINTER.           GA1JPGM 
00368 *                                                                 GA1JPGM 
00369 *01  BLL-CELLS.                                                   GA1JPGM 
00370 *    02  FILLER                  PIC S9(8)  COMP.                 GA1JPGM 
00371 *    02  COMMAREA-PNTR           PIC S9(8)  COMP.                 GA1JPGM 
00372 *    02  ALL-LEVEL-TAB-PNTR      PIC S9(8)  COMP.                 GA1JPGM 
00373 *    02  ALL-LEVEL-TAB-PNTR2     PIC S9(8)  COMP.                 GA1JPGM 
00374 *    02  COPY-AREA-PNTR          PIC S9(8)  COMP.                 GA1JPGM 
00375 *    02  GRP-SPEC-PNTR           PIC S9(8)  COMP.                 GA1JPGM 
00376 *    02  CONTRACT-PNTR           PIC S9(8)  COMP.                 GA1JPGM 
00377 *    02  CONTRACT-PNTR2          PIC S9(8)  COMP.                 GA1JPGM 
00378 *    02  BEN-PROV-PNTR           PIC S9(8)  COMP.                 GA1JPGM 
00379 *                                                                 GA1JPGM 
00380 *01  GCA-COMMAREA.                                                GA1JPGM 
00381 *COPY G2ALCKEC.                                                   GA1JPGM 
00382      EJECT                                                        GA1JPGM 
00383  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA1JPGM 
00384  COPY GCIOPRM1.                                                   GA1JPGM 
00385      EJECT                                                        GA1JPGM 
00386  COPY GCWRKDCC.                                                   GA1JPGM 
00387      SKIP3                                                        GA1JPGM 
00388      SKIP3                                                        GA1JPGM 
00389      SKIP3                                                        GA1JPGM 
00390  COPY GCTADIPC.                                                   GA1JPGM 
00391      EJECT                                                        GA1JPGM 
00392 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00393 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00394 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00395  01  COPY-OF-TABLE-AREA.                                          GA1JPGM 
00396      05  COPY-OF-TABLE   OCCURS 396 TIMES   INDEXED BY  COPY-IDX. GA1JPGM 
00397        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA1JPGM 
00398        10  COPY-CODE-FUNCTION          PIC X(3).                  GA1JPGM 
00399 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00400      EJECT                                                        GA1JPGM 
00401  01  IO-PARM-GRP-SPEC-RECORD.                                     GA1JPGM 
00402  COPY GCIOPRM2.                                                   GA1JPGM 
00403      EJECT                                                        GA1JPGM 
00404  COPY GCWRKDC2.                                                   GA1JPGM 
00405      EJECT                                                        GA1JPGM 
00406  COPY GCGROUPC.                                                   GA1JPGM 
00407      EJECT                                                        GA1JPGM 
00408                                                                   GA1JPGM 
00409  01  IO-PARM-CONTRACT-RECORD.                                     GA1JPGM 
00410  COPY GCIOPRM3.                                                   GA1JPGM 
00411      EJECT                                                        GA1JPGM 
00412  COPY GCWRKDC3.                                                   GA1JPGM 
00413      EJECT                                                        GA1JPGM 
00414  COPY GCCONTRC.                                                   GA1JPGM 
00415      EJECT                                                        GA1JPGM 
00416                                                                   GA1JPGM 
00417  01  IO-PARM-BEN-PROV-RECORD.                                     GA1JPGM 
00418  COPY GCIOPRM4.                                                   GA1JPGM 
00419      EJECT                                                        GA1JPGM 
00420  COPY GCWRKDC4.                                                   GA1JPGM 
00421      EJECT                                                        GA1JPGM 
00422  COPY GCBENPVC.                                                   GA1JPGM 
00423      EJECT                                                        GA1JPGM 
00424  PROCEDURE DIVISION.                                              GA1JPGM 
00425                                                                   GA1JPGM 
00426 ******************************************************************GA1JPGM 
00427 **                H O U S E K E E P I N G                         GA1JPGM 
00428 **                                                                GA1JPGM 
00429 ** DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM. GA1JPGM 
00430 **                                                                GA1JPGM 
00431 ******************************************************************GA1JPGM 
00432  0000-HOUSEKEEPING SECTION.                                       GA1JPGM 
00433                                                                   GA1JPGM 
00434      MOVE  LOW-VALUES  TO  WS-HEX-00.                             GA1JPGM 
00435      IF EIBAID  =  DFHCLEAR                                       GA1JPGM 
00436          EXEC CICS SEND FROM(WS-ONE-LOW)                          GA1JPGM 
00437                         ERASE                                     GA1JPGM 
00438          END-EXEC                                                 GA1JPGM 
00439          EXEC CICS RETURN                                         GA1JPGM 
00440          END-EXEC.                                                GA1JPGM 
00441                                                                   GA1JPGM 
00442      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA1JPGM 
00443         NOSTG(9010-NO-STORAGE)                                    GA1JPGM 
00444         PGMIDERR(9020-PGM-ID-ERROR)   END-EXEC.                   GA1JPGM 
00445      EJECT                                                        GA1JPGM 
00446 ******************************************************************GA1JPGM 
00447 **                     M A I N L I N E                            GA1JPGM 
00448 **                                                                GA1JPGM 
00449 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA1JPGM 
00450 **  TAKEN BY THE OPERATOR.                                        GA1JPGM 
00451 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA1JPGM 
00452 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO DETERMINE  GA1JPGM 
00453 **     WHICH ENTRIES, IF ANY, THEY MIGHT WANT TO DELETE.          GA1JPGM 
00454 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA1JPGM 
00455 **     KEY PF12 OR PF24.                                          GA1JPGM 
00456 **  3. RECEIVE THE SCREEN.                                        GA1JPGM 
00457 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA1JPGM 
00458 **     MENU.                                                      GA1JPGM 
00459 **  5. IF THEY USED THE ENTER PF7/PF19, PF8/PF20, PF10/PF22,      GA1JPGM 
00460 **     PF11/PF23 KEY THEN PERFORM NORMAL DELETE LOGIC.            GA1JPGM 
00461 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA1JPGM 
00462 **     (RETURN) TO THE ADD PROGRAM (GA2JPGM).                     GA1JPGM 
00463 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA1JPGM 
00464 **     (RETURN) TO THE PREVIOUS MENU.                             GA1JPGM 
00465 **  8. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA1JPGM 
00466 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA1JPGM 
00467 **                                                                GA1JPGM 
00468 ******************************************************************GA1JPGM 
00469  1000-MAIN-LINE SECTION.                                          GA1JPGM 
00470                                                                   GA1JPGM 
00471      MOVE '1000'  TO  WS-PARA-ID.                                 GA1JPGM 
00472      IF EIBTRNID  NOT =  'GA1J'                                   GA1JPGM 
00473         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA1JPGM 
00474         GO TO 1099-RETURN.                                        GA1JPGM 
00475                                                                   GA1JPGM 
00476 ******************************************************************GA1JPGM 
00477 ***  IF EIBAID  =  DFHPF12 OR  =  DFHPF24                      ***GA1JPGM 
00478 ***     PERFORM 7000-PRINT-HARDCOPY                            ***GA1JPGM 
00479 ***     GO TO 1099-RETURN.                                     ***GA1JPGM 
00480 ******************************************************************GA1JPGM 
00481                                                                   GA1JPGM 
00482      EXEC CICS RECEIVE   MAP('GA1JI01') MAPSET('GA1JSET')         GA1JPGM 
00483         INTO(GA1JI01I) END-EXEC.                                  GA1JPGM 
00484                                                                   GA1JPGM 
00485      IF MAP-SCREEN-ID  NOT = '001J00'                             GA1JPGM 
00486         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA1JPGM 
00487                                                                   GA1JPGM 
00488      IF EIBAID  =  DFHENTER OR                                    GA1JPGM 
00489                    DFHPF7   OR DFHPF19 OR                         GA1JPGM 
00490                    DFHPF8   OR DFHPF20 OR                         GA1JPGM 
00491                    DFHPF10  OR DFHPF22 OR                         GA1JPGM 
00492                    DFHPF11  OR DFHPF23                            GA1JPGM 
00493         PERFORM 2000-DELETE-PROCESSING                            GA1JPGM 
00494         GO TO 1099-RETURN.                                        GA1JPGM 
00495                                                                   GA1JPGM 
00496      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA1JPGM 
00497         PERFORM 3000-XCTL-TO-ADD-SCREEN.                          GA1JPGM 
00498                                                                   GA1JPGM 
00499      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA1JPGM 
00500         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA1JPGM 
00501                                                                   GA1JPGM 
00502      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1JPGM 
00503      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1JPGM 
00504      MOVE '*** INVALID REQUEST. THE PF KEY USED HAS NO MEANING TO GA1JPGM 
00505 -    'THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                   GA1JPGM 
00506      EXEC CICS SEND   MAP('GA1JI01') MAPSET('GA1JSET') DATAONLY   GA1JPGM 
00507         FROM(GA1JI01O) CURSOR END-EXEC.                           GA1JPGM 
00508                                                                   GA1JPGM 
00509  1099-RETURN.                                                     GA1JPGM 
00510 *    EXEC CICS RETURN   END-EXEC.                                 GA1JPGM 
00511      EXEC CICS RETURN TRANSID ('GA1J')                            GA1JPGM 
00512                COMMAREA (DFHCOMMAREA)                             GA1JPGM 
00513      END-EXEC.                                                    GA1JPGM 
00514                                                                   GA1JPGM 
00515      GOBACK.                                                      GA1JPGM 
00516      EJECT                                                        GA1JPGM 
00517 ******************************************************************GA1JPGM 
00518 **              D E L E T E   P R O C E S S I N G                 GA1JPGM 
00519 **                                                                GA1JPGM 
00520 **  WE WILL PERFORM THE FOLLOWING OPERATIONS IN DELETE PROCESSING:GA1JPGM 
00521 ** 1. VALIDATE THAT THE ACTION CODE IS EITHER BLANK, 'D', OR LOW- GA1JPGM 
00522 **    VALUES (IF THE OPERATOR KEYED ERASE EOF).                   GA1JPGM 
00523 ** 2. READ THE TABULAR RECORD AND MAKE A COPY OF THE RECORD.      GA1JPGM 
00524 **    (WE WILL BE MOVING ENTRIES THAT AREN'T DELETED FROM THE COPYGA1JPGM 
00525 **    BACK INTO THE RECORD THAT WE READ.)                         GA1JPGM 
00526 ** 3. FIND THE ENTRY IN THE COPY THAT CORRESPONDS TO THE ENTRY ON GA1JPGM 
00527 **    THE SCREEN.  IF THE SCREEN HAS BEEN POSITIONED PAST SOME    GA1JPGM 
00528 **    ENTRIES IN THE COPY THEY WILL BE MOVED BACK INTO THE RECORD.GA1JPGM 
00529 ** 4. IF THE ENTRY ON THE SCREEN AND IN THE COPY MATCH BUT THE    GA1JPGM 
00530 **    ENTRY IS NOT MARKED FOR DELETION THEN SAVE THE ENTRY.       GA1JPGM 
00531 ** 5. IF THE TWO ENTRIES MATCH AND IT IS MARKED FOR DELETION THEN GA1JPGM 
00532 **    POSITION THE INDEX FOR THE SCREEN AND FOR THE COPY PAST THISGA1JPGM 
00533 **    ENTRY.                                                      GA1JPGM 
00534 ** 6. IF WE GET PAST THE LAST ENTRY ON THE SCREEN AND THERE ARE   GA1JPGM 
00535 **    MORE ENTRIES IN THE COPY THEN MOVE ALL OF THEM BACK INTO THEGA1JPGM 
00536 **    RECORD.                                                     GA1JPGM 
00537 ** 7. FINALLY REWRITE THE RECORD BACK ONTO THE WORKFILE.  SAVE THEGA1JPGM 
00538 **    NEXT ENTRY TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD THEGA1JPGM 
00539 **    DISPLAY, AND SEND THE SCREEN TO THE OPERATOR.               GA1JPGM 
00540 ** 8. IF NO ENTRIES WERE MARKED FOR DELETION THEN STEPS 2 THRU 7  GA1JPGM 
00541 **    ARE BYPASSED; WE READ THE ALL LEVEL TABULAR RECORD:         GA1JPGM 
00542 **     A. IF ENTER WAS KEYED - SAVE THE NEXT ENTRY TO BE DISPLAYEDGA1JPGM 
00543 **        PERFORM THE ROUTINE TO BUILD THE DISPLAY, AND SEND THE  GA1JPGM 
00544 **        SCREEN TO THE OPERATOR.                                 GA1JPGM 
00545 **     B. IF PF7/PF19  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1JPGM 
00546 **        PREVIOUS PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO   GA1JPGM 
00547 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1JPGM 
00548 **     C. IF PF8/PF20  KEYED - COMPUTE THE FIRST ENTRY OF THE     GA1JPGM 
00549 **        NEXT PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1JPGM 
00550 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1JPGM 
00551 **     D. IF PF10/PF22  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1JPGM 
00552 **        LAST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILD GA1JPGM 
00553 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1JPGM 
00554 **     E. IF PF11/PF23  KEYED - COMPUTE THE FIRST ENTRY OF THE    GA1JPGM 
00555 **        FIRST PAGE TO BE DISPLAYED, PERFORM THE ROUTINE TO BUILDGA1JPGM 
00556 **        BUILD THE DISPLAY, AND SEND THE SCREEN TO THE OPERATOR. GA1JPGM 
00557 **                                                                GA1JPGM 
00558 **                                                                GA1JPGM 
00559 ******************************************************************GA1JPGM 
00560  2000-DELETE-PROCESSING SECTION.                                  GA1JPGM 
00561                                                                   GA1JPGM 
00562      MOVE '2000'  TO  WS-PARA-ID.                                 GA1JPGM 
00563      MOVE 'N'  TO  WS-ERROR-SW.                                   GA1JPGM 
00564      MOVE ZERO  TO  WS-DELETE-COUNT.                              GA1JPGM 
00565      SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1JPGM 
00566                                                                   GA1JPGM 
00567      MOVE '2010'  TO  WS-PARA-ID.                                 GA1JPGM 
00568  2010-VALIDATE-ACT-CODE.                                          GA1JPGM 
00569      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D'               GA1JPGM 
00570         ADD 1  TO  WS-DELETE-COUNT.                               GA1JPGM 
00571      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2) =  'D' OR            GA1JPGM 
00572         =  SPACE OR  =  LOW-VALUES                                GA1JPGM 
00573         MOVE DFHBMUNF  TO                                         GA1JPGM 
00574            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1JPGM 
00575         MOVE DFHBMASF  TO                                         GA1JPGM 
00576 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00577 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00578 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00579            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1JPGM 
00580            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1JPGM 
00581      ELSE                                                         GA1JPGM 
00582         MOVE DFHBMUBF  TO                                         GA1JPGM 
00583            MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)              GA1JPGM 
00584         MOVE DFHBMABF  TO                                         GA1JPGM 
00585            MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX1, MAP-IDX2)       GA1JPGM 
00586            MAP-CODE-FUNCTION-ATTR (MAP-IDX1, MAP-IDX2)            GA1JPGM 
00587 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00588         IF WS-ERROR-SW  NOT =  'Y'                                GA1JPGM 
00589            MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2)  GA1JPGM 
00590            MOVE 'Y'  TO  WS-ERROR-SW.                             GA1JPGM 
00591                                                                   GA1JPGM 
00592      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1JPGM 
00593         SET MAP-IDX1  UP BY  1                                    GA1JPGM 
00594      ELSE                                                         GA1JPGM 
00595         IF MAP-IDX2  <  WS-MAP-COL                                GA1JPGM 
00596            SET MAP-IDX1  TO  1                                    GA1JPGM 
00597            SET MAP-IDX2  UP BY  1                                 GA1JPGM 
00598         ELSE                                                      GA1JPGM 
00599            GO TO 2020-DONE-VALIDATE-A-C.                          GA1JPGM 
00600 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00601 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00602 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00603      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)               GA1JPGM 
00604             NOT =  LOW-VALUES  AND                                GA1JPGM 
00605         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2)                    GA1JPGM 
00606             NOT =  LOW-VALUES                                     GA1JPGM 
00607 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00608         GO TO 2010-VALIDATE-ACT-CODE.                             GA1JPGM 
00609                                                                   GA1JPGM 
00610  2020-DONE-VALIDATE-A-C.                                          GA1JPGM 
00611      MOVE '2020'  TO  WS-PARA-ID.                                 GA1JPGM 
00612      SET MAP-IDX1  TO  1.                                         GA1JPGM 
00613      IF WS-ERROR-SW  =  'Y'                                       GA1JPGM 
00614         MOVE '*** INVALID ACTION CODE FOUND ***'  TO              GA1JPGM 
00615            MAP-ERROR-MESSAGE                                      GA1JPGM 
00616         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA1JPGM 
00617                              MAP-TITLE-LINE                       GA1JPGM 
00618                              MAP-SCREEN-ID                        GA1JPGM 
00619                              MAP-ALL-LEVEL-TAB-ID                 GA1JPGM 
00620                              MAP-ALL-LEVEL-TAB-SLOT               GA1JPGM 
00621                              MAP-ID-LINE                          GA1JPGM 
00622                              MAP-FROM-MENU-ID                     GA1JPGM 
00623 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00624 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1JPGM 
00625 **  ADD ITS MAP FIELD NAME HERE.                                  GA1JPGM 
00626 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00627         MOVE '2100'  TO  WS-PARA-ID                               GA1JPGM 
00628         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA1JPGM 
00629            VARYING MAP-IDX2 FROM  1  BY  1                        GA1JPGM 
00630                             UNTIL MAP-IDX2  >  WS-MAP-COL         GA1JPGM 
00631              AFTER MAP-IDX1 FROM  1  BY  1                        GA1JPGM 
00632                             UNTIL MAP-IDX1  > WS-MAP-ROW          GA1JPGM 
00633         EXEC CICS SEND   MAP('GA1JI01') MAPSET('GA1JSET') DATAONLYGA1JPGM 
00634            FROM(GA1JI01O) CURSOR END-EXEC                         GA1JPGM 
00635         GO TO 2099-EXIT.                                          GA1JPGM 
00636                                                                   GA1JPGM 
00637      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1JPGM 
00638              GC-GCIOPARM-LEN +                                    GA1JPGM 
00639              GC-WORKFILE-KEY-LEN +                                GA1JPGM 
00640              GC-GCTABULR-ADIP-FIXED-LEN +                         GA1JPGM 
00641           (GC-GCTABULR-ADIP-VARY-MAX-OCUR *                       GA1JPGM 
00642                             GC-GCTABULR-ADIP-VARY-LEN).           GA1JPGM 
00643                                                                   GA1JPGM 
00644 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA1JPGM 
00645      EXEC CICS GETMAIN                                            GA1JPGM 
00646         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA1JPGM 
00647         INITIMG(WS-HEX-00)                                        GA1JPGM 
00648         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1JPGM 
00649 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1JPGM 
00650 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1JPGM 
00651                                                                   GA1JPGM 
00652      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1JPGM 
00653         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1JPGM 
00654         MOVE 'G'                    TO GCIO-WRK-STATUS-CODE       GA1JPGM 
00655         MOVE 'G3'                   TO GCIO-WRK-RECORD-TYPE       GA1JPGM 
00656         MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE         GA1JPGM 
00657         MOVE GCA-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3      GA1JPGM 
00658         MOVE GRP-SPEC-GROUP-NO      TO GCIO-WRK-GROUP-NO          GA1JPGM 
00659         MOVE GCA-SEC-NO-1           TO GCIO-WRK-SEC-NO-1          GA1JPGM 
00660         MOVE GRP-SPEC-SECTION-NO    TO GCIO-WRK-SECTION-NO        GA1JPGM 
00661         MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE          GA1JPGM 
00662         MOVE SPACES                 TO GCIO-WRK-LINE-OF-BUS,      GA1JPGM 
00663                                        GCIO-WRK-PROVIDER-CONTROL  GA1JPGM 
00664         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1JPGM 
00665         MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN         GA1JPGM 
00666         MOVE MAP-ALL-LEVEL-TAB-ID   TO GCIO-WRK-PROVISION-ID      GA1JPGM 
00667         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA1JPGM 
00668         MOVE SPACES                 TO GCIO-WRK-TAB-PROVISION-ID  GA1JPGM 
00669         MOVE ZEROES                 TO GCIO-WRK-TAB-PROV-SLOT-NO. GA1JPGM 
00670                                                                   GA1JPGM 
00671      IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA1JPGM 
00672         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1JPGM 
00673         MOVE 'C'                    TO GCIO-WRK-STATUS-CODE       GA1JPGM 
00674         MOVE 'C3'                   TO GCIO-WRK-RECORD-TYPE       GA1JPGM 
00675         MOVE GCA-PLAN-CODE          TO GCIO-WRK-PLAN-CODE         GA1JPGM 
00676         MOVE GCA-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3      GA1JPGM 
00677         MOVE CONTRACT-GROUP-NO      TO GCIO-WRK-GROUP-NO          GA1JPGM 
00678         MOVE GCA-SEC-NO-1           TO GCIO-WRK-SEC-NO-1          GA1JPGM 
00679         MOVE CONTRACT-SECTION-NO    TO GCIO-WRK-SECTION-NO        GA1JPGM 
00680         MOVE GCA-PKG-CODE           TO GCIO-WRK-PKG-CODE          GA1JPGM 
00681         MOVE CONTRACT-LOB           TO GCIO-WRK-LINE-OF-BUS       GA1JPGM 
00682         MOVE CONTRACT-PROV-CTL      TO GCIO-WRK-PROVIDER-CONTROL  GA1JPGM 
00683         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1JPGM 
00684         MOVE GCA-EFFDT-CEN          TO GCIO-WRK-EFFDT-CEN         GA1JPGM 
00685         MOVE MAP-ALL-LEVEL-TAB-ID   TO GCIO-WRK-PROVISION-ID      GA1JPGM 
00686         MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCIO-WRK-PROVISION-SLOT-NO GA1JPGM 
00687         MOVE SPACES                 TO GCIO-WRK-TAB-PROVISION-ID  GA1JPGM 
00688         MOVE ZEROES                 TO GCIO-WRK-TAB-PROV-SLOT-NO. GA1JPGM 
00689                                                                   GA1JPGM 
00690      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1JPGM 
00691         MOVE SPACES               TO GCIO-WORKFILE-KEY            GA1JPGM 
00692         MOVE 'C'                     TO GCIO-WRK-STATUS-CODE      GA1JPGM 
00693         MOVE 'C5'                    TO GCIO-WRK-RECORD-TYPE      GA1JPGM 
00694         MOVE GCA-PLAN-CODE           TO GCIO-WRK-PLAN-CODE        GA1JPGM 
00695         MOVE GCA-GROUP-NO-1-3        TO GCIO-WRK-GROUP-NO-1-3     GA1JPGM 
00696         MOVE BEN-PROV-GROUP-NO       TO GCIO-WRK-GROUP-NO         GA1JPGM 
00697         MOVE GCA-SEC-NO-1            TO GCIO-WRK-SEC-NO-1         GA1JPGM 
00698         MOVE BEN-PROV-SECTION-NO     TO GCIO-WRK-SECTION-NO       GA1JPGM 
00699         MOVE GCA-PKG-CODE            TO GCIO-WRK-PKG-CODE         GA1JPGM 
00700         MOVE BEN-PROV-LOB            TO GCIO-WRK-LINE-OF-BUS      GA1JPGM 
00701         MOVE BEN-PROV-PROV-CTL       TO GCIO-WRK-PROVIDER-CONTROL GA1JPGM 
00702         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA1JPGM 
00703         MOVE GCA-EFFDT-CEN           TO GCIO-WRK-EFFDT-CEN        GA1JPGM 
00704         MOVE BEN-PROV-ID-NO          TO GCIO-WRK-PROVISION-ID     GA1JPGM 
00705         MOVE 9999999                 TO GCIO-WRK-PROVISION-SLOT-NOGA1JPGM 
00706         MOVE MAP-ALL-LEVEL-TAB-ID    TO GCIO-WRK-TAB-PROVISION-ID GA1JPGM 
00707         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-TAB-PROV-SLOT-NO.GA1JPGM 
00708                                                                   GA1JPGM 
00709 **   MOVE  WS-Y  TO  WS-YY.                                       GA1JPGM 
00710 **   IF WS-M  >  2                                                GA1JPGM 
00711 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1JPGM 
00712 **         REMAINDER  WS-REMAINDER                                GA1JPGM 
00713 **   ELSE                                                         GA1JPGM 
00714 **      MOVE 1  TO  WS-REMAINDER.                                 GA1JPGM 
00715 **   SET WS-M-IDX  TO  WS-M.                                      GA1JPGM 
00716 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1JPGM 
00717 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1JPGM 
00718 **   IF WS-REMAINDER  =  ZERO                                     GA1JPGM 
00719 **      ADD 1  TO  WS-DDD.                                        GA1JPGM 
00720 **                                                                GA1JPGM 
00721 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1JPGM 
00722      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA1JPGM 
00723      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA1JPGM 
00724      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA1JPGM 
00725                                                                   GA1JPGM 
00726      IF WS-DELETE-COUNT  =  ZERO                                  GA1JPGM 
00727         GO TO 2080-READ-NEXT-SCREENS-FIELDS.                      GA1JPGM 
00728                                                                   GA1JPGM 
00729                                                                   GA1JPGM 
00730      IF  EIBAID  =  DFHPF7   OR DFHPF19 OR                        GA1JPGM 
00731                     DFHPF8   OR DFHPF20 OR                        GA1JPGM 
00732                     DFHPF10  OR DFHPF22 OR                        GA1JPGM 
00733                     DFHPF11  OR DFHPF23                           GA1JPGM 
00734      THEN                                                         GA1JPGM 
00735          MOVE '*** ACTION CODE ENTRY INVALID WHEN PAGING ***'     GA1JPGM 
00736                           TO MAP-ERROR-MESSAGE                    GA1JPGM 
00737          MOVE -1          TO  MAP-SELECT-LEN                      GA1JPGM 
00738          MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                   GA1JPGM 
00739                               MAP-SCREEN-ID                       GA1JPGM 
00740 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00741 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1JPGM 
00742 **  ADD ITS MAP FIELD NAME HERE.                                  GA1JPGM 
00743 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00744          MOVE '2100'  TO  WS-PARA-ID                              GA1JPGM 
00745          PERFORM 2100-DONT-RETRANSMIT-FIELDS                      GA1JPGM 
00746             VARYING MAP-IDX2 FROM  1  BY  1                       GA1JPGM 
00747                              UNTIL MAP-IDX2  >  WS-MAP-COL        GA1JPGM 
00748               AFTER MAP-IDX1 FROM  1  BY  1                       GA1JPGM 
00749                              UNTIL MAP-IDX1  >  WS-MAP-ROW        GA1JPGM 
00750          EXEC CICS SEND   MAP('GA1JI01')                          GA1JPGM 
00751                           MAPSET('GA1JSET')                       GA1JPGM 
00752                           DATAONLY                                GA1JPGM 
00753                           FROM(GA1JI01O)                          GA1JPGM 
00754                           CURSOR                                  GA1JPGM 
00755                           END-EXEC                                GA1JPGM 
00756          GO TO 2099-EXIT                                          GA1JPGM 
00757      ELSE                                                         GA1JPGM 
00758          NEXT SENTENCE.                                           GA1JPGM 
00759                                                                   GA1JPGM 
00760                                                                   GA1JPGM 
00761 ******************************************************************GA1JPGM 
00762 *      WE FOUND ENTRIES TO DELETE AND THERE WERE NO ERRORS.       GA1JPGM 
00763 *                                                                 GA1JPGM 
00764 ******************************************************************GA1JPGM 
00765                                                                   GA1JPGM 
00766      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1JPGM 
00767      TO   GAG-ENTRY-COUNT.                                        GA1JPGM 
00768                                                                   GA1JPGM 
00769      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1JPGM 
00770                                                                   GA1JPGM 
00771      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1JPGM 
00772         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1JPGM 
00773         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1JPGM 
00774                                                                   GA1JPGM 
00775      IF  NOT GCIO-GOOD-RETURN                                     GA1JPGM 
00776         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1JPGM 
00777 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1JPGM 
00778         MOVE '1JF1'  TO  WS-ABEND-CODE                            GA1JPGM 
00779         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1JPGM 
00780                                                                   GA1JPGM 
00781      COMPUTE  WS-COPY-LENGTH  =                                   GA1JPGM 
00782            GAG-ENTRY-COUNT  *  GC-GCTABULR-ADIP-VARY-LEN.         GA1JPGM 
00783                                                                   GA1JPGM 
00784 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA1JPGM 
00785      EXEC CICS GETMAIN                                            GA1JPGM 
00786         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA1JPGM 
00787         LENGTH(WS-COPY-LENGTH)                                    GA1JPGM 
00788         INITIMG(WS-HEX-00) END-EXEC.                              GA1JPGM 
00789 ***  SERVICE RELOAD COPY-OF-TABLE-AREA.                           GA1JPGM 
00790                                                                   GA1JPGM 
00791      MOVE GAG-ENTRY-COUNT  TO  GAG-ENTRY-COUNT.                   GA1JPGM 
00792      SET COPY-IDX, GAG-INDEX  TO  1.                              GA1JPGM 
00793                                                                   GA1JPGM 
00794      MOVE '2030'  TO  WS-PARA-ID.                                 GA1JPGM 
00795  2030-MAKE-A-COPY-OF-RECORD.                                      GA1JPGM 
00796      IF GAG-INDEX  NOT >  GAG-ENTRY-COUNT                         GA1JPGM 
00797         MOVE GAG-ENTRY (GAG-INDEX)  TO  COPY-OF-TABLE (COPY-IDX)  GA1JPGM 
00798         SET COPY-IDX, GAG-INDEX  UP BY 1                          GA1JPGM 
00799         GO TO 2030-MAKE-A-COPY-OF-RECORD.                         GA1JPGM 
00800                                                                   GA1JPGM 
00801      SET MAP-IDX1, MAP-IDX2, COPY-IDX, GAG-INDEX  TO  1.          GA1JPGM 
00802      MOVE '2040'  TO  WS-PARA-ID.                                 GA1JPGM 
00803  2040-DELETE-MARKED-ENTRIES.                                      GA1JPGM 
00804 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00805 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00806 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00807      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1JPGM 
00808             AND                                                   GA1JPGM 
00809         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1JPGM 
00810         GO TO 2060-SAVE-REST-OF-COPY.                             GA1JPGM 
00811                                                                   GA1JPGM 
00812      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) >             GA1JPGM 
00813         COPY-PROCEDURE-ARGUMENT (COPY-IDX)                        GA1JPGM 
00814         GO TO 2050-SAVE-COPIED-ENTRY                              GA1JPGM 
00815      ELSE                                                         GA1JPGM 
00816         IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) <          GA1JPGM 
00817            COPY-PROCEDURE-ARGUMENT (COPY-IDX)                     GA1JPGM 
00818            MOVE '1JL1'  TO  WS-ABEND-CODE                         GA1JPGM 
00819            MOVE '*** PROGRAM ERROR FOUND IN PARA 2040, PLEASE INFOGA1JPGM 
00820 -    'RM SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                 GA1JPGM 
00821            PERFORM 9999-ERROR-MSG-THEN-ABEND.                     GA1JPGM 
00822                                                                   GA1JPGM 
00823 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00824                                                                   GA1JPGM 
00825      IF MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2)  NOT =  'D'          GA1JPGM 
00826         IF MAP-IDX1  <  WS-MAP-ROW                                GA1JPGM 
00827            SET MAP-IDX1  UP BY  1                                 GA1JPGM 
00828            GO TO 2050-SAVE-COPIED-ENTRY                           GA1JPGM 
00829         ELSE                                                      GA1JPGM 
00830            IF MAP-IDX2  <  WS-MAP-COL                             GA1JPGM 
00831               SET MAP-IDX1  TO  1                                 GA1JPGM 
00832               SET MAP-IDX2  UP BY  1                              GA1JPGM 
00833               GO TO 2050-SAVE-COPIED-ENTRY                        GA1JPGM 
00834            ELSE                                                   GA1JPGM 
00835               GO TO 2060-SAVE-REST-OF-COPY.                       GA1JPGM 
00836                                                                   GA1JPGM 
00837      SET COPY-IDX  UP BY  1.                                      GA1JPGM 
00838      IF COPY-IDX  NOT <  GAG-ENTRY-COUNT                          GA1JPGM 
00839         MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAG-ENTRY (GAG-INDEX)  GA1JPGM 
00840         SET  GAG-ENTRY-COUNT  TO  GAG-INDEX                       GA1JPGM 
00841         MOVE GAG-ENTRY-COUNT  TO  GAG-ENTRY-COUNT                 GA1JPGM 
00842         GO TO 2070-UPDATE-MODIFIED-REC.                           GA1JPGM 
00843                                                                   GA1JPGM 
00844      IF MAP-IDX1  <  WS-MAP-ROW                                   GA1JPGM 
00845         SET MAP-IDX1  UP BY  1                                    GA1JPGM 
00846         GO TO 2040-DELETE-MARKED-ENTRIES.                         GA1JPGM 
00847                                                                   GA1JPGM 
00848      IF MAP-IDX2  <  WS-MAP-COL                                   GA1JPGM 
00849         SET MAP-IDX1  TO  1                                       GA1JPGM 
00850         SET MAP-IDX2  UP BY  1                                    GA1JPGM 
00851         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1JPGM 
00852      ELSE                                                         GA1JPGM 
00853         GO TO 2060-SAVE-REST-OF-COPY.                             GA1JPGM 
00854                                                                   GA1JPGM 
00855  2050-SAVE-COPIED-ENTRY.                                          GA1JPGM 
00856      MOVE '2050'  TO  WS-PARA-ID.                                 GA1JPGM 
00857      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAG-ENTRY (GAG-INDEX).    GA1JPGM 
00858                                                                   GA1JPGM 
00859      SET GAG-INDEX  UP BY  1.                                     GA1JPGM 
00860      IF COPY-IDX  <  GAG-ENTRY-COUNT                              GA1JPGM 
00861         SET COPY-IDX  UP BY  1                                    GA1JPGM 
00862         GO TO 2040-DELETE-MARKED-ENTRIES                          GA1JPGM 
00863      ELSE                                                         GA1JPGM 
00864 ***      SOMETHING'S WRONG WE SHOULDN'T BE IN THIS POSITION.  THE GA1JPGM 
00865 ***      MAP HAS MORE ENTRIES BUT WE HAVE JUST REACHED THE END OF GA1JPGM 
00866 ***      THE TABLE OF ENTRIES.                                    GA1JPGM 
00867         MOVE '1JL2'  TO  WS-ABEND-CODE                            GA1JPGM 
00868         MOVE '*** PROGRAM ERROR FOUND IN PARA 2050, PLEASE INFORM GA1JPGM 
00869 -    'SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE                    GA1JPGM 
00870         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1JPGM 
00871                                                                   GA1JPGM 
00872  2060-SAVE-REST-OF-COPY.                                          GA1JPGM 
00873      MOVE '2060'  TO  WS-PARA-ID.                                 GA1JPGM 
00874      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAG-ENTRY (GAG-INDEX).    GA1JPGM 
00875                                                                   GA1JPGM 
00876      SET GAG-INDEX  UP BY  1.                                     GA1JPGM 
00877      IF COPY-IDX  <  GAG-ENTRY-COUNT                              GA1JPGM 
00878         SET COPY-IDX  UP BY  1                                    GA1JPGM 
00879         GO TO 2060-SAVE-REST-OF-COPY.                             GA1JPGM 
00880                                                                   GA1JPGM 
00881      SET GAG-INDEX  DOWN BY  1.                                   GA1JPGM 
00882      SET GAG-ENTRY-COUNT  TO  GAG-INDEX.                          GA1JPGM 
00883      MOVE GAG-ENTRY-COUNT  TO  GAG-ENTRY-COUNT.                   GA1JPGM 
00884                                                                   GA1JPGM 
00885  2070-UPDATE-MODIFIED-REC.                                        GA1JPGM 
00886      MOVE '2070'  TO  WS-PARA-ID.                                 GA1JPGM 
00887                                                                   GA1JPGM 
00888 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA1JPGM 
00889                                                                   GA1JPGM 
00890      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA1JPGM 
00891      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA1JPGM 
00892                                                                   GA1JPGM 
00893      COMPUTE  GCIO-RECORD-LENGTH  =                               GA1JPGM 
00894              GC-WORKFILE-KEY-LEN        +                         GA1JPGM 
00895              GC-GCTABULR-ADIP-FIXED-LEN +                         GA1JPGM 
00896             (GAG-ENTRY-COUNT  *  GC-GCTABULR-ADIP-VARY-LEN).      GA1JPGM 
00897                                                                   GA1JPGM 
00898      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA1JPGM 
00899              GC-GCIOPARM-LEN      +                               GA1JPGM 
00900              GCIO-RECORD-LENGTH.                                  GA1JPGM 
00901                                                                   GA1JPGM 
00902      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1JPGM 
00903         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1JPGM 
00904         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1JPGM 
00905                                                                   GA1JPGM 
00906      IF GCIO-GOOD-RETURN                                          GA1JPGM 
00907         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1JPGM 
00908      MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASE CGA1JPGM 
00909 -    'ONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.            GA1JPGM 
00910      MOVE '1JF2'  TO  WS-ABEND-CODE.                              GA1JPGM 
00911      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1JPGM 
00912                                                                   GA1JPGM 
00913  2080-READ-NEXT-SCREENS-FIELDS.                                   GA1JPGM 
00914      MOVE  '2080'  TO  WS-PARA-ID.                                GA1JPGM 
00915      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA1JPGM 
00916      TO   GAG-ENTRY-COUNT.                                        GA1JPGM 
00917      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA1JPGM 
00918      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1JPGM 
00919         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA1JPGM 
00920         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA1JPGM 
00921                                                                   GA1JPGM 
00922      IF GCIO-GOOD-RETURN                                          GA1JPGM 
00923         GO TO 2090-BUILD-NEXT-DISPLAY.                            GA1JPGM 
00924      MOVE '1JF3'  TO  WS-ABEND-CODE.                              GA1JPGM 
00925      MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE CONGA1JPGM 
00926 -    'TACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE.              GA1JPGM 
00927      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1JPGM 
00928                                                                   GA1JPGM 
00929  2090-BUILD-NEXT-DISPLAY.                                         GA1JPGM 
00930      MOVE  '2090'  TO  WS-PARA-ID.                                GA1JPGM 
00931      SET MAP-IDX1  TO  WS-MAP-ROW.                                GA1JPGM 
00932      SET MAP-IDX2  TO  WS-MAP-COL.                                GA1JPGM 
00933      SET GAG-INDEX  TO  1.                                        GA1JPGM 
00934 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00935 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
00936 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00937      IF MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES  GA1JPGM 
00938             AND                                                   GA1JPGM 
00939         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) = LOW-VALUES       GA1JPGM 
00940         MOVE GAG-ENTRY (GAG-INDEX)  TO  WS-SAVED-FIELDS           GA1JPGM 
00941      ELSE                                                         GA1JPGM 
00942         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) TO       GA1JPGM 
00943            WS-SAVED-PROCED-ARGUMENT                               GA1JPGM 
00944         MOVE MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2) TO            GA1JPGM 
00945            WS-SAVED-CODE-FUNCTION.                                GA1JPGM 
00946 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
00947                                                                   GA1JPGM 
00948                                                                   GA1JPGM 
00949      IF  MAP-SELECT-LEN > 0        AND                            GA1JPGM 
00950          MAP-SELECT     > SPACES                                  GA1JPGM 
00951          MOVE MAP-SELECT TO WS-SAVED-PROCED-ARGUMENT.             GA1JPGM 
00952                                                                   GA1JPGM 
00953      IF  EIBAID  =  DFHPF7   OR DFHPF19 OR                        GA1JPGM 
00954                     DFHPF8   OR DFHPF20 OR                        GA1JPGM 
00955                     DFHPF10  OR DFHPF22 OR                        GA1JPGM 
00956                     DFHPF11  OR DFHPF23                           GA1JPGM 
00957      THEN                                                         GA1JPGM 
00958          MOVE SPACES TO MAP-SELECT                                GA1JPGM 
00959      ELSE                                                         GA1JPGM 
00960          GO TO 2090-FILL-THE-SCREEN.                              GA1JPGM 
00961                                                                   GA1JPGM 
00962      IF  EIBAID  =  DFHPF7   OR DFHPF19                           GA1JPGM 
00963          GO TO 2090-PAGE-BACKWARD.                                GA1JPGM 
00964      IF  EIBAID  =  DFHPF8   OR DFHPF20                           GA1JPGM 
00965          GO TO 2090-PAGE-FORWARD.                                 GA1JPGM 
00966      IF  EIBAID  =  DFHPF10  OR DFHPF22                           GA1JPGM 
00967          GO TO 2090-PAGE-TO-BOTTOM.                               GA1JPGM 
00968      IF  EIBAID  =  DFHPF11  OR DFHPF23                           GA1JPGM 
00969          GO TO 2090-PAGE-TO-TOP.                                  GA1JPGM 
00970                                                                   GA1JPGM 
00971  2090-PAGE-BACKWARD.                                              GA1JPGM 
00972                                                                   GA1JPGM 
00973      MOVE MAP-PROCEDURE-ARGUMENT(1 1) TO WS-SAVED-PROCED-ARGUMENT.GA1JPGM 
00974                                                                   GA1JPGM 
00975      SEARCH GAG-ENTRY                                             GA1JPGM 
00976          AT END                                                   GA1JPGM 
00977                MOVE GAG-PROCEDURE-ARGUMENT(1)                     GA1JPGM 
00978                  TO WS-SAVED-PROCED-ARGUMENT                      GA1JPGM 
00979                GO TO 2090-FILL-THE-SCREEN                         GA1JPGM 
00980          WHEN                                                     GA1JPGM 
00981                WS-SAVED-PROCED-ARGUMENT =                         GA1JPGM 
00982                GAG-PROCEDURE-ARGUMENT(GAG-INDEX)                  GA1JPGM 
00983                SET WS-GAG-INDEX TO GAG-INDEX.                     GA1JPGM 
00984                                                                   GA1JPGM 
00985      COMPUTE WS-GAG-INDEX = WS-GAG-INDEX                          GA1JPGM 
00986                           - (WS-MAP-ROW * WS-MAP-COL)             GA1JPGM 
00987                           + 1.                                    GA1JPGM 
00988                                                                   GA1JPGM 
00989      IF  WS-GAG-INDEX < +0                                        GA1JPGM 
00990      THEN                                                         GA1JPGM 
00991          MOVE GAG-PROCEDURE-ARGUMENT(1)                           GA1JPGM 
00992            TO WS-SAVED-PROCED-ARGUMENT                            GA1JPGM 
00993      ELSE                                                         GA1JPGM 
00994          SET  GAG-INDEX TO WS-GAG-INDEX                           GA1JPGM 
00995          MOVE GAG-PROCEDURE-ARGUMENT(GAG-INDEX)                   GA1JPGM 
00996            TO WS-SAVED-PROCED-ARGUMENT.                           GA1JPGM 
00997                                                                   GA1JPGM 
00998      GO TO 2090-FILL-THE-SCREEN.                                  GA1JPGM 
00999                                                                   GA1JPGM 
01000  2090-PAGE-FORWARD.                                               GA1JPGM 
01001                                                                   GA1JPGM 
01002      IF  MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2) = LOW-VALUES GA1JPGM 
01003      THEN                                                         GA1JPGM 
01004          MOVE GAG-PROCEDURE-ARGUMENT(GAG-INDEX)                   GA1JPGM 
01005            TO WS-SAVED-PROCED-ARGUMENT                            GA1JPGM 
01006      ELSE                                                         GA1JPGM 
01007          MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2)         GA1JPGM 
01008            TO WS-SAVED-PROCED-ARGUMENT.                           GA1JPGM 
01009                                                                   GA1JPGM 
01010      GO TO 2090-FILL-THE-SCREEN.                                  GA1JPGM 
01011                                                                   GA1JPGM 
01012  2090-PAGE-TO-BOTTOM.                                             GA1JPGM 
01013                                                                   GA1JPGM 
01014      COMPUTE WS-GAG-INDEX = GAG-ENTRY-COUNT                       GA1JPGM 
01015                           - (WS-MAP-ROW * WS-MAP-COL).            GA1JPGM 
01016                                                                   GA1JPGM 
01017      IF  WS-GAG-INDEX < +0                                        GA1JPGM 
01018      THEN                                                         GA1JPGM 
01019          MOVE GAG-PROCEDURE-ARGUMENT(1)                           GA1JPGM 
01020            TO WS-SAVED-PROCED-ARGUMENT                            GA1JPGM 
01021      ELSE                                                         GA1JPGM 
01022          SET  GAG-INDEX TO WS-GAG-INDEX                           GA1JPGM 
01023          MOVE GAG-PROCEDURE-ARGUMENT(GAG-INDEX)                   GA1JPGM 
01024                         TO WS-SAVED-PROCED-ARGUMENT.              GA1JPGM 
01025                                                                   GA1JPGM 
01026      GO TO 2090-FILL-THE-SCREEN.                                  GA1JPGM 
01027                                                                   GA1JPGM 
01028  2090-PAGE-TO-TOP.                                                GA1JPGM 
01029                                                                   GA1JPGM 
01030      SET  GAG-INDEX TO 1.                                         GA1JPGM 
01031      MOVE GAG-PROCEDURE-ARGUMENT(GAG-INDEX)                       GA1JPGM 
01032        TO WS-SAVED-PROCED-ARGUMENT.                               GA1JPGM 
01033                                                                   GA1JPGM 
01034      GO TO 2090-FILL-THE-SCREEN.                                  GA1JPGM 
01035                                                                   GA1JPGM 
01036  2090-FILL-THE-SCREEN.                                            GA1JPGM 
01037                                                                   GA1JPGM 
01038      PERFORM 4500-FILL-THE-SCREEN.                                GA1JPGM 
01039      EXEC CICS SEND   MAP   ('GA1JI01')                           GA1JPGM 
01040                       MAPSET('GA1JSET')                           GA1JPGM 
01041                       ERASE                                       GA1JPGM 
01042                       FROM  (GA1JI01O)                            GA1JPGM 
01043                       END-EXEC.                                   GA1JPGM 
01044                                                                   GA1JPGM 
01045                                                                   GA1JPGM 
01046  2099-EXIT.   EXIT.                                               GA1JPGM 
01047      EJECT                                                        GA1JPGM 
01048  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA1JPGM 
01049 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01050 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
01051 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01052      MOVE LOW-VALUES  TO                                          GA1JPGM 
01053         MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),                     GA1JPGM 
01054         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1JPGM 
01055         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1JPGM 
01056 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01057                                                                   GA1JPGM 
01058  2199-EXIT.   EXIT.                                               GA1JPGM 
01059      EJECT                                                        GA1JPGM 
01060 ******************************************************************GA1JPGM 
01061 **          X C T L   T O   A D D   S C R E E N                   GA1JPGM 
01062 **                                                                GA1JPGM 
01063 **  THE OPERATOR WANTS TO SWITCH MODES, FROM DELETING ENTRIES TO  GA1JPGM 
01064 ** ADDING ENTRIES.  WE READ THE ALL LEVEL TABULAR & PASS THE      GA1JPGM 
01065 ** ADDRESS OF THE I/O PARMS, WORKFILE KEY, AND ALL LEVEL TABULAR  GA1JPGM 
01066 ** RECORD TO THE ADD PROGRAM.  (DEPENDING ON THE MENU THE PROGRAM GA1JPGM 
01067 ** ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE            GA1JPGM 
01068 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA1JPGM 
01069 ******************************************************************GA1JPGM 
01070  3000-XCTL-TO-ADD-SCREEN SECTION.                                 GA1JPGM 
01071      MOVE '3000'  TO  WS-PARA-ID.                                 GA1JPGM 
01072                                                                   GA1JPGM 
01073 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA1JPGM 
01074 **   EXEC CICS GETMAIN                                            GA1JPGM 
01075 **      SET(ADDRESS OF GCA-COMMAREA)                              GA1JPGM 
01076 **      INITIMG(WS-HEX-00)                                        GA1JPGM 
01077 **      LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA1JPGM 
01078 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1JPGM 
01079                                                                   GA1JPGM 
01080 **   IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1JPGM 
01081 **      MOVE MAP-ID-LINE  TO GROUP-SPECIFIC-ID-LINE               GA1JPGM 
01082 **      MOVE GRP-SPEC-GROUP-NO  TO  GCA-GRP-NO                    GA1JPGM 
01083 **      MOVE GRP-SPEC-SECTION-NO  TO  GCA-SECTN-NO                GA1JPGM 
01084 **      MOVE GRP-SPEC-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1JPGM 
01085 **      MOVE GRP-SPEC-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1JPGM 
01086 **      MOVE SPACES  TO  GCA-L-O-B,                               GA1JPGM 
01087 **                       GCA-PROV-CTL,                            GA1JPGM 
01088 **                       GCA-BEN-PROV-ID.                         GA1JPGM 
01089 **                                                                GA1JPGM 
01090 **   IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA1JPGM 
01091 **      MOVE MAP-ID-LINE  TO CONTRACT-ID-LINE                     GA1JPGM 
01092 **      MOVE CONTRACT-GROUP-NO  TO  GCA-GRP-NO                    GA1JPGM 
01093 **      MOVE CONTRACT-SECTION-NO  TO  GCA-SECTN-NO                GA1JPGM 
01094 **      MOVE CONTRACT-LOB  TO  GCA-L-O-B                          GA1JPGM 
01095 **      MOVE CONTRACT-PROV-CTL  TO  GCA-PROV-CTL                  GA1JPGM 
01096 **      MOVE CONTRACT-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1JPGM 
01097 **      MOVE CONTRACT-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1JPGM 
01098 **      MOVE SPACES  TO  GCA-BEN-PROV-ID.                         GA1JPGM 
01099                                                                   GA1JPGM 
01100 **   IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1JPGM 
01101 **      MOVE MAP-ID-LINE  TO BENEFIT-PROVISION-ID-LINE            GA1JPGM 
01102 **      MOVE BEN-PROV-GROUP-NO  TO  GCA-GRP-NO                    GA1JPGM 
01103 **      MOVE BEN-PROV-SECTION-NO  TO  GCA-SECTN-NO                GA1JPGM 
01104 **      MOVE BEN-PROV-LOB  TO  GCA-L-O-B                          GA1JPGM 
01105 **      MOVE BEN-PROV-PROV-CTL  TO  GCA-PROV-CTL                  GA1JPGM 
01106 **      MOVE BEN-PROV-FAM-REL-LVL  TO  GCA-FAM-REL-LVL            GA1JPGM 
01107 **      MOVE BEN-PROV-EFF-DATE  TO  GCA-EFFECTIVE-DATE            GA1JPGM 
01108 **      MOVE BEN-PROV-ID-NO  TO  GCA-BEN-PROV-ID.                 GA1JPGM 
01109                                                                   GA1JPGM 
01110 **   MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID.          GA1JPGM 
01111 **   MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT.      GA1JPGM 
01112 **   MOVE SPACES  TO  GCA-ALL-LEVEL-TAB-FUNC-CODE,                GA1JPGM 
01113 **                    GCA-INTERNAL-TAB-ID,                        GA1JPGM 
01114 **                    GCA-INTERNAL-TAB-SLOT,                      GA1JPGM 
01115 **                    GCA-OCCURS-ENTRY-COUNTER,                   GA1JPGM 
01116 **                    GCA-ADD-DEL-IND.                            GA1JPGM 
01117 **   MOVE MAP-FROM-MENU-ID  TO GCA-FROM-MENU-ID.                  GA1JPGM 
01118 **   MOVE ZEROES  TO  GCA-EFF-DT.                                 GA1JPGM 
01119                                                                   GA1JPGM 
01120 **   SET COMMAREA-PNTR TO ADDRESS                                 GA1JPGM 
01121 **   OF  GCA-COMMAREA.                                            GA1JPGM 
01122                                                                   GA1JPGM 
01123 **   EXEC CICS XCTL  PROGRAM('GA2JPGM') COMMAREA(COMMAREA-PNTR)   GA1JPGM 
01124 ***     LENGTH(4) END-EXEC.                                       GA1JPGM 
01125      EXEC CICS XCTL  PROGRAM('GA2JPGM')                           GA1JPGM 
01126                      COMMAREA(DFHCOMMAREA)                        GA1JPGM 
01127                      LENGTH (LENGTH OF DFHCOMMAREA)               GA1JPGM 
01128      END-EXEC.                                                    GA1JPGM 
01129                                                                   GA1JPGM 
01130  3099-EXIT.   EXIT.                                               GA1JPGM 
01131      EJECT                                                        GA1JPGM 
01132 ***************************************************************** GA1JPGM 
01133 **          D I S P L A Y   F I R S T   S C R E E N               GA1JPGM 
01134 **                                                                GA1JPGM 
01135 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE MENU ORGA1JPGM 
01136 ** THE ADD PROGRAM, EITHER OF THOSE TWO PROGRAMS WILL READ THE    GA1JPGM 
01137 ** ALL LEVEL TABULAR RECORD & PASS US THE RECORD (PRECEEDED BY I/OGA1JPGM 
01138 ** PARMS AND WORKFILE KEY).  WE WILL THEN USE THAT RECORD TO BUILDGA1JPGM 
01139 ** THE SCREEN IMAGE.                                              GA1JPGM 
01140 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA1JPGM 
01141 ** (DEPENDING UPON THE TYPE OF SCREEN THE PROGRAM ORIGINATED FROM)GA1JPGM 
01142 ** SAVES THE FIRST ENTRY TO BE DISPLAYED, PERFORMS THE ROUTINE    GA1JPGM 
01143 ** WHICH USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO BE       GA1JPGM 
01144 ** DISPLAYED, THEN FILLS THE SCREEN WITH ALL SUCCEEDING ENTRIES,  GA1JPGM 
01145 ** AND FINALLY SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR   GA1JPGM 
01146 ** DETERMINATION OF APPROPRIATE ACTION.                           GA1JPGM 
01147 ******************************************************************GA1JPGM 
01148  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA1JPGM 
01149      MOVE '4000'  TO  WS-PARA-ID.                                 GA1JPGM 
01150                                                                   GA1JPGM 
01151 ***  MOVE LOW-VALUES TO SCREEN                                    GA1JPGM 
01152      MOVE LOW-VALUES TO GA1JI01I.                                 GA1JPGM 
01153                                                                   GA1JPGM 
01154      IF  EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                   GA1JPGM 
01155         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA1JPGM 
01156            TO MAP-ERROR-MESSAGE                                   GA1JPGM 
01157         MOVE '1JC1'  TO  WS-ABEND-CODE                            GA1JPGM 
01158         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1JPGM 
01159                                                                   GA1JPGM 
01160 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA1JPGM 
01161 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA1JPGM 
01162 ***  MOVE GCA-RECORD-POINTER  TO  ALL-LEVEL-TAB-PNTR.             GA1JPGM 
01163 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA1JPGM 
01164 ***  ADD ALL-LEVEL-TAB-PNTR,  4096  GIVING  ALL-LEVEL-TAB-PNTR2.  GA1JPGM 
01165                                                                   GA1JPGM 
01166 **   SET ADDRESS OF GCA-COMMAREA                                  GA1JPGM 
01167 **   TO  INCOMING-COMMAREA-PNTR.                                  GA1JPGM 
01168                                                                   GA1JPGM 
01169      SET ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD                    GA1JPGM 
01170      TO  GCA-RECORD-POINTER.                                      GA1JPGM 
01171                                                                   GA1JPGM 
01172      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-ALL-LEVEL-TAB-ID.         GA1JPGM 
01173      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-ALL-LEVEL-TAB-SLOT.     GA1JPGM 
01174      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA1JPGM 
01175 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01176 **  IF THE TABULAR RECORD HAS AN INCLUDE/EXCLUDE FIELD,           GA1JPGM 
01177 **  ITS MOVE TO THE MAP SHOULD BE HERE.                           GA1JPGM 
01178 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01179                                                                   GA1JPGM 
01180      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA1JPGM 
01181         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-TITLE-LINE        GA1JPGM 
01182         MOVE 'GROUP SPECIFIC ID = '  TO GRP-SPEC-ID-HEADING       GA1JPGM 
01183         MOVE 'GRP= '  TO  GRP-SPEC-GROUP-HEADING                  GA1JPGM 
01184         MOVE GCA-GRP-NO  TO  GRP-SPEC-GROUP-NO                    GA1JPGM 
01185         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA1JPGM 
01186         MOVE GCA-SECTN-NO  TO  GRP-SPEC-SECTION-NO                GA1JPGM 
01187         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA1JPGM 
01188         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA1JPGM 
01189         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA1JPGM 
01190         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA1JPGM 
01191                                                                   GA1JPGM 
01192      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA1JPGM 
01193         MOVE CONTRACT-TITLE-LINE  TO  MAP-TITLE-LINE              GA1JPGM 
01194         MOVE 'CONTRACT ID = '  TO  CONTRACT-ID-HEADING            GA1JPGM 
01195         MOVE 'GRP= '  TO  CONTRACT-GROUP-HEADING                  GA1JPGM 
01196         MOVE GCA-GRP-NO  TO  CONTRACT-GROUP-NO                    GA1JPGM 
01197         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA1JPGM 
01198         MOVE GCA-SECTN-NO  TO  CONTRACT-SECTION-NO                GA1JPGM 
01199         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA1JPGM 
01200         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA1JPGM 
01201         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA1JPGM 
01202         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA1JPGM 
01203         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA1JPGM 
01204         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA1JPGM 
01205         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA1JPGM 
01206         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA1JPGM 
01207                                                                   GA1JPGM 
01208      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA1JPGM 
01209         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-TITLE-LINE     GA1JPGM 
01210         MOVE 'GRP= '  TO  BEN-PROV-GROUP-HEADING                  GA1JPGM 
01211         MOVE GCA-GRP-NO  TO  BEN-PROV-GROUP-NO                    GA1JPGM 
01212         MOVE ' SEC= '  TO  BEN-PROV-SECTION-HEADING               GA1JPGM 
01213         MOVE GCA-SECTN-NO  TO  BEN-PROV-SECTION-NO                GA1JPGM 
01214         MOVE ' LOB= '  TO  BEN-PROV-LOB-HEADING                   GA1JPGM 
01215         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA1JPGM 
01216         MOVE ' PRV= '  TO  BEN-PROV-PROV-CTL-HEADING              GA1JPGM 
01217         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA1JPGM 
01218         MOVE ' FR= '  TO  BEN-PROV-FAM-REL-HEADING                GA1JPGM 
01219         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA1JPGM 
01220         MOVE ' EFDT= '  TO  BEN-PROV-EFF-DT-HEADING               GA1JPGM 
01221         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA1JPGM 
01222         MOVE ' BPVID= '  TO  BEN-PROV-ID-HEADING                  GA1JPGM 
01223         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA1JPGM 
01224                                                                   GA1JPGM 
01225      SET GAG-INDEX  TO  1.                                        GA1JPGM 
01226      MOVE GAG-ENTRY (GAG-INDEX)  TO  WS-SAVED-FIELDS.             GA1JPGM 
01227                                                                   GA1JPGM 
01228      PERFORM 4500-FILL-THE-SCREEN.                                GA1JPGM 
01229      EXEC CICS SEND   MAP('GA1JI01') MAPSET('GA1JSET') ERASE      GA1JPGM 
01230         FROM(GA1JI01O) END-EXEC.                                  GA1JPGM 
01231                                                                   GA1JPGM 
01232  4099-EXIT.   EXIT.                                               GA1JPGM 
01233      EJECT                                                        GA1JPGM 
01234 ***************************************************************** GA1JPGM 
01235 **             F I L L   T H E   S C R E E N                      GA1JPGM 
01236 **                                                                GA1JPGM 
01237 **   THIS SECTION USES THE SAVED ENTRY TO FIND THE FIRST ENTRY TO GA1JPGM 
01238 ** BE DISPLAYED THEN MOVES ALL THE FOLLOWING ENTRIES THAT WILL FITGA1JPGM 
01239 ** ON THE SCREEN.  IF THE SCREEN HAS EXTRA ENTRIES THE ACTION CODEGA1JPGM 
01240 ** FOR THOSE ENTRIES WILL HAVE ITS ATTRIBUTE SET TO AUTO-SKIP SO  GA1JPGM 
01241 ** THE OPERATOR CANNOT ERRONEOUSLY MARK THIS ENTRY FOR DELETION.  GA1JPGM 
01242 ******************************************************************GA1JPGM 
01243  4500-FILL-THE-SCREEN SECTION.                                    GA1JPGM 
01244                                                                   GA1JPGM 
01245      MOVE '4500'  TO  WS-PARA-ID.                                 GA1JPGM 
01246      MOVE  GAG-ENTRY-COUNT  TO  GAG-ENTRY-COUNT.                  GA1JPGM 
01247      SET MAP-IDX1,  MAP-IDX2  TO  1.                              GA1JPGM 
01248                                                                   GA1JPGM 
01249      COMPUTE WS-SELECT-OF = GAG-ENTRY-COUNT - 1.                  GA1JPGM 
01250      MOVE WS-SELECT-OF      TO WS-SELECT-OF-MASK.                 GA1JPGM 
01251      MOVE WS-SELECT-OF-MASK TO MAP-SELECT-FROM                    GA1JPGM 
01252                                MAP-SELECT-TO                      GA1JPGM 
01253                                MAP-SELECT-OF.                     GA1JPGM 
01254                                                                   GA1JPGM 
01255      IF GAG-ENTRY-COUNT  NOT >  1                                 GA1JPGM 
01256         MOVE '4530'  TO  WS-PARA-ID                               GA1JPGM 
01257         GO TO 4530-FILL-REST-WITH-NULLS.                          GA1JPGM 
01258                                                                   GA1JPGM 
01259      SET GAG-INDEX  TO  1.                                        GA1JPGM 
01260      MOVE '4510'  TO  WS-PARA-ID.                                 GA1JPGM 
01261  4510-FIND-1ST-ENTRY-TO-DISPLAY.                                  GA1JPGM 
01262 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01263 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
01264 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01265      IF GAG-PROCEDURE-ARGUMENT (GAG-INDEX)  <                     GA1JPGM 
01266         WS-SAVED-PROCED-ARGUMENT  OR                              GA1JPGM 
01267            GAG-COMBINATION-CODE-FUNCTION (GAG-INDEX)  <           GA1JPGM 
01268               WS-SAVED-CODE-FUNCTION                              GA1JPGM 
01269 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01270         SET GAG-INDEX  UP BY  1                                   GA1JPGM 
01271         IF  GAG-INDEX  <  GAG-ENTRY-COUNT                         GA1JPGM 
01272            GO TO 4510-FIND-1ST-ENTRY-TO-DISPLAY                   GA1JPGM 
01273         ELSE                                                      GA1JPGM 
01274            SET GAG-INDEX  TO  1.                                  GA1JPGM 
01275                                                                   GA1JPGM 
01276      SET  WS-SELECT-FROM       TO GAG-INDEX.                      GA1JPGM 
01277      MOVE WS-SELECT-FROM       TO WS-SELECT-FROM-MASK.            GA1JPGM 
01278      MOVE WS-SELECT-FROM-MASK  TO MAP-SELECT-FROM.                GA1JPGM 
01279                                                                   GA1JPGM 
01280      MOVE '4520'  TO  WS-PARA-ID.                                 GA1JPGM 
01281  4520-DISPLAY-ENTRIES-TO-DELETE.                                  GA1JPGM 
01282      MOVE DFHBMUNF  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1JPGM 
01283      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2).   GA1JPGM 
01284 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01285 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
01286 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01287      MOVE GAG-PROCEDURE-ARGUMENT (GAG-INDEX) TO                   GA1JPGM 
01288         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2).              GA1JPGM 
01289      MOVE GAG-COMBINATION-CODE-FUNCTION (GAG-INDEX) TO            GA1JPGM 
01290         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1JPGM 
01291 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01292                                                                   GA1JPGM 
01293      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1JPGM 
01294         SET  MAP-IDX1  UP BY  1                                   GA1JPGM 
01295      ELSE                                                         GA1JPGM 
01296         IF MAP-IDX2  <  WS-MAP-COL                                GA1JPGM 
01297            SET  MAP-IDX1  TO  1                                   GA1JPGM 
01298            SET  MAP-IDX2  UP BY  1                                GA1JPGM 
01299         ELSE                                                      GA1JPGM 
01300            SET  WS-SELECT-TO         TO GAG-INDEX                 GA1JPGM 
01301            MOVE WS-SELECT-TO         TO WS-SELECT-TO-MASK         GA1JPGM 
01302            MOVE WS-SELECT-TO-MASK    TO MAP-SELECT-TO             GA1JPGM 
01303            GO TO 4540-DETERMINE-MSG-TO-DISPLAY.                   GA1JPGM 
01304                                                                   GA1JPGM 
01305      IF GAG-INDEX  <  (GAG-ENTRY-COUNT - 1 )                      GA1JPGM 
01306         SET  GAG-INDEX  UP BY  1                                  GA1JPGM 
01307         GO TO  4520-DISPLAY-ENTRIES-TO-DELETE.                    GA1JPGM 
01308                                                                   GA1JPGM 
01309      SET  WS-SELECT-TO         TO GAG-INDEX.                      GA1JPGM 
01310      MOVE WS-SELECT-TO         TO WS-SELECT-TO-MASK.              GA1JPGM 
01311      MOVE WS-SELECT-TO-MASK    TO MAP-SELECT-TO.                  GA1JPGM 
01312                                                                   GA1JPGM 
01313      MOVE '4530'  TO  WS-PARA-ID.                                 GA1JPGM 
01314  4530-FILL-REST-WITH-NULLS.                                       GA1JPGM 
01315      MOVE DFHBMASK  TO  MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2).GA1JPGM 
01316 **+**+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01317 **  THIS AREA MUST BE CHANGED TO MATCH TABULAR RECORD.            GA1JPGM 
01318 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01319      MOVE LOW-VALUES  TO  MAP-ACTION-CODE (MAP-IDX1, MAP-IDX2),   GA1JPGM 
01320         MAP-PROCEDURE-ARGUMENT (MAP-IDX1, MAP-IDX2),              GA1JPGM 
01321         MAP-CODE-FUNCTION (MAP-IDX1, MAP-IDX2).                   GA1JPGM 
01322 **++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  GA1JPGM 
01323      IF MAP-IDX1   <  WS-MAP-ROW                                  GA1JPGM 
01324         SET  MAP-IDX1  UP BY  1                                   GA1JPGM 
01325         GO TO 4530-FILL-REST-WITH-NULLS                           GA1JPGM 
01326      ELSE                                                         GA1JPGM 
01327         IF MAP-IDX2  <  WS-MAP-COL                                GA1JPGM 
01328            SET  MAP-IDX1  TO  1                                   GA1JPGM 
01329            SET  MAP-IDX2  UP BY  1                                GA1JPGM 
01330            GO TO 4530-FILL-REST-WITH-NULLS.                       GA1JPGM 
01331                                                                   GA1JPGM 
01332      MOVE '4540'  TO  WS-PARA-ID.                                 GA1JPGM 
01333  4540-DETERMINE-MSG-TO-DISPLAY.                                   GA1JPGM 
01334      IF GAG-ENTRY-COUNT  =  1                                     GA1JPGM 
01335         MOVE '*** NO ENTRIES TO DELETE ***'  TO  MAP-ERROR-MESSAGEGA1JPGM 
01336         GO TO 4599-EXIT.                                          GA1JPGM 
01337                                                                   GA1JPGM 
01338      IF MAP-ACTION-CODE-ATTR (MAP-IDX1, MAP-IDX2)  =  DFHBMASK    GA1JPGM 
01339         MOVE '*** NO MORE ENTRIES TO DISPLAY ***'                 GA1JPGM 
01340           TO  MAP-ERROR-MESSAGE.                                  GA1JPGM 
01341                                                                   GA1JPGM 
01342  4599-EXIT.     EXIT.                                             GA1JPGM 
01343      EJECT                                                        GA1JPGM 
01344 ***************************************************************** GA1JPGM 
01345 **        X C T L   T O   P R E V I O U S   M E N U               GA1JPGM 
01346 **                                                                GA1JPGM 
01347 **  THE OPERATOR WANTS TO RETURN TO THE MENU THIS PROGRAM         GA1JPGM 
01348 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD AND     GA1JPGM 
01349 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA1JPGM 
01350 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM MENU  GA1JPGM 
01351 ** ID' FIELD).                                                    GA1JPGM 
01352 ******************************************************************GA1JPGM 
01353  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA1JPGM 
01354      MOVE '5000'  TO  WS-PARA-ID.                                 GA1JPGM 
01355                                                                   GA1JPGM 
01356      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA1JPGM 
01357         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA1JPGM 
01358                                                                   GA1JPGM 
01359      IF  MAP-FROM-MENU-ID  = 'GC4A'                               GA1JPGM 
01360         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA1JPGM 
01361                                                                   GA1JPGM 
01362      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA1JPGM 
01363         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA1JPGM 
01364                                                                   GA1JPGM 
01365      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA1JPGM 
01366         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA1JPGM 
01367                                                                   GA1JPGM 
01368                                                                   GA1JPGM 
01369  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA1JPGM 
01370      MOVE '5010'  TO  WS-PARA-ID.                                 GA1JPGM 
01371                                                                   GA1JPGM 
01372      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1JPGM 
01373          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1JPGM 
01374                 GC-GCGRPSPC-FIXED-LEN      +                      GA1JPGM 
01375         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1JPGM 
01376                                                                   GA1JPGM 
01377 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA1JPGM 
01378      EXEC CICS GETMAIN                                            GA1JPGM 
01379         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA1JPGM 
01380         INITIMG(WS-HEX-00)                                        GA1JPGM 
01381         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01382 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA1JPGM 
01383                                                                   GA1JPGM 
01384      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1JPGM 
01385                                                                   GA1JPGM 
01386      MOVE 'G'                  TO GCIO-WRK-STATUS-CODE.           GA1JPGM 
01387      MOVE 'G2'                 TO GCIO-WRK-RECORD-TYPE.           GA1JPGM 
01388      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1JPGM 
01389      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1JPGM 
01390      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA1JPGM 
01391      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1JPGM 
01392      MOVE SPACES               TO GCIO-WRK-LINE-OF-BUS,           GA1JPGM 
01393                                   GCIO-WRK-PROVIDER-CONTROL.      GA1JPGM 
01394      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1JPGM 
01395      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1JPGM 
01396                                                                   GA1JPGM 
01397 **   MOVE  WS-Y  TO  WS-YY.                                       GA1JPGM 
01398 **   IF WS-M  >  2                                                GA1JPGM 
01399 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1JPGM 
01400 **         REMAINDER  WS-REMAINDER                                GA1JPGM 
01401 **   ELSE                                                         GA1JPGM 
01402 **      MOVE 1  TO  WS-REMAINDER.                                 GA1JPGM 
01403 **   SET WS-M-IDX  TO  WS-M.                                      GA1JPGM 
01404 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1JPGM 
01405 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1JPGM 
01406 **   IF WS-REMAINDER  =  ZERO                                     GA1JPGM 
01407 **      ADD 1  TO  WS-DDD.                                        GA1JPGM 
01408 **                                                                GA1JPGM 
01409 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1JPGM 
01410 **                                                                GA1JPGM 
01411      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA1JPGM 
01412      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1JPGM 
01413                       GCIO-WRK-TAB-PROVISION-ID.                  GA1JPGM 
01414      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1JPGM 
01415                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1JPGM 
01416      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA1JPGM 
01417                                                                   GA1JPGM 
01418      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA1JPGM 
01419      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA1JPGM 
01420                                                                   GA1JPGM 
01421      MOVE  'RD '  TO  GCIO2-FILE-ACCESS-CODE.                     GA1JPGM 
01422                                                                   GA1JPGM 
01423      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1JPGM 
01424         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA1JPGM 
01425         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01426                                                                   GA1JPGM 
01427      IF  NOT GCIO2-GOOD-RETURN                                    GA1JPGM 
01428         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1JPGM 
01429 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1JPGM 
01430         MOVE '1JF4'  TO  WS-ABEND-CODE                            GA1JPGM 
01431         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1JPGM 
01432                                                                   GA1JPGM 
01433      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1JPGM 
01434          GC-WORKFILE-KEY-LEN        +                             GA1JPGM 
01435                 GC-GCGRPSPC-FIXED-LEN      +                      GA1JPGM 
01436         (GC-GCGRPSPC-VARY-LEN    *  GC-GCGRPSPC-VARY-MAX-OCUR).   GA1JPGM 
01437                                                                   GA1JPGM 
01438      EXEC CICS XCTL  PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)   GA1JPGM 
01439         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01440                                                                   GA1JPGM 
01441      GO  TO  5099-EXIT.                                           GA1JPGM 
01442                                                                   GA1JPGM 
01443  5020-XCTL-TO-CONTRACT-MENU.                                      GA1JPGM 
01444      MOVE '5020'  TO  WS-PARA-ID.                                 GA1JPGM 
01445                                                                   GA1JPGM 
01446      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1JPGM 
01447          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1JPGM 
01448                 GC-GCCONTR-FIXED-LEN       +                      GA1JPGM 
01449         (GC-GCCONTR-VARY-LEN     *  GC-GCCONTR-VARY-MAX-OCUR).    GA1JPGM 
01450                                                                   GA1JPGM 
01451 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA1JPGM 
01452      EXEC CICS GETMAIN                                            GA1JPGM 
01453         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA1JPGM 
01454         INITIMG(WS-HEX-00)                                        GA1JPGM 
01455         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01456 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA1JPGM 
01457 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA1JPGM 
01458                                                                   GA1JPGM 
01459      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1JPGM 
01460                                                                   GA1JPGM 
01461      MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           GA1JPGM 
01462      MOVE 'C2'                 TO GCIO-WRK-RECORD-TYPE.           GA1JPGM 
01463      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1JPGM 
01464      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1JPGM 
01465      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA1JPGM 
01466      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1JPGM 
01467      MOVE CONTRACT-LOB         TO GCIO-WRK-LINE-OF-BUS.           GA1JPGM 
01468      MOVE CONTRACT-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL.      GA1JPGM 
01469      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1JPGM 
01470      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1JPGM 
01471                                                                   GA1JPGM 
01472 **   MOVE  WS-Y  TO  WS-YY.                                       GA1JPGM 
01473 **   IF WS-M  >  2                                                GA1JPGM 
01474 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1JPGM 
01475 **         REMAINDER  WS-REMAINDER                                GA1JPGM 
01476 **   ELSE                                                         GA1JPGM 
01477 **      MOVE 1  TO  WS-REMAINDER.                                 GA1JPGM 
01478 **   SET WS-M-IDX  TO  WS-M.                                      GA1JPGM 
01479 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1JPGM 
01480 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1JPGM 
01481 **   IF WS-REMAINDER  =  ZERO                                     GA1JPGM 
01482 **      ADD 1  TO  WS-DDD.                                        GA1JPGM 
01483 **                                                                GA1JPGM 
01484 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1JPGM 
01485      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA1JPGM 
01486      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA1JPGM 
01487                       GCIO-WRK-TAB-PROVISION-ID.                  GA1JPGM 
01488      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA1JPGM 
01489                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1JPGM 
01490      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA1JPGM 
01491                                                                   GA1JPGM 
01492      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA1JPGM 
01493      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA1JPGM 
01494                                                                   GA1JPGM 
01495      MOVE  'RD '  TO  GCIO3-FILE-ACCESS-CODE.                     GA1JPGM 
01496                                                                   GA1JPGM 
01497      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1JPGM 
01498         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA1JPGM 
01499         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01500                                                                   GA1JPGM 
01501      IF  NOT GCIO3-GOOD-RETURN                                    GA1JPGM 
01502         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1JPGM 
01503 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1JPGM 
01504         MOVE '1JF5'  TO  WS-ABEND-CODE                            GA1JPGM 
01505         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1JPGM 
01506                                                                   GA1JPGM 
01507      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1JPGM 
01508          GC-WORKFILE-KEY-LEN        +                             GA1JPGM 
01509                 GC-GCCONTR-FIXED-LEN       +                      GA1JPGM 
01510         (GC-GCCONTR-VARY-LEN     *  GC-GCCONTR-VARY-MAX-OCUR).    GA1JPGM 
01511                                                                   GA1JPGM 
01512      EXEC CICS XCTL  PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)   GA1JPGM 
01513         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01514                                                                   GA1JPGM 
01515      GO  TO  5099-EXIT.                                           GA1JPGM 
01516                                                                   GA1JPGM 
01517  5030-XCTL-TO-BEN-PROV-MENU.                                      GA1JPGM 
01518      MOVE '5030'  TO  WS-PARA-ID.                                 GA1JPGM 
01519                                                                   GA1JPGM 
01520      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1JPGM 
01521          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA1JPGM 
01522                 GC-GCBENPRV-FIXED-LEN      +                      GA1JPGM 
01523         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1JPGM 
01524                                                                   GA1JPGM 
01525 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA1JPGM 
01526      EXEC CICS GETMAIN                                            GA1JPGM 
01527         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA1JPGM 
01528         INITIMG(WS-HEX-00)                                        GA1JPGM 
01529         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01530 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA1JPGM 
01531                                                                   GA1JPGM 
01532      MOVE SPACES               TO GCIO-WORKFILE-KEY.              GA1JPGM 
01533                                                                   GA1JPGM 
01534      MOVE 'C'                  TO GCIO-WRK-STATUS-CODE.           GA1JPGM 
01535      MOVE 'C4'                 TO GCIO-WRK-RECORD-TYPE.           GA1JPGM 
01536      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA1JPGM 
01537      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA1JPGM 
01538      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM            GA1JPGM 
01539      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA1JPGM 
01540      MOVE BEN-PROV-LOB         TO GCIO-WRK-LINE-OF-BUS.           GA1JPGM 
01541      MOVE BEN-PROV-PROV-CTL    TO GCIO-WRK-PROVIDER-CONTROL.      GA1JPGM 
01542      MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA1JPGM 
01543      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA1JPGM 
01544      MOVE BEN-PROV-ID-NO       TO GCIO-WRK-PROVISION-ID.          GA1JPGM 
01545                                                                   GA1JPGM 
01546 **   MOVE  WS-Y  TO  WS-YY.                                       GA1JPGM 
01547 **   IF WS-M  >  2                                                GA1JPGM 
01548 **      DIVIDE  WS-Y  BY  4  GIVING  WS-QUOTIENT                  GA1JPGM 
01549 **         REMAINDER  WS-REMAINDER                                GA1JPGM 
01550 **   ELSE                                                         GA1JPGM 
01551 **      MOVE 1  TO  WS-REMAINDER.                                 GA1JPGM 
01552 **   SET WS-M-IDX  TO  WS-M.                                      GA1JPGM 
01553 **   MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA1JPGM 
01554 **   COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA1JPGM 
01555 **   IF WS-REMAINDER  =  ZERO                                     GA1JPGM 
01556 **      ADD 1  TO  WS-DDD.                                        GA1JPGM 
01557 **                                                                GA1JPGM 
01558 **   MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA1JPGM 
01559      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA1JPGM 
01560      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA1JPGM 
01561      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA1JPGM 
01562      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA1JPGM 
01563      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA1JPGM 
01564                                                                   GA1JPGM 
01565      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA1JPGM 
01566      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA1JPGM 
01567                                                                   GA1JPGM 
01568      MOVE  'RD '  TO  GCIO4-FILE-ACCESS-CODE.                     GA1JPGM 
01569                                                                   GA1JPGM 
01570      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA1JPGM 
01571         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA1JPGM 
01572         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01573                                                                   GA1JPGM 
01574      IF  NOT GCIO4-GOOD-RETURN                                    GA1JPGM 
01575         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA1JPGM 
01576 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA1JPGM 
01577         MOVE '1JF6'  TO  WS-ABEND-CODE                            GA1JPGM 
01578         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA1JPGM 
01579                                                                   GA1JPGM 
01580      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA1JPGM 
01581          GC-WORKFILE-KEY-LEN        +                             GA1JPGM 
01582                 GC-GCBENPRV-FIXED-LEN      +                      GA1JPGM 
01583         (GC-GCBENPRV-VARY-LEN    *  GC-GCBENPRV-VARY-MAX-OCUR).   GA1JPGM 
01584                                                                   GA1JPGM 
01585      EXEC CICS XCTL  PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)   GA1JPGM 
01586         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA1JPGM 
01587                                                                   GA1JPGM 
01588      GO  TO  5099-EXIT.                                           GA1JPGM 
01589                                                                   GA1JPGM 
01590                                                                   GA1JPGM 
01591                                                                   GA1JPGM 
01592  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA1JPGM 
01593      MOVE '5040'  TO  WS-PARA-ID.                                 GA1JPGM 
01594                                                                   GA1JPGM 
01595      EXEC CICS XCTL                                               GA1JPGM 
01596                PROGRAM('GTM1PGM')                                 GA1JPGM 
01597                END-EXEC.                                          GA1JPGM 
01598                                                                   GA1JPGM 
01599                                                                   GA1JPGM 
01600      GO  TO  5099-EXIT.                                           GA1JPGM 
01601                                                                   GA1JPGM 
01602  5099-EXIT.                                                       GA1JPGM 
01603      EXIT.                                                        GA1JPGM 
01604      EJECT                                                        GA1JPGM 
01605 ***************************************************************** GA1JPGM 
01606 **           X C T L   T O   M A I N   M E N U                    GA1JPGM 
01607 **                                                                GA1JPGM 
01608 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA1JPGM 
01609 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA1JPGM 
01610 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA1JPGM 
01611 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOME OF AGA1JPGM 
01612 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA1JPGM 
01613 ** AND PROGRESS DOWN.                                             GA1JPGM 
01614 ******************************************************************GA1JPGM 
01615  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA1JPGM 
01616      MOVE '6000'  TO  WS-PARA-ID.                                 GA1JPGM 
01617      MOVE '1JP1'  TO  WS-ABEND-CODE.                              GA1JPGM 
01618                                                                   GA1JPGM 
01619      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA1JPGM 
01620                                                                   GA1JPGM 
01621  6099-EXIT.     EXIT.                                             GA1JPGM 
01622      EJECT                                                        GA1JPGM 
01623 ***************************************************************** GA1JPGM 
01624 **              P R I N T   H A R D C O P Y                       GA1JPGM 
01625 **                                                                GA1JPGM 
01626 **   THE OPERATOR HAS KEYED THE PF12 OF PF24 KEY INDICATING THEY  GA1JPGM 
01627 ** WANT A HARDCOPY IMAGE OF THE CURRENT SCREEN.  WE LINK TO THE   GA1JPGM 
01628 ** 'CSCRTCPY' PROGRAM WHICH WILL PRINT THE SCREEN, AND RETURN US AGA1JPGM 
01629 ** RETURN CODE INDICATING SUCCESS OR THE TYPE OF ERROR.           GA1JPGM 
01630 ******************************************************************GA1JPGM 
01631 *7000-PRINT-HARDCOPY SECTION.                                     GA1JPGM 
01632 **   MOVE '7000'  TO  WS-PARA-ID.                                 GA1JPGM 
01633 **                                                                GA1JPGM 
01634 **   MOVE 'CSCRTCPY'  TO  PRINT-PROGRAM-ID.                       GA1JPGM 
01635 **   MOVE 'PRN'  TO  PRINT-REQUEST-TYPE.                          GA1JPGM 
01636 **   SET MAP-IDX1, MAP-IDX2  TO  1.                               GA1JPGM 
01637 **   MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1JPGM 
01638 **   EXEC CICS LINK   PROGRAM('CSCRTCPY')                         GA1JPGM 
01639 **      LENGTH(PRINT-COMMAREA-LEN)                                GA1JPGM 
01640 **      COMMAREA(CSCRTCPY-COMMAREA-DEFINITION) END-EXEC.          GA1JPGM 
01641 **                                                                GA1JPGM 
01642 **   IF PRINT-OK                                                  GA1JPGM 
01643 **      MOVE '    *** HARDCOPY REQUEST COMPLETED ***'             GA1JPGM 
01644 **        TO MAP-ERROR-MESSAGE                                    GA1JPGM 
01645 **      GO TO 7010-SEND-SCREEN.                                   GA1JPGM 
01646 **                                                                GA1JPGM 
01647 **   IF PRINT-OUT-OF-SERVICE                                      GA1JPGM 
01648 **      MOVE '                *** OUT OF SERVICE ***'             GA1JPGM 
01649 **        TO MAP-ERROR-MESSAGE                                    GA1JPGM 
01650 **      GO TO 7010-SEND-SCREEN.                                   GA1JPGM 
01651 **                                                                GA1JPGM 
01652 **   IF PRINT-NO-TEMP-STORAGE                                     GA1JPGM 
01653 **      MOVE '               *** NO TEMP STORAGE ***'             GA1JPGM 
01654 **        TO MAP-ERROR-MESSAGE                                    GA1JPGM 
01655 **      GO TO 7010-SEND-SCREEN.                                   GA1JPGM 
01656 **                                                                GA1JPGM 
01657 **   IF PRINT-NO-PRINTER                                          GA1JPGM 
01658 **      MOVE '           *** NO PRINTER ATTACHED ***'             GA1JPGM 
01659 **        TO MAP-ERROR-MESSAGE                                    GA1JPGM 
01660 **      GO TO 7010-SEND-SCREEN.                                   GA1JPGM 
01661 **                                                                GA1JPGM 
01662 **   MOVE '                  *** TS INT CTL ERR ***'              GA1JPGM 
01663 **     TO MAP-ERROR-MESSAGE.                                      GA1JPGM 
01664 **                                                                GA1JPGM 
01665 *7010-SEND-SCREEN.                                                GA1JPGM 
01666 **   MOVE '7010'  TO  WS-PARA-ID.                                 GA1JPGM 
01667 **                                                                GA1JPGM 
01668 **   EXEC CICS SEND   MAP('GA1JI01') MAPSET('GA1JSET') DATAONLY   GA1JPGM 
01669 **      FROM(GA1JI01O) CURSOR END-EXEC.                           GA1JPGM 
01670 **                                                                GA1JPGM 
01671 *7099-EXIT.     EXIT.                                             GA1JPGM 
01672 **   EJECT                                                        GA1JPGM 
01673 ******************************************************************GA1JPGM 
01674  9010-NO-STORAGE SECTION.                                         GA1JPGM 
01675                                                                   GA1JPGM 
01676      MOVE '1JS1'  TO  WS-ABEND-CODE.                              GA1JPGM 
01677      MOVE '*** CICS IS UNABLE TO FIND STORAGE REQUESTED BY THIS PGGA1JPGM 
01678 -    'M, NOTIFY SYSTEMS ***'  TO  MAP-ERROR-MESSAGE.              GA1JPGM 
01679                                                                   GA1JPGM 
01680      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1JPGM 
01681                                                                   GA1JPGM 
01682  9019-EXIT.     EXIT.                                             GA1JPGM 
01683      SKIP3                                                        GA1JPGM 
01684      SKIP3                                                        GA1JPGM 
01685 ******************************************************************GA1JPGM 
01686  9020-PGM-ID-ERROR SECTION.                                       GA1JPGM 
01687                                                                   GA1JPGM 
01688 **     THIS ERROR CAN BE INVOKED BY A NUMBER OF DIFFERENT REQUESTSGA1JPGM 
01689 **     THE PROGRAMMER SHOULD CHECK THE WS-PARA-ID FIELD IN THE    GA1JPGM 
01690 **     DUMP TO DETERMINE WHAT CODE CAUSED THIS ABEND.             GA1JPGM 
01691                                                                   GA1JPGM 
01692      MOVE '1JP2'  TO  WS-ABEND-CODE.                              GA1JPGM 
01693      MOVE '*** CICS CAN T ACCESS A PGM, TABLE, OR MAP FOR THIS PGMGA1JPGM 
01694 -    '. CALL SYSTEMS ***'  TO  MAP-ERROR-MESSAGE.                 GA1JPGM 
01695                                                                   GA1JPGM 
01696      PERFORM 9999-ERROR-MSG-THEN-ABEND.                           GA1JPGM 
01697                                                                   GA1JPGM 
01698  9029-EXIT.     EXIT.                                             GA1JPGM 
01699      SKIP3                                                        GA1JPGM 
01700      SKIP3                                                        GA1JPGM 
01701 ******************************************************************GA1JPGM 
01702  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA1JPGM 
01703                                                                   GA1JPGM 
01704      SET MAP-IDX1  TO  7.                                         GA1JPGM 
01705      SET MAP-IDX2  TO  1.                                         GA1JPGM 
01706      MOVE -1  TO  MAP-ACTION-CODE-LEN (MAP-IDX1, MAP-IDX2).       GA1JPGM 
01707                                                                   GA1JPGM 
01708      EXEC CICS SEND   MAP('GA1JI01') MAPSET('GA1JSET') ERASE      GA1JPGM 
01709         FROM(GA1JI01O) WAIT END-EXEC.                             GA1JPGM 
01710                                                                   GA1JPGM 
01711      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA1JPGM 
01712                                                                   GA1JPGM 
01713  9999-EXIT.     EXIT.                                             GA1JPGM 
