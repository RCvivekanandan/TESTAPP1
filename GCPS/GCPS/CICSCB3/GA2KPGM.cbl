00001 *      LAST MAINTENANCE TIME: 14.43.32  DATE: 01/13/86            09/03/03
00002 *      LAST MAINTENANCE TIME: 11.27.29  DATE: 01/06/86            GA2KPGM 
00003  ID DIVISION.                                                        LV003
00004  PROGRAM-ID.     GA2KPGM.                                         GA2KPGM 
00005 ***  THIS IS A COBOL II PROGRAM                                   GA2KPGM 
00006  AUTHOR.         S BUCH.                                          GA2KPGM 
00007  DATE-WRITTEN.   02/07/85.                                        GA2KPGM 
00008  DATE-COMPILED.                                                   GA2KPGM 
00009      SKIP3                                                        GA2KPGM 
00010 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2KPGM 
00011 *** * * * * * * +-------------------------+ * * * * * * * * * * **GA2KPGM 
00012 *** * * * * * * |   U P D A T E   L O G   | * * * * * * * * * * **GA2KPGM 
00013 *** * * * * * * +-------------------------+ * * * * * * * * * * **GA2KPGM 
00014 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * GA2KPGM 
00015 *   DATE    PROGRAMMER  UPDATES                                 * GA2KPGM 
00016 * --------  ----------  --------------------------------------- * GA2KPGM 
00017 * 01-21-86      LET     CHANGED WS CONTRACT LENGTHS AND REMOVED * GA2KPGM 
00018 *                       HANDLE CONDITIONS EXCEPT FOR MAPAIL.    * GA2KPGM 
00019 *                                                               * GA2KPGM 
00020 * 03-20-86      NJS     ADDED CODE TO CHECK FOR A RETURN CODE   * GA2KPGM 
00021 *                       OF '20' FROM GCVIOPGM.  THIS IMPILES    * GA2KPGM 
00022 *                       THE EDIT TABLE WAS EMPTY AND FIELD VAL- * GA2KPGM 
00023 *                       IDATION COULD NOT BE PERFORMED.  PF4/   * GA2KPGM 
00024 *                       PF16 CAN BE USED TO ACCEPT THE DATA AS  * GA2KPGM 
00025 *                       SHOWN ON THE SCREEN AND CONTINUE        * GA2KPGM 
00026 *                       PROCESSING.                             * GA2KPGM 
00027 *                                                               * GA2KPGM 
00028 * 06/18/86      DES     FIXED PF4/16 CODE TO ACCEPT EMPTY       * GA2KPGM 
00029 *  (EL500)              VALIDATION TABLE CONDITION ONLY,        * GA2KPGM 
00030 *                       ALL OTHER ERRORS STILL MUST BE FIXED    * GA2KPGM 
00031 *                                                               * GA2KPGM 
00032 * 08/27/86      JLA     1. DARKEN SELECT FIELD.                 * GA2KPGM 
00033 *  (D136)               2. DARKEN LOCATION COUNTERS             * GA2KPGM 
00034 *                       3. USE USER DEFINED LOGICAL MAP FOR     * GA2KPGM 
00035 *                          SCREEN.  THIS REPLACES THE PARTIAL   * GA2KPGM 
00036 *                          USE OF BMS MAP AND USER DEFINED.     * GA2KPGM 
00037 *                                                               * GA2KPGM 
00038 * 12/18/86      DES     FIX PROGRAM TO ACCEPT THE ENTRY OF      * GA2KPGM 
00039 *  (D136)               MULTIPLE FIELDS.                        * GA2KPGM 
00040 *                                                               * GA2KPGM 
00041 *  01/30/87     JLA     CHANGES FOR SINGLE TABULAR SUPPORT      * GA2KPGM 
00042 *  (D0120)              EXECUTED FROM TRANSACTION GTM1:         * GA2KPGM 
00043 *                       1. PF1/PF13 - CONSTRUCT COMMAREA AS     * GA2KPGM 
00044 *                          IF GC4A HAD CALLED, XCTL TO ADD      * GA2KPGM 
00045 *                          SCREEN PROGRAM.                      * GA2KPGM 
00046 *                       2. PF3/PF15 - CONSTRUCT COMMAREA AS     * GA2KPGM 
00047 *                          IF GC4A HAD CALLED, XCTL TO          * GA2KPGM 
00048 *                          GTM1PGM.                             * GA2KPGM 
00049 *                                                               * GA2KPGM 
00050 *   8/17/87     FRY     CAPTURE OPERATOR-ID WHEN A 'C3', 'C5',  * GA2KPGM 
00051 *   D0116               OR 'G3' RECORD IS UPDATED.              * GA2KPGM 
00052 *                                                               * GA2KPGM 
00053 *                                                               * GA2KPGM 
00054 * D1013 10/07/87  FCG  REMOVE LINK TO CSEXECIO AND REPLACE WITH * GA2KPGM 
00055 *                      LINK TO GCPPDIO. REPLACED LOGIC CODE     * GA2KPGM 
00056 *                      TO PROCESS WITH NEW INTERFACE PROGRAM.   * GA2KPGM 
00057 *                                                               * GA2KPGM 
00058 *                                                               * GA2KPGM 
00059 * 11161  10/24/90  ENW CHANGED  PROGRAM TO BRING IN COPYBOOK    * GA2KPGM 
00060 *                      GCCDRLEN.  REMOVED PF12/24 HARDCOPY      * GA2KPGM 
00061 *                      ROUTINES. REMOVED HARD CODED LENGTHS.    * GA2KPGM 
00062 *                                                               * GA2KPGM 
00063 *                                                               * GA2KPGM 
00064 *D12009 08/28/91  TPM   INCREASED THE FAMILY-RELATION FIELD     * GA2KPGM 
00065 *                           FROM ONE POSITION TO TWO POSITIONS. * GA2KPGM 
00066 *                                                               * GA2KPGM 
00067 *D12009 09/30/91  GDM   CONVERT TO COBOL II                     * GA2KPGM 
00068 *                                                               * GA2KPGM 
00069 *           08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       * GA2KPGM 
00070 *                                                               * GA2KPGM 
00071 *   D365A   05/06/03    GTF EXPAND PROCEDURE ARGUMENT FROM 6 TO * GA2KPGM 
00072 *                           7 BYTES. CHANGE # OF OCCURS TO 396  * GA2KPGM 
00073 *                           ON #ADOP TABULAR.                   * GA2KPGM 
00073 *                                                               * GA2KPGM 
00073 *ICD-10  07/06/11  BA EXPAND MAP-SELECT FIELD FROM 6 TO 7 BYTES.* GA2KPGM 
      *                     EXPAND PROCED-CODE FROM 5 TO 7 BYTES.     *         
      *                     CHANGE LOGIC FOR ICD-10 REQUIREMENTS.     *         
00074 ***************************************************************** GA2KPGM 
00075 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2KPGM 
00076 ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****GA2KPGM 
00077      SKIP3                                                        GA2KPGM 
00078      SKIP3                                                        GA2KPGM 
00079 ******************************************************************GA2KPGM 
00080 *   GA2KPGM   ALL LEVEL CONDITIONAL PROCEDURES MAINTENANCE PGM    GA2KPGM 
00081 *                          DENTAL OUTPATIENT            GA2K      GA2KPGM 
00082 *                                                                 GA2KPGM 
00083 *     THIS PROGRAM WILL ADD ENTRIES TO THE ANCILLARY RELATIONSHIP GA2KPGM 
00084 *   BENEFIT CODE ALL LEVEL TABULAR RECORD.                        GA2KPGM 
00085 *                                                                 GA2KPGM 
00086 *     THE ADD SCREEN WILL DISPLAY AN EMPTY SCREEN FOR THE OPERATORGA2KPGM 
00087 *   TO ADD ENTRIES TO THIS PARTICULAR ALL LEVEL TABULAR RECORD.   GA2KPGM 
00088 *   THE PROGRAM READS THE ENTRIES, & VALIDATES THE FORMAT OF EACH GA2KPGM 
00089 *   FIELD IN AN ENTRY (ASKING FOR A CORRECTION FOR ANY FIELD IN   GA2KPGM 
00090 *   ERROR).  IF NO ERRORS HAVE BEEN FOUND WE THEN SET ALL ENTRIES GA2KPGM 
00091 *   IN ASCENDING SEQUENCE, AND THEN INSERT THEM INTO THEIR PROPER GA2KPGM 
00092 *   POSITION IN THE RECORD, FINALLY UPDATE THE FILE WITH THE EXTRAGA2KPGM 
00093 *   ENTRIES FOR THIS ALL LEVEL TABULAR RECORD.                    GA2KPGM 
00094 *                                                                 GA2KPGM 
00095 *    TO EXECUTE THE DELETE SCREEN FOR THIS SET OF DATA (ID:#ADOP) GA2KPGM 
00096 *   THE OPERATOR PRESSES THE PF1 KEY WHICH CAUSES THE PROGRAM TO  GA2KPGM 
00097 *   XCTL TO TRANS GA1K OR PROGRAM GA1KPGM.  THIS PROGRAM WILL     GA2KPGM 
00098 *   DISPLAY ALL FIELDS ALLOWING THE OPERATOR TO CHOOSE THOSE      GA2KPGM 
00099 *   ENTRIES TO DELETE BY ENTERING 'D' IN THE CORRESPONDING ACTION GA2KPGM 
00100 *   CODE.                                                         GA2KPGM 
00101 *                                                                 GA2KPGM 
00102 *   FUNC CODE: GA2K                                               GA2KPGM 
00103 *   MAPSET:    GA2KSETC  <<<< REDEFINED BY USER DEFINED MAP >>>>  GA2KPGM 
00104 *   FILES:     GCPSWORK                                           GA2KPGM 
00105 *                                                                 GA2KPGM 
00106 ******************************************************************GA2KPGM 
00107      SKIP3                                                        GA2KPGM 
00108  ENVIRONMENT DIVISION.                                            GA2KPGM 
00109 /                                                                 GA2KPGM 
00110  DATA DIVISION.                                                   GA2KPGM 
00111  WORKING-STORAGE SECTION.                                         GA2KPGM 
00112  01  WS-BEGIN                    PIC X(57)  VALUE                 GA2KPGM 
00113      '***GA2KPGM WS BEGINS***    ***PARAGRAPH NUMBER FOLLOWS***'. GA2KPGM 
00114  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           GA2KPGM 
00115                                                                   GA2KPGM 
00116  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           GA2KPGM 
00117                                                                   GA2KPGM 
00118                                                                   GA2KPGM 
00119 ******************************************************************GA2KPGM 
00120 ** THE FIELDS LISTED BELOW ARE USED WHEN CALLING THE PROCEDURE  **GA2KPGM 
00121 ** OR DIAGNOSIS HCSC FILES.                                     **GA2KPGM 
00122 ******************************************************************GA2KPGM 
00123  01  PROCED-KEY.                                                  GA2KPGM 
00124      03  SYSTEM-INDICATOR        PIC X(1) VALUE SPACE.            GA2KPGM 
      *** ICD-10 START                                                          
00125      03  PROCED-CODE             PIC X(7).                        GA2KPGM 
00126      03  PROCEDR-DIGIT REDEFINES PROCED-CODE                      GA2KPGM 
00127              OCCURS 7 TIMES      PIC X(1).                        GA2KPGM 
00128 *** ICD-10 END                                                    GA2KPGM 
                                                                                
00129  01  WS-PRO-HAF-COMM-LEN         PIC S9(4)  COMP  VALUE +344.     GA2KPGM 
00130                                                                   GA2KPGM 
00131 ** MAP COBOL SCREEN DSECTS **                                     GA2KPGM 
00132  01  WS-I-O-MAP-AREA             PIC X(20)  VALUE                 GA2KPGM 
00133      '***  I/O MAPAREA ***'.                                      GA2KPGM 
00134  COPY GA2KSETC.                                                   GA2KPGM 
00135 /*****************************************************************GA2KPGM 
00136 ******************************************************************GA2KPGM 
00137 ******************************************************************GA2KPGM 
00138 **                                                              **GA2KPGM 
00139 **    THIS IS A USER DEFINED LOGICAL MAP.  ANY CHANGES TO       **GA2KPGM 
00140 **     MAPSET GA2KSETC AFFECTING IT\
00141 **     FOR HERE.                                                **GA2KPGM 
00142 **                                                              **GA2KPGM 
00143 **+**  OCCURS COUNT MUST BE CHANGED TO MATCH THE MAP.           **GA2KPGM 
00144 **                                                              **GA2KPGM 
00145 **                                            JLA 8/27/86       **GA2KPGM 
00146 **                                                              **GA2KPGM 
00147 ******************************************************************GA2KPGM 
00148 ******************************************************************GA2KPGM 
00149 ******************************************************************GA2KPGM 
00150                                                                   GA2KPGM 
00151  01  MAP-USER-DEFINED     REDEFINES   GA2KI01I.                   GA2KPGM 
00152                                                                   GA2KPGM 
00153      05  MAP-STORAGE-ACCOUNTING           PIC X(12).              GA2KPGM 
00154                                                                   GA2KPGM 
00155      05  MAP-FUNCTION-CODE-LEN            PIC S9(4) COMP SYNC.    GA2KPGM 
00156      05  MAP-FUNCTION-CODE-ATTR           PIC X.                  GA2KPGM 
00157      05  MAP-FUNCTION-CODE                PIC X(04).              GA2KPGM 
00158                                                                   GA2KPGM 
00159      05  MAP-TITLE-LINE-LEN               PIC S9(4) COMP SYNC.    GA2KPGM 
00160      05  MAP-TITLE-LINE-ATTR              PIC X.                  GA2KPGM 
00161      05  MAP-TITLE-LINE                   PIC X(43).              GA2KPGM 
00162                                                                   GA2KPGM 
00163      05  MAP-ADD-INQUIRE-LEN              PIC S9(4) COMP SYNC.    GA2KPGM 
00164      05  MAP-ADD-INQUIRE-ATTR             PIC X.                  GA2KPGM 
00165      05  MAP-ADD-INQUIRE                  PIC X(03).              GA2KPGM 
00166                                                                   GA2KPGM 
00167      05  MAP-SCREEN-ID-LEN                PIC S9(4) COMP SYNC.    GA2KPGM 
00168      05  MAP-SCREEN-ID-ATTR               PIC X.                  GA2KPGM 
00169      05  MAP-SCREEN-ID                    PIC X(06).              GA2KPGM 
00170                                                                   GA2KPGM 
00171      05  MAP-ID-LINE-LEN                  PIC S9(4) COMP SYNC.    GA2KPGM 
00172      05  MAP-ID-LINE-ATTR                 PIC X.                  GA2KPGM 
00173      05  MAP-ID-LINE                      PIC X(79).              GA2KPGM 
00174      05  GROUP-SPECIFIC-ID-LINE REDEFINES MAP-ID-LINE.            GA2KPGM 
00175          10  GRP-SPEC-PLAN-HEADING            PIC X(5).           GA2KPGM 
00176          10  GRP-SPEC-PLAN-CODE               PIC X(3).           GA2KPGM 
00177          10  GRP-SPEC-GROUP-HEADING           PIC X(6).           GA2KPGM 
00178          10  GRP-SPEC-GROUP-NO                PIC X(9).           GA2KPGM 
00179          10  GRP-SPEC-SECTION-HEADING         PIC X(6).           GA2KPGM 
00180          10  GRP-SPEC-SECTION-NO              PIC X(5).           GA2KPGM 
00181          10  GRP-SPEC-PKG-HEADING             PIC X(6).           GA2KPGM 
00182          10  GRP-SPEC-PKG-CODE                PIC X(3).           GA2KPGM 
00183          10  GRP-SPEC-FAM-REL-HEADING         PIC X(5).           GA2KPGM 
00184          10  GRP-SPEC-FAM-REL-LVL             PIC XX.             GA2KPGM 
00185          10  GRP-SPEC-EFF-DT-HEADING          PIC X(7).           GA2KPGM 
00186          10  GRP-SPEC-EFF-DATE                PIC X(6).           GA2KPGM 
00187          10  FILLER                           PIC X(16).          GA2KPGM 
00188      05  CONTRACT-ID-LINE  REDEFINES  MAP-ID-LINE.                GA2KPGM 
00189          10  CONTRACT-PLAN-HEADING            PIC X(5).           GA2KPGM 
00190          10  CONTRACT-PLAN-CODE               PIC X(3).           GA2KPGM 
00191          10  CONTRACT-GROUP-HEADING           PIC X(6).           GA2KPGM 
00192          10  CONTRACT-GROUP-NO                PIC X(9).           GA2KPGM 
00193          10  CONTRACT-SECTION-HEADING         PIC X(6).           GA2KPGM 
00194          10  CONTRACT-SECTION-NO              PIC X(5).           GA2KPGM 
00195          10  CONTRACT-PKG-HEADING             PIC X(6).           GA2KPGM 
00196          10  CONTRACT-PKG-CODE                PIC X(3).           GA2KPGM 
00197          10  CONTRACT-LOB-HEADING             PIC X(6).           GA2KPGM 
00198          10  CONTRACT-LOB                     PIC X.              GA2KPGM 
00199          10  CONTRACT-PROV-CTL-HEADING        PIC X(6).           GA2KPGM 
00200          10  CONTRACT-PROV-CTL                PIC XX.             GA2KPGM 
00201          10  CONTRACT-FAM-REL-HEADING         PIC X(5).           GA2KPGM 
00202          10  CONTRACT-FAM-REL-LVL             PIC XX.             GA2KPGM 
00203          10  CONTRACT-EFF-DT-HEADING          PIC X(7).           GA2KPGM 
00204          10  CONTRACT-EFF-DATE                PIC X(6).           GA2KPGM 
00205          10  FILLER                           PIC X(01).          GA2KPGM 
00206      05  BENEFIT-PROVISION-ID-LINE  REDEFINES  MAP-ID-LINE.       GA2KPGM 
00207          10  BEN-PROV-PLAN-HEADING            PIC X(4).           GA2KPGM 
00208          10  BEN-PROV-PLAN-CODE               PIC X(3).           GA2KPGM 
00209          10  BEN-PROV-GROUP-HEADING           PIC X(4).           GA2KPGM 
00210          10  BEN-PROV-GROUP-NO                PIC X(9).           GA2KPGM 
00211          10  BEN-PROV-SECTION-HEADING         PIC X(4).           GA2KPGM 
00212          10  BEN-PROV-SECTION-NO              PIC X(5).           GA2KPGM 
00213          10  BEN-PROV-PKG-HEADING             PIC X(4).           GA2KPGM 
00214          10  BEN-PROV-PKG-CODE                PIC X(3).           GA2KPGM 
00215          10  BEN-PROV-LOB-HEADING             PIC X(4).           GA2KPGM 
00216          10  BEN-PROV-LOB                     PIC X.              GA2KPGM 
00217          10  BEN-PROV-PROV-CTL-HEADING        PIC X(4).           GA2KPGM 
00218          10  BEN-PROV-PROV-CTL                PIC XX.             GA2KPGM 
00219          10  BEN-PROV-FAM-REL-HEADING         PIC X(3).           GA2KPGM 
00220          10  BEN-PROV-FAM-REL-LVL             PIC XX.             GA2KPGM 
00221          10  BEN-PROV-EFF-DT-HEADING          PIC X(5).           GA2KPGM 
00222          10  BEN-PROV-EFF-DATE                PIC X(6).           GA2KPGM 
00223          10  BEN-PROV-ID-HEADING              PIC X(6).           GA2KPGM 
00224          10  BEN-PROV-ID-NO                   PIC X(6).           GA2KPGM 
00225          10  FILLER                           PIC X(04).          GA2KPGM 
00226                                                                   GA2KPGM 
00227      05  MAP-ALL-LEVEL-TAB-ID-LEN         PIC S9(4) COMP SYNC.    GA2KPGM 
00228      05  MAP-ALL-LEVEL-TAB-ID-ATTR        PIC X.                  GA2KPGM 
00229      05  MAP-ALL-LEVEL-TAB-ID             PIC X(06).              GA2KPGM 
00230                                                                   GA2KPGM 
00231      05  MAP-ALL-LEVEL-TAB-SLOT-LEN       PIC S9(4) COMP SYNC.    GA2KPGM 
00232      05  MAP-ALL-LEVEL-TAB-SLOT-ATTR      PIC X.                  GA2KPGM 
00233      05  MAP-ALL-LEVEL-TAB-SLOT           PIC X(07).              GA2KPGM 
00234                                                                   GA2KPGM 
00235      05  MAP-FROM-MENU-ID-LEN             PIC S9(4) COMP SYNC.    GA2KPGM 
00236      05  MAP-FROM-MENU-ID-ATTR            PIC X.                  GA2KPGM 
00237      05  MAP-FROM-MENU-ID                 PIC X(04).              GA2KPGM 
00238                                                                   GA2KPGM 
00239      05  MAP-SELECT-LABEL-LEN             PIC S9(4) COMP SYNC.    GA2KPGM 
00240      05  MAP-SELECT-LABEL-ATTR            PIC X.                  GA2KPGM 
00241      05  MAP-SELECT-LABEL                 PIC X(07).              GA2KPGM 
00242                                                                   GA2KPGM 
00243      05  MAP-SELECT-LEN                   PIC S9(4) COMP SYNC.    GA2KPGM 
00244      05  MAP-SELECT-ATTR                  PIC X.                  GA2KPGM 
00245      05  MAP-SELECT                       PIC X(07).              GA2KPGM 
00246                                                                   GA2KPGM 
00247      05  MAP-SELECT-FROM-LEN              PIC S9(4) COMP SYNC.    GA2KPGM 
00248      05  MAP-SELECT-FROM-ATTR             PIC X.                  GA2KPGM 
00249      05  MAP-SELECT-FROM                  PIC X(03).              GA2KPGM 
00250                                                                   GA2KPGM 
00251      05  MAP-SELECT-TO-LABEL-LEN          PIC S9(4) COMP SYNC.    GA2KPGM 
00252      05  MAP-SELECT-TO-LABEL-ATTR         PIC X.                  GA2KPGM 
00253      05  MAP-SELECT-TO-LABEL              PIC X(02).              GA2KPGM 
00254                                                                   GA2KPGM 
00255      05  MAP-SELECT-TO-LEN                PIC S9(4) COMP SYNC.    GA2KPGM 
00256      05  MAP-SELECT-TO-ATTR               PIC X.                  GA2KPGM 
00257      05  MAP-SELECT-TO                    PIC X(03).              GA2KPGM 
00258                                                                   GA2KPGM 
00259      05  MAP-SELECT-OF-LABEL-LEN          PIC S9(4) COMP SYNC.    GA2KPGM 
00260      05  MAP-SELECT-OF-LABEL-ATTR         PIC X.                  GA2KPGM 
00261      05  MAP-SELECT-OF-LABEL              PIC X(02).              GA2KPGM 
00262                                                                   GA2KPGM 
00263      05  MAP-SELECT-OF-LEN                PIC S9(4) COMP SYNC.    GA2KPGM 
00264      05  MAP-SELECT-OF-ATTR               PIC X.                  GA2KPGM 
00265      05  MAP-SELECT-OF                    PIC X(03).              GA2KPGM 
00266                                                                   GA2KPGM 
00267      05  MAP-SELECT-DISPLAY-LABEL-LEN     PIC S9(4) COMP SYNC.    GA2KPGM 
00268      05  MAP-SELECT-DISPLAY-LABEL-ATTR    PIC X.                  GA2KPGM 
00269      05  MAP-SELECT-DISPLAY-LABEL         PIC X(24).              GA2KPGM 
00270                                                                   GA2KPGM 
00271      05  MAP-PROCEDURE-ARGUMENT-ROW  OCCURS 15 TIMES              GA2KPGM 
00272          INDEXED BY MAP-IDX.                                      GA2KPGM 
00273          15  MAP-PROCEDURE-ARGUMENT-LEN   PIC S9(4) COMP SYNC.    GA2KPGM 
00274          15  MAP-PROCEDURE-ARGUMENT-ATTR  PIC X.                  GA2KPGM 
00275          15  MAP-PROCEDURE-ARGUMENT       PIC X(7).               GA2KPGM 
00276          15  MAP-CODE-FUNCTION-LEN        PIC S9(4) COMP SYNC.    GA2KPGM 
00277          15  MAP-CODE-FUNCTION-ATTR       PIC X.                  GA2KPGM 
00278          15  MAP-CODE-FUNCTION            PIC X(3).               GA2KPGM 
00279                                                                   GA2KPGM 
00280      05  MAP-PAGING-LABEL-LEN             PIC S9(4) COMP SYNC.    GA2KPGM 
00281      05  MAP-PAGING-LABEL-ATTR            PIC X.                  GA2KPGM 
00282      05  MAP-PAGING-LABEL                 PIC X(79).              GA2KPGM 
00283                                                                   GA2KPGM 
00284      05  MAP-ERROR-MESSAGE-LEN            PIC S9(4) COMP SYNC.    GA2KPGM 
00285      05  MAP-ERROR-MESSAGE-ATTR           PIC X.                  GA2KPGM 
00286      05  MAP-ERROR-MESSAGE                PIC X(79).              GA2KPGM 
00287      SKIP3                                                        GA2KPGM 
00288  01  FILLER.                                                      GA2KPGM 
00289 ****************************************************************  GA2KPGM 
00290 **   THIS FIELD DESCRIBES THE NUMBER OF OCCURS IN THE MAP.        GA2KPGM 
00291 ****************************************************************  GA2KPGM 
00292      05  WS-MAP-ROW                  PIC S999 COMP-3  VALUE +15.  GA2KPGM 
00293 /                                                                 GA2KPGM 
00294 ** ALTERNATIVE WORKFILE KEYS **                                   GA2KPGM 
00295  01  FILLER                      PIC X(32)  VALUE                 GA2KPGM 
00296      '*** ALTERNATIVE WORKFILE KEY ***'.                          GA2KPGM 
00297  01  WS-ALT-WORKFILE-KEYS.                                        GA2KPGM 
00298  COPY GCWRKKEY.                                                   GA2KPGM 
00299 /                                                                 GA2KPGM 
00300 ** HARDCOPY WORK AREA **                                          GA2KPGM 
00301  01  FILLER                      PIC X(26)  VALUE                 GA2KPGM 
00302      '*** HARDCOPY WORK AREA ***'.                                GA2KPGM 
00303 *01  WS-HARDCOPY-COMMAREA.                                        GA2KPGM 
00304 *COPY PRNCOBOL.                                                   GA2KPGM 
00305                                                                   GA2KPGM 
00306 ** DATE FORMATTING AREA **                                        GA2KPGM 
00307  01  FILLER                      PIC X(28)  VALUE                 GA2KPGM 
00308      '*** DATE FORMATTING AREA ***'.                              GA2KPGM 
00309  01  WS-DATE-AREA.                                                GA2KPGM 
00310      05  WS-MDY.                                                  GA2KPGM 
00311        10  WS-M                  PIC 99.                          GA2KPGM 
00312        10  WS-D                  PIC 99.                          GA2KPGM 
00313        10  WS-Y                  PIC 99.                          GA2KPGM 
00314      05  WS-YYDDD                PIC 9(5).                        GA2KPGM 
00315      05  FILLER          REDEFINES   WS-YYDDD.                    GA2KPGM 
00316        10  WS-YY                     PIC 99.                      GA2KPGM 
00317        10  WS-DDD                    PIC 999.                     GA2KPGM 
00318                                                                   GA2KPGM 
00319 ******************************************************            GA2KPGM 
00320 **    MONTH TABLE FOR DATE CONVERSION                             GA2KPGM 
00321 **    WILL BE GENERATED ONLY ONCE                                 GA2KPGM 
00322 ******************************************************            GA2KPGM 
00323  01   WS-JUL-GREG-DATE-CONV-TAB.                                  GA2KPGM 
00324      05  WS-MONTH-TABLE   OCCURS 13  INDEXED BY WS-M-IDX          GA2KPGM 
00325          PIC S999 COMP-3.                                         GA2KPGM 
00326                                                                   GA2KPGM 
00327 ** WORKFIELDS **                                                  GA2KPGM 
00328  01  FILLER                           PIC X(16)                   GA2KPGM 
00329              VALUE  '** WORKFIELDS **'.                           GA2KPGM 
00330  01  WS-WORK-FIELDS.                                              GA2KPGM 
00331      05  WS-HEX-00                    PIC X.                      GA2KPGM 
00332      05  WS-QUOTIENT                  PIC 999  COMP-3.            GA2KPGM 
00333      05  WS-REMAINDER                 PIC 999  COMP-3.            GA2KPGM 
00334      05  WS-ADD-COUNT                 PIC 999  COMP-3.            GA2KPGM 
00335 ***  05  WS-NON-SPECIAL-CHARACTERS    PIC X(37)                   GA2KPGM 
00336 ***    VALUE '1234567890 ABCDEFGHIJKLMNOPQRSTUVWXYZ'.             GA2KPGM 
00337                                                                   GA2KPGM 
00338  01  WS-TEST-AREA                     PIC X(7).                   GA2KPGM 
00339  01  WS-TEST-DATA REDEFINES WS-TEST-AREA.                         GA2KPGM 
00340      05  WS-TEST-DETAIL OCCURS 7 TIMES PIC X(01).                 GA2KPGM 
00341          88  WS-NON-SPECIAL-CHARACTERS VALUE                      GA2KPGM 
00342                                        SPACE                      GA2KPGM 
00343                                        '0' THRU '9'               GA2KPGM 
00344                                        'A' THRU 'I'               GA2KPGM 
00345                                        'J' THRU 'R'               GA2KPGM 
00346                                        'S' THRU 'Z'.              GA2KPGM 
00347                                                                   GA2KPGM 
00348 ****************************************************************  GA2KPGM 
00349 ** THIS GROUP OF FIELDS MATCHES ONE OCCURENCE IN THE TABULAR      GA2KPGM 
00350 ** RECORD, IT IS USED AS A TEMPORARY HOLD AREA.                   GA2KPGM 
00351 ****************************************************************  GA2KPGM 
00352      05  WS-SAVED-FIELDS.                                         GA2KPGM 
00353        10  WS-SAVED-PROCEDURE-ARGUMENT  PIC X(7).                 GA2KPGM 
00354        10  WS-SAVED-CODE-FUNCTION       PIC X(3).                 GA2KPGM 
00355 ****************************************************************  GA2KPGM 
00356 ** THIS IS THE AREA IN WHICH THE SORTING OF NEW ENTRIES HAPPENS.  GA2KPGM 
00357 ** ONE MORE OCCURENCE IS PROVIDED THAN IS FOUND ON THE SCREEN,    GA2KPGM 
00358 ** THIS IS FOR THE TRAILER.                                       GA2KPGM 
00359 ****************************************************************  GA2KPGM 
00360      05  WS-SORTED-TAB     OCCURS 16 TIMES INDEXED BY             GA2KPGM 
00361          WS-SORT-IDX, WS-SORT-IDX2, WS-SORT-IDX3.                 GA2KPGM 
00362        10  WS-PROCEDURE-ARGUMENT      PIC X(7).                   GA2KPGM 
00363        10  WS-CODE-FUNCTION           PIC X(3).                   GA2KPGM 
00364                                                                   GA2KPGM 
00365 *** SWITCHES ***                                                  GA2KPGM 
00366  01  FILLER                           PIC X(14)                   GA2KPGM 
00367              VALUE  '** SWITCHES **'.                             GA2KPGM 
00368  01  WS-SWITCHES.                                                 GA2KPGM 
00369      05  WS-ERROR-SW                  PIC X.                      GA2KPGM 
00370                                                                   GA2KPGM 
00371 ** TITLE LINES **                                                 GA2KPGM 
00372  01  WS-TITLE-LINES.                                              GA2KPGM 
00373      05  GROUP-SPECIFIC-TITLE-LINE            PIC X(43)  VALUE    GA2KPGM 
00374          '   GROUP SPECIFIC CONDITIONAL PROCEDURES   '.           GA2KPGM 
00375      05  CONTRACT-TITLE-LINE                  PIC X(43)  VALUE    GA2KPGM 
00376          '      CONTRACT CONDITIONAL PROCEDURES      '.           GA2KPGM 
00377      05  BENEFIT-PROVISION-TITLE-LINE         PIC X(43)  VALUE    GA2KPGM 
00378          '  BENEFIT PROVISION CONDITIONAL PROCEDURES '.           GA2KPGM 
00379                                                                   GA2KPGM 
00380 /                                                                 GA2KPGM 
00381 *** RECORD LENGTHS ***                                            GA2KPGM 
00382  01  FILLER                           PIC X(20)                   GA2KPGM 
00383              VALUE  '** RECORD LENGTHS **'.                       GA2KPGM 
00384  01  WS-RECORD-LENGTHS.                                           GA2KPGM 
00385     05 WS-IO-PARM-WRK-ALL-LVL-TAB-LEN PIC S9(4) COMP.             GA2KPGM 
00386     05 WS-XCTL-WRK-LEN                PIC S9(4) COMP.             GA2KPGM 
00387     05 WS-COPY-LENGTH                 PIC S9(4) COMP.             GA2KPGM 
00388     05 GCVI-COMMAREA-LEN              PIC S9(4) COMP   VALUE +19. GA2KPGM 
00389     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP   VALUE +100.GA2KPGM 
00390 *   05 GC-GCIOPARM-LEN                PIC S9(5) COMP-3 VALUE +228.GA2KPGM 
00391 *   05 GC-WORKFILE-KEY-LEN            PIC S9(5) COMP-3 VALUE +64. GA2KPGM 
00392 *   05 WS-GRP-SPEC-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +410.GA2KPGM 
00393 *   05 WS-GRP-SPEC-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2KPGM 
00394 *   05 WS-GRP-SPEC-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +30. GA2KPGM 
00395 *   05 WS-CONTRACT-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +563.GA2KPGM 
00396 *   05 WS-CONTRACT-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2KPGM 
00397 *   05 WS-CONTRACT-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +520.GA2KPGM 
00398 *   05 WS-BEN-PROV-FIXED-PORTION      PIC S9(5) COMP-3 VALUE +501.GA2KPGM 
00399 *   05 WS-BEN-PROV-KEY-LENGTH         PIC S9(5) COMP-3 VALUE +10. GA2KPGM 
00400 *   05 WS-BEN-PROV-MAX-OCCURS         PIC S9(5) COMP-3 VALUE +15. GA2KPGM 
00401 ****************************************************************  GA2KPGM 
00402 **   THESE FIELDS DESCRIBE THE TABULAR RECORD.                    GA2KPGM 
00403 ****************************************************************  GA2KPGM 
00404 *   05 GC-GCTABULR-ADOP-FIXED-LEN     PIC S9(5) COMP-3 VALUE +40. GA2KPGM 
00405 *   05 GC-GCTABULR-ADOP-VARY-LEN      PIC S9(5) COMP-3 VALUE +9.  GA2KPGM 
00406 *   05 GC-GCTABULR-ADOP-VARY-MAX-OCUR                             GA2KPGM 
00407 *                                     PIC S9(5) COMP-3 VALUE +440.GA2KPGM 
00408 /                                                                 GA2KPGM 
00409  COPY COBXIO.                                                     GA2KPGM 
00410 /                                                                 GA2KPGM 
00411 ** ATTRIBUTES **                                                  GA2KPGM 
00412  COPY DFHBMSCA.                                                   GA2KPGM 
00413      02  DFHBMABF                     PIC X VALUE 'Z'.            GA2KPGM 
00414 /                                                                 GA2KPGM 
00415 ** ATTENTION IDENTIFIERS **                                       GA2KPGM 
00416  COPY DFHAID.                                                     GA2KPGM 
00417 /                                                                 GA2KPGM 
00418  01  GCVIOPGMS-PARM.                                              GA2KPGM 
00419  COPY GCVINTRC.                                                   GA2KPGM 
00420                                                                   GA2KPGM 
00421  01  WS-GCPS-LENGTHS.                                             GA2KPGM 
00422      COPY GCCDRLEN.                                               GA2KPGM 
00423 /                                                                 GA2KPGM 
00424  01  COMMAREA-POINTER-AREA.                                       GA2KPGM 
00425      05  COMMAREA-PNTR-COMP PIC S9(8) COMP.                       GA2KPGM 
00426      05  COMMAREA-PNTR      REDEFINES                             GA2KPGM 
00427          COMMAREA-PNTR-COMP USAGE IS POINTER.                     GA2KPGM 
00428 /                                                                 GA2KPGM 
00429  01  WS-END                          PIC X(16)  VALUE             GA2KPGM 
00430      '*** W/S ENDS ***'.                                          GA2KPGM 
00431 /                                                                 GA2KPGM 
00432  LINKAGE SECTION.                                                 GA2KPGM 
00433 /                                                                 GA2KPGM 
00434  01  DFHCOMMAREA.                                                 GA2KPGM 
00435  COPY G2ALCKEC.                                                   GA2KPGM 
00436                                                                   GA2KPGM 
00437 *    05  INCOMING-COMMAREA-PNTR-COMP PIC S9(8) COMP.              GA2KPGM 
00438 *    05  INCOMING-COMMAREA-PNTR      REDEFINES                    GA2KPGM 
00439 *        INCOMING-COMMAREA-PNTR-COMP USAGE IS POINTER.            GA2KPGM 
00440 **                                                                GA2KPGM 
00441 *01  BLL-CELLS.                                                   GA2KPGM 
00442 *    02  FILLER                      PIC S9(8)  COMP.             GA2KPGM 
00443 *    02  COMMAREA-PNTR               PIC S9(8)  COMP.             GA2KPGM 
00444 *    02  ALL-LEVEL-TAB-PNTR          PIC S9(8)  COMP.             GA2KPGM 
00445 *    02  ALL-LEVEL-TAB-PNTR2         PIC S9(8)  COMP.             GA2KPGM 
00446 *    02  COPY-AREA-PNTR              PIC S9(8)  COMP.             GA2KPGM 
00447 *    02  GRP-SPEC-PNTR               PIC S9(8)  COMP.             GA2KPGM 
00448 *    02  CONTRACT-PNTR               PIC S9(8)  COMP.             GA2KPGM 
00449 *    02  CONTRACT-PNTR2              PIC S9(8)  COMP.             GA2KPGM 
00450 *    02  BEN-PROV-PNTR               PIC S9(8)  COMP.             GA2KPGM 
00451 *    02  GCPPDIO-BLL-PNTR            PIC S9(8)  COMP.             GA2KPGM 
00452 *                                                                 GA2KPGM 
00453 *01  GCA-COMMAREA.                                                GA2KPGM 
00454 *COPY G2ALCKEC.                                                   GA2KPGM 
00455 /                                                                 GA2KPGM 
00456 ** I/O PARM, WORKFILE KEY, AND CONTRACT TABULAR RECORD **         GA2KPGM 
00457  01  IO-PARM-ALL-LVL-TAB-RECORD.                                  GA2KPGM 
00458  COPY GCIOPRM1.                                                   GA2KPGM 
00459 /                                                                 GA2KPGM 
00460  COPY GCWRKDCC.                                                   GA2KPGM 
00461 /                                                                 GA2KPGM 
00462  COPY GCTADOPC.                                                   GA2KPGM 
00463 /                                                                 GA2KPGM 
00464 ***************************************************************** GA2KPGM 
00465 ** THIS AREA MATCHES THE TABLE FROM THE TABULAR RECORD, THE       GA2KPGM 
00466 ** ENTRIES ARE COPIED HERE FROM THE RECORD AND THEN MERGED WITH   GA2KPGM 
00467 ** DATA FROM THE SCREEN BACK INTO THE RECORD BY THE SORT PROCESS. GA2KPGM 
00468 ***************************************************************** GA2KPGM 
00469  01  COPY-OF-TABLE-AREA.                                          GA2KPGM 
00470      05  COPY-OF-TABLE    OCCURS 396 TIMES    INDEXED BY          GA2KPGM 
00471            COPY-IDX.                                              GA2KPGM 
00472        10  COPY-PROCEDURE-ARGUMENT     PIC X(7).                  GA2KPGM 
00473        10  COPY-CODE-FUNCTION          PIC X(3).                  GA2KPGM 
00474 /                                                                 GA2KPGM 
00475 ** IO PARM, WITH WORKFILE KEY, AND RECORDS **                     GA2KPGM 
00476  01  IO-PARM-GRP-SPEC-RECORD.                                     GA2KPGM 
00477  COPY GCIOPRM2.                                                   GA2KPGM 
00478 /                                                                 GA2KPGM 
00479  COPY GCWRKDC2.                                                   GA2KPGM 
00480 /                                                                 GA2KPGM 
00481  COPY GCGROUPC.                                                   GA2KPGM 
00482 /                                                                 GA2KPGM 
00483                                                                   GA2KPGM 
00484  01  IO-PARM-CONTRACT-RECORD.                                     GA2KPGM 
00485  COPY GCIOPRM3.                                                   GA2KPGM 
00486 /                                                                 GA2KPGM 
00487  COPY GCWRKDC3.                                                   GA2KPGM 
00488 /                                                                 GA2KPGM 
00489  COPY GCCONTRC.                                                   GA2KPGM 
00490 /                                                                 GA2KPGM 
00491                                                                   GA2KPGM 
00492  01  IO-PARM-BEN-PROV-RECORD.                                     GA2KPGM 
00493  COPY GCIOPRM4.                                                   GA2KPGM 
00494 /                                                                 GA2KPGM 
00495  COPY GCWRKDC4.                                                   GA2KPGM 
00496 /                                                                 GA2KPGM 
00497  COPY GCBENPVC.                                                   GA2KPGM 
00498 /                                                                 GA2KPGM 
00499 ** IO PARM AREA **                                                GA2KPGM 
00500  01  GCPPDIO-PARM-AREA.                                           GA2KPGM 
00501  COPY GCPPDIOC.                                                   GA2KPGM 
00502 /                                                                 GA2KPGM 
00503                                                                   GA2KPGM 
00504  PROCEDURE DIVISION.                                              GA2KPGM 
00505                                                                   GA2KPGM 
00506 ******************************************************************GA2KPGM 
00507 **                H O U S E K E E P I N G                         GA2KPGM 
00508 **                                                                GA2KPGM 
00509 **  DO THE ONETIME LOGIC THAT IS DONE AT THE START OF THE PROGRAM.GA2KPGM 
00510 **                                                                GA2KPGM 
00511 ******************************************************************GA2KPGM 
00512  0000-HOUSEKEEPING SECTION.                                       GA2KPGM 
00513                                                                   GA2KPGM 
00514      MOVE ZERO  TO  WS-MONTH-TABLE (1),  WS-YYDDD.                GA2KPGM 
00515      MOVE LOW-VALUES  TO  WS-HEX-00.                              GA2KPGM 
00516      MOVE  31  TO  WS-MONTH-TABLE (2).                            GA2KPGM 
00517      MOVE  59  TO  WS-MONTH-TABLE (3).                            GA2KPGM 
00518      MOVE  90  TO  WS-MONTH-TABLE (4).                            GA2KPGM 
00519      MOVE 120  TO  WS-MONTH-TABLE (5).                            GA2KPGM 
00520      MOVE 151  TO  WS-MONTH-TABLE (6).                            GA2KPGM 
00521      MOVE 181  TO  WS-MONTH-TABLE (7).                            GA2KPGM 
00522      MOVE 212  TO  WS-MONTH-TABLE (8).                            GA2KPGM 
00523      MOVE 243  TO  WS-MONTH-TABLE (9).                            GA2KPGM 
00524      MOVE 273  TO  WS-MONTH-TABLE (10).                           GA2KPGM 
00525      MOVE 304  TO  WS-MONTH-TABLE (11).                           GA2KPGM 
00526      MOVE 334  TO  WS-MONTH-TABLE (12).                           GA2KPGM 
00527      MOVE 400  TO  WS-MONTH-TABLE (13).                           GA2KPGM 
00528                                                                   GA2KPGM 
00529      EXEC CICS HANDLE CONDITION   MAPFAIL(6000-XCTL-TO-MAIN-MENU) GA2KPGM 
00530                                   END-EXEC.                       GA2KPGM 
00531 /                                                                 GA2KPGM 
00532 ******************************************************************GA2KPGM 
00533 **                     M A I N L I N E                            GA2KPGM 
00534 **                                                                GA2KPGM 
00535 **   THE MAINLINE DETERMINES THE PROGRAM FLOW BASED ON THE ACTIONSGA2KPGM 
00536 **  TAKEN BY THE OPERATOR.                                        GA2KPGM 
00537 **  1. IF THIS PROGRAM GOT CONTROL FROM ANOTHER TRANSACTION THEN  GA2KPGM 
00538 **     WE WANT TO DISPLAY THE FIRST SCREEN FOR THEM TO ENTER      GA2KPGM 
00539 **     ADDITIONS FROM.                                            GA2KPGM 
00540 **  2. IF THE OPERATOR WANTS THE SCREEN PRINTED THEY USED FUNCTIONGA2KPGM 
00541 **     KEY PF12 OR PF24.                                          GA2KPGM 
00542 **  3. RECEIVE THE SCREEN.                                        GA2KPGM 
00543 **  4. IF THE SCREEN ID IS NOT ON THE SCREEN THEN XCTL TO THE MAINGA2KPGM 
00544 **     MENU.                                                      GA2KPGM 
00545 **  5. IF THEY USED THE ENTER KEY THEN PERFORM NORMAL ADD LOGIC.  GA2KPGM 
00546 **  6. IF THEY USED EITHER FUNCTION KEY PF1 OR PF13 THEN XCTL     GA2KPGM 
00547 **     (RETURN) TO THE DELETE PROGRAM (GA1KPGM).                  GA2KPGM 
00548 **  7. IF THEY USED EITHER FUNCTION KEY PF3 OR PF15 THEN XCTL     GA2KPGM 
00549 **     (RETURN) TO THE PREVIOUS MENU.                             GA2KPGM 
00550 **  8. IF THEY USED EITHER FUNCTION KEY PF4 OR PF16 THEN EXECUTE  GA2KPGM 
00551 **     THE NORMAL ADD LOGIC, EXCEPT TO BYPASS AN EMPTY VALIDATION GA2KPGM 
00552 **     TABLE CONDITION FOR THE COMBINATION CODE FIELD.            GA2KPGM 
00553 **  9. IF NONE OF THE ABOVE THEN THEY'VE USED AN INVALID FUNCTION GA2KPGM 
00554 **     KEY, BUILD AND DISPLAY AN ERROR MESSAGE.                   GA2KPGM 
00555 **                                                                GA2KPGM 
00556 ******************************************************************GA2KPGM 
00557  1000-MAIN-LINE SECTION.                                          GA2KPGM 
00558                                                                   GA2KPGM 
00559      MOVE '1000'  TO  WS-PARA-ID.                                 GA2KPGM 
00560      IF EIBTRNID  NOT =  'GA2K'                                   GA2KPGM 
00561         PERFORM 4000-DISPLAY-FIRST-SCREEN                         GA2KPGM 
00562         GO TO 1099-RETURN.                                        GA2KPGM 
00563                                                                   GA2KPGM 
00564      EXEC CICS RECEIVE   MAP('GA2KI01') MAPSET('GA2KSET')         GA2KPGM 
00565         INTO(GA2KI01I) END-EXEC.                                  GA2KPGM 
00566                                                                   GA2KPGM 
00567      IF MAP-SCREEN-ID  NOT = '002K00'                             GA2KPGM 
00568         PERFORM 6000-XCTL-TO-MAIN-MENU.                           GA2KPGM 
00569                                                                   GA2KPGM 
00570      IF EIBAID  =  DFHENTER                                       GA2KPGM 
00571         PERFORM 2000-ADD-PROCESSING                               GA2KPGM 
00572         GO TO 1099-RETURN.                                        GA2KPGM 
00573                                                                   GA2KPGM 
00574      IF EIBAID  =  DFHPF1 OR  =  DFHPF13                          GA2KPGM 
00575         PERFORM 3000-XCTL-TO-DEL-SCREEN.                          GA2KPGM 
00576                                                                   GA2KPGM 
00577      IF EIBAID  =  DFHPF4 OR  =  DFHPF16                          GA2KPGM 
00578         PERFORM 2000-ADD-PROCESSING                               GA2KPGM 
00579         GO TO 1099-RETURN.                                        GA2KPGM 
00580                                                                   GA2KPGM 
00581      IF EIBAID  =  DFHPF3 OR  =  DFHPF15                          GA2KPGM 
00582         PERFORM 5000-XCTL-TO-PREVIOUS-MENU.                       GA2KPGM 
00583                                                                   GA2KPGM 
00584      SET MAP-IDX  TO  1.                                          GA2KPGM 
00585      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX).           GA2KPGM 
00586      MOVE '*** INVALID REQUEST.  THE PF KEY USED HAS NO MEANING TOGA2KPGM 
00587 -    ' THIS PROGRAM ***'  TO  MAP-ERROR-MESSAGE.                  GA2KPGM 
00588      EXEC CICS SEND   MAP('GA2KI01') MAPSET('GA2KSET') DATAONLY   GA2KPGM 
00589         FROM(GA2KI01O) CURSOR END-EXEC.                           GA2KPGM 
00590                                                                   GA2KPGM 
00591  1099-RETURN.                                                     GA2KPGM 
00592 *    EXEC CICS RETURN   END-EXEC.                                 GA2KPGM 
00593      EXEC CICS RETURN TRANSID ('GA2K')                            GA2KPGM 
00594                COMMAREA (DFHCOMMAREA)                             GA2KPGM 
00595                END-EXEC.                                          GA2KPGM 
00596                                                                   GA2KPGM 
00597      GOBACK.                                                      GA2KPGM 
00598 /                                                                 GA2KPGM 
00599 ******************************************************************GA2KPGM 
00600 **               A D D   P R O C E S S I N G                      GA2KPGM 
00601 **                                                                GA2KPGM 
00602 **   THIS IS THE PROGRAM LOGIC THAT WILL BE PERFORMED FOR THE     GA2KPGM 
00603 **  MAJORITY OF THE TRANSACTIONS PROCESSED BY GA2KPGM.            GA2KPGM 
00604 **  1. RESET ALL ATTRIBUTES TO NORMAL INTENSITY.                  GA2KPGM 
00605 **  2. DETERMINE IF ANY VALUES WERE ENTERED FOR THIS LINE.  IF NOTGA2KPGM 
00606 **     SKIP TO THE NEXT LINE.                                     GA2KPGM 
00607 **  3. VALIDATE EACH FIELD.  ALPHANUMERIC FIELDS WILL NOT ACCEPTEDGA2KPGM 
00608 **     WITH SPECIAL CHARACTERS, AND NUMERIC FIELDS ARE TESTED     GA2KPGM 
00609 **     WITH THE NUMERIC CLASS TEST.  THE OPERATOR MUST ENTER SOME GA2KPGM 
00610 **     VALUE FOR EACH FIELD IN A LINE IN WHICH ANY OTHER FIELD HASGA2KPGM 
00611 **     DATA.                                                      GA2KPGM 
00612 **  4. IF THE OPERATOR HAS ENTERED NO ADDITIONS ON A SCREEN AN    GA2KPGM 
00613 **     APPROPRIATE MESSAGE IS DISPLAYED.                          GA2KPGM 
00614 **  5. ALL LINES, THAT CONTAIN DATA, ARE SEQUENCED INTO ASCENDING GA2KPGM 
00615 **     ORDER, FIELD BY FIELD.                                     GA2KPGM 
00616 **  6. THE ALL LEVEL TABULAR RECORD IS READ, AND A COPY OF THE    GA2KPGM 
00617 **     TABLE IS MADE.                                             GA2KPGM 
00618 **  7. THEN THE TWO TABLES (SEQUENCED ENTRIES FROM THE SCREEN, ANDGA2KPGM 
00619 **     COPY OF THE RECORDS TABLE) ARE MERGED IN ASCENDING SEQUENCEGA2KPGM 
00620 **     BACK INTO THE RECORD.                                      GA2KPGM 
00621 **  8. THE RECORD IS REWRITTEN BACK ONTO THE WORKFILE, AND A FRESHGA2KPGM 
00622 **     SCREEN IS DISPLAYED TO THE OPERATOR FOR MORE ADDITIONS.    GA2KPGM 
00623 **                                                                GA2KPGM 
00624 ******************************************************************GA2KPGM 
00625  2000-ADD-PROCESSING SECTION.                                     GA2KPGM 
00626                                                                   GA2KPGM 
00627      MOVE '2000'  TO  WS-PARA-ID.                                 GA2KPGM 
00628      MOVE 'N'  TO  WS-ERROR-SW.                                   GA2KPGM 
00629      MOVE 'Y'  TO  GCVI-TABLE-SW.                                 GA2KPGM 
00630      MOVE ZERO  TO  WS-ADD-COUNT.                                 GA2KPGM 
00631      SET MAP-IDX  TO  1.                                          GA2KPGM 
00632                                                                   GA2KPGM 
00633      MOVE '2005'  TO  WS-PARA-ID.                                 GA2KPGM 
00634  2005-RESET-ALL-ATTRIBUTES.                                       GA2KPGM 
00635      MOVE DFHBMUNF TO MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)       GA2KPGM 
00636                       MAP-CODE-FUNCTION-ATTR (MAP-IDX).           GA2KPGM 
00637      IF MAP-IDX   <  WS-MAP-ROW                                   GA2KPGM 
00638         SET MAP-IDX   UP BY  1                                    GA2KPGM 
00639         GO TO 2005-RESET-ALL-ATTRIBUTES.                          GA2KPGM 
00640                                                                   GA2KPGM 
00641      SET MAP-IDX  TO  1.                                          GA2KPGM 
00642      MOVE '2010'  TO  WS-PARA-ID.                                 GA2KPGM 
00643  2010-VALIDATE-ADD-ENTRIES.                                       GA2KPGM 
00644      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)  =  ZERO  AND        GA2KPGM 
00645         MAP-CODE-FUNCTION-LEN (MAP-IDX)  =  ZERO                  GA2KPGM 
00646         IF MAP-IDX   <  WS-MAP-ROW                                GA2KPGM 
00647            SET MAP-IDX   UP BY  1                                 GA2KPGM 
00648            GO TO 2010-VALIDATE-ADD-ENTRIES                        GA2KPGM 
00649         ELSE                                                      GA2KPGM 
00650            GO TO 2020-CHECK-FOR-ERRORS.                           GA2KPGM 
00651                                                                   GA2KPGM 
00652      IF MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)  =  ZERO             GA2KPGM 
00653         MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  GA2KPGM 
00654         MOVE '???????'  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)      GA2KPGM 
00655         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2KPGM 
00656            MOVE 'Y'  TO  WS-ERROR-SW                              GA2KPGM 
00657            MOVE -1   TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)     GA2KPGM 
00658            MOVE ' *** PROCEDURE ARGUMENT IS INVALID ***'  TO      GA2KPGM 
00659                                                 MAP-ERROR-MESSAGE GA2KPGM 
00660         ELSE                                                      GA2KPGM 
00661            NEXT SENTENCE                                          GA2KPGM 
00662      ELSE                                                         GA2KPGM 
00663         PERFORM 2015-EDIT-PROCEDURE-ARGUMENT                      GA2KPGM 
00664            THRU 2015-EXIT.                                        GA2KPGM 
00665                                                                   GA2KPGM 
00666      IF  MAP-CODE-FUNCTION-LEN (MAP-IDX)  =  ZERO                 GA2KPGM 
00667         MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)       GA2KPGM 
00668         MOVE '???'  TO  MAP-CODE-FUNCTION (MAP-IDX)               GA2KPGM 
00669         IF  WS-ERROR-SW  NOT  =  'Y'                              GA2KPGM 
00670            MOVE 'Y'  TO  WS-ERROR-SW                              GA2KPGM 
00671            MOVE -1   TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)          GA2KPGM 
00672            MOVE ' *** CODE FUNCTION IS INVALID ***'               GA2KPGM 
00673              TO  MAP-ERROR-MESSAGE                                GA2KPGM 
00674         ELSE                                                      GA2KPGM 
00675            NEXT SENTENCE                                          GA2KPGM 
00676      ELSE                                                         GA2KPGM 
00677        MOVE MAP-CODE-FUNCTION(MAP-IDX)  TO                        GA2KPGM 
00678            WS-TEST-AREA                                           GA2KPGM 
00679 ***    MOVE MAP-CODE-FUNCTION(MAP-IDX)  TO  WS-SAVED-CODE-FUNCTIONGA2KPGM 
00680 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION  FROM  QUOTES  TO  '\
00681 ***     TRANSFORM  WS-SAVED-CODE-FUNCTION  FROM                   GA2KPGM 
00682 ***                         WS-NON-SPECIAL-CHARACTERS  TO  QUOTES GA2KPGM 
00683 ***     IF  WS-SAVED-CODE-FUNCTION  NOT =  QUOTES                 GA2KPGM 
00684         IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                GA2KPGM 
00685                 WS-NON-SPECIAL-CHARACTERS  (2) AND                GA2KPGM 
00686                 WS-NON-SPECIAL-CHARACTERS  (3) AND                GA2KPGM 
00687                 WS-NON-SPECIAL-CHARACTERS  (4) AND                GA2KPGM 
00688                 WS-NON-SPECIAL-CHARACTERS  (5) AND                GA2KPGM 
00689                 WS-NON-SPECIAL-CHARACTERS  (6) AND                GA2KPGM 
00690                 WS-NON-SPECIAL-CHARACTERS  (7)                    GA2KPGM 
00691            MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX)    GA2KPGM 
00692            IF  WS-ERROR-SW  NOT =  'Y'                            GA2KPGM 
00693               MOVE 'Y'  TO  WS-ERROR-SW                           GA2KPGM 
00694               MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)        GA2KPGM 
00695               MOVE '        *** CODE FUNCTION IS INVALID ***'     GA2KPGM 
00696                 TO  MAP-ERROR-MESSAGE.                            GA2KPGM 
00697                                                                   GA2KPGM 
00698      IF MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX)  NOT =  DFHBMUBF     GA2KPGM 
00699                            AND                                    GA2KPGM 
00700         MAP-CODE-FUNCTION-ATTR(MAP-IDX)  NOT =  DFHBMUBF          GA2KPGM 
00701         ADD 1  TO  WS-ADD-COUNT                                   GA2KPGM 
00702         SET WS-SORT-IDX  TO  WS-ADD-COUNT                         GA2KPGM 
00703         MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX) TO                  GA2KPGM 
00704                             WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)   GA2KPGM 
00705         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO                      GA2KPGM 
00706                                  WS-CODE-FUNCTION (WS-SORT-IDX).  GA2KPGM 
00707                                                                   GA2KPGM 
00708      IF MAP-CODE-FUNCTION-ATTR (MAP-IDX)  NOT =  DFHBMUBF         GA2KPGM 
00709         MOVE 'MULT02' TO  GCVI-FIELDS-KEY-ID                      GA2KPGM 
00710         MOVE ZEROES  TO  GCVI-RETURN-CODE                         GA2KPGM 
00711         MOVE MAP-CODE-FUNCTION (MAP-IDX)  TO  GCVI-VALUE-LEN-3    GA2KPGM 
00712         EXEC CICS LINK PROGRAM('GCVIOPGM')                        GA2KPGM 
00713                        COMMAREA(GCVIOPGMS-PARM)                   GA2KPGM 
00714                        LENGTH(GCVI-COMMAREA-LEN) END-EXEC         GA2KPGM 
00715         IF GCVI-VALUE-NOT-FOUND                                   GA2KPGM 
00716            IF WS-ERROR-SW  NOT =  'Y'                             GA2KPGM 
00717               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR (MAP-IDX) GA2KPGM 
00718               MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)        GA2KPGM 
00719               MOVE '*** COMBINATION CODE INVALID ***'             GA2KPGM 
00720                 TO  MAP-ERROR-MESSAGE                             GA2KPGM 
00721               MOVE 'Y'  TO  WS-ERROR-SW                           GA2KPGM 
00722            ELSE                                                   GA2KPGM 
00723               MOVE DFHBMUBF  TO  MAP-CODE-FUNCTION-ATTR(MAP-IDX)  GA2KPGM 
00724         ELSE                                                      GA2KPGM 
00725            IF GCVI-VALUE-NOT-LOADED                               GA2KPGM 
00726               IF EIBAID  =  DFHPF4 OR  =  DFHPF16                 GA2KPGM 
00727                  NEXT SENTENCE                                    GA2KPGM 
00728               ELSE                                                GA2KPGM 
00729                  MOVE DFHBMUBF  TO                                GA2KPGM 
00730                                 MAP-CODE-FUNCTION-ATTR (MAP-IDX)  GA2KPGM 
00731                  IF WS-ERROR-SW  NOT =  'Y'                       GA2KPGM 
00732                     MOVE 'Y'  TO  WS-ERROR-SW                     GA2KPGM 
00733                     MOVE -1  TO  MAP-CODE-FUNCTION-LEN (MAP-IDX)  GA2KPGM 
00734               MOVE 'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESS PGA2KPGM 
00735 -                  'F4/PF16 TO CONTINUE'  TO  MAP-ERROR-MESSAGE.  GA2KPGM 
00736                                                                   GA2KPGM 
00737      IF MAP-IDX   <  WS-MAP-ROW                                   GA2KPGM 
00738         SET MAP-IDX   UP BY  1                                    GA2KPGM 
00739         GO TO 2010-VALIDATE-ADD-ENTRIES.                          GA2KPGM 
00740 /                                                                 GA2KPGM 
00741 ****************************************************************  GA2KPGM 
00742 ** THIS GENERIC ROUTINE WILL EDIT THE PROCEDURE CODE. FIELDS  **  GA2KPGM 
00743 ** REQUIRING PROBABLE 'TAILORING' APPEAR AS -->NAME.          **  GA2KPGM 
00744 **                                                            **  GA2KPGM 
00745 ** REQUIRED WORKING-STORAGE:                                  **  GA2KPGM 
00746 **   1. 01  PROCED-KEY.                                       **  GA2KPGM 
00747 **          03  SYSTEM-INDICATOR          PIC X(1).           **  GA2KPGM 
00748 **          03  PROCED-CODE               PIC X(7).           **  GA2KPGM 
00749 **              04  PROCEDR-DIGIT REDEFINES PROCED-CODE       **  GA2KPGM 
00750 **                  OCCURS 7 TIMES        PIC X(1).           **  GA2KPGM 
00751 **                                                            **  GA2KPGM 
00752 **   2. 01  WS-PRO-HAF-COMM-LEN   PIC S9(4) COMP VALUE +344.  **  GA2KPGM 
00753 **                                                            **  GA2KPGM 
00754 **   3. COPY COBXIO.                                          **  GA2KPGM 
00755 ****************************************************************  GA2KPGM 
00756                                                                   GA2KPGM 
00757  2015-EDIT-PROCEDURE-ARGUMENT.                                    GA2KPGM 
00758                                                                   GA2KPGM 
00759 ***  MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2KPGM 
00760 ***                                   WS-SAVED-PROCEDURE-ARGUMENT.GA2KPGM 
00761                                                                   GA2KPGM 
00762      MOVE MAP-PROCEDURE-ARGUMENT (MAP-IDX)  TO                    GA2KPGM 
00763          WS-TEST-AREA.                                            GA2KPGM 
00764                                                                   GA2KPGM 
00765 ***  TRANSFORM WS-SAVED-PROCEDURE-ARGUMENT  FROM  QUOTES  TO  '\
00766 ***  TRANSFORM WS-SAVED-PROCEDURE-ARGUMENT  FROM                  GA2KPGM 
00767 ***                         WS-NON-SPECIAL-CHARACTERS  TO  QUOTES.GA2KPGM 
00768 ***  IF WS-SAVED-PROCEDURE-ARGUMENT  NOT =  QUOTES                GA2KPGM 
00769                                                                   GA2KPGM 
00770      IF  NOT WS-NON-SPECIAL-CHARACTERS  (1) AND                   GA2KPGM 
00771              WS-NON-SPECIAL-CHARACTERS  (2) AND                   GA2KPGM 
00772              WS-NON-SPECIAL-CHARACTERS  (3) AND                   GA2KPGM 
00773              WS-NON-SPECIAL-CHARACTERS  (4) AND                   GA2KPGM 
00774              WS-NON-SPECIAL-CHARACTERS  (5) AND                   GA2KPGM 
00775              WS-NON-SPECIAL-CHARACTERS  (6) AND                   GA2KPGM 
00776              WS-NON-SPECIAL-CHARACTERS  (7)                       GA2KPGM 
00777         MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR (MAP-IDX)  GA2KPGM 
00778         IF WS-ERROR-SW  NOT =  'Y'                                GA2KPGM 
00779            MOVE 'Y' TO  WS-ERROR-SW                               GA2KPGM 
00780            MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)      GA2KPGM 
00781            MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'   GA2KPGM 
00782                                             TO  MAP-ERROR-MESSAGE GA2KPGM 
00783            GO TO 2015-EXIT                                        GA2KPGM 
00784          ELSE                                                     GA2KPGM 
00785            GO TO 2015-EXIT.                                       GA2KPGM 
00786                                                                   GA2KPGM 
00788      EXEC CICS GETMAIN                                            GA2KPGM 
00789         SET(ADDRESS OF GCPPDIO-PARM-AREA)                         GA2KPGM 
00790         INITIMG(WS-HEX-00)                                        GA2KPGM 
00791         LENGTH(GCPPDIO-CA-LEN) END-EXEC.                          GA2KPGM 
00792                                                                   GA2KPGM 
00793 ***  SERVICE RELOAD GCPPDIO-PARM-AREA.                            GA2KPGM 
00794                                                                   GA2KPGM 
           MOVE MAP-PROCEDURE-ARGUMENT(MAP-IDX) TO PROCED-CODE.                 
                                                                                
00786 *** ICD-10 START                                                  GA2KPGM 
           IF WS-TEST-AREA (1:3) = 'BIT'                                        
              MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX)           
                                                                                
              IF WS-ERROR-SW  NOT =  'Y'                                        
                 MOVE 'Y' TO  WS-ERROR-SW                                       
                 MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)               
                 MOVE                                                           
           ' *** REQUESTED PCG BIT IS NOT ALLOWED FOR THIS TABULAR ***'         
                                                  TO  MAP-ERROR-MESSAGE         
              END-IF                                                            
                                                                                
              GO TO 2015-EXIT.                                                  
      *** ICD-10 END                                                            
                                                                                
           MOVE 'PRCDR03 '  TO  GCPPDIO-REQUEST-TYPE                            
                                                                                
           IF PROCEDR-DIGIT(5)  =  SPACE                                        
              MOVE  'H'  TO  SYSTEM-INDICATOR                                   
           ELSE                                                                 
      *** ICD-10 START                                                          
              IF PROCEDR-DIGIT(7)  =  SPACE                                     
                 MOVE  'C'  TO  SYSTEM-INDICATOR                                
              ELSE                                                              
                 MOVE  'Z'  TO  SYSTEM-INDICATOR                                
              END-IF                                                            
           END-IF                                                               
      *** ICD-10 END                                                            
      *    END-IF                                                               
                                                                                
           MOVE PROCED-KEY  TO  GCPPDIO-SERVICE-CODE-AREA.                      
                                                                                
00805      EXEC CICS LINK PROGRAM('GCPPDIO')                            GA2KPGM 
00806           COMMAREA(GCPPDIO-PARM-AREA)                             GA2KPGM 
00807           LENGTH(GCPPDIO-CA-LEN)                                  GA2KPGM 
00808      END-EXEC.                                                    GA2KPGM 
00809                                                                   GA2KPGM 
00810      IF GCPPDIO-SUCCESSFUL                                        GA2KPGM 
00811          GO TO 2015-EXIT.                                         GA2KPGM 
00812                                                                   GA2KPGM 
00813      IF NOT GCPPDIO-REC-NOT-FOUND                                 GA2KPGM 
00814          MOVE 'DEW1'  TO  WS-ABEND-CODE                           GA2KPGM 
00815          MOVE GCPPDIO-RETURN-MESSAGE TO MAP-ERROR-MESSAGE         GA2KPGM 
00816          PERFORM 9999-ERROR-MSG-THEN-ABEND.                       GA2KPGM 
00817                                                                   GA2KPGM 
00818      MOVE DFHBMUBF  TO  MAP-PROCEDURE-ARGUMENT-ATTR(MAP-IDX).     GA2KPGM 
00819                                                                   GA2KPGM 
00820      IF WS-ERROR-SW  NOT =  'Y'                                   GA2KPGM 
00821          MOVE 'Y' TO  WS-ERROR-SW                                 GA2KPGM 
00822          MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN(MAP-IDX)         GA2KPGM 
00823          MOVE '        *** PROCEDURE ARGUMENT IS INVALID ***'     GA2KPGM 
00824            TO  MAP-ERROR-MESSAGE.                                 GA2KPGM 
00825                                                                   GA2KPGM 
00826  2015-EXIT. EXIT.                                                 GA2KPGM 
00827 /                                                                 GA2KPGM 
00828  2020-CHECK-FOR-ERRORS.                                           GA2KPGM 
00829      MOVE '2020'  TO  WS-PARA-ID.                                 GA2KPGM 
00830      IF WS-ERROR-SW  =  'Y' OR  GCVI-TABLE-SW  =  'N'             GA2KPGM 
00831         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA2KPGM 
00832                              MAP-TITLE-LINE                       GA2KPGM 
00833                              MAP-SCREEN-ID                        GA2KPGM 
00834                              MAP-ALL-LEVEL-TAB-ID                 GA2KPGM 
00835                              MAP-ALL-LEVEL-TAB-SLOT               GA2KPGM 
00836                              MAP-FROM-MENU-ID                     GA2KPGM 
00837                              MAP-ID-LINE                          GA2KPGM 
00838         MOVE '2100'  TO  WS-PARA-ID                               GA2KPGM 
00839         PERFORM 2100-DONT-RETRANSMIT-FIELDS                       GA2KPGM 
00840            VARYING MAP-IDX  FROM  1  BY  1                        GA2KPGM 
00841               UNTIL MAP-IDX  >  WS-MAP-ROW                        GA2KPGM 
00842         MOVE '2020'  TO  WS-PARA-ID                               GA2KPGM 
00843         EXEC CICS SEND   MAP('GA2KI01') MAPSET('GA2KSET')         GA2KPGM 
00844            DATAONLY FROM(GA2KI01O) CURSOR END-EXEC                GA2KPGM 
00845         GO TO 2099-EXIT.                                          GA2KPGM 
00846                                                                   GA2KPGM 
00847      IF WS-ADD-COUNT  NOT >  ZERO                                 GA2KPGM 
00848         MOVE '                  *** ADD ENTRY NOT FOUND ***'      GA2KPGM 
00849            TO  MAP-ERROR-MESSAGE                                  GA2KPGM 
00850         SET MAP-IDX  TO  1                                        GA2KPGM 
00851         MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX)         GA2KPGM 
00852         MOVE LOW-VALUES  TO  MAP-FUNCTION-CODE                    GA2KPGM 
00853                              MAP-TITLE-LINE                       GA2KPGM 
00854                              MAP-SCREEN-ID                        GA2KPGM 
00855                              MAP-ALL-LEVEL-TAB-ID                 GA2KPGM 
00856                              MAP-ALL-LEVEL-TAB-SLOT               GA2KPGM 
00857                              MAP-FROM-MENU-ID                     GA2KPGM 
00858                              MAP-ID-LINE                          GA2KPGM 
00859         EXEC CICS SEND   MAP('GA2KI01') MAPSET('GA2KSET')         GA2KPGM 
00860            DATAONLY FROM(GA2KI01O) CURSOR END-EXEC                GA2KPGM 
00861         GO TO 2099-EXIT.                                          GA2KPGM 
00862                                                                   GA2KPGM 
00863  2025-CONTINUE-UPDATE.                                            GA2KPGM 
00864      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2KPGM 
00865               GC-GCIOPARM-LEN + GC-WORKFILE-KEY-LEN +             GA2KPGM 
00866                         GC-GCTABULR-ADOP-FIXED-LEN +              GA2KPGM 
00867           (GC-GCTABULR-ADOP-VARY-LEN *                            GA2KPGM 
00868           GC-GCTABULR-ADOP-VARY-MAX-OCUR).                        GA2KPGM 
00869                                                                   GA2KPGM 
00870 ***  EXEC CICS GETMAIN SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00) GA2KPGM 
00871      EXEC CICS GETMAIN                                            GA2KPGM 
00872         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2KPGM 
00873         INITIMG(WS-HEX-00)                                        GA2KPGM 
00874         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2KPGM 
00875 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2KPGM 
00876 ***  ADD ALL-LEVEL-TAB-PNTR, 4096  GIVING  ALL-LEVEL-TAB-PNTR2.   GA2KPGM 
00877                                                                   GA2KPGM 
00878      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2KPGM 
00879         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2KPGM 
00880         MOVE  'G'   TO  GCIO-WRK-STATUS-CODE                      GA2KPGM 
00881         MOVE  'G3'  TO  GCIO-WRK-RECORD-TYPE                      GA2KPGM 
00882         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2KPGM 
00883 *AB*****MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2KPGM 
00884         MOVE GRP-SPEC-GROUP-NO     TO GCIO-WRK-GROUP-NUM          GA2KPGM 
00885 *AB*****MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2KPGM 
00886         MOVE GRP-SPEC-SECTION-NO   TO GCIO-WRK-SECTION-NUM        GA2KPGM 
00887         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2KPGM 
00888         MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                    GA2KPGM 
00889                          GCIO-WRK-PROVIDER-CONTROL                GA2KPGM 
00890         MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2KPGM 
00891         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2KPGM 
00892         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-PROVISION-ID       GA2KPGM 
00893         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-PROVISION-SLOT-NOGA2KPGM 
00894         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2KPGM 
00895         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2KPGM 
00896                                                                   GA2KPGM 
00897      IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA2KPGM 
00898         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2KPGM 
00899         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2KPGM 
00900         MOVE  'C3'  TO  GCIO-WRK-RECORD-TYPE                      GA2KPGM 
00901         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2KPGM 
00902 *AB*****MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2KPGM 
00903         MOVE CONTRACT-GROUP-NO     TO GCIO-WRK-GROUP-NUM          GA2KPGM 
00904 *AB*****MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2KPGM 
00905         MOVE CONTRACT-SECTION-NO   TO GCIO-WRK-SECTION-NUM        GA2KPGM 
00906         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2KPGM 
00907         MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2KPGM 
00908         MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2KPGM 
00909         MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2KPGM 
00910         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2KPGM 
00911         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-PROVISION-ID       GA2KPGM 
00912         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-PROVISION-SLOT-NOGA2KPGM 
00913         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2KPGM 
00914         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2KPGM 
00915                                                                   GA2KPGM 
00916      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2KPGM 
00917         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2KPGM 
00918         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2KPGM 
00919         MOVE  'C5'  TO  GCIO-WRK-RECORD-TYPE                      GA2KPGM 
00920         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2KPGM 
00921 *AB*****MOVE GCA-GROUP-NO-1-3      TO GCIO-WRK-GROUP-NO-1-3       GA2KPGM 
00922         MOVE BEN-PROV-GROUP-NO     TO GCIO-WRK-GROUP-NUM          GA2KPGM 
00923 *AB*****MOVE GCA-SEC-NO-1          TO GCIO-WRK-SEC-NO-1           GA2KPGM 
00924         MOVE BEN-PROV-SECTION-NO   TO GCIO-WRK-SECTION-NUM        GA2KPGM 
00925         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2KPGM 
00926         MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS               GA2KPGM 
00927         MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL     GA2KPGM 
00928         MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL GA2KPGM 
00929         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2KPGM 
00930         MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID            GA2KPGM 
00931         MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO              GA2KPGM 
00932         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCIO-WRK-TAB-PROVISION-ID   GA2KPGM 
00933         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCIO-WRK-TAB-PROV-SLOT-NO.GA2KPGM 
00934                                                                   GA2KPGM 
00935      MOVE 'GCPSWORK'  TO  GCIO-FILE-DDNAME.                       GA2KPGM 
00936      MOVE 1  TO  GCIO-IO-AREA-TO-USE.                             GA2KPGM 
00937      MOVE GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                   GA2KPGM 
00938                                                                   GA2KPGM 
00939      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2KPGM 
00940      TO   GAH-ENTRY-COUNT.                                        GA2KPGM 
00941                                                                   GA2KPGM 
00942      MOVE  'RU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2KPGM 
00943                                                                   GA2KPGM 
00944      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2KPGM 
00945         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2KPGM 
00946         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2KPGM 
00947                                                                   GA2KPGM 
00948      IF  NOT GCIO-GOOD-RETURN                                     GA2KPGM 
00949         MOVE '*** ERROR READING ALL LEVEL TABULAR.  CONTACT SYSTEMGA2KPGM 
00950 -    'S AREA ***'  TO  MAP-ERROR-MESSAGE                          GA2KPGM 
00951         MOVE '2KF1'  TO  WS-ABEND-CODE                            GA2KPGM 
00952         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
00953                                                                   GA2KPGM 
00954      IF  WS-ADD-COUNT  NOT >  ZERO                                GA2KPGM 
00955         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2KPGM 
00956                                                                   GA2KPGM 
00957      SET WS-SORT-IDX  TO  1.                                      GA2KPGM 
00958      SET WS-SORT-IDX2  TO  2.                                     GA2KPGM 
00959      MOVE '2030'  TO  WS-PARA-ID.                                 GA2KPGM 
00960                                                                   GA2KPGM 
00961  2030-ONE-ENTRY-IN-RITE-SEQ.                                      GA2KPGM 
00962      IF WS-SORT-IDX2  >  WS-ADD-COUNT                             GA2KPGM 
00963         GO TO 2040-ARE-WE-DONE-WITH-SORT.                         GA2KPGM 
00964                                                                   GA2KPGM 
00965      IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) <                     GA2KPGM 
00966         WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)                      GA2KPGM 
00967         SET WS-SORT-IDX2  UP BY  1                                GA2KPGM 
00968         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2KPGM 
00969      ELSE                                                         GA2KPGM 
00970         IF WS-PROCEDURE-ARGUMENT (WS-SORT-IDX) >                  GA2KPGM 
00971                               WS-PROCEDURE-ARGUMENT (WS-SORT-IDX2)GA2KPGM 
00972            MOVE WS-SORTED-TAB (WS-SORT-IDX)  TO  WS-SAVED-FIELDS  GA2KPGM 
00973            MOVE WS-SORTED-TAB (WS-SORT-IDX2)  TO                  GA2KPGM 
00974                                         WS-SORTED-TAB(WS-SORT-IDX)GA2KPGM 
00975            MOVE WS-SAVED-FIELDS  TO  WS-SORTED-TAB(WS-SORT-IDX2)  GA2KPGM 
00976            SET WS-SORT-IDX2  UP BY  1                             GA2KPGM 
00977            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2KPGM 
00978                                                                   GA2KPGM 
00979      IF WS-CODE-FUNCTION (WS-SORT-IDX) <                          GA2KPGM 
00980                                    WS-CODE-FUNCTION (WS-SORT-IDX2)GA2KPGM 
00981         SET WS-SORT-IDX2  UP BY  1                                GA2KPGM 
00982         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ                          GA2KPGM 
00983      ELSE                                                         GA2KPGM 
00984         IF WS-CODE-FUNCTION (WS-SORT-IDX) >                       GA2KPGM 
00985                                    WS-CODE-FUNCTION (WS-SORT-IDX2)GA2KPGM 
00986            MOVE WS-SORTED-TAB(WS-SORT-IDX)  TO  WS-SAVED-FIELDS   GA2KPGM 
00987            MOVE WS-SORTED-TAB (WS-SORT-IDX2)  TO                  GA2KPGM 
00988                                        WS-SORTED-TAB (WS-SORT-IDX)GA2KPGM 
00989            MOVE WS-SAVED-FIELDS  TO  WS-SORTED-TAB (WS-SORT-IDX2) GA2KPGM 
00990            SET WS-SORT-IDX2  UP BY  1                             GA2KPGM 
00991            GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                      GA2KPGM 
00992                                                                   GA2KPGM 
00993      SET WS-SORT-IDX3  TO  WS-ADD-COUNT.                          GA2KPGM 
00994      MOVE WS-SORTED-TAB (WS-SORT-IDX3)  TO                        GA2KPGM 
00995                                    WS-SORTED-TAB (WS-SORT-IDX2).  GA2KPGM 
00996      SUBTRACT  1  FROM  WS-ADD-COUNT.                             GA2KPGM 
00997      GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                            GA2KPGM 
00998                                                                   GA2KPGM 
00999  2040-ARE-WE-DONE-WITH-SORT.                                      GA2KPGM 
01000      MOVE '2040'  TO  WS-PARA-ID.                                 GA2KPGM 
01001      SET WS-SORT-IDX  UP BY  1.                                   GA2KPGM 
01002      IF WS-SORT-IDX  <  WS-ADD-COUNT OR  =  WS-ADD-COUNT          GA2KPGM 
01003         SET WS-SORT-IDX2  TO  WS-SORT-IDX                         GA2KPGM 
01004         SET WS-SORT-IDX2  UP BY  1                                GA2KPGM 
01005         MOVE '2030'  TO  WS-PARA-ID                               GA2KPGM 
01006         GO TO 2030-ONE-ENTRY-IN-RITE-SEQ.                         GA2KPGM 
01007                                                                   GA2KPGM 
01008      SET WS-ADD-COUNT  TO  WS-SORT-IDX.                           GA2KPGM 
01009      MOVE HIGH-VALUES  TO  WS-PROCEDURE-ARGUMENT (WS-SORT-IDX)    GA2KPGM 
01010                            WS-CODE-FUNCTION (WS-SORT-IDX).        GA2KPGM 
01011      MOVE GAH-ENTRY-COUNT  TO  GAH-ENTRY-COUNT.                   GA2KPGM 
01012                                                                   GA2KPGM 
01013      COMPUTE  WS-COPY-LENGTH  =                                   GA2KPGM 
01014           GAH-ENTRY-COUNT  *  GC-GCTABULR-ADOP-VARY-LEN.          GA2KPGM 
01015                                                                   GA2KPGM 
01016 ***  EXEC CICS GETMAIN  SET(COPY-AREA-PNTR) LENGTH(WS-COPY-LENGTH)GA2KPGM 
01017      EXEC CICS GETMAIN                                            GA2KPGM 
01018         SET(ADDRESS OF COPY-OF-TABLE-AREA)                        GA2KPGM 
01019         LENGTH(WS-COPY-LENGTH)                                    GA2KPGM 
01020         INITIMG(WS-HEX-00) END-EXEC.                              GA2KPGM 
01021 ***  SERVICE RELOAD  COPY-OF-TABLE-AREA.                          GA2KPGM 
01022                                                                   GA2KPGM 
01023      SET COPY-IDX,  GAH-INDEX  TO 1.                              GA2KPGM 
01024                                                                   GA2KPGM 
01025      MOVE '2050'  TO  WS-PARA-ID.                                 GA2KPGM 
01026  2050-MAKE-A-COPY-OF-RECORD.                                      GA2KPGM 
01027      IF GAH-INDEX  NOT >  GAH-ENTRY-COUNT                         GA2KPGM 
01028         MOVE GAH-ENTRY (GAH-INDEX)  TO  COPY-OF-TABLE (COPY-IDX)  GA2KPGM 
01029         SET COPY-IDX, GAH-INDEX  UP BY  1                         GA2KPGM 
01030         GO TO 2050-MAKE-A-COPY-OF-RECORD.                         GA2KPGM 
01031                                                                   GA2KPGM 
01032      IF WS-ADD-COUNT  +  GAH-ENTRY-COUNT  >                       GA2KPGM 
01033         GC-GCTABULR-ADOP-VARY-MAX-OCUR                            GA2KPGM 
01034         MOVE '*** ERROR - PGM ABOUT TO EXCEED MAX RECORD SIZE.  PLGA2KPGM 
01035 -    'EASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE       GA2KPGM 
01036         MOVE '2KL1'  TO  WS-ABEND-CODE                            GA2KPGM 
01037         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01038                                                                   GA2KPGM 
01039      SET WS-SORT-IDX, COPY-IDX, GAH-INDEX  TO  1.                 GA2KPGM 
01040      MOVE '2060'  TO  WS-PARA-ID.                                 GA2KPGM 
01041  2060-MERGE-IN-NEW-ENTRIES.                                       GA2KPGM 
01042      IF WS-SORT-IDX  >  WS-ADD-COUNT                              GA2KPGM 
01043         SET GAH-INDEX  DOWN BY  1                                 GA2KPGM 
01044         SET GAH-ENTRY-COUNT  TO  GAH-INDEX                        GA2KPGM 
01045         MOVE GAH-ENTRY-COUNT  TO  GAH-ENTRY-COUNT                 GA2KPGM 
01046         GO TO 2090-UPDATE-ALL-LVL-TAB-REC.                        GA2KPGM 
01047                                                                   GA2KPGM 
01048      IF WS-SORTED-TAB (WS-SORT-IDX)  =  HIGH-VALUES  AND          GA2KPGM 
01049         COPY-OF-TABLE (COPY-IDX)  NOT  =  HIGH-VALUES             GA2KPGM 
01050         GO TO 2070-SAVE-COPIED-ENTRY.                             GA2KPGM 
01051                                                                   GA2KPGM 
01052      IF WS-SORTED-TAB (WS-SORT-IDX)  NOT  =  HIGH-VALUES  AND     GA2KPGM 
01053         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2KPGM 
01054         GO TO 2080-INSERT-NEW-ENTRY.                              GA2KPGM 
01055                                                                   GA2KPGM 
01056      IF WS-SORTED-TAB (WS-SORT-IDX)  =  HIGH-VALUES  AND          GA2KPGM 
01057         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2KPGM 
01058         NEXT SENTENCE                                             GA2KPGM 
01059      ELSE                                                         GA2KPGM 
01060         IF WS-PROCEDURE-ARGUMENT(WS-SORT-IDX)  >                  GA2KPGM 
01061                                 COPY-PROCEDURE-ARGUMENT (COPY-IDX)GA2KPGM 
01062            GO TO 2070-SAVE-COPIED-ENTRY                           GA2KPGM 
01063         ELSE                                                      GA2KPGM 
01064            IF WS-PROCEDURE-ARGUMENT(WS-SORT-IDX)  <               GA2KPGM 
01065                                 COPY-PROCEDURE-ARGUMENT (COPY-IDX)GA2KPGM 
01066               GO TO 2080-INSERT-NEW-ENTRY.                        GA2KPGM 
01067                                                                   GA2KPGM 
01068      IF WS-SORTED-TAB(WS-SORT-IDX)  =  HIGH-VALUES  AND           GA2KPGM 
01069         COPY-OF-TABLE (COPY-IDX)  =  HIGH-VALUES                  GA2KPGM 
01070         NEXT SENTENCE                                             GA2KPGM 
01071      ELSE                                                         GA2KPGM 
01072         IF WS-CODE-FUNCTION (WS-SORT-IDX)  >                      GA2KPGM 
01073                                       COPY-CODE-FUNCTION(COPY-IDX)GA2KPGM 
01074            GO TO 2070-SAVE-COPIED-ENTRY                           GA2KPGM 
01075         ELSE                                                      GA2KPGM 
01076            IF WS-CODE-FUNCTION (WS-SORT-IDX)  <                   GA2KPGM 
01077                                       COPY-CODE-FUNCTION(COPY-IDX)GA2KPGM 
01078               GO TO 2080-INSERT-NEW-ENTRY.                        GA2KPGM 
01079                                                                   GA2KPGM 
01080 ******************************************************************GA2KPGM 
01081 **   AT THIS POINT THE NEW ENTRY'S THREE FIELDS MUST BE EQUAL TO  GA2KPGM 
01082 **   THE OLD ENTRY, WE WILL DELETE THE NEW ENTRY BY INCREMENTING  GA2KPGM 
01083 **   THE INDEX FOR THE NEW ENTRY PAST THAT ONE ENTRY.  SAVE THE   GA2KPGM 
01084 **   ENTRY FROM THE COPY BECAUSE NEXT NEW ENTRY MUST BE GREATER.  GA2KPGM 
01085 ******************************************************************GA2KPGM 
01086                                                                   GA2KPGM 
01087      SET WS-SORT-IDX  UP BY  1.                                   GA2KPGM 
01088                                                                   GA2KPGM 
01089  2070-SAVE-COPIED-ENTRY.                                          GA2KPGM 
01090      MOVE '2070'  TO  WS-PARA-ID.                                 GA2KPGM 
01091      MOVE COPY-OF-TABLE (COPY-IDX)  TO  GAH-ENTRY (GAH-INDEX).    GA2KPGM 
01092                                                                   GA2KPGM 
01093      IF COPY-IDX  NOT >  GAH-ENTRY-COUNT                          GA2KPGM 
01094         SET COPY-IDX  UP BY  1                                    GA2KPGM 
01095         SET GAH-INDEX  UP BY  1                                   GA2KPGM 
01096         MOVE '2060'  TO  WS-PARA-ID                               GA2KPGM 
01097         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2KPGM 
01098      ELSE                                                         GA2KPGM 
01099         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2KPGM 
01100 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2KPGM 
01101         MOVE '2KL2'  TO  WS-ABEND-CODE                            GA2KPGM 
01102         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01103                                                                   GA2KPGM 
01104  2080-INSERT-NEW-ENTRY.                                           GA2KPGM 
01105      MOVE '2080'  TO  WS-PARA-ID.                                 GA2KPGM 
01106                                                                   GA2KPGM 
01107      MOVE WS-SORTED-TAB(WS-SORT-IDX)  TO  GAH-ENTRY (GAH-INDEX).  GA2KPGM 
01108                                                                   GA2KPGM 
01109      IF WS-SORT-IDX  NOT >  WS-ADD-COUNT                          GA2KPGM 
01110         SET WS-SORT-IDX  UP BY  1                                 GA2KPGM 
01111         SET GAH-INDEX  UP BY  1                                   GA2KPGM 
01112         MOVE '2060'  TO  WS-PARA-ID                               GA2KPGM 
01113         GO TO 2060-MERGE-IN-NEW-ENTRIES                           GA2KPGM 
01114      ELSE                                                         GA2KPGM 
01115         MOVE '*** ERROR - PGM SUBSCRIPT ABOUT TO EXCEED ITS MAX.  GA2KPGM 
01116 -    'PLEASE CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE     GA2KPGM 
01117         MOVE '2KL3'  TO  WS-ABEND-CODE                            GA2KPGM 
01118         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01119                                                                   GA2KPGM 
01120  2090-UPDATE-ALL-LVL-TAB-REC.                                     GA2KPGM 
01121      MOVE '2090'  TO  WS-PARA-ID.                                 GA2KPGM 
01122                                                                   GA2KPGM 
01123 *-- SET INDICATOR TO CAPTURE OPERATOR-ID.                         GA2KPGM 
01124                                                                   GA2KPGM 
01125      MOVE  '1'    TO  GCIO-OPER-ID-IND.                           GA2KPGM 
01126      MOVE  'WU '  TO  GCIO-FILE-ACCESS-CODE.                      GA2KPGM 
01127                                                                   GA2KPGM 
01128      COMPUTE  GCIO-RECORD-LENGTH  =   GC-WORKFILE-KEY-LEN        +GA2KPGM 
01129                     GC-GCTABULR-ADOP-FIXED-LEN +                  GA2KPGM 
01130              (GAH-ENTRY-COUNT  *                                  GA2KPGM 
01131              GC-GCTABULR-ADOP-VARY-LEN).                          GA2KPGM 
01132                                                                   GA2KPGM 
01133      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2KPGM 
01134            GC-GCIOPARM-LEN      +  GCIO-RECORD-LENGTH.            GA2KPGM 
01135                                                                   GA2KPGM 
01136      EXEC CICS LINK   PROGRAM('GCIOPGM')                          GA2KPGM 
01137         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2KPGM 
01138         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2KPGM 
01139                                                                   GA2KPGM 
01140      IF NOT GCIO-GOOD-RETURN                                      GA2KPGM 
01141         MOVE '*** ERROR REWRITING ALL LEVEL TABULAR RECORD.  PLEASGA2KPGM 
01142 -    'E CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE          GA2KPGM 
01143         MOVE '2KF2'  TO  WS-ABEND-CODE                            GA2KPGM 
01144         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01145                                                                   GA2KPGM 
01146      PERFORM 2100-DONT-RETRANSMIT-FIELDS                          GA2KPGM 
01147         VARYING MAP-IDX  FROM 1  BY  1                            GA2KPGM 
01148            UNTIL  MAP-IDX  >  WS-MAP-ROW.                         GA2KPGM 
01149                                                                   GA2KPGM 
01150                                                                   GA2KPGM 
01151 *--------- DARKEN SELECTION LINE 5 AND PAGING MESSAGE LINE 23.    GA2KPGM 
01152                                                                   GA2KPGM 
01153      MOVE DFHBMASD TO MAP-SELECT-LABEL-ATTR                       GA2KPGM 
01154                       MAP-SELECT-ATTR                             GA2KPGM 
01155                       MAP-SELECT-FROM-ATTR                        GA2KPGM 
01156                       MAP-SELECT-TO-LABEL-ATTR                    GA2KPGM 
01157                       MAP-SELECT-TO-ATTR                          GA2KPGM 
01158                       MAP-SELECT-OF-LABEL-ATTR                    GA2KPGM 
01159                       MAP-SELECT-OF-ATTR                          GA2KPGM 
01160                       MAP-SELECT-DISPLAY-LABEL-ATTR               GA2KPGM 
01161                       MAP-PAGING-LABEL-ATTR.                      GA2KPGM 
01162                                                                   GA2KPGM 
01163      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (1).                 GA2KPGM 
01164      EXEC CICS SEND   MAP('GA2KI01') MAPSET('GA2KSET') ERASE      GA2KPGM 
01165         FROM(GA2KI01O) CURSOR END-EXEC.                           GA2KPGM 
01166                                                                   GA2KPGM 
01167  2099-EXIT.   EXIT.                                               GA2KPGM 
01168 /                                                                 GA2KPGM 
01169 ******************************************************************GA2KPGM 
01170 **          D O N ' T   R E T R A N S M I T   F I E L D S         GA2KPGM 
01171 **                                                                GA2KPGM 
01172 **   WILL INSURE THAT WE DON'T RETRANSMIT BACK INFORMATION THAT ISGA2KPGM 
01173 **  ALREADY ON THE OPERATORS SCREEN.                              GA2KPGM 
01174 **                                                                GA2KPGM 
01175 ******************************************************************GA2KPGM 
01176  2100-DONT-RETRANSMIT-FIELDS SECTION.                             GA2KPGM 
01177                                                                   GA2KPGM 
01178      MOVE LOW-VALUES  TO  MAP-PROCEDURE-ARGUMENT (MAP-IDX)        GA2KPGM 
01179                           MAP-CODE-FUNCTION (MAP-IDX).            GA2KPGM 
01180                                                                   GA2KPGM 
01181  2199-EXIT.   EXIT.                                               GA2KPGM 
01182 /                                                                 GA2KPGM 
01183 ******************************************************************GA2KPGM 
01184 **          X C T L   T O   D E L   S C R E E N                   GA2KPGM 
01185 **                                                                GA2KPGM 
01186 **  THE OPERATOR WANTS TO SWITCH MODES, FROM ADDING ENTRIES TO    GA2KPGM 
01187 ** DELETING ENTRIES.  WE READ THE ALL LEVEL TABULAR RECORD & PASS GA2KPGM 
01188 ** THE ADDRESS OF THE I/O PARMS, WORKFILE KEY, & ALL LEVEL TABULARGA2KPGM 
01189 ** RECORD TO THE DELETE PROGRAM.  (DEPENDING ON THE MENU THE      GA2KPGM 
01190 ** PROGRAM ORIGINALLY CAME FROM, THE FIELDS ARE MOVED FROM THE    GA2KPGM 
01191 ** IDENTIFICATION LINE ON THE SCREEN TO THE RECORD).              GA2KPGM 
01192 ******************************************************************GA2KPGM 
01193  3000-XCTL-TO-DEL-SCREEN SECTION.                                 GA2KPGM 
01194      MOVE '3000'  TO  WS-PARA-ID.                                 GA2KPGM 
01195                                                                   GA2KPGM 
01196      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-TAB-LEN  =                   GA2KPGM 
01197            GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +   GA2KPGM 
01198            GC-GCTABULR-ADOP-FIXED-LEN +                           GA2KPGM 
01199           (GC-GCTABULR-ADOP-VARY-LEN    *                         GA2KPGM 
01200           GC-GCTABULR-ADOP-VARY-MAX-OCUR).                        GA2KPGM 
01201                                                                   GA2KPGM 
01202 ***  EXEC CICS GETMAIN  SET(ALL-LEVEL-TAB-PNTR) INITIMG(WS-HEX-00)GA2KPGM 
01203      EXEC CICS GETMAIN                                            GA2KPGM 
01204         SET(ADDRESS OF IO-PARM-ALL-LVL-TAB-RECORD)                GA2KPGM 
01205         INITIMG(WS-HEX-00)                                        GA2KPGM 
01206         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2KPGM 
01207 ***  SERVICE RELOAD  IO-PARM-ALL-LVL-TAB-RECORD.                  GA2KPGM 
01208 ***  ADD ALL-LEVEL-TAB-PNTR, 4096 GIVING  ALL-LEVEL-TAB-PNTR2.    GA2KPGM 
01209                                                                   GA2KPGM 
01210      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2KPGM 
01211                                                                   GA2KPGM 
01212 ***  EXEC CICS GETMAIN  SET(COMMAREA-PNTR) INITIMG(WS-HEX-00)     GA2KPGM 
01213 *    EXEC CICS GETMAIN                                            GA2KPGM 
01214 *       SET(ADDRESS OF GCA-COMMAREA)                              GA2KPGM 
01215 *       INITIMG(WS-HEX-00)                                        GA2KPGM 
01216 *       LENGTH(WS-COMMUNICATION-KEY-LEN) END-EXEC.                GA2KPGM 
01217 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2KPGM 
01218                                                                   GA2KPGM 
01219      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2KPGM 
01220         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2KPGM 
01221         MOVE  'G'   TO  GCIO-WRK-STATUS-CODE                      GA2KPGM 
01222         MOVE  'G3'  TO  GCIO-WRK-RECORD-TYPE                      GA2KPGM 
01223         MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE          GA2KPGM 
01224         MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM          GA2KPGM 
01225         MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM        GA2KPGM 
01226         MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE           GA2KPGM 
01227         MOVE SPACES  TO  GCA-L-O-B                                GA2KPGM 
01228                          GCA-PROV-CTL                             GA2KPGM 
01229                          GCA-BEN-PROV-ID                          GA2KPGM 
01230                          GCIO-WRK-LINE-OF-BUS                     GA2KPGM 
01231                          GCIO-WRK-PROVIDER-CONTROL                GA2KPGM 
01232         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2KPGM 
01233         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2KPGM 
01234         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2KPGM 
01235                            GCIO-WRK-PROVISION-ID                  GA2KPGM 
01236         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT    GA2KPGM 
01237                            GCIO-WRK-PROVISION-SLOT-NO             GA2KPGM 
01238         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2KPGM 
01239         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2KPGM 
01240                                                                   GA2KPGM 
01241      IF  MAP-FROM-MENU-ID  = 'GC4A' OR 'GTM1'                     GA2KPGM 
01242         MOVE SPACES TO GCIO-WORKFILE-KEY                          GA2KPGM 
01243         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2KPGM 
01244         MOVE  'C3'  TO  GCIO-WRK-RECORD-TYPE                      GA2KPGM 
01245         MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE          GA2KPGM 
01246         MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM          GA2KPGM 
01247         MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM        GA2KPGM 
01248         MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE           GA2KPGM 
01249         MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS        GA2KPGM 
01250         MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL   GA2KPGM 
01251         MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVLGA2KPGM 
01252         MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN          GA2KPGM 
01253         MOVE SPACES  TO  GCA-BEN-PROV-ID                          GA2KPGM 
01254         MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID        GA2KPGM 
01255                            GCIO-WRK-PROVISION-ID                  GA2KPGM 
01256         MOVE MAP-ALL-LEVEL-TAB-SLOT  TO GCA-ALL-LEVEL-TAB-SLOT    GA2KPGM 
01257                            GCIO-WRK-PROVISION-SLOT-NO             GA2KPGM 
01258         MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID                GA2KPGM 
01259         MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.               GA2KPGM 
01260                                                                   GA2KPGM 
01261      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2KPGM 
01262        MOVE SPACES TO GCIO-WORKFILE-KEY                           GA2KPGM 
01263         MOVE  'C'   TO  GCIO-WRK-STATUS-CODE                      GA2KPGM 
01264         MOVE  'C5'  TO  GCIO-WRK-RECORD-TYPE                      GA2KPGM 
01265        MOVE GCA-PLAN-CODE         TO GCIO-WRK-PLAN-CODE           GA2KPGM 
01266        MOVE GCA-GROUP-NUM         TO GCIO-WRK-GROUP-NUM           GA2KPGM 
01267        MOVE GCA-SECTION-NUM       TO GCIO-WRK-SECTION-NUM         GA2KPGM 
01268        MOVE GCA-PKG-CODE          TO GCIO-WRK-PKG-CODE            GA2KPGM 
01269        MOVE GCA-L-O-B             TO GCIO-WRK-LINE-OF-BUS         GA2KPGM 
01270        MOVE GCA-PROV-CTL          TO GCIO-WRK-PROVIDER-CONTROL    GA2KPGM 
01271        MOVE GCA-FAM-REL-LVL       TO GCIO-WRK-FAMILY-RELATION-LVL GA2KPGM 
01272        MOVE GCA-EFFDT-CEN         TO GCIO-WRK-EFFDT-CEN           GA2KPGM 
01273        MOVE GCA-BEN-PROV-ID       TO GCIO-WRK-PROVISION-ID        GA2KPGM 
01274        MOVE MAP-ALL-LEVEL-TAB-ID  TO GCA-ALL-LEVEL-TAB-ID         GA2KPGM 
01275                                      GCIO-WRK-TAB-PROVISION-ID    GA2KPGM 
01276        MOVE MAP-ALL-LEVEL-TAB-SLOT TO GCA-ALL-LEVEL-TAB-SLOT      GA2KPGM 
01277                                       GCIO-WRK-TAB-PROV-SLOT-NO   GA2KPGM 
01278        MOVE 9999999               TO GCIO-WRK-PROVISION-SLOT-NO.  GA2KPGM 
01279                                                                   GA2KPGM 
01280 *    MOVE WS-Y  TO  WS-YY.                                        GA2KPGM 
01281 *    IF  WS-M  >  2                                               GA2KPGM 
01282 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2KPGM 
01283 *          REMAINDER  WS-REMAINDER                                GA2KPGM 
01284 *    ELSE                                                         GA2KPGM 
01285 *       MOVE 1  TO  WS-REMAINDER.                                 GA2KPGM 
01286 *    SET WS-M-IDX  TO  WS-M.                                      GA2KPGM 
01287 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2KPGM 
01288 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2KPGM 
01289 *    IF  WS-REMAINDER  =  ZERO                                    GA2KPGM 
01290 *       ADD 1  TO  WS-DDD.                                        GA2KPGM 
01291                                                                   GA2KPGM 
01292 *    MOVE  WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE                  GA2KPGM 
01293 *                        GCA-EFF-DT.                              GA2KPGM 
01294      MOVE  'GCPSWORK'  TO  GCIO-FILE-DDNAME.                      GA2KPGM 
01295      MOVE  SPACES  TO  GCA-INTERNAL-TAB-ID,                       GA2KPGM 
01296                        GCA-INTERNAL-TAB-SLOT,                     GA2KPGM 
01297                        GCA-ADD-DEL-IND,                           GA2KPGM 
01298                        GCA-ALL-LEVEL-TAB-FUNC-CODE,               GA2KPGM 
01299                        GCA-OCCURS-ENTRY-COUNTER.                  GA2KPGM 
01300      MOVE  MAP-FROM-MENU-ID  TO GCA-FROM-MENU-ID.                 GA2KPGM 
01301      MOVE  GCIO-WORKFILE-KEY  TO  GCIO-FILE-KEY.                  GA2KPGM 
01302 ***  MOVE  ALL-LEVEL-TAB-PNTR TO GCA-RECORD-POINTER.              GA2KPGM 
01303                                                                   GA2KPGM 
01304      SET GCA-RECORD-POINTER TO ADDRESS                            GA2KPGM 
01305      OF  IO-PARM-ALL-LVL-TAB-RECORD.                              GA2KPGM 
01306                                                                   GA2KPGM 
01307      MOVE GC-GCTABULR-ACDR-VARY-MAX-OCUR                          GA2KPGM 
01308      TO   GAH-ENTRY-COUNT.                                        GA2KPGM 
01309                                                                   GA2KPGM 
01310      MOVE  'RD '  TO  GCIO-FILE-ACCESS-CODE.                      GA2KPGM 
01311                                                                   GA2KPGM 
01312      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2KPGM 
01313         COMMAREA(IO-PARM-ALL-LVL-TAB-RECORD)                      GA2KPGM 
01314         LENGTH(WS-IO-PARM-WRK-ALL-LVL-TAB-LEN) END-EXEC.          GA2KPGM 
01315                                                                   GA2KPGM 
01316      IF  NOT GCIO-GOOD-RETURN                                     GA2KPGM 
01317         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2KPGM 
01318 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2KPGM 
01319         MOVE '2KF3'  TO  WS-ABEND-CODE                            GA2KPGM 
01320         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01321                                                                   GA2KPGM 
01322 *    SET COMMAREA-PNTR TO ADDRESS                                 GA2KPGM 
01323 *    OF GCA-COMMAREA.                                             GA2KPGM 
01324                                                                   GA2KPGM 
01325 *    EXEC CICS XCTL  PROGRAM('GA1KPGM') COMMAREA(COMMAREA-PNTR)   GA2KPGM 
01326 *       LENGTH(4)  END-EXEC.                                      GA2KPGM 
01327      EXEC CICS XCTL  PROGRAM('GA1KPGM')                           GA2KPGM 
01328                      COMMAREA(DFHCOMMAREA)                        GA2KPGM 
01329                      LENGTH (LENGTH OF DFHCOMMAREA)               GA2KPGM 
01330      END-EXEC.                                                    GA2KPGM 
01331                                                                   GA2KPGM 
01332  3099-EXIT.   EXIT.                                               GA2KPGM 
01333 /                                                                 GA2KPGM 
01334 ***************************************************************** GA2KPGM 
01335 **          D I S P L A Y   F I R S T   S C R E E N               GA2KPGM 
01336 **                                                                GA2KPGM 
01337 **   THE OPERATOR HAS JUST REQUESTED THIS PROGRAM FROM THE DELETE GA2KPGM 
01338 ** PROGRAM, THAT PROGRAM WILL PASS THE ADDRESS OF A PARAMETER LISTGA2KPGM 
01339 ** CONTAINING THE FIELDS FROM THE HEADER OF THE SCREEN.           GA2KPGM 
01340 **   THIS SECTION MOVES ALL THE HEADER INFORMATION TO THE SCREEN, GA2KPGM 
01341 ** AND SENDS THE SCREEN IMAGE TO THE OPERATOR FOR THEIR           GA2KPGM 
01342 ** DETERMINATION OF APPROPRIATE ACTION.                           GA2KPGM 
01343 ******************************************************************GA2KPGM 
01344  4000-DISPLAY-FIRST-SCREEN SECTION.                               GA2KPGM 
01345      MOVE '4000'  TO  WS-PARA-ID.                                 GA2KPGM 
01346                                                                   GA2KPGM 
01347 ***  MOVE LOW-VALUES TO SCREEN                                    GA2KPGM 
01348      MOVE LOW-VALUES TO GA2KI01I.                                 GA2KPGM 
01349                                                                   GA2KPGM 
01350      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    GA2KPGM 
01351         MOVE '*** COMMONAREA LENGTH IS INVALID ***'               GA2KPGM 
01352            TO MAP-ERROR-MESSAGE                                   GA2KPGM 
01353         MOVE '2KC1'  TO  WS-ABEND-CODE                            GA2KPGM 
01354         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01355                                                                   GA2KPGM 
01356 ***  MOVE INCOMING-COMMAREA-PNTR  TO  COMMAREA-PNTR.              GA2KPGM 
01357 ***  SERVICE RELOAD  GCA-COMMAREA.                                GA2KPGM 
01358                                                                   GA2KPGM 
01359 *    SET ADDRESS OF GCA-COMMAREA                                  GA2KPGM 
01360 *    TO  INCOMING-COMMAREA-PNTR.                                  GA2KPGM 
01361                                                                   GA2KPGM 
01362      MOVE GCA-ALL-LEVEL-TAB-ID  TO  MAP-ALL-LEVEL-TAB-ID.         GA2KPGM 
01363      MOVE GCA-ALL-LEVEL-TAB-SLOT  TO  MAP-ALL-LEVEL-TAB-SLOT.     GA2KPGM 
01364      MOVE GCA-FROM-MENU-ID  TO  MAP-FROM-MENU-ID.                 GA2KPGM 
01365                                                                   GA2KPGM 
01366      IF  GCA-FROM-MENU-ID  =  'GS3A'                              GA2KPGM 
01367         MOVE GROUP-SPECIFIC-TITLE-LINE  TO  MAP-TITLE-LINE        GA2KPGM 
01368         MOVE 'PLN= '  TO  GRP-SPEC-PLAN-HEADING                   GA2KPGM 
01369         MOVE GCA-PLAN-CODE TO GRP-SPEC-PLAN-CODE                  GA2KPGM 
01370         MOVE ' GRP= '  TO  GRP-SPEC-GROUP-HEADING                 GA2KPGM 
01371         MOVE GCA-GROUP-NUM TO  GRP-SPEC-GROUP-NO                  GA2KPGM 
01372         MOVE ' SEC= '  TO  GRP-SPEC-SECTION-HEADING               GA2KPGM 
01373         MOVE GCA-SECTION-NUM TO  GRP-SPEC-SECTION-NO              GA2KPGM 
01374         MOVE ' PKG= '  TO  GRP-SPEC-PKG-HEADING                   GA2KPGM 
01375         MOVE GCA-PKG-CODE TO GRP-SPEC-PKG-CODE                    GA2KPGM 
01376         MOVE ' FR= '  TO  GRP-SPEC-FAM-REL-HEADING                GA2KPGM 
01377         MOVE GCA-FAM-REL-LVL  TO  GRP-SPEC-FAM-REL-LVL            GA2KPGM 
01378         MOVE ' EFDT= '  TO  GRP-SPEC-EFF-DT-HEADING               GA2KPGM 
01379         MOVE GCA-EFFECTIVE-DATE  TO  GRP-SPEC-EFF-DATE.           GA2KPGM 
01380                                                                   GA2KPGM 
01381      IF  GCA-FROM-MENU-ID  =  'GC4A' OR 'GTM1'                    GA2KPGM 
01382         MOVE CONTRACT-TITLE-LINE  TO  MAP-TITLE-LINE              GA2KPGM 
01383         MOVE 'PLN= '  TO  CONTRACT-PLAN-HEADING                   GA2KPGM 
01384         MOVE GCA-PLAN-CODE TO CONTRACT-PLAN-CODE                  GA2KPGM 
01385         MOVE ' GRP= '  TO  CONTRACT-GROUP-HEADING                 GA2KPGM 
01386         MOVE GCA-GROUP-NUM TO  CONTRACT-GROUP-NO                  GA2KPGM 
01387         MOVE ' SEC= '  TO  CONTRACT-SECTION-HEADING               GA2KPGM 
01388         MOVE GCA-SECTION-NUM TO  CONTRACT-SECTION-NO              GA2KPGM 
01389         MOVE ' PKG= '  TO  CONTRACT-PKG-HEADING                   GA2KPGM 
01390         MOVE GCA-PKG-CODE TO CONTRACT-PKG-CODE                    GA2KPGM 
01391         MOVE ' LOB= '  TO  CONTRACT-LOB-HEADING                   GA2KPGM 
01392         MOVE GCA-L-O-B  TO  CONTRACT-LOB                          GA2KPGM 
01393         MOVE ' PRV= '  TO  CONTRACT-PROV-CTL-HEADING              GA2KPGM 
01394         MOVE GCA-PROV-CTL  TO  CONTRACT-PROV-CTL                  GA2KPGM 
01395         MOVE ' FR= '  TO  CONTRACT-FAM-REL-HEADING                GA2KPGM 
01396         MOVE GCA-FAM-REL-LVL  TO  CONTRACT-FAM-REL-LVL            GA2KPGM 
01397         MOVE ' EFDT= '  TO  CONTRACT-EFF-DT-HEADING               GA2KPGM 
01398         MOVE GCA-EFFECTIVE-DATE  TO  CONTRACT-EFF-DATE.           GA2KPGM 
01399                                                                   GA2KPGM 
01400      IF  GCA-FROM-MENU-ID  =  'GC8A'                              GA2KPGM 
01401         MOVE BENEFIT-PROVISION-TITLE-LINE  TO  MAP-TITLE-LINE     GA2KPGM 
01402         MOVE ' PLN'  TO  BEN-PROV-PLAN-HEADING                    GA2KPGM 
01403         MOVE GCA-PLAN-CODE TO BEN-PROV-PLAN-CODE                  GA2KPGM 
01404         MOVE ' GRP'  TO  BEN-PROV-GROUP-HEADING                   GA2KPGM 
01405         MOVE GCA-GROUP-NUM TO  BEN-PROV-GROUP-NO                  GA2KPGM 
01406         MOVE ' SEC'  TO  BEN-PROV-SECTION-HEADING                 GA2KPGM 
01407         MOVE GCA-SECTION-NUM TO  BEN-PROV-SECTION-NO              GA2KPGM 
01408         MOVE ' PKG'  TO  BEN-PROV-PKG-HEADING                     GA2KPGM 
01409         MOVE GCA-PKG-CODE TO BEN-PROV-PKG-CODE                    GA2KPGM 
01410         MOVE ' LOB'  TO  BEN-PROV-LOB-HEADING                     GA2KPGM 
01411         MOVE GCA-L-O-B  TO  BEN-PROV-LOB                          GA2KPGM 
01412         MOVE ' PRV'  TO  BEN-PROV-PROV-CTL-HEADING                GA2KPGM 
01413         MOVE GCA-PROV-CTL  TO  BEN-PROV-PROV-CTL                  GA2KPGM 
01414         MOVE ' FR'  TO  BEN-PROV-FAM-REL-HEADING                  GA2KPGM 
01415         MOVE GCA-FAM-REL-LVL  TO  BEN-PROV-FAM-REL-LVL            GA2KPGM 
01416         MOVE ' EFDT'  TO  BEN-PROV-EFF-DT-HEADING                 GA2KPGM 
01417         MOVE GCA-EFFECTIVE-DATE  TO  BEN-PROV-EFF-DATE            GA2KPGM 
01418         MOVE ' BPVID'  TO  BEN-PROV-ID-HEADING                    GA2KPGM 
01419         MOVE GCA-BEN-PROV-ID  TO  BEN-PROV-ID-NO.                 GA2KPGM 
01420                                                                   GA2KPGM 
01421                                                                   GA2KPGM 
01422 *--------- DARKEN SELECTION LINE 5 AND PAGING MESSAGE LINE 23.    GA2KPGM 
01423                                                                   GA2KPGM 
01424      MOVE DFHBMASD TO MAP-SELECT-LABEL-ATTR                       GA2KPGM 
01425                       MAP-SELECT-ATTR                             GA2KPGM 
01426                       MAP-SELECT-FROM-ATTR                        GA2KPGM 
01427                       MAP-SELECT-TO-LABEL-ATTR                    GA2KPGM 
01428                       MAP-SELECT-TO-ATTR                          GA2KPGM 
01429                       MAP-SELECT-OF-LABEL-ATTR                    GA2KPGM 
01430                       MAP-SELECT-OF-ATTR                          GA2KPGM 
01431                       MAP-SELECT-DISPLAY-LABEL-ATTR               GA2KPGM 
01432                       MAP-PAGING-LABEL-ATTR.                      GA2KPGM 
01433                                                                   GA2KPGM 
01434      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (1).                 GA2KPGM 
01435      EXEC CICS SEND   MAP('GA2KI01') MAPSET('GA2KSET') ERASE      GA2KPGM 
01436         FROM(GA2KI01O) CURSOR END-EXEC.                           GA2KPGM 
01437                                                                   GA2KPGM 
01438  4099-EXIT.   EXIT.                                               GA2KPGM 
01439 /                                                                 GA2KPGM 
01440 ***************************************************************** GA2KPGM 
01441 **        X C T L   T O   P R E V I O U S   M E N U               GA2KPGM 
01442 **                                                                GA2KPGM 
01443 **  THE OPERATOR WANTS TO RETURN TO THE SCREEN THIS PROGRAM       GA2KPGM 
01444 ** ORIGINATED FROM.  WE READ THE ALL LEVEL TABULAR RECORD &       GA2KPGM 
01445 ** PASS IT PRECEEDED BY THE WORKFILE KEY TO THE CORRECT           GA2KPGM 
01446 ** ORIGINATING PROGRAM (DETERMINED BY THE CODE IN THE 'FROM       GA2KPGM 
01447 ** MENU ID' FIELD).                                               GA2KPGM 
01448 ******************************************************************GA2KPGM 
01449  5000-XCTL-TO-PREVIOUS-MENU SECTION.                              GA2KPGM 
01450      MOVE '5000'  TO  WS-PARA-ID.                                 GA2KPGM 
01451                                                                   GA2KPGM 
01452      IF  MAP-FROM-MENU-ID  = 'GS3A'                               GA2KPGM 
01453         GO TO 5010-XCTL-TO-GRP-SPEC-MENU.                         GA2KPGM 
01454                                                                   GA2KPGM 
01455      IF  MAP-FROM-MENU-ID  = 'GC4A'                               GA2KPGM 
01456         GO TO 5020-XCTL-TO-CONTRACT-MENU.                         GA2KPGM 
01457                                                                   GA2KPGM 
01458      IF  MAP-FROM-MENU-ID  = 'GC8A'                               GA2KPGM 
01459         GO TO 5030-XCTL-TO-BEN-PROV-MENU.                         GA2KPGM 
01460                                                                   GA2KPGM 
01461      IF  MAP-FROM-MENU-ID  = 'GTM1'                               GA2KPGM 
01462         GO TO 5040-XCTL-TO-SINGLE-TAB-MENU.                       GA2KPGM 
01463                                                                   GA2KPGM 
01464                                                                   GA2KPGM 
01465  5010-XCTL-TO-GRP-SPEC-MENU.                                      GA2KPGM 
01466      MOVE '5010'  TO  WS-PARA-ID.                                 GA2KPGM 
01467                                                                   GA2KPGM 
01468      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2KPGM 
01469          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2KPGM 
01470                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2KPGM 
01471                                                                   GA2KPGM 
01472 ***  EXEC CICS GETMAIN  SET(GRP-SPEC-PNTR) INITIMG(WS-HEX-00)     GA2KPGM 
01473      EXEC CICS GETMAIN                                            GA2KPGM 
01474         SET(ADDRESS OF IO-PARM-GRP-SPEC-RECORD)                   GA2KPGM 
01475         INITIMG(WS-HEX-00)                                        GA2KPGM 
01476         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01477 ***  SERVICE RELOAD  IO-PARM-GRP-SPEC-RECORD.                     GA2KPGM 
01478                                                                   GA2KPGM 
01479      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2KPGM 
01480                                                                   GA2KPGM 
01481      MOVE 'G'   TO  GCIO-WRK-STATUS-CODE.                         GA2KPGM 
01482      MOVE 'G2'  TO  GCIO-WRK-RECORD-TYPE.                         GA2KPGM 
01483      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2KPGM 
01484      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2KPGM 
01485      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2KPGM 
01486      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2KPGM 
01487      MOVE SPACES  TO  GCIO-WRK-LINE-OF-BUS,                       GA2KPGM 
01488                       GCIO-WRK-PROVIDER-CONTROL.                  GA2KPGM 
01489      MOVE GRP-SPEC-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2KPGM 
01490      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2KPGM 
01491                                                                   GA2KPGM 
01492 *    MOVE WS-Y  TO  WS-YY.                                        GA2KPGM 
01493 *    IF  WS-M  >  2                                               GA2KPGM 
01494 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2KPGM 
01495 *          REMAINDER  WS-REMAINDER                                GA2KPGM 
01496 *    ELSE                                                         GA2KPGM 
01497 *       MOVE 1  TO  WS-REMAINDER.                                 GA2KPGM 
01498 *    SET WS-M-IDX  TO  WS-M.                                      GA2KPGM 
01499 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2KPGM 
01500 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2KPGM 
01501 *    IF  WS-REMAINDER  =  ZERO                                    GA2KPGM 
01502 *       ADD 1  TO  WS-DDD.                                        GA2KPGM 
01503                                                                   GA2KPGM 
01504 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2KPGM 
01505      MOVE 'GCPSWORK'  TO  GCIO2-FILE-DDNAME.                      GA2KPGM 
01506      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2KPGM 
01507                       GCIO-WRK-TAB-PROVISION-ID.                  GA2KPGM 
01508      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2KPGM 
01509                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2KPGM 
01510      MOVE GCIO-WORKFILE-KEY  TO  GCIO2-FILE-KEY.                  GA2KPGM 
01511                                                                   GA2KPGM 
01512      MOVE GC-GCGRPSPC-VARY-MAX-OCUR                               GA2KPGM 
01513      TO   GCG-COUNT-TAB-PROVN-POINTERS.                           GA2KPGM 
01514                                                                   GA2KPGM 
01515      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      GA2KPGM 
01516                                                                   GA2KPGM 
01517      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2KPGM 
01518         COMMAREA(IO-PARM-GRP-SPEC-RECORD)                         GA2KPGM 
01519         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01520                                                                   GA2KPGM 
01521      IF  NOT GCIO2-GOOD-RETURN                                    GA2KPGM 
01522         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2KPGM 
01523 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2KPGM 
01524         MOVE '2KF4'  TO  WS-ABEND-CODE                            GA2KPGM 
01525         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01526                                                                   GA2KPGM 
01527      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2KPGM 
01528          GC-WORKFILE-KEY-LEN        +                             GA2KPGM 
01529                 GC-GCGRPSPC-MAX-REC-LEN.                          GA2KPGM 
01530                                                                   GA2KPGM 
01531      EXEC CICS XCTL PROGRAM('GS3APGM') COMMAREA(WORK-RECORD-2)    GA2KPGM 
01532         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01533                                                                   GA2KPGM 
01534      GO TO 5099-EXIT.                                             GA2KPGM 
01535                                                                   GA2KPGM 
01536  5020-XCTL-TO-CONTRACT-MENU.                                      GA2KPGM 
01537      MOVE '5020'  TO  WS-PARA-ID.                                 GA2KPGM 
01538                                                                   GA2KPGM 
01539      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2KPGM 
01540          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2KPGM 
01541                 GC-GCCONTR-MAX-REC-LEN.                           GA2KPGM 
01542                                                                   GA2KPGM 
01543 ***  EXEC CICS GETMAIN  SET(CONTRACT-PNTR) INITIMG(WS-HEX-00)     GA2KPGM 
01544      EXEC CICS GETMAIN                                            GA2KPGM 
01545         SET(ADDRESS OF IO-PARM-CONTRACT-RECORD)                   GA2KPGM 
01546         INITIMG(WS-HEX-00)                                        GA2KPGM 
01547         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01548 ***  SERVICE RELOAD  IO-PARM-CONTRACT-RECORD.                     GA2KPGM 
01549 ***  ADD  CONTRACT-PNTR,  4096  GIVING  CONTRACT-PNTR2.           GA2KPGM 
01550                                                                   GA2KPGM 
01551      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2KPGM 
01552                                                                   GA2KPGM 
01553      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2KPGM 
01554      MOVE 'C2'  TO  GCIO-WRK-RECORD-TYPE.                         GA2KPGM 
01555      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2KPGM 
01556      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2KPGM 
01557      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2KPGM 
01558      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2KPGM 
01559      MOVE CONTRACT-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2KPGM 
01560      MOVE CONTRACT-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2KPGM 
01561      MOVE CONTRACT-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2KPGM 
01562      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2KPGM 
01563                                                                   GA2KPGM 
01564 *    MOVE WS-Y  TO  WS-YY.                                        GA2KPGM 
01565 *    IF  WS-M  >  2                                               GA2KPGM 
01566 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2KPGM 
01567 *          REMAINDER  WS-REMAINDER                                GA2KPGM 
01568 *    ELSE                                                         GA2KPGM 
01569 *       MOVE 1  TO  WS-REMAINDER.                                 GA2KPGM 
01570 *    SET WS-M-IDX  TO  WS-M.                                      GA2KPGM 
01571 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2KPGM 
01572 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2KPGM 
01573 *    IF  WS-REMAINDER  =  ZERO                                    GA2KPGM 
01574 *       ADD 1  TO  WS-DDD.                                        GA2KPGM 
01575 *                                                                 GA2KPGM 
01576 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2KPGM 
01577      MOVE 'GCPSWORK'  TO  GCIO3-FILE-DDNAME.                      GA2KPGM 
01578      MOVE SPACES  TO  GCIO-WRK-PROVISION-ID,                      GA2KPGM 
01579                       GCIO-WRK-TAB-PROVISION-ID.                  GA2KPGM 
01580      MOVE ZEROES  TO  GCIO-WRK-PROVISION-SLOT-NO,                 GA2KPGM 
01581                       GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2KPGM 
01582      MOVE GCIO-WORKFILE-KEY  TO  GCIO3-FILE-KEY.                  GA2KPGM 
01583                                                                   GA2KPGM 
01584      MOVE GC-GCCONTR-VARY-MAX-OCUR                                GA2KPGM 
01585      TO   GCT-COUNT-BEN-PROVN-POINTERS.                           GA2KPGM 
01586                                                                   GA2KPGM 
01587      MOVE 'RD '  TO  GCIO3-FILE-ACCESS-CODE.                      GA2KPGM 
01588                                                                   GA2KPGM 
01589      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2KPGM 
01590         COMMAREA(IO-PARM-CONTRACT-RECORD)                         GA2KPGM 
01591         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01592                                                                   GA2KPGM 
01593      IF  NOT GCIO3-GOOD-RETURN                                    GA2KPGM 
01594         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2KPGM 
01595 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2KPGM 
01596         MOVE '2KF5'  TO  WS-ABEND-CODE                            GA2KPGM 
01597         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01598                                                                   GA2KPGM 
01599      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2KPGM 
01600          GC-WORKFILE-KEY-LEN        +                             GA2KPGM 
01601                 GC-GCCONTR-MAX-REC-LEN.                           GA2KPGM 
01602                                                                   GA2KPGM 
01603      EXEC CICS XCTL PROGRAM('GC4APGM') COMMAREA(WORK-RECORD-3)    GA2KPGM 
01604         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01605                                                                   GA2KPGM 
01606      GO TO 5099-EXIT.                                             GA2KPGM 
01607                                                                   GA2KPGM 
01608  5030-XCTL-TO-BEN-PROV-MENU.                                      GA2KPGM 
01609      MOVE '5030'  TO  WS-PARA-ID.                                 GA2KPGM 
01610                                                                   GA2KPGM 
01611      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2KPGM 
01612          GC-GCIOPARM-LEN      +  GC-WORKFILE-KEY-LEN        +     GA2KPGM 
01613                 GC-GCBENPRV-MAX-REC-LEN.                          GA2KPGM 
01614                                                                   GA2KPGM 
01615 ***  EXEC CICS GETMAIN  SET(BEN-PROV-PNTR) INITIMG(WS-HEX-00)     GA2KPGM 
01616      EXEC CICS GETMAIN                                            GA2KPGM 
01617         SET(ADDRESS OF IO-PARM-BEN-PROV-RECORD)                   GA2KPGM 
01618         INITIMG(WS-HEX-00)                                        GA2KPGM 
01619         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01620 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-RECORD.                     GA2KPGM 
01621                                                                   GA2KPGM 
01622      MOVE SPACES      TO  GCIO-WORKFILE-KEY.                      GA2KPGM 
01623                                                                   GA2KPGM 
01624      MOVE 'C'   TO  GCIO-WRK-STATUS-CODE.                         GA2KPGM 
01625      MOVE 'C4'  TO  GCIO-WRK-RECORD-TYPE.                         GA2KPGM 
01626      MOVE GCA-PLAN-CODE        TO GCIO-WRK-PLAN-CODE.             GA2KPGM 
01627      MOVE GCA-GROUP-NUM        TO GCIO-WRK-GROUP-NUM.             GA2KPGM 
01628      MOVE GCA-SECTION-NUM      TO GCIO-WRK-SECTION-NUM.           GA2KPGM 
01629      MOVE GCA-PKG-CODE         TO GCIO-WRK-PKG-CODE.              GA2KPGM 
01630      MOVE BEN-PROV-LOB  TO  GCIO-WRK-LINE-OF-BUS.                 GA2KPGM 
01631      MOVE BEN-PROV-PROV-CTL  TO  GCIO-WRK-PROVIDER-CONTROL.       GA2KPGM 
01632      MOVE BEN-PROV-FAM-REL-LVL TO GCIO-WRK-FAMILY-RELATION-LVL.   GA2KPGM 
01633      MOVE BEN-PROV-EFF-DATE  TO  WS-MDY.                          GA2KPGM 
01634      MOVE BEN-PROV-ID-NO  TO  GCIO-WRK-PROVISION-ID.              GA2KPGM 
01635      MOVE GCA-EFFDT-CEN        TO GCIO-WRK-EFFDT-CEN.             GA2KPGM 
01636 *    MOVE WS-Y  TO  WS-YY.                                        GA2KPGM 
01637 *    IF  WS-M  >  2                                               GA2KPGM 
01638 *       DIVIDE WS-Y  BY 4  GIVING  WS-QUOTIENT                    GA2KPGM 
01639 *          REMAINDER  WS-REMAINDER                                GA2KPGM 
01640 *    ELSE                                                         GA2KPGM 
01641 *       MOVE 1  TO  WS-REMAINDER.                                 GA2KPGM 
01642 *    SET WS-M-IDX  TO  WS-M.                                      GA2KPGM 
01643 *    MOVE WS-MONTH-TABLE (WS-M-IDX)  TO  WS-DDD.                  GA2KPGM 
01644 *    COMPUTE WS-DDD  =  WS-DDD  +  WS-D.                          GA2KPGM 
01645 *    IF  WS-REMAINDER  =  ZERO                                    GA2KPGM 
01646 *       ADD 1  TO  WS-DDD.                                        GA2KPGM 
01647                                                                   GA2KPGM 
01648 *    MOVE WS-YYDDD  TO  GCIO-WRK-EFFECTIVE-DATE.                  GA2KPGM 
01649      MOVE 'GCPSWORK'  TO  GCIO4-FILE-DDNAME.                      GA2KPGM 
01650      MOVE 9999999  TO  GCIO-WRK-PROVISION-SLOT-NO.                GA2KPGM 
01651      MOVE SPACES  TO  GCIO-WRK-TAB-PROVISION-ID.                  GA2KPGM 
01652      MOVE ZEROES  TO  GCIO-WRK-TAB-PROV-SLOT-NO.                  GA2KPGM 
01653      MOVE GCIO-WORKFILE-KEY  TO  GCIO4-FILE-KEY.                  GA2KPGM 
01654                                                                   GA2KPGM 
01655      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               GA2KPGM 
01656      TO   GCP-COUNT-TAB-PROVN-POINTERS.                           GA2KPGM 
01657                                                                   GA2KPGM 
01658      MOVE 'RD '  TO  GCIO4-FILE-ACCESS-CODE.                      GA2KPGM 
01659                                                                   GA2KPGM 
01660      EXEC CICS LINK  PROGRAM('GCIOPGM')                           GA2KPGM 
01661         COMMAREA(IO-PARM-BEN-PROV-RECORD)                         GA2KPGM 
01662         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01663                                                                   GA2KPGM 
01664      IF  NOT GCIO4-GOOD-RETURN                                    GA2KPGM 
01665         MOVE '*** ERROR READING ALL LEVEL TABULAR RECORD.  PLEASE GA2KPGM 
01666 -    'CONTACT SYSTEMS AREA ***'  TO  MAP-ERROR-MESSAGE            GA2KPGM 
01667         MOVE '2KF6'  TO  WS-ABEND-CODE                            GA2KPGM 
01668         PERFORM 9999-ERROR-MSG-THEN-ABEND.                        GA2KPGM 
01669                                                                   GA2KPGM 
01670      COMPUTE  WS-XCTL-WRK-LEN  =                                  GA2KPGM 
01671          GC-WORKFILE-KEY-LEN        +                             GA2KPGM 
01672                 GC-GCBENPRV-MAX-REC-LEN.                          GA2KPGM 
01673                                                                   GA2KPGM 
01674      EXEC CICS XCTL PROGRAM('GC8APGM') COMMAREA(WORK-RECORD-4)    GA2KPGM 
01675         LENGTH(WS-XCTL-WRK-LEN) END-EXEC.                         GA2KPGM 
01676                                                                   GA2KPGM 
01677      GO TO 5099-EXIT.                                             GA2KPGM 
01678                                                                   GA2KPGM 
01679  5040-XCTL-TO-SINGLE-TAB-MENU.                                    GA2KPGM 
01680      MOVE '5040'  TO  WS-PARA-ID.                                 GA2KPGM 
01681                                                                   GA2KPGM 
01682      EXEC CICS XCTL                                               GA2KPGM 
01683                PROGRAM('GTM1PGM')                                 GA2KPGM 
01684                END-EXEC.                                          GA2KPGM 
01685                                                                   GA2KPGM 
01686      GO  TO  5099-EXIT.                                           GA2KPGM 
01687                                                                   GA2KPGM 
01688                                                                   GA2KPGM 
01689  5099-EXIT.         EXIT.                                         GA2KPGM 
01690 /                                                                 GA2KPGM 
01691 ***************************************************************** GA2KPGM 
01692 **           X C T L   T O   M A I N   M E N U                    GA2KPGM 
01693 **                                                                GA2KPGM 
01694 **   THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO    GA2KPGM 
01695 ** OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE GA2KPGM 
01696 ** XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE      GA2KPGM 
01697 ** PROPER MENUS TO GET TO US.  WE ARE A MODULE AT THE BOTTOM OF A GA2KPGM 
01698 ** PYRAMID; TO GET HERE YOU MUST START AT THE TOP (THE MAIN MENU),GA2KPGM 
01699 ** AND PROGRESS DOWN.                                             GA2KPGM 
01700 ******************************************************************GA2KPGM 
01701  6000-XCTL-TO-MAIN-MENU SECTION.                                  GA2KPGM 
01702      MOVE '6000'  TO  WS-PARA-ID.                                 GA2KPGM 
01703      MOVE '2KP1'  TO  WS-ABEND-CODE.                              GA2KPGM 
01704                                                                   GA2KPGM 
01705      EXEC CICS XCTL   PROGRAM('GCPSPGM') END-EXEC.                GA2KPGM 
01706                                                                   GA2KPGM 
01707  6099-EXIT.     EXIT.                                             GA2KPGM 
01708 /                                                                 GA2KPGM 
01709  7099-EXIT.     EXIT.                                             GA2KPGM 
01710 /      E R R O R   M E S S A G E   T H E N   A B E N D            GA2KPGM 
01711 ******************************************************************GA2KPGM 
01712  9999-ERROR-MSG-THEN-ABEND SECTION.                               GA2KPGM 
01713                                                                   GA2KPGM 
01714      SET MAP-IDX   TO  7.                                         GA2KPGM 
01715      MOVE -1  TO  MAP-PROCEDURE-ARGUMENT-LEN (MAP-IDX).           GA2KPGM 
01716                                                                   GA2KPGM 
01717 *--------- DARKEN SELECTION LINE 5 AND PAGING MESSAGE LINE 23.    GA2KPGM 
01718                                                                   GA2KPGM 
01719      MOVE DFHBMASD TO MAP-SELECT-LABEL-ATTR                       GA2KPGM 
01720                       MAP-SELECT-ATTR                             GA2KPGM 
01721                       MAP-SELECT-FROM-ATTR                        GA2KPGM 
01722                       MAP-SELECT-TO-LABEL-ATTR                    GA2KPGM 
01723                       MAP-SELECT-TO-ATTR                          GA2KPGM 
01724                       MAP-SELECT-OF-LABEL-ATTR                    GA2KPGM 
01725                       MAP-SELECT-OF-ATTR                          GA2KPGM 
01726                       MAP-SELECT-DISPLAY-LABEL-ATTR               GA2KPGM 
01727                       MAP-PAGING-LABEL-ATTR.                      GA2KPGM 
01728                                                                   GA2KPGM 
01729      EXEC CICS SEND   MAP('GA2KI01') MAPSET('GA2KSET') ERASE      GA2KPGM 
01730         FROM(GA2KI01O) CURSOR WAIT END-EXEC.                      GA2KPGM 
01731                                                                   GA2KPGM 
01732      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            GA2KPGM 
01733                                                                   GA2KPGM 
01734  9999-EXIT.     EXIT.                                             GA2KPGM 
