00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7B1PGM.                                         G7B1PGM 
00003 *** THIS IS A COBOL II PROGRAM                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7B1PGM 
00005  DATE-WRITTEN.   03/09/87.                                        G7B1PGM 
00006  DATE-COMPILED.                                                   G7B1PGM 
00007 ***************************************************************** G7B1PGM 
00008 *                                                               * G7B1PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7B1PGM 
00010 *                                                               * G7B1PGM 
00011 *                                                               * G7B1PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7B1PGM 
00013 *                                                               * G7B1PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7B1PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7B1PGM 
00016 *                             (SAME AS 'GC5A')                  * G7B1PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7B1PGM 
00018 *                              'GC5A')                          * G7B1PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7B1PGM 
00020 *                              PROVISION)                       * G7B1PGM 
00021 *                                                               * G7B1PGM 
00022 *  D116       7-15-87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7B1PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7B1PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7B1PGM 
00025 *                                                               * G7B1PGM 
00026 *  D12009    10-01-91  GDM 1. CONVERT TO COBOL II               * G7B1PGM 
00027 *                          2. REMOVE PF12 LOGIC                 * G7B1PGM 
00028 *                                                               * G7B1PGM 
00029 *  D14726    10/28/97  GDM 1. ADDED MILLENNIUM PROCESSING FOR   * G7B1PGM 
00030 *                             DATE                              * G7B1PGM 
00031 *                          2. EXPAND THE COMMAREA KEY TO        * G7B1PGM 
00032 *                             SUPPORT THE TEXAS MERGER.         * G7B1PGM 
00033 *                                                               * G7B1PGM 
00034 * 14726/     03/27/98  GSP    ADDED PLAN AND PACKAGE CODE AND   * G7B1PGM 
00035 * 15057                       INCREASED GROUP AND SECTION ON    * G7B1PGM 
00036 *                             THE SCREEN.                       * G7B1PGM 
00037 *                                                               * G7B1PGM 
00038 *            12/11/02  AKK    COMPILE FOR OPID                  * G7B1PGM 
00039 *                                                               * G7B1PGM 
00040 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7B1PGM 
00041 *                           G7B1SET                              *G7B1PGM 
00042 ***************************************************************** G7B1PGM 
00043                                                                   G7B1PGM 
00044 ***************************************************************** G7B1PGM 
00045 *                                                               * G7B1PGM 
00046 *    G7B1PGM  - PROGRAM 1 OF 2 PROGRAMS TO UPDATE THE FORMAT 'B'* G7B1PGM 
00047 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7B1PGM 
00048 *                                                               * G7B1PGM 
00049 *    TRANSID: G7B1                                              * G7B1PGM 
00050 *    MAPSET:  G7B1SETC    (GIB1PGM WHICH SHARES THIS MAP)       * G7B1PGM 
00051 *    VALGEN:  NONE                                              * G7B1PGM 
00052 *                                                               * G7B1PGM 
00053 *    PROGRAM NARRATIVE:                                         * G7B1PGM 
00054 *                                                               * G7B1PGM 
00055 *        PROGRAM CHECKS FOR TRANS CODE 'G7B1'.  AN INVALID      * G7B1PGM 
00056 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7B1PGM 
00057 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7B1PGM 
00058 *                                                               * G7B1PGM 
00059 *        THE MAIN FUNCTIONS ARE :                               * G7B1PGM 
00060 *        1. HARDCOPY REQUEST,                                   * G7B1PGM 
00061 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7B1PGM 
00062 *           USER,                                               * G7B1PGM 
00063 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7B1PGM 
00064 *                                                               * G7B1PGM 
00065 *        HARDCOPY REQUEST                                       * G7B1PGM 
00066 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7B1PGM 
00067 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7B1PGM 
00068 *           THE SCREEN BUFFER.                                  * G7B1PGM 
00069 *                                                               * G7B1PGM 
00070 *        PROCESS INPUT DATA (UPDATE).                           * G7B1PGM 
00071 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7B1PGM 
00072 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7B1PGM 
00073 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7B1PGM 
00074 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7B1PGM 
00075 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7B1PGM 
00076 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7B1PGM 
00077 *                                                               * G7B1PGM 
00078 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7B1PGM 
00079 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7B1PGM 
00080 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7B1PGM 
00081 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7B1PGM 
00082 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7B1PGM 
00083 *                                                               * G7B1PGM 
00084 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7B1PGM 
00085 *                                     -----------------------   * G7B1PGM 
00086 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7B1PGM 
00087 *           -----------------------------------------------     * G7B1PGM 
00088 *                                                               * G7B1PGM 
00089 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7B1PGM 
00090 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7B1PGM 
00091 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7B1PGM 
00092 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7B1PGM 
00093 *           SENT BACK TO THE USER.                              * G7B1PGM 
00094 *                                                               * G7B1PGM 
00095 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7B1PGM 
00096 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7B1PGM 
00097 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7B1PGM 
00098 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7B1PGM 
00099 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7B1PGM 
00100 *           PF KEY LOGIC,                                       * G7B1PGM 
00101 *              PF6/PF18       GOES TO GC8APGM                   * G7B1PGM 
00102 *              PF8/PF20/ENTER GOES TO G7B2PGM                   * G7B1PGM 
00103 *              FOR PF7/PF19   GOES TO GC6CPGM                   * G7B1PGM 
00104 *                                                               * G7B1PGM 
00105 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7B1PGM 
00106 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7B1PGM 
00107 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7B1PGM 
00108 *                                                               * G7B1PGM 
00109 ***************************************************************** G7B1PGM 
00110                                                                   G7B1PGM 
00111  ENVIRONMENT DIVISION.                                            G7B1PGM 
00112  DATA DIVISION.                                                   G7B1PGM 
00113 /                                                                 G7B1PGM 
00114  WORKING-STORAGE SECTION.                                         G7B1PGM 
00115  01  WS-BEGIN                    PIC X(58) VALUE                  G7B1PGM 
00116      '*** G7B1PGM  WORKING-STORAGE BEGINS HERE ***'.              G7B1PGM 
00117                                                                   G7B1PGM 
00118                                                                   G7B1PGM 
00119  01  WS-01-ABEND-AREA.                                            G7B1PGM 
00120      05  FILLER                   PIC X(16)  VALUE                G7B1PGM 
00121          '** ABEND AREA **'.                                      G7B1PGM 
00122                                                                   G7B1PGM 
00123      05  WS-01-ABEND-CODES-AND-MSG.                               G7B1PGM 
00124          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7B1PGM 
00125          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7B1PGM 
00126                                                                   G7B1PGM 
00127          10  WS-01-ABCODE-B1F1          PIC X(04)  VALUE  'B1F1'. G7B1PGM 
00128          10  WS-01-ABCODE-B1F1-MSG      PIC X(44)  VALUE          G7B1PGM 
00129             'W/F CONTRACT CANNOT BE FOUND             '.          G7B1PGM 
00130                                                                   G7B1PGM 
00131          10  WS-01-ABCODE-B1F2          PIC X(04)  VALUE  'B1F2'. G7B1PGM 
00132          10  WS-01-ABCODE-B1F2-MSG      PIC X(44)  VALUE          G7B1PGM 
00133             'W/F BEN PROV CANNOT BE FOUND             '.          G7B1PGM 
00134                                                                   G7B1PGM 
00135          10  WS-01-ABCODE-B1F3          PIC X(04)  VALUE  'B1F3'. G7B1PGM 
00136          10  WS-01-ABCODE-B1F3-MSG      PIC X(44)  VALUE          G7B1PGM 
00137             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7B1PGM 
00138                                                                   G7B1PGM 
00139          10  WS-01-ABCODE-B1F4          PIC X(04)  VALUE  'B1F4'. G7B1PGM 
00140          10  WS-01-ABCODE-B1F4-MSG      PIC X(44)  VALUE          G7B1PGM 
00141             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7B1PGM 
00142                                                                   G7B1PGM 
00143          10  WS-01-ABCODE-B1L1          PIC X(04)  VALUE  'B1L1'. G7B1PGM 
00144          10  WS-01-ABCODE-B1L1-MSG      PIC X(44)  VALUE          G7B1PGM 
00145             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7B1PGM 
00146                                                                   G7B1PGM 
00147          10  WS-01-ABCODE-B1P1          PIC X(04)  VALUE  'B1P1'. G7B1PGM 
00148          10  WS-01-ABCODE-B1P1-MSG      PIC X(44)  VALUE          G7B1PGM 
00149             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7B1PGM 
00150                                                                   G7B1PGM 
00151          10  WS-01-ABCODE-B1P2          PIC X(04)  VALUE  'B1P2'. G7B1PGM 
00152          10  WS-01-ABCODE-B1P2-MSG      PIC X(44)  VALUE          G7B1PGM 
00153             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7B1PGM 
00154                                                                   G7B1PGM 
00155                                                                   G7B1PGM 
00156  01  WS-02-AREA.                                                  G7B1PGM 
00157      05  FILLER                   PIC X(16)  VALUE                G7B1PGM 
00158          '** WS-02-AREA **'.                                      G7B1PGM 
00159                                                                   G7B1PGM 
00160      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7B1PGM 
00161          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7B1PGM 
00162                                                    'GC6C' 'G7B2'  G7B1PGM 
00163                                                    'G7B1'.        G7B1PGM 
00164                                                                   G7B1PGM 
00165          88  WS-02-MY-EIBTRNID                     VALUE  'G7B1'. G7B1PGM 
00166                                                                   G7B1PGM 
00167                                                                   G7B1PGM 
00168      05  WS-02-COMPUTED-LENGTHS.                                  G7B1PGM 
00169          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7B1PGM 
00170          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7B1PGM 
00171          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7B1PGM 
00172                                                                   G7B1PGM 
00173      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7B1PGM 
00174                                                                   G7B1PGM 
00175      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7B1PGM 
00176                                                                   G7B1PGM 
00177      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7B1PGM 
00178      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7B1PGM 
00179          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7B1PGM 
00180                                         PIC X.                    G7B1PGM 
00181          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7B1PGM 
00182                                                    '0' THRU '9'   G7B1PGM 
00183                                                    'A' THRU 'Z'   G7B1PGM 
00184                                                    SPACE.         G7B1PGM 
00185                                                                   G7B1PGM 
00186      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7B1PGM 
00187          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7B1PGM 
00188          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7B1PGM 
00189                                                                   G7B1PGM 
00190      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7B1PGM 
00191          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7B1PGM 
00192                                                                   G7B1PGM 
00193      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7B1PGM 
00194                                                                   G7B1PGM 
00195      05  WS-02-HEX-F00000.                                        G7B1PGM 
00196          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7B1PGM 
00197          10  FILLER                     PIC  X(09) VALUE          G7B1PGM 
00198                                                    LOW-VALUES.    G7B1PGM 
00199                                                                   G7B1PGM 
00200      05  WS-02-HSP-ADM-RESTRN-DAYS-X.                             G7B1PGM 
00201          10  WS-02-HSP-ADM-RESTRN-DAYS  PIC  9(3)    VALUE ZEROS. G7B1PGM 
00202          10  WS-02-S1HADRD              REDEFINES                 G7B1PGM 
00203              WS-02-HSP-ADM-RESTRN-DAYS  PIC  X(3).                G7B1PGM 
00204                                                                   G7B1PGM 
00205      05  WS-02-STAY-CD-X.                                         G7B1PGM 
00206          10  WS-02-STAY-CD                PIC 9(3)    VALUE ZEROS.G7B1PGM 
00207          10  WS-02-S1STYCD              REDEFINES                 G7B1PGM 
00208              WS-02-STAY-CD                PIC X(3).               G7B1PGM 
00209                                                                   G7B1PGM 
00210      05  WS-02-TREAT-TIME-FACTOR-X.                               G7B1PGM 
00211          10  WS-02-TREAT-TIME-FACTOR      PIC 9(3)    VALUE ZEROS.G7B1PGM 
00212          10  WS-02-S1TRTMF              REDEFINES                 G7B1PGM 
00213              WS-02-TREAT-TIME-FACTOR      PIC X(3).               G7B1PGM 
00214 /                                                                 G7B1PGM 
00215  01  WT-00-G7B1PGM-TABLES.                                        G7B1PGM 
00216      05  FILLER                   PIC X(16)  VALUE                G7B1PGM 
00217          '*G7B1PGM TABLES*'.                                      G7B1PGM 
00218                                                                   G7B1PGM 
00219  01  WT-01-TABLE.                                                 G7B1PGM 
00220      05  FILLER                  PIC X(16) VALUE                  G7B1PGM 
00221          '* WT-01-TABLE  *'.                                      G7B1PGM 
00222 ******************************************************************G7B1PGM 
00223 *    WT-01   MESSAGE TABLE                                       *G7B1PGM 
00224 ******************************************************************G7B1PGM 
00225  01  FILLER.                                                      G7B1PGM 
00226      05  WT-01-MESSAGE-VALUES.                                    G7B1PGM 
00227                                                                   G7B1PGM 
00228 *----------------------------------------------------------------*G7B1PGM 
00229          10  WT-01-ENTRY-001.                                     G7B1PGM 
00230              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00231              15  WT-01-MESSAGE-TEXT-001.                          G7B1PGM 
00232                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00233                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00234                  20  FILLER          PIC X(3)  VALUE  '001'.      G7B1PGM 
00235                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00236                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00237                      ' INVALID PFKEY SELECTION                    G7B1PGM 
00238 -                    '                         '.                 G7B1PGM 
00239              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00240                                                                   G7B1PGM 
00241 *----------------------------------------------------------------*G7B1PGM 
00242          10  WT-01-ENTRY-002.                                     G7B1PGM 
00243              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00244              15  WT-01-MESSAGE-TEXT-002.                          G7B1PGM 
00245                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00246                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00247                  20  FILLER          PIC X(3)  VALUE  '002'.      G7B1PGM 
00248                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00249                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00250                      'HOSPITAL ADMISSION RESTRICTION DAYS REQUIREDG7B1PGM 
00251 -                    ' WHEN INDICATOR IS CODED '.                 G7B1PGM 
00252              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00253                                                                   G7B1PGM 
00254 *----------------------------------------------------------------*G7B1PGM 
00255          10  WT-01-ENTRY-003.                                     G7B1PGM 
00256              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00257              15  WT-01-MESSAGE-TEXT-003.                          G7B1PGM 
00258                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00259                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00260                  20  FILLER          PIC X(3)  VALUE  '003'.      G7B1PGM 
00261                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00262                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00263                      'INDICATOR REQUIRED WHEN HOSPITAL ADMISSION RG7B1PGM 
00264 -                    'ESTRICTION DAYS IS CODED '.                 G7B1PGM 
00265              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00266                                                                   G7B1PGM 
00267 *----------------------------------------------------------------*G7B1PGM 
00268          10  WT-01-ENTRY-004.                                     G7B1PGM 
00269              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00270              15  WT-01-MESSAGE-TEXT-004.                          G7B1PGM 
00271                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00272                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00273                  20  FILLER          PIC X(3)  VALUE  '004'.      G7B1PGM 
00274                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00275                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00276                      'STAY CODE DAYS REQUIRED WHEN INDICATOR IS C G7B1PGM 
00277 -                    'ODED                     '.                 G7B1PGM 
00278              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00279                                                                   G7B1PGM 
00280 *----------------------------------------------------------------*G7B1PGM 
00281          10  WT-01-ENTRY-005.                                     G7B1PGM 
00282              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00283              15  WT-01-MESSAGE-TEXT-005.                          G7B1PGM 
00284                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00285                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00286                  20  FILLER          PIC X(3)  VALUE  '005'.      G7B1PGM 
00287                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00288                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00289                      'INDICATOR REQUIRED WHEN STAY CODE DAYS IS C G7B1PGM 
00290 -                    'ODED            '.                          G7B1PGM 
00291              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00292                                                                   G7B1PGM 
00293 *----------------------------------------------------------------*G7B1PGM 
00294          10  WT-01-ENTRY-006.                                     G7B1PGM 
00295              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00296              15  WT-01-MESSAGE-TEXT-006.                          G7B1PGM 
00297                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00298                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00299                  20  FILLER          PIC X(3)  VALUE  '006'.      G7B1PGM 
00300                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00301                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00302                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7B1PGM 
00303 -                    ' PF4/PF16 TO CONTINUE    '.                 G7B1PGM 
00304              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00305                                                                   G7B1PGM 
00306 *----------------------------------------------------------------*G7B1PGM 
00307          10  WT-01-ENTRY-007.                                     G7B1PGM 
00308              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00309              15  WT-01-MESSAGE-TEXT-007.                          G7B1PGM 
00310                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00311                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00312                  20  FILLER          PIC X(3)  VALUE  '007'.      G7B1PGM 
00313                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00314                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00315                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7B1PGM 
00316 -                    'E CALL SYSTEMS           '.                 G7B1PGM 
00317              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00318                                                                   G7B1PGM 
00319 *----------------------------------------------------------------*G7B1PGM 
00320          10  WT-01-ENTRY-008.                                     G7B1PGM 
00321              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00322              15  WT-01-MESSAGE-TEXT-008.                          G7B1PGM 
00323                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00324                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00325                  20  FILLER          PIC X(3)  VALUE  '008'.      G7B1PGM 
00326                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00327                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00328                      'FIELD HAS AN INVALID VALUE                  G7B1PGM 
00329 -                    '                         '.                 G7B1PGM 
00330              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00331                                                                   G7B1PGM 
00332 *----------------------------------------------------------------*G7B1PGM 
00333          10  WT-01-ENTRY-009.                                     G7B1PGM 
00334              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00335              15  WT-01-MESSAGE-TEXT-009.                          G7B1PGM 
00336                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00337                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00338                  20  FILLER          PIC X(3)  VALUE  '009'.      G7B1PGM 
00339                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00340                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00341                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7B1PGM 
00342 -                    'YSTEM)                   '.                 G7B1PGM 
00343              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00344                                                                   G7B1PGM 
00345 *----------------------------------------------------------------*G7B1PGM 
00346          10  WT-01-ENTRY-010.                                     G7B1PGM 
00347              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00348              15  WT-01-MESSAGE-TEXT-010.                          G7B1PGM 
00349                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00350                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00351                  20  FILLER          PIC X(3)  VALUE  '010'.      G7B1PGM 
00352                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00353                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00354                      'PROVISION PRICING METHOD REQUIRES FLAT RATE G7B1PGM 
00355 -                    'PER DIEM BE > ZERO       '.                 G7B1PGM 
00356              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00357                                                                   G7B1PGM 
00358 *----------------------------------------------------------------*G7B1PGM 
00359          10  WT-01-ENTRY-011.                                     G7B1PGM 
00360              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00361              15  WT-01-MESSAGE-TEXT-011.                          G7B1PGM 
00362                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00363                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00364                  20  FILLER          PIC X(3)  VALUE  '011'.      G7B1PGM 
00365                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00366                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00367                      'PROVISION PRICING METHOD REQUIRES ECF FLAT RG7B1PGM 
00368 -                    'ATE PER DIEM > ZERO      '.                 G7B1PGM 
00369              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00370                                                                   G7B1PGM 
00371 *----------------------------------------------------------------*G7B1PGM 
00372          10  WT-01-ENTRY-012.                                     G7B1PGM 
00373              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00374              15  WT-01-MESSAGE-TEXT-012.                          G7B1PGM 
00375                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00376                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00377                  20  FILLER          PIC X(3)  VALUE  '012'.      G7B1PGM 
00378                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00379                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00380                      'INDICATOR REQUIRED WHEN TREATMENT TIME FACTOG7B1PGM 
00381 -                    'R DAYS IS CODED          '.                 G7B1PGM 
00382              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00383                                                                   G7B1PGM 
00384 *----------------------------------------------------------------*G7B1PGM 
00385          10  WT-01-ENTRY-013.                                     G7B1PGM 
00386              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00387              15  WT-01-MESSAGE-TEXT-013.                          G7B1PGM 
00388                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00389                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00390                  20  FILLER          PIC X(3)  VALUE  '013'.      G7B1PGM 
00391                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00392                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00393                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7B1PGM 
00394 -                    '                         '.                 G7B1PGM 
00395              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00396                                                                   G7B1PGM 
00397 *----------------------------------------------------------------*G7B1PGM 
00398          10  WT-01-ENTRY-014.                                     G7B1PGM 
00399              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00400              15  WT-01-MESSAGE-TEXT-014.                          G7B1PGM 
00401                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00402                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00403                  20  FILLER          PIC X(3)  VALUE  '014'.      G7B1PGM 
00404                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00405                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00406                      'TREATMENT TIME FACTOR DAYS REQUIRED WHEN INDG7B1PGM 
00407 -                    'ICATOR IS CODED          '.                 G7B1PGM 
00408              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00409                                                                   G7B1PGM 
00410 *----------------------------------------------------------------*G7B1PGM 
00411          10  WT-01-ENTRY-015.                                     G7B1PGM 
00412              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B1PGM 
00413              15  WT-01-MESSAGE-TEXT-015.                          G7B1PGM 
00414                  20  FILLER          PIC X(4)  VALUE  'G7B1'.     G7B1PGM 
00415                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B1PGM 
00416                  20  FILLER          PIC X(3)  VALUE  '015'.      G7B1PGM 
00417                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B1PGM 
00418                  20  FILLER          PIC X(70) VALUE              G7B1PGM 
00419                      '********** F U T U R E   U S E *************G7B1PGM 
00420 -                    '*************************'.                 G7B1PGM 
00421              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B1PGM 
00422                                                                   G7B1PGM 
00423 *----------------------------------------------------------------*G7B1PGM 
00424                                                                   G7B1PGM 
00425      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7B1PGM 
00426          WT-01-MESSAGE-VALUES         OCCURS 015 TIMES            G7B1PGM 
00427                                      INDEXED BY WT-01-INDEX.      G7B1PGM 
00428          10  WT-01-ENTRY.                                         G7B1PGM 
00429              15  FILLER              PIC X(02).                   G7B1PGM 
00430              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7B1PGM 
00431              15  FILLER              PIC X(02).                   G7B1PGM 
00432                                                                   G7B1PGM 
00433                                                                   G7B1PGM 
00434 /*** MAP FIELD ATTRIBUTES                                         G7B1PGM 
00435  COPY DFHBMSCA.                                                   G7B1PGM 
00436 *                         AUTOSKIP, BRIGHT, FSET                  G7B1PGM 
00437      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7B1PGM 
00438                                                                   G7B1PGM 
00439 /*** ATTENTION KEYS                                               G7B1PGM 
00440  COPY DFHAID.                                                     G7B1PGM 
00441                                                                   G7B1PGM 
00442 /***  PROVISION MAINTENANCE SCREEN                                G7B1PGM 
00443  COPY  G7B1SETC.                                                  G7B1PGM 
00444                                                                   G7B1PGM 
00445 /*** DATE ROUTINE COMMAREA                                        G7B1PGM 
00446  01  HGADATES-COMMAREA.                                           G7B1PGM 
00447  COPY HGCDAT01.                                                   G7B1PGM 
00448                                                                   G7B1PGM 
00449 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7B1PGM 
00450  01  GCVIOPGM-PARM-LIST.                                          G7B1PGM 
00451  COPY GCVINTRC.                                                   G7B1PGM 
00452                                                                   G7B1PGM 
00453 /*** ALTERNATIVE WORKFILE KEYS                                    G7B1PGM 
00454  01  FILLER.                                                      G7B1PGM 
00455      COPY GCWRKKEY.                                               G7B1PGM 
00456                                                                   G7B1PGM 
00457 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7B1PGM 
00458  01  FILLER.                                                      G7B1PGM 
00459      COPY GCCDRLEN.                                               G7B1PGM 
00460                                                                   G7B1PGM 
00461                                                                   G7B1PGM 
00462  01  WS-END                       PIC X(58) VALUE                 G7B1PGM 
00463      '*** G7B1PGM  WORKING-STORAGE ENDS HERE ***'.                G7B1PGM 
00464 /                                                                 G7B1PGM 
00465  LINKAGE SECTION.                                                 G7B1PGM 
00466 /                                                                 G7B1PGM 
00467  01  DFHCOMMAREA.                                                 G7B1PGM 
00468      COPY  GCWRKDCC.                                              G7B1PGM 
00469      COPY  GCBENPVC.                                              G7B1PGM 
00470 /                                                                 G7B1PGM 
00471 *01  BLL-CELLS.                                                   G7B1PGM 
00472 *    05  FILLER                   PIC S9(08)  COMP.               G7B1PGM 
00473 *    05  BEN-PROV-PNTR            PIC S9(08)  COMP.               G7B1PGM 
00474 *    05  CONTRACT-PNTR            PIC S9(08)  COMP.               G7B1PGM 
00475 *    05  CONTRACT-PNTR-2          PIC S9(08)  COMP.               G7B1PGM 
00476 *                                                                 G7B1PGM 
00477 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7B1PGM 
00478  01  IO-PARM-BEN-PROV-AREA.                                       G7B1PGM 
00479      COPY  GCIOPRM2.                                              G7B1PGM 
00480      COPY  GCWRKDC2.                                              G7B1PGM 
00481      COPY  GCBENPV2.                                              G7B1PGM 
00482                                                                   G7B1PGM 
00483 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7B1PGM 
00484  01  IO-PARM-CONTRACT-AREA.                                       G7B1PGM 
00485      COPY  GCIOPRM3.                                              G7B1PGM 
00486      COPY  GCWRKDC3.                                              G7B1PGM 
00487      COPY  GCCONTR2.                                              G7B1PGM 
00488 /                                                                 G7B1PGM 
00489  PROCEDURE DIVISION.                                              G7B1PGM 
00490                                                                   G7B1PGM 
00491 ****************************************************************  G7B1PGM 
00492 *                                                              *  G7B1PGM 
00493 *           P R O C E S S     C O N T R O L                    *  G7B1PGM 
00494 *                                                              *  G7B1PGM 
00495 ****************************************************************  G7B1PGM 
00496  0000-000-PROCESS-CONTROL       SECTION.                          G7B1PGM 
00497  0000-010.                                                        G7B1PGM 
00498                                                                   G7B1PGM 
00499      IF  EIBAID  =  DFHCLEAR                                      G7B1PGM 
00500          EXEC CICS  RETURN                                        G7B1PGM 
00501                     END-EXEC.                                     G7B1PGM 
00502                                                                   G7B1PGM 
00503      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7B1PGM 
00504                                                                   G7B1PGM 
00505      IF  WS-02-MY-EIBTRNID                                        G7B1PGM 
00506      THEN                                                         G7B1PGM 
00507          PERFORM  2000-000-PROCESS-INPUT                          G7B1PGM 
00508      ELSE                                                         G7B1PGM 
00509          PERFORM  1000-000-DISPLAY-SCREEN.                        G7B1PGM 
00510                                                                   G7B1PGM 
00511                                                                   G7B1PGM 
00512 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7B1PGM 
00513                                                                   G7B1PGM 
00514      MOVE WS-01-ABCODE-B1L1     TO WS-01-ABCODE                   G7B1PGM 
00515      MOVE WS-01-ABCODE-B1L1-MSG TO WS-01-ABCODE-MSG               G7B1PGM 
00516      PERFORM  9999-000-ABEND-THE-TASK.                            G7B1PGM 
00517                                                                   G7B1PGM 
00518      GOBACK.                                                      G7B1PGM 
00519                                                                   G7B1PGM 
00520                                                                   G7B1PGM 
00521  0000-900-EXIT.                                                   G7B1PGM 
00522      EXIT.                                                        G7B1PGM 
00523 /***************************************************************  G7B1PGM 
00524 *                                                              *  G7B1PGM 
00525 * 1000  DISPLAY INITIAL SCREEN                                 *  G7B1PGM 
00526 *                                                              *  G7B1PGM 
00527 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7B1PGM 
00528 *                                                              *  G7B1PGM 
00529 ****************************************************************  G7B1PGM 
00530  1000-000-DISPLAY-SCREEN        SECTION.                          G7B1PGM 
00531  1000-010.                                                        G7B1PGM 
00532                                                                   G7B1PGM 
00533 *----MOVE LOW-VALUES TO SCREEN                                    G7B1PGM 
00534      MOVE LOW-VALUES TO G7B1I01I.                                 G7B1PGM 
00535                                                                   G7B1PGM 
00536 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7B1PGM 
00537                                                                   G7B1PGM 
00538      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7B1PGM 
00539          MOVE WS-01-ABCODE-B1P1     TO WS-01-ABCODE               G7B1PGM 
00540          MOVE WS-01-ABCODE-B1P1-MSG TO WS-01-ABCODE-MSG           G7B1PGM 
00541          PERFORM 9999-000-ABEND-THE-TASK.                         G7B1PGM 
00542                                                                   G7B1PGM 
00543                                                                   G7B1PGM 
00544 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7B1PGM 
00545                                                                   G7B1PGM 
00546      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7B1PGM 
00547                                         + GC-GCBENPRV-FIXED-LEN   G7B1PGM 
00548                                         + GC-GCBENPRV-VARY-LEN.   G7B1PGM 
00549                                                                   G7B1PGM 
00550                                                                   G7B1PGM 
00551 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7B1PGM 
00552                                                                   G7B1PGM 
00553      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7B1PGM 
00554          MOVE WS-01-ABCODE-B1P2     TO WS-01-ABCODE               G7B1PGM 
00555          MOVE WS-01-ABCODE-B1P2-MSG TO WS-01-ABCODE-MSG           G7B1PGM 
00556          PERFORM 9999-000-ABEND-THE-TASK.                         G7B1PGM 
00557                                                                   G7B1PGM 
00558                                                                   G7B1PGM 
00559 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7B1PGM 
00560 *          BY CALLER IN COMMAREA.                                 G7B1PGM 
00561                                                                   G7B1PGM 
00562      MOVE WRK-PLAN-CODE                      TO S1PLNCDO.         G7B1PGM 
00563      MOVE WRK-GROUP-NUM                      TO S1GRPNOO.         G7B1PGM 
00564      MOVE WRK-SECTION-NUM                    TO S1SECNOO.         G7B1PGM 
00565      MOVE WRK-PKG-CODE                       TO S1PKGCDO.         G7B1PGM 
00566      MOVE WRK-PROV-CTL                       TO S1PRVO.           G7B1PGM 
00567      MOVE WRK-FAM-REL-LEVEL                  TO S1FRLO.           G7B1PGM 
00568      MOVE WRK-L-O-B                          TO S1LOBO.           G7B1PGM 
00569                                                                   G7B1PGM 
00570      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7B1PGM 
00571      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7B1PGM 
00572      IF  HGADATE-RETURN = ZEROS                                   G7B1PGM 
00573      THEN                                                         G7B1PGM 
00574          MOVE DFHBMASF                       TO S1EFFDTA          G7B1PGM 
00575          MOVE HGADATE-DATE2                  TO S1EFFDTO          G7B1PGM 
00576      ELSE                                                         G7B1PGM 
00577          MOVE DFHBMABF                       TO S1EFFDTA          G7B1PGM 
00578          MOVE HGADATE-JULIAN1                TO S1EFFDTO.         G7B1PGM 
00579                                                                   G7B1PGM 
00580      MOVE GCP-PROVN-ID                       TO S1BPVIDO.         G7B1PGM 
00581      MOVE GPB-HOSP-ADM-RESTRN-IND            TO S1HADMRO.         G7B1PGM 
00582                                                                   G7B1PGM 
00583      IF  GPB-HSP-ADM-RESTRN-DAYS = ZEROS                          G7B1PGM 
00584          MOVE WS-02-HEX-F00000               TO S1HADRDO          G7B1PGM 
00585      ELSE                                                         G7B1PGM 
00586          MOVE   GPB-HSP-ADM-RESTRN-DAYS      TO                   G7B1PGM 
00587               WS-02-HSP-ADM-RESTRN-DAYS                           G7B1PGM 
00588          MOVE WS-02-HSP-ADM-RESTRN-DAYS-X    TO S1HADRDO.         G7B1PGM 
00589                                                                   G7B1PGM 
00590      MOVE  GPB-HOSP-COND-RELATSP-IND         TO S1HCNDRO.         G7B1PGM 
00591      MOVE  GPB-REHAB-ADM-RESTRN-IND          TO S1RHADRO.         G7B1PGM 
00592      MOVE  GPB-STAY-CODE-IND                 TO S1STCDIO.         G7B1PGM 
00593                                                                   G7B1PGM 
00594      IF  GPB-STAY-CD = ZEROS                                      G7B1PGM 
00595          MOVE WS-02-HEX-F00000               TO S1STYCDO          G7B1PGM 
00596      ELSE                                                         G7B1PGM 
00597          MOVE GPB-STAY-CD                    TO WS-02-STAY-CD     G7B1PGM 
00598          MOVE WS-02-STAY-CD-X                TO S1STYCDO.         G7B1PGM 
00599                                                                   G7B1PGM 
00600      MOVE GPB-TREAT-TIME-FACTOR-IND          TO S1TRTMIO.         G7B1PGM 
00601                                                                   G7B1PGM 
00602      IF  GPB-TREAT-TIME-FACTOR = ZEROS                            G7B1PGM 
00603          MOVE WS-02-HEX-F00000               TO S1TRTMFO          G7B1PGM 
00604      ELSE                                                         G7B1PGM 
00605          MOVE GPB-TREAT-TIME-FACTOR          TO                   G7B1PGM 
00606                                            WS-02-TREAT-TIME-FACTORG7B1PGM 
00607          MOVE WS-02-TREAT-TIME-FACTOR-X      TO S1TRTMFO.         G7B1PGM 
00608                                                                   G7B1PGM 
00609      MOVE GPB-ELIG-METHD-OF-TREAT-IND        TO S1ELMTIO.         G7B1PGM 
00610      MOVE GPB-REPR-REPLAC-RESTRN-IND         TO S1RRRSTO.         G7B1PGM 
00611      MOVE GPB-CERTN-REPETN-REQRD-IND         TO S1RRCERO.         G7B1PGM 
00612                                                                   G7B1PGM 
00613                                                                   G7B1PGM 
00614 *------- SEND INITIAL SCREEN ------------------------------------*G7B1PGM 
00615                                                                   G7B1PGM 
00616      MOVE  -1 TO  S1HADMRL.                                       G7B1PGM 
00617      PERFORM 9100-000-SEND-THEN-RETURN.                           G7B1PGM 
00618                                                                   G7B1PGM 
00619                                                                   G7B1PGM 
00620  1000-900-EXIT.                                                   G7B1PGM 
00621      EXIT.                                                        G7B1PGM 
00622 /***************************************************************  G7B1PGM 
00623 *                                                              *  G7B1PGM 
00624 * 2000    P R O C E S S    I N P U T                           *  G7B1PGM 
00625 *                                                              *  G7B1PGM 
00626 ****************************************************************  G7B1PGM 
00627  2000-000-PROCESS-INPUT         SECTION.                          G7B1PGM 
00628  2000-010.                                                        G7B1PGM 
00629                                                                   G7B1PGM 
00630 *------ VALIDATE PFKEY USAGE ------------------------------------*G7B1PGM 
00631                                                                   G7B1PGM 
00632      IF  EIBAID = DFHENTER OR                                     G7B1PGM 
00633                   DFHPF3   OR  DFHPF15 OR                         G7B1PGM 
00634                   DFHPF4   OR  DFHPF16 OR                         G7B1PGM 
00635                   DFHPF6   OR  DFHPF18 OR                         G7B1PGM 
00636                   DFHPF7   OR  DFHPF19 OR                         G7B1PGM 
00637                   DFHPF8   OR  DFHPF20 OR                         G7B1PGM 
00638                   DFHPF24                                         G7B1PGM 
00639      THEN                                                         G7B1PGM 
00640          NEXT SENTENCE                                            G7B1PGM 
00641      ELSE                                                         G7B1PGM 
00642          SET WT-01-INDEX TO +01                                   G7B1PGM 
00643          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7B1PGM 
00644          PERFORM 9100-000-SEND-THEN-RETURN.                       G7B1PGM 
00645                                                                   G7B1PGM 
00646                                                                   G7B1PGM 
00647                                                                   G7B1PGM 
00648      EXEC CICS  HANDLE CONDITION                                  G7B1PGM 
00649                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7B1PGM 
00650                        END-EXEC.                                  G7B1PGM 
00651                                                                   G7B1PGM 
00652                                                                   G7B1PGM 
00653      EXEC CICS  RECEIVE MAP   ('G7B1I01')                         G7B1PGM 
00654                         MAPSET('G7B1SET')                         G7B1PGM 
00655                         END-EXEC.                                 G7B1PGM 
00656                                                                   G7B1PGM 
00657                                                                   G7B1PGM 
00658      IF  S1FUNCI  NOT = 'G7B1'  OR                                G7B1PGM 
00659          S1SCRNI  NOT = '007B01'                                  G7B1PGM 
00660          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7B1PGM 
00661                                                                   G7B1PGM 
00662                                                                   G7B1PGM 
00663 *--- RETURN TO GCPS MENU? ---------------------------------------*G7B1PGM 
00664                                                                   G7B1PGM 
00665      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7B1PGM 
00666          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7B1PGM 
00667                                                                   G7B1PGM 
00668 *--- PROCESS SCREEN FIELDS --------------------------------------*G7B1PGM 
00669                                                                   G7B1PGM 
00670      PERFORM 2100-000-FIELD-EDITS.                                G7B1PGM 
00671                                                                   G7B1PGM 
00672      IF  WS-02-SCREEN-HAS-ERRORS                                  G7B1PGM 
00673          PERFORM 9100-000-SEND-THEN-RETURN.                       G7B1PGM 
00674                                                                   G7B1PGM 
00675      PERFORM 2200-000-LOGICAL-EDITS.                              G7B1PGM 
00676                                                                   G7B1PGM 
00677      IF  WS-02-SCREEN-HAS-ERRORS                                  G7B1PGM 
00678          PERFORM 9100-000-SEND-THEN-RETURN.                       G7B1PGM 
00679                                                                   G7B1PGM 
00680      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7B1PGM 
00681                                                                   G7B1PGM 
00682      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7B1PGM 
00683                                                                   G7B1PGM 
00684                                                                   G7B1PGM 
00685  2000-900-EXIT.                                                   G7B1PGM 
00686      EXIT.                                                        G7B1PGM 
00687 /***************************************************************  G7B1PGM 
00688 *                                                              *  G7B1PGM 
00689 * 2100  DO SCREEN FIELD EDITS                                  *  G7B1PGM 
00690 *                                                              *  G7B1PGM 
00691 ****************************************************************  G7B1PGM 
00692  2100-000-FIELD-EDITS           SECTION.                          G7B1PGM 
00693  2100-010.                                                        G7B1PGM 
00694                                                                   G7B1PGM 
00695 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7B1PGM 
00696                                                                   G7B1PGM 
00697      MOVE DFHBMUNF TO  S1HADMRA                                   G7B1PGM 
00698                        S1HADRDA                                   G7B1PGM 
00699                        S1HCNDRA                                   G7B1PGM 
00700                        S1RHADRA                                   G7B1PGM 
00701                        S1STCDIA                                   G7B1PGM 
00702                        S1STYCDA                                   G7B1PGM 
00703                        S1TRTMIA                                   G7B1PGM 
00704                        S1TRTMFA                                   G7B1PGM 
00705                        S1ELMTIA                                   G7B1PGM 
00706                        S1RRRSTA                                   G7B1PGM 
00707                        S1RRCERA.                                  G7B1PGM 
00708                                                                   G7B1PGM 
00709      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7B1PGM 
00710                                                                   G7B1PGM 
00711                                                                   G7B1PGM 
00712 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION IND ----------*G7B1PGM 
00713 *   1. ALPHANUMERIC                                               G7B1PGM 
00714 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
00715                                                                   G7B1PGM 
00716      MOVE  S1HADMRI TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
00717      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
00718      THEN                                                         G7B1PGM 
00719          MOVE  S1HADMRI TO GCVI-VALUE                             G7B1PGM 
00720          MOVE  'BPAA01' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
00721          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
00722          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
00723          THEN                                                     G7B1PGM 
00724              MOVE  -1        TO  S1HADMRL                         G7B1PGM 
00725              MOVE  DFHBMUBF  TO  S1HADMRA                         G7B1PGM 
00726              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
00727              THEN                                                 G7B1PGM 
00728                  NEXT SENTENCE                                    G7B1PGM 
00729              ELSE                                                 G7B1PGM 
00730                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
00731                  SET WT-01-INDEX TO +09                           G7B1PGM 
00732                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
00733          ELSE                                                     G7B1PGM 
00734              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
00735              THEN                                                 G7B1PGM 
00736                  MOVE  DFHBMUBF  TO  S1HADMRA                     G7B1PGM 
00737              ELSE                                                 G7B1PGM 
00738                  NEXT SENTENCE                                    G7B1PGM 
00739      ELSE                                                         G7B1PGM 
00740          MOVE  -1        TO  S1HADMRL                             G7B1PGM 
00741          MOVE  DFHBMUBF  TO  S1HADMRA                             G7B1PGM 
00742          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00743          THEN                                                     G7B1PGM 
00744              NEXT SENTENCE                                        G7B1PGM 
00745          ELSE                                                     G7B1PGM 
00746              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00747              SET WT-01-INDEX TO +08                               G7B1PGM 
00748              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00749                                                                   G7B1PGM 
00750                                                                   G7B1PGM 
00751                                                                   G7B1PGM 
00752 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION DAYS ---------*G7B1PGM 
00753 *   1. NUMERICS                                                   G7B1PGM 
00754                                                                   G7B1PGM 
00755      IF  S1HADRDI IS NUMERIC                                      G7B1PGM 
00756      THEN                                                         G7B1PGM 
00757          NEXT SENTENCE                                            G7B1PGM 
00758      ELSE                                                         G7B1PGM 
00759          MOVE  -1        TO  S1HADRDL                             G7B1PGM 
00760          MOVE  DFHBMUBF  TO  S1HADRDA                             G7B1PGM 
00761          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00762          THEN                                                     G7B1PGM 
00763              NEXT SENTENCE                                        G7B1PGM 
00764          ELSE                                                     G7B1PGM 
00765              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00766              SET WT-01-INDEX TO +13                               G7B1PGM 
00767              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00768                                                                   G7B1PGM 
00769                                                                   G7B1PGM 
00770 *-- VALIDATE ------ HOSP. COND. RELATIONSHIP IND ----------------*G7B1PGM 
00771 *   1. ALPHANUMERIC                                               G7B1PGM 
00772 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
00773                                                                   G7B1PGM 
00774      MOVE  S1HCNDRI TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
00775      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
00776      THEN                                                         G7B1PGM 
00777          MOVE  S1HCNDRI TO GCVI-VALUE                             G7B1PGM 
00778          MOVE  'BPAA03' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
00779          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
00780          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
00781          THEN                                                     G7B1PGM 
00782              MOVE  -1        TO  S1HCNDRL                         G7B1PGM 
00783              MOVE  DFHBMUBF  TO  S1HCNDRA                         G7B1PGM 
00784              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
00785              THEN                                                 G7B1PGM 
00786                  NEXT SENTENCE                                    G7B1PGM 
00787              ELSE                                                 G7B1PGM 
00788                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
00789                  SET WT-01-INDEX TO +09                           G7B1PGM 
00790                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
00791          ELSE                                                     G7B1PGM 
00792              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
00793              THEN                                                 G7B1PGM 
00794                  MOVE  DFHBMUBF  TO  S1HCNDRA                     G7B1PGM 
00795              ELSE                                                 G7B1PGM 
00796                  NEXT SENTENCE                                    G7B1PGM 
00797      ELSE                                                         G7B1PGM 
00798          MOVE  -1        TO  S1HCNDRL                             G7B1PGM 
00799          MOVE  DFHBMUBF  TO  S1HCNDRA                             G7B1PGM 
00800          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00801          THEN                                                     G7B1PGM 
00802              NEXT SENTENCE                                        G7B1PGM 
00803          ELSE                                                     G7B1PGM 
00804              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00805              SET WT-01-INDEX TO +08                               G7B1PGM 
00806              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00807                                                                   G7B1PGM 
00808                                                                   G7B1PGM 
00809 *-- VALIDATE ------ REHAB. AD. RESTRICTION IND ------------------*G7B1PGM 
00810 *   1. ALPHANUMERIC                                               G7B1PGM 
00811 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
00812                                                                   G7B1PGM 
00813      MOVE  S1RHADRI TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
00814      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
00815      THEN                                                         G7B1PGM 
00816          MOVE  S1RHADRI TO GCVI-VALUE                             G7B1PGM 
00817          MOVE  'BPAA04' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
00818          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
00819          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
00820          THEN                                                     G7B1PGM 
00821              MOVE  -1        TO  S1RHADRL                         G7B1PGM 
00822              MOVE  DFHBMUBF  TO  S1RHADRA                         G7B1PGM 
00823              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
00824              THEN                                                 G7B1PGM 
00825                  NEXT SENTENCE                                    G7B1PGM 
00826              ELSE                                                 G7B1PGM 
00827                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
00828                  SET WT-01-INDEX TO +09                           G7B1PGM 
00829                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
00830          ELSE                                                     G7B1PGM 
00831              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
00832              THEN                                                 G7B1PGM 
00833                  MOVE  DFHBMUBF  TO  S1RHADRA                     G7B1PGM 
00834              ELSE                                                 G7B1PGM 
00835                  NEXT SENTENCE                                    G7B1PGM 
00836      ELSE                                                         G7B1PGM 
00837          MOVE  -1        TO  S1RHADRL                             G7B1PGM 
00838          MOVE  DFHBMUBF  TO  S1RHADRA                             G7B1PGM 
00839          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00840          THEN                                                     G7B1PGM 
00841              NEXT SENTENCE                                        G7B1PGM 
00842          ELSE                                                     G7B1PGM 
00843              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00844              SET WT-01-INDEX TO +08                               G7B1PGM 
00845              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00846                                                                   G7B1PGM 
00847                                                                   G7B1PGM 
00848 *-- VALIDATE ------ STAY CODE INDICATOR -------------------------*G7B1PGM 
00849 *   1. ALPHANUMERIC                                               G7B1PGM 
00850 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
00851                                                                   G7B1PGM 
00852      MOVE  S1STCDII TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
00853      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
00854      THEN                                                         G7B1PGM 
00855          MOVE  S1STCDII TO GCVI-VALUE                             G7B1PGM 
00856          MOVE  'BPAA12' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
00857          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
00858          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
00859          THEN                                                     G7B1PGM 
00860              MOVE  -1        TO  S1STCDIL                         G7B1PGM 
00861              MOVE  DFHBMUBF  TO  S1STCDIA                         G7B1PGM 
00862              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
00863              THEN                                                 G7B1PGM 
00864                  NEXT SENTENCE                                    G7B1PGM 
00865              ELSE                                                 G7B1PGM 
00866                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
00867                  SET WT-01-INDEX TO +09                           G7B1PGM 
00868                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
00869          ELSE                                                     G7B1PGM 
00870              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
00871              THEN                                                 G7B1PGM 
00872                  MOVE  DFHBMUBF  TO  S1STCDIA                     G7B1PGM 
00873              ELSE                                                 G7B1PGM 
00874                  NEXT SENTENCE                                    G7B1PGM 
00875      ELSE                                                         G7B1PGM 
00876          MOVE  -1        TO  S1STCDIL                             G7B1PGM 
00877          MOVE  DFHBMUBF  TO  S1STCDIA                             G7B1PGM 
00878          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00879          THEN                                                     G7B1PGM 
00880              NEXT SENTENCE                                        G7B1PGM 
00881          ELSE                                                     G7B1PGM 
00882              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00883              SET WT-01-INDEX TO +08                               G7B1PGM 
00884              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00885                                                                   G7B1PGM 
00886                                                                   G7B1PGM 
00887 *-- VALIDATE ------ STAY CODE -----------------------------------*G7B1PGM 
00888 *   1. NUMERICS                                                   G7B1PGM 
00889                                                                   G7B1PGM 
00890      IF  S1STYCDI IS NUMERIC                                      G7B1PGM 
00891      THEN                                                         G7B1PGM 
00892          NEXT SENTENCE                                            G7B1PGM 
00893      ELSE                                                         G7B1PGM 
00894          MOVE  -1        TO  S1STYCDL                             G7B1PGM 
00895          MOVE  DFHBMUBF  TO  S1STYCDA                             G7B1PGM 
00896          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00897          THEN                                                     G7B1PGM 
00898              NEXT SENTENCE                                        G7B1PGM 
00899          ELSE                                                     G7B1PGM 
00900              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00901              SET WT-01-INDEX TO +13                               G7B1PGM 
00902              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00903                                                                   G7B1PGM 
00904                                                                   G7B1PGM 
00905 *-- VALIDATE ------ TREATMENT TIME FACTOR IND -------------------*G7B1PGM 
00906 *   1. ALPHANUMERIC                                               G7B1PGM 
00907 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
00908                                                                   G7B1PGM 
00909      MOVE  S1TRTMII TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
00910      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
00911      THEN                                                         G7B1PGM 
00912          MOVE  S1TRTMII TO GCVI-VALUE                             G7B1PGM 
00913          MOVE  'BPBA07' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
00914          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
00915          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
00916          THEN                                                     G7B1PGM 
00917              MOVE  -1        TO  S1TRTMIL                         G7B1PGM 
00918              MOVE  DFHBMUBF  TO  S1TRTMIA                         G7B1PGM 
00919              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
00920              THEN                                                 G7B1PGM 
00921                  NEXT SENTENCE                                    G7B1PGM 
00922              ELSE                                                 G7B1PGM 
00923                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
00924                  SET WT-01-INDEX TO +09                           G7B1PGM 
00925                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
00926          ELSE                                                     G7B1PGM 
00927              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
00928              THEN                                                 G7B1PGM 
00929                  MOVE  DFHBMUBF  TO  S1TRTMIA                     G7B1PGM 
00930              ELSE                                                 G7B1PGM 
00931                  NEXT SENTENCE                                    G7B1PGM 
00932      ELSE                                                         G7B1PGM 
00933          MOVE  -1        TO  S1TRTMIL                             G7B1PGM 
00934          MOVE  DFHBMUBF  TO  S1TRTMIA                             G7B1PGM 
00935          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00936          THEN                                                     G7B1PGM 
00937              NEXT SENTENCE                                        G7B1PGM 
00938          ELSE                                                     G7B1PGM 
00939              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00940              SET WT-01-INDEX TO +08                               G7B1PGM 
00941              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00942                                                                   G7B1PGM 
00943                                                                   G7B1PGM 
00944 *-- VALIDATE ------ TREATMENT TIME FACTOR -----------------------*G7B1PGM 
00945 *   1. NUMERICS                                                   G7B1PGM 
00946                                                                   G7B1PGM 
00947      IF  S1TRTMFI IS NUMERIC                                      G7B1PGM 
00948      THEN                                                         G7B1PGM 
00949          NEXT SENTENCE                                            G7B1PGM 
00950      ELSE                                                         G7B1PGM 
00951          MOVE  -1        TO  S1TRTMFL                             G7B1PGM 
00952          MOVE  DFHBMUBF  TO  S1TRTMFA                             G7B1PGM 
00953          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00954          THEN                                                     G7B1PGM 
00955              NEXT SENTENCE                                        G7B1PGM 
00956          ELSE                                                     G7B1PGM 
00957              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00958              SET WT-01-INDEX TO +13                               G7B1PGM 
00959              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00960                                                                   G7B1PGM 
00961                                                                   G7B1PGM 
00962 *-- VALIDATE ------ ELIG. METHOD OF TREATMENT IND ---------------*G7B1PGM 
00963 *   1. ALPHANUMERIC                                               G7B1PGM 
00964 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
00965                                                                   G7B1PGM 
00966      MOVE  S1ELMTII TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
00967      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
00968      THEN                                                         G7B1PGM 
00969          MOVE  S1ELMTII TO GCVI-VALUE                             G7B1PGM 
00970          MOVE  'BPBA09' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
00971          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
00972          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
00973          THEN                                                     G7B1PGM 
00974              MOVE  -1        TO  S1ELMTIL                         G7B1PGM 
00975              MOVE  DFHBMUBF  TO  S1ELMTIA                         G7B1PGM 
00976              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
00977              THEN                                                 G7B1PGM 
00978                  NEXT SENTENCE                                    G7B1PGM 
00979              ELSE                                                 G7B1PGM 
00980                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
00981                  SET WT-01-INDEX TO +09                           G7B1PGM 
00982                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
00983          ELSE                                                     G7B1PGM 
00984              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
00985              THEN                                                 G7B1PGM 
00986                  MOVE  DFHBMUBF  TO  S1ELMTIA                     G7B1PGM 
00987              ELSE                                                 G7B1PGM 
00988                  NEXT SENTENCE                                    G7B1PGM 
00989      ELSE                                                         G7B1PGM 
00990          MOVE  -1        TO  S1ELMTIL                             G7B1PGM 
00991          MOVE  DFHBMUBF  TO  S1ELMTIA                             G7B1PGM 
00992          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
00993          THEN                                                     G7B1PGM 
00994              NEXT SENTENCE                                        G7B1PGM 
00995          ELSE                                                     G7B1PGM 
00996              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
00997              SET WT-01-INDEX TO +08                               G7B1PGM 
00998              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
00999                                                                   G7B1PGM 
01000                                                                   G7B1PGM 
01001 *-- VALIDATE ------ REPAIR/REPLACE RESTRICTION IND --------------*G7B1PGM 
01002 *   1. ALPHANUMERIC                                               G7B1PGM 
01003 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
01004                                                                   G7B1PGM 
01005      MOVE  S1RRRSTI TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
01006      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
01007      THEN                                                         G7B1PGM 
01008          MOVE  S1RRRSTI TO GCVI-VALUE                             G7B1PGM 
01009          MOVE  'BPBA10' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
01010          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
01011          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
01012          THEN                                                     G7B1PGM 
01013              MOVE  -1        TO  S1RRRSTL                         G7B1PGM 
01014              MOVE  DFHBMUBF  TO  S1RRRSTA                         G7B1PGM 
01015              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
01016              THEN                                                 G7B1PGM 
01017                  NEXT SENTENCE                                    G7B1PGM 
01018              ELSE                                                 G7B1PGM 
01019                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01020                  SET WT-01-INDEX TO +09                           G7B1PGM 
01021                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
01022          ELSE                                                     G7B1PGM 
01023              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
01024              THEN                                                 G7B1PGM 
01025                  MOVE  DFHBMUBF  TO  S1RRRSTA                     G7B1PGM 
01026              ELSE                                                 G7B1PGM 
01027                  NEXT SENTENCE                                    G7B1PGM 
01028      ELSE                                                         G7B1PGM 
01029          MOVE  -1        TO  S1RRRSTL                             G7B1PGM 
01030          MOVE  DFHBMUBF  TO  S1RRRSTA                             G7B1PGM 
01031          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01032          THEN                                                     G7B1PGM 
01033              NEXT SENTENCE                                        G7B1PGM 
01034          ELSE                                                     G7B1PGM 
01035              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
01036              SET WT-01-INDEX TO +08                               G7B1PGM 
01037              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
01038                                                                   G7B1PGM 
01039                                                                   G7B1PGM 
01040 *-- VALIDATE ------ REPAIR/REPLACE RESTRICTION IND --------------*G7B1PGM 
01041 *   1. ALPHANUMERIC                                               G7B1PGM 
01042 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B1PGM 
01043                                                                   G7B1PGM 
01044      MOVE  S1RRCERI TO WS-02-CLASS-TEST-AREA.                     G7B1PGM 
01045      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B1PGM 
01046      THEN                                                         G7B1PGM 
01047          MOVE  S1RRCERI TO GCVI-VALUE                             G7B1PGM 
01048          MOVE  'BPAB02' TO GCVI-FIELDS-KEY-ID                     G7B1PGM 
01049          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B1PGM 
01050          IF  GCVI-VALUE-NOT-FOUND                                 G7B1PGM 
01051          THEN                                                     G7B1PGM 
01052              MOVE  -1        TO  S1RRCERL                         G7B1PGM 
01053              MOVE  DFHBMUBF  TO  S1RRCERA                         G7B1PGM 
01054              IF  WS-02-SCREEN-HAS-ERRORS                          G7B1PGM 
01055              THEN                                                 G7B1PGM 
01056                  NEXT SENTENCE                                    G7B1PGM 
01057              ELSE                                                 G7B1PGM 
01058                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01059                  SET WT-01-INDEX TO +09                           G7B1PGM 
01060                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
01061          ELSE                                                     G7B1PGM 
01062              IF  GCVI-VALUE-NOT-LOADED                            G7B1PGM 
01063              THEN                                                 G7B1PGM 
01064                  MOVE  DFHBMUBF  TO  S1RRCERA                     G7B1PGM 
01065              ELSE                                                 G7B1PGM 
01066                  NEXT SENTENCE                                    G7B1PGM 
01067      ELSE                                                         G7B1PGM 
01068          MOVE  -1        TO  S1RRCERL                             G7B1PGM 
01069          MOVE  DFHBMUBF  TO  S1RRCERA                             G7B1PGM 
01070          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01071          THEN                                                     G7B1PGM 
01072              NEXT SENTENCE                                        G7B1PGM 
01073          ELSE                                                     G7B1PGM 
01074              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7B1PGM 
01075              SET WT-01-INDEX TO +08                               G7B1PGM 
01076              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B1PGM 
01077                                                                   G7B1PGM 
01078                                                                   G7B1PGM 
01079  2100-900-EXIT.                                                   G7B1PGM 
01080      EXIT.                                                        G7B1PGM 
01081 /***************************************************************  G7B1PGM 
01082 *                                                              *  G7B1PGM 
01083 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7B1PGM 
01084 *                                                              *  G7B1PGM 
01085 ****************************************************************  G7B1PGM 
01086  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7B1PGM 
01087  2110-010.                                                        G7B1PGM 
01088                                                                   G7B1PGM 
01089      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7B1PGM 
01090                                                                   G7B1PGM 
01091      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7B1PGM 
01092                       COMMAREA(GCVIOPGM-PARM-LIST)                G7B1PGM 
01093                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7B1PGM 
01094                       END-EXEC.                                   G7B1PGM 
01095                                                                   G7B1PGM 
01096      IF  GCVI-VALUE-NOT-LOADED                                    G7B1PGM 
01097          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7B1PGM 
01098                                                                   G7B1PGM 
01099  2110-900-EXIT.                                                   G7B1PGM 
01100      EXIT.                                                        G7B1PGM 
01101 /***************************************************************  G7B1PGM 
01102 *                                                              *  G7B1PGM 
01103 * 2200  DO SCREEN LOGICAL EDITS                                *  G7B1PGM 
01104 *                                                              *  G7B1PGM 
01105 ****************************************************************  G7B1PGM 
01106  2200-000-LOGICAL-EDITS         SECTION.                          G7B1PGM 
01107  2200-010.                                                        G7B1PGM 
01108                                                                   G7B1PGM 
01109 *----------------------------------------------------------------*G7B1PGM 
01110 *                                                                *G7B1PGM 
01111 *  IF   HOSPITAL ADMISSION RESTRICTION IND (S1HADMR) > ZERO      *G7B1PGM 
01112 *                                                                *G7B1PGM 
01113 *  THEN HOSPITAL ADMISSION RESTRICTION DAYS(S1HADRD):            *G7B1PGM 
01114 *                      MUST BE > ZERO.                           *G7B1PGM 
01115 *                                                                *G7B1PGM 
01116 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7B1PGM 
01117 *                                                                *G7B1PGM 
01118 *----------------------------------------------------------------*G7B1PGM 
01119                                                                   G7B1PGM 
01120      IF  S1HADMRI     > ZEROS                                     G7B1PGM 
01121          AND                                                      G7B1PGM 
01122          S1HADRDI NOT > ZEROS                                     G7B1PGM 
01123      THEN                                                         G7B1PGM 
01124          MOVE  -1        TO  S1HADRDL                             G7B1PGM 
01125          MOVE  DFHBMUBF  TO  S1HADRDA                             G7B1PGM 
01126                              S1HADMRA                             G7B1PGM 
01127          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01128          THEN                                                     G7B1PGM 
01129              NEXT SENTENCE                                        G7B1PGM 
01130          ELSE                                                     G7B1PGM 
01131              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01132              SET WT-01-INDEX TO +02                               G7B1PGM 
01133              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7B1PGM 
01134      ELSE                                                         G7B1PGM 
01135          NEXT SENTENCE.                                           G7B1PGM 
01136                                                                   G7B1PGM 
01137      IF  S1HADRDI     > ZEROS                                     G7B1PGM 
01138          AND                                                      G7B1PGM 
01139          S1HADMRI NOT > ZEROS                                     G7B1PGM 
01140      THEN                                                         G7B1PGM 
01141          MOVE  -1        TO  S1HADMRL                             G7B1PGM 
01142          MOVE  DFHBMUBF  TO  S1HADMRA                             G7B1PGM 
01143                              S1HADRDA                             G7B1PGM 
01144          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01145          THEN                                                     G7B1PGM 
01146              NEXT SENTENCE                                        G7B1PGM 
01147          ELSE                                                     G7B1PGM 
01148              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01149              SET WT-01-INDEX TO +03                               G7B1PGM 
01150              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7B1PGM 
01151      ELSE                                                         G7B1PGM 
01152          NEXT SENTENCE.                                           G7B1PGM 
01153                                                                   G7B1PGM 
01154                                                                   G7B1PGM 
01155 *----------------------------------------------------------------*G7B1PGM 
01156 *                                                                *G7B1PGM 
01157 *  IF   STAY CODE INDICATOR (S1STCDI) > ZERO                     *G7B1PGM 
01158 *                                                                *G7B1PGM 
01159 *  THEN STAY CODE (S1STYCD) MUST BE > ZERO                       *G7B1PGM 
01160 *                                                                *G7B1PGM 
01161 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7B1PGM 
01162 *                                                                *G7B1PGM 
01163 *----------------------------------------------------------------*G7B1PGM 
01164                                                                   G7B1PGM 
01165      IF  S1STCDII     > ZEROS                                     G7B1PGM 
01166          AND                                                      G7B1PGM 
01167          S1STYCDI NOT > ZEROS                                     G7B1PGM 
01168      THEN                                                         G7B1PGM 
01169          MOVE  -1        TO  S1STYCDL                             G7B1PGM 
01170          MOVE  DFHBMUBF  TO  S1STYCDA                             G7B1PGM 
01171                              S1STCDIA                             G7B1PGM 
01172          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01173          THEN                                                     G7B1PGM 
01174              NEXT SENTENCE                                        G7B1PGM 
01175          ELSE                                                     G7B1PGM 
01176              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01177              SET WT-01-INDEX TO +04                               G7B1PGM 
01178              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7B1PGM 
01179      ELSE                                                         G7B1PGM 
01180          NEXT SENTENCE.                                           G7B1PGM 
01181                                                                   G7B1PGM 
01182      IF  S1STYCDI     > ZEROS                                     G7B1PGM 
01183          AND                                                      G7B1PGM 
01184          S1STCDII NOT > ZEROS                                     G7B1PGM 
01185      THEN                                                         G7B1PGM 
01186          MOVE  -1        TO  S1STCDIL                             G7B1PGM 
01187          MOVE  DFHBMUBF  TO  S1STCDIA                             G7B1PGM 
01188                              S1STYCDA                             G7B1PGM 
01189          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01190          THEN                                                     G7B1PGM 
01191              NEXT SENTENCE                                        G7B1PGM 
01192          ELSE                                                     G7B1PGM 
01193              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01194              SET WT-01-INDEX TO +05                               G7B1PGM 
01195              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7B1PGM 
01196      ELSE                                                         G7B1PGM 
01197          NEXT SENTENCE.                                           G7B1PGM 
01198                                                                   G7B1PGM 
01199                                                                   G7B1PGM 
01200 *----------------------------------------------------------------*G7B1PGM 
01201 *                                                                *G7B1PGM 
01202 *  IF   TREATMENT TIME IND  (S1TRTMI) > ZERO                     *G7B1PGM 
01203 *                                                                *G7B1PGM 
01204 *  THEN TREATMENT TIME FACTOR (S1TRTMF) MUST BE > ZERO           *G7B1PGM 
01205 *                                                                *G7B1PGM 
01206 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7B1PGM 
01207 *                                                                *G7B1PGM 
01208 *----------------------------------------------------------------*G7B1PGM 
01209                                                                   G7B1PGM 
01210      IF  S1TRTMII     > ZEROS                                     G7B1PGM 
01211          AND                                                      G7B1PGM 
01212          S1TRTMFI NOT > ZEROS                                     G7B1PGM 
01213      THEN                                                         G7B1PGM 
01214          MOVE  -1        TO  S1TRTMFL                             G7B1PGM 
01215          MOVE  DFHBMUBF  TO  S1TRTMFA                             G7B1PGM 
01216                              S1TRTMIA                             G7B1PGM 
01217          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01218          THEN                                                     G7B1PGM 
01219              NEXT SENTENCE                                        G7B1PGM 
01220          ELSE                                                     G7B1PGM 
01221              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01222              SET WT-01-INDEX TO +14                               G7B1PGM 
01223              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7B1PGM 
01224      ELSE                                                         G7B1PGM 
01225          NEXT SENTENCE.                                           G7B1PGM 
01226                                                                   G7B1PGM 
01227      IF  S1TRTMFI     > ZEROS                                     G7B1PGM 
01228          AND                                                      G7B1PGM 
01229          S1TRTMII NOT > ZEROS                                     G7B1PGM 
01230      THEN                                                         G7B1PGM 
01231          MOVE  -1        TO  S1TRTMIL                             G7B1PGM 
01232          MOVE  DFHBMUBF  TO  S1TRTMIA                             G7B1PGM 
01233                              S1TRTMFA                             G7B1PGM 
01234          IF  WS-02-SCREEN-HAS-ERRORS                              G7B1PGM 
01235          THEN                                                     G7B1PGM 
01236              NEXT SENTENCE                                        G7B1PGM 
01237          ELSE                                                     G7B1PGM 
01238              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B1PGM 
01239              SET WT-01-INDEX TO +12                               G7B1PGM 
01240              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7B1PGM 
01241      ELSE                                                         G7B1PGM 
01242          NEXT SENTENCE.                                           G7B1PGM 
01243                                                                   G7B1PGM 
01244                                                                   G7B1PGM 
01245 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7B1PGM 
01246                                                                   G7B1PGM 
01247      IF  WS-02-SCREEN-HAS-ERRORS                                  G7B1PGM 
01248      THEN                                                         G7B1PGM 
01249          NEXT SENTENCE                                            G7B1PGM 
01250      ELSE                                                         G7B1PGM 
01251          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7B1PGM 
01252          THEN                                                     G7B1PGM 
01253              IF EIBAID = DFHPF4 OR DFHPF16                        G7B1PGM 
01254              THEN                                                 G7B1PGM 
01255                  NEXT SENTENCE                                    G7B1PGM 
01256              ELSE                                                 G7B1PGM 
01257                  MOVE  -1        TO S1ERRL                        G7B1PGM 
01258                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B1PGM 
01259                  SET WT-01-INDEX TO +06                           G7B1PGM 
01260                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B1PGM 
01261          ELSE                                                     G7B1PGM 
01262              NEXT SENTENCE.                                       G7B1PGM 
01263                                                                   G7B1PGM 
01264                                                                   G7B1PGM 
01265  2200-900-EXIT.                                                   G7B1PGM 
01266      EXIT.                                                        G7B1PGM 
01267 /***************************************************************  G7B1PGM 
01268 *                                                              *  G7B1PGM 
01269 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7B1PGM 
01270 *        REWRITE TO WORKFILE.                                  *  G7B1PGM 
01271 *                                                              *  G7B1PGM 
01272 ****************************************************************  G7B1PGM 
01273  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7B1PGM 
01274  2300-010.                                                        G7B1PGM 
01275                                                                   G7B1PGM 
01276 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7B1PGM 
01277                                                                   G7B1PGM 
01278      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7B1PGM 
01279      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7B1PGM 
01280      TO   GCP2-COUNT-TAB-PROVN-POINTERS.                          G7B1PGM 
01281      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7B1PGM 
01282      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7B1PGM 
01283      IF  NOT GCIO2-GOOD-RETURN                                    G7B1PGM 
01284          MOVE WS-01-ABCODE-B1F2     TO WS-01-ABCODE               G7B1PGM 
01285          MOVE WS-01-ABCODE-B1F2-MSG TO WS-01-ABCODE-MSG           G7B1PGM 
01286          PERFORM  9999-000-ABEND-THE-TASK.                        G7B1PGM 
01287                                                                   G7B1PGM 
01288                                                                   G7B1PGM 
01289 *----- SAVE FIELDS FROM SCREEN THAT CANNOT BE DIRECTLY ----------*G7B1PGM 
01290 *        COMPARED TO THE RECORD                                   G7B1PGM 
01291                                                                   G7B1PGM 
01292      MOVE S1HADRDI    TO WS-02-HSP-ADM-RESTRN-DAYS-X.             G7B1PGM 
01293      MOVE S1STYCDI    TO WS-02-STAY-CD-X.                         G7B1PGM 
01294      MOVE S1TRTMFI    TO WS-02-TREAT-TIME-FACTOR.                 G7B1PGM 
01295                                                                   G7B1PGM 
01296                                                                   G7B1PGM 
01297 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7B1PGM 
01298                                                                   G7B1PGM 
01299      IF     S1HADMRI              =  GPB2-HOSP-ADM-RESTRN-IND     G7B1PGM 
01300         AND WS-02-HSP-ADM-RESTRN-DAYS                             G7B1PGM 
01301                                   =  GPB2-HSP-ADM-RESTRN-DAYS     G7B1PGM 
01302         AND S1HCNDRI              =  GPB2-HOSP-COND-RELATSP-IND   G7B1PGM 
01303         AND S1RHADRI              =  GPB2-REHAB-ADM-RESTRN-IND    G7B1PGM 
01304         AND S1STCDII              =  GPB2-STAY-CODE-IND           G7B1PGM 
01305         AND WS-02-STAY-CD         =  GPB2-STAY-CD                 G7B1PGM 
01306         AND S1TRTMII              =  GPB2-TREAT-TIME-FACTOR-IND   G7B1PGM 
01307         AND WS-02-TREAT-TIME-FACTOR                               G7B1PGM 
01308                                   =  GPB2-TREAT-TIME-FACTOR       G7B1PGM 
01309         AND S1ELMTII              =  GPB2-ELIG-METHD-OF-TREAT-IND G7B1PGM 
01310         AND S1RRRSTI              =  GPB2-REPR-REPLAC-RESTRN-IND  G7B1PGM 
01311         AND S1RRCERI              =  GPB2-CERTN-REPETN-REQRD-IND  G7B1PGM 
01312      THEN                                                         G7B1PGM 
01313          GO TO 2300-900-EXIT                                      G7B1PGM 
01314      ELSE                                                         G7B1PGM 
01315          NEXT SENTENCE.                                           G7B1PGM 
01316                                                                   G7B1PGM 
01317                                                                   G7B1PGM 
01318 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7B1PGM 
01319                                                                   G7B1PGM 
01320      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7B1PGM 
01321      TO   GCP2-COUNT-TAB-PROVN-POINTERS.                          G7B1PGM 
01322      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7B1PGM 
01323      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7B1PGM 
01324      IF  NOT GCIO2-GOOD-RETURN                                    G7B1PGM 
01325          MOVE WS-01-ABCODE-B1F3     TO WS-01-ABCODE               G7B1PGM 
01326          MOVE WS-01-ABCODE-B1F3-MSG TO WS-01-ABCODE-MSG           G7B1PGM 
01327          PERFORM  9999-000-ABEND-THE-TASK.                        G7B1PGM 
01328                                                                   G7B1PGM 
01329                                                                   G7B1PGM 
01330 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7B1PGM 
01331                                                                   G7B1PGM 
01332      MOVE S1HADMRI              TO GPB2-HOSP-ADM-RESTRN-IND.      G7B1PGM 
01333      MOVE WS-02-HSP-ADM-RESTRN-DAYS                               G7B1PGM 
01334                                 TO GPB2-HSP-ADM-RESTRN-DAYS.      G7B1PGM 
01335      MOVE S1HCNDRI              TO GPB2-HOSP-COND-RELATSP-IND.    G7B1PGM 
01336      MOVE S1RHADRI              TO GPB2-REHAB-ADM-RESTRN-IND.     G7B1PGM 
01337      MOVE S1STCDII              TO GPB2-STAY-CODE-IND.            G7B1PGM 
01338      MOVE WS-02-STAY-CD         TO GPB2-STAY-CD.                  G7B1PGM 
01339      MOVE S1TRTMII              TO GPB2-TREAT-TIME-FACTOR-IND.    G7B1PGM 
01340      MOVE WS-02-TREAT-TIME-FACTOR                                 G7B1PGM 
01341                                 TO GPB2-TREAT-TIME-FACTOR.        G7B1PGM 
01342      MOVE S1ELMTII              TO GPB2-ELIG-METHD-OF-TREAT-IND.  G7B1PGM 
01343      MOVE S1RRRSTI              TO GPB2-REPR-REPLAC-RESTRN-IND.   G7B1PGM 
01344      MOVE S1RRCERI              TO GPB2-CERTN-REPETN-REQRD-IND.   G7B1PGM 
01345                                                                   G7B1PGM 
01346                                                                   G7B1PGM 
01347 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7B1PGM 
01348                                                                   G7B1PGM 
01349 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7B1PGM 
01350                                                                   G7B1PGM 
01351      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7B1PGM 
01352      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7B1PGM 
01353      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7B1PGM 
01354      IF  NOT GCIO2-GOOD-RETURN                                    G7B1PGM 
01355          MOVE WS-01-ABCODE-B1F4     TO WS-01-ABCODE               G7B1PGM 
01356          MOVE WS-01-ABCODE-B1F4-MSG TO WS-01-ABCODE-MSG           G7B1PGM 
01357          PERFORM  9999-000-ABEND-THE-TASK.                        G7B1PGM 
01358                                                                   G7B1PGM 
01359  2300-900-EXIT.                                                   G7B1PGM 
01360      EXIT.                                                        G7B1PGM 
01361 /***************************************************************  G7B1PGM 
01362 *                                                              *  G7B1PGM 
01363 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7B1PGM 
01364 *                                                              *  G7B1PGM 
01365 ****************************************************************  G7B1PGM 
01366  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7B1PGM 
01367  2310-010.                                                        G7B1PGM 
01368                                                                   G7B1PGM 
01369                                                                   G7B1PGM 
01370 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7B1PGM 
01371                                                                   G7B1PGM 
01372      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7B1PGM 
01373                                         + GC-WORKFILE-KEY-LEN     G7B1PGM 
01374                                         + GC-GCBENPRV-MAX-REC-LEN.G7B1PGM 
01375                                                                   G7B1PGM 
01376 ***  EXEC CICS  GETMAIN  SET    (BEN-PROV-PNTR)                   G7B1PGM 
01377      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7B1PGM 
01378                          INITIMG(WS-02-HEX-00)                    G7B1PGM 
01379                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7B1PGM 
01380                          END-EXEC.                                G7B1PGM 
01381                                                                   G7B1PGM 
01382 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-AREA.                       G7B1PGM 
01383                                                                   G7B1PGM 
01384 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7B1PGM 
01385                                                                   G7B1PGM 
01386      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7B1PGM 
01387      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7B1PGM 
01388      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7B1PGM 
01389      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7B1PGM 
01390      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7B1PGM 
01391      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7B1PGM 
01392                                                                   G7B1PGM 
01393      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7B1PGM 
01394      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7B1PGM 
01395      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7B1PGM 
01396      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7B1PGM 
01397      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7B1PGM 
01398      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7B1PGM 
01399      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7B1PGM 
01400      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7B1PGM 
01401      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7B1PGM 
01402                                                                   G7B1PGM 
01403 ***  MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7B1PGM 
01404 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7B1PGM 
01405 ***  IF  HGADATE-RETURN = ZEROS                                   G7B1PGM 
01406 ***  THEN                                                         G7B1PGM 
01407 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7B1PGM 
01408 ***  ELSE                                                         G7B1PGM 
01409 ***      SET WT-01-INDEX TO +07                                   G7B1PGM 
01410 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7B1PGM 
01411 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7B1PGM 
01412                                                                   G7B1PGM 
01413      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7B1PGM 
01414      MOVE S1BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7B1PGM 
01415      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7B1PGM 
01416      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7B1PGM 
01417      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7B1PGM 
01418      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7B1PGM 
01419      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7B1PGM 
01420                                                                   G7B1PGM 
01421                                                                   G7B1PGM 
01422  2310-900-EXIT.                                                   G7B1PGM 
01423      EXIT.                                                        G7B1PGM 
01424 /***************************************************************  G7B1PGM 
01425 *                                                              *  G7B1PGM 
01426 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7B1PGM 
01427 *                                                              *  G7B1PGM 
01428 ****************************************************************  G7B1PGM 
01429  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7B1PGM 
01430  2400-010.                                                        G7B1PGM 
01431                                                                   G7B1PGM 
01432                                                                   G7B1PGM 
01433      IF  EIBAID = DFHPF7  OR DFHPF19                              G7B1PGM 
01434      THEN                                                         G7B1PGM 
01435          MOVE 'GC6CPGM' TO WS-02-NEXT-PROGRAM.                    G7B1PGM 
01436                                                                   G7B1PGM 
01437                                                                   G7B1PGM 
01438      IF  EIBAID = DFHENTER OR                                     G7B1PGM 
01439                   DFHPF4   OR DFHPF16 OR                          G7B1PGM 
01440                   DFHPF8   OR DFHPF20                             G7B1PGM 
01441      THEN                                                         G7B1PGM 
01442          MOVE 'G7B2PGM' TO WS-02-NEXT-PROGRAM.                    G7B1PGM 
01443                                                                   G7B1PGM 
01444      IF  EIBAID = DFHPF6  OR DFHPF18                              G7B1PGM 
01445      THEN                                                         G7B1PGM 
01446          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7B1PGM 
01447                                                                   G7B1PGM 
01448                                                                   G7B1PGM 
01449      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7B1PGM 
01450                       COMMAREA(WORK-RECORD-2)                     G7B1PGM 
01451                       LENGTH  (GCIO2-RECORD-LENGTH)               G7B1PGM 
01452                       END-EXEC.                                   G7B1PGM 
01453                                                                   G7B1PGM 
01454  2400-900-EXIT.                                                   G7B1PGM 
01455      EXIT.                                                        G7B1PGM 
01456 /***************************************************************  G7B1PGM 
01457 *                                                              *  G7B1PGM 
01458 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7B1PGM 
01459 *         PROVISION RECORD (TYPE=C4)                           *  G7B1PGM 
01460 *                                                              *  G7B1PGM 
01461 ****************************************************************  G7B1PGM 
01462  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7B1PGM 
01463  5000-010.                                                        G7B1PGM 
01464                                                                   G7B1PGM 
01465      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7B1PGM 
01466                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7B1PGM 
01467                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7B1PGM 
01468                       END-EXEC.                                   G7B1PGM 
01469                                                                   G7B1PGM 
01470                                                                   G7B1PGM 
01471  5000-900-EXIT.                                                   G7B1PGM 
01472      EXIT.                                                        G7B1PGM 
01473 /***************************************************************  G7B1PGM 
01474 *                                                              *  G7B1PGM 
01475 * 5100                                                         *  G7B1PGM 
01476 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7B1PGM 
01477 *                                                              *  G7B1PGM 
01478 ****************************************************************  G7B1PGM 
01479  5100-000-W-F-CONTRACT-IO       SECTION.                          G7B1PGM 
01480  5100-010.                                                        G7B1PGM 
01481                                                                   G7B1PGM 
01482      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7B1PGM 
01483                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7B1PGM 
01484                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7B1PGM 
01485                       END-EXEC.                                   G7B1PGM 
01486                                                                   G7B1PGM 
01487                                                                   G7B1PGM 
01488  5100-900-EXIT.                                                   G7B1PGM 
01489      EXIT.                                                        G7B1PGM 
01490 /***************************************************************  G7B1PGM 
01491 *                                                              *  G7B1PGM 
01492 * 9000   MOVE MESSAGE TO SCREEN                                *  G7B1PGM 
01493 *                                                              *  G7B1PGM 
01494 ****************************************************************  G7B1PGM 
01495  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7B1PGM 
01496  9000-010.                                                        G7B1PGM 
01497                                                                   G7B1PGM 
01498      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S1ERRO.              G7B1PGM 
01499                                                                   G7B1PGM 
01500  9000-900-EXIT.                                                   G7B1PGM 
01501      EXIT.                                                        G7B1PGM 
01502 /***************************************************************  G7B1PGM 
01503 *                                                              *  G7B1PGM 
01504 * 9100 SEND SCREEN AND RETURN                                  *  G7B1PGM 
01505 *                                                              *  G7B1PGM 
01506 ****************************************************************  G7B1PGM 
01507  9100-000-SEND-THEN-RETURN      SECTION.                          G7B1PGM 
01508  9100-010.                                                        G7B1PGM 
01509                                                                   G7B1PGM 
01510                                                                   G7B1PGM 
01511 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7B1PGM 
01512      MOVE  -1 TO  S1ERRL.                                         G7B1PGM 
01513                                                                   G7B1PGM 
01514                                                                   G7B1PGM 
01515      IF  WS-02-MY-EIBTRNID                                        G7B1PGM 
01516      THEN                                                         G7B1PGM 
01517          EXEC CICS  SEND MAP('G7B1I01')                           G7B1PGM 
01518                          MAPSET('G7B1SET')                        G7B1PGM 
01519                          DATAONLY                                 G7B1PGM 
01520                          CURSOR                                   G7B1PGM 
01521                          END-EXEC                                 G7B1PGM 
01522      ELSE                                                         G7B1PGM 
01523          EXEC CICS  SEND MAP('G7B1I01')                           G7B1PGM 
01524                          MAPSET('G7B1SET')                        G7B1PGM 
01525                          ERASE                                    G7B1PGM 
01526                          CURSOR                                   G7B1PGM 
01527                          END-EXEC.                                G7B1PGM 
01528                                                                   G7B1PGM 
01529      EXEC CICS RETURN                                             G7B1PGM 
01530                TRANSID  ('G7B1')                                  G7B1PGM 
01531                COMMAREA (DFHCOMMAREA)                             G7B1PGM 
01532                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7B1PGM 
01533                END-EXEC.                                          G7B1PGM 
01534 *                                                                 G7B1PGM 
01535  9100-900-EXIT.                                                   G7B1PGM 
01536      EXIT.                                                        G7B1PGM 
01537 /*****************************************************************G7B1PGM 
01538 *                                                                *G7B1PGM 
01539 * 9200    XCTL TO GCPSPGM                                        *G7B1PGM 
01540 *                                                                *G7B1PGM 
01541 *                                                                *G7B1PGM 
01542 ******************************************************************G7B1PGM 
01543  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7B1PGM 
01544  9200-010.                                                        G7B1PGM 
01545                                                                   G7B1PGM 
01546      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7B1PGM 
01547                       END-EXEC.                                   G7B1PGM 
01548                                                                   G7B1PGM 
01549  9200-900-EXIT.                                                   G7B1PGM 
01550      EXIT.                                                        G7B1PGM 
01551 /*****************************************************************G7B1PGM 
01552 *                                                                *G7B1PGM 
01553 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7B1PGM 
01554 *                                                                *G7B1PGM 
01555 *                                                                *G7B1PGM 
01556 ******************************************************************G7B1PGM 
01557  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7B1PGM 
01558  9210-010.                                                        G7B1PGM 
01559                                                                   G7B1PGM 
01560      IF  S1GRPNOI = '000SPS000'                                   G7B1PGM 
01561          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7B1PGM 
01562                           END-EXEC.                               G7B1PGM 
01563                                                                   G7B1PGM 
01564 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7B1PGM 
01565                                                                   G7B1PGM 
01566      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7B1PGM 
01567                                        + GC-WORKFILE-KEY-LEN      G7B1PGM 
01568                                        + GC-GCCONTR-MAX-REC-LEN.  G7B1PGM 
01569                                                                   G7B1PGM 
01570 ***  EXEC CICS  GETMAIN  SET    (CONTRACT-PNTR)                   G7B1PGM 
01571      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7B1PGM 
01572                          INITIMG(WS-02-HEX-00)                    G7B1PGM 
01573                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7B1PGM 
01574                          END-EXEC.                                G7B1PGM 
01575                                                                   G7B1PGM 
01576 ***  COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7B1PGM 
01577 ***  SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7B1PGM 
01578                                                                   G7B1PGM 
01579 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7B1PGM 
01580                                                                   G7B1PGM 
01581      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7B1PGM 
01582      TO   GCT2-COUNT-BEN-PROVN-POINTERS.                          G7B1PGM 
01583      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7B1PGM 
01584      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7B1PGM 
01585                                                                   G7B1PGM 
01586      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7B1PGM 
01587      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7B1PGM 
01588      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7B1PGM 
01589      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7B1PGM 
01590      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7B1PGM 
01591      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7B1PGM 
01592      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7B1PGM 
01593      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7B1PGM 
01594      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7B1PGM 
01595      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7B1PGM 
01596      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7B1PGM 
01597      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7B1PGM 
01598      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7B1PGM 
01599      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7B1PGM 
01600                                                                   G7B1PGM 
01601 ***  MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7B1PGM 
01602 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7B1PGM 
01603 ***  IF  HGADATE-RETURN = ZEROS                                   G7B1PGM 
01604 ***  THEN                                                         G7B1PGM 
01605 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7B1PGM 
01606 ***  ELSE                                                         G7B1PGM 
01607 ***      SET WT-01-INDEX TO +07                                   G7B1PGM 
01608 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7B1PGM 
01609 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7B1PGM 
01610                                                                   G7B1PGM 
01611      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7B1PGM 
01612      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7B1PGM 
01613      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7B1PGM 
01614      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7B1PGM 
01615      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7B1PGM 
01616      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7B1PGM 
01617      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7B1PGM 
01618                                                                   G7B1PGM 
01619      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7B1PGM 
01620                                                                   G7B1PGM 
01621      IF  NOT GCIO3-GOOD-RETURN                                    G7B1PGM 
01622          MOVE WS-01-ABCODE-B1F1     TO WS-01-ABCODE               G7B1PGM 
01623          MOVE WS-01-ABCODE-B1F1-MSG TO WS-01-ABCODE-MSG           G7B1PGM 
01624          PERFORM  9999-000-ABEND-THE-TASK.                        G7B1PGM 
01625                                                                   G7B1PGM 
01626      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7B1PGM 
01627                       COMMAREA(WORK-RECORD-3)                     G7B1PGM 
01628                       LENGTH  (GCIO3-RECORD-LENGTH)               G7B1PGM 
01629                       END-EXEC.                                   G7B1PGM 
01630                                                                   G7B1PGM 
01631  9210-900-EXIT.                                                   G7B1PGM 
01632      EXIT.                                                        G7B1PGM 
01633 /*****************************************************************G7B1PGM 
01634 *                                                                *G7B1PGM 
01635 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7B1PGM 
01636 *                                                                *G7B1PGM 
01637 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7B1PGM 
01638 *                                                                *G7B1PGM 
01639 ******************************************************************G7B1PGM 
01640  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7B1PGM 
01641  9800-010.                                                        G7B1PGM 
01642                                                                   G7B1PGM 
01643      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7B1PGM 
01644      MOVE 'M'   TO  HGADATE-FORM1.                                G7B1PGM 
01645      MOVE 'J'   TO  HGADATE-FORM2.                                G7B1PGM 
01646      MOVE ZEROS TO  HGADATE-RETURN                                G7B1PGM 
01647                     HGADATE-AMOUNT.                               G7B1PGM 
01648      EXEC CICS LINK PROGRAM ('HGADATES')                          G7B1PGM 
01649                     COMMAREA(HGADATES-COMMAREA)                   G7B1PGM 
01650                     LENGTH  (24)                                  G7B1PGM 
01651                     END-EXEC.                                     G7B1PGM 
01652                                                                   G7B1PGM 
01653  9800-900-900-EXIT.                                               G7B1PGM 
01654      EXIT.                                                        G7B1PGM 
01655 /*****************************************************************G7B1PGM 
01656 *                                                                *G7B1PGM 
01657 * 9810    J U L I A N    T O    G R E G O R I A N                *G7B1PGM 
01658 *                                                                *G7B1PGM 
01659 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7B1PGM 
01660 *                                                                *G7B1PGM 
01661 ******************************************************************G7B1PGM 
01662  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7B1PGM 
01663  9810-010.                                                        G7B1PGM 
01664                                                                   G7B1PGM 
01665      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7B1PGM 
01666      MOVE 'J'   TO  HGADATE-FORM1.                                G7B1PGM 
01667      MOVE 'M'   TO  HGADATE-FORM2.                                G7B1PGM 
01668      MOVE ZEROS TO  HGADATE-RETURN                                G7B1PGM 
01669                     HGADATE-AMOUNT.                               G7B1PGM 
01670      EXEC CICS LINK PROGRAM ('HGADATES')                          G7B1PGM 
01671                     COMMAREA(HGADATES-COMMAREA)                   G7B1PGM 
01672                     LENGTH  (24)                                  G7B1PGM 
01673                     END-EXEC.                                     G7B1PGM 
01674                                                                   G7B1PGM 
01675  9810-900-900-EXIT.                                               G7B1PGM 
01676      EXIT.                                                        G7B1PGM 
01677 /***************************************************************  G7B1PGM 
01678 *                                                              *  G7B1PGM 
01679 * 9999  ABEND THE TASK                                         *  G7B1PGM 
01680 *                                                              *  G7B1PGM 
01681 ****************************************************************  G7B1PGM 
01682  9999-000-ABEND-THE-TASK SECTION.                                 G7B1PGM 
01683  9999-010.                                                        G7B1PGM 
01684                                                                   G7B1PGM 
01685      EXEC CICS  ABEND                                             G7B1PGM 
01686                 ABCODE(WS-01-ABCODE)                              G7B1PGM 
01687                 END-EXEC.                                         G7B1PGM 
01688                                                                   G7B1PGM 
01689  9900-900-EXIT.                                                   G7B1PGM 
01690      EXIT.                                                        G7B1PGM 
