00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7D1PGM.                                         G7D1PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7D1PGM 
00005  DATE-WRITTEN.   03/12/87.                                        G7D1PGM 
00006  DATE-COMPILED.                                                   G7D1PGM 
00007 ***************************************************************** G7D1PGM 
00008 *                                                               * G7D1PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7D1PGM 
00010 *                                                               * G7D1PGM 
00011 *                                                               * G7D1PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7D1PGM 
00013 *                                                               * G7D1PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7D1PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7D1PGM 
00016 *                             (SAME AS 'GC5A')                  * G7D1PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7D1PGM 
00018 *                              'GC5A')                          * G7D1PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7D1PGM 
00020 *                              PROVISION)                       * G7D1PGM 
00021 *                                                               * G7D1PGM 
00022 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7D1PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7D1PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7D1PGM 
00025 *                                                               * G7D1PGM 
00026 *  M216       1/07/88  FCG    ADDED LOGIC FOR EXCEPTION SCHEDULE* G7D1PGM 
00027 *                             ID TO USE FIELD VALIDATION SUB-   * G7D1PGM 
00028 *                             SYSTEM.                           * G7D1PGM 
00029 *                                                               * G7D1PGM 
00030 *  D129      09/18/89  GDM    CONVERT FOR DECIMAL               * G7D1PGM 
00031 *                                                               * G7D1PGM 
00032 *  D129      09/19/89  GDM    CONVERT TOP VS COBOL/2            * G7D1PGM 
00033 *                                                               * G7D1PGM 
00034 *  D12009    08/23/91  BSO   -CORRECT ERR MESSAGES IN AREA \
00035 *                            -CORRECT ALPHA CLASS TEST          * G7D1PGM 
00036 *                                                               * G7D1PGM 
00037 *  XXXXX   09/09/91  ENW  CORRECTED EDIT FOR BENEFIT-SCOPE-ID.  * G7D1PGM 
00038 *                                                               * G7D1PGM 
00039 *  D14726    10/28/97  GDM 1. ADDED MILLENNIUM PROCESSING FOR   * G7D1PGM 
00040 *                             DATE                              * G7D1PGM 
00041 *                          2. EXPAND THE COMMAREA KEY TO        * G7D1PGM 
00042 *                             SUPPORT THE TEXAS MERGER.         * G7D1PGM 
00043 *                                                               * G7D1PGM 
00044 * 14726/     03/30/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7D1PGM 
00045 * 15057                     INCREASED GROUP AND SECTION ON      * G7D1PGM 
00046 *                           THE SCREEN.                         * G7D1PGM 
00047 *                                                               * G7D1PGM 
00048 *            12/11/02  AKK  OPID COMPILE                        * G7D1PGM 
00049 *                                                               * G7D1PGM 
00050 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7D1PGM 
00051 *                           G7D1SET                              *G7D1PGM 
00052 ***************************************************************** G7D1PGM 
00053                                                                   G7D1PGM 
00054 ***************************************************************** G7D1PGM 
00055 *                                                               * G7D1PGM 
00056 *    G7D1PGM  - PROGRAM 1 OF 2 PROGRAMS TO UPDATE THE FORMAT 'D'* G7D1PGM 
00057 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7D1PGM 
00058 *                                                               * G7D1PGM 
00059 *    TRANSID: G7D1                                              * G7D1PGM 
00060 *    MAPSET:  G7D1SETC    (GID1PGM WHICH SHARES THIS MAP)       * G7D1PGM 
00061 *    VALGEN:  NONE                                              * G7D1PGM 
00062 *                                                               * G7D1PGM 
00063 *    PROGRAM NARRATIVE:                                         * G7D1PGM 
00064 *                                                               * G7D1PGM 
00065 *        PROGRAM CHECKS FOR TRANS CODE 'G7D1'.  AN INVALID      * G7D1PGM 
00066 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7D1PGM 
00067 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7D1PGM 
00068 *                                                               * G7D1PGM 
00069 *        THE MAIN FUNCTIONS ARE :                               * G7D1PGM 
00070 *        1. HARDCOPY REQUEST,                                   * G7D1PGM 
00071 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7D1PGM 
00072 *           USER,                                               * G7D1PGM 
00073 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7D1PGM 
00074 *                                                               * G7D1PGM 
00075 *        HARDCOPY REQUEST                                       * G7D1PGM 
00076 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7D1PGM 
00077 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7D1PGM 
00078 *           THE SCREEN BUFFER.                                  * G7D1PGM 
00079 *                                                               * G7D1PGM 
00080 *        PROCESS INPUT DATA (UPDATE).                           * G7D1PGM 
00081 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7D1PGM 
00082 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7D1PGM 
00083 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7D1PGM 
00084 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7D1PGM 
00085 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7D1PGM 
00086 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7D1PGM 
00087 *                                                               * G7D1PGM 
00088 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7D1PGM 
00089 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7D1PGM 
00090 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7D1PGM 
00091 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7D1PGM 
00092 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7D1PGM 
00093 *                                                               * G7D1PGM 
00094 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7D1PGM 
00095 *                                     -----------------------   * G7D1PGM 
00096 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7D1PGM 
00097 *           -----------------------------------------------     * G7D1PGM 
00098 *                                                               * G7D1PGM 
00099 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7D1PGM 
00100 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7D1PGM 
00101 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7D1PGM 
00102 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7D1PGM 
00103 *           SENT BACK TO THE USER.                              * G7D1PGM 
00104 *                                                               * G7D1PGM 
00105 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7D1PGM 
00106 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7D1PGM 
00107 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7D1PGM 
00108 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7D1PGM 
00109 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7D1PGM 
00110 *           PF KEY LOGIC,                                       * G7D1PGM 
00111 *              PF6/PF18       GOES TO GC8APGM                   * G7D1PGM 
00112 *              PF8/PF20/ENTER GOES TO G7D2PGM                   * G7D1PGM 
00113 *              FOR PF7/PF19   GOES TO GC6CPGM                   * G7D1PGM 
00114 *                                                               * G7D1PGM 
00115 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7D1PGM 
00116 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7D1PGM 
00117 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7D1PGM 
00118 *                                                               * G7D1PGM 
00119 ***************************************************************** G7D1PGM 
00120                                                                   G7D1PGM 
00121  ENVIRONMENT DIVISION.                                            G7D1PGM 
00122  DATA DIVISION.                                                   G7D1PGM 
00123 /                                                                 G7D1PGM 
00124  WORKING-STORAGE SECTION.                                         G7D1PGM 
00125  01  WS-BEGIN                    PIC X(58) VALUE                  G7D1PGM 
00126      '*** G7D1PGM  WORKING-STORAGE BEGINS HERE ***'.              G7D1PGM 
00127                                                                   G7D1PGM 
00128                                                                   G7D1PGM 
00129  01  WS-01-ABEND-AREA.                                            G7D1PGM 
00130      05  FILLER                   PIC X(16)  VALUE                G7D1PGM 
00131          '** ABEND AREA **'.                                      G7D1PGM 
00132                                                                   G7D1PGM 
00133      05  WS-01-ABEND-CODES-AND-MSG.                               G7D1PGM 
00134          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7D1PGM 
00135          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7D1PGM 
00136                                                                   G7D1PGM 
00137          10  WS-01-ABCODE-D1F1          PIC X(04)  VALUE  'D1F1'. G7D1PGM 
00138          10  WS-01-ABCODE-D1F1-MSG      PIC X(44)  VALUE          G7D1PGM 
00139             'W/F CONTRACT CANNOT BE FOUND             '.          G7D1PGM 
00140                                                                   G7D1PGM 
00141          10  WS-01-ABCODE-D1F2          PIC X(04)  VALUE  'D1F2'. G7D1PGM 
00142          10  WS-01-ABCODE-D1F2-MSG      PIC X(44)  VALUE          G7D1PGM 
00143             'W/F BEN PROV CANNOT BE FOUND             '.          G7D1PGM 
00144                                                                   G7D1PGM 
00145          10  WS-01-ABCODE-D1F3          PIC X(04)  VALUE  'D1F3'. G7D1PGM 
00146          10  WS-01-ABCODE-D1F3-MSG      PIC X(44)  VALUE          G7D1PGM 
00147             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7D1PGM 
00148                                                                   G7D1PGM 
00149          10  WS-01-ABCODE-D1F4          PIC X(04)  VALUE  'D1F4'. G7D1PGM 
00150          10  WS-01-ABCODE-D1F4-MSG      PIC X(44)  VALUE          G7D1PGM 
00151             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7D1PGM 
00152                                                                   G7D1PGM 
00153          10  WS-01-ABCODE-D1L1          PIC X(04)  VALUE  'D1L1'. G7D1PGM 
00154          10  WS-01-ABCODE-D1L1-MSG      PIC X(44)  VALUE          G7D1PGM 
00155             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7D1PGM 
00156                                                                   G7D1PGM 
00157          10  WS-01-ABCODE-D1P1          PIC X(04)  VALUE  'D1P1'. G7D1PGM 
00158          10  WS-01-ABCODE-D1P1-MSG      PIC X(44)  VALUE          G7D1PGM 
00159             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7D1PGM 
00160                                                                   G7D1PGM 
00161          10  WS-01-ABCODE-D1P2          PIC X(04)  VALUE  'D1P2'. G7D1PGM 
00162          10  WS-01-ABCODE-D1P2-MSG      PIC X(44)  VALUE          G7D1PGM 
00163             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7D1PGM 
00164                                                                   G7D1PGM 
00165  01  WS-02-AREA.                                                  G7D1PGM 
00166      05  FILLER                   PIC X(16)  VALUE                G7D1PGM 
00167          '** WS-02-AREA **'.                                      G7D1PGM 
00168      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7D1PGM 
00169          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7D1PGM 
00170                                                    'GC6C' 'G7D2'  G7D1PGM 
00171                                                    'G7D1'.        G7D1PGM 
00172          88  WS-02-MY-EIBTRNID                     VALUE  'G7D1'. G7D1PGM 
00173                                                                   G7D1PGM 
00174      05  WS-02-COMPUTED-LENGTHS.                                  G7D1PGM 
00175          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7D1PGM 
00176          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7D1PGM 
00177          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7D1PGM 
00178                                                                   G7D1PGM 
00179      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7D1PGM 
00180                                                                   G7D1PGM 
00181      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7D1PGM 
00182                                                                   G7D1PGM 
00183      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7D1PGM 
00184      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7D1PGM 
00185          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7D1PGM 
00186                                         PIC X.                    G7D1PGM 
00187          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7D1PGM 
00188                                                    '0' THRU '9'   G7D1PGM 
00189                                                    'A' THRU 'I'   G7D1PGM 
00190                                                    'J' THRU 'R'   G7D1PGM 
00191                                                    'S' THRU 'Z'   G7D1PGM 
00192                                                    SPACE.         G7D1PGM 
00193          88  WS-02-CLASS-BLANK                     VALUES         G7D1PGM 
00194                                                    SPACE.         G7D1PGM 
00195                                                                   G7D1PGM 
00196      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7D1PGM 
00197          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7D1PGM 
00198          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7D1PGM 
00199                                                                   G7D1PGM 
00200      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7D1PGM 
00201          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7D1PGM 
00202                                                                   G7D1PGM 
00203      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7D1PGM 
00204                                                                   G7D1PGM 
00205      05  WS-02-HEX-F00000.                                        G7D1PGM 
00206          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7D1PGM 
00207          10  FILLER                     PIC  X(09) VALUE          G7D1PGM 
00208                                                    LOW-VALUES.    G7D1PGM 
00209                                                                   G7D1PGM 
00210      05  WS-02-RATIO                    PIC  9(2)V9 VALUE  ZEROS. G7D1PGM 
00211 *    05  FILLER    REDEFINES  WS-02-RATIO.                        G7D1PGM 
00212 *        10  FILLER                     PIC  X.                   G7D1PGM 
00213 *        10  WS-02-RATIO-DIVISOR        PIC  X.                   G7D1PGM 
00214 *        10  WS-02-RATIO-BASE           PIC  X.                   G7D1PGM 
00215                                                                   G7D1PGM 
00216      05  WS-02-HSP-ADM-RESTRN-DAYS-X.                             G7D1PGM 
00217          10  WS-02-HSP-ADM-RESTRN-DAYS  PIC  9(3)    VALUE ZEROS. G7D1PGM 
00218          10  WS-02-S1HADRD              REDEFINES                 G7D1PGM 
00219              WS-02-HSP-ADM-RESTRN-DAYS  PIC  X(3).                G7D1PGM 
00220                                                                   G7D1PGM 
00221      05  WS-02-STAY-CD-X.                                         G7D1PGM 
00222          10  WS-02-STAY-CD                PIC 9(3)    VALUE ZEROS.G7D1PGM 
00223          10  WS-02-S1STYCD              REDEFINES                 G7D1PGM 
00224              WS-02-STAY-CD                PIC X(3).               G7D1PGM 
00225                                                                   G7D1PGM 
00226      05  WS-02-DAYS-RDCN-RAT-BASIC-AP-X.                          G7D1PGM 
00227          10  WS-02-DAYS-RDCN-RAT-BASIC-APL PIC 99V9  VALUE ZEROS. G7D1PGM 
00228                                                                   G7D1PGM 
00229      05  WS-02-DAYS-RDCN-RAT-BASIC-BA-X.                          G7D1PGM 
00230          10  WS-02-DAYS-RDCN-RAT-BASIC-BASE PIC 99V9  VALUE ZEROS.G7D1PGM 
00231                                                                   G7D1PGM 
00232      05  WS-02-DAYS-RDCN-RAT-SEC-AP-X.                            G7D1PGM 
00233          10  WS-02-DAYS-RDCN-RAT-SEC-APL    PIC 99V9  VALUE ZEROS.G7D1PGM 
00234                                                                   G7D1PGM 
00235      05  WS-02-DAYS-RDCN-RAT-SEC-BA-X.                            G7D1PGM 
00236          10  WS-02-DAYS-RDCN-RAT-SEC-BASE   PIC 99V9  VALUE ZEROS.G7D1PGM 
00237                                                                   G7D1PGM 
00238      05  WS-3POS-MAX-AMT          PIC 99V9 VALUE 99.9.            G7D1PGM 
00239      05  WS-02-DISP-3POS-DEC      PIC 99.9.                       G7D1PGM 
00240      05  WS-GPD2-DAYS-RDCN-RAT-BAS-APL           PIC 99V9.        G7D1PGM 
00241      05  WS-GPD2-DAYS-RDCN-RAT-BAS-BASE          PIC 99V9.        G7D1PGM 
00242      05  WS-GPD2-DAYS-RDCN-RAT-SEC-APL           PIC 99V9.        G7D1PGM 
00243      05  WS-GPD2-DAYS-RDCN-RAT-SEC-BASE          PIC 99V9.        G7D1PGM 
00244 /                                                                 G7D1PGM 
00245  01  WT-00-G7D1PGM-TABLES.                                        G7D1PGM 
00246      05  FILLER                   PIC X(16)  VALUE                G7D1PGM 
00247          '*G7D1PGM TABLES*'.                                      G7D1PGM 
00248                                                                   G7D1PGM 
00249  01  WT-01-TABLE.                                                 G7D1PGM 
00250      05  FILLER                  PIC X(16) VALUE                  G7D1PGM 
00251          '* WT-01-TABLE  *'.                                      G7D1PGM 
00252 ******************************************************************G7D1PGM 
00253 *    WT-01   MESSAGE TABLE                                       *G7D1PGM 
00254 ******************************************************************G7D1PGM 
00255  01  FILLER.                                                      G7D1PGM 
00256      05  WT-01-MESSAGE-VALUES.                                    G7D1PGM 
00257                                                                   G7D1PGM 
00258 *----------------------------------------------------------------*G7D1PGM 
00259          10  WT-01-ENTRY-001.                                     G7D1PGM 
00260              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00261              15  WT-01-MESSAGE-TEXT-001.                          G7D1PGM 
00262                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00263                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00264                  20  FILLER          PIC X(3)  VALUE  '001'.      G7D1PGM 
00265                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00266                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00267                      ' INVALID PFKEY SELECTION                    G7D1PGM 
00268 -                    '                         '.                 G7D1PGM 
00269              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00270                                                                   G7D1PGM 
00271 *----------------------------------------------------------------*G7D1PGM 
00272          10  WT-01-ENTRY-002.                                     G7D1PGM 
00273              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00274              15  WT-01-MESSAGE-TEXT-002.                          G7D1PGM 
00275                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00276                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00277                  20  FILLER          PIC X(3)  VALUE  '002'.      G7D1PGM 
00278                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00279                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00280                      'HOSP ADMISSION RESTRICTION DAYS REQUIRED WHEG7D1PGM 
00281 -                    'N INDICATOR IS CODED     '.                 G7D1PGM 
00282              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00283                                                                   G7D1PGM 
00284 *----------------------------------------------------------------*G7D1PGM 
00285          10  WT-01-ENTRY-003.                                     G7D1PGM 
00286              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00287              15  WT-01-MESSAGE-TEXT-003.                          G7D1PGM 
00288                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00289                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00290                  20  FILLER          PIC X(3)  VALUE  '003'.      G7D1PGM 
00291                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00292                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00293                      'INDICATOR REQUIRED WHEN HOSPITAL ADMISSION RG7D1PGM 
00294 -                    'ESTRICTION DAYS CODED    '.                 G7D1PGM 
00295              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00296                                                                   G7D1PGM 
00297 *----------------------------------------------------------------*G7D1PGM 
00298          10  WT-01-ENTRY-004.                                     G7D1PGM 
00299              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00300              15  WT-01-MESSAGE-TEXT-004.                          G7D1PGM 
00301                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00302                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00303                  20  FILLER          PIC X(3)  VALUE  '004'.      G7D1PGM 
00304                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00305                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00306                      'STAY CODE DAYS REQUIRED WHEN INDICATOR IS C G7D1PGM 
00307 -                    'ODED                     '.                 G7D1PGM 
00308              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00309                                                                   G7D1PGM 
00310 *----------------------------------------------------------------*G7D1PGM 
00311          10  WT-01-ENTRY-005.                                     G7D1PGM 
00312              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00313              15  WT-01-MESSAGE-TEXT-005.                          G7D1PGM 
00314                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00315                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00316                  20  FILLER          PIC X(3)  VALUE  '005'.      G7D1PGM 
00317                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00318                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00319                      'INDICATOR REQUIRED WHEN STAY CODE DAYS IS COG7D1PGM 
00320 -                    'DED                      '.                 G7D1PGM 
00321              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00322                                                                   G7D1PGM 
00323 *----------------------------------------------------------------*G7D1PGM 
00324          10  WT-01-ENTRY-006.                                     G7D1PGM 
00325              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00326              15  WT-01-MESSAGE-TEXT-006.                          G7D1PGM 
00327                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00328                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00329                  20  FILLER          PIC X(3)  VALUE  '006'.      G7D1PGM 
00330                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00331                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00332                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7D1PGM 
00333 -                    ' PF4/PF16 TO CONTINUE    '.                 G7D1PGM 
00334              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00335                                                                   G7D1PGM 
00336 *----------------------------------------------------------------*G7D1PGM 
00337          10  WT-01-ENTRY-007.                                     G7D1PGM 
00338              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00339              15  WT-01-MESSAGE-TEXT-007.                          G7D1PGM 
00340                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00341                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00342                  20  FILLER          PIC X(3)  VALUE  '007'.      G7D1PGM 
00343                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00344                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00345                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7D1PGM 
00346 -                    'E CALL SYSTEMS           '.                 G7D1PGM 
00347              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00348                                                                   G7D1PGM 
00349 *----------------------------------------------------------------*G7D1PGM 
00350          10  WT-01-ENTRY-008.                                     G7D1PGM 
00351              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00352              15  WT-01-MESSAGE-TEXT-008.                          G7D1PGM 
00353                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00354                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00355                  20  FILLER          PIC X(3)  VALUE  '008'.      G7D1PGM 
00356                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00357                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00358                      'FIELD HAS AN INVALID VALUE                  G7D1PGM 
00359 -                    '                         '.                 G7D1PGM 
00360              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00361                                                                   G7D1PGM 
00362 *----------------------------------------------------------------*G7D1PGM 
00363          10  WT-01-ENTRY-009.                                     G7D1PGM 
00364              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00365              15  WT-01-MESSAGE-TEXT-009.                          G7D1PGM 
00366                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00367                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00368                  20  FILLER          PIC X(3)  VALUE  '009'.      G7D1PGM 
00369                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00370                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00371                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7D1PGM 
00372 -                    'YSTEM)                   '.                 G7D1PGM 
00373              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00374                                                                   G7D1PGM 
00375 *----------------------------------------------------------------*G7D1PGM 
00376          10  WT-01-ENTRY-010.                                     G7D1PGM 
00377              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00378              15  WT-01-MESSAGE-TEXT-010.                          G7D1PGM 
00379                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00380                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00381                  20  FILLER          PIC X(3)  VALUE  '010'.      G7D1PGM 
00382                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00383                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00384                      'FILL IN THE FIRST TWO POSITIONS AND SPACE OUG7D1PGM 
00385 -                    'T THE SECOND TWO.        '.                 G7D1PGM 
00386              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00387                                                                   G7D1PGM 
00388 *----------------------------------------------------------------*G7D1PGM 
00389          10  WT-01-ENTRY-011.                                     G7D1PGM 
00390              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00391              15  WT-01-MESSAGE-TEXT-011.                          G7D1PGM 
00392                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00393                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00394                  20  FILLER          PIC X(3)  VALUE  '011'.      G7D1PGM 
00395                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00396                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00397                      '********** F U T U R E   U S E *************G7D1PGM 
00398 -                    '*************************'.                 G7D1PGM 
00399              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00400                                                                   G7D1PGM 
00401 *----------------------------------------------------------------*G7D1PGM 
00402          10  WT-01-ENTRY-012.                                     G7D1PGM 
00403              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00404              15  WT-01-MESSAGE-TEXT-012.                          G7D1PGM 
00405                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00406                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00407                  20  FILLER          PIC X(3)  VALUE  '012'.      G7D1PGM 
00408                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00409                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00410                      'RATIO BASE CANNOT EQUAL ZEROES              G7D1PGM 
00411 -                    '                         '.                 G7D1PGM 
00412              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00413                                                                   G7D1PGM 
00414 *----------------------------------------------------------------*G7D1PGM 
00415          10  WT-01-ENTRY-013.                                     G7D1PGM 
00416              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00417              15  WT-01-MESSAGE-TEXT-013.                          G7D1PGM 
00418                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00419                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00420                  20  FILLER          PIC X(3)  VALUE  '013'.      G7D1PGM 
00421                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00422                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00423                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7D1PGM 
00424 -                    '                         '.                 G7D1PGM 
00425              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00426                                                                   G7D1PGM 
00427 *----------------------------------------------------------------*G7D1PGM 
00428          10  WT-01-ENTRY-014.                                     G7D1PGM 
00429              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00430              15  WT-01-MESSAGE-TEXT-014.                          G7D1PGM 
00431                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00432                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00433                  20  FILLER          PIC X(3)  VALUE  '014'.      G7D1PGM 
00434                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00435                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00436                      'FIELD EXCEEDS LENGTH OF 3 POSITIONS. FORMAT G7D1PGM 
00437 -                    'IS 99.9                  '.                 G7D1PGM 
00438              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00439                                                                   G7D1PGM 
00440 *----------------------------------------------------------------*G7D1PGM 
00441          10  WT-01-ENTRY-015.                                     G7D1PGM 
00442              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D1PGM 
00443              15  WT-01-MESSAGE-TEXT-015.                          G7D1PGM 
00444                  20  FILLER          PIC X(4)  VALUE  'G7D1'.     G7D1PGM 
00445                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D1PGM 
00446                  20  FILLER          PIC X(3)  VALUE  '015'.      G7D1PGM 
00447                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D1PGM 
00448                  20  FILLER          PIC X(70) VALUE              G7D1PGM 
00449                      ' INVALID DECIMAL DETECTED '.                G7D1PGM 
00450              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D1PGM 
00451                                                                   G7D1PGM 
00452 *----------------------------------------------------------------*G7D1PGM 
00453                                                                   G7D1PGM 
00454      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7D1PGM 
00455          WT-01-MESSAGE-VALUES         OCCURS 015 TIMES            G7D1PGM 
00456                                      INDEXED BY WT-01-INDEX.      G7D1PGM 
00457          10  WT-01-ENTRY.                                         G7D1PGM 
00458              15  FILLER              PIC X(02).                   G7D1PGM 
00459              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7D1PGM 
00460              15  FILLER              PIC X(02).                   G7D1PGM 
00461                                                                   G7D1PGM 
00462                                                                   G7D1PGM 
00463 /*** MAP FIELD ATTRIBUTES                                         G7D1PGM 
00464  COPY DFHBMSCA.                                                   G7D1PGM 
00465 *                         AUTOSKIP, BRIGHT, FSET                  G7D1PGM 
00466      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7D1PGM 
00467                                                                   G7D1PGM 
00468 /*** ATTENTION KEYS                                               G7D1PGM 
00469  COPY DFHAID.                                                     G7D1PGM 
00470                                                                   G7D1PGM 
00471 /***  PROVISION MAINTENANCE SCREEN                                G7D1PGM 
00472  COPY  G7D1SETC.                                                  G7D1PGM 
00473                                                                   G7D1PGM 
00474 /*** DATE ROUTINE COMMAREA                                        G7D1PGM 
00475  01  HGADATES-COMMAREA.                                           G7D1PGM 
00476  COPY HGCDAT01.                                                   G7D1PGM 
00477                                                                   G7D1PGM 
00478 /*** DECIMAL CONVERT COMMAREA                                     G7D1PGM 
00479  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7D1PGM 
00480  COPY GCDCCA01.                                                   G7D1PGM 
00481                                                                   G7D1PGM 
00482 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7D1PGM 
00483  01  GCVIOPGM-PARM-LIST.                                          G7D1PGM 
00484  COPY GCVINTRC.                                                   G7D1PGM 
00485                                                                   G7D1PGM 
00486 /*** ALTERNATIVE WORKFILE KEYS                                    G7D1PGM 
00487  01  FILLER.                                                      G7D1PGM 
00488      COPY GCWRKKEY.                                               G7D1PGM 
00489                                                                   G7D1PGM 
00490 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7D1PGM 
00491  01  FILLER.                                                      G7D1PGM 
00492      COPY GCCDRLEN.                                               G7D1PGM 
00493                                                                   G7D1PGM 
00494                                                                   G7D1PGM 
00495  01  WS-END                       PIC X(58) VALUE                 G7D1PGM 
00496      '*** G7D1PGM  WORKING-STORAGE ENDS HERE ***'.                G7D1PGM 
00497 /                                                                 G7D1PGM 
00498  LINKAGE SECTION.                                                 G7D1PGM 
00499 /                                                                 G7D1PGM 
00500  01  DFHCOMMAREA.                                                 G7D1PGM 
00501      COPY  GCWRKDCC.                                              G7D1PGM 
00502      COPY  GCBENPVC.                                              G7D1PGM 
00503 /                                                                 G7D1PGM 
00504 *01  BLL-CELLS.                                                   G7D1PGM 
00505 *    05  FILLER                   PIC S9(08)  COMP.               G7D1PGM 
00506 *    05  BEN-PROV-PNTR            PIC S9(08)  COMP.               G7D1PGM 
00507 *    05  CONTRACT-PNTR            PIC S9(08)  COMP.               G7D1PGM 
00508 *    05  CONTRACT-PNTR-2          PIC S9(08)  COMP.               G7D1PGM 
00509                                                                   G7D1PGM 
00510 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7D1PGM 
00511  01  IO-PARM-BEN-PROV-AREA.                                       G7D1PGM 
00512      COPY  GCIOPRM2.                                              G7D1PGM 
00513      COPY  GCWRKDC2.                                              G7D1PGM 
00514      COPY  GCBENPV2.                                              G7D1PGM 
00515                                                                   G7D1PGM 
00516 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7D1PGM 
00517  01  IO-PARM-CONTRACT-AREA.                                       G7D1PGM 
00518      COPY  GCIOPRM3.                                              G7D1PGM 
00519      COPY  GCWRKDC3.                                              G7D1PGM 
00520      COPY  GCCONTR2.                                              G7D1PGM 
00521 /                                                                 G7D1PGM 
00522  PROCEDURE DIVISION.                                              G7D1PGM 
00523                                                                   G7D1PGM 
00524 ****************************************************************  G7D1PGM 
00525 *                                                              *  G7D1PGM 
00526 *           P R O C E S S     C O N T R O L                    *  G7D1PGM 
00527 *                                                              *  G7D1PGM 
00528 ****************************************************************  G7D1PGM 
00529  0000-000-PROCESS-CONTROL       SECTION.                          G7D1PGM 
00530  0000-010.                                                        G7D1PGM 
00531                                                                   G7D1PGM 
00532 *    SERVICE RELOAD  BLL-CELLS.                                   G7D1PGM 
00533                                                                   G7D1PGM 
00534      IF  EIBAID  =  DFHCLEAR                                      G7D1PGM 
00535          EXEC CICS  RETURN                                        G7D1PGM 
00536                     END-EXEC.                                     G7D1PGM 
00537                                                                   G7D1PGM 
00538      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7D1PGM 
00539                                                                   G7D1PGM 
00540      IF  WS-02-MY-EIBTRNID                                        G7D1PGM 
00541      THEN                                                         G7D1PGM 
00542          PERFORM  2000-000-PROCESS-INPUT                          G7D1PGM 
00543      ELSE                                                         G7D1PGM 
00544          PERFORM  1000-000-DISPLAY-SCREEN.                        G7D1PGM 
00545                                                                   G7D1PGM 
00546                                                                   G7D1PGM 
00547 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7D1PGM 
00548                                                                   G7D1PGM 
00549      MOVE WS-01-ABCODE-D1L1     TO WS-01-ABCODE                   G7D1PGM 
00550      MOVE WS-01-ABCODE-D1L1-MSG TO WS-01-ABCODE-MSG               G7D1PGM 
00551      PERFORM  9999-000-ABEND-THE-TASK.                            G7D1PGM 
00552                                                                   G7D1PGM 
00553      GOBACK.                                                      G7D1PGM 
00554                                                                   G7D1PGM 
00555                                                                   G7D1PGM 
00556  0000-900-EXIT.                                                   G7D1PGM 
00557      EXIT.                                                        G7D1PGM 
00558 /***************************************************************  G7D1PGM 
00559 *                                                              *  G7D1PGM 
00560 * 1000  DISPLAY INITIAL SCREEN                                 *  G7D1PGM 
00561 *                                                              *  G7D1PGM 
00562 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7D1PGM 
00563 *                                                              *  G7D1PGM 
00564 ****************************************************************  G7D1PGM 
00565  1000-000-DISPLAY-SCREEN        SECTION.                          G7D1PGM 
00566  1000-010.                                                        G7D1PGM 
00567                                                                   G7D1PGM 
00568 *------- D129     MOVE LOW VALUES TO SCREEN FOR FIRST DIAPLAY     G7D1PGM 
00569 *                                                                 G7D1PGM 
00570      MOVE LOW-VALUES TO G7D1I01I.                                 G7D1PGM 
00571                                                                   G7D1PGM 
00572 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7D1PGM 
00573                                                                   G7D1PGM 
00574      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7D1PGM 
00575          MOVE WS-01-ABCODE-D1P1     TO WS-01-ABCODE               G7D1PGM 
00576          MOVE WS-01-ABCODE-D1P1-MSG TO WS-01-ABCODE-MSG           G7D1PGM 
00577          PERFORM 9999-000-ABEND-THE-TASK.                         G7D1PGM 
00578                                                                   G7D1PGM 
00579                                                                   G7D1PGM 
00580 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7D1PGM 
00581                                                                   G7D1PGM 
00582      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7D1PGM 
00583                                         + GC-GCBENPRV-FIXED-LEN   G7D1PGM 
00584                                         + GC-GCBENPRV-VARY-LEN.   G7D1PGM 
00585                                                                   G7D1PGM 
00586                                                                   G7D1PGM 
00587 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7D1PGM 
00588                                                                   G7D1PGM 
00589      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7D1PGM 
00590          MOVE WS-01-ABCODE-D1P2     TO WS-01-ABCODE               G7D1PGM 
00591          MOVE WS-01-ABCODE-D1P2-MSG TO WS-01-ABCODE-MSG           G7D1PGM 
00592          PERFORM 9999-000-ABEND-THE-TASK.                         G7D1PGM 
00593                                                                   G7D1PGM 
00594                                                                   G7D1PGM 
00595 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7D1PGM 
00596 *          BY CALLER IN COMMAREA.                                 G7D1PGM 
00597                                                                   G7D1PGM 
00598      MOVE WRK-PLAN-CODE                      TO S1PLNCDO.         G7D1PGM 
00599      MOVE WRK-GROUP-NUM                      TO S1GRPNOO.         G7D1PGM 
00600      MOVE WRK-SECTION-NUM                    TO S1SECNOO.         G7D1PGM 
00601      MOVE WRK-PKG-CODE                       TO S1PKGCDO.         G7D1PGM 
00602      MOVE WRK-PROV-CTL                       TO S1PRVO.           G7D1PGM 
00603      MOVE WRK-FAM-REL-LEVEL                  TO S1FRLO.           G7D1PGM 
00604      MOVE WRK-L-O-B                          TO S1LOBO.           G7D1PGM 
00605                                                                   G7D1PGM 
00606      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7D1PGM 
00607      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7D1PGM 
00608      IF  HGADATE-RETURN = ZEROS                                   G7D1PGM 
00609      THEN                                                         G7D1PGM 
00610          MOVE DFHBMASF                       TO S1EFFDTA          G7D1PGM 
00611          MOVE HGADATE-DATE2                  TO S1EFFDTO          G7D1PGM 
00612      ELSE                                                         G7D1PGM 
00613          MOVE DFHBMABF                       TO S1EFFDTA          G7D1PGM 
00614          MOVE HGADATE-JULIAN1                TO S1EFFDTO.         G7D1PGM 
00615                                                                   G7D1PGM 
00616      MOVE GCP-PROVN-ID                       TO S1BPVIDO.         G7D1PGM 
00617                                                                   G7D1PGM 
00618      MOVE GPD-BEN-SCOPE-ID                   TO S1BESCIO.         G7D1PGM 
00619      MOVE GPD-EXCP-SCHED-ID                  TO S1EXCSCO.         G7D1PGM 
00620      MOVE GPD-HOSP-ADM-RESTRN-IND            TO S1HADMRO.         G7D1PGM 
00621                                                                   G7D1PGM 
00622      IF  GPD-HSP-ADM-RESTRN-DAYS = ZEROS                          G7D1PGM 
00623          MOVE WS-02-HEX-F00000               TO S1HADRDO          G7D1PGM 
00624      ELSE                                                         G7D1PGM 
00625          MOVE   GPD-HSP-ADM-RESTRN-DAYS      TO                   G7D1PGM 
00626               WS-02-HSP-ADM-RESTRN-DAYS                           G7D1PGM 
00627          MOVE WS-02-HSP-ADM-RESTRN-DAYS-X    TO S1HADRDO.         G7D1PGM 
00628                                                                   G7D1PGM 
00629      MOVE  GPD-HOSP-COND-RELATSP-IND         TO S1HCNDRO.         G7D1PGM 
00630      MOVE  GPD-STAY-CODE-IND                 TO S1STCDIO.         G7D1PGM 
00631                                                                   G7D1PGM 
00632      IF  GPD-STAY-CD = ZEROS                                      G7D1PGM 
00633          MOVE WS-02-HEX-F00000               TO S1STYCDO          G7D1PGM 
00634      ELSE                                                         G7D1PGM 
00635          MOVE GPD-STAY-CD                    TO WS-02-STAY-CD     G7D1PGM 
00636          MOVE WS-02-STAY-CD-X                TO S1STYCDO.         G7D1PGM 
00637                                                                   G7D1PGM 
00638      MOVE  GPD-CORRIDOR-OVERRIDE             TO S1CORORO.         G7D1PGM 
00639      MOVE  GPD-DAYS-RDCN-RAT-IND             TO S1RDDYIO.         G7D1PGM 
00640                                                                   G7D1PGM 
00641 *    MOVE  GPD-DAYS-RDCN-RAT-BASIC-APL       TO WS-02-RATIO.      G7D1PGM 
00642 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV1BO.         G7D1PGM 
00643 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS1BO.         G7D1PGM 
00644                                                                   G7D1PGM 
00645 *    D129     ADDED FOR CONVERSION.                               G7D1PGM 
00646 *                                                                 G7D1PGM 
00647      MOVE  GPD-DAYS-RDCN-RAT-BASIC-APL    TO                      G7D1PGM 
00648            WS-02-DAYS-RDCN-RAT-BASIC-APL.                         G7D1PGM 
00649      MOVE  WS-02-DAYS-RDCN-RAT-BASIC-APL  TO WS-02-DISP-3POS-DEC. G7D1PGM 
00650      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRB1O.            G7D1PGM 
00651                                                                   G7D1PGM 
00652 *    MOVE  GPD-DAYS-RDCN-RAT-BASIC-BASE      TO WS-02-RATIO.      G7D1PGM 
00653 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV2BO.         G7D1PGM 
00654 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS2BO.         G7D1PGM 
00655                                                                   G7D1PGM 
00656 *    D129     ADDED FOR CONVERSION.                               G7D1PGM 
00657 *                                                                 G7D1PGM 
00658      MOVE  GPD-DAYS-RDCN-RAT-BASIC-BASE   TO                      G7D1PGM 
00659            WS-02-DAYS-RDCN-RAT-BASIC-BASE.                        G7D1PGM 
00660      MOVE  WS-02-DAYS-RDCN-RAT-BASIC-BASE TO WS-02-DISP-3POS-DEC. G7D1PGM 
00661      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRB2O.            G7D1PGM 
00662                                                                   G7D1PGM 
00663 *    MOVE  GPD-DAYS-RDCN-RAT-SEC-APL         TO WS-02-RATIO.      G7D1PGM 
00664 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV1SO.         G7D1PGM 
00665 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS1SO.         G7D1PGM 
00666                                                                   G7D1PGM 
00667 *    D129     ADDED FOR CONVERSION.                               G7D1PGM 
00668 *                                                                 G7D1PGM 
00669      MOVE  GPD-DAYS-RDCN-RAT-SEC-APL      TO                      G7D1PGM 
00670            WS-02-DAYS-RDCN-RAT-SEC-APL.                           G7D1PGM 
00671      MOVE  WS-02-DAYS-RDCN-RAT-SEC-APL    TO WS-02-DISP-3POS-DEC. G7D1PGM 
00672      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRS1O.            G7D1PGM 
00673                                                                   G7D1PGM 
00674 *    MOVE  GPD-DAYS-RDCN-RAT-SEC-BASE        TO WS-02-RATIO.      G7D1PGM 
00675 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV2SO.         G7D1PGM 
00676 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS2SO.         G7D1PGM 
00677                                                                   G7D1PGM 
00678 *    D129     ADDED FOR CONVERSION.                               G7D1PGM 
00679 *                                                                 G7D1PGM 
00680      MOVE  GPD-DAYS-RDCN-RAT-SEC-BASE     TO                      G7D1PGM 
00681            WS-02-DAYS-RDCN-RAT-SEC-BASE.                          G7D1PGM 
00682      MOVE  WS-02-DAYS-RDCN-RAT-SEC-BASE   TO WS-02-DISP-3POS-DEC. G7D1PGM 
00683      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRS2O.            G7D1PGM 
00684                                                                   G7D1PGM 
00685                                                                   G7D1PGM 
00686                                                                   G7D1PGM 
00687 *------- SEND INITIAL SCREEN ------------------------------------*G7D1PGM 
00688                                                                   G7D1PGM 
00689      MOVE  -1 TO  S1BESCIL.                                       G7D1PGM 
00690      PERFORM 9100-000-SEND-THEN-RETURN.                           G7D1PGM 
00691                                                                   G7D1PGM 
00692                                                                   G7D1PGM 
00693  1000-900-EXIT.                                                   G7D1PGM 
00694      EXIT.                                                        G7D1PGM 
00695 /***************************************************************  G7D1PGM 
00696 *                                                              *  G7D1PGM 
00697 * 2000    P R O C E S S    I N P U T                           *  G7D1PGM 
00698 *                                                              *  G7D1PGM 
00699 ****************************************************************  G7D1PGM 
00700  2000-000-PROCESS-INPUT         SECTION.                          G7D1PGM 
00701  2000-010.                                                        G7D1PGM 
00702                                                                   G7D1PGM 
00703 *------ VALIDATE PFKEY USAGE ------------------------------------*G7D1PGM 
00704                                                                   G7D1PGM 
00705      IF  EIBAID = DFHENTER OR                                     G7D1PGM 
00706                   DFHPF3   OR  DFHPF15 OR                         G7D1PGM 
00707                   DFHPF4   OR  DFHPF16 OR                         G7D1PGM 
00708                   DFHPF6   OR  DFHPF18 OR                         G7D1PGM 
00709                   DFHPF7   OR  DFHPF19 OR                         G7D1PGM 
00710                   DFHPF8   OR  DFHPF20                            G7D1PGM 
00711      THEN                                                         G7D1PGM 
00712          NEXT SENTENCE                                            G7D1PGM 
00713      ELSE                                                         G7D1PGM 
00714          SET WT-01-INDEX TO +01                                   G7D1PGM 
00715          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D1PGM 
00716          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D1PGM 
00717                                                                   G7D1PGM 
00718                                                                   G7D1PGM 
00719                                                                   G7D1PGM 
00720      EXEC CICS  HANDLE CONDITION                                  G7D1PGM 
00721                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7D1PGM 
00722                        END-EXEC.                                  G7D1PGM 
00723                                                                   G7D1PGM 
00724                                                                   G7D1PGM 
00725      EXEC CICS  RECEIVE MAP   ('G7D1I01')                         G7D1PGM 
00726                         MAPSET('G7D1SET')                         G7D1PGM 
00727                         END-EXEC.                                 G7D1PGM 
00728                                                                   G7D1PGM 
00729                                                                   G7D1PGM 
00730      IF  S1FUNCI  NOT = 'G7D1'  OR                                G7D1PGM 
00731          S1SCRNI  NOT = '007D01'                                  G7D1PGM 
00732          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7D1PGM 
00733                                                                   G7D1PGM 
00734                                                                   G7D1PGM 
00735 *--- RETURN TO GCPS MENU? ---------------------------------------*G7D1PGM 
00736                                                                   G7D1PGM 
00737      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7D1PGM 
00738          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7D1PGM 
00739                                                                   G7D1PGM 
00740 *--- PROCESS SCREEN FIELDS --------------------------------------*G7D1PGM 
00741                                                                   G7D1PGM 
00742      PERFORM 2100-000-FIELD-EDITS.                                G7D1PGM 
00743                                                                   G7D1PGM 
00744      IF  WS-02-SCREEN-HAS-ERRORS                                  G7D1PGM 
00745          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D1PGM 
00746                                                                   G7D1PGM 
00747      PERFORM 2200-000-LOGICAL-EDITS.                              G7D1PGM 
00748                                                                   G7D1PGM 
00749      IF  WS-02-SCREEN-HAS-ERRORS                                  G7D1PGM 
00750          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D1PGM 
00751                                                                   G7D1PGM 
00752      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7D1PGM 
00753                                                                   G7D1PGM 
00754      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7D1PGM 
00755                                                                   G7D1PGM 
00756                                                                   G7D1PGM 
00757  2000-900-EXIT.                                                   G7D1PGM 
00758      EXIT.                                                        G7D1PGM 
00759 /***************************************************************  G7D1PGM 
00760 *                                                              *  G7D1PGM 
00761 * 2100  DO SCREEN FIELD EDITS                                  *  G7D1PGM 
00762 *                                                              *  G7D1PGM 
00763 ****************************************************************  G7D1PGM 
00764  2100-000-FIELD-EDITS           SECTION.                          G7D1PGM 
00765  2100-010.                                                        G7D1PGM 
00766                                                                   G7D1PGM 
00767 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7D1PGM 
00768                                                                   G7D1PGM 
00769      MOVE DFHBMUNF TO  S1BESCIA                                   G7D1PGM 
00770                        S1EXCSCA                                   G7D1PGM 
00771                        S1HADMRA                                   G7D1PGM 
00772                        S1HADRDA                                   G7D1PGM 
00773                        S1HCNDRA                                   G7D1PGM 
00774                        S1STCDIA                                   G7D1PGM 
00775                        S1STYCDA                                   G7D1PGM 
00776                        S1CORORA                                   G7D1PGM 
00777                        S1RDDYIA                                   G7D1PGM 
00778                        S1DRRB1A                                   G7D1PGM 
00779                        S1DRRB2A                                   G7D1PGM 
00780                        S1DRRS1A                                   G7D1PGM 
00781                        S1DRRS2A                                   G7D1PGM 
00782                                                                   G7D1PGM 
00783      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7D1PGM 
00784                                                                   G7D1PGM 
00785                                                                   G7D1PGM 
00786 *-- VALIDATE ------ BENEFIT SCOPE -------------------------------*G7D1PGM 
00787 *   1. ALPHANUMERIC                                               G7D1PGM 
00788 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
00789                                                                   G7D1PGM 
00790      MOVE  S1BESCII TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
00791      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7D1PGM 
00792          WS-02-CLASS-ALPHANUMERIC(2) AND                          G7D1PGM 
00793          WS-02-CLASS-BLANK       (3) AND                          G7D1PGM 
00794          WS-02-CLASS-BLANK       (4)                              G7D1PGM 
00795      THEN                                                         G7D1PGM 
00796          MOVE  S1BESCII TO GCVI-VALUE                             G7D1PGM 
00797          MOVE  'BPCA01' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
00798          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
00799          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
00800          THEN                                                     G7D1PGM 
00801              MOVE  -1        TO  S1BESCIL                         G7D1PGM 
00802              MOVE  DFHBMUBF  TO  S1BESCIA                         G7D1PGM 
00803              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
00804              THEN                                                 G7D1PGM 
00805                  NEXT SENTENCE                                    G7D1PGM 
00806              ELSE                                                 G7D1PGM 
00807                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
00808                  SET WT-01-INDEX TO +08                           G7D1PGM 
00809                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
00810          ELSE                                                     G7D1PGM 
00811              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
00812              THEN                                                 G7D1PGM 
00813                  MOVE  DFHBMUBF  TO  S1BESCIA                     G7D1PGM 
00814              ELSE                                                 G7D1PGM 
00815                  NEXT SENTENCE                                    G7D1PGM 
00816      ELSE                                                         G7D1PGM 
00817          MOVE  -1        TO  S1BESCIL                             G7D1PGM 
00818          MOVE  DFHBMUBF  TO  S1BESCIA                             G7D1PGM 
00819          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
00820          THEN                                                     G7D1PGM 
00821              NEXT SENTENCE                                        G7D1PGM 
00822          ELSE                                                     G7D1PGM 
00823              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
00824              SET WT-01-INDEX TO +10                               G7D1PGM 
00825              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
00826                                                                   G7D1PGM 
00827                                                                   G7D1PGM 
00828 *-- VALIDATE ------ EXCEPTION SCHEDULE ID -----------------------*G7D1PGM 
00829 *   1. ALPHANUMERIC                                               G7D1PGM 
00830 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
00831                                                                   G7D1PGM 
00832      MOVE  S1EXCSCI TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
00833      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7D1PGM 
00834          WS-02-CLASS-ALPHANUMERIC(2) AND                          G7D1PGM 
00835          WS-02-CLASS-ALPHANUMERIC(3) AND                          G7D1PGM 
00836          WS-02-CLASS-ALPHANUMERIC(4)                              G7D1PGM 
00837      THEN                                                         G7D1PGM 
00838          MOVE  S1EXCSCI TO GCVI-VALUE                             G7D1PGM 
00839          MOVE  'BPCA10' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
00840          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
00841          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
00842          THEN                                                     G7D1PGM 
00843              MOVE  -1        TO  S1EXCSCL                         G7D1PGM 
00844              MOVE  DFHBMUBF  TO  S1EXCSCA                         G7D1PGM 
00845              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
00846              THEN                                                 G7D1PGM 
00847                  NEXT SENTENCE                                    G7D1PGM 
00848              ELSE                                                 G7D1PGM 
00849                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
00850                  SET WT-01-INDEX TO +09                           G7D1PGM 
00851                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
00852          ELSE                                                     G7D1PGM 
00853              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
00854              THEN                                                 G7D1PGM 
00855                  MOVE  DFHBMUBF  TO  S1EXCSCA                     G7D1PGM 
00856              ELSE                                                 G7D1PGM 
00857                  NEXT SENTENCE                                    G7D1PGM 
00858      ELSE                                                         G7D1PGM 
00859          MOVE  -1        TO  S1EXCSCL                             G7D1PGM 
00860          MOVE  DFHBMUBF  TO  S1EXCSCA                             G7D1PGM 
00861          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
00862          THEN                                                     G7D1PGM 
00863              NEXT SENTENCE                                        G7D1PGM 
00864          ELSE                                                     G7D1PGM 
00865              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
00866              SET WT-01-INDEX TO +08                               G7D1PGM 
00867              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
00868                                                                   G7D1PGM 
00869                                                                   G7D1PGM 
00870 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION IND ----------*G7D1PGM 
00871 *   1. ALPHANUMERIC                                               G7D1PGM 
00872 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
00873                                                                   G7D1PGM 
00874      MOVE  S1HADMRI TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
00875      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D1PGM 
00876      THEN                                                         G7D1PGM 
00877          MOVE  S1HADMRI TO GCVI-VALUE                             G7D1PGM 
00878          MOVE  'BPAA01' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
00879          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
00880          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
00881          THEN                                                     G7D1PGM 
00882              MOVE  -1        TO  S1HADMRL                         G7D1PGM 
00883              MOVE  DFHBMUBF  TO  S1HADMRA                         G7D1PGM 
00884              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
00885              THEN                                                 G7D1PGM 
00886                  NEXT SENTENCE                                    G7D1PGM 
00887              ELSE                                                 G7D1PGM 
00888                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
00889                  SET WT-01-INDEX TO +09                           G7D1PGM 
00890                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
00891          ELSE                                                     G7D1PGM 
00892              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
00893              THEN                                                 G7D1PGM 
00894                  MOVE  DFHBMUBF  TO  S1HADMRA                     G7D1PGM 
00895              ELSE                                                 G7D1PGM 
00896                  NEXT SENTENCE                                    G7D1PGM 
00897      ELSE                                                         G7D1PGM 
00898          MOVE  -1        TO  S1HADMRL                             G7D1PGM 
00899          MOVE  DFHBMUBF  TO  S1HADMRA                             G7D1PGM 
00900          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
00901          THEN                                                     G7D1PGM 
00902              NEXT SENTENCE                                        G7D1PGM 
00903          ELSE                                                     G7D1PGM 
00904              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
00905              SET WT-01-INDEX TO +08                               G7D1PGM 
00906              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
00907                                                                   G7D1PGM 
00908                                                                   G7D1PGM 
00909                                                                   G7D1PGM 
00910 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION DAYS ---------*G7D1PGM 
00911 *   1. NUMERICS                                                   G7D1PGM 
00912                                                                   G7D1PGM 
00913      IF  S1HADRDI IS NUMERIC                                      G7D1PGM 
00914      THEN                                                         G7D1PGM 
00915          NEXT SENTENCE                                            G7D1PGM 
00916      ELSE                                                         G7D1PGM 
00917          MOVE  -1        TO  S1HADRDL                             G7D1PGM 
00918          MOVE  DFHBMUBF  TO  S1HADRDA                             G7D1PGM 
00919          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
00920          THEN                                                     G7D1PGM 
00921              NEXT SENTENCE                                        G7D1PGM 
00922          ELSE                                                     G7D1PGM 
00923              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
00924              SET WT-01-INDEX TO +13                               G7D1PGM 
00925              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
00926                                                                   G7D1PGM 
00927                                                                   G7D1PGM 
00928 *-- VALIDATE ------ HOSP. COND. RELATIONSHIP IND ----------------*G7D1PGM 
00929 *   1. ALPHANUMERIC                                               G7D1PGM 
00930 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
00931                                                                   G7D1PGM 
00932      MOVE  S1HCNDRI TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
00933      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D1PGM 
00934      THEN                                                         G7D1PGM 
00935          MOVE  S1HCNDRI TO GCVI-VALUE                             G7D1PGM 
00936          MOVE  'BPAA03' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
00937          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
00938          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
00939          THEN                                                     G7D1PGM 
00940              MOVE  -1        TO  S1HCNDRL                         G7D1PGM 
00941              MOVE  DFHBMUBF  TO  S1HCNDRA                         G7D1PGM 
00942              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
00943              THEN                                                 G7D1PGM 
00944                  NEXT SENTENCE                                    G7D1PGM 
00945              ELSE                                                 G7D1PGM 
00946                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
00947                  SET WT-01-INDEX TO +09                           G7D1PGM 
00948                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
00949          ELSE                                                     G7D1PGM 
00950              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
00951              THEN                                                 G7D1PGM 
00952                  MOVE  DFHBMUBF  TO  S1HCNDRA                     G7D1PGM 
00953              ELSE                                                 G7D1PGM 
00954                  NEXT SENTENCE                                    G7D1PGM 
00955      ELSE                                                         G7D1PGM 
00956          MOVE  -1        TO  S1HCNDRL                             G7D1PGM 
00957          MOVE  DFHBMUBF  TO  S1HCNDRA                             G7D1PGM 
00958          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
00959          THEN                                                     G7D1PGM 
00960              NEXT SENTENCE                                        G7D1PGM 
00961          ELSE                                                     G7D1PGM 
00962              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
00963              SET WT-01-INDEX TO +08                               G7D1PGM 
00964              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
00965                                                                   G7D1PGM 
00966                                                                   G7D1PGM 
00967 *-- VALIDATE ------ STAY CODE INDICATOR -------------------------*G7D1PGM 
00968 *   1. ALPHANUMERIC                                               G7D1PGM 
00969 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
00970                                                                   G7D1PGM 
00971      MOVE  S1STCDII TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
00972      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D1PGM 
00973      THEN                                                         G7D1PGM 
00974          MOVE  S1STCDII TO GCVI-VALUE                             G7D1PGM 
00975          MOVE  'BPAA12' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
00976          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
00977          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
00978          THEN                                                     G7D1PGM 
00979              MOVE  -1        TO  S1STCDIL                         G7D1PGM 
00980              MOVE  DFHBMUBF  TO  S1STCDIA                         G7D1PGM 
00981              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
00982              THEN                                                 G7D1PGM 
00983                  NEXT SENTENCE                                    G7D1PGM 
00984              ELSE                                                 G7D1PGM 
00985                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
00986                  SET WT-01-INDEX TO +09                           G7D1PGM 
00987                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
00988          ELSE                                                     G7D1PGM 
00989              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
00990              THEN                                                 G7D1PGM 
00991                  MOVE  DFHBMUBF  TO  S1STCDIA                     G7D1PGM 
00992              ELSE                                                 G7D1PGM 
00993                  NEXT SENTENCE                                    G7D1PGM 
00994      ELSE                                                         G7D1PGM 
00995          MOVE  -1        TO  S1STCDIL                             G7D1PGM 
00996          MOVE  DFHBMUBF  TO  S1STCDIA                             G7D1PGM 
00997          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
00998          THEN                                                     G7D1PGM 
00999              NEXT SENTENCE                                        G7D1PGM 
01000          ELSE                                                     G7D1PGM 
01001              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01002              SET WT-01-INDEX TO +08                               G7D1PGM 
01003              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01004                                                                   G7D1PGM 
01005                                                                   G7D1PGM 
01006 *-- VALIDATE ------ STAY CODE -----------------------------------*G7D1PGM 
01007 *   1. NUMERICS                                                   G7D1PGM 
01008                                                                   G7D1PGM 
01009      IF  S1STYCDI IS NUMERIC                                      G7D1PGM 
01010      THEN                                                         G7D1PGM 
01011          NEXT SENTENCE                                            G7D1PGM 
01012      ELSE                                                         G7D1PGM 
01013          MOVE  -1        TO  S1STYCDL                             G7D1PGM 
01014          MOVE  DFHBMUBF  TO  S1STYCDA                             G7D1PGM 
01015          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01016          THEN                                                     G7D1PGM 
01017              NEXT SENTENCE                                        G7D1PGM 
01018          ELSE                                                     G7D1PGM 
01019              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01020              SET WT-01-INDEX TO +13                               G7D1PGM 
01021              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01022                                                                   G7D1PGM 
01023                                                                   G7D1PGM 
01024 *-- VALIDATE ------ CORRIDOR OVERRIDE ---------------------------*G7D1PGM 
01025 *   1. ALPHANUMERIC                                               G7D1PGM 
01026 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
01027                                                                   G7D1PGM 
01028      MOVE  S1CORORI TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
01029      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D1PGM 
01030      THEN                                                         G7D1PGM 
01031          MOVE  S1CORORI TO GCVI-VALUE                             G7D1PGM 
01032          MOVE  'BPCA02' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
01033          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
01034          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
01035          THEN                                                     G7D1PGM 
01036              MOVE  -1        TO  S1CORORL                         G7D1PGM 
01037              MOVE  DFHBMUBF  TO  S1CORORA                         G7D1PGM 
01038              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
01039              THEN                                                 G7D1PGM 
01040                  NEXT SENTENCE                                    G7D1PGM 
01041              ELSE                                                 G7D1PGM 
01042                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
01043                  SET WT-01-INDEX TO +09                           G7D1PGM 
01044                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01045          ELSE                                                     G7D1PGM 
01046              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
01047              THEN                                                 G7D1PGM 
01048                  MOVE  DFHBMUBF  TO  S1CORORA                     G7D1PGM 
01049              ELSE                                                 G7D1PGM 
01050                  NEXT SENTENCE                                    G7D1PGM 
01051      ELSE                                                         G7D1PGM 
01052          MOVE  -1        TO  S1CORORL                             G7D1PGM 
01053          MOVE  DFHBMUBF  TO  S1CORORA                             G7D1PGM 
01054          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01055          THEN                                                     G7D1PGM 
01056              NEXT SENTENCE                                        G7D1PGM 
01057          ELSE                                                     G7D1PGM 
01058              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01059              SET WT-01-INDEX TO +08                               G7D1PGM 
01060              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01061                                                                   G7D1PGM 
01062                                                                   G7D1PGM 
01063 *-- VALIDATE ------ DAYS REDUCTION RATIO IND --------------------*G7D1PGM 
01064 *   1. ALPHANUMERIC                                               G7D1PGM 
01065 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D1PGM 
01066                                                                   G7D1PGM 
01067      MOVE  S1RDDYII TO WS-02-CLASS-TEST-AREA.                     G7D1PGM 
01068      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D1PGM 
01069      THEN                                                         G7D1PGM 
01070          MOVE  S1RDDYII TO GCVI-VALUE                             G7D1PGM 
01071          MOVE  'BPAA05' TO GCVI-FIELDS-KEY-ID                     G7D1PGM 
01072          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D1PGM 
01073          IF  GCVI-VALUE-NOT-FOUND                                 G7D1PGM 
01074          THEN                                                     G7D1PGM 
01075              MOVE  -1        TO  S1RDDYIL                         G7D1PGM 
01076              MOVE  DFHBMUBF  TO  S1RDDYIA                         G7D1PGM 
01077              IF  WS-02-SCREEN-HAS-ERRORS                          G7D1PGM 
01078              THEN                                                 G7D1PGM 
01079                  NEXT SENTENCE                                    G7D1PGM 
01080              ELSE                                                 G7D1PGM 
01081                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
01082                  SET WT-01-INDEX TO +09                           G7D1PGM 
01083                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01084          ELSE                                                     G7D1PGM 
01085              IF  GCVI-VALUE-NOT-LOADED                            G7D1PGM 
01086              THEN                                                 G7D1PGM 
01087                  MOVE  DFHBMUBF  TO  S1RDDYIA                     G7D1PGM 
01088              ELSE                                                 G7D1PGM 
01089                  NEXT SENTENCE                                    G7D1PGM 
01090      ELSE                                                         G7D1PGM 
01091          MOVE  -1        TO  S1RDDYIL                             G7D1PGM 
01092          MOVE  DFHBMUBF  TO  S1RDDYIA                             G7D1PGM 
01093          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01094          THEN                                                     G7D1PGM 
01095              NEXT SENTENCE                                        G7D1PGM 
01096          ELSE                                                     G7D1PGM 
01097              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01098              SET WT-01-INDEX TO +08                               G7D1PGM 
01099              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01100                                                                   G7D1PGM 
01101                                                                   G7D1PGM 
01102 *-- VALIDATE ------ DAYS REDUCTION RATIO BASIC (2 FIELDS) -------*G7D1PGM 
01103 *  D129                                                           G7D1PGM 
01104                                                                   G7D1PGM 
01105      MOVE S1DRRB1I TO D-C-RECEIVE-FIELD.                          G7D1PGM 
01106      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7D1PGM 
01107      MOVE '00' TO D-C-RETURN-CODE.                                G7D1PGM 
01108      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7D1PGM 
01109      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7D1PGM 
01110      IF D-C-RETURN-CODE = '00'                                    G7D1PGM 
01111          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7D1PGM 
01112              MOVE -1       TO S1DRRB1L                            G7D1PGM 
01113              MOVE DFHBMUBF TO S1DRRB1A                            G7D1PGM 
01114              IF WS-02-SCREEN-HAS-ERRORS                           G7D1PGM 
01115                  NEXT SENTENCE                                    G7D1PGM 
01116              ELSE                                                 G7D1PGM 
01117                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7D1PGM 
01118                  SET WT-01-INDEX TO +14                           G7D1PGM 
01119                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01120          ELSE                                                     G7D1PGM 
01121              MOVE D-C-RETURN-FIELD-DEC1                           G7D1PGM 
01122                TO WS-02-DAYS-RDCN-RAT-BASIC-APL                   G7D1PGM 
01123              MOVE WS-02-DAYS-RDCN-RAT-BASIC-APL                   G7D1PGM 
01124                TO WS-02-DISP-3POS-DEC                             G7D1PGM 
01125              MOVE WS-02-DISP-3POS-DEC                             G7D1PGM 
01126                TO S1DRRB1O                                        G7D1PGM 
01127      ELSE                                                         G7D1PGM 
01128          MOVE -1       TO S1DRRB1L                                G7D1PGM 
01129          MOVE DFHBMUBF TO S1DRRB1A                                G7D1PGM 
01130          IF WS-02-SCREEN-HAS-ERRORS                               G7D1PGM 
01131              NEXT SENTENCE                                        G7D1PGM 
01132          ELSE                                                     G7D1PGM 
01133              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7D1PGM 
01134              IF D-C-RETURN-CODE = '10'                            G7D1PGM 
01135                  SET WT-01-INDEX TO +13                           G7D1PGM 
01136                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01137              ELSE                                                 G7D1PGM 
01138                  SET WT-01-INDEX TO +15                           G7D1PGM 
01139                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7D1PGM 
01140                                                                   G7D1PGM 
01141      MOVE S1DRRB2I TO D-C-RECEIVE-FIELD.                          G7D1PGM 
01142      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7D1PGM 
01143      MOVE '00' TO D-C-RETURN-CODE.                                G7D1PGM 
01144      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7D1PGM 
01145      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7D1PGM 
01146      IF D-C-RETURN-CODE = '00'                                    G7D1PGM 
01147          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7D1PGM 
01148              MOVE -1       TO S1DRRB2L                            G7D1PGM 
01149              MOVE DFHBMUBF TO S1DRRB2A                            G7D1PGM 
01150              IF WS-02-SCREEN-HAS-ERRORS                           G7D1PGM 
01151                  NEXT SENTENCE                                    G7D1PGM 
01152              ELSE                                                 G7D1PGM 
01153                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7D1PGM 
01154                  SET WT-01-INDEX TO +14                           G7D1PGM 
01155                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01156          ELSE                                                     G7D1PGM 
01157              MOVE D-C-RETURN-FIELD-DEC1                           G7D1PGM 
01158                TO WS-02-DAYS-RDCN-RAT-BASIC-BASE                  G7D1PGM 
01159              MOVE WS-02-DAYS-RDCN-RAT-BASIC-BASE                  G7D1PGM 
01160                TO WS-02-DISP-3POS-DEC                             G7D1PGM 
01161              MOVE WS-02-DISP-3POS-DEC                             G7D1PGM 
01162                TO S1DRRB2O                                        G7D1PGM 
01163      ELSE                                                         G7D1PGM 
01164          MOVE -1       TO S1DRRB2L                                G7D1PGM 
01165          MOVE DFHBMUBF TO S1DRRB2A                                G7D1PGM 
01166          IF WS-02-SCREEN-HAS-ERRORS                               G7D1PGM 
01167              NEXT SENTENCE                                        G7D1PGM 
01168          ELSE                                                     G7D1PGM 
01169              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7D1PGM 
01170              IF D-C-RETURN-CODE = '10'                            G7D1PGM 
01171                  SET WT-01-INDEX TO +13                           G7D1PGM 
01172                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01173              ELSE                                                 G7D1PGM 
01174                  SET WT-01-INDEX TO +15                           G7D1PGM 
01175                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7D1PGM 
01176                                                                   G7D1PGM 
01177 *    D-I-V-I-S-O-R                                                G7D1PGM 
01178 *    IF  S1DIV1BI IS NUMERIC                                      G7D1PGM 
01179 *    THEN                                                         G7D1PGM 
01180 *        NEXT SENTENCE                                            G7D1PGM 
01181 *    ELSE                                                         G7D1PGM 
01182 *        MOVE  -1        TO  S1DIV1BL                             G7D1PGM 
01183 *        MOVE  DFHBMUBF  TO  S1DIV1BA                             G7D1PGM 
01184 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01185 *        THEN                                                     G7D1PGM 
01186 *            NEXT SENTENCE                                        G7D1PGM 
01187 *        ELSE                                                     G7D1PGM 
01188 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01189 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01190 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01191                                                                   G7D1PGM 
01192 *    B-A-S-E                                                      G7D1PGM 
01193 *    IF  S1BAS1BI IS NUMERIC                                      G7D1PGM 
01194 *    THEN                                                         G7D1PGM 
01195 *        NEXT SENTENCE                                            G7D1PGM 
01196 *    ELSE                                                         G7D1PGM 
01197 *        MOVE  -1        TO  S1BAS1BL                             G7D1PGM 
01198 *        MOVE  DFHBMUBF  TO  S1BAS1BA                             G7D1PGM 
01199 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01200 *        THEN                                                     G7D1PGM 
01201 *            NEXT SENTENCE                                        G7D1PGM 
01202 *        ELSE                                                     G7D1PGM 
01203 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01204 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01205 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01206                                                                   G7D1PGM 
01207 *    D-I-V-I-S-O-R                                                G7D1PGM 
01208 *    IF  S1DIV2BI IS NUMERIC                                      G7D1PGM 
01209 *    THEN                                                         G7D1PGM 
01210 *        NEXT SENTENCE                                            G7D1PGM 
01211 *    ELSE                                                         G7D1PGM 
01212 *        MOVE  -1        TO  S1DIV2BL                             G7D1PGM 
01213 *        MOVE  DFHBMUBF  TO  S1DIV2BA                             G7D1PGM 
01214 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01215 *        THEN                                                     G7D1PGM 
01216 *            NEXT SENTENCE                                        G7D1PGM 
01217 *        ELSE                                                     G7D1PGM 
01218 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01219 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01220 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01221                                                                   G7D1PGM 
01222 *    B-A-S-E                                                      G7D1PGM 
01223 *    IF  S1BAS2BI IS NUMERIC                                      G7D1PGM 
01224 *    THEN                                                         G7D1PGM 
01225 *        NEXT SENTENCE                                            G7D1PGM 
01226 *    ELSE                                                         G7D1PGM 
01227 *        MOVE  -1        TO  S1BAS2BL                             G7D1PGM 
01228 *        MOVE  DFHBMUBF  TO  S1BAS2BA                             G7D1PGM 
01229 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01230 *        THEN                                                     G7D1PGM 
01231 *            NEXT SENTENCE                                        G7D1PGM 
01232 *        ELSE                                                     G7D1PGM 
01233 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01234 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01235 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01236                                                                   G7D1PGM 
01237                                                                   G7D1PGM 
01238 *-- VALIDATE ------ DAYS REDUCTION RATIO SECONDARY (2 FIELDS) ---*G7D1PGM 
01239 *  D129                                                           G7D1PGM 
01240                                                                   G7D1PGM 
01241      MOVE S1DRRS1I TO D-C-RECEIVE-FIELD.                          G7D1PGM 
01242      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7D1PGM 
01243      MOVE '00' TO D-C-RETURN-CODE.                                G7D1PGM 
01244      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7D1PGM 
01245      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7D1PGM 
01246      IF D-C-RETURN-CODE = '00'                                    G7D1PGM 
01247          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7D1PGM 
01248              MOVE -1       TO S1DRRS1L                            G7D1PGM 
01249              MOVE DFHBMUBF TO S1DRRS1A                            G7D1PGM 
01250              IF WS-02-SCREEN-HAS-ERRORS                           G7D1PGM 
01251                  NEXT SENTENCE                                    G7D1PGM 
01252              ELSE                                                 G7D1PGM 
01253                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7D1PGM 
01254                  SET WT-01-INDEX TO +14                           G7D1PGM 
01255                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01256          ELSE                                                     G7D1PGM 
01257              MOVE D-C-RETURN-FIELD-DEC1                           G7D1PGM 
01258                TO WS-02-DAYS-RDCN-RAT-SEC-APL                     G7D1PGM 
01259              MOVE WS-02-DAYS-RDCN-RAT-SEC-APL                     G7D1PGM 
01260                TO WS-02-DISP-3POS-DEC                             G7D1PGM 
01261              MOVE WS-02-DISP-3POS-DEC                             G7D1PGM 
01262                TO S1DRRS1O                                        G7D1PGM 
01263      ELSE                                                         G7D1PGM 
01264          MOVE -1       TO S1DRRS1L                                G7D1PGM 
01265          MOVE DFHBMUBF TO S1DRRS1A                                G7D1PGM 
01266          IF WS-02-SCREEN-HAS-ERRORS                               G7D1PGM 
01267              NEXT SENTENCE                                        G7D1PGM 
01268          ELSE                                                     G7D1PGM 
01269              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7D1PGM 
01270              IF D-C-RETURN-CODE = '10'                            G7D1PGM 
01271                  SET WT-01-INDEX TO +13                           G7D1PGM 
01272                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01273              ELSE                                                 G7D1PGM 
01274                  SET WT-01-INDEX TO +15                           G7D1PGM 
01275                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7D1PGM 
01276                                                                   G7D1PGM 
01277      MOVE S1DRRS2I TO D-C-RECEIVE-FIELD.                          G7D1PGM 
01278      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7D1PGM 
01279      MOVE '00' TO D-C-RETURN-CODE.                                G7D1PGM 
01280      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7D1PGM 
01281      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7D1PGM 
01282      IF D-C-RETURN-CODE = '00'                                    G7D1PGM 
01283          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7D1PGM 
01284              MOVE -1       TO S1DRRS2L                            G7D1PGM 
01285              MOVE DFHBMUBF TO S1DRRS2A                            G7D1PGM 
01286              IF WS-02-SCREEN-HAS-ERRORS                           G7D1PGM 
01287                  NEXT SENTENCE                                    G7D1PGM 
01288              ELSE                                                 G7D1PGM 
01289                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7D1PGM 
01290                  SET WT-01-INDEX TO +14                           G7D1PGM 
01291                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01292          ELSE                                                     G7D1PGM 
01293              MOVE D-C-RETURN-FIELD-DEC1                           G7D1PGM 
01294                TO WS-02-DAYS-RDCN-RAT-SEC-BASE                    G7D1PGM 
01295              MOVE WS-02-DAYS-RDCN-RAT-SEC-BASE                    G7D1PGM 
01296                TO WS-02-DISP-3POS-DEC                             G7D1PGM 
01297              MOVE WS-02-DISP-3POS-DEC                             G7D1PGM 
01298                TO S1DRRS2O                                        G7D1PGM 
01299      ELSE                                                         G7D1PGM 
01300          MOVE -1       TO S1DRRS2L                                G7D1PGM 
01301          MOVE DFHBMUBF TO S1DRRS2A                                G7D1PGM 
01302          IF WS-02-SCREEN-HAS-ERRORS                               G7D1PGM 
01303              NEXT SENTENCE                                        G7D1PGM 
01304          ELSE                                                     G7D1PGM 
01305              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7D1PGM 
01306              IF D-C-RETURN-CODE = '10'                            G7D1PGM 
01307                  SET WT-01-INDEX TO +13                           G7D1PGM 
01308                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01309              ELSE                                                 G7D1PGM 
01310                  SET WT-01-INDEX TO +15                           G7D1PGM 
01311                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7D1PGM 
01312                                                                   G7D1PGM 
01313                                                                   G7D1PGM 
01314 *    D-I-V-I-S-O-R                                                G7D1PGM 
01315 *    IF  S1DIV1SI IS NUMERIC                                      G7D1PGM 
01316 *    THEN                                                         G7D1PGM 
01317 *        NEXT SENTENCE                                            G7D1PGM 
01318 *    ELSE                                                         G7D1PGM 
01319 *        MOVE  -1        TO  S1DIV1SL                             G7D1PGM 
01320 *        MOVE  DFHBMUBF  TO  S1DIV1SA                             G7D1PGM 
01321 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01322 *        THEN                                                     G7D1PGM 
01323 *            NEXT SENTENCE                                        G7D1PGM 
01324 *        ELSE                                                     G7D1PGM 
01325 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01326 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01327 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01328                                                                   G7D1PGM 
01329 *    B-A-S-E                                                      G7D1PGM 
01330 *    IF  S1BAS1SI IS NUMERIC                                      G7D1PGM 
01331 *    THEN                                                         G7D1PGM 
01332 *        NEXT SENTENCE                                            G7D1PGM 
01333 *    ELSE                                                         G7D1PGM 
01334 *        MOVE  -1        TO  S1BAS1SL                             G7D1PGM 
01335 *        MOVE  DFHBMUBF  TO  S1BAS1SA                             G7D1PGM 
01336 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01337 *        THEN                                                     G7D1PGM 
01338 *            NEXT SENTENCE                                        G7D1PGM 
01339 *        ELSE                                                     G7D1PGM 
01340 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01341 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01342 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01343                                                                   G7D1PGM 
01344 *    D-I-V-I-S-O-R                                                G7D1PGM 
01345 *    IF  S1DIV2SI IS NUMERIC                                      G7D1PGM 
01346 *    THEN                                                         G7D1PGM 
01347 *        NEXT SENTENCE                                            G7D1PGM 
01348 *    ELSE                                                         G7D1PGM 
01349 *        MOVE  -1        TO  S1DIV2SL                             G7D1PGM 
01350 *        MOVE  DFHBMUBF  TO  S1DIV2SA                             G7D1PGM 
01351 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01352 *        THEN                                                     G7D1PGM 
01353 *            NEXT SENTENCE                                        G7D1PGM 
01354 *        ELSE                                                     G7D1PGM 
01355 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01356 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01357 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01358                                                                   G7D1PGM 
01359 *    B-A-S-E                                                      G7D1PGM 
01360 *    IF  S1BAS2SI IS NUMERIC                                      G7D1PGM 
01361 *    THEN                                                         G7D1PGM 
01362 *        NEXT SENTENCE                                            G7D1PGM 
01363 *    ELSE                                                         G7D1PGM 
01364 *        MOVE  -1        TO  S1BAS2SL                             G7D1PGM 
01365 *        MOVE  DFHBMUBF  TO  S1BAS2SA                             G7D1PGM 
01366 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01367 *        THEN                                                     G7D1PGM 
01368 *            NEXT SENTENCE                                        G7D1PGM 
01369 *        ELSE                                                     G7D1PGM 
01370 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01371 *            SET WT-01-INDEX TO +13                               G7D1PGM 
01372 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D1PGM 
01373                                                                   G7D1PGM 
01374  2100-900-EXIT.                                                   G7D1PGM 
01375      EXIT.                                                        G7D1PGM 
01376 /***************************************************************  G7D1PGM 
01377 *                                                              *  G7D1PGM 
01378 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7D1PGM 
01379 *                                                              *  G7D1PGM 
01380 ****************************************************************  G7D1PGM 
01381  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7D1PGM 
01382  2110-010.                                                        G7D1PGM 
01383                                                                   G7D1PGM 
01384      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7D1PGM 
01385                                                                   G7D1PGM 
01386      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7D1PGM 
01387                       COMMAREA(GCVIOPGM-PARM-LIST)                G7D1PGM 
01388                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7D1PGM 
01389                       END-EXEC.                                   G7D1PGM 
01390                                                                   G7D1PGM 
01391      IF  GCVI-VALUE-NOT-LOADED                                    G7D1PGM 
01392          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7D1PGM 
01393                                                                   G7D1PGM 
01394  2110-900-EXIT.                                                   G7D1PGM 
01395      EXIT.                                                        G7D1PGM 
01396 /***************************************************************  G7D1PGM 
01397 *                                                              *  G7D1PGM 
01398 * 2200  DO SCREEN LOGICAL EDITS                                *  G7D1PGM 
01399 *                                                              *  G7D1PGM 
01400 ****************************************************************  G7D1PGM 
01401  2200-000-LOGICAL-EDITS         SECTION.                          G7D1PGM 
01402  2200-010.                                                        G7D1PGM 
01403                                                                   G7D1PGM 
01404 *----------------------------------------------------------------*G7D1PGM 
01405 *                                                                *G7D1PGM 
01406 *  IF   HOSPITAL ADMISSION RESTRICTION IND (S1HADMR) > ZERO      *G7D1PGM 
01407 *                                                                *G7D1PGM 
01408 *  THEN HOSPITAL ADMISSION RESTRICTION DAYS(S1HADRD):            *G7D1PGM 
01409 *                      MUST BE > ZERO.                           *G7D1PGM 
01410 *                                                                *G7D1PGM 
01411 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7D1PGM 
01412 *                                                                *G7D1PGM 
01413 *----------------------------------------------------------------*G7D1PGM 
01414                                                                   G7D1PGM 
01415      IF  S1HADMRI     > ZEROS                                     G7D1PGM 
01416          AND                                                      G7D1PGM 
01417          S1HADRDI NOT > ZEROS                                     G7D1PGM 
01418      THEN                                                         G7D1PGM 
01419          MOVE  -1        TO  S1HADRDL                             G7D1PGM 
01420          MOVE  DFHBMUBF  TO  S1HADRDA                             G7D1PGM 
01421                              S1HADMRA                             G7D1PGM 
01422          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01423          THEN                                                     G7D1PGM 
01424              NEXT SENTENCE                                        G7D1PGM 
01425          ELSE                                                     G7D1PGM 
01426              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01427              SET WT-01-INDEX TO +02                               G7D1PGM 
01428              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D1PGM 
01429      ELSE                                                         G7D1PGM 
01430          NEXT SENTENCE.                                           G7D1PGM 
01431                                                                   G7D1PGM 
01432      IF  S1HADRDI     > ZEROS                                     G7D1PGM 
01433          AND                                                      G7D1PGM 
01434          S1HADMRI NOT > ZEROS                                     G7D1PGM 
01435      THEN                                                         G7D1PGM 
01436          MOVE  -1        TO  S1HADMRL                             G7D1PGM 
01437          MOVE  DFHBMUBF  TO  S1HADMRA                             G7D1PGM 
01438                              S1HADRDA                             G7D1PGM 
01439          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01440          THEN                                                     G7D1PGM 
01441              NEXT SENTENCE                                        G7D1PGM 
01442          ELSE                                                     G7D1PGM 
01443              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01444              SET WT-01-INDEX TO +03                               G7D1PGM 
01445              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D1PGM 
01446      ELSE                                                         G7D1PGM 
01447          NEXT SENTENCE.                                           G7D1PGM 
01448                                                                   G7D1PGM 
01449                                                                   G7D1PGM 
01450 *----------------------------------------------------------------*G7D1PGM 
01451 *                                                                *G7D1PGM 
01452 *  IF   STAY CODE INDICATOR (S1STCDI) > ZERO                     *G7D1PGM 
01453 *                                                                *G7D1PGM 
01454 *  THEN STAY CODE (S1STYCD) MUST BE > ZERO                       *G7D1PGM 
01455 *                                                                *G7D1PGM 
01456 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7D1PGM 
01457 *                                                                *G7D1PGM 
01458 *----------------------------------------------------------------*G7D1PGM 
01459                                                                   G7D1PGM 
01460      IF  S1STCDII     > ZEROS                                     G7D1PGM 
01461          AND                                                      G7D1PGM 
01462          S1STYCDI NOT > ZEROS                                     G7D1PGM 
01463      THEN                                                         G7D1PGM 
01464          MOVE  -1        TO  S1STYCDL                             G7D1PGM 
01465          MOVE  DFHBMUBF  TO  S1STYCDA                             G7D1PGM 
01466                              S1STCDIA                             G7D1PGM 
01467          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01468          THEN                                                     G7D1PGM 
01469              NEXT SENTENCE                                        G7D1PGM 
01470          ELSE                                                     G7D1PGM 
01471              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01472              SET WT-01-INDEX TO +04                               G7D1PGM 
01473              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D1PGM 
01474      ELSE                                                         G7D1PGM 
01475          NEXT SENTENCE.                                           G7D1PGM 
01476                                                                   G7D1PGM 
01477      IF  S1STYCDI     > ZEROS                                     G7D1PGM 
01478          AND                                                      G7D1PGM 
01479          S1STCDII NOT > ZEROS                                     G7D1PGM 
01480      THEN                                                         G7D1PGM 
01481          MOVE  -1        TO  S1STCDIL                             G7D1PGM 
01482          MOVE  DFHBMUBF  TO  S1STCDIA                             G7D1PGM 
01483                              S1STYCDA                             G7D1PGM 
01484          IF  WS-02-SCREEN-HAS-ERRORS                              G7D1PGM 
01485          THEN                                                     G7D1PGM 
01486              NEXT SENTENCE                                        G7D1PGM 
01487          ELSE                                                     G7D1PGM 
01488              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01489              SET WT-01-INDEX TO +05                               G7D1PGM 
01490              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D1PGM 
01491      ELSE                                                         G7D1PGM 
01492          NEXT SENTENCE.                                           G7D1PGM 
01493                                                                   G7D1PGM 
01494                                                                   G7D1PGM 
01495 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7D1PGM 
01496                                                                   G7D1PGM 
01497      IF  WS-02-SCREEN-HAS-ERRORS                                  G7D1PGM 
01498      THEN                                                         G7D1PGM 
01499          NEXT SENTENCE                                            G7D1PGM 
01500      ELSE                                                         G7D1PGM 
01501          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7D1PGM 
01502          THEN                                                     G7D1PGM 
01503              IF EIBAID = DFHPF4 OR DFHPF16                        G7D1PGM 
01504              THEN                                                 G7D1PGM 
01505                  NEXT SENTENCE                                    G7D1PGM 
01506              ELSE                                                 G7D1PGM 
01507                  MOVE  -1        TO S1ERRL                        G7D1PGM 
01508                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D1PGM 
01509                  SET WT-01-INDEX TO +06                           G7D1PGM 
01510                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D1PGM 
01511          ELSE                                                     G7D1PGM 
01512              NEXT SENTENCE.                                       G7D1PGM 
01513                                                                   G7D1PGM 
01514                                                                   G7D1PGM 
01515  2200-900-EXIT.                                                   G7D1PGM 
01516      EXIT.                                                        G7D1PGM 
01517 /***************************************************************  G7D1PGM 
01518 *                                                              *  G7D1PGM 
01519 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7D1PGM 
01520 *        REWRITE TO WORKFILE.                                  *  G7D1PGM 
01521 *                                                              *  G7D1PGM 
01522 ****************************************************************  G7D1PGM 
01523  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7D1PGM 
01524  2300-010.                                                        G7D1PGM 
01525                                                                   G7D1PGM 
01526 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7D1PGM 
01527                                                                   G7D1PGM 
01528      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7D1PGM 
01529      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7D1PGM 
01530        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7D1PGM 
01531      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7D1PGM 
01532      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7D1PGM 
01533      IF  NOT GCIO2-GOOD-RETURN                                    G7D1PGM 
01534          MOVE WS-01-ABCODE-D1F2     TO WS-01-ABCODE               G7D1PGM 
01535          MOVE WS-01-ABCODE-D1F2-MSG TO WS-01-ABCODE-MSG           G7D1PGM 
01536          PERFORM  9999-000-ABEND-THE-TASK.                        G7D1PGM 
01537                                                                   G7D1PGM 
01538                                                                   G7D1PGM 
01539 *----- SAVE FIELDS FROM SCREEN THAT CANNOT BE DIRECTLY ----------*G7D1PGM 
01540 *        COMPARED TO THE RECORD                                   G7D1PGM 
01541                                                                   G7D1PGM 
01542      MOVE S1HADRDI    TO WS-02-HSP-ADM-RESTRN-DAYS-X.             G7D1PGM 
01543                                                                   G7D1PGM 
01544 *    MOVE S1STYCDI    TO WS-02-STAY-CD-X.                         G7D1PGM 
01545 *    MOVE ZEROS       TO WS-02-RATIO.                             G7D1PGM 
01546 *    MOVE S1DIV1BI    TO WS-02-RATIO-DIVISOR.                     G7D1PGM 
01547 *    MOVE S1BAS1BI    TO WS-02-RATIO-BASE.                        G7D1PGM 
01548 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-BASIC-AP-X.          G7D1PGM 
01549                                                                   G7D1PGM 
01550 *    MOVE ZEROS       TO WS-02-RATIO.                             G7D1PGM 
01551 *    MOVE S1DIV2BI    TO WS-02-RATIO-DIVISOR.                     G7D1PGM 
01552 *    MOVE S1BAS2BI    TO WS-02-RATIO-BASE.                        G7D1PGM 
01553 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-BASIC-BA-X.          G7D1PGM 
01554                                                                   G7D1PGM 
01555 *    MOVE ZEROS       TO WS-02-RATIO.                             G7D1PGM 
01556 *    MOVE S1DIV1SI    TO WS-02-RATIO-DIVISOR.                     G7D1PGM 
01557 *    MOVE S1BAS1SI    TO WS-02-RATIO-BASE.                        G7D1PGM 
01558 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-SEC-AP-X.            G7D1PGM 
01559                                                                   G7D1PGM 
01560 *    MOVE ZEROS       TO WS-02-RATIO.                             G7D1PGM 
01561 *    MOVE S1DIV2SI    TO WS-02-RATIO-DIVISOR.                     G7D1PGM 
01562 *    MOVE S1BAS2SI    TO WS-02-RATIO-BASE.                        G7D1PGM 
01563 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-SEC-BA-X.            G7D1PGM 
01564                                                                   G7D1PGM 
01565                                                                   G7D1PGM 
01566 *------- DEFAULT RATIOS TO 1.0 : 1.0 IF RATIO IND = 0 -----------*G7D1PGM 
01567                                                                   G7D1PGM 
01568 *    IF  S1RDDYII = '0' OR ' '                                    G7D1PGM 
01569 *    THEN                                                         G7D1PGM 
01570 *        MOVE 01.0 TO WS-02-DAYS-RDCN-RAT-BASIC-APL               G7D1PGM 
01571 *                     WS-02-DAYS-RDCN-RAT-BASIC-BASE              G7D1PGM 
01572 *                     WS-02-DAYS-RDCN-RAT-SEC-APL                 G7D1PGM 
01573 *                     WS-02-DAYS-RDCN-RAT-SEC-BASE                G7D1PGM 
01574 *        MOVE '1'  TO S1DIV1BI                                    G7D1PGM 
01575 *                     S1DIV2BI                                    G7D1PGM 
01576 *                     S1DIV1SI                                    G7D1PGM 
01577 *                     S1DIV2SI                                    G7D1PGM 
01578 *        MOVE '0'  TO S1BAS1BI                                    G7D1PGM 
01579 *                     S1BAS2BI                                    G7D1PGM 
01580 *                     S1BAS1SI                                    G7D1PGM 
01581 *                     S1BAS2SI                                    G7D1PGM 
01582 *    ELSE                                                         G7D1PGM 
01583 *        NEXT SENTENCE.                                           G7D1PGM 
01584                                                                   G7D1PGM 
01585                                                                   G7D1PGM 
01586 *------- BASE OF RATIOS CANNOT BE 0.0 ---------------------------*G7D1PGM 
01587                                                                   G7D1PGM 
01588 *    IF  WS-02-DAYS-RDCN-RAT-BASIC-BASE = ZEROS                   G7D1PGM 
01589 *    THEN                                                         G7D1PGM 
01590 *        MOVE  -1        TO S1DIV2BL                              G7D1PGM 
01591 *        MOVE  DFHBMUBF  TO S1DIV2BA                              G7D1PGM 
01592 *                           S1BAS2BA                              G7D1PGM 
01593 *        SET WT-01-INDEX TO +12                                   G7D1PGM 
01594 *        MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH             G7D1PGM 
01595 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D1PGM 
01596 *        IF  WS-02-DAYS-RDCN-RAT-SEC-BASE = ZEROS                 G7D1PGM 
01597 *        THEN                                                     G7D1PGM 
01598 *            MOVE  DFHBMUBF  TO S1DIV2SA                          G7D1PGM 
01599 *                               S1BAS2SA                          G7D1PGM 
01600 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7D1PGM 
01601 *        ELSE                                                     G7D1PGM 
01602 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7D1PGM 
01603 *    ELSE                                                         G7D1PGM 
01604 *        IF  WS-02-DAYS-RDCN-RAT-SEC-BASE = ZEROS                 G7D1PGM 
01605 *        THEN                                                     G7D1PGM 
01606 *            MOVE  -1        TO S1DIV2SL                          G7D1PGM 
01607 *            MOVE  DFHBMUBF  TO S1DIV2SA                          G7D1PGM 
01608 *                               S1BAS2SA                          G7D1PGM 
01609 *            SET WT-01-INDEX TO +12                               G7D1PGM 
01610 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D1PGM 
01611 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D1PGM 
01612 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7D1PGM 
01613 *        ELSE                                                     G7D1PGM 
01614 *            NEXT SENTENCE.                                       G7D1PGM 
01615                                                                   G7D1PGM 
01616                                                                   G7D1PGM 
01617 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7D1PGM 
01618                                                                   G7D1PGM 
01619         MOVE GPD2-DAYS-RDCN-RAT-BASIC-APL TO                      G7D1PGM 
01620           WS-GPD2-DAYS-RDCN-RAT-BAS-APL.                          G7D1PGM 
01621                                                                   G7D1PGM 
01622         MOVE GPD2-DAYS-RDCN-RAT-BASIC-BASE TO                     G7D1PGM 
01623           WS-GPD2-DAYS-RDCN-RAT-BAS-BASE.                         G7D1PGM 
01624                                                                   G7D1PGM 
01625         MOVE GPD2-DAYS-RDCN-RAT-SEC-APL TO                        G7D1PGM 
01626           WS-GPD2-DAYS-RDCN-RAT-SEC-APL.                          G7D1PGM 
01627                                                                   G7D1PGM 
01628         MOVE GPD2-DAYS-RDCN-RAT-SEC-BASE TO                       G7D1PGM 
01629           WS-GPD2-DAYS-RDCN-RAT-SEC-BASE.                         G7D1PGM 
01630                                                                   G7D1PGM 
01631      IF     S1BESCII              =  GPD2-BEN-SCOPE-ID            G7D1PGM 
01632         AND S1EXCSCI              =  GPD2-EXCP-SCHED-ID           G7D1PGM 
01633         AND S1HADMRI              =  GPD2-HOSP-ADM-RESTRN-IND     G7D1PGM 
01634         AND WS-02-HSP-ADM-RESTRN-DAYS                             G7D1PGM 
01635                                   =  GPD2-HSP-ADM-RESTRN-DAYS     G7D1PGM 
01636         AND S1HCNDRI              =  GPD2-HOSP-COND-RELATSP-IND   G7D1PGM 
01637         AND S1STCDII              =  GPD2-STAY-CODE-IND           G7D1PGM 
01638         AND WS-02-STAY-CD         =  GPD2-STAY-CD                 G7D1PGM 
01639         AND S1CORORI              =  GPD2-CORRIDOR-OVERRIDE       G7D1PGM 
01640         AND S1RDDYII              =  GPD2-DAYS-RDCN-RAT-IND       G7D1PGM 
01641         AND WS-02-DAYS-RDCN-RAT-BASIC-APL                         G7D1PGM 
01642                                 = WS-GPD2-DAYS-RDCN-RAT-BAS-APL   G7D1PGM 
01643         AND WS-02-DAYS-RDCN-RAT-BASIC-BASE                        G7D1PGM 
01644                                 = WS-GPD2-DAYS-RDCN-RAT-BAS-BASE  G7D1PGM 
01645         AND WS-02-DAYS-RDCN-RAT-SEC-APL                           G7D1PGM 
01646                                 = WS-GPD2-DAYS-RDCN-RAT-SEC-APL   G7D1PGM 
01647         AND WS-02-DAYS-RDCN-RAT-SEC-BASE                          G7D1PGM 
01648                                 = WS-GPD2-DAYS-RDCN-RAT-SEC-BASE  G7D1PGM 
01649      THEN                                                         G7D1PGM 
01650          GO TO 2300-900-EXIT                                      G7D1PGM 
01651      ELSE                                                         G7D1PGM 
01652          NEXT SENTENCE.                                           G7D1PGM 
01653                                                                   G7D1PGM 
01654                                                                   G7D1PGM 
01655 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7D1PGM 
01656                                                                   G7D1PGM 
01657      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7D1PGM 
01658      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7D1PGM 
01659      IF  NOT GCIO2-GOOD-RETURN                                    G7D1PGM 
01660          MOVE WS-01-ABCODE-D1F3     TO WS-01-ABCODE               G7D1PGM 
01661          MOVE WS-01-ABCODE-D1F3-MSG TO WS-01-ABCODE-MSG           G7D1PGM 
01662          PERFORM  9999-000-ABEND-THE-TASK.                        G7D1PGM 
01663                                                                   G7D1PGM 
01664                                                                   G7D1PGM 
01665 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7D1PGM 
01666                                                                   G7D1PGM 
01667      MOVE S1BESCII              TO GPD2-BEN-SCOPE-ID.             G7D1PGM 
01668      MOVE S1EXCSCI              TO GPD2-EXCP-SCHED-ID.            G7D1PGM 
01669      MOVE S1HADMRI              TO GPD2-HOSP-ADM-RESTRN-IND.      G7D1PGM 
01670      MOVE WS-02-HSP-ADM-RESTRN-DAYS                               G7D1PGM 
01671                                 TO GPD2-HSP-ADM-RESTRN-DAYS.      G7D1PGM 
01672      MOVE S1HCNDRI              TO GPD2-HOSP-COND-RELATSP-IND.    G7D1PGM 
01673      MOVE S1STCDII              TO GPD2-STAY-CODE-IND.            G7D1PGM 
01674      MOVE WS-02-STAY-CD         TO GPD2-STAY-CD.                  G7D1PGM 
01675      MOVE S1CORORI              TO GPD2-CORRIDOR-OVERRIDE.        G7D1PGM 
01676      MOVE S1RDDYII              TO GPD2-DAYS-RDCN-RAT-IND.        G7D1PGM 
01677      MOVE WS-02-DAYS-RDCN-RAT-BASIC-APL                           G7D1PGM 
01678                                 TO GPD2-DAYS-RDCN-RAT-BASIC-APL.  G7D1PGM 
01679      MOVE WS-02-DAYS-RDCN-RAT-BASIC-BASE                          G7D1PGM 
01680                                 TO GPD2-DAYS-RDCN-RAT-BASIC-BASE. G7D1PGM 
01681      MOVE WS-02-DAYS-RDCN-RAT-SEC-APL                             G7D1PGM 
01682                                 TO GPD2-DAYS-RDCN-RAT-SEC-APL.    G7D1PGM 
01683      MOVE WS-02-DAYS-RDCN-RAT-SEC-BASE                            G7D1PGM 
01684                                 TO GPD2-DAYS-RDCN-RAT-SEC-BASE.   G7D1PGM 
01685                                                                   G7D1PGM 
01686                                                                   G7D1PGM 
01687 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7D1PGM 
01688                                                                   G7D1PGM 
01689 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7D1PGM 
01690                                                                   G7D1PGM 
01691      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7D1PGM 
01692      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7D1PGM 
01693      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7D1PGM 
01694      IF  NOT GCIO2-GOOD-RETURN                                    G7D1PGM 
01695          MOVE WS-01-ABCODE-D1F4     TO WS-01-ABCODE               G7D1PGM 
01696          MOVE WS-01-ABCODE-D1F4-MSG TO WS-01-ABCODE-MSG           G7D1PGM 
01697          PERFORM  9999-000-ABEND-THE-TASK.                        G7D1PGM 
01698                                                                   G7D1PGM 
01699  2300-900-EXIT.                                                   G7D1PGM 
01700      EXIT.                                                        G7D1PGM 
01701 /***************************************************************  G7D1PGM 
01702 *                                                              *  G7D1PGM 
01703 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7D1PGM 
01704 *                                                              *  G7D1PGM 
01705 ****************************************************************  G7D1PGM 
01706  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7D1PGM 
01707  2310-010.                                                        G7D1PGM 
01708                                                                   G7D1PGM 
01709                                                                   G7D1PGM 
01710 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7D1PGM 
01711                                                                   G7D1PGM 
01712      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7D1PGM 
01713                                         + GC-WORKFILE-KEY-LEN     G7D1PGM 
01714                                         + GC-GCBENPRV-MAX-REC-LEN.G7D1PGM 
01715                                                                   G7D1PGM 
01716      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7D1PGM 
01717                          INITIMG(WS-02-HEX-00)                    G7D1PGM 
01718                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7D1PGM 
01719                          END-EXEC.                                G7D1PGM 
01720                                                                   G7D1PGM 
01721 *    SERVICE RELOAD  IO-PARM-BEN-PROV-AREA.                       G7D1PGM 
01722                                                                   G7D1PGM 
01723 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7D1PGM 
01724                                                                   G7D1PGM 
01725      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7D1PGM 
01726                                                                   G7D1PGM 
01727      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7D1PGM 
01728      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7D1PGM 
01729      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7D1PGM 
01730      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7D1PGM 
01731      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7D1PGM 
01732      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7D1PGM 
01733                                                                   G7D1PGM 
01734      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7D1PGM 
01735      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7D1PGM 
01736      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7D1PGM 
01737      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7D1PGM 
01738      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7D1PGM 
01739      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7D1PGM 
01740      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7D1PGM 
01741      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7D1PGM 
01742                                                                   G7D1PGM 
01743 ***  MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7D1PGM 
01744 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7D1PGM 
01745 ***  IF  HGADATE-RETURN = ZEROS                                   G7D1PGM 
01746 ***  THEN                                                         G7D1PGM 
01747 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7D1PGM 
01748 ***  ELSE                                                         G7D1PGM 
01749 ***      SET WT-01-INDEX TO +07                                   G7D1PGM 
01750 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D1PGM 
01751 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7D1PGM 
01752                                                                   G7D1PGM 
01753      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7D1PGM 
01754      MOVE S1BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7D1PGM 
01755      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7D1PGM 
01756      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7D1PGM 
01757      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7D1PGM 
01758      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7D1PGM 
01759      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7D1PGM 
01760                                                                   G7D1PGM 
01761                                                                   G7D1PGM 
01762  2310-900-EXIT.                                                   G7D1PGM 
01763      EXIT.                                                        G7D1PGM 
01764 /***************************************************************  G7D1PGM 
01765 *                                                              *  G7D1PGM 
01766 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7D1PGM 
01767 *                                                              *  G7D1PGM 
01768 ****************************************************************  G7D1PGM 
01769  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7D1PGM 
01770  2400-010.                                                        G7D1PGM 
01771                                                                   G7D1PGM 
01772                                                                   G7D1PGM 
01773      IF  EIBAID = DFHPF7  OR DFHPF19                              G7D1PGM 
01774      THEN                                                         G7D1PGM 
01775          MOVE 'GC6CPGM' TO WS-02-NEXT-PROGRAM.                    G7D1PGM 
01776                                                                   G7D1PGM 
01777      IF  EIBAID = DFHENTER OR                                     G7D1PGM 
01778                   DFHPF4   OR DFHPF16 OR                          G7D1PGM 
01779                   DFHPF8   OR DFHPF20                             G7D1PGM 
01780      THEN                                                         G7D1PGM 
01781          MOVE 'G7D2PGM' TO WS-02-NEXT-PROGRAM.                    G7D1PGM 
01782                                                                   G7D1PGM 
01783      IF  EIBAID = DFHPF6  OR DFHPF18                              G7D1PGM 
01784      THEN                                                         G7D1PGM 
01785          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7D1PGM 
01786                                                                   G7D1PGM 
01787                                                                   G7D1PGM 
01788      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7D1PGM 
01789                       COMMAREA(WORK-RECORD-2)                     G7D1PGM 
01790                       LENGTH  (GCIO2-RECORD-LENGTH)               G7D1PGM 
01791                       END-EXEC.                                   G7D1PGM 
01792                                                                   G7D1PGM 
01793  2400-900-EXIT.                                                   G7D1PGM 
01794      EXIT.                                                        G7D1PGM 
01795 /***************************************************************  G7D1PGM 
01796 *                                                              *  G7D1PGM 
01797 * 2500   LINK TO GX3APGM FOR CONVERSION.                          G7D1PGM 
01798 *                                                              *  G7D1PGM 
01799 ****************************************************************  G7D1PGM 
01800  2500-LINK-TO-GX3APGM.                                            G7D1PGM 
01801                                                                   G7D1PGM 
01802      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7D1PGM 
01803                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7D1PGM 
01804                       LENGTH  (+51)                               G7D1PGM 
01805                       END-EXEC.                                   G7D1PGM 
01806                                                                   G7D1PGM 
01807                                                                   G7D1PGM 
01808  2500-EXIT.                                                       G7D1PGM 
01809      EXIT.                                                        G7D1PGM 
01810 /***************************************************************  G7D1PGM 
01811 *                                                              *  G7D1PGM 
01812 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7D1PGM 
01813 *         PROVISION RECORD (TYPE=C4)                           *  G7D1PGM 
01814 *                                                              *  G7D1PGM 
01815 ****************************************************************  G7D1PGM 
01816  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7D1PGM 
01817  5000-010.                                                        G7D1PGM 
01818                                                                   G7D1PGM 
01819      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7D1PGM 
01820                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7D1PGM 
01821                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7D1PGM 
01822                       END-EXEC.                                   G7D1PGM 
01823                                                                   G7D1PGM 
01824                                                                   G7D1PGM 
01825  5000-900-EXIT.                                                   G7D1PGM 
01826      EXIT.                                                        G7D1PGM 
01827 /***************************************************************  G7D1PGM 
01828 *                                                              *  G7D1PGM 
01829 * 5100                                                         *  G7D1PGM 
01830 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7D1PGM 
01831 *                                                              *  G7D1PGM 
01832 ****************************************************************  G7D1PGM 
01833  5100-000-W-F-CONTRACT-IO       SECTION.                          G7D1PGM 
01834  5100-010.                                                        G7D1PGM 
01835                                                                   G7D1PGM 
01836      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7D1PGM 
01837                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7D1PGM 
01838                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7D1PGM 
01839                       END-EXEC.                                   G7D1PGM 
01840                                                                   G7D1PGM 
01841                                                                   G7D1PGM 
01842  5100-900-EXIT.                                                   G7D1PGM 
01843      EXIT.                                                        G7D1PGM 
01844 /***************************************************************  G7D1PGM 
01845 *                                                              *  G7D1PGM 
01846 * 9000   MOVE MESSAGE TO SCREEN                                *  G7D1PGM 
01847 *                                                              *  G7D1PGM 
01848 ****************************************************************  G7D1PGM 
01849  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7D1PGM 
01850  9000-010.                                                        G7D1PGM 
01851                                                                   G7D1PGM 
01852      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S1ERRO.              G7D1PGM 
01853                                                                   G7D1PGM 
01854  9000-900-EXIT.                                                   G7D1PGM 
01855      EXIT.                                                        G7D1PGM 
01856 /***************************************************************  G7D1PGM 
01857 *                                                              *  G7D1PGM 
01858 * 9100 SEND SCREEN AND RETURN                                  *  G7D1PGM 
01859 *                                                              *  G7D1PGM 
01860 ****************************************************************  G7D1PGM 
01861  9100-000-SEND-THEN-RETURN      SECTION.                          G7D1PGM 
01862  9100-010.                                                        G7D1PGM 
01863                                                                   G7D1PGM 
01864                                                                   G7D1PGM 
01865 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7D1PGM 
01866      MOVE  -1 TO  S1ERRL.                                         G7D1PGM 
01867                                                                   G7D1PGM 
01868                                                                   G7D1PGM 
01869      IF  WS-02-MY-EIBTRNID                                        G7D1PGM 
01870      THEN                                                         G7D1PGM 
01871          EXEC CICS  SEND MAP   ('G7D1I01')                        G7D1PGM 
01872                          MAPSET('G7D1SET')                        G7D1PGM 
01873                          DATAONLY                                 G7D1PGM 
01874                          CURSOR                                   G7D1PGM 
01875                          END-EXEC                                 G7D1PGM 
01876      ELSE                                                         G7D1PGM 
01877          EXEC CICS  SEND MAP   ('G7D1I01')                        G7D1PGM 
01878                          MAPSET('G7D1SET')                        G7D1PGM 
01879                          ERASE                                    G7D1PGM 
01880                          CURSOR                                   G7D1PGM 
01881                          END-EXEC.                                G7D1PGM 
01882                                                                   G7D1PGM 
01883      EXEC CICS RETURN                                             G7D1PGM 
01884                TRANSID  ('G7D1')                                  G7D1PGM 
01885                COMMAREA (DFHCOMMAREA)                             G7D1PGM 
01886                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7D1PGM 
01887                END-EXEC.                                          G7D1PGM 
01888 *                                                                 G7D1PGM 
01889  9100-900-EXIT.                                                   G7D1PGM 
01890      EXIT.                                                        G7D1PGM 
01891 /*****************************************************************G7D1PGM 
01892 *                                                                *G7D1PGM 
01893 * 9200    XCTL TO GCPSPGM                                        *G7D1PGM 
01894 *                                                                *G7D1PGM 
01895 *                                                                *G7D1PGM 
01896 ******************************************************************G7D1PGM 
01897  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7D1PGM 
01898  9200-010.                                                        G7D1PGM 
01899                                                                   G7D1PGM 
01900      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7D1PGM 
01901                       END-EXEC.                                   G7D1PGM 
01902                                                                   G7D1PGM 
01903  9200-900-EXIT.                                                   G7D1PGM 
01904      EXIT.                                                        G7D1PGM 
01905 /*****************************************************************G7D1PGM 
01906 *                                                                *G7D1PGM 
01907 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7D1PGM 
01908 *                                                                *G7D1PGM 
01909 *                                                                *G7D1PGM 
01910 ******************************************************************G7D1PGM 
01911  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7D1PGM 
01912  9210-010.                                                        G7D1PGM 
01913                                                                   G7D1PGM 
01914      IF  S1GRPNOI = '000SPS000'                                   G7D1PGM 
01915          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7D1PGM 
01916                           END-EXEC.                               G7D1PGM 
01917                                                                   G7D1PGM 
01918 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7D1PGM 
01919                                                                   G7D1PGM 
01920      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7D1PGM 
01921                                        + GC-WORKFILE-KEY-LEN      G7D1PGM 
01922                                        + GC-GCCONTR-MAX-REC-LEN.  G7D1PGM 
01923                                                                   G7D1PGM 
01924      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7D1PGM 
01925                          INITIMG(WS-02-HEX-00)                    G7D1PGM 
01926                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7D1PGM 
01927                          END-EXEC.                                G7D1PGM 
01928                                                                   G7D1PGM 
01929 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7D1PGM 
01930 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7D1PGM 
01931                                                                   G7D1PGM 
01932 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7D1PGM 
01933                                                                   G7D1PGM 
01934      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7D1PGM 
01935        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7D1PGM 
01936                                                                   G7D1PGM 
01937      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7D1PGM 
01938      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7D1PGM 
01939                                                                   G7D1PGM 
01940      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7D1PGM 
01941      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7D1PGM 
01942      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7D1PGM 
01943      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7D1PGM 
01944      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7D1PGM 
01945      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7D1PGM 
01946                                                                   G7D1PGM 
01947      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7D1PGM 
01948      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7D1PGM 
01949      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7D1PGM 
01950      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7D1PGM 
01951      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7D1PGM 
01952      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7D1PGM 
01953      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7D1PGM 
01954      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7D1PGM 
01955                                                                   G7D1PGM 
01956      MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7D1PGM 
01957      PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7D1PGM 
01958      IF  HGADATE-RETURN = ZEROS                                   G7D1PGM 
01959      THEN                                                         G7D1PGM 
01960          MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7D1PGM 
01961      ELSE                                                         G7D1PGM 
01962          SET WT-01-INDEX TO +07                                   G7D1PGM 
01963          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D1PGM 
01964          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D1PGM 
01965                                                                   G7D1PGM 
01966      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7D1PGM 
01967      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7D1PGM 
01968      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7D1PGM 
01969      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7D1PGM 
01970      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7D1PGM 
01971      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7D1PGM 
01972      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7D1PGM 
01973                                                                   G7D1PGM 
01974      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7D1PGM 
01975                                                                   G7D1PGM 
01976      IF  NOT GCIO3-GOOD-RETURN                                    G7D1PGM 
01977          MOVE WS-01-ABCODE-D1F1     TO WS-01-ABCODE               G7D1PGM 
01978          MOVE WS-01-ABCODE-D1F1-MSG TO WS-01-ABCODE-MSG           G7D1PGM 
01979          PERFORM  9999-000-ABEND-THE-TASK.                        G7D1PGM 
01980                                                                   G7D1PGM 
01981      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7D1PGM 
01982                       COMMAREA(WORK-RECORD-3)                     G7D1PGM 
01983                       LENGTH  (GCIO3-RECORD-LENGTH)               G7D1PGM 
01984                       END-EXEC.                                   G7D1PGM 
01985                                                                   G7D1PGM 
01986  9210-900-EXIT.                                                   G7D1PGM 
01987      EXIT.                                                        G7D1PGM 
01988 /*****************************************************************G7D1PGM 
01989 *                                                                *G7D1PGM 
01990 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7D1PGM 
01991 *                                                                *G7D1PGM 
01992 *                                                                *G7D1PGM 
01993 ******************************************************************G7D1PGM 
01994  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7D1PGM 
01995  9220-010.                                                        G7D1PGM 
01996                                                                   G7D1PGM 
01997      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7D1PGM 
01998                       END-EXEC.                                   G7D1PGM 
01999                                                                   G7D1PGM 
02000  9220-900-EXIT.                                                   G7D1PGM 
02001      EXIT.                                                        G7D1PGM 
02002 /*****************************************************************G7D1PGM 
02003 *                                                                *G7D1PGM 
02004 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7D1PGM 
02005 *                                                                *G7D1PGM 
02006 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7D1PGM 
02007 *                                                                *G7D1PGM 
02008 ******************************************************************G7D1PGM 
02009  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7D1PGM 
02010  9800-010.                                                        G7D1PGM 
02011                                                                   G7D1PGM 
02012      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7D1PGM 
02013      MOVE 'M'   TO  HGADATE-FORM1.                                G7D1PGM 
02014      MOVE 'J'   TO  HGADATE-FORM2.                                G7D1PGM 
02015      MOVE ZEROS TO  HGADATE-RETURN                                G7D1PGM 
02016                     HGADATE-AMOUNT.                               G7D1PGM 
02017      EXEC CICS LINK PROGRAM ('HGADATES')                          G7D1PGM 
02018                     COMMAREA(HGADATES-COMMAREA)                   G7D1PGM 
02019                     LENGTH  (24)                                  G7D1PGM 
02020                     END-EXEC.                                     G7D1PGM 
02021                                                                   G7D1PGM 
02022  9800-900-900-EXIT.                                               G7D1PGM 
02023      EXIT.                                                        G7D1PGM 
02024 /*****************************************************************G7D1PGM 
02025 *                                                                *G7D1PGM 
02026 * 9810    J U L I A N    T O    G R E G O R I A N                *G7D1PGM 
02027 *                                                                *G7D1PGM 
02028 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7D1PGM 
02029 *                                                                *G7D1PGM 
02030 ******************************************************************G7D1PGM 
02031  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7D1PGM 
02032  9810-010.                                                        G7D1PGM 
02033                                                                   G7D1PGM 
02034      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7D1PGM 
02035      MOVE 'J'   TO  HGADATE-FORM1.                                G7D1PGM 
02036      MOVE 'M'   TO  HGADATE-FORM2.                                G7D1PGM 
02037      MOVE ZEROS TO  HGADATE-RETURN                                G7D1PGM 
02038                     HGADATE-AMOUNT.                               G7D1PGM 
02039      EXEC CICS LINK PROGRAM ('HGADATES')                          G7D1PGM 
02040                     COMMAREA(HGADATES-COMMAREA)                   G7D1PGM 
02041                     LENGTH  (24)                                  G7D1PGM 
02042                     END-EXEC.                                     G7D1PGM 
02043                                                                   G7D1PGM 
02044  9810-900-900-EXIT.                                               G7D1PGM 
02045      EXIT.                                                        G7D1PGM 
02046 /***************************************************************  G7D1PGM 
02047 *                                                              *  G7D1PGM 
02048 * 9999  ABEND THE TASK                                         *  G7D1PGM 
02049 *                                                              *  G7D1PGM 
02050 ****************************************************************  G7D1PGM 
02051  9999-000-ABEND-THE-TASK SECTION.                                 G7D1PGM 
02052  9999-010.                                                        G7D1PGM 
02053                                                                   G7D1PGM 
02054      EXEC CICS  ABEND                                             G7D1PGM 
02055                 ABCODE(WS-01-ABCODE)                              G7D1PGM 
02056                 END-EXEC.                                         G7D1PGM 
02057                                                                   G7D1PGM 
02058  9900-900-EXIT.                                                   G7D1PGM 
02059      EXIT.                                                        G7D1PGM 
