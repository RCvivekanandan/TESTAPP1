00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7D2PGM.                                         G7D2PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7D2PGM 
00005  DATE-WRITTEN.   03/12/87.                                        G7D2PGM 
00006  DATE-COMPILED.                                                   G7D2PGM 
00007 ***************************************************************** G7D2PGM 
00008 *                                                               * G7D2PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7D2PGM 
00010 *                                                               * G7D2PGM 
00011 *                                                               * G7D2PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7D2PGM 
00013 *                                                               * G7D2PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7D2PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7D2PGM 
00016 *                             (SAME AS 'GC5A')                  * G7D2PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7D2PGM 
00018 *                              'GC5A')                          * G7D2PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7D2PGM 
00020 *                              PROVISION)                       * G7D2PGM 
00021 *                                                               * G7D2PGM 
00022 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7D2PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7D2PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7D2PGM 
00025 *                                                               * G7D2PGM 
00026 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7D2PGM 
00027 *                                                               * G7D2PGM 
00028 *  D129      08/25/89  GDM    CONVERT FOR DECIMALS.             * G7D2PGM 
00029 *                                                               * G7D2PGM 
00030 *  D129      09/08/89  GDM    CONVERT TO VS COBOL/2             * G7D2PGM 
00031 *                                                               * G7D2PGM 
00032 *  D12009    08/23/91  BSO   -CORRECT ERR MESSAGES IN AREA \
00033 *                            -CORRECT ALPHA CLASS TEST AREA     * G7D2PGM 
00034 *                                                               * G7D2PGM 
00035 *  D14726    11/06/97  GDM 1. ADDED MILLENNIUM PROCESSING FOR   * G7D2PGM 
00036 *                             DATE                              * G7D2PGM 
00037 *                          2. EXPAND THE COMMAREA KEY TO        * G7D2PGM 
00038 *                             SUPPORT THE TEXAS MERGER.         * G7D2PGM 
00039 *                                                               * G7D2PGM 
00040 * 14726/     03/30/98  GSP    ADDED PLAN AND PACKAGE CODE AND   * G7D2PGM 
00041 * 15057                       INCREASED GROUP AND SECTION ON    * G7D2PGM 
00042 *                             THE SCREEN.                       * G7D2PGM 
00043 *                                                               * G7D2PGM 
00044 *            12/11/02  AKK    OPID COMPILE                      * G7D2PGM 
00045 *                                                               * G7D2PGM 
00046 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7D2PGM 
00047 *                           G7D2SET                              *G7D2PGM 
00048 ***************************************************************** G7D2PGM 
00049                                                                   G7D2PGM 
00050 ***************************************************************** G7D2PGM 
00051 *                                                               * G7D2PGM 
00052 *    G7D2PGM  - PROGRAM 2 OF 2 PROGRAMS TO UPDATE THE FORMAT 'C'* G7D2PGM 
00053 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7D2PGM 
00054 *                                                               * G7D2PGM 
00055 *    TRANSID: G7D2                                              * G7D2PGM 
00056 *    MAPSET:  G7D2SETC    (GID2PGM WHICH SHARES THIS MAP)       * G7D2PGM 
00057 *    VALGEN:  NONE                                              * G7D2PGM 
00058 *                                                               * G7D2PGM 
00059 *    PROGRAM NARRATIVE:                                         * G7D2PGM 
00060 *                                                               * G7D2PGM 
00061 *        PROGRAM CHECKS FOR TRANS CODE 'G7D2'.  AN INVALID      * G7D2PGM 
00062 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7D2PGM 
00063 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7D2PGM 
00064 *                                                               * G7D2PGM 
00065 *        THE MAIN FUNCTIONS ARE :                               * G7D2PGM 
00066 *        1. HARDCOPY REQUEST,                                   * G7D2PGM 
00067 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7D2PGM 
00068 *           USER,                                               * G7D2PGM 
00069 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7D2PGM 
00070 *                                                               * G7D2PGM 
00071 *        HARDCOPY REQUEST                                       * G7D2PGM 
00072 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7D2PGM 
00073 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7D2PGM 
00074 *           THE SCREEN BUFFER.                                  * G7D2PGM 
00075 *                                                               * G7D2PGM 
00076 *        PROCESS INPUT DATA (UPDATE).                           * G7D2PGM 
00077 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7D2PGM 
00078 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7D2PGM 
00079 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7D2PGM 
00080 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7D2PGM 
00081 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7D2PGM 
00082 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7D2PGM 
00083 *                                                               * G7D2PGM 
00084 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7D2PGM 
00085 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7D2PGM 
00086 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7D2PGM 
00087 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7D2PGM 
00088 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7D2PGM 
00089 *                                                               * G7D2PGM 
00090 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7D2PGM 
00091 *                                     -----------------------   * G7D2PGM 
00092 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7D2PGM 
00093 *           -----------------------------------------------     * G7D2PGM 
00094 *                                                               * G7D2PGM 
00095 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7D2PGM 
00096 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7D2PGM 
00097 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7D2PGM 
00098 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7D2PGM 
00099 *           SENT BACK TO THE USER.                              * G7D2PGM 
00100 *                                                               * G7D2PGM 
00101 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7D2PGM 
00102 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7D2PGM 
00103 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7D2PGM 
00104 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7D2PGM 
00105 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7D2PGM 
00106 *           PF KEY LOGIC,                                       * G7D2PGM 
00107 *              PF6/PF18       GOES TO GC8APGM                   * G7D2PGM 
00108 *              PF8/PF20/ENTER GOES TO GC6APGM                   * G7D2PGM 
00109 *              FOR PF7/PF19   GOES TO G7D1PGM                   * G7D2PGM 
00110 *                                                               * G7D2PGM 
00111 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7D2PGM 
00112 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7D2PGM 
00113 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7D2PGM 
00114 *                                                               * G7D2PGM 
00115 ***************************************************************** G7D2PGM 
00116                                                                   G7D2PGM 
00117  ENVIRONMENT DIVISION.                                            G7D2PGM 
00118  DATA DIVISION.                                                   G7D2PGM 
00119 /                                                                 G7D2PGM 
00120  WORKING-STORAGE SECTION.                                         G7D2PGM 
00121  01  WS-BEGIN                    PIC X(58) VALUE                  G7D2PGM 
00122      '*** G7D2PGM  WORKING-STORAGE BEGINS HERE ***'.              G7D2PGM 
00123                                                                   G7D2PGM 
00124                                                                   G7D2PGM 
00125  01  WS-01-ABEND-AREA.                                            G7D2PGM 
00126      05  FILLER                   PIC X(16)  VALUE                G7D2PGM 
00127          '** ABEND AREA **'.                                      G7D2PGM 
00128                                                                   G7D2PGM 
00129      05  WS-01-ABEND-CODES-AND-MSG.                               G7D2PGM 
00130          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7D2PGM 
00131          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7D2PGM 
00132                                                                   G7D2PGM 
00133          10  WS-01-ABCODE-D2F1          PIC X(04)  VALUE  'D2F1'. G7D2PGM 
00134          10  WS-01-ABCODE-D2F1-MSG      PIC X(44)  VALUE          G7D2PGM 
00135             'W/F CONTRACT CANNOT BE FOUND             '.          G7D2PGM 
00136                                                                   G7D2PGM 
00137          10  WS-01-ABCODE-D2F2          PIC X(04)  VALUE  'D2F2'. G7D2PGM 
00138          10  WS-01-ABCODE-D2F2-MSG      PIC X(44)  VALUE          G7D2PGM 
00139             'W/F BEN PROV CANNOT BE FOUND             '.          G7D2PGM 
00140                                                                   G7D2PGM 
00141          10  WS-01-ABCODE-D2F3          PIC X(04)  VALUE  'D2F3'. G7D2PGM 
00142          10  WS-01-ABCODE-D2F3-MSG      PIC X(44)  VALUE          G7D2PGM 
00143             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7D2PGM 
00144                                                                   G7D2PGM 
00145          10  WS-01-ABCODE-D2F4          PIC X(04)  VALUE  'D2F4'. G7D2PGM 
00146          10  WS-01-ABCODE-D2F4-MSG      PIC X(44)  VALUE          G7D2PGM 
00147             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7D2PGM 
00148                                                                   G7D2PGM 
00149          10  WS-01-ABCODE-D2L1          PIC X(04)  VALUE  'D2L1'. G7D2PGM 
00150          10  WS-01-ABCODE-D2L1-MSG      PIC X(44)  VALUE          G7D2PGM 
00151             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7D2PGM 
00152                                                                   G7D2PGM 
00153          10  WS-01-ABCODE-D2P1          PIC X(04)  VALUE  'D2P1'. G7D2PGM 
00154          10  WS-01-ABCODE-D2P1-MSG      PIC X(44)  VALUE          G7D2PGM 
00155             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7D2PGM 
00156                                                                   G7D2PGM 
00157          10  WS-01-ABCODE-D2P2          PIC X(04)  VALUE  'D2P2'. G7D2PGM 
00158          10  WS-01-ABCODE-D2P2-MSG      PIC X(44)  VALUE          G7D2PGM 
00159             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7D2PGM 
00160                                                                   G7D2PGM 
00161  01  WS-02-AREA.                                                  G7D2PGM 
00162      05  FILLER                   PIC X(16)  VALUE                G7D2PGM 
00163          '** WS-02-AREA **'.                                      G7D2PGM 
00164      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7D2PGM 
00165          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7D2PGM 
00166                                                    'GC6A' 'G7D1'  G7D2PGM 
00167                                                    'G7D2'.        G7D2PGM 
00168          88  WS-02-MY-EIBTRNID                     VALUE  'G7D2'. G7D2PGM 
00169                                                                   G7D2PGM 
00170      05  WS-02-COMPUTED-LENGTHS.                                  G7D2PGM 
00171          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7D2PGM 
00172          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7D2PGM 
00173          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7D2PGM 
00174                                                                   G7D2PGM 
00175      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7D2PGM 
00176                                                                   G7D2PGM 
00177      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7D2PGM 
00178                                                                   G7D2PGM 
00179      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7D2PGM 
00180      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7D2PGM 
00181          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7D2PGM 
00182                                         PIC X.                    G7D2PGM 
00183          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7D2PGM 
00184                                                    '0' THRU '9'   G7D2PGM 
00185                                                    'A' THRU 'I'   G7D2PGM 
00186                                                    'J' THRU 'R'   G7D2PGM 
00187                                                    'S' THRU 'Z'   G7D2PGM 
00188                                                    SPACE.         G7D2PGM 
00189                                                                   G7D2PGM 
00190      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7D2PGM 
00191          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7D2PGM 
00192          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7D2PGM 
00193                                                                   G7D2PGM 
00194      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7D2PGM 
00195          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7D2PGM 
00196                                                                   G7D2PGM 
00197      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7D2PGM 
00198                                                                   G7D2PGM 
00199      05  WS-02-HEX-F00000.                                        G7D2PGM 
00200          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7D2PGM 
00201          10  FILLER                     PIC  X(09) VALUE          G7D2PGM 
00202                                                    LOW-VALUES.    G7D2PGM 
00203                                                                   G7D2PGM 
00204      05  WS-02-MAX-AMT-PER-VISIT-X.                               G7D2PGM 
00205          10  WS-02-MAX-AMT-PER-VISIT      PIC 999V99  VALUE ZEROS.G7D2PGM 
00206 *        10  WS-02-S2MAXAT              REDEFINES                 G7D2PGM 
00207 *            WS-02-MAX-AMT-PER-VISIT      PIC X(5).               G7D2PGM 
00208                                                                   G7D2PGM 
00209      05  WS-02-BEN-MAX-VISIT-DAYS-X.                              G7D2PGM 
00210          10  WS-02-BEN-MAX-VISIT-DAYS     PIC 999     VALUE ZEROS.G7D2PGM 
00211          10  WS-02-S2BMVST              REDEFINES                 G7D2PGM 
00212              WS-02-BEN-MAX-VISIT-DAYS     PIC X(3).               G7D2PGM 
00213                                                                   G7D2PGM 
00214      05  WS-02-FLAT-RATE-PDM-AMT-X.                               G7D2PGM 
00215          10  WS-02-FLAT-RATE-PDM-AMT      PIC 9(5)V99 VALUE ZEROS.G7D2PGM 
00216 *        10  WS-02-S2FLPDY              REDEFINES                 G7D2PGM 
00217 *            WS-02-FLAT-RATE-PDM-AMT      PIC X(7).               G7D2PGM 
00218                                                                   G7D2PGM 
00219      05  WS-02-TREAT-TIME-FACTOR-X.                               G7D2PGM 
00220          10  WS-02-TREAT-TIME-FACTOR      PIC 999     VALUE ZEROS.G7D2PGM 
00221          10  WS-02-S2TRTMF              REDEFINES                 G7D2PGM 
00222              WS-02-TREAT-TIME-FACTOR      PIC X(3).               G7D2PGM 
00223                                                                   G7D2PGM 
00224      05  WS-7POS-MAX-AMT          PIC 99999V99 VALUE 99999.99.    G7D2PGM 
00225      05  WS-02-DISP-7POS-DEC      PIC 9(5).99.                    G7D2PGM 
00226      05  WS-GPD2-FLAT-RATE-PDM-AMT        PIC 9(5)V99.            G7D2PGM 
00227                                                                   G7D2PGM 
00228      05  WS-5POS-MAX-AMT          PIC 999V99 VALUE 999.99.        G7D2PGM 
00229      05  WS-02-DISP-5POS-DEC      PIC 9(3).99.                    G7D2PGM 
00230      05  WS-GPD2-MAX-AMT-PER-VISIT        PIC 9(3)V99.            G7D2PGM 
00231 /                                                                 G7D2PGM 
00232  01  WT-00-G7D2PGM-TABLES.                                        G7D2PGM 
00233      05  FILLER                   PIC X(16)  VALUE                G7D2PGM 
00234          '*G7D2PGM TABLES*'.                                      G7D2PGM 
00235                                                                   G7D2PGM 
00236  01  WT-01-TABLE.                                                 G7D2PGM 
00237      05  FILLER                  PIC X(16) VALUE                  G7D2PGM 
00238          '* WT-01-TABLE  *'.                                      G7D2PGM 
00239 ******************************************************************G7D2PGM 
00240 *    WT-01   MESSAGE TABLE                                       *G7D2PGM 
00241 ******************************************************************G7D2PGM 
00242  01  FILLER.                                                      G7D2PGM 
00243      05  WT-01-MESSAGE-VALUES.                                    G7D2PGM 
00244                                                                   G7D2PGM 
00245 *----------------------------------------------------------------*G7D2PGM 
00246          10  WT-01-ENTRY-001.                                     G7D2PGM 
00247              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00248              15  WT-01-MESSAGE-TEXT-001.                          G7D2PGM 
00249                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00250                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00251                  20  FILLER          PIC X(3)  VALUE  '001'.      G7D2PGM 
00252                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00253                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00254                      ' INVALID PFKEY SELECTION                    G7D2PGM 
00255 -                    '                         '.                 G7D2PGM 
00256              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00257                                                                   G7D2PGM 
00258 *----------------------------------------------------------------*G7D2PGM 
00259          10  WT-01-ENTRY-002.                                     G7D2PGM 
00260              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00261              15  WT-01-MESSAGE-TEXT-002.                          G7D2PGM 
00262                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00263                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00264                  20  FILLER          PIC X(3)  VALUE  '002'.      G7D2PGM 
00265                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00266                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00267                      'TREATMENT TIME FACTOR DAYS REQUIRED WHEN INDG7D2PGM 
00268 -                    'ICATOR IS CODED          '.                 G7D2PGM 
00269              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00270                                                                   G7D2PGM 
00271 *----------------------------------------------------------------*G7D2PGM 
00272          10  WT-01-ENTRY-003.                                     G7D2PGM 
00273              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00274              15  WT-01-MESSAGE-TEXT-003.                          G7D2PGM 
00275                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00276                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00277                  20  FILLER          PIC X(3)  VALUE  '003'.      G7D2PGM 
00278                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00279                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00280                      'INDICATOR REQUIRED WHEN TREATMENT TIME FACTOG7D2PGM 
00281 -                    'R DAYS IS CODED          '.                 G7D2PGM 
00282              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00283                                                                   G7D2PGM 
00284 *----------------------------------------------------------------*G7D2PGM 
00285          10  WT-01-ENTRY-004.                                     G7D2PGM 
00286              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00287              15  WT-01-MESSAGE-TEXT-004.                          G7D2PGM 
00288                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00289                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00290                  20  FILLER          PIC X(3)  VALUE  '004'.      G7D2PGM 
00291                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00292                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00293                      'BENEFIT MAXIMUM VISITS REQUIRED WHEN INDICATG7D2PGM 
00294 -                    'OR IS CODED              '.                 G7D2PGM 
00295              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00296                                                                   G7D2PGM 
00297 *----------------------------------------------------------------*G7D2PGM 
00298          10  WT-01-ENTRY-005.                                     G7D2PGM 
00299              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00300              15  WT-01-MESSAGE-TEXT-005.                          G7D2PGM 
00301                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00302                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00303                  20  FILLER          PIC X(3)  VALUE  '005'.      G7D2PGM 
00304                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00305                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00306                      'INDICATOR REQUIRED WHEN BENEFIT MAXIMUM VISIG7D2PGM 
00307 -                    'TS IS CODED              '.                 G7D2PGM 
00308              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00309                                                                   G7D2PGM 
00310 *----------------------------------------------------------------*G7D2PGM 
00311          10  WT-01-ENTRY-006.                                     G7D2PGM 
00312              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00313              15  WT-01-MESSAGE-TEXT-006.                          G7D2PGM 
00314                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00315                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00316                  20  FILLER          PIC X(3)  VALUE  '006'.      G7D2PGM 
00317                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00318                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00319                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7D2PGM 
00320 -                    ' PF4/PF16 TO CONTINUE    '.                 G7D2PGM 
00321              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00322                                                                   G7D2PGM 
00323 *----------------------------------------------------------------*G7D2PGM 
00324          10  WT-01-ENTRY-007.                                     G7D2PGM 
00325              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00326              15  WT-01-MESSAGE-TEXT-007.                          G7D2PGM 
00327                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00328                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00329                  20  FILLER          PIC X(3)  VALUE  '007'.      G7D2PGM 
00330                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00331                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00332                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7D2PGM 
00333 -                    'E CALL SYSTEMS           '.                 G7D2PGM 
00334              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00335                                                                   G7D2PGM 
00336 *----------------------------------------------------------------*G7D2PGM 
00337          10  WT-01-ENTRY-008.                                     G7D2PGM 
00338              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00339              15  WT-01-MESSAGE-TEXT-008.                          G7D2PGM 
00340                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00341                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00342                  20  FILLER          PIC X(3)  VALUE  '008'.      G7D2PGM 
00343                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00344                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00345                      'FIELD HAS AN INVALID VALUE                  G7D2PGM 
00346 -                    '                         '.                 G7D2PGM 
00347              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00348                                                                   G7D2PGM 
00349 *----------------------------------------------------------------*G7D2PGM 
00350          10  WT-01-ENTRY-009.                                     G7D2PGM 
00351              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00352              15  WT-01-MESSAGE-TEXT-009.                          G7D2PGM 
00353                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00354                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00355                  20  FILLER          PIC X(3)  VALUE  '009'.      G7D2PGM 
00356                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00357                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00358                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7D2PGM 
00359 -                    'YSTEM)                   '.                 G7D2PGM 
00360              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00361                                                                   G7D2PGM 
00362 *----------------------------------------------------------------*G7D2PGM 
00363          10  WT-01-ENTRY-010.                                     G7D2PGM 
00364              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00365              15  WT-01-MESSAGE-TEXT-010.                          G7D2PGM 
00366                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00367                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00368                  20  FILLER          PIC X(3)  VALUE  '010'.      G7D2PGM 
00369                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00370                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00371                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7D2PGM 
00372 -                    '                         '.                 G7D2PGM 
00373              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00374 *----------------------------------------------------------------*G7D2PGM 
00375          10  WT-01-ENTRY-011.                                     G7D2PGM 
00376              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00377              15  WT-01-MESSAGE-TEXT-009.                          G7D2PGM 
00378                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00379                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00380                  20  FILLER          PIC X(3)  VALUE  '011'.      G7D2PGM 
00381                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00382                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00383        'FIELD EXCEEDS LENGTH OF 7 POSITIONS   FORMAT IS 99999.99'.G7D2PGM 
00384              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00385                                                                   G7D2PGM 
00386 *----------------------------------------------------------------*G7D2PGM 
00387          10  WT-01-ENTRY-012.                                     G7D2PGM 
00388              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00389              15  WT-01-MESSAGE-TEXT-009.                          G7D2PGM 
00390                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00391                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00392                  20  FILLER          PIC X(3)  VALUE  '012'.      G7D2PGM 
00393                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00394                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00395       'FIELD EXCEEDS LENGTH OF 5 POSITIONS   FORMAT IS 999.99'.   G7D2PGM 
00396              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00397                                                                   G7D2PGM 
00398 *----------------------------------------------------------------*G7D2PGM 
00399          10  WT-01-ENTRY-013.                                     G7D2PGM 
00400              15  FILLER              PIC X(2)  VALUE '¬>'.        G7D2PGM 
00401              15  WT-01-MESSAGE-TEXT-010.                          G7D2PGM 
00402                  20  FILLER          PIC X(4)  VALUE  'G7D2'.     G7D2PGM 
00403                  20  FILLER          PIC X(1)  VALUE  '-'.        G7D2PGM 
00404                  20  FILLER          PIC X(3)  VALUE  '013'.      G7D2PGM 
00405                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7D2PGM 
00406                  20  FILLER          PIC X(70) VALUE              G7D2PGM 
00407                      ' INVALID DECIMAL DETECTED'.                 G7D2PGM 
00408              15  FILLER              PIC X(2)  VALUE '<¬'.        G7D2PGM 
00409 *----------------------------------------------------------------*G7D2PGM 
00410                                                                   G7D2PGM 
00411      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7D2PGM 
00412          WT-01-MESSAGE-VALUES         OCCURS 013 TIMES            G7D2PGM 
00413                                      INDEXED BY WT-01-INDEX.      G7D2PGM 
00414          10  WT-01-ENTRY.                                         G7D2PGM 
00415              15  FILLER              PIC X(02).                   G7D2PGM 
00416              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7D2PGM 
00417              15  FILLER              PIC X(02).                   G7D2PGM 
00418                                                                   G7D2PGM 
00419                                                                   G7D2PGM 
00420 /*** MAP FIELD ATTRIBUTES                                         G7D2PGM 
00421  COPY DFHBMSCA.                                                   G7D2PGM 
00422 *                         AUTOSKIP, BRIGHT, FSET                  G7D2PGM 
00423      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7D2PGM 
00424                                                                   G7D2PGM 
00425 /*** ATTENTION KEYS                                               G7D2PGM 
00426  COPY DFHAID.                                                     G7D2PGM 
00427                                                                   G7D2PGM 
00428 /***  PROVISION MAINTENANCE SCREEN                                G7D2PGM 
00429  COPY  G7D2SETC.                                                  G7D2PGM 
00430                                                                   G7D2PGM 
00431 /*** DATE ROUTINE COMMAREA                                        G7D2PGM 
00432  01  HGADATES-COMMAREA.                                           G7D2PGM 
00433  COPY HGCDAT01.                                                   G7D2PGM 
00434                                                                   G7D2PGM 
00435 /*** DECIMAL CONVERT COMMAREA                                     G7D2PGM 
00436  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7D2PGM 
00437  COPY GCDCCA01.                                                   G7D2PGM 
00438                                                                   G7D2PGM 
00439 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7D2PGM 
00440  01  GCVIOPGM-PARM-LIST.                                          G7D2PGM 
00441  COPY GCVINTRC.                                                   G7D2PGM 
00442                                                                   G7D2PGM 
00443 /*** ALTERNATIVE WORKFILE KEYS                                    G7D2PGM 
00444  01  FILLER.                                                      G7D2PGM 
00445      COPY GCWRKKEY.                                               G7D2PGM 
00446                                                                   G7D2PGM 
00447 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7D2PGM 
00448  01  FILLER.                                                      G7D2PGM 
00449      COPY GCCDRLEN.                                               G7D2PGM 
00450                                                                   G7D2PGM 
00451                                                                   G7D2PGM 
00452  01  WS-END                       PIC X(58) VALUE                 G7D2PGM 
00453      '*** G7D2PGM  WORKING-STORAGE ENDS HERE ***'.                G7D2PGM 
00454 /                                                                 G7D2PGM 
00455  LINKAGE SECTION.                                                 G7D2PGM 
00456 /                                                                 G7D2PGM 
00457  01  DFHCOMMAREA.                                                 G7D2PGM 
00458      COPY  GCWRKDCC.                                              G7D2PGM 
00459      COPY  GCBENPVC.                                              G7D2PGM 
00460 /                                                                 G7D2PGM 
00461 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7D2PGM 
00462  01  IO-PARM-BEN-PROV-AREA.                                       G7D2PGM 
00463      COPY  GCIOPRM2.                                              G7D2PGM 
00464      COPY  GCWRKDC2.                                              G7D2PGM 
00465      COPY  GCBENPV2.                                              G7D2PGM 
00466                                                                   G7D2PGM 
00467 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7D2PGM 
00468  01  IO-PARM-CONTRACT-AREA.                                       G7D2PGM 
00469      COPY  GCIOPRM3.                                              G7D2PGM 
00470      COPY  GCWRKDC3.                                              G7D2PGM 
00471      COPY  GCCONTR2.                                              G7D2PGM 
00472 /                                                                 G7D2PGM 
00473  PROCEDURE DIVISION.                                              G7D2PGM 
00474                                                                   G7D2PGM 
00475 ****************************************************************  G7D2PGM 
00476 *                                                              *  G7D2PGM 
00477 *           P R O C E S S     C O N T R O L                    *  G7D2PGM 
00478 *                                                              *  G7D2PGM 
00479 ****************************************************************  G7D2PGM 
00480  0000-000-PROCESS-CONTROL       SECTION.                          G7D2PGM 
00481  0000-010.                                                        G7D2PGM 
00482                                                                   G7D2PGM 
00483      IF  EIBAID  =  DFHCLEAR                                      G7D2PGM 
00484          EXEC CICS  RETURN                                        G7D2PGM 
00485                     END-EXEC.                                     G7D2PGM 
00486                                                                   G7D2PGM 
00487      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7D2PGM 
00488                                                                   G7D2PGM 
00489      IF  WS-02-MY-EIBTRNID                                        G7D2PGM 
00490      THEN                                                         G7D2PGM 
00491          PERFORM  2000-000-PROCESS-INPUT                          G7D2PGM 
00492      ELSE                                                         G7D2PGM 
00493          PERFORM  1000-000-DISPLAY-SCREEN.                        G7D2PGM 
00494                                                                   G7D2PGM 
00495                                                                   G7D2PGM 
00496 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7D2PGM 
00497                                                                   G7D2PGM 
00498      MOVE WS-01-ABCODE-D2L1     TO WS-01-ABCODE                   G7D2PGM 
00499      MOVE WS-01-ABCODE-D2L1-MSG TO WS-01-ABCODE-MSG               G7D2PGM 
00500      PERFORM  9999-000-ABEND-THE-TASK.                            G7D2PGM 
00501                                                                   G7D2PGM 
00502      GOBACK.                                                      G7D2PGM 
00503                                                                   G7D2PGM 
00504                                                                   G7D2PGM 
00505  0000-900-EXIT.                                                   G7D2PGM 
00506      EXIT.                                                        G7D2PGM 
00507 /***************************************************************  G7D2PGM 
00508 *                                                              *  G7D2PGM 
00509 * 1000  DISPLAY INITIAL SCREEN                                 *  G7D2PGM 
00510 *                                                              *  G7D2PGM 
00511 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7D2PGM 
00512 *                                                              *  G7D2PGM 
00513 ****************************************************************  G7D2PGM 
00514  1000-000-DISPLAY-SCREEN        SECTION.                          G7D2PGM 
00515  1000-010.                                                        G7D2PGM 
00516                                                                   G7D2PGM 
00517 *------- SET SCREEN TO LOW VALUES FOR FIRST DISPLAY               G7D2PGM 
00518 *                                                                 G7D2PGM 
00519      MOVE LOW-VALUES TO G7D2I01I.                                 G7D2PGM 
00520                                                                   G7D2PGM 
00521 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7D2PGM 
00522                                                                   G7D2PGM 
00523      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7D2PGM 
00524          MOVE WS-01-ABCODE-D2P1     TO WS-01-ABCODE               G7D2PGM 
00525          MOVE WS-01-ABCODE-D2P1-MSG TO WS-01-ABCODE-MSG           G7D2PGM 
00526          PERFORM 9999-000-ABEND-THE-TASK.                         G7D2PGM 
00527                                                                   G7D2PGM 
00528                                                                   G7D2PGM 
00529 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7D2PGM 
00530                                                                   G7D2PGM 
00531      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7D2PGM 
00532                                         + GC-GCBENPRV-FIXED-LEN   G7D2PGM 
00533                                         + GC-GCBENPRV-VARY-LEN.   G7D2PGM 
00534                                                                   G7D2PGM 
00535                                                                   G7D2PGM 
00536 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7D2PGM 
00537                                                                   G7D2PGM 
00538      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7D2PGM 
00539          MOVE WS-01-ABCODE-D2P2     TO WS-01-ABCODE               G7D2PGM 
00540          MOVE WS-01-ABCODE-D2P2-MSG TO WS-01-ABCODE-MSG           G7D2PGM 
00541          PERFORM 9999-000-ABEND-THE-TASK.                         G7D2PGM 
00542                                                                   G7D2PGM 
00543                                                                   G7D2PGM 
00544 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7D2PGM 
00545 *          BY CALLER IN COMMAREA.                                 G7D2PGM 
00546                                                                   G7D2PGM 
00547      MOVE WRK-PLAN-CODE                      TO S2PLNCDO.         G7D2PGM 
00548      MOVE WRK-GROUP-NUM                      TO S2GRPNOO.         G7D2PGM 
00549      MOVE WRK-SECTION-NUM                    TO S2SECNOO.         G7D2PGM 
00550      MOVE WRK-PKG-CODE                       TO S2PKGCDO.         G7D2PGM 
00551      MOVE WRK-PROV-CTL                       TO S2PRVO.           G7D2PGM 
00552      MOVE WRK-FAM-REL-LEVEL                  TO S2FRLO.           G7D2PGM 
00553      MOVE WRK-L-O-B                          TO S2LOBO.           G7D2PGM 
00554                                                                   G7D2PGM 
00555      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7D2PGM 
00556      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7D2PGM 
00557      IF  HGADATE-RETURN = ZEROS                                   G7D2PGM 
00558      THEN                                                         G7D2PGM 
00559          MOVE DFHBMASF                       TO S2EFFDTA          G7D2PGM 
00560          MOVE HGADATE-DATE2                  TO S2EFFDTO          G7D2PGM 
00561      ELSE                                                         G7D2PGM 
00562          MOVE DFHBMABF                       TO S2EFFDTA          G7D2PGM 
00563          MOVE HGADATE-JULIAN1                TO S2EFFDTO.         G7D2PGM 
00564                                                                   G7D2PGM 
00565      MOVE GCP-PROVN-ID                       TO S2BPVIDO.         G7D2PGM 
00566                                                                   G7D2PGM 
00567      MOVE GPD-BEN-MAX-VISIT-IND              TO S2BMVSIO.         G7D2PGM 
00568                                                                   G7D2PGM 
00569      IF  GPD-BEN-MAX-VISIT-DAYS = ZEROS                           G7D2PGM 
00570          MOVE WS-02-HEX-F00000               TO S2BMVSTO          G7D2PGM 
00571      ELSE                                                         G7D2PGM 
00572          MOVE   GPD-BEN-MAX-VISIT-DAYS       TO                   G7D2PGM 
00573               WS-02-BEN-MAX-VISIT-DAYS                            G7D2PGM 
00574          MOVE WS-02-BEN-MAX-VISIT-DAYS-X     TO S2BMVSTO.         G7D2PGM 
00575                                                                   G7D2PGM 
00576 *    IF  GPD-MAX-AMT-PER-VISIT = ZEROS                            G7D2PGM 
00577 *        MOVE WS-02-HEX-F00000               TO S2XAVO            G7D2PGM 
00578 *    ELSE                                                         G7D2PGM 
00579 *        MOVE   GPD-MAX-AMT-PER-VISIT        TO                   G7D2PGM 
00580 *             WS-02-MAX-AMT-PER-VISIT                             G7D2PGM 
00581 *        MOVE WS-02-MAX-AMT-PER-VISIT-X      TO S2XAVO.           G7D2PGM 
00582                                                                   G7D2PGM 
00583 *    D129 ADDED FOR CONVERSION.                                   G7D2PGM 
00584 *                                                                 G7D2PGM 
00585          MOVE   GPD-MAX-AMT-PER-VISIT        TO                   G7D2PGM 
00586               WS-02-MAX-AMT-PER-VISIT.                            G7D2PGM 
00587          MOVE WS-02-MAX-AMT-PER-VISIT        TO                   G7D2PGM 
00588               WS-02-DISP-5POS-DEC.                                G7D2PGM 
00589          MOVE WS-02-DISP-5POS-DEC            TO S2XAVO.           G7D2PGM 
00590                                                                   G7D2PGM 
00591 *    IF  GPD-FLAT-RATE-PDM-AMT = ZEROS                            G7D2PGM 
00592 *        MOVE WS-02-HEX-F00000               TO S2FLPDYO          G7D2PGM 
00593 *    ELSE                                                         G7D2PGM 
00594 *        MOVE   GPD-FLAT-RATE-PDM-AMT        TO                   G7D2PGM 
00595 *             WS-02-FLAT-RATE-PDM-AMT                             G7D2PGM 
00596 *        MOVE WS-02-FLAT-RATE-PDM-AMT-X      TO S2FLPDYO.         G7D2PGM 
00597                                                                   G7D2PGM 
00598 *    D129 ADDED FOR CONVERSION.                                   G7D2PGM 
00599 *                                                                 G7D2PGM 
00600          MOVE   GPD-FLAT-RATE-PDM-AMT        TO                   G7D2PGM 
00601               WS-02-FLAT-RATE-PDM-AMT.                            G7D2PGM 
00602          MOVE WS-02-FLAT-RATE-PDM-AMT        TO                   G7D2PGM 
00603               WS-02-DISP-7POS-DEC.                                G7D2PGM 
00604          MOVE WS-02-DISP-7POS-DEC            TO S2FLPDYO.         G7D2PGM 
00605                                                                   G7D2PGM 
00606      MOVE GPD-CERT-REQRMNT-IND               TO S2CRRIO.          G7D2PGM 
00607      MOVE GPD-TREAT-TIME-FACTOR-IND          TO S2TRTMIO.         G7D2PGM 
00608                                                                   G7D2PGM 
00609      IF  GPD-TREAT-TIME-FACTOR = ZEROS                            G7D2PGM 
00610          MOVE WS-02-HEX-F00000               TO S2TRTMFO          G7D2PGM 
00611      ELSE                                                         G7D2PGM 
00612          MOVE   GPD-TREAT-TIME-FACTOR        TO                   G7D2PGM 
00613               WS-02-TREAT-TIME-FACTOR                             G7D2PGM 
00614          MOVE WS-02-TREAT-TIME-FACTOR-X      TO S2TRTMFO.         G7D2PGM 
00615                                                                   G7D2PGM 
00616                                                                   G7D2PGM 
00617                                                                   G7D2PGM 
00618                                                                   G7D2PGM 
00619 *------- SEND INITIAL SCREEN ------------------------------------*G7D2PGM 
00620                                                                   G7D2PGM 
00621      MOVE  -1 TO  S2BMVSIL.                                       G7D2PGM 
00622      PERFORM 9100-000-SEND-THEN-RETURN.                           G7D2PGM 
00623                                                                   G7D2PGM 
00624                                                                   G7D2PGM 
00625  1000-900-EXIT.                                                   G7D2PGM 
00626      EXIT.                                                        G7D2PGM 
00627 /***************************************************************  G7D2PGM 
00628 *                                                              *  G7D2PGM 
00629 * 2000    P R O C E S S    I N P U T                           *  G7D2PGM 
00630 *                                                              *  G7D2PGM 
00631 ****************************************************************  G7D2PGM 
00632  2000-000-PROCESS-INPUT         SECTION.                          G7D2PGM 
00633  2000-010.                                                        G7D2PGM 
00634                                                                   G7D2PGM 
00635 *------ VALIDATE PFKEY USAGE ------------------------------------*G7D2PGM 
00636                                                                   G7D2PGM 
00637      IF  EIBAID = DFHENTER OR                                     G7D2PGM 
00638                   DFHPF3   OR  DFHPF15 OR                         G7D2PGM 
00639                   DFHPF4   OR  DFHPF16 OR                         G7D2PGM 
00640                   DFHPF6   OR  DFHPF18 OR                         G7D2PGM 
00641                   DFHPF7   OR  DFHPF19 OR                         G7D2PGM 
00642                   DFHPF8   OR  DFHPF20                            G7D2PGM 
00643      THEN                                                         G7D2PGM 
00644          NEXT SENTENCE                                            G7D2PGM 
00645      ELSE                                                         G7D2PGM 
00646          SET WT-01-INDEX TO +01                                   G7D2PGM 
00647          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D2PGM 
00648          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D2PGM 
00649                                                                   G7D2PGM 
00650                                                                   G7D2PGM 
00651                                                                   G7D2PGM 
00652      EXEC CICS  HANDLE CONDITION                                  G7D2PGM 
00653                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7D2PGM 
00654                        END-EXEC.                                  G7D2PGM 
00655                                                                   G7D2PGM 
00656                                                                   G7D2PGM 
00657      EXEC CICS  RECEIVE MAP   ('G7D2I01')                         G7D2PGM 
00658                         MAPSET('G7D2SET')                         G7D2PGM 
00659                         END-EXEC.                                 G7D2PGM 
00660                                                                   G7D2PGM 
00661                                                                   G7D2PGM 
00662      IF  S2FUNCI  NOT = 'G7D2'  OR                                G7D2PGM 
00663          S2SCRNI  NOT = '007D02'                                  G7D2PGM 
00664          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7D2PGM 
00665                                                                   G7D2PGM 
00666                                                                   G7D2PGM 
00667 *--- RETURN TO GCPS MENU? ---------------------------------------*G7D2PGM 
00668                                                                   G7D2PGM 
00669      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7D2PGM 
00670          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7D2PGM 
00671                                                                   G7D2PGM 
00672 *--- PROCESS SCREEN FIELDS --------------------------------------*G7D2PGM 
00673                                                                   G7D2PGM 
00674      PERFORM 2100-000-FIELD-EDITS.                                G7D2PGM 
00675                                                                   G7D2PGM 
00676      IF  WS-02-SCREEN-HAS-ERRORS                                  G7D2PGM 
00677          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D2PGM 
00678                                                                   G7D2PGM 
00679      PERFORM 2200-000-LOGICAL-EDITS.                              G7D2PGM 
00680                                                                   G7D2PGM 
00681      IF  WS-02-SCREEN-HAS-ERRORS                                  G7D2PGM 
00682          PERFORM 9100-000-SEND-THEN-RETURN.                       G7D2PGM 
00683                                                                   G7D2PGM 
00684      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7D2PGM 
00685                                                                   G7D2PGM 
00686      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7D2PGM 
00687                                                                   G7D2PGM 
00688                                                                   G7D2PGM 
00689  2000-900-EXIT.                                                   G7D2PGM 
00690      EXIT.                                                        G7D2PGM 
00691 /***************************************************************  G7D2PGM 
00692 *                                                              *  G7D2PGM 
00693 * 2100  DO SCREEN FIELD EDITS                                  *  G7D2PGM 
00694 *                                                              *  G7D2PGM 
00695 ****************************************************************  G7D2PGM 
00696  2100-000-FIELD-EDITS           SECTION.                          G7D2PGM 
00697  2100-010.                                                        G7D2PGM 
00698                                                                   G7D2PGM 
00699 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7D2PGM 
00700                                                                   G7D2PGM 
00701      MOVE DFHBMUNF TO  S2BMVSIA                                   G7D2PGM 
00702                        S2BMVSTA                                   G7D2PGM 
00703                        S2XAVA                                     G7D2PGM 
00704                        S2FLPDYA                                   G7D2PGM 
00705                        S2CRRIA                                    G7D2PGM 
00706                        S2TRTMIA                                   G7D2PGM 
00707                        S2TRTMFA.                                  G7D2PGM 
00708                                                                   G7D2PGM 
00709      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7D2PGM 
00710                                                                   G7D2PGM 
00711                                                                   G7D2PGM 
00712 *-- VALIDATE ------ BENEFIT MAXIMUM VISITS IND ------------------*G7D2PGM 
00713 *   1. ALPHANUMERIC                                               G7D2PGM 
00714 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D2PGM 
00715                                                                   G7D2PGM 
00716      MOVE  S2BMVSII TO WS-02-CLASS-TEST-AREA.                     G7D2PGM 
00717      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7D2PGM 
00718          WS-02-CLASS-ALPHANUMERIC(2)                              G7D2PGM 
00719      THEN                                                         G7D2PGM 
00720          MOVE  S2BMVSII TO GCVI-VALUE                             G7D2PGM 
00721          MOVE  'BPDB01' TO GCVI-FIELDS-KEY-ID                     G7D2PGM 
00722          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D2PGM 
00723          IF  GCVI-VALUE-NOT-FOUND                                 G7D2PGM 
00724          THEN                                                     G7D2PGM 
00725              MOVE  -1        TO  S2BMVSIL                         G7D2PGM 
00726              MOVE  DFHBMUBF  TO  S2BMVSIA                         G7D2PGM 
00727              IF  WS-02-SCREEN-HAS-ERRORS                          G7D2PGM 
00728              THEN                                                 G7D2PGM 
00729                  NEXT SENTENCE                                    G7D2PGM 
00730              ELSE                                                 G7D2PGM 
00731                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D2PGM 
00732                  SET WT-01-INDEX TO +09                           G7D2PGM 
00733                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00734          ELSE                                                     G7D2PGM 
00735              IF  GCVI-VALUE-NOT-LOADED                            G7D2PGM 
00736              THEN                                                 G7D2PGM 
00737                  MOVE  DFHBMUBF  TO  S2BMVSIA                     G7D2PGM 
00738              ELSE                                                 G7D2PGM 
00739                  NEXT SENTENCE                                    G7D2PGM 
00740      ELSE                                                         G7D2PGM 
00741          MOVE  -1        TO  S2BMVSIL                             G7D2PGM 
00742          MOVE  DFHBMUBF  TO  S2BMVSIA                             G7D2PGM 
00743          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00744          THEN                                                     G7D2PGM 
00745              NEXT SENTENCE                                        G7D2PGM 
00746          ELSE                                                     G7D2PGM 
00747              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00748              SET WT-01-INDEX TO +08                               G7D2PGM 
00749              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00750                                                                   G7D2PGM 
00751                                                                   G7D2PGM 
00752 *-- VALIDATE ------ BENEFIT MAXIMUM VISITS ----------------------*G7D2PGM 
00753 *   1. NUMERICS                                                   G7D2PGM 
00754                                                                   G7D2PGM 
00755      IF  S2BMVSTI IS NUMERIC                                      G7D2PGM 
00756      THEN                                                         G7D2PGM 
00757          NEXT SENTENCE                                            G7D2PGM 
00758      ELSE                                                         G7D2PGM 
00759          MOVE  -1        TO  S2BMVSTL                             G7D2PGM 
00760          MOVE  DFHBMUBF  TO  S2BMVSTA                             G7D2PGM 
00761          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00762          THEN                                                     G7D2PGM 
00763              NEXT SENTENCE                                        G7D2PGM 
00764          ELSE                                                     G7D2PGM 
00765              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00766              SET WT-01-INDEX TO +10                               G7D2PGM 
00767              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00768                                                                   G7D2PGM 
00769                                                                   G7D2PGM 
00770 *-- VALIDATE ------ MAXIMUM AMOUNT PER VISIT --------------------*G7D2PGM 
00771 *   D129 DECIMAL CONVERT                                          G7D2PGM 
00772                                                                   G7D2PGM 
00773      MOVE S2XAVI TO D-C-RECEIVE-FIELD.                            G7D2PGM 
00774      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7D2PGM 
00775      MOVE '00' TO D-C-RETURN-CODE.                                G7D2PGM 
00776      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7D2PGM 
00777      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7D2PGM 
00778      IF D-C-RETURN-CODE = '00'                                    G7D2PGM 
00779          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7D2PGM 
00780              MOVE -1       TO S2XAVL                              G7D2PGM 
00781              MOVE DFHBMUBF TO S2XAVA                              G7D2PGM 
00782              IF WS-02-SCREEN-HAS-ERRORS                           G7D2PGM 
00783                  NEXT SENTENCE                                    G7D2PGM 
00784              ELSE                                                 G7D2PGM 
00785                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7D2PGM 
00786                  SET WT-01-INDEX TO +12                           G7D2PGM 
00787                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00788          ELSE                                                     G7D2PGM 
00789              MOVE D-C-RETURN-FIELD-DEC2                           G7D2PGM 
00790                TO WS-02-MAX-AMT-PER-VISIT                         G7D2PGM 
00791              MOVE WS-02-MAX-AMT-PER-VISIT                         G7D2PGM 
00792                TO WS-02-DISP-5POS-DEC                             G7D2PGM 
00793              MOVE WS-02-DISP-5POS-DEC                             G7D2PGM 
00794                TO S2XAVO                                          G7D2PGM 
00795      ELSE                                                         G7D2PGM 
00796          MOVE -1       TO S2XAVL                                  G7D2PGM 
00797          MOVE DFHBMUBF TO S2XAVA                                  G7D2PGM 
00798          IF WS-02-SCREEN-HAS-ERRORS                               G7D2PGM 
00799              NEXT SENTENCE                                        G7D2PGM 
00800          ELSE                                                     G7D2PGM 
00801              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7D2PGM 
00802              IF D-C-RETURN-CODE = '10'                            G7D2PGM 
00803                  SET WT-01-INDEX TO +10                           G7D2PGM 
00804                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00805              ELSE                                                 G7D2PGM 
00806                  SET WT-01-INDEX TO +13                           G7D2PGM 
00807                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7D2PGM 
00808                                                                   G7D2PGM 
00809 *    IF  S2XAVI IS NUMERIC                                        G7D2PGM 
00810 *    THEN                                                         G7D2PGM 
00811 *        NEXT SENTENCE                                            G7D2PGM 
00812 *    ELSE                                                         G7D2PGM 
00813 *        MOVE  -1        TO  S2XAVL                               G7D2PGM 
00814 *        MOVE  DFHBMUBF  TO  S2XAVA                               G7D2PGM 
00815 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00816 *        THEN                                                     G7D2PGM 
00817 *            NEXT SENTENCE                                        G7D2PGM 
00818 *        ELSE                                                     G7D2PGM 
00819 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00820 *            SET WT-01-INDEX TO +10                               G7D2PGM 
00821 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00822                                                                   G7D2PGM 
00823                                                                   G7D2PGM 
00824 *-- VALIDATE ------ FLAT RATE PER-DIEM AMOUNT -------------------*G7D2PGM 
00825 *   D129 DECIMAL CONVERT                                          G7D2PGM 
00826                                                                   G7D2PGM 
00827      MOVE S2FLPDYI TO D-C-RECEIVE-FIELD.                          G7D2PGM 
00828      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7D2PGM 
00829      MOVE '00' TO D-C-RETURN-CODE.                                G7D2PGM 
00830      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7D2PGM 
00831      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7D2PGM 
00832      IF D-C-RETURN-CODE = '00'                                    G7D2PGM 
00833          IF D-C-RETURN-FIELD-DEC2 > WS-7POS-MAX-AMT               G7D2PGM 
00834              MOVE -1       TO S2FLPDYL                            G7D2PGM 
00835              MOVE DFHBMUBF TO S2FLPDYA                            G7D2PGM 
00836              IF WS-02-SCREEN-HAS-ERRORS                           G7D2PGM 
00837                  NEXT SENTENCE                                    G7D2PGM 
00838              ELSE                                                 G7D2PGM 
00839                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7D2PGM 
00840                  SET WT-01-INDEX TO +11                           G7D2PGM 
00841                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00842          ELSE                                                     G7D2PGM 
00843              MOVE D-C-RETURN-FIELD-DEC2                           G7D2PGM 
00844                TO WS-02-FLAT-RATE-PDM-AMT                         G7D2PGM 
00845              MOVE WS-02-FLAT-RATE-PDM-AMT                         G7D2PGM 
00846                TO WS-02-DISP-7POS-DEC                             G7D2PGM 
00847              MOVE WS-02-DISP-7POS-DEC                             G7D2PGM 
00848                TO S2FLPDYO                                        G7D2PGM 
00849      ELSE                                                         G7D2PGM 
00850          MOVE -1       TO S2FLPDYL                                G7D2PGM 
00851          MOVE DFHBMUBF TO S2FLPDYA                                G7D2PGM 
00852          IF WS-02-SCREEN-HAS-ERRORS                               G7D2PGM 
00853              NEXT SENTENCE                                        G7D2PGM 
00854          ELSE                                                     G7D2PGM 
00855              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7D2PGM 
00856              IF D-C-RETURN-CODE = '10'                            G7D2PGM 
00857                  SET WT-01-INDEX TO +10                           G7D2PGM 
00858                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00859              ELSE                                                 G7D2PGM 
00860                  SET WT-01-INDEX TO +13                           G7D2PGM 
00861                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7D2PGM 
00862                                                                   G7D2PGM 
00863                                                                   G7D2PGM 
00864 *    IF  S2FLPDYI IS NUMERIC                                      G7D2PGM 
00865 *    THEN                                                         G7D2PGM 
00866 *        NEXT SENTENCE                                            G7D2PGM 
00867 *    ELSE                                                         G7D2PGM 
00868 *        MOVE  -1        TO  S2FLPDYL                             G7D2PGM 
00869 *        MOVE  DFHBMUBF  TO  S2FLPDYA                             G7D2PGM 
00870 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00871 *        THEN                                                     G7D2PGM 
00872 *            NEXT SENTENCE                                        G7D2PGM 
00873 *        ELSE                                                     G7D2PGM 
00874 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00875 *            SET WT-01-INDEX TO +10                               G7D2PGM 
00876 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00877                                                                   G7D2PGM 
00878                                                                   G7D2PGM 
00879 *-- VALIDATE ------ CERTIFICATION REPETITION REQUIREMENT IND ----*G7D2PGM 
00880 *   1. ALPHANUMERIC                                               G7D2PGM 
00881 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D2PGM 
00882                                                                   G7D2PGM 
00883      MOVE  S2CRRII TO WS-02-CLASS-TEST-AREA.                      G7D2PGM 
00884      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D2PGM 
00885      THEN                                                         G7D2PGM 
00886          MOVE  S2CRRII TO GCVI-VALUE                              G7D2PGM 
00887          MOVE  'BPAB02' TO GCVI-FIELDS-KEY-ID                     G7D2PGM 
00888          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D2PGM 
00889          IF  GCVI-VALUE-NOT-FOUND                                 G7D2PGM 
00890          THEN                                                     G7D2PGM 
00891              MOVE  -1        TO  S2CRRIL                          G7D2PGM 
00892              MOVE  DFHBMUBF  TO  S2CRRIA                          G7D2PGM 
00893              IF  WS-02-SCREEN-HAS-ERRORS                          G7D2PGM 
00894              THEN                                                 G7D2PGM 
00895                  NEXT SENTENCE                                    G7D2PGM 
00896              ELSE                                                 G7D2PGM 
00897                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D2PGM 
00898                  SET WT-01-INDEX TO +09                           G7D2PGM 
00899                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00900          ELSE                                                     G7D2PGM 
00901              IF  GCVI-VALUE-NOT-LOADED                            G7D2PGM 
00902              THEN                                                 G7D2PGM 
00903                  MOVE  DFHBMUBF  TO  S2CRRIA                      G7D2PGM 
00904              ELSE                                                 G7D2PGM 
00905                  NEXT SENTENCE                                    G7D2PGM 
00906      ELSE                                                         G7D2PGM 
00907          MOVE  -1        TO  S2CRRIL                              G7D2PGM 
00908          MOVE  DFHBMUBF  TO  S2CRRIA                              G7D2PGM 
00909          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00910          THEN                                                     G7D2PGM 
00911              NEXT SENTENCE                                        G7D2PGM 
00912          ELSE                                                     G7D2PGM 
00913              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00914              SET WT-01-INDEX TO +08                               G7D2PGM 
00915              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00916                                                                   G7D2PGM 
00917                                                                   G7D2PGM 
00918 *-- VALIDATE ------ TREATMENT TIME FACTOR IND -------------------*G7D2PGM 
00919 *   1. ALPHANUMERIC                                               G7D2PGM 
00920 *   2. FIELD VALIDATION SUB-SYSTEM                                G7D2PGM 
00921                                                                   G7D2PGM 
00922      MOVE  S2TRTMII TO WS-02-CLASS-TEST-AREA.                     G7D2PGM 
00923      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7D2PGM 
00924      THEN                                                         G7D2PGM 
00925          MOVE  S2TRTMII TO GCVI-VALUE                             G7D2PGM 
00926          MOVE  'BPBA07' TO GCVI-FIELDS-KEY-ID                     G7D2PGM 
00927          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7D2PGM 
00928          IF  GCVI-VALUE-NOT-FOUND                                 G7D2PGM 
00929          THEN                                                     G7D2PGM 
00930              MOVE  -1        TO  S2TRTMIL                         G7D2PGM 
00931              MOVE  DFHBMUBF  TO  S2TRTMIA                         G7D2PGM 
00932              IF  WS-02-SCREEN-HAS-ERRORS                          G7D2PGM 
00933              THEN                                                 G7D2PGM 
00934                  NEXT SENTENCE                                    G7D2PGM 
00935              ELSE                                                 G7D2PGM 
00936                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D2PGM 
00937                  SET WT-01-INDEX TO +09                           G7D2PGM 
00938                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
00939          ELSE                                                     G7D2PGM 
00940              IF  GCVI-VALUE-NOT-LOADED                            G7D2PGM 
00941              THEN                                                 G7D2PGM 
00942                  MOVE  DFHBMUBF  TO  S2TRTMIA                     G7D2PGM 
00943              ELSE                                                 G7D2PGM 
00944                  NEXT SENTENCE                                    G7D2PGM 
00945      ELSE                                                         G7D2PGM 
00946          MOVE  -1        TO  S2TRTMIL                             G7D2PGM 
00947          MOVE  DFHBMUBF  TO  S2TRTMIA                             G7D2PGM 
00948          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00949          THEN                                                     G7D2PGM 
00950              NEXT SENTENCE                                        G7D2PGM 
00951          ELSE                                                     G7D2PGM 
00952              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00953              SET WT-01-INDEX TO +08                               G7D2PGM 
00954              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00955                                                                   G7D2PGM 
00956                                                                   G7D2PGM 
00957 *-- VALIDATE ------ TREATMENT TIME FACTOR -----------------------*G7D2PGM 
00958 *   1. NUMERICS                                                   G7D2PGM 
00959                                                                   G7D2PGM 
00960      IF  S2TRTMFI IS NUMERIC                                      G7D2PGM 
00961      THEN                                                         G7D2PGM 
00962          NEXT SENTENCE                                            G7D2PGM 
00963      ELSE                                                         G7D2PGM 
00964          MOVE  -1        TO  S2TRTMFL                             G7D2PGM 
00965          MOVE  DFHBMUBF  TO  S2TRTMFA                             G7D2PGM 
00966          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
00967          THEN                                                     G7D2PGM 
00968              NEXT SENTENCE                                        G7D2PGM 
00969          ELSE                                                     G7D2PGM 
00970              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
00971              SET WT-01-INDEX TO +10                               G7D2PGM 
00972              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7D2PGM 
00973                                                                   G7D2PGM 
00974                                                                   G7D2PGM 
00975  2100-900-EXIT.                                                   G7D2PGM 
00976      EXIT.                                                        G7D2PGM 
00977 /***************************************************************  G7D2PGM 
00978 *                                                              *  G7D2PGM 
00979 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7D2PGM 
00980 *                                                              *  G7D2PGM 
00981 ****************************************************************  G7D2PGM 
00982  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7D2PGM 
00983  2110-010.                                                        G7D2PGM 
00984                                                                   G7D2PGM 
00985      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7D2PGM 
00986                                                                   G7D2PGM 
00987      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7D2PGM 
00988                       COMMAREA(GCVIOPGM-PARM-LIST)                G7D2PGM 
00989                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7D2PGM 
00990                       END-EXEC.                                   G7D2PGM 
00991                                                                   G7D2PGM 
00992      IF  GCVI-VALUE-NOT-LOADED                                    G7D2PGM 
00993          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7D2PGM 
00994                                                                   G7D2PGM 
00995  2110-900-EXIT.                                                   G7D2PGM 
00996      EXIT.                                                        G7D2PGM 
00997 /***************************************************************  G7D2PGM 
00998 *                                                              *  G7D2PGM 
00999 * 2200  DO SCREEN LOGICAL EDITS                                *  G7D2PGM 
01000 *                                                              *  G7D2PGM 
01001 ****************************************************************  G7D2PGM 
01002  2200-000-LOGICAL-EDITS         SECTION.                          G7D2PGM 
01003  2200-010.                                                        G7D2PGM 
01004                                                                   G7D2PGM 
01005                                                                   G7D2PGM 
01006 *----------------------------------------------------------------*G7D2PGM 
01007 *                                                                *G7D2PGM 
01008 *  IF   BENEFIT MAXIMUM VISITS IND  (S2BMVSI) > ZERO             *G7D2PGM 
01009 *                                                                *G7D2PGM 
01010 *  THEN BENEFIT MAXIMUM VISITS      (S2BMVST) MUST BE > ZERO     *G7D2PGM 
01011 *                                                                *G7D2PGM 
01012 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7D2PGM 
01013 *                                                                *G7D2PGM 
01014 *----------------------------------------------------------------*G7D2PGM 
01015                                                                   G7D2PGM 
01016      IF  S2BMVSII     > ZEROS                                     G7D2PGM 
01017          AND                                                      G7D2PGM 
01018          S2BMVSTI NOT > ZEROS                                     G7D2PGM 
01019      THEN                                                         G7D2PGM 
01020          MOVE  -1        TO  S2BMVSTL                             G7D2PGM 
01021          MOVE  DFHBMUBF  TO  S2BMVSTA                             G7D2PGM 
01022                              S2BMVSIA                             G7D2PGM 
01023          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
01024          THEN                                                     G7D2PGM 
01025              NEXT SENTENCE                                        G7D2PGM 
01026          ELSE                                                     G7D2PGM 
01027              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
01028              SET WT-01-INDEX TO +04                               G7D2PGM 
01029              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D2PGM 
01030      ELSE                                                         G7D2PGM 
01031          NEXT SENTENCE.                                           G7D2PGM 
01032                                                                   G7D2PGM 
01033      IF  S2BMVSTI     > ZEROS                                     G7D2PGM 
01034          AND                                                      G7D2PGM 
01035          S2BMVSII NOT > ZEROS                                     G7D2PGM 
01036      THEN                                                         G7D2PGM 
01037          MOVE  -1        TO  S2BMVSIL                             G7D2PGM 
01038          MOVE  DFHBMUBF  TO  S2BMVSIA                             G7D2PGM 
01039                              S2BMVSTA                             G7D2PGM 
01040          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
01041          THEN                                                     G7D2PGM 
01042              NEXT SENTENCE                                        G7D2PGM 
01043          ELSE                                                     G7D2PGM 
01044              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
01045              SET WT-01-INDEX TO +05                               G7D2PGM 
01046              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D2PGM 
01047      ELSE                                                         G7D2PGM 
01048          NEXT SENTENCE.                                           G7D2PGM 
01049                                                                   G7D2PGM 
01050                                                                   G7D2PGM 
01051 *----------------------------------------------------------------*G7D2PGM 
01052 *                                                                *G7D2PGM 
01053 *  IF   TREATMENT TIME IND  (S2TRTMI) > ZERO                     *G7D2PGM 
01054 *                                                                *G7D2PGM 
01055 *  THEN TREATMENT TIME FACTOR (S2TRTMF) MUST BE > ZERO           *G7D2PGM 
01056 *                                                                *G7D2PGM 
01057 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7D2PGM 
01058 *                                                                *G7D2PGM 
01059 *----------------------------------------------------------------*G7D2PGM 
01060                                                                   G7D2PGM 
01061      IF  S2TRTMII     > ZEROS                                     G7D2PGM 
01062          AND                                                      G7D2PGM 
01063          S2TRTMFI NOT > ZEROS                                     G7D2PGM 
01064      THEN                                                         G7D2PGM 
01065          MOVE  -1        TO  S2TRTMFL                             G7D2PGM 
01066          MOVE  DFHBMUBF  TO  S2TRTMFA                             G7D2PGM 
01067                              S2TRTMIA                             G7D2PGM 
01068          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
01069          THEN                                                     G7D2PGM 
01070              NEXT SENTENCE                                        G7D2PGM 
01071          ELSE                                                     G7D2PGM 
01072              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
01073              SET WT-01-INDEX TO +02                               G7D2PGM 
01074              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D2PGM 
01075      ELSE                                                         G7D2PGM 
01076          NEXT SENTENCE.                                           G7D2PGM 
01077                                                                   G7D2PGM 
01078      IF  S2TRTMFI     > ZEROS                                     G7D2PGM 
01079          AND                                                      G7D2PGM 
01080          S2TRTMII NOT > ZEROS                                     G7D2PGM 
01081      THEN                                                         G7D2PGM 
01082          MOVE  -1        TO  S2TRTMIL                             G7D2PGM 
01083          MOVE  DFHBMUBF  TO  S2TRTMIA                             G7D2PGM 
01084                              S2TRTMFA                             G7D2PGM 
01085          IF  WS-02-SCREEN-HAS-ERRORS                              G7D2PGM 
01086          THEN                                                     G7D2PGM 
01087              NEXT SENTENCE                                        G7D2PGM 
01088          ELSE                                                     G7D2PGM 
01089              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7D2PGM 
01090              SET WT-01-INDEX TO +03                               G7D2PGM 
01091              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7D2PGM 
01092      ELSE                                                         G7D2PGM 
01093          NEXT SENTENCE.                                           G7D2PGM 
01094                                                                   G7D2PGM 
01095                                                                   G7D2PGM 
01096                                                                   G7D2PGM 
01097 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7D2PGM 
01098                                                                   G7D2PGM 
01099      IF  WS-02-SCREEN-HAS-ERRORS                                  G7D2PGM 
01100      THEN                                                         G7D2PGM 
01101          NEXT SENTENCE                                            G7D2PGM 
01102      ELSE                                                         G7D2PGM 
01103          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7D2PGM 
01104          THEN                                                     G7D2PGM 
01105              IF EIBAID = DFHPF4 OR DFHPF16                        G7D2PGM 
01106              THEN                                                 G7D2PGM 
01107                  NEXT SENTENCE                                    G7D2PGM 
01108              ELSE                                                 G7D2PGM 
01109                  MOVE  -1        TO S2ERRL                        G7D2PGM 
01110                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7D2PGM 
01111                  SET WT-01-INDEX TO +06                           G7D2PGM 
01112                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7D2PGM 
01113          ELSE                                                     G7D2PGM 
01114              NEXT SENTENCE.                                       G7D2PGM 
01115                                                                   G7D2PGM 
01116                                                                   G7D2PGM 
01117                                                                   G7D2PGM 
01118                                                                   G7D2PGM 
01119  2200-900-EXIT.                                                   G7D2PGM 
01120      EXIT.                                                        G7D2PGM 
01121 /***************************************************************  G7D2PGM 
01122 *                                                              *  G7D2PGM 
01123 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7D2PGM 
01124 *        REWRITE TO WORKFILE.                                  *  G7D2PGM 
01125 *                                                              *  G7D2PGM 
01126 ****************************************************************  G7D2PGM 
01127  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7D2PGM 
01128  2300-010.                                                        G7D2PGM 
01129                                                                   G7D2PGM 
01130 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7D2PGM 
01131                                                                   G7D2PGM 
01132      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7D2PGM 
01133      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7D2PGM 
01134        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7D2PGM 
01135      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7D2PGM 
01136      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7D2PGM 
01137      IF  NOT GCIO2-GOOD-RETURN                                    G7D2PGM 
01138          MOVE WS-01-ABCODE-D2F2     TO WS-01-ABCODE               G7D2PGM 
01139          MOVE WS-01-ABCODE-D2F2-MSG TO WS-01-ABCODE-MSG           G7D2PGM 
01140          PERFORM  9999-000-ABEND-THE-TASK.                        G7D2PGM 
01141                                                                   G7D2PGM 
01142                                                                   G7D2PGM 
01143 *----- SAVE SCREEN FIELDS THAT CANNOT BE COMPARED DIRECTLY ------*G7D2PGM 
01144 *           WITH FIELDS IN BENEFIT RECORD.                        G7D2PGM 
01145                                                                   G7D2PGM 
01146                                                                   G7D2PGM 
01147      MOVE S2BMVSTI   TO WS-02-BEN-MAX-VISIT-DAYS-X.               G7D2PGM 
01148 *    MOVE S2XAVI     TO WS-02-MAX-AMT-PER-VISIT-X.                G7D2PGM 
01149 *    MOVE S2FLPDYI   TO WS-02-FLAT-RATE-PDM-AMT.                  G7D2PGM 
01150      MOVE S2TRTMFI   TO WS-02-TREAT-TIME-FACTOR-X.                G7D2PGM 
01151                                                                   G7D2PGM 
01152                                                                   G7D2PGM 
01153                                                                   G7D2PGM 
01154 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7D2PGM 
01155                                                                   G7D2PGM 
01156      MOVE GPD2-MAX-AMT-PER-VISIT                                  G7D2PGM 
01157                   TO WS-GPD2-MAX-AMT-PER-VISIT.                   G7D2PGM 
01158                                                                   G7D2PGM 
01159      MOVE GPD2-FLAT-RATE-PDM-AMT                                  G7D2PGM 
01160                TO WS-GPD2-FLAT-RATE-PDM-AMT.                      G7D2PGM 
01161                                                                   G7D2PGM 
01162      IF      S2BMVSII                 = GPD2-BEN-MAX-VISIT-IND    G7D2PGM 
01163          AND WS-02-BEN-MAX-VISIT-DAYS = GPD2-BEN-MAX-VISIT-DAYS   G7D2PGM 
01164          AND WS-02-MAX-AMT-PER-VISIT  = WS-GPD2-MAX-AMT-PER-VISIT G7D2PGM 
01165          AND WS-02-FLAT-RATE-PDM-AMT  = WS-GPD2-FLAT-RATE-PDM-AMT G7D2PGM 
01166          AND S2CRRII                  = GPD2-CERT-REQRMNT-IND     G7D2PGM 
01167          AND S2TRTMII                 = GPD2-TREAT-TIME-FACTOR-INDG7D2PGM 
01168          AND WS-02-TREAT-TIME-FACTOR  = GPD2-TREAT-TIME-FACTOR    G7D2PGM 
01169      THEN                                                         G7D2PGM 
01170          GO TO 2300-900-EXIT                                      G7D2PGM 
01171      ELSE                                                         G7D2PGM 
01172          NEXT SENTENCE.                                           G7D2PGM 
01173                                                                   G7D2PGM 
01174                                                                   G7D2PGM 
01175 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7D2PGM 
01176                                                                   G7D2PGM 
01177      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7D2PGM 
01178      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7D2PGM 
01179      IF  NOT GCIO2-GOOD-RETURN                                    G7D2PGM 
01180          MOVE WS-01-ABCODE-D2F3     TO WS-01-ABCODE               G7D2PGM 
01181          MOVE WS-01-ABCODE-D2F3-MSG TO WS-01-ABCODE-MSG           G7D2PGM 
01182          PERFORM  9999-000-ABEND-THE-TASK.                        G7D2PGM 
01183                                                                   G7D2PGM 
01184                                                                   G7D2PGM 
01185 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7D2PGM 
01186                                                                   G7D2PGM 
01187      MOVE S2BMVSII                 TO GPD2-BEN-MAX-VISIT-IND.     G7D2PGM 
01188      MOVE WS-02-BEN-MAX-VISIT-DAYS TO GPD2-BEN-MAX-VISIT-DAYS.    G7D2PGM 
01189      MOVE WS-02-MAX-AMT-PER-VISIT  TO GPD2-MAX-AMT-PER-VISIT.     G7D2PGM 
01190      MOVE WS-02-FLAT-RATE-PDM-AMT  TO GPD2-FLAT-RATE-PDM-AMT.     G7D2PGM 
01191      MOVE S2CRRII                  TO GPD2-CERT-REQRMNT-IND.      G7D2PGM 
01192      MOVE S2TRTMII                 TO GPD2-TREAT-TIME-FACTOR-IND. G7D2PGM 
01193      MOVE WS-02-TREAT-TIME-FACTOR  TO GPD2-TREAT-TIME-FACTOR.     G7D2PGM 
01194                                                                   G7D2PGM 
01195                                                                   G7D2PGM 
01196 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7D2PGM 
01197                                                                   G7D2PGM 
01198 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7D2PGM 
01199                                                                   G7D2PGM 
01200      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7D2PGM 
01201      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7D2PGM 
01202      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7D2PGM 
01203      IF  NOT GCIO2-GOOD-RETURN                                    G7D2PGM 
01204          MOVE WS-01-ABCODE-D2F4     TO WS-01-ABCODE               G7D2PGM 
01205          MOVE WS-01-ABCODE-D2F4-MSG TO WS-01-ABCODE-MSG           G7D2PGM 
01206          PERFORM  9999-000-ABEND-THE-TASK.                        G7D2PGM 
01207                                                                   G7D2PGM 
01208  2300-900-EXIT.                                                   G7D2PGM 
01209      EXIT.                                                        G7D2PGM 
01210 /***************************************************************  G7D2PGM 
01211 *                                                              *  G7D2PGM 
01212 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7D2PGM 
01213 *                                                              *  G7D2PGM 
01214 ****************************************************************  G7D2PGM 
01215  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7D2PGM 
01216  2310-010.                                                        G7D2PGM 
01217                                                                   G7D2PGM 
01218                                                                   G7D2PGM 
01219 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7D2PGM 
01220                                                                   G7D2PGM 
01221      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7D2PGM 
01222                                         + GC-WORKFILE-KEY-LEN     G7D2PGM 
01223                                         + GC-GCBENPRV-MAX-REC-LEN.G7D2PGM 
01224                                                                   G7D2PGM 
01225      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7D2PGM 
01226                          INITIMG(WS-02-HEX-00)                    G7D2PGM 
01227                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7D2PGM 
01228                          END-EXEC.                                G7D2PGM 
01229                                                                   G7D2PGM 
01230 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7D2PGM 
01231                                                                   G7D2PGM 
01232      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7D2PGM 
01233                                                                   G7D2PGM 
01234      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7D2PGM 
01235      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7D2PGM 
01236      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7D2PGM 
01237      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7D2PGM 
01238      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7D2PGM 
01239      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7D2PGM 
01240                                                                   G7D2PGM 
01241      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7D2PGM 
01242      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7D2PGM 
01243      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7D2PGM 
01244      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7D2PGM 
01245      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7D2PGM 
01246      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7D2PGM 
01247      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7D2PGM 
01248      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7D2PGM 
01249                                                                   G7D2PGM 
01250 ***  MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7D2PGM 
01251 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7D2PGM 
01252 ***  IF  HGADATE-RETURN = ZEROS                                   G7D2PGM 
01253 ***  THEN                                                         G7D2PGM 
01254 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7D2PGM 
01255 ***  ELSE                                                         G7D2PGM 
01256 ***      SET WT-01-INDEX TO +07                                   G7D2PGM 
01257 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D2PGM 
01258 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7D2PGM 
01259                                                                   G7D2PGM 
01260      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7D2PGM 
01261      MOVE S2BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7D2PGM 
01262      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7D2PGM 
01263      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7D2PGM 
01264      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7D2PGM 
01265      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7D2PGM 
01266      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7D2PGM 
01267                                                                   G7D2PGM 
01268                                                                   G7D2PGM 
01269  2310-900-EXIT.                                                   G7D2PGM 
01270      EXIT.                                                        G7D2PGM 
01271 /***************************************************************  G7D2PGM 
01272 *                                                              *  G7D2PGM 
01273 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7D2PGM 
01274 *                                                              *  G7D2PGM 
01275 ****************************************************************  G7D2PGM 
01276  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7D2PGM 
01277  2400-010.                                                        G7D2PGM 
01278                                                                   G7D2PGM 
01279                                                                   G7D2PGM 
01280      IF  EIBAID = DFHPF7  OR DFHPF19                              G7D2PGM 
01281      THEN                                                         G7D2PGM 
01282          MOVE 'G7D1PGM' TO WS-02-NEXT-PROGRAM.                    G7D2PGM 
01283                                                                   G7D2PGM 
01284      IF  EIBAID = DFHENTER OR                                     G7D2PGM 
01285                   DFHPF4   OR DFHPF16 OR                          G7D2PGM 
01286                   DFHPF8   OR DFHPF20                             G7D2PGM 
01287      THEN                                                         G7D2PGM 
01288          MOVE 'GC6APGM' TO WS-02-NEXT-PROGRAM.                    G7D2PGM 
01289                                                                   G7D2PGM 
01290      IF  EIBAID = DFHPF6  OR DFHPF18                              G7D2PGM 
01291      THEN                                                         G7D2PGM 
01292          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7D2PGM 
01293                                                                   G7D2PGM 
01294                                                                   G7D2PGM 
01295      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7D2PGM 
01296                       COMMAREA(WORK-RECORD-2)                     G7D2PGM 
01297                       LENGTH  (GCIO2-RECORD-LENGTH)               G7D2PGM 
01298                       END-EXEC.                                   G7D2PGM 
01299                                                                   G7D2PGM 
01300  2400-900-EXIT.                                                   G7D2PGM 
01301      EXIT.                                                        G7D2PGM 
01302 /***************************************************************  G7D2PGM 
01303 *                                                              *  G7D2PGM 
01304 * 2500   LINK TO GX3APGM FOR CONVERSION.                       *  G7D2PGM 
01305 *                                                              *  G7D2PGM 
01306 ****************************************************************  G7D2PGM 
01307  2500-LINK-TO-GX3APGM.                                            G7D2PGM 
01308                                                                   G7D2PGM 
01309      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7D2PGM 
01310                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7D2PGM 
01311                       LENGTH  (+51)                               G7D2PGM 
01312                       END-EXEC.                                   G7D2PGM 
01313                                                                   G7D2PGM 
01314  2500-EXIT.                                                       G7D2PGM 
01315      EXIT.                                                        G7D2PGM 
01316 /***************************************************************  G7D2PGM 
01317 *                                                              *  G7D2PGM 
01318 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7D2PGM 
01319 *         PROVISION RECORD (TYPE=C4)                           *  G7D2PGM 
01320 *                                                              *  G7D2PGM 
01321 ****************************************************************  G7D2PGM 
01322  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7D2PGM 
01323  5000-010.                                                        G7D2PGM 
01324                                                                   G7D2PGM 
01325      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7D2PGM 
01326                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7D2PGM 
01327                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7D2PGM 
01328                       END-EXEC.                                   G7D2PGM 
01329                                                                   G7D2PGM 
01330                                                                   G7D2PGM 
01331  5000-900-EXIT.                                                   G7D2PGM 
01332      EXIT.                                                        G7D2PGM 
01333 /***************************************************************  G7D2PGM 
01334 *                                                              *  G7D2PGM 
01335 * 5100                                                         *  G7D2PGM 
01336 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7D2PGM 
01337 *                                                              *  G7D2PGM 
01338 ****************************************************************  G7D2PGM 
01339  5100-000-W-F-CONTRACT-IO       SECTION.                          G7D2PGM 
01340  5100-010.                                                        G7D2PGM 
01341                                                                   G7D2PGM 
01342      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7D2PGM 
01343                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7D2PGM 
01344                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7D2PGM 
01345                       END-EXEC.                                   G7D2PGM 
01346                                                                   G7D2PGM 
01347                                                                   G7D2PGM 
01348  5100-900-EXIT.                                                   G7D2PGM 
01349      EXIT.                                                        G7D2PGM 
01350 /***************************************************************  G7D2PGM 
01351 *                                                              *  G7D2PGM 
01352 * 9000   MOVE MESSAGE TO SCREEN                                *  G7D2PGM 
01353 *                                                              *  G7D2PGM 
01354 ****************************************************************  G7D2PGM 
01355  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7D2PGM 
01356  9000-010.                                                        G7D2PGM 
01357                                                                   G7D2PGM 
01358      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S2ERRO.              G7D2PGM 
01359                                                                   G7D2PGM 
01360  9000-900-EXIT.                                                   G7D2PGM 
01361      EXIT.                                                        G7D2PGM 
01362 /***************************************************************  G7D2PGM 
01363 *                                                              *  G7D2PGM 
01364 * 9100 SEND SCREEN AND RETURN                                  *  G7D2PGM 
01365 *                                                              *  G7D2PGM 
01366 ****************************************************************  G7D2PGM 
01367  9100-000-SEND-THEN-RETURN      SECTION.                          G7D2PGM 
01368  9100-010.                                                        G7D2PGM 
01369                                                                   G7D2PGM 
01370                                                                   G7D2PGM 
01371 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7D2PGM 
01372      MOVE  -1 TO  S2ERRL.                                         G7D2PGM 
01373                                                                   G7D2PGM 
01374                                                                   G7D2PGM 
01375      IF  WS-02-MY-EIBTRNID                                        G7D2PGM 
01376      THEN                                                         G7D2PGM 
01377          EXEC CICS  SEND MAP   ('G7D2I01')                        G7D2PGM 
01378                          MAPSET('G7D2SET')                        G7D2PGM 
01379                          DATAONLY                                 G7D2PGM 
01380                          CURSOR                                   G7D2PGM 
01381                          END-EXEC                                 G7D2PGM 
01382      ELSE                                                         G7D2PGM 
01383          EXEC CICS  SEND MAP   ('G7D2I01')                        G7D2PGM 
01384                          MAPSET('G7D2SET')                        G7D2PGM 
01385                          ERASE                                    G7D2PGM 
01386                          CURSOR                                   G7D2PGM 
01387                          END-EXEC.                                G7D2PGM 
01388                                                                   G7D2PGM 
01389      EXEC CICS RETURN                                             G7D2PGM 
01390                TRANSID  ('G7D2')                                  G7D2PGM 
01391                COMMAREA (DFHCOMMAREA)                             G7D2PGM 
01392                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7D2PGM 
01393                END-EXEC.                                          G7D2PGM 
01394 *                                                                 G7D2PGM 
01395  9100-900-EXIT.                                                   G7D2PGM 
01396      EXIT.                                                        G7D2PGM 
01397 /*****************************************************************G7D2PGM 
01398 *                                                                *G7D2PGM 
01399 * 9200    XCTL TO GCPSPGM                                        *G7D2PGM 
01400 *                                                                *G7D2PGM 
01401 *                                                                *G7D2PGM 
01402 ******************************************************************G7D2PGM 
01403  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7D2PGM 
01404  9200-010.                                                        G7D2PGM 
01405                                                                   G7D2PGM 
01406      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7D2PGM 
01407                       END-EXEC.                                   G7D2PGM 
01408                                                                   G7D2PGM 
01409  9200-900-EXIT.                                                   G7D2PGM 
01410      EXIT.                                                        G7D2PGM 
01411 /*****************************************************************G7D2PGM 
01412 *                                                                *G7D2PGM 
01413 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7D2PGM 
01414 *                                                                *G7D2PGM 
01415 *                                                                *G7D2PGM 
01416 ******************************************************************G7D2PGM 
01417  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7D2PGM 
01418  9210-010.                                                        G7D2PGM 
01419                                                                   G7D2PGM 
01420      IF  S2GRPNOI = '000SPS000'                                   G7D2PGM 
01421          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7D2PGM 
01422                           END-EXEC.                               G7D2PGM 
01423                                                                   G7D2PGM 
01424 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7D2PGM 
01425                                                                   G7D2PGM 
01426      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7D2PGM 
01427                                        + GC-WORKFILE-KEY-LEN      G7D2PGM 
01428                                        + GC-GCCONTR-MAX-REC-LEN.  G7D2PGM 
01429                                                                   G7D2PGM 
01430      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7D2PGM 
01431                          INITIMG(WS-02-HEX-00)                    G7D2PGM 
01432                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7D2PGM 
01433                          END-EXEC.                                G7D2PGM 
01434                                                                   G7D2PGM 
01435 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7D2PGM 
01436 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7D2PGM 
01437                                                                   G7D2PGM 
01438 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7D2PGM 
01439                                                                   G7D2PGM 
01440      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7D2PGM 
01441        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7D2PGM 
01442                                                                   G7D2PGM 
01443      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7D2PGM 
01444      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7D2PGM 
01445                                                                   G7D2PGM 
01446      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7D2PGM 
01447      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7D2PGM 
01448      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7D2PGM 
01449      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7D2PGM 
01450      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7D2PGM 
01451      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7D2PGM 
01452                                                                   G7D2PGM 
01453      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7D2PGM 
01454      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7D2PGM 
01455      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7D2PGM 
01456      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7D2PGM 
01457      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7D2PGM 
01458      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7D2PGM 
01459      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7D2PGM 
01460      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7D2PGM 
01461                                                                   G7D2PGM 
01462 ***  MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7D2PGM 
01463 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7D2PGM 
01464 ***  IF  HGADATE-RETURN = ZEROS                                   G7D2PGM 
01465 ***  THEN                                                         G7D2PGM 
01466 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7D2PGM 
01467 ***  ELSE                                                         G7D2PGM 
01468 ***      SET WT-01-INDEX TO +07                                   G7D2PGM 
01469 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7D2PGM 
01470 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7D2PGM 
01471                                                                   G7D2PGM 
01472      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7D2PGM 
01473      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7D2PGM 
01474      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7D2PGM 
01475      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7D2PGM 
01476      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7D2PGM 
01477      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7D2PGM 
01478      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7D2PGM 
01479                                                                   G7D2PGM 
01480      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7D2PGM 
01481                                                                   G7D2PGM 
01482      IF  NOT GCIO3-GOOD-RETURN                                    G7D2PGM 
01483          MOVE WS-01-ABCODE-D2F1     TO WS-01-ABCODE               G7D2PGM 
01484          MOVE WS-01-ABCODE-D2F1-MSG TO WS-01-ABCODE-MSG           G7D2PGM 
01485          PERFORM  9999-000-ABEND-THE-TASK.                        G7D2PGM 
01486                                                                   G7D2PGM 
01487      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7D2PGM 
01488                       COMMAREA(WORK-RECORD-3)                     G7D2PGM 
01489                       LENGTH  (GCIO3-RECORD-LENGTH)               G7D2PGM 
01490                       END-EXEC.                                   G7D2PGM 
01491                                                                   G7D2PGM 
01492  9210-900-EXIT.                                                   G7D2PGM 
01493      EXIT.                                                        G7D2PGM 
01494 /*****************************************************************G7D2PGM 
01495 *                                                                *G7D2PGM 
01496 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7D2PGM 
01497 *                                                                *G7D2PGM 
01498 *                                                                *G7D2PGM 
01499 ******************************************************************G7D2PGM 
01500  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7D2PGM 
01501  9220-010.                                                        G7D2PGM 
01502                                                                   G7D2PGM 
01503      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7D2PGM 
01504                       END-EXEC.                                   G7D2PGM 
01505                                                                   G7D2PGM 
01506  9220-900-EXIT.                                                   G7D2PGM 
01507      EXIT.                                                        G7D2PGM 
01508 /*****************************************************************G7D2PGM 
01509 *                                                                *G7D2PGM 
01510 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7D2PGM 
01511 *                                                                *G7D2PGM 
01512 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7D2PGM 
01513 *                                                                *G7D2PGM 
01514 ******************************************************************G7D2PGM 
01515  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7D2PGM 
01516  9800-010.                                                        G7D2PGM 
01517                                                                   G7D2PGM 
01518      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7D2PGM 
01519      MOVE 'M'   TO  HGADATE-FORM1.                                G7D2PGM 
01520      MOVE 'J'   TO  HGADATE-FORM2.                                G7D2PGM 
01521      MOVE ZEROS TO  HGADATE-RETURN                                G7D2PGM 
01522                     HGADATE-AMOUNT.                               G7D2PGM 
01523      EXEC CICS LINK PROGRAM ('HGADATES')                          G7D2PGM 
01524                     COMMAREA(HGADATES-COMMAREA)                   G7D2PGM 
01525                     LENGTH  (24)                                  G7D2PGM 
01526                     END-EXEC.                                     G7D2PGM 
01527                                                                   G7D2PGM 
01528  9800-900-900-EXIT.                                               G7D2PGM 
01529      EXIT.                                                        G7D2PGM 
01530 /*****************************************************************G7D2PGM 
01531 *                                                                *G7D2PGM 
01532 * 9810    J U L I A N    T O    G R E G O R I A N                *G7D2PGM 
01533 *                                                                *G7D2PGM 
01534 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7D2PGM 
01535 *                                                                *G7D2PGM 
01536 ******************************************************************G7D2PGM 
01537  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7D2PGM 
01538  9810-010.                                                        G7D2PGM 
01539                                                                   G7D2PGM 
01540      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7D2PGM 
01541      MOVE 'J'   TO  HGADATE-FORM1.                                G7D2PGM 
01542      MOVE 'M'   TO  HGADATE-FORM2.                                G7D2PGM 
01543      MOVE ZEROS TO  HGADATE-RETURN                                G7D2PGM 
01544                     HGADATE-AMOUNT.                               G7D2PGM 
01545      EXEC CICS LINK PROGRAM ('HGADATES')                          G7D2PGM 
01546                     COMMAREA(HGADATES-COMMAREA)                   G7D2PGM 
01547                     LENGTH  (24)                                  G7D2PGM 
01548                     END-EXEC.                                     G7D2PGM 
01549                                                                   G7D2PGM 
01550  9810-900-900-EXIT.                                               G7D2PGM 
01551      EXIT.                                                        G7D2PGM 
01552 /***************************************************************  G7D2PGM 
01553 *                                                              *  G7D2PGM 
01554 * 9999  ABEND THE TASK                                         *  G7D2PGM 
01555 *                                                              *  G7D2PGM 
01556 ****************************************************************  G7D2PGM 
01557  9999-000-ABEND-THE-TASK SECTION.                                 G7D2PGM 
01558  9999-010.                                                        G7D2PGM 
01559                                                                   G7D2PGM 
01560      EXEC CICS  ABEND                                             G7D2PGM 
01561                 ABCODE(WS-01-ABCODE)                              G7D2PGM 
01562                 END-EXEC.                                         G7D2PGM 
01563                                                                   G7D2PGM 
01564  9900-900-EXIT.                                                   G7D2PGM 
01565      EXIT.                                                        G7D2PGM 
