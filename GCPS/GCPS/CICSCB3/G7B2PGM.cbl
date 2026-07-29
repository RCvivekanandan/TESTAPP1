00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7B2PGM.                                         G7B2PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7B2PGM 
00005  DATE-WRITTEN.   03/09/87.                                        G7B2PGM 
00006  DATE-COMPILED.                                                   G7B2PGM 
00007 ***************************************************************** G7B2PGM 
00008 *                                                               * G7B2PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7B2PGM 
00010 *                                                               * G7B2PGM 
00011 *                                                               * G7B2PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7B2PGM 
00013 *                                                               * G7B2PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7B2PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7B2PGM 
00016 *                             (SAME AS 'GC5A')                  * G7B2PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7B2PGM 
00018 *                              'GC5A')                          * G7B2PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7B2PGM 
00020 *                              PROVISION)                       * G7B2PGM 
00021 *                                                               * G7B2PGM 
00022 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7B2PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7B2PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7B2PGM 
00025 *                                                               * G7B2PGM 
00026 *  D129      08/22/89  GDM    CONVERT FOR DECIMALS.             * G7B2PGM 
00027 *                                                               * G7B2PGM 
00028 *  D129      09/05/89  GDM    CONVERT TO VS COBOL/2             * G7B2PGM 
00029 *                                                               * G7B2PGM 
00030 *  D12009    08/23/91  BSO    -CORRECT ERR MESSAGES IN AREA \
00031 *                             -CORRECT ALPHA CLASS TEST AREA    * G7B2PGM 
00032 *                                                               * G7B2PGM 
00033 *  D14726    10/28/97  GDM 1. ADDED MILLENNIUM PROCESSING FOR   * G7B2PGM 
00034 *                             DATE                              * G7B2PGM 
00035 *                          2. EXPAND THE COMMAREA KEY TO        * G7B2PGM 
00036 *                             SUPPORT THE TEXAS MERGER.         * G7B2PGM 
00037 *                                                               * G7B2PGM 
00038 * 14726/     03/30/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7B2PGM 
00039 * 15057                     INCREASED GROUP AND SECTION ON      * G7B2PGM 
00040 *                           THE SCREEN.                         * G7B2PGM 
00041 *                                                               * G7B2PGM 
00042 *            12/11/02  AKK  COMPILE FOR OPID                    * G7B2PGM 
00043 *                                                               * G7B2PGM 
00044 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7B2PGM 
00045 *                           G72ASET                              *G7B2PGM 
00046 ***************************************************************** G7B2PGM 
00047                                                                   G7B2PGM 
00048 ***************************************************************** G7B2PGM 
00049 *                                                               * G7B2PGM 
00050 *    G7B2PGM  - PROGRAM 2 OF 2 PROGRAMS TO UPDATE THE FORMAT 'B'* G7B2PGM 
00051 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7B2PGM 
00052 *                                                               * G7B2PGM 
00053 *    TRANSID: G7B2                                              * G7B2PGM 
00054 *    MAPSET:  G7B2SETC    (GIB2PGM WHICH SHARES THIS MAP)       * G7B2PGM 
00055 *    VALGEN:  NONE                                              * G7B2PGM 
00056 *                                                               * G7B2PGM 
00057 *    PROGRAM NARRATIVE:                                         * G7B2PGM 
00058 *                                                               * G7B2PGM 
00059 *        PROGRAM CHECKS FOR TRANS CODE 'G7B2'.  AN INVALID      * G7B2PGM 
00060 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7B2PGM 
00061 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7B2PGM 
00062 *                                                               * G7B2PGM 
00063 *        THE MAIN FUNCTIONS ARE :                               * G7B2PGM 
00064 *        1. HARDCOPY REQUEST,                                   * G7B2PGM 
00065 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7B2PGM 
00066 *           USER,                                               * G7B2PGM 
00067 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7B2PGM 
00068 *                                                               * G7B2PGM 
00069 *        HARDCOPY REQUEST                                       * G7B2PGM 
00070 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7B2PGM 
00071 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7B2PGM 
00072 *           THE SCREEN BUFFER.                                  * G7B2PGM 
00073 *                                                               * G7B2PGM 
00074 *        PROCESS INPUT DATA (UPDATE).                           * G7B2PGM 
00075 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7B2PGM 
00076 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7B2PGM 
00077 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7B2PGM 
00078 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7B2PGM 
00079 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7B2PGM 
00080 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7B2PGM 
00081 *                                                               * G7B2PGM 
00082 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7B2PGM 
00083 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7B2PGM 
00084 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7B2PGM 
00085 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7B2PGM 
00086 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7B2PGM 
00087 *                                                               * G7B2PGM 
00088 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7B2PGM 
00089 *                                     -----------------------   * G7B2PGM 
00090 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7B2PGM 
00091 *           -----------------------------------------------     * G7B2PGM 
00092 *                                                               * G7B2PGM 
00093 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7B2PGM 
00094 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7B2PGM 
00095 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7B2PGM 
00096 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7B2PGM 
00097 *           SENT BACK TO THE USER.                              * G7B2PGM 
00098 *                                                               * G7B2PGM 
00099 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7B2PGM 
00100 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7B2PGM 
00101 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7B2PGM 
00102 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7B2PGM 
00103 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7B2PGM 
00104 *           PF KEY LOGIC,                                       * G7B2PGM 
00105 *              PF6/PF18       GOES TO GC8APGM                   * G7B2PGM 
00106 *              PF8/PF20/ENTER GOES TO GC6APGM                   * G7B2PGM 
00107 *              FOR PF7/PF19   GOES TO G7B1PGM                   * G7B2PGM 
00108 *                                                               * G7B2PGM 
00109 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7B2PGM 
00110 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7B2PGM 
00111 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7B2PGM 
00112 *                                                               * G7B2PGM 
00113 ***************************************************************** G7B2PGM 
00114                                                                   G7B2PGM 
00115  ENVIRONMENT DIVISION.                                            G7B2PGM 
00116  DATA DIVISION.                                                   G7B2PGM 
00117 /                                                                 G7B2PGM 
00118  WORKING-STORAGE SECTION.                                         G7B2PGM 
00119  01  WS-BEGIN                    PIC X(58) VALUE                  G7B2PGM 
00120      '*** G7B2PGM  WORKING-STORAGE BEGINS HERE ***'.              G7B2PGM 
00121                                                                   G7B2PGM 
00122                                                                   G7B2PGM 
00123  01  WS-01-ABEND-AREA.                                            G7B2PGM 
00124      05  FILLER                   PIC X(16)  VALUE                G7B2PGM 
00125          '** ABEND AREA **'.                                      G7B2PGM 
00126                                                                   G7B2PGM 
00127      05  WS-01-ABEND-CODES-AND-MSG.                               G7B2PGM 
00128          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7B2PGM 
00129          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7B2PGM 
00130                                                                   G7B2PGM 
00131          10  WS-01-ABCODE-B2F1          PIC X(04)  VALUE  'B2F1'. G7B2PGM 
00132          10  WS-01-ABCODE-B2F1-MSG      PIC X(44)  VALUE          G7B2PGM 
00133             'W/F CONTRACT CANNOT BE FOUND             '.          G7B2PGM 
00134                                                                   G7B2PGM 
00135          10  WS-01-ABCODE-B2F2          PIC X(04)  VALUE  'B2F2'. G7B2PGM 
00136          10  WS-01-ABCODE-B2F2-MSG      PIC X(44)  VALUE          G7B2PGM 
00137             'W/F BEN PROV CANNOT BE FOUND             '.          G7B2PGM 
00138                                                                   G7B2PGM 
00139          10  WS-01-ABCODE-B2F3          PIC X(04)  VALUE  'B2F3'. G7B2PGM 
00140          10  WS-01-ABCODE-B2F3-MSG      PIC X(44)  VALUE          G7B2PGM 
00141             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7B2PGM 
00142                                                                   G7B2PGM 
00143          10  WS-01-ABCODE-B2F4          PIC X(04)  VALUE  'B2F4'. G7B2PGM 
00144          10  WS-01-ABCODE-B2F4-MSG      PIC X(44)  VALUE          G7B2PGM 
00145             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7B2PGM 
00146                                                                   G7B2PGM 
00147          10  WS-01-ABCODE-B2L1          PIC X(04)  VALUE  'B2L1'. G7B2PGM 
00148          10  WS-01-ABCODE-B2L1-MSG      PIC X(44)  VALUE          G7B2PGM 
00149             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7B2PGM 
00150                                                                   G7B2PGM 
00151          10  WS-01-ABCODE-B2P1          PIC X(04)  VALUE  'B2P1'. G7B2PGM 
00152          10  WS-01-ABCODE-B2P1-MSG      PIC X(44)  VALUE          G7B2PGM 
00153             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7B2PGM 
00154                                                                   G7B2PGM 
00155          10  WS-01-ABCODE-B2P2          PIC X(04)  VALUE  'B2P2'. G7B2PGM 
00156          10  WS-01-ABCODE-B2P2-MSG      PIC X(44)  VALUE          G7B2PGM 
00157             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7B2PGM 
00158                                                                   G7B2PGM 
00159  01  WS-02-AREA.                                                  G7B2PGM 
00160      05  FILLER                   PIC X(16)  VALUE                G7B2PGM 
00161          '** WS-02-AREA **'.                                      G7B2PGM 
00162      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7B2PGM 
00163          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7B2PGM 
00164                                                    'GC6A' 'G7B1'  G7B2PGM 
00165                                                    'G7B2'.        G7B2PGM 
00166          88  WS-02-MY-EIBTRNID                     VALUE  'G7B2'. G7B2PGM 
00167                                                                   G7B2PGM 
00168      05  WS-02-COMPUTED-LENGTHS.                                  G7B2PGM 
00169          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7B2PGM 
00170          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7B2PGM 
00171          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7B2PGM 
00172                                                                   G7B2PGM 
00173      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7B2PGM 
00174                                                                   G7B2PGM 
00175      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7B2PGM 
00176                                                                   G7B2PGM 
00177      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7B2PGM 
00178      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7B2PGM 
00179          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7B2PGM 
00180                                         PIC X.                    G7B2PGM 
00181          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7B2PGM 
00182                                                    '0' THRU '9'   G7B2PGM 
00183                                                    'A' THRU 'I'   G7B2PGM 
00184                                                    'J' THRU 'R'   G7B2PGM 
00185                                                    'S' THRU 'Z'   G7B2PGM 
00186                                                    SPACE.         G7B2PGM 
00187                                                                   G7B2PGM 
00188      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7B2PGM 
00189          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7B2PGM 
00190          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7B2PGM 
00191                                                                   G7B2PGM 
00192      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7B2PGM 
00193          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7B2PGM 
00194                                                                   G7B2PGM 
00195      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7B2PGM 
00196                                                                   G7B2PGM 
00197      05  WS-02-HEX-F00000.                                        G7B2PGM 
00198          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7B2PGM 
00199          10  FILLER                     PIC  X(09) VALUE          G7B2PGM 
00200                                                    LOW-VALUES.    G7B2PGM 
00201                                                                   G7B2PGM 
00202      05  WS-02-MAX-AMT-PER-VISIT-X.                               G7B2PGM 
00203          10  WS-02-MAX-AMT-PER-VISIT      PIC 999V99  VALUE ZEROS.G7B2PGM 
00204 *        10  WS-02-S2MAXAT              REDEFINES                 G7B2PGM 
00205 *            WS-02-MAX-AMT-PER-VISIT      PIC X(5).               G7B2PGM 
00206                                                                   G7B2PGM 
00207      05  WS-5POS-MAX-AMT                PIC 999V99 VALUE 999.99.  G7B2PGM 
00208      05  WS-02-DISP-5POS-DEC            PIC 9(3).99.              G7B2PGM 
00209      05  WS-GPB2-MAX-AMT-PER-VISIT      PIC 9(3)V99.              G7B2PGM 
00210 /                                                                 G7B2PGM 
00211  01  WT-00-G7B2PGM-TABLES.                                        G7B2PGM 
00212      05  FILLER                   PIC X(16)  VALUE                G7B2PGM 
00213          '*G7B2PGM TABLES*'.                                      G7B2PGM 
00214                                                                   G7B2PGM 
00215  01  WT-01-TABLE.                                                 G7B2PGM 
00216      05  FILLER                  PIC X(16) VALUE                  G7B2PGM 
00217          '* WT-01-TABLE  *'.                                      G7B2PGM 
00218 ******************************************************************G7B2PGM 
00219 *    WT-01   MESSAGE TABLE                                       *G7B2PGM 
00220 ******************************************************************G7B2PGM 
00221  01  FILLER.                                                      G7B2PGM 
00222      05  WT-01-MESSAGE-VALUES.                                    G7B2PGM 
00223                                                                   G7B2PGM 
00224 *----------------------------------------------------------------*G7B2PGM 
00225          10  WT-01-ENTRY-001.                                     G7B2PGM 
00226              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00227              15  WT-01-MESSAGE-TEXT-001.                          G7B2PGM 
00228                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00229                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00230                  20  FILLER          PIC X(3)  VALUE  '001'.      G7B2PGM 
00231                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00232                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00233                      ' INVALID PFKEY SELECTION                    G7B2PGM 
00234 -                    '                         '.                 G7B2PGM 
00235              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00236                                                                   G7B2PGM 
00237 *----------------------------------------------------------------*G7B2PGM 
00238          10  WT-01-ENTRY-002.                                     G7B2PGM 
00239              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00240              15  WT-01-MESSAGE-TEXT-002.                          G7B2PGM 
00241                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00242                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00243                  20  FILLER          PIC X(3)  VALUE  '002'.      G7B2PGM 
00244                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00245                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00246                      'INVALID DECIMAL DETECTED'.                  G7B2PGM 
00247              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00248                                                                   G7B2PGM 
00249 *----------------------------------------------------------------*G7B2PGM 
00250          10  WT-01-ENTRY-003.                                     G7B2PGM 
00251              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00252              15  WT-01-MESSAGE-TEXT-003.                          G7B2PGM 
00253                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00254                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00255                  20  FILLER          PIC X(3)  VALUE  '003'.      G7B2PGM 
00256                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00257                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00258                      'FIELD EXCEEDS LENGTH OF 5 POSITIONS  FORMAT G7B2PGM 
00259 -                    ' IS 999.99'.                                G7B2PGM 
00260              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00261                                                                   G7B2PGM 
00262 *----------------------------------------------------------------*G7B2PGM 
00263          10  WT-01-ENTRY-004.                                     G7B2PGM 
00264              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00265              15  WT-01-MESSAGE-TEXT-004.                          G7B2PGM 
00266                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00267                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00268                  20  FILLER          PIC X(3)  VALUE  '004'.      G7B2PGM 
00269                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00270                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00271                      '********** F U T U R E   U S E *************G7B2PGM 
00272 -                    '*************************'.                 G7B2PGM 
00273              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00274                                                                   G7B2PGM 
00275 *----------------------------------------------------------------*G7B2PGM 
00276          10  WT-01-ENTRY-005.                                     G7B2PGM 
00277              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00278              15  WT-01-MESSAGE-TEXT-005.                          G7B2PGM 
00279                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00280                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00281                  20  FILLER          PIC X(3)  VALUE  '005'.      G7B2PGM 
00282                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00283                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00284                      '********** F U T U R E   U S E *************G7B2PGM 
00285 -                    '*************************'.                 G7B2PGM 
00286              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00287                                                                   G7B2PGM 
00288 *----------------------------------------------------------------*G7B2PGM 
00289          10  WT-01-ENTRY-006.                                     G7B2PGM 
00290              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00291              15  WT-01-MESSAGE-TEXT-006.                          G7B2PGM 
00292                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00293                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00294                  20  FILLER          PIC X(3)  VALUE  '006'.      G7B2PGM 
00295                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00296                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00297                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7B2PGM 
00298 -                    ' PF4/PF16 TO CONTINUE    '.                 G7B2PGM 
00299              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00300                                                                   G7B2PGM 
00301 *----------------------------------------------------------------*G7B2PGM 
00302          10  WT-01-ENTRY-007.                                     G7B2PGM 
00303              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00304              15  WT-01-MESSAGE-TEXT-007.                          G7B2PGM 
00305                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00306                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00307                  20  FILLER          PIC X(3)  VALUE  '007'.      G7B2PGM 
00308                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00309                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00310                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7B2PGM 
00311 -                    'E CALL SYSTEMS           '.                 G7B2PGM 
00312              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00313                                                                   G7B2PGM 
00314 *----------------------------------------------------------------*G7B2PGM 
00315          10  WT-01-ENTRY-008.                                     G7B2PGM 
00316              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00317              15  WT-01-MESSAGE-TEXT-008.                          G7B2PGM 
00318                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00319                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00320                  20  FILLER          PIC X(3)  VALUE  '008'.      G7B2PGM 
00321                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00322                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00323                      'FIELD HAS AN INVALID VALUE                  G7B2PGM 
00324 -                    '                         '.                 G7B2PGM 
00325              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00326                                                                   G7B2PGM 
00327 *----------------------------------------------------------------*G7B2PGM 
00328          10  WT-01-ENTRY-009.                                     G7B2PGM 
00329              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00330              15  WT-01-MESSAGE-TEXT-009.                          G7B2PGM 
00331                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00332                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00333                  20  FILLER          PIC X(3)  VALUE  '009'.      G7B2PGM 
00334                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00335                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00336                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7B2PGM 
00337 -                    'YSTEM)                   '.                 G7B2PGM 
00338              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00339                                                                   G7B2PGM 
00340 *----------------------------------------------------------------*G7B2PGM 
00341          10  WT-01-ENTRY-010.                                     G7B2PGM 
00342              15  FILLER              PIC X(2)  VALUE '¬>'.        G7B2PGM 
00343              15  WT-01-MESSAGE-TEXT-010.                          G7B2PGM 
00344                  20  FILLER          PIC X(4)  VALUE  'G7B2'.     G7B2PGM 
00345                  20  FILLER          PIC X(1)  VALUE  '-'.        G7B2PGM 
00346                  20  FILLER          PIC X(3)  VALUE  '010'.      G7B2PGM 
00347                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7B2PGM 
00348                  20  FILLER          PIC X(70) VALUE              G7B2PGM 
00349                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7B2PGM 
00350 -                    '                         '.                 G7B2PGM 
00351              15  FILLER              PIC X(2)  VALUE '<¬'.        G7B2PGM 
00352 *----------------------------------------------------------------*G7B2PGM 
00353                                                                   G7B2PGM 
00354      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7B2PGM 
00355          WT-01-MESSAGE-VALUES         OCCURS 010 TIMES            G7B2PGM 
00356                                      INDEXED BY WT-01-INDEX.      G7B2PGM 
00357          10  WT-01-ENTRY.                                         G7B2PGM 
00358              15  FILLER              PIC X(02).                   G7B2PGM 
00359              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7B2PGM 
00360              15  FILLER              PIC X(02).                   G7B2PGM 
00361                                                                   G7B2PGM 
00362                                                                   G7B2PGM 
00363 /*** MAP FIELD ATTRIBUTES                                         G7B2PGM 
00364  COPY DFHBMSCA.                                                   G7B2PGM 
00365 *                         AUTOSKIP, BRIGHT, FSET                  G7B2PGM 
00366      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7B2PGM 
00367                                                                   G7B2PGM 
00368 /*** ATTENTION KEYS                                               G7B2PGM 
00369  COPY DFHAID.                                                     G7B2PGM 
00370                                                                   G7B2PGM 
00371 /***  PROVISION MAINTENANCE SCREEN                                G7B2PGM 
00372  COPY  G7B2SETC.                                                  G7B2PGM 
00373                                                                   G7B2PGM 
00374 /*** DATE ROUTINE COMMAREA                                        G7B2PGM 
00375  01  HGADATES-COMMAREA.                                           G7B2PGM 
00376  COPY HGCDAT01.                                                   G7B2PGM 
00377                                                                   G7B2PGM 
00378 /*** DECIMAL CONVERSION COMMAREA                                  G7B2PGM 
00379  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7B2PGM 
00380  COPY GCDCCA01.                                                   G7B2PGM 
00381                                                                   G7B2PGM 
00382 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7B2PGM 
00383  01  GCVIOPGM-PARM-LIST.                                          G7B2PGM 
00384  COPY GCVINTRC.                                                   G7B2PGM 
00385                                                                   G7B2PGM 
00386 /*** ALTERNATIVE WORKFILE KEYS                                    G7B2PGM 
00387  01  FILLER.                                                      G7B2PGM 
00388      COPY GCWRKKEY.                                               G7B2PGM 
00389                                                                   G7B2PGM 
00390 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7B2PGM 
00391  01  FILLER.                                                      G7B2PGM 
00392      COPY GCCDRLEN.                                               G7B2PGM 
00393                                                                   G7B2PGM 
00394                                                                   G7B2PGM 
00395  01  WS-END                       PIC X(58) VALUE                 G7B2PGM 
00396      '*** G7B2PGM  WORKING-STORAGE ENDS HERE ***'.                G7B2PGM 
00397 /                                                                 G7B2PGM 
00398  LINKAGE SECTION.                                                 G7B2PGM 
00399 /                                                                 G7B2PGM 
00400  01  DFHCOMMAREA.                                                 G7B2PGM 
00401      COPY  GCWRKDCC.                                              G7B2PGM 
00402      COPY  GCBENPVC.                                              G7B2PGM 
00403 /                                                                 G7B2PGM 
00404 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7B2PGM 
00405  01  IO-PARM-BEN-PROV-AREA.                                       G7B2PGM 
00406      COPY  GCIOPRM2.                                              G7B2PGM 
00407      COPY  GCWRKDC2.                                              G7B2PGM 
00408      COPY  GCBENPV2.                                              G7B2PGM 
00409                                                                   G7B2PGM 
00410 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7B2PGM 
00411  01  IO-PARM-CONTRACT-AREA.                                       G7B2PGM 
00412      COPY  GCIOPRM3.                                              G7B2PGM 
00413      COPY  GCWRKDC3.                                              G7B2PGM 
00414      COPY  GCCONTR2.                                              G7B2PGM 
00415 /                                                                 G7B2PGM 
00416  PROCEDURE DIVISION.                                              G7B2PGM 
00417                                                                   G7B2PGM 
00418 ****************************************************************  G7B2PGM 
00419 *                                                              *  G7B2PGM 
00420 *           P R O C E S S     C O N T R O L                    *  G7B2PGM 
00421 *                                                              *  G7B2PGM 
00422 ****************************************************************  G7B2PGM 
00423  0000-000-PROCESS-CONTROL       SECTION.                          G7B2PGM 
00424  0000-010.                                                        G7B2PGM 
00425                                                                   G7B2PGM 
00426      IF  EIBAID  =  DFHCLEAR                                      G7B2PGM 
00427          EXEC CICS  RETURN                                        G7B2PGM 
00428                     END-EXEC.                                     G7B2PGM 
00429                                                                   G7B2PGM 
00430      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7B2PGM 
00431                                                                   G7B2PGM 
00432      IF  WS-02-MY-EIBTRNID                                        G7B2PGM 
00433      THEN                                                         G7B2PGM 
00434          PERFORM  2000-000-PROCESS-INPUT                          G7B2PGM 
00435      ELSE                                                         G7B2PGM 
00436          PERFORM  1000-000-DISPLAY-SCREEN.                        G7B2PGM 
00437                                                                   G7B2PGM 
00438                                                                   G7B2PGM 
00439 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7B2PGM 
00440                                                                   G7B2PGM 
00441      MOVE WS-01-ABCODE-B2L1     TO WS-01-ABCODE                   G7B2PGM 
00442      MOVE WS-01-ABCODE-B2L1-MSG TO WS-01-ABCODE-MSG               G7B2PGM 
00443      PERFORM  9999-000-ABEND-THE-TASK.                            G7B2PGM 
00444                                                                   G7B2PGM 
00445      GOBACK.                                                      G7B2PGM 
00446                                                                   G7B2PGM 
00447                                                                   G7B2PGM 
00448  0000-900-EXIT.                                                   G7B2PGM 
00449      EXIT.                                                        G7B2PGM 
00450 /***************************************************************  G7B2PGM 
00451 *                                                              *  G7B2PGM 
00452 * 1000  DISPLAY INITIAL SCREEN                                 *  G7B2PGM 
00453 *                                                              *  G7B2PGM 
00454 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7B2PGM 
00455 *                                                              *  G7B2PGM 
00456 ****************************************************************  G7B2PGM 
00457  1000-000-DISPLAY-SCREEN        SECTION.                          G7B2PGM 
00458  1000-010.                                                        G7B2PGM 
00459                                                                   G7B2PGM 
00460 *------- D129     SET SCREEN TO LOW VALUES FOR FIRST DISPLAY      G7B2PGM 
00461 *                                                                 G7B2PGM 
00462      MOVE LOW-VALUES TO G7B2I01I.                                 G7B2PGM 
00463                                                                   G7B2PGM 
00464 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7B2PGM 
00465                                                                   G7B2PGM 
00466      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7B2PGM 
00467          MOVE WS-01-ABCODE-B2P1     TO WS-01-ABCODE               G7B2PGM 
00468          MOVE WS-01-ABCODE-B2P1-MSG TO WS-01-ABCODE-MSG           G7B2PGM 
00469          PERFORM 9999-000-ABEND-THE-TASK.                         G7B2PGM 
00470                                                                   G7B2PGM 
00471                                                                   G7B2PGM 
00472 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7B2PGM 
00473                                                                   G7B2PGM 
00474      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7B2PGM 
00475                                         + GC-GCBENPRV-FIXED-LEN   G7B2PGM 
00476                                         + GC-GCBENPRV-VARY-LEN.   G7B2PGM 
00477                                                                   G7B2PGM 
00478                                                                   G7B2PGM 
00479 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7B2PGM 
00480                                                                   G7B2PGM 
00481      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7B2PGM 
00482          MOVE WS-01-ABCODE-B2P2     TO WS-01-ABCODE               G7B2PGM 
00483          MOVE WS-01-ABCODE-B2P2-MSG TO WS-01-ABCODE-MSG           G7B2PGM 
00484          PERFORM 9999-000-ABEND-THE-TASK.                         G7B2PGM 
00485                                                                   G7B2PGM 
00486                                                                   G7B2PGM 
00487 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7B2PGM 
00488 *          BY CALLER IN COMMAREA.                                 G7B2PGM 
00489                                                                   G7B2PGM 
00490      MOVE WRK-PLAN-CODE                      TO S2PLNCDO.         G7B2PGM 
00491      MOVE WRK-GROUP-NUM                      TO S2GRPNOO.         G7B2PGM 
00492      MOVE WRK-SECTION-NUM                    TO S2SECNOO.         G7B2PGM 
00493      MOVE WRK-PKG-CODE                       TO S2PKGCDO.         G7B2PGM 
00494      MOVE WRK-PROV-CTL                       TO S2PRVO.           G7B2PGM 
00495      MOVE WRK-FAM-REL-LEVEL                  TO S2FRLO.           G7B2PGM 
00496      MOVE WRK-L-O-B                          TO S2LOBO.           G7B2PGM 
00497                                                                   G7B2PGM 
00498      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7B2PGM 
00499      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7B2PGM 
00500      IF  HGADATE-RETURN = ZEROS                                   G7B2PGM 
00501      THEN                                                         G7B2PGM 
00502          MOVE DFHBMASF                       TO S2EFFDTA          G7B2PGM 
00503          MOVE HGADATE-DATE2                  TO S2EFFDTO          G7B2PGM 
00504      ELSE                                                         G7B2PGM 
00505          MOVE DFHBMABF                       TO S2EFFDTA          G7B2PGM 
00506          MOVE HGADATE-JULIAN1                TO S2EFFDTO.         G7B2PGM 
00507                                                                   G7B2PGM 
00508      MOVE GCP-PROVN-ID                       TO S2BPVIDO.         G7B2PGM 
00509                                                                   G7B2PGM 
00510      MOVE GPB-PHYS-EXAM-IND                  TO S2PHEXIO.         G7B2PGM 
00511      MOVE GPB-PROF-CHRG-HSP-CLM              TO S2PCOHCO.         G7B2PGM 
00512      MOVE GPB-AMBULANCE-ELIG-IND             TO S2AMBEIO.         G7B2PGM 
00513      MOVE GPB-ALCO-ELIG-MEMB-CLS-OVRD        TO S2AECOIO.         G7B2PGM 
00514      MOVE GPB-DRUG-ELIG-MEMB-CLS-OVRD        TO S2DECOIO.         G7B2PGM 
00515      MOVE GPB-ECF-SNF-OVRD-IND               TO S2ESOVIO.         G7B2PGM 
00516      MOVE GPB-NORM-NWBORN-OVRD-IND           TO S2NNOVIO.         G7B2PGM 
00517                                                                   G7B2PGM 
00518 *    IF  GPB-MAX-AMT-PER-VISIT = ZEROS                            G7B2PGM 
00519 *        MOVE WS-02-HEX-F00000               TO S2MAXATO          G7B2PGM 
00520 *    ELSE                                                         G7B2PGM 
00521 *        MOVE   GPB-MAX-AMT-PER-VISIT        TO                   G7B2PGM 
00522 *             WS-02-MAX-AMT-PER-VISIT                             G7B2PGM 
00523 *        MOVE WS-02-MAX-AMT-PER-VISIT-X      TO S2MAXATO.         G7B2PGM 
00524                                                                   G7B2PGM 
00525 * D129     DECIMAL CONVERSION.                                    G7B2PGM 
00526 *                                                                 G7B2PGM 
00527          MOVE   GPB-MAX-AMT-PER-VISIT        TO                   G7B2PGM 
00528               WS-02-MAX-AMT-PER-VISIT.                            G7B2PGM 
00529          MOVE WS-02-MAX-AMT-PER-VISIT        TO                   G7B2PGM 
00530               WS-02-DISP-5POS-DEC.                                G7B2PGM 
00531          MOVE WS-02-DISP-5POS-DEC            TO S2MAXATO.         G7B2PGM 
00532                                                                   G7B2PGM 
00533      MOVE GPB-TRANSSXL-PMT-RESTR-OVRD        TO S2TXPRIO.         G7B2PGM 
00534                                                                   G7B2PGM 
00535                                                                   G7B2PGM 
00536                                                                   G7B2PGM 
00537 *------- SEND INITIAL SCREEN ------------------------------------*G7B2PGM 
00538                                                                   G7B2PGM 
00539      MOVE  -1 TO  S2PHEXIL.                                       G7B2PGM 
00540      PERFORM 9100-000-SEND-THEN-RETURN.                           G7B2PGM 
00541                                                                   G7B2PGM 
00542                                                                   G7B2PGM 
00543  1000-900-EXIT.                                                   G7B2PGM 
00544      EXIT.                                                        G7B2PGM 
00545 /***************************************************************  G7B2PGM 
00546 *                                                              *  G7B2PGM 
00547 * 2000    P R O C E S S    I N P U T                           *  G7B2PGM 
00548 *                                                              *  G7B2PGM 
00549 ****************************************************************  G7B2PGM 
00550  2000-000-PROCESS-INPUT         SECTION.                          G7B2PGM 
00551  2000-010.                                                        G7B2PGM 
00552                                                                   G7B2PGM 
00553 *------ VALIDATE PFKEY USAGE ------------------------------------*G7B2PGM 
00554                                                                   G7B2PGM 
00555      IF  EIBAID = DFHENTER OR                                     G7B2PGM 
00556                   DFHPF3   OR  DFHPF15 OR                         G7B2PGM 
00557                   DFHPF4   OR  DFHPF16 OR                         G7B2PGM 
00558                   DFHPF6   OR  DFHPF18 OR                         G7B2PGM 
00559                   DFHPF7   OR  DFHPF19 OR                         G7B2PGM 
00560                   DFHPF8   OR  DFHPF20                            G7B2PGM 
00561      THEN                                                         G7B2PGM 
00562          NEXT SENTENCE                                            G7B2PGM 
00563      ELSE                                                         G7B2PGM 
00564          SET WT-01-INDEX TO +01                                   G7B2PGM 
00565          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7B2PGM 
00566          PERFORM 9100-000-SEND-THEN-RETURN.                       G7B2PGM 
00567                                                                   G7B2PGM 
00568                                                                   G7B2PGM 
00569                                                                   G7B2PGM 
00570      EXEC CICS  HANDLE CONDITION                                  G7B2PGM 
00571                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7B2PGM 
00572                        END-EXEC.                                  G7B2PGM 
00573                                                                   G7B2PGM 
00574                                                                   G7B2PGM 
00575      EXEC CICS  RECEIVE MAP   ('G7B2I01')                         G7B2PGM 
00576                         MAPSET('G7B2SET')                         G7B2PGM 
00577                         END-EXEC.                                 G7B2PGM 
00578                                                                   G7B2PGM 
00579                                                                   G7B2PGM 
00580      IF  S2FUNCI  NOT = 'G7B2'  OR                                G7B2PGM 
00581          S2SCRNI  NOT = '007B02'                                  G7B2PGM 
00582          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7B2PGM 
00583                                                                   G7B2PGM 
00584                                                                   G7B2PGM 
00585 *--- RETURN TO GCPS MENU? ---------------------------------------*G7B2PGM 
00586                                                                   G7B2PGM 
00587      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7B2PGM 
00588          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7B2PGM 
00589                                                                   G7B2PGM 
00590 *--- PROCESS SCREEN FIELDS --------------------------------------*G7B2PGM 
00591                                                                   G7B2PGM 
00592      PERFORM 2100-000-FIELD-EDITS.                                G7B2PGM 
00593                                                                   G7B2PGM 
00594      IF  WS-02-SCREEN-HAS-ERRORS                                  G7B2PGM 
00595          PERFORM 9100-000-SEND-THEN-RETURN.                       G7B2PGM 
00596                                                                   G7B2PGM 
00597      PERFORM 2200-000-LOGICAL-EDITS.                              G7B2PGM 
00598                                                                   G7B2PGM 
00599      IF  WS-02-SCREEN-HAS-ERRORS                                  G7B2PGM 
00600          PERFORM 9100-000-SEND-THEN-RETURN.                       G7B2PGM 
00601                                                                   G7B2PGM 
00602      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7B2PGM 
00603                                                                   G7B2PGM 
00604      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7B2PGM 
00605                                                                   G7B2PGM 
00606                                                                   G7B2PGM 
00607  2000-900-EXIT.                                                   G7B2PGM 
00608      EXIT.                                                        G7B2PGM 
00609 /***************************************************************  G7B2PGM 
00610 *                                                              *  G7B2PGM 
00611 * 2100  DO SCREEN FIELD EDITS                                  *  G7B2PGM 
00612 *                                                              *  G7B2PGM 
00613 ****************************************************************  G7B2PGM 
00614  2100-000-FIELD-EDITS           SECTION.                          G7B2PGM 
00615  2100-010.                                                        G7B2PGM 
00616                                                                   G7B2PGM 
00617 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7B2PGM 
00618                                                                   G7B2PGM 
00619      MOVE DFHBMUNF TO  S2PHEXIA                                   G7B2PGM 
00620                        S2PCOHCA                                   G7B2PGM 
00621                        S2AMBEIA                                   G7B2PGM 
00622                        S2AECOIA                                   G7B2PGM 
00623                        S2DECOIA                                   G7B2PGM 
00624                        S2ESOVIA                                   G7B2PGM 
00625                        S2NNOVIA                                   G7B2PGM 
00626                        S2MAXATA                                   G7B2PGM 
00627                        S2TXPRIA.                                  G7B2PGM 
00628                                                                   G7B2PGM 
00629      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7B2PGM 
00630                                                                   G7B2PGM 
00631                                                                   G7B2PGM 
00632 *-- VALIDATE ------ PHYSICAL EXAM IND ---------------------------*G7B2PGM 
00633 *   1. ALPHANUMERIC                                               G7B2PGM 
00634 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00635                                                                   G7B2PGM 
00636      MOVE  S2PHEXII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00637      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00638      THEN                                                         G7B2PGM 
00639          MOVE  S2PHEXII TO GCVI-VALUE                             G7B2PGM 
00640          MOVE  'BPBB01' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00641          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00642          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00643          THEN                                                     G7B2PGM 
00644              MOVE  -1        TO  S2PHEXIL                         G7B2PGM 
00645              MOVE  DFHBMUBF  TO  S2PHEXIA                         G7B2PGM 
00646              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00647              THEN                                                 G7B2PGM 
00648                  NEXT SENTENCE                                    G7B2PGM 
00649              ELSE                                                 G7B2PGM 
00650                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00651                  SET WT-01-INDEX TO +09                           G7B2PGM 
00652                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00653          ELSE                                                     G7B2PGM 
00654              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00655              THEN                                                 G7B2PGM 
00656                  MOVE  DFHBMUBF  TO  S2PHEXIA                     G7B2PGM 
00657              ELSE                                                 G7B2PGM 
00658                  NEXT SENTENCE                                    G7B2PGM 
00659      ELSE                                                         G7B2PGM 
00660          MOVE  -1        TO  S2PHEXIL                             G7B2PGM 
00661          MOVE  DFHBMUBF  TO  S2PHEXIA                             G7B2PGM 
00662          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00663          THEN                                                     G7B2PGM 
00664              NEXT SENTENCE                                        G7B2PGM 
00665          ELSE                                                     G7B2PGM 
00666              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00667              SET WT-01-INDEX TO +08                               G7B2PGM 
00668              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00669                                                                   G7B2PGM 
00670                                                                   G7B2PGM 
00671 *-- VALIDATE ------ PROFESSINAL CHARGES ON A HOSPITAL CLAIM -----*G7B2PGM 
00672 *   1. ALPHANUMERIC                                               G7B2PGM 
00673 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00674                                                                   G7B2PGM 
00675      MOVE  S2PCOHCI TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00676      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00677      THEN                                                         G7B2PGM 
00678          MOVE  S2PCOHCI TO GCVI-VALUE                             G7B2PGM 
00679          MOVE  'BPBB02' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00680          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00681          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00682          THEN                                                     G7B2PGM 
00683              MOVE  -1        TO  S2PCOHCL                         G7B2PGM 
00684              MOVE  DFHBMUBF  TO  S2PCOHCA                         G7B2PGM 
00685              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00686              THEN                                                 G7B2PGM 
00687                  NEXT SENTENCE                                    G7B2PGM 
00688              ELSE                                                 G7B2PGM 
00689                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00690                  SET WT-01-INDEX TO +09                           G7B2PGM 
00691                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00692          ELSE                                                     G7B2PGM 
00693              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00694              THEN                                                 G7B2PGM 
00695                  MOVE  DFHBMUBF  TO  S2PCOHCA                     G7B2PGM 
00696              ELSE                                                 G7B2PGM 
00697                  NEXT SENTENCE                                    G7B2PGM 
00698      ELSE                                                         G7B2PGM 
00699          MOVE  -1        TO  S2PCOHCL                             G7B2PGM 
00700          MOVE  DFHBMUBF  TO  S2PCOHCA                             G7B2PGM 
00701          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00702          THEN                                                     G7B2PGM 
00703              NEXT SENTENCE                                        G7B2PGM 
00704          ELSE                                                     G7B2PGM 
00705              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00706              SET WT-01-INDEX TO +08                               G7B2PGM 
00707              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00708                                                                   G7B2PGM 
00709                                                                   G7B2PGM 
00710 *-- VALIDATE ------ AMBULANCE ELIGIBILITY IND -------------------*G7B2PGM 
00711 *   1. ALPHANUMERIC                                               G7B2PGM 
00712 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00713                                                                   G7B2PGM 
00714      MOVE  S2AMBEII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00715      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7B2PGM 
00716          WS-02-CLASS-ALPHANUMERIC(2)                              G7B2PGM 
00717      THEN                                                         G7B2PGM 
00718          MOVE  S2AMBEII TO GCVI-VALUE                             G7B2PGM 
00719          MOVE  'BPBB03' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00720          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00721          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00722          THEN                                                     G7B2PGM 
00723              MOVE  -1        TO  S2AMBEIL                         G7B2PGM 
00724              MOVE  DFHBMUBF  TO  S2AMBEIA                         G7B2PGM 
00725              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00726              THEN                                                 G7B2PGM 
00727                  NEXT SENTENCE                                    G7B2PGM 
00728              ELSE                                                 G7B2PGM 
00729                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00730                  SET WT-01-INDEX TO +09                           G7B2PGM 
00731                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00732          ELSE                                                     G7B2PGM 
00733              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00734              THEN                                                 G7B2PGM 
00735                  MOVE  DFHBMUBF  TO  S2AMBEIA                     G7B2PGM 
00736              ELSE                                                 G7B2PGM 
00737                  NEXT SENTENCE                                    G7B2PGM 
00738      ELSE                                                         G7B2PGM 
00739          MOVE  -1        TO  S2AMBEIL                             G7B2PGM 
00740          MOVE  DFHBMUBF  TO  S2AMBEIA                             G7B2PGM 
00741          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00742          THEN                                                     G7B2PGM 
00743              NEXT SENTENCE                                        G7B2PGM 
00744          ELSE                                                     G7B2PGM 
00745              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00746              SET WT-01-INDEX TO +08                               G7B2PGM 
00747              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00748                                                                   G7B2PGM 
00749                                                                   G7B2PGM 
00750 *-- VALIDATE ------ ALCOHOL ELIGIBLE MEMBER OVERRIDE IND --------*G7B2PGM 
00751 *   1. ALPHANUMERIC                                               G7B2PGM 
00752 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00753                                                                   G7B2PGM 
00754      MOVE  S2AECOII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00755      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00756      THEN                                                         G7B2PGM 
00757          MOVE  S2AECOII TO GCVI-VALUE                             G7B2PGM 
00758          MOVE  'BPAB03' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00759          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00760          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00761          THEN                                                     G7B2PGM 
00762              MOVE  -1        TO  S2AECOIL                         G7B2PGM 
00763              MOVE  DFHBMUBF  TO  S2AECOIA                         G7B2PGM 
00764              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00765              THEN                                                 G7B2PGM 
00766                  NEXT SENTENCE                                    G7B2PGM 
00767              ELSE                                                 G7B2PGM 
00768                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00769                  SET WT-01-INDEX TO +09                           G7B2PGM 
00770                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00771          ELSE                                                     G7B2PGM 
00772              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00773              THEN                                                 G7B2PGM 
00774                  MOVE  DFHBMUBF  TO  S2AECOIA                     G7B2PGM 
00775              ELSE                                                 G7B2PGM 
00776                  NEXT SENTENCE                                    G7B2PGM 
00777      ELSE                                                         G7B2PGM 
00778          MOVE  -1        TO  S2AECOIL                             G7B2PGM 
00779          MOVE  DFHBMUBF  TO  S2AECOIA                             G7B2PGM 
00780          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00781          THEN                                                     G7B2PGM 
00782              NEXT SENTENCE                                        G7B2PGM 
00783          ELSE                                                     G7B2PGM 
00784              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00785              SET WT-01-INDEX TO +08                               G7B2PGM 
00786              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00787                                                                   G7B2PGM 
00788                                                                   G7B2PGM 
00789 *-- VALIDATE ------ DRUG ELIGIBLE MEMBER OVERRIDE IND -----------*G7B2PGM 
00790 *   1. ALPHANUMERIC                                               G7B2PGM 
00791 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00792                                                                   G7B2PGM 
00793      MOVE  S2DECOII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00794      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00795      THEN                                                         G7B2PGM 
00796          MOVE  S2DECOII TO GCVI-VALUE                             G7B2PGM 
00797          MOVE  'BPAB04' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00798          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00799          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00800          THEN                                                     G7B2PGM 
00801              MOVE  -1        TO  S2DECOIL                         G7B2PGM 
00802              MOVE  DFHBMUBF  TO  S2DECOIA                         G7B2PGM 
00803              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00804              THEN                                                 G7B2PGM 
00805                  NEXT SENTENCE                                    G7B2PGM 
00806              ELSE                                                 G7B2PGM 
00807                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00808                  SET WT-01-INDEX TO +09                           G7B2PGM 
00809                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00810          ELSE                                                     G7B2PGM 
00811              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00812              THEN                                                 G7B2PGM 
00813                  MOVE  DFHBMUBF  TO  S2DECOIA                     G7B2PGM 
00814              ELSE                                                 G7B2PGM 
00815                  NEXT SENTENCE                                    G7B2PGM 
00816      ELSE                                                         G7B2PGM 
00817          MOVE  -1        TO  S2DECOIL                             G7B2PGM 
00818          MOVE  DFHBMUBF  TO  S2DECOIA                             G7B2PGM 
00819          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00820          THEN                                                     G7B2PGM 
00821              NEXT SENTENCE                                        G7B2PGM 
00822          ELSE                                                     G7B2PGM 
00823              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00824              SET WT-01-INDEX TO +08                               G7B2PGM 
00825              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00826                                                                   G7B2PGM 
00827                                                                   G7B2PGM 
00828 *-- VALIDATE ------ ECF/SNF OVERRIDE INDICATOR ------------------*G7B2PGM 
00829 *   1. ALPHANUMERIC                                               G7B2PGM 
00830 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00831                                                                   G7B2PGM 
00832      MOVE  S2ESOVII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00833      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00834      THEN                                                         G7B2PGM 
00835          MOVE  S2ESOVII TO GCVI-VALUE                             G7B2PGM 
00836          MOVE  'BPAB05' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00837          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00838          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00839          THEN                                                     G7B2PGM 
00840              MOVE  -1        TO  S2ESOVIL                         G7B2PGM 
00841              MOVE  DFHBMUBF  TO  S2ESOVIA                         G7B2PGM 
00842              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00843              THEN                                                 G7B2PGM 
00844                  NEXT SENTENCE                                    G7B2PGM 
00845              ELSE                                                 G7B2PGM 
00846                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00847                  SET WT-01-INDEX TO +09                           G7B2PGM 
00848                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00849          ELSE                                                     G7B2PGM 
00850              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00851              THEN                                                 G7B2PGM 
00852                  MOVE  DFHBMUBF  TO  S2ESOVIA                     G7B2PGM 
00853              ELSE                                                 G7B2PGM 
00854                  NEXT SENTENCE                                    G7B2PGM 
00855      ELSE                                                         G7B2PGM 
00856          MOVE  -1        TO  S2ESOVIL                             G7B2PGM 
00857          MOVE  DFHBMUBF  TO  S2ESOVIA                             G7B2PGM 
00858          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00859          THEN                                                     G7B2PGM 
00860              NEXT SENTENCE                                        G7B2PGM 
00861          ELSE                                                     G7B2PGM 
00862              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00863              SET WT-01-INDEX TO +08                               G7B2PGM 
00864              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00865                                                                   G7B2PGM 
00866                                                                   G7B2PGM 
00867 *-- VALIDATE ------ NORMAL NEWBORN OVERRIDE IND -----------------*G7B2PGM 
00868 *   1. ALPHANUMERIC                                               G7B2PGM 
00869 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00870                                                                   G7B2PGM 
00871      MOVE  S2NNOVII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00872      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00873      THEN                                                         G7B2PGM 
00874          MOVE  S2NNOVII TO GCVI-VALUE                             G7B2PGM 
00875          MOVE  'BPAB05' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00876          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00877          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00878          THEN                                                     G7B2PGM 
00879              MOVE  -1        TO  S2NNOVIL                         G7B2PGM 
00880              MOVE  DFHBMUBF  TO  S2NNOVIA                         G7B2PGM 
00881              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00882              THEN                                                 G7B2PGM 
00883                  NEXT SENTENCE                                    G7B2PGM 
00884              ELSE                                                 G7B2PGM 
00885                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00886                  SET WT-01-INDEX TO +09                           G7B2PGM 
00887                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00888          ELSE                                                     G7B2PGM 
00889              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00890              THEN                                                 G7B2PGM 
00891                  MOVE  DFHBMUBF  TO  S2NNOVIA                     G7B2PGM 
00892              ELSE                                                 G7B2PGM 
00893                  NEXT SENTENCE                                    G7B2PGM 
00894      ELSE                                                         G7B2PGM 
00895          MOVE  -1        TO  S2NNOVIL                             G7B2PGM 
00896          MOVE  DFHBMUBF  TO  S2NNOVIA                             G7B2PGM 
00897          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00898          THEN                                                     G7B2PGM 
00899              NEXT SENTENCE                                        G7B2PGM 
00900          ELSE                                                     G7B2PGM 
00901              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00902              SET WT-01-INDEX TO +08                               G7B2PGM 
00903              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00904                                                                   G7B2PGM 
00905                                                                   G7B2PGM 
00906 *-- VALIDATE ------ MAX AMOUNT PER VISIT ------------------------*G7B2PGM 
00907 *  D129     CONVERSION                                            G7B2PGM 
00908                                                                   G7B2PGM 
00909      MOVE S2MAXATI TO D-C-RECEIVE-FIELD.                          G7B2PGM 
00910      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7B2PGM 
00911      MOVE '00' TO D-C-RETURN-CODE.                                G7B2PGM 
00912      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7B2PGM 
00913      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7B2PGM 
00914      IF D-C-RETURN-CODE = '00'                                    G7B2PGM 
00915          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7B2PGM 
00916              MOVE -1       TO S2MAXATL                            G7B2PGM 
00917              MOVE DFHBMUBF TO S2MAXATA                            G7B2PGM 
00918              IF WS-02-SCREEN-HAS-ERRORS                           G7B2PGM 
00919                  NEXT SENTENCE                                    G7B2PGM 
00920              ELSE                                                 G7B2PGM 
00921                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7B2PGM 
00922                  SET WT-01-INDEX TO +3                            G7B2PGM 
00923                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00924          ELSE                                                     G7B2PGM 
00925              MOVE D-C-RETURN-FIELD-DEC2                           G7B2PGM 
00926                TO WS-02-MAX-AMT-PER-VISIT                         G7B2PGM 
00927              MOVE WS-02-MAX-AMT-PER-VISIT                         G7B2PGM 
00928                TO WS-02-DISP-5POS-DEC                             G7B2PGM 
00929              MOVE WS-02-DISP-5POS-DEC                             G7B2PGM 
00930                TO S2MAXATO                                        G7B2PGM 
00931      ELSE                                                         G7B2PGM 
00932          MOVE -1       TO S2MAXATL                                G7B2PGM 
00933          MOVE DFHBMUBF TO S2MAXATA                                G7B2PGM 
00934          IF WS-02-SCREEN-HAS-ERRORS                               G7B2PGM 
00935              NEXT SENTENCE                                        G7B2PGM 
00936          ELSE                                                     G7B2PGM 
00937              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7B2PGM 
00938              IF D-C-RETURN-CODE = '10'                            G7B2PGM 
00939                  SET WT-01-INDEX TO +10                           G7B2PGM 
00940                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00941              ELSE                                                 G7B2PGM 
00942                  SET WT-01-INDEX TO +2                            G7B2PGM 
00943                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7B2PGM 
00944                                                                   G7B2PGM 
00945 *    IF  S2MAXATI IS NUMERIC                                      G7B2PGM 
00946 *    THEN                                                         G7B2PGM 
00947 *        NEXT SENTENCE                                            G7B2PGM 
00948 *    ELSE                                                         G7B2PGM 
00949 *        MOVE  -1        TO  S2MAXATL                             G7B2PGM 
00950 *        MOVE  DFHBMUBF  TO  S2MAXATA                             G7B2PGM 
00951 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00952 *        THEN                                                     G7B2PGM 
00953 *            NEXT SENTENCE                                        G7B2PGM 
00954 *        ELSE                                                     G7B2PGM 
00955 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00956 *            SET WT-01-INDEX TO +10                               G7B2PGM 
00957 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00958                                                                   G7B2PGM 
00959                                                                   G7B2PGM 
00960 *-- VALIDATE ------ TRANS-SEXUAL RESTR. OVERRIDE IND ------------*G7B2PGM 
00961 *   1. ALPHANUMERIC                                               G7B2PGM 
00962 *   2. FIELD VALIDATION SUB-SYSTEM                                G7B2PGM 
00963                                                                   G7B2PGM 
00964      MOVE  S2TXPRII TO WS-02-CLASS-TEST-AREA.                     G7B2PGM 
00965      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7B2PGM 
00966      THEN                                                         G7B2PGM 
00967          MOVE  S2TXPRII TO GCVI-VALUE                             G7B2PGM 
00968          MOVE  'BPAB04' TO GCVI-FIELDS-KEY-ID                     G7B2PGM 
00969          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7B2PGM 
00970          IF  GCVI-VALUE-NOT-FOUND                                 G7B2PGM 
00971          THEN                                                     G7B2PGM 
00972              MOVE  -1        TO  S2TXPRIL                         G7B2PGM 
00973              MOVE  DFHBMUBF  TO  S2TXPRIA                         G7B2PGM 
00974              IF  WS-02-SCREEN-HAS-ERRORS                          G7B2PGM 
00975              THEN                                                 G7B2PGM 
00976                  NEXT SENTENCE                                    G7B2PGM 
00977              ELSE                                                 G7B2PGM 
00978                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
00979                  SET WT-01-INDEX TO +09                           G7B2PGM 
00980                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
00981          ELSE                                                     G7B2PGM 
00982              IF  GCVI-VALUE-NOT-LOADED                            G7B2PGM 
00983              THEN                                                 G7B2PGM 
00984                  MOVE  DFHBMUBF  TO  S2TXPRIA                     G7B2PGM 
00985              ELSE                                                 G7B2PGM 
00986                  NEXT SENTENCE                                    G7B2PGM 
00987      ELSE                                                         G7B2PGM 
00988          MOVE  -1        TO  S2TXPRIL                             G7B2PGM 
00989          MOVE  DFHBMUBF  TO  S2TXPRIA                             G7B2PGM 
00990          IF  WS-02-SCREEN-HAS-ERRORS                              G7B2PGM 
00991          THEN                                                     G7B2PGM 
00992              NEXT SENTENCE                                        G7B2PGM 
00993          ELSE                                                     G7B2PGM 
00994              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7B2PGM 
00995              SET WT-01-INDEX TO +08                               G7B2PGM 
00996              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7B2PGM 
00997                                                                   G7B2PGM 
00998                                                                   G7B2PGM 
00999                                                                   G7B2PGM 
01000  2100-900-EXIT.                                                   G7B2PGM 
01001      EXIT.                                                        G7B2PGM 
01002 /***************************************************************  G7B2PGM 
01003 *                                                              *  G7B2PGM 
01004 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7B2PGM 
01005 *                                                              *  G7B2PGM 
01006 ****************************************************************  G7B2PGM 
01007  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7B2PGM 
01008  2110-010.                                                        G7B2PGM 
01009                                                                   G7B2PGM 
01010      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7B2PGM 
01011                                                                   G7B2PGM 
01012      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7B2PGM 
01013                       COMMAREA(GCVIOPGM-PARM-LIST)                G7B2PGM 
01014                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7B2PGM 
01015                       END-EXEC.                                   G7B2PGM 
01016                                                                   G7B2PGM 
01017      IF  GCVI-VALUE-NOT-LOADED                                    G7B2PGM 
01018          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7B2PGM 
01019                                                                   G7B2PGM 
01020  2110-900-EXIT.                                                   G7B2PGM 
01021      EXIT.                                                        G7B2PGM 
01022 /***************************************************************  G7B2PGM 
01023 *                                                              *  G7B2PGM 
01024 * 2200  DO SCREEN LOGICAL EDITS                                *  G7B2PGM 
01025 *                                                              *  G7B2PGM 
01026 ****************************************************************  G7B2PGM 
01027  2200-000-LOGICAL-EDITS         SECTION.                          G7B2PGM 
01028  2200-010.                                                        G7B2PGM 
01029                                                                   G7B2PGM 
01030 *----------------------------------------------------------------*G7B2PGM 
01031 *                                                                *G7B2PGM 
01032 *                                                                *G7B2PGM 
01033 *   THIS PROGRAM HAS NO REQUIREMENT FOR LOGICAL EDITS            *G7B2PGM 
01034 *                                                                *G7B2PGM 
01035 *                                                                *G7B2PGM 
01036 *----------------------------------------------------------------*G7B2PGM 
01037                                                                   G7B2PGM 
01038                                                                   G7B2PGM 
01039 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7B2PGM 
01040                                                                   G7B2PGM 
01041      IF  WS-02-SCREEN-HAS-ERRORS                                  G7B2PGM 
01042      THEN                                                         G7B2PGM 
01043          NEXT SENTENCE                                            G7B2PGM 
01044      ELSE                                                         G7B2PGM 
01045          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7B2PGM 
01046          THEN                                                     G7B2PGM 
01047              IF EIBAID = DFHPF4 OR DFHPF16                        G7B2PGM 
01048              THEN                                                 G7B2PGM 
01049                  NEXT SENTENCE                                    G7B2PGM 
01050              ELSE                                                 G7B2PGM 
01051                  MOVE  -1        TO S2ERRL                        G7B2PGM 
01052                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7B2PGM 
01053                  SET WT-01-INDEX TO +06                           G7B2PGM 
01054                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7B2PGM 
01055          ELSE                                                     G7B2PGM 
01056              NEXT SENTENCE.                                       G7B2PGM 
01057                                                                   G7B2PGM 
01058                                                                   G7B2PGM 
01059  2200-900-EXIT.                                                   G7B2PGM 
01060      EXIT.                                                        G7B2PGM 
01061 /***************************************************************  G7B2PGM 
01062 *                                                              *  G7B2PGM 
01063 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7B2PGM 
01064 *        REWRITE TO WORKFILE.                                  *  G7B2PGM 
01065 *                                                              *  G7B2PGM 
01066 ****************************************************************  G7B2PGM 
01067  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7B2PGM 
01068  2300-010.                                                        G7B2PGM 
01069                                                                   G7B2PGM 
01070 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7B2PGM 
01071                                                                   G7B2PGM 
01072      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7B2PGM 
01073      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7B2PGM 
01074        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7B2PGM 
01075      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7B2PGM 
01076      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7B2PGM 
01077      IF  NOT GCIO2-GOOD-RETURN                                    G7B2PGM 
01078          MOVE WS-01-ABCODE-B2F2     TO WS-01-ABCODE               G7B2PGM 
01079          MOVE WS-01-ABCODE-B2F2-MSG TO WS-01-ABCODE-MSG           G7B2PGM 
01080          PERFORM  9999-000-ABEND-THE-TASK.                        G7B2PGM 
01081                                                                   G7B2PGM 
01082                                                                   G7B2PGM 
01083 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7B2PGM 
01084                                                                   G7B2PGM 
01085 *    MOVE S2MAXATI TO WS-02-MAX-AMT-PER-VISIT-X.                  G7B2PGM 
01086                                                                   G7B2PGM 
01087      MOVE GPB2-MAX-AMT-PER-VISIT     TO                           G7B2PGM 
01088          WS-GPB2-MAX-AMT-PER-VISIT.                               G7B2PGM 
01089                                                                   G7B2PGM 
01090      IF   GPB2-PHYS-EXAM-IND            = S2PHEXII AND            G7B2PGM 
01091           GPB2-PROF-CHRG-HSP-CLM        = S2PCOHCI AND            G7B2PGM 
01092           GPB2-AMBULANCE-ELIG-IND       = S2AMBEII AND            G7B2PGM 
01093           GPB2-ALCO-ELIG-MEMB-CLS-OVRD  = S2AECOII AND            G7B2PGM 
01094           GPB2-DRUG-ELIG-MEMB-CLS-OVRD  = S2DECOII AND            G7B2PGM 
01095           GPB2-ECF-SNF-OVRD-IND         = S2ESOVII AND            G7B2PGM 
01096           GPB2-NORM-NWBORN-OVRD-IND     = S2NNOVII AND            G7B2PGM 
01097        WS-GPB2-MAX-AMT-PER-VISIT        =                         G7B2PGM 
01098                            WS-02-MAX-AMT-PER-VISIT AND            G7B2PGM 
01099           GPB2-TRANSSXL-PMT-RESTR-OVRD  = S2TXPRII                G7B2PGM 
01100      THEN                                                         G7B2PGM 
01101          GO TO 2300-900-EXIT                                      G7B2PGM 
01102      ELSE                                                         G7B2PGM 
01103          NEXT SENTENCE.                                           G7B2PGM 
01104                                                                   G7B2PGM 
01105                                                                   G7B2PGM 
01106 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7B2PGM 
01107                                                                   G7B2PGM 
01108      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7B2PGM 
01109      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7B2PGM 
01110      IF  NOT GCIO2-GOOD-RETURN                                    G7B2PGM 
01111          MOVE WS-01-ABCODE-B2F3     TO WS-01-ABCODE               G7B2PGM 
01112          MOVE WS-01-ABCODE-B2F3-MSG TO WS-01-ABCODE-MSG           G7B2PGM 
01113          PERFORM  9999-000-ABEND-THE-TASK.                        G7B2PGM 
01114                                                                   G7B2PGM 
01115                                                                   G7B2PGM 
01116 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7B2PGM 
01117                                                                   G7B2PGM 
01118      MOVE S2PHEXII TO GPB2-PHYS-EXAM-IND.                         G7B2PGM 
01119      MOVE S2PCOHCI TO GPB2-PROF-CHRG-HSP-CLM.                     G7B2PGM 
01120      MOVE S2AMBEII TO GPB2-AMBULANCE-ELIG-IND.                    G7B2PGM 
01121      MOVE S2AECOII TO GPB2-ALCO-ELIG-MEMB-CLS-OVRD.               G7B2PGM 
01122      MOVE S2DECOII TO GPB2-DRUG-ELIG-MEMB-CLS-OVRD.               G7B2PGM 
01123      MOVE S2ESOVII TO GPB2-ECF-SNF-OVRD-IND.                      G7B2PGM 
01124      MOVE S2NNOVII TO GPB2-NORM-NWBORN-OVRD-IND.                  G7B2PGM 
01125      MOVE WS-02-MAX-AMT-PER-VISIT                                 G7B2PGM 
01126                    TO GPB2-MAX-AMT-PER-VISIT.                     G7B2PGM 
01127      MOVE S2TXPRII TO GPB2-TRANSSXL-PMT-RESTR-OVRD.               G7B2PGM 
01128                                                                   G7B2PGM 
01129                                                                   G7B2PGM 
01130 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7B2PGM 
01131                                                                   G7B2PGM 
01132 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7B2PGM 
01133                                                                   G7B2PGM 
01134      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7B2PGM 
01135      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7B2PGM 
01136      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7B2PGM 
01137      IF  NOT GCIO2-GOOD-RETURN                                    G7B2PGM 
01138          MOVE WS-01-ABCODE-B2F4     TO WS-01-ABCODE               G7B2PGM 
01139          MOVE WS-01-ABCODE-B2F4-MSG TO WS-01-ABCODE-MSG           G7B2PGM 
01140          PERFORM  9999-000-ABEND-THE-TASK.                        G7B2PGM 
01141                                                                   G7B2PGM 
01142  2300-900-EXIT.                                                   G7B2PGM 
01143      EXIT.                                                        G7B2PGM 
01144 /***************************************************************  G7B2PGM 
01145 *                                                              *  G7B2PGM 
01146 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7B2PGM 
01147 *                                                              *  G7B2PGM 
01148 ****************************************************************  G7B2PGM 
01149  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7B2PGM 
01150  2310-010.                                                        G7B2PGM 
01151                                                                   G7B2PGM 
01152                                                                   G7B2PGM 
01153 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7B2PGM 
01154                                                                   G7B2PGM 
01155      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7B2PGM 
01156                                         + GC-WORKFILE-KEY-LEN     G7B2PGM 
01157                                         + GC-GCBENPRV-MAX-REC-LEN.G7B2PGM 
01158                                                                   G7B2PGM 
01159      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7B2PGM 
01160                          INITIMG(WS-02-HEX-00)                    G7B2PGM 
01161                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7B2PGM 
01162                          END-EXEC.                                G7B2PGM 
01163                                                                   G7B2PGM 
01164 *    SERVICE RELOAD  IO-PARM-BEN-PROV-AREA.                       G7B2PGM 
01165                                                                   G7B2PGM 
01166 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7B2PGM 
01167                                                                   G7B2PGM 
01168      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7B2PGM 
01169                                                                   G7B2PGM 
01170      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7B2PGM 
01171      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7B2PGM 
01172      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7B2PGM 
01173      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7B2PGM 
01174      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7B2PGM 
01175      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7B2PGM 
01176                                                                   G7B2PGM 
01177      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7B2PGM 
01178      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7B2PGM 
01179      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7B2PGM 
01180      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7B2PGM 
01181      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7B2PGM 
01182      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7B2PGM 
01183      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7B2PGM 
01184      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7B2PGM 
01185                                                                   G7B2PGM 
01186 ***  MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7B2PGM 
01187 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7B2PGM 
01188 ***  IF  HGADATE-RETURN = ZEROS                                   G7B2PGM 
01189 ***  THEN                                                         G7B2PGM 
01190 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7B2PGM 
01191 ***  ELSE                                                         G7B2PGM 
01192 ***      SET WT-01-INDEX TO +07                                   G7B2PGM 
01193 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7B2PGM 
01194 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7B2PGM 
01195                                                                   G7B2PGM 
01196      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7B2PGM 
01197      MOVE S2BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7B2PGM 
01198      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7B2PGM 
01199      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7B2PGM 
01200      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7B2PGM 
01201      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7B2PGM 
01202      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7B2PGM 
01203                                                                   G7B2PGM 
01204                                                                   G7B2PGM 
01205  2310-900-EXIT.                                                   G7B2PGM 
01206      EXIT.                                                        G7B2PGM 
01207 /***************************************************************  G7B2PGM 
01208 *                                                              *  G7B2PGM 
01209 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7B2PGM 
01210 *                                                              *  G7B2PGM 
01211 ****************************************************************  G7B2PGM 
01212  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7B2PGM 
01213  2400-010.                                                        G7B2PGM 
01214                                                                   G7B2PGM 
01215                                                                   G7B2PGM 
01216      IF  EIBAID = DFHPF7  OR DFHPF19                              G7B2PGM 
01217      THEN                                                         G7B2PGM 
01218          MOVE 'G7B1PGM' TO WS-02-NEXT-PROGRAM.                    G7B2PGM 
01219                                                                   G7B2PGM 
01220      IF  EIBAID = DFHENTER OR                                     G7B2PGM 
01221                   DFHPF4   OR DFHPF16 OR                          G7B2PGM 
01222                   DFHPF8   OR DFHPF20                             G7B2PGM 
01223      THEN                                                         G7B2PGM 
01224          MOVE 'GC6APGM' TO WS-02-NEXT-PROGRAM.                    G7B2PGM 
01225                                                                   G7B2PGM 
01226      IF  EIBAID = DFHPF6  OR DFHPF18                              G7B2PGM 
01227      THEN                                                         G7B2PGM 
01228          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7B2PGM 
01229                                                                   G7B2PGM 
01230                                                                   G7B2PGM 
01231      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7B2PGM 
01232                       COMMAREA(WORK-RECORD-2)                     G7B2PGM 
01233                       LENGTH  (GCIO2-RECORD-LENGTH)               G7B2PGM 
01234                       END-EXEC.                                   G7B2PGM 
01235                                                                   G7B2PGM 
01236  2400-900-EXIT.                                                   G7B2PGM 
01237      EXIT.                                                        G7B2PGM 
01238 /***************************************************************  G7B2PGM 
01239 *                                                              *  G7B2PGM 
01240 * 2500   LINK TO GX3APGM FOR CONVERSION.                       *  G7B2PGM 
01241 *                                                              *  G7B2PGM 
01242 ****************************************************************  G7B2PGM 
01243  2500-LINK-TO-GX3APGM.                                            G7B2PGM 
01244                                                                   G7B2PGM 
01245      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7B2PGM 
01246                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7B2PGM 
01247                       LENGTH  (+51)                               G7B2PGM 
01248                       END-EXEC.                                   G7B2PGM 
01249                                                                   G7B2PGM 
01250  2500-EXIT.                                                       G7B2PGM 
01251      EXIT.                                                        G7B2PGM 
01252 /***************************************************************  G7B2PGM 
01253 *                                                              *  G7B2PGM 
01254 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7B2PGM 
01255 *         PROVISION RECORD (TYPE=C4)                           *  G7B2PGM 
01256 *                                                              *  G7B2PGM 
01257 ****************************************************************  G7B2PGM 
01258  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7B2PGM 
01259  5000-010.                                                        G7B2PGM 
01260                                                                   G7B2PGM 
01261      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7B2PGM 
01262                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7B2PGM 
01263                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7B2PGM 
01264                       END-EXEC.                                   G7B2PGM 
01265                                                                   G7B2PGM 
01266                                                                   G7B2PGM 
01267  5000-900-EXIT.                                                   G7B2PGM 
01268      EXIT.                                                        G7B2PGM 
01269 /***************************************************************  G7B2PGM 
01270 *                                                              *  G7B2PGM 
01271 * 5100                                                         *  G7B2PGM 
01272 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7B2PGM 
01273 *                                                              *  G7B2PGM 
01274 ****************************************************************  G7B2PGM 
01275  5100-000-W-F-CONTRACT-IO       SECTION.                          G7B2PGM 
01276  5100-010.                                                        G7B2PGM 
01277                                                                   G7B2PGM 
01278      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7B2PGM 
01279                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7B2PGM 
01280                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7B2PGM 
01281                       END-EXEC.                                   G7B2PGM 
01282                                                                   G7B2PGM 
01283                                                                   G7B2PGM 
01284  5100-900-EXIT.                                                   G7B2PGM 
01285      EXIT.                                                        G7B2PGM 
01286 /***************************************************************  G7B2PGM 
01287 *                                                              *  G7B2PGM 
01288 * 9000   MOVE MESSAGE TO SCREEN                                *  G7B2PGM 
01289 *                                                              *  G7B2PGM 
01290 ****************************************************************  G7B2PGM 
01291  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7B2PGM 
01292  9000-010.                                                        G7B2PGM 
01293                                                                   G7B2PGM 
01294      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S2ERRO.              G7B2PGM 
01295                                                                   G7B2PGM 
01296  9000-900-EXIT.                                                   G7B2PGM 
01297      EXIT.                                                        G7B2PGM 
01298 /***************************************************************  G7B2PGM 
01299 *                                                              *  G7B2PGM 
01300 * 9100 SEND SCREEN AND RETURN                                  *  G7B2PGM 
01301 *                                                              *  G7B2PGM 
01302 ****************************************************************  G7B2PGM 
01303  9100-000-SEND-THEN-RETURN      SECTION.                          G7B2PGM 
01304  9100-010.                                                        G7B2PGM 
01305                                                                   G7B2PGM 
01306                                                                   G7B2PGM 
01307 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7B2PGM 
01308      MOVE  -1 TO  S2ERRL.                                         G7B2PGM 
01309                                                                   G7B2PGM 
01310                                                                   G7B2PGM 
01311      IF  WS-02-MY-EIBTRNID                                        G7B2PGM 
01312      THEN                                                         G7B2PGM 
01313          EXEC CICS  SEND MAP   ('G7B2I01')                        G7B2PGM 
01314                          MAPSET('G7B2SET')                        G7B2PGM 
01315                          DATAONLY                                 G7B2PGM 
01316                          CURSOR                                   G7B2PGM 
01317                          END-EXEC                                 G7B2PGM 
01318      ELSE                                                         G7B2PGM 
01319          EXEC CICS  SEND MAP   ('G7B2I01')                        G7B2PGM 
01320                          MAPSET('G7B2SET')                        G7B2PGM 
01321                          ERASE                                    G7B2PGM 
01322                          CURSOR                                   G7B2PGM 
01323                          END-EXEC.                                G7B2PGM 
01324                                                                   G7B2PGM 
01325                                                                   G7B2PGM 
01326      EXEC CICS RETURN                                             G7B2PGM 
01327                TRANSID  ('G7B2')                                  G7B2PGM 
01328                COMMAREA (DFHCOMMAREA)                             G7B2PGM 
01329                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7B2PGM 
01330                END-EXEC.                                          G7B2PGM 
01331 *                                                                 G7B2PGM 
01332  9100-900-EXIT.                                                   G7B2PGM 
01333      EXIT.                                                        G7B2PGM 
01334 /*****************************************************************G7B2PGM 
01335 *                                                                *G7B2PGM 
01336 * 9200    XCTL TO GCPSPGM                                        *G7B2PGM 
01337 *                                                                *G7B2PGM 
01338 *                                                                *G7B2PGM 
01339 ******************************************************************G7B2PGM 
01340  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7B2PGM 
01341  9200-010.                                                        G7B2PGM 
01342                                                                   G7B2PGM 
01343      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7B2PGM 
01344                       END-EXEC.                                   G7B2PGM 
01345                                                                   G7B2PGM 
01346  9200-900-EXIT.                                                   G7B2PGM 
01347      EXIT.                                                        G7B2PGM 
01348 /*****************************************************************G7B2PGM 
01349 *                                                                *G7B2PGM 
01350 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7B2PGM 
01351 *                                                                *G7B2PGM 
01352 *                                                                *G7B2PGM 
01353 ******************************************************************G7B2PGM 
01354  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7B2PGM 
01355  9210-010.                                                        G7B2PGM 
01356                                                                   G7B2PGM 
01357      IF  S2GRPNOI = '000SPS000'                                   G7B2PGM 
01358          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7B2PGM 
01359                           END-EXEC.                               G7B2PGM 
01360                                                                   G7B2PGM 
01361 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7B2PGM 
01362                                                                   G7B2PGM 
01363      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7B2PGM 
01364                                        + GC-WORKFILE-KEY-LEN      G7B2PGM 
01365                                        + GC-GCCONTR-MAX-REC-LEN.  G7B2PGM 
01366                                                                   G7B2PGM 
01367      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7B2PGM 
01368                          INITIMG(WS-02-HEX-00)                    G7B2PGM 
01369                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7B2PGM 
01370                          END-EXEC.                                G7B2PGM 
01371                                                                   G7B2PGM 
01372 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7B2PGM 
01373 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7B2PGM 
01374                                                                   G7B2PGM 
01375 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7B2PGM 
01376                                                                   G7B2PGM 
01377      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7B2PGM 
01378        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7B2PGM 
01379                                                                   G7B2PGM 
01380      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7B2PGM 
01381      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7B2PGM 
01382                                                                   G7B2PGM 
01383      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7B2PGM 
01384      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7B2PGM 
01385      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7B2PGM 
01386      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7B2PGM 
01387      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7B2PGM 
01388      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7B2PGM 
01389                                                                   G7B2PGM 
01390      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7B2PGM 
01391      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7B2PGM 
01392      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7B2PGM 
01393      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7B2PGM 
01394      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7B2PGM 
01395      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7B2PGM 
01396      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7B2PGM 
01397      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7B2PGM 
01398                                                                   G7B2PGM 
01399 ***  MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7B2PGM 
01400 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7B2PGM 
01401 ***  IF  HGADATE-RETURN = ZEROS                                   G7B2PGM 
01402 ***  THEN                                                         G7B2PGM 
01403 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7B2PGM 
01404 ***  ELSE                                                         G7B2PGM 
01405 ***      SET WT-01-INDEX TO +07                                   G7B2PGM 
01406 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7B2PGM 
01407 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7B2PGM 
01408                                                                   G7B2PGM 
01409      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7B2PGM 
01410      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7B2PGM 
01411      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7B2PGM 
01412      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7B2PGM 
01413      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7B2PGM 
01414      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7B2PGM 
01415      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7B2PGM 
01416                                                                   G7B2PGM 
01417      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7B2PGM 
01418                                                                   G7B2PGM 
01419      IF  NOT GCIO3-GOOD-RETURN                                    G7B2PGM 
01420          MOVE WS-01-ABCODE-B2F1     TO WS-01-ABCODE               G7B2PGM 
01421          MOVE WS-01-ABCODE-B2F1-MSG TO WS-01-ABCODE-MSG           G7B2PGM 
01422          PERFORM  9999-000-ABEND-THE-TASK.                        G7B2PGM 
01423                                                                   G7B2PGM 
01424      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7B2PGM 
01425                       COMMAREA(WORK-RECORD-3)                     G7B2PGM 
01426                       LENGTH  (GCIO3-RECORD-LENGTH)               G7B2PGM 
01427                       END-EXEC.                                   G7B2PGM 
01428                                                                   G7B2PGM 
01429  9210-900-EXIT.                                                   G7B2PGM 
01430      EXIT.                                                        G7B2PGM 
01431 /*****************************************************************G7B2PGM 
01432 *                                                                *G7B2PGM 
01433 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7B2PGM 
01434 *                                                                *G7B2PGM 
01435 *                                                                *G7B2PGM 
01436 ******************************************************************G7B2PGM 
01437  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7B2PGM 
01438  9220-010.                                                        G7B2PGM 
01439                                                                   G7B2PGM 
01440      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7B2PGM 
01441                       END-EXEC.                                   G7B2PGM 
01442                                                                   G7B2PGM 
01443  9220-900-EXIT.                                                   G7B2PGM 
01444      EXIT.                                                        G7B2PGM 
01445 /*****************************************************************G7B2PGM 
01446 *                                                                *G7B2PGM 
01447 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7B2PGM 
01448 *                                                                *G7B2PGM 
01449 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7B2PGM 
01450 *                                                                *G7B2PGM 
01451 ******************************************************************G7B2PGM 
01452  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7B2PGM 
01453  9800-010.                                                        G7B2PGM 
01454                                                                   G7B2PGM 
01455      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7B2PGM 
01456      MOVE 'M'   TO  HGADATE-FORM1.                                G7B2PGM 
01457      MOVE 'J'   TO  HGADATE-FORM2.                                G7B2PGM 
01458      MOVE ZEROS TO  HGADATE-RETURN                                G7B2PGM 
01459                     HGADATE-AMOUNT.                               G7B2PGM 
01460      EXEC CICS LINK PROGRAM ('HGADATES')                          G7B2PGM 
01461                     COMMAREA(HGADATES-COMMAREA)                   G7B2PGM 
01462                     LENGTH  (24)                                  G7B2PGM 
01463                     END-EXEC.                                     G7B2PGM 
01464                                                                   G7B2PGM 
01465  9800-900-900-EXIT.                                               G7B2PGM 
01466      EXIT.                                                        G7B2PGM 
01467 /*****************************************************************G7B2PGM 
01468 *                                                                *G7B2PGM 
01469 * 9810    J U L I A N    T O    G R E G O R I A N                *G7B2PGM 
01470 *                                                                *G7B2PGM 
01471 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7B2PGM 
01472 *                                                                *G7B2PGM 
01473 ******************************************************************G7B2PGM 
01474  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7B2PGM 
01475  9810-010.                                                        G7B2PGM 
01476                                                                   G7B2PGM 
01477      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7B2PGM 
01478      MOVE 'J'   TO  HGADATE-FORM1.                                G7B2PGM 
01479      MOVE 'M'   TO  HGADATE-FORM2.                                G7B2PGM 
01480      MOVE ZEROS TO  HGADATE-RETURN                                G7B2PGM 
01481                     HGADATE-AMOUNT.                               G7B2PGM 
01482      EXEC CICS LINK PROGRAM ('HGADATES')                          G7B2PGM 
01483                     COMMAREA(HGADATES-COMMAREA)                   G7B2PGM 
01484                     LENGTH  (24)                                  G7B2PGM 
01485                     END-EXEC.                                     G7B2PGM 
01486                                                                   G7B2PGM 
01487  9810-900-900-EXIT.                                               G7B2PGM 
01488      EXIT.                                                        G7B2PGM 
01489 /***************************************************************  G7B2PGM 
01490 *                                                              *  G7B2PGM 
01491 * 9999  ABEND THE TASK                                         *  G7B2PGM 
01492 *                                                              *  G7B2PGM 
01493 ****************************************************************  G7B2PGM 
01494  9999-000-ABEND-THE-TASK SECTION.                                 G7B2PGM 
01495  9999-010.                                                        G7B2PGM 
01496                                                                   G7B2PGM 
01497      EXEC CICS  ABEND                                             G7B2PGM 
01498                 ABCODE(WS-01-ABCODE)                              G7B2PGM 
01499                 END-EXEC.                                         G7B2PGM 
01500                                                                   G7B2PGM 
01501  9900-900-EXIT.                                                   G7B2PGM 
01502      EXIT.                                                        G7B2PGM 
