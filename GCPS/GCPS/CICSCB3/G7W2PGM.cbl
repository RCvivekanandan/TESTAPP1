00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7W2PGM.                                         G7W2PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7W2PGM 
00005  DATE-WRITTEN.   03/16/87.                                        G7W2PGM 
00006  DATE-COMPILED.                                                   G7W2PGM 
00007 ***************************************************************** G7W2PGM 
00008 *                                                               * G7W2PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7W2PGM 
00010 *                                                               * G7W2PGM 
00011 *                                                               * G7W2PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7W2PGM 
00013 *                                                               * G7W2PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7W2PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7W2PGM 
00016 *                             (SAME AS 'GC5A')                  * G7W2PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7W2PGM 
00018 *                              'GC5A')                          * G7W2PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7W2PGM 
00020 *                              PROVISION)                       * G7W2PGM 
00021 *                                                               * G7W2PGM 
00022 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7W2PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7W2PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7W2PGM 
00025 *                                                               * G7W2PGM 
00026 *  D129      08/28/89  GDM    CONVERT FOR DECIMALS.               G7W2PGM 
00027 *                                                               * G7W2PGM 
00028 *  D129      09/08/89  GDM    CONVERT TO VS COBOL/2             * G7W2PGM 
00029 *                                                               * G7W2PGM 
00030 *  D12009    08/23/91  BSO   -CORRECT ERR MESSAGES IN AREA \
00031 *                            -CORRECT ALPHA CLASS TEST AREA     * G7W2PGM 
00032 *                                                               * G7W2PGM 
00033 * 14726/     03/30/98  GSP    ADDED PLAN AND PACKAGE CODE AND   * G7W2PGM 
00034 * 15057                       INCREASED GROUP AND SECTION ON    * G7W2PGM 
00035 *                             THE SCREEN.                       * G7W2PGM 
00036 *                                                               * G7W2PGM 
00037 *            12/11/02  GAKK   OPID COMPILE                      * G7W2PGM 
00038 *                                                               * G7W2PGM 
00039 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7W2PGM 
00040 *                           G7W2SET                              *G7W2PGM 
00041 ***************************************************************** G7W2PGM 
00042                                                                   G7W2PGM 
00043 ***************************************************************** G7W2PGM 
00044 *                                                               * G7W2PGM 
00045 *    G7W2PGM  - PROGRAM 2 OF 2 PROGRAMS TO UPDATE THE FORMAT 'W'* G7W2PGM 
00046 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7W2PGM 
00047 *                                                               * G7W2PGM 
00048 *    TRANSID: G7W2                                              * G7W2PGM 
00049 *    MAPSET:  G7W2SETC    (GIW2PGM WHICH SHARES THIS MAP)       * G7W2PGM 
00050 *    VALGEN:  NONE                                              * G7W2PGM 
00051 *                                                               * G7W2PGM 
00052 *    PROGRAM NARRATIVE:                                         * G7W2PGM 
00053 *                                                               * G7W2PGM 
00054 *        PROGRAM CHECKS FOR TRANS CODE 'G7W2'.  AN INVALID      * G7W2PGM 
00055 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7W2PGM 
00056 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7W2PGM 
00057 *                                                               * G7W2PGM 
00058 *        THE MAIN FUNCTIONS ARE :                               * G7W2PGM 
00059 *        1. HARDCOPY REQUEST,                                   * G7W2PGM 
00060 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7W2PGM 
00061 *           USER,                                               * G7W2PGM 
00062 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7W2PGM 
00063 *                                                               * G7W2PGM 
00064 *        HARDCOPY REQUEST                                       * G7W2PGM 
00065 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7W2PGM 
00066 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7W2PGM 
00067 *           THE SCREEN BUFFER.                                  * G7W2PGM 
00068 *                                                               * G7W2PGM 
00069 *        PROCESS INPUT DATA (UPDATE).                           * G7W2PGM 
00070 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7W2PGM 
00071 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7W2PGM 
00072 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7W2PGM 
00073 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7W2PGM 
00074 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7W2PGM 
00075 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7W2PGM 
00076 *                                                               * G7W2PGM 
00077 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7W2PGM 
00078 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7W2PGM 
00079 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7W2PGM 
00080 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7W2PGM 
00081 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7W2PGM 
00082 *                                                               * G7W2PGM 
00083 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7W2PGM 
00084 *                                     -----------------------   * G7W2PGM 
00085 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7W2PGM 
00086 *           -----------------------------------------------     * G7W2PGM 
00087 *                                                               * G7W2PGM 
00088 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7W2PGM 
00089 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7W2PGM 
00090 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7W2PGM 
00091 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7W2PGM 
00092 *           SENT BACK TO THE USER.                              * G7W2PGM 
00093 *                                                               * G7W2PGM 
00094 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7W2PGM 
00095 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7W2PGM 
00096 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7W2PGM 
00097 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7W2PGM 
00098 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7W2PGM 
00099 *           PF KEY LOGIC,                                       * G7W2PGM 
00100 *              PF6/PF18       GOES TO GC8APGM                   * G7W2PGM 
00101 *              PF8/PF20/ENTER GOES TO GC6APGM                   * G7W2PGM 
00102 *              FOR PF7/PF19   GOES TO G7W1PGM                   * G7W2PGM 
00103 *                                                               * G7W2PGM 
00104 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7W2PGM 
00105 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7W2PGM 
00106 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7W2PGM 
00107 *                                                               * G7W2PGM 
00108 ***************************************************************** G7W2PGM 
00109                                                                   G7W2PGM 
00110  ENVIRONMENT DIVISION.                                            G7W2PGM 
00111  DATA DIVISION.                                                   G7W2PGM 
00112 /                                                                 G7W2PGM 
00113  WORKING-STORAGE SECTION.                                         G7W2PGM 
00114  01  WS-BEGIN                    PIC X(58) VALUE                  G7W2PGM 
00115      '*** G7W2PGM  WORKING-STORAGE BEGINS HERE ***'.              G7W2PGM 
00116                                                                   G7W2PGM 
00117                                                                   G7W2PGM 
00118  01  WS-01-ABEND-AREA.                                            G7W2PGM 
00119      05  FILLER                   PIC X(16)  VALUE                G7W2PGM 
00120          '** ABEND AREA **'.                                      G7W2PGM 
00121                                                                   G7W2PGM 
00122      05  WS-01-ABEND-CODES-AND-MSG.                               G7W2PGM 
00123          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7W2PGM 
00124          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7W2PGM 
00125                                                                   G7W2PGM 
00126          10  WS-01-ABCODE-W2F1          PIC X(04)  VALUE  'W2F1'. G7W2PGM 
00127          10  WS-01-ABCODE-W2F1-MSG      PIC X(44)  VALUE          G7W2PGM 
00128             'W/F CONTRACT CANNOT BE FOUND             '.          G7W2PGM 
00129                                                                   G7W2PGM 
00130          10  WS-01-ABCODE-W2F2          PIC X(04)  VALUE  'W2F2'. G7W2PGM 
00131          10  WS-01-ABCODE-W2F2-MSG      PIC X(44)  VALUE          G7W2PGM 
00132             'W/F BEN PROV CANNOT BE FOUND             '.          G7W2PGM 
00133                                                                   G7W2PGM 
00134          10  WS-01-ABCODE-W2F3          PIC X(04)  VALUE  'W2F3'. G7W2PGM 
00135          10  WS-01-ABCODE-W2F3-MSG      PIC X(44)  VALUE          G7W2PGM 
00136             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7W2PGM 
00137                                                                   G7W2PGM 
00138          10  WS-01-ABCODE-W2F4          PIC X(04)  VALUE  'W2F4'. G7W2PGM 
00139          10  WS-01-ABCODE-W2F4-MSG      PIC X(44)  VALUE          G7W2PGM 
00140             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7W2PGM 
00141                                                                   G7W2PGM 
00142          10  WS-01-ABCODE-W2L1          PIC X(04)  VALUE  'W2L1'. G7W2PGM 
00143          10  WS-01-ABCODE-W2L1-MSG      PIC X(44)  VALUE          G7W2PGM 
00144             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7W2PGM 
00145                                                                   G7W2PGM 
00146          10  WS-01-ABCODE-W2P1          PIC X(04)  VALUE  'W2P1'. G7W2PGM 
00147          10  WS-01-ABCODE-W2P1-MSG      PIC X(44)  VALUE          G7W2PGM 
00148             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7W2PGM 
00149                                                                   G7W2PGM 
00150          10  WS-01-ABCODE-W2P2          PIC X(04)  VALUE  'W2P2'. G7W2PGM 
00151          10  WS-01-ABCODE-W2P2-MSG      PIC X(44)  VALUE          G7W2PGM 
00152             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7W2PGM 
00153                                                                   G7W2PGM 
00154  01  WS-02-AREA.                                                  G7W2PGM 
00155      05  FILLER                   PIC X(16)  VALUE                G7W2PGM 
00156          '** WS-02-AREA **'.                                      G7W2PGM 
00157      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7W2PGM 
00158          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7W2PGM 
00159                                                    'GC6A' 'G7W1'  G7W2PGM 
00160                                                    'G7W2'.        G7W2PGM 
00161          88  WS-02-MY-EIBTRNID                     VALUE  'G7W2'. G7W2PGM 
00162                                                                   G7W2PGM 
00163      05  WS-02-COMPUTED-LENGTHS.                                  G7W2PGM 
00164          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7W2PGM 
00165          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7W2PGM 
00166          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7W2PGM 
00167                                                                   G7W2PGM 
00168      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7W2PGM 
00169                                                                   G7W2PGM 
00170      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7W2PGM 
00171                                                                   G7W2PGM 
00172      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7W2PGM 
00173      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7W2PGM 
00174          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7W2PGM 
00175                                         PIC X.                    G7W2PGM 
00176          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7W2PGM 
00177                                                    '0' THRU '9'   G7W2PGM 
00178                                                    'A' THRU 'I'   G7W2PGM 
00179                                                    'J' THRU 'R'   G7W2PGM 
00180                                                    'S' THRU 'Z'   G7W2PGM 
00181                                                    SPACE.         G7W2PGM 
00182                                                                   G7W2PGM 
00183      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7W2PGM 
00184          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7W2PGM 
00185          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7W2PGM 
00186                                                                   G7W2PGM 
00187      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7W2PGM 
00188          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7W2PGM 
00189                                                                   G7W2PGM 
00190      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7W2PGM 
00191                                                                   G7W2PGM 
00192      05  WS-02-HEX-F00000.                                        G7W2PGM 
00193          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7W2PGM 
00194          10  FILLER                     PIC  X(09) VALUE          G7W2PGM 
00195                                                    LOW-VALUES.    G7W2PGM 
00196                                                                   G7W2PGM 
00197                                                                   G7W2PGM 
00198      05  WS-02-FLAT-RATE-PDM-AMT-X.                               G7W2PGM 
00199          10  WS-02-FLAT-RATE-PDM-AMT      PIC 9(5)V99 VALUE ZEROS.G7W2PGM 
00200 *        10  WS-02-S2FLPDY              REDEFINES                 G7W2PGM 
00201 *            WS-02-FLAT-RATE-PDM-AMT      PIC X(7).               G7W2PGM 
00202                                                                   G7W2PGM 
00203      05  WS-02-ADDN-ALLOW-AMT-PER-DAY-X.                          G7W2PGM 
00204          10  WS-02-ADDN-ALLOW-AMT-PER-DAY PIC 9(3)V99 VALUE ZEROS.G7W2PGM 
00205 *        10  WS-02-S2ADALD              REDEFINES                 G7W2PGM 
00206 *            WS-02-ADDN-ALLOW-AMT-PER-DAY PIC X(5).               G7W2PGM 
00207                                                                   G7W2PGM 
00208      05  WS-02-TREAT-TIME-FACTOR-X.                               G7W2PGM 
00209          10  WS-02-TREAT-TIME-FACTOR      PIC 999     VALUE ZEROS.G7W2PGM 
00210          10  WS-02-S2TRTMF              REDEFINES                 G7W2PGM 
00211              WS-02-TREAT-TIME-FACTOR      PIC X(3).               G7W2PGM 
00212                                                                   G7W2PGM 
00213      05  WS-5POS-MAX-AMT                PIC 999V99 VALUE 999.99.  G7W2PGM 
00214      05  WS-02-DISP-5POS-DEC            PIC 9(3).99.              G7W2PGM 
00215      05  WS-GPW2-ADDN-ALLOW-AMT-PER-DAY PIC 9(3)V99.              G7W2PGM 
00216                                                                   G7W2PGM 
00217      05  WS-7POS-MAX-AMT              PIC 99999V99 VALUE 99999.99.G7W2PGM 
00218      05  WS-02-DISP-7POS-DEC          PIC 9(5).99.                G7W2PGM 
00219      05  WS-GPW2-FLAT-RATE-PDM-AMT    PIC 9(5)V99.                G7W2PGM 
00220 /                                                                 G7W2PGM 
00221  01  WT-00-G7W2PGM-TABLES.                                        G7W2PGM 
00222      05  FILLER                   PIC X(16)  VALUE                G7W2PGM 
00223          '*G7W2PGM TABLES*'.                                      G7W2PGM 
00224                                                                   G7W2PGM 
00225  01  WT-01-TABLE.                                                 G7W2PGM 
00226      05  FILLER                  PIC X(16) VALUE                  G7W2PGM 
00227          '* WT-01-TABLE  *'.                                      G7W2PGM 
00228 ******************************************************************G7W2PGM 
00229 *    WT-01   MESSAGE TABLE                                       *G7W2PGM 
00230 ******************************************************************G7W2PGM 
00231  01  FILLER.                                                      G7W2PGM 
00232      05  WT-01-MESSAGE-VALUES.                                    G7W2PGM 
00233                                                                   G7W2PGM 
00234 *----------------------------------------------------------------*G7W2PGM 
00235          10  WT-01-ENTRY-001.                                     G7W2PGM 
00236              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00237              15  WT-01-MESSAGE-TEXT-001.                          G7W2PGM 
00238                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00239                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00240                  20  FILLER          PIC X(3)  VALUE  '001'.      G7W2PGM 
00241                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00242                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00243                      ' INVALID PFKEY SELECTION                    G7W2PGM 
00244 -                    '                         '.                 G7W2PGM 
00245              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00246                                                                   G7W2PGM 
00247 *----------------------------------------------------------------*G7W2PGM 
00248          10  WT-01-ENTRY-002.                                     G7W2PGM 
00249              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00250              15  WT-01-MESSAGE-TEXT-002.                          G7W2PGM 
00251                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00252                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00253                  20  FILLER          PIC X(3)  VALUE  '002'.      G7W2PGM 
00254                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00255                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00256                      'TREATMENT TIME FACTOR DAYS REQUIRED WHEN INDG7W2PGM 
00257 -                    'ICATOR IS CODED          '.                 G7W2PGM 
00258              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00259                                                                   G7W2PGM 
00260 *----------------------------------------------------------------*G7W2PGM 
00261          10  WT-01-ENTRY-003.                                     G7W2PGM 
00262              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00263              15  WT-01-MESSAGE-TEXT-003.                          G7W2PGM 
00264                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00265                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00266                  20  FILLER          PIC X(3)  VALUE  '003'.      G7W2PGM 
00267                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00268                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00269                      'INDICATOR REQUIRED WHEN TREATMENT TIME FACTOG7W2PGM 
00270 -                    'R DAYS IS CODED          '.                 G7W2PGM 
00271              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00272                                                                   G7W2PGM 
00273 *----------------------------------------------------------------*G7W2PGM 
00274          10  WT-01-ENTRY-004.                                     G7W2PGM 
00275              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00276              15  WT-01-MESSAGE-TEXT-004.                          G7W2PGM 
00277                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00278                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00279                  20  FILLER          PIC X(3)  VALUE  '004'.      G7W2PGM 
00280                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00281                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00282                      '********************* F U T U R E    U S E *G7W2PGM 
00283 -                    '*************************'.                 G7W2PGM 
00284              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00285                                                                   G7W2PGM 
00286 *----------------------------------------------------------------*G7W2PGM 
00287          10  WT-01-ENTRY-005.                                     G7W2PGM 
00288              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00289              15  WT-01-MESSAGE-TEXT-005.                          G7W2PGM 
00290                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00291                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00292                  20  FILLER          PIC X(3)  VALUE  '005'.      G7W2PGM 
00293                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00294                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00295                      '********************* F U T U R E    U S E *G7W2PGM 
00296 -                    '*************************'.                 G7W2PGM 
00297              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00298                                                                   G7W2PGM 
00299 *----------------------------------------------------------------*G7W2PGM 
00300          10  WT-01-ENTRY-006.                                     G7W2PGM 
00301              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00302              15  WT-01-MESSAGE-TEXT-006.                          G7W2PGM 
00303                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00304                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00305                  20  FILLER          PIC X(3)  VALUE  '006'.      G7W2PGM 
00306                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00307                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00308                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7W2PGM 
00309 -                    ' PF4/PF16 TO CONTINUE    '.                 G7W2PGM 
00310              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00311                                                                   G7W2PGM 
00312 *----------------------------------------------------------------*G7W2PGM 
00313          10  WT-01-ENTRY-007.                                     G7W2PGM 
00314              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00315              15  WT-01-MESSAGE-TEXT-007.                          G7W2PGM 
00316                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00317                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00318                  20  FILLER          PIC X(3)  VALUE  '007'.      G7W2PGM 
00319                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00320                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00321                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7W2PGM 
00322 -                    'E CALL SYSTEMS           '.                 G7W2PGM 
00323              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00324                                                                   G7W2PGM 
00325 *----------------------------------------------------------------*G7W2PGM 
00326          10  WT-01-ENTRY-008.                                     G7W2PGM 
00327              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00328              15  WT-01-MESSAGE-TEXT-008.                          G7W2PGM 
00329                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00330                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00331                  20  FILLER          PIC X(3)  VALUE  '008'.      G7W2PGM 
00332                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00333                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00334                      'FIELD HAS AN INVALID VALUE                  G7W2PGM 
00335 -                    '                         '.                 G7W2PGM 
00336              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00337                                                                   G7W2PGM 
00338 *----------------------------------------------------------------*G7W2PGM 
00339          10  WT-01-ENTRY-009.                                     G7W2PGM 
00340              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00341              15  WT-01-MESSAGE-TEXT-009.                          G7W2PGM 
00342                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00343                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00344                  20  FILLER          PIC X(3)  VALUE  '009'.      G7W2PGM 
00345                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00346                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00347                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7W2PGM 
00348 -                    'YSTEM)                   '.                 G7W2PGM 
00349              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00350                                                                   G7W2PGM 
00351 *----------------------------------------------------------------*G7W2PGM 
00352          10  WT-01-ENTRY-010.                                     G7W2PGM 
00353              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00354              15  WT-01-MESSAGE-TEXT-010.                          G7W2PGM 
00355                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00356                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00357                  20  FILLER          PIC X(3)  VALUE  '010'.      G7W2PGM 
00358                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00359                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00360                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7W2PGM 
00361 -                    '                         '.                 G7W2PGM 
00362              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00363 *----------------------------------------------------------------*G7W2PGM 
00364          10  WT-01-ENTRY-011.                                     G7W2PGM 
00365              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00366              15  WT-01-MESSAGE-TEXT-004.                          G7W2PGM 
00367                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00368                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00369                  20  FILLER          PIC X(3)  VALUE  '011'.      G7W2PGM 
00370                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00371                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00372       'FIELD EXCEEDS LENGTH OF 7 POSITIONS   FORMAT IS 99999.99'. G7W2PGM 
00373              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00374                                                                   G7W2PGM 
00375 *----------------------------------------------------------------*G7W2PGM 
00376          10  WT-01-ENTRY-012.                                     G7W2PGM 
00377              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00378              15  WT-01-MESSAGE-TEXT-005.                          G7W2PGM 
00379                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00380                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00381                  20  FILLER          PIC X(3)  VALUE  '012'.      G7W2PGM 
00382                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00383                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00384       'FIELD EXCEEDS LENGTH OF 5 POSITIONS   FORMAT IS 999.99'.   G7W2PGM 
00385              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00386                                                                   G7W2PGM 
00387 *----------------------------------------------------------------*G7W2PGM 
00388                                                                   G7W2PGM 
00389          10  WT-01-ENTRY-013.                                     G7W2PGM 
00390              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W2PGM 
00391              15  WT-01-MESSAGE-TEXT-005.                          G7W2PGM 
00392                  20  FILLER          PIC X(4)  VALUE  'G7W2'.     G7W2PGM 
00393                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W2PGM 
00394                  20  FILLER          PIC X(3)  VALUE  '013'.      G7W2PGM 
00395                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W2PGM 
00396                  20  FILLER          PIC X(70) VALUE              G7W2PGM 
00397                      ' INVALID DECIMAL DETECTED'.                 G7W2PGM 
00398              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W2PGM 
00399                                                                   G7W2PGM 
00400 *----------------------------------------------------------------*G7W2PGM 
00401                                                                   G7W2PGM 
00402      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7W2PGM 
00403          WT-01-MESSAGE-VALUES         OCCURS 013 TIMES            G7W2PGM 
00404                                      INDEXED BY WT-01-INDEX.      G7W2PGM 
00405          10  WT-01-ENTRY.                                         G7W2PGM 
00406              15  FILLER              PIC X(02).                   G7W2PGM 
00407              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7W2PGM 
00408              15  FILLER              PIC X(02).                   G7W2PGM 
00409                                                                   G7W2PGM 
00410                                                                   G7W2PGM 
00411 /*** MAP FIELD ATTRIBUTES                                         G7W2PGM 
00412  COPY DFHBMSCA.                                                   G7W2PGM 
00413 *                         AUTOSKIP, BRIGHT, FSET                  G7W2PGM 
00414      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7W2PGM 
00415                                                                   G7W2PGM 
00416 /*** ATTENTION KEYS                                               G7W2PGM 
00417  COPY DFHAID.                                                     G7W2PGM 
00418                                                                   G7W2PGM 
00419 /***  PROVISION MAINTENANCE SCREEN                                G7W2PGM 
00420  COPY  G7W2SETC.                                                  G7W2PGM 
00421                                                                   G7W2PGM 
00422 /*** DATE ROUTINE COMMAREA                                        G7W2PGM 
00423  01  HGADATES-COMMAREA.                                           G7W2PGM 
00424  COPY HGCDAT01.                                                   G7W2PGM 
00425                                                                   G7W2PGM 
00426 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7W2PGM 
00427  01  GCVIOPGM-PARM-LIST.                                          G7W2PGM 
00428  COPY GCVINTRC.                                                   G7W2PGM 
00429                                                                   G7W2PGM 
00430 /*** DECIMAL CONVERT COMMAREA                                     G7W2PGM 
00431  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7W2PGM 
00432  COPY GCDCCA01.                                                   G7W2PGM 
00433                                                                   G7W2PGM 
00434 /*** ALTERNATIVE WORKFILE KEYS                                    G7W2PGM 
00435  01  FILLER.                                                      G7W2PGM 
00436      COPY GCWRKKEY.                                               G7W2PGM 
00437                                                                   G7W2PGM 
00438 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7W2PGM 
00439  01  FILLER.                                                      G7W2PGM 
00440      COPY GCCDRLEN.                                               G7W2PGM 
00441                                                                   G7W2PGM 
00442                                                                   G7W2PGM 
00443  01  WS-END                       PIC X(58) VALUE                 G7W2PGM 
00444      '*** G7W2PGM  WORKING-STORAGE ENDS HERE ***'.                G7W2PGM 
00445 /                                                                 G7W2PGM 
00446  LINKAGE SECTION.                                                 G7W2PGM 
00447 /                                                                 G7W2PGM 
00448  01  DFHCOMMAREA.                                                 G7W2PGM 
00449      COPY  GCWRKDCC.                                              G7W2PGM 
00450      COPY  GCBENPVC.                                              G7W2PGM 
00451 /                                                                 G7W2PGM 
00452 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7W2PGM 
00453  01  IO-PARM-BEN-PROV-AREA.                                       G7W2PGM 
00454      COPY  GCIOPRM2.                                              G7W2PGM 
00455      COPY  GCWRKDC2.                                              G7W2PGM 
00456      COPY  GCBENPV2.                                              G7W2PGM 
00457                                                                   G7W2PGM 
00458 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7W2PGM 
00459  01  IO-PARM-CONTRACT-AREA.                                       G7W2PGM 
00460      COPY  GCIOPRM3.                                              G7W2PGM 
00461      COPY  GCWRKDC3.                                              G7W2PGM 
00462      COPY  GCCONTR2.                                              G7W2PGM 
00463 /                                                                 G7W2PGM 
00464  PROCEDURE DIVISION.                                              G7W2PGM 
00465                                                                   G7W2PGM 
00466 ****************************************************************  G7W2PGM 
00467 *                                                              *  G7W2PGM 
00468 *           P R O C E S S     C O N T R O L                    *  G7W2PGM 
00469 *                                                              *  G7W2PGM 
00470 ****************************************************************  G7W2PGM 
00471  0000-000-PROCESS-CONTROL       SECTION.                          G7W2PGM 
00472  0000-010.                                                        G7W2PGM 
00473                                                                   G7W2PGM 
00474      IF  EIBAID  =  DFHCLEAR                                      G7W2PGM 
00475          EXEC CICS  RETURN                                        G7W2PGM 
00476                     END-EXEC.                                     G7W2PGM 
00477                                                                   G7W2PGM 
00478      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7W2PGM 
00479                                                                   G7W2PGM 
00480      IF  WS-02-MY-EIBTRNID                                        G7W2PGM 
00481      THEN                                                         G7W2PGM 
00482          PERFORM  2000-000-PROCESS-INPUT                          G7W2PGM 
00483      ELSE                                                         G7W2PGM 
00484          PERFORM  1000-000-DISPLAY-SCREEN.                        G7W2PGM 
00485                                                                   G7W2PGM 
00486                                                                   G7W2PGM 
00487 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7W2PGM 
00488                                                                   G7W2PGM 
00489      MOVE WS-01-ABCODE-W2L1     TO WS-01-ABCODE                   G7W2PGM 
00490      MOVE WS-01-ABCODE-W2L1-MSG TO WS-01-ABCODE-MSG               G7W2PGM 
00491      PERFORM  9999-000-ABEND-THE-TASK.                            G7W2PGM 
00492                                                                   G7W2PGM 
00493      GOBACK.                                                      G7W2PGM 
00494                                                                   G7W2PGM 
00495                                                                   G7W2PGM 
00496  0000-900-EXIT.                                                   G7W2PGM 
00497      EXIT.                                                        G7W2PGM 
00498 /***************************************************************  G7W2PGM 
00499 *                                                              *  G7W2PGM 
00500 * 1000  DISPLAY INITIAL SCREEN                                 *  G7W2PGM 
00501 *                                                              *  G7W2PGM 
00502 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7W2PGM 
00503 *                                                              *  G7W2PGM 
00504 ****************************************************************  G7W2PGM 
00505  1000-000-DISPLAY-SCREEN        SECTION.                          G7W2PGM 
00506  1000-010.                                                        G7W2PGM 
00507                                                                   G7W2PGM 
00508 *------- MOVE LOW VALUES TO FIRST SCREEN FOR DISPLAY              G7W2PGM 
00509 *                                                                 G7W2PGM 
00510      MOVE LOW-VALUES TO G7W2I01I.                                 G7W2PGM 
00511                                                                   G7W2PGM 
00512 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7W2PGM 
00513                                                                   G7W2PGM 
00514      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7W2PGM 
00515          MOVE WS-01-ABCODE-W2P1     TO WS-01-ABCODE               G7W2PGM 
00516          MOVE WS-01-ABCODE-W2P1-MSG TO WS-01-ABCODE-MSG           G7W2PGM 
00517          PERFORM 9999-000-ABEND-THE-TASK.                         G7W2PGM 
00518                                                                   G7W2PGM 
00519                                                                   G7W2PGM 
00520 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7W2PGM 
00521                                                                   G7W2PGM 
00522      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7W2PGM 
00523                                         + GC-GCBENPRV-FIXED-LEN   G7W2PGM 
00524                                         + GC-GCBENPRV-VARY-LEN.   G7W2PGM 
00525                                                                   G7W2PGM 
00526                                                                   G7W2PGM 
00527 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7W2PGM 
00528                                                                   G7W2PGM 
00529      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7W2PGM 
00530          MOVE WS-01-ABCODE-W2P2     TO WS-01-ABCODE               G7W2PGM 
00531          MOVE WS-01-ABCODE-W2P2-MSG TO WS-01-ABCODE-MSG           G7W2PGM 
00532          PERFORM 9999-000-ABEND-THE-TASK.                         G7W2PGM 
00533                                                                   G7W2PGM 
00534                                                                   G7W2PGM 
00535 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7W2PGM 
00536 *          BY CALLER IN COMMAREA.                                 G7W2PGM 
00537                                                                   G7W2PGM 
00538      MOVE WRK-PLAN-CODE                      TO S2PLNCDO.         G7W2PGM 
00539      MOVE WRK-GROUP-NUM                      TO S2GRPNOO.         G7W2PGM 
00540      MOVE WRK-SECTION-NUM                    TO S2SECNOO.         G7W2PGM 
00541      MOVE WRK-PKG-CODE                       TO S2PKGCDO.         G7W2PGM 
00542      MOVE WRK-PROV-CTL                       TO S2PRVO.           G7W2PGM 
00543      MOVE WRK-FAM-REL-LEVEL                  TO S2FRLO.           G7W2PGM 
00544      MOVE WRK-L-O-B                          TO S2LOBO.           G7W2PGM 
00545                                                                   G7W2PGM 
00546      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7W2PGM 
00547      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7W2PGM 
00548      IF  HGADATE-RETURN = ZEROS                                   G7W2PGM 
00549      THEN                                                         G7W2PGM 
00550          MOVE DFHBMASF                       TO S2EFFDTA          G7W2PGM 
00551          MOVE HGADATE-DATE2                  TO S2EFFDTO          G7W2PGM 
00552      ELSE                                                         G7W2PGM 
00553          MOVE DFHBMABF                       TO S2EFFDTA          G7W2PGM 
00554          MOVE HGADATE-JULIAN1                TO S2EFFDTO.         G7W2PGM 
00555                                                                   G7W2PGM 
00556      MOVE GCP-PROVN-ID                       TO S2BPVIDO.         G7W2PGM 
00557                                                                   G7W2PGM 
00558 *    IF  GPW-FLAT-RATE-PDM-AMT = ZEROS                            G7W2PGM 
00559 *        MOVE WS-02-HEX-F00000               TO S2FLPDYO          G7W2PGM 
00560 *    ELSE                                                         G7W2PGM 
00561 *        MOVE   GPW-FLAT-RATE-PDM-AMT        TO                   G7W2PGM 
00562 *             WS-02-FLAT-RATE-PDM-AMT                             G7W2PGM 
00563 *        MOVE WS-02-FLAT-RATE-PDM-AMT-X      TO S2FLPDYO.         G7W2PGM 
00564                                                                   G7W2PGM 
00565 **   D129     DECIMAL CONVERSION.                                 G7W2PGM 
00566 *                                                                 G7W2PGM 
00567          MOVE   GPW-FLAT-RATE-PDM-AMT        TO                   G7W2PGM 
00568               WS-02-FLAT-RATE-PDM-AMT                             G7W2PGM 
00569          MOVE WS-02-FLAT-RATE-PDM-AMT        TO                   G7W2PGM 
00570               WS-02-DISP-7POS-DEC.                                G7W2PGM 
00571          MOVE WS-02-DISP-7POS-DEC            TO S2FLPDYO.         G7W2PGM 
00572                                                                   G7W2PGM 
00573 *    IF  GPW-ADDN-ALLOW-AMT-PER-DAY = ZEROS                       G7W2PGM 
00574 *        MOVE WS-02-HEX-F00000               TO S2ADALDO          G7W2PGM 
00575 *    ELSE                                                         G7W2PGM 
00576 *        MOVE   GPW-ADDN-ALLOW-AMT-PER-DAY   TO                   G7W2PGM 
00577 *             WS-02-ADDN-ALLOW-AMT-PER-DAY                        G7W2PGM 
00578 *        MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY-X TO S2ADALDO.         G7W2PGM 
00579                                                                   G7W2PGM 
00580 **   D129     DECIMAL CONVERSION.                                 G7W2PGM 
00581 *                                                                 G7W2PGM 
00582          MOVE   GPW-ADDN-ALLOW-AMT-PER-DAY   TO                   G7W2PGM 
00583               WS-02-ADDN-ALLOW-AMT-PER-DAY.                       G7W2PGM 
00584          MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY   TO                   G7W2PGM 
00585               WS-02-DISP-5POS-DEC.                                G7W2PGM 
00586          MOVE WS-02-DISP-5POS-DEC            TO S2ADALDO.         G7W2PGM 
00587                                                                   G7W2PGM 
00588      MOVE GPW-CERTFN-REPETN-REQRM-IND        TO S2RRCERO.         G7W2PGM 
00589      MOVE GPW-TREAT-TIME-FACTOR-IND          TO S2TRTMIO.         G7W2PGM 
00590                                                                   G7W2PGM 
00591      IF  GPW-TREAT-TIME-FACTOR = ZEROS                            G7W2PGM 
00592          MOVE WS-02-HEX-F00000               TO S2TRTMFO          G7W2PGM 
00593      ELSE                                                         G7W2PGM 
00594          MOVE   GPW-TREAT-TIME-FACTOR        TO                   G7W2PGM 
00595               WS-02-TREAT-TIME-FACTOR                             G7W2PGM 
00596          MOVE WS-02-TREAT-TIME-FACTOR-X      TO S2TRTMFO.         G7W2PGM 
00597                                                                   G7W2PGM 
00598      MOVE GPW-ELIG-METHD-OF-TREAT-IND        TO S2ELMTIO.         G7W2PGM 
00599      MOVE GPW-PHYS-EXAM-IND                  TO S2PHEXIO.         G7W2PGM 
00600      MOVE GPW-REHAB-ADM-RESTRN-IND           TO S2RHARIO.         G7W2PGM 
00601                                                                   G7W2PGM 
00602                                                                   G7W2PGM 
00603                                                                   G7W2PGM 
00604                                                                   G7W2PGM 
00605 *------- SEND INITIAL SCREEN ------------------------------------*G7W2PGM 
00606                                                                   G7W2PGM 
00607      MOVE  -1 TO  S2FLPDYL.                                       G7W2PGM 
00608      PERFORM 9100-000-SEND-THEN-RETURN.                           G7W2PGM 
00609                                                                   G7W2PGM 
00610                                                                   G7W2PGM 
00611  1000-900-EXIT.                                                   G7W2PGM 
00612      EXIT.                                                        G7W2PGM 
00613 /***************************************************************  G7W2PGM 
00614 *                                                              *  G7W2PGM 
00615 * 2000    P R O C E S S    I N P U T                           *  G7W2PGM 
00616 *                                                              *  G7W2PGM 
00617 ****************************************************************  G7W2PGM 
00618  2000-000-PROCESS-INPUT         SECTION.                          G7W2PGM 
00619  2000-010.                                                        G7W2PGM 
00620                                                                   G7W2PGM 
00621 *------ VALIDATE PFKEY USAGE ------------------------------------*G7W2PGM 
00622                                                                   G7W2PGM 
00623      IF  EIBAID = DFHENTER OR                                     G7W2PGM 
00624                   DFHPF3   OR  DFHPF15 OR                         G7W2PGM 
00625                   DFHPF4   OR  DFHPF16 OR                         G7W2PGM 
00626                   DFHPF6   OR  DFHPF18 OR                         G7W2PGM 
00627                   DFHPF7   OR  DFHPF19 OR                         G7W2PGM 
00628                   DFHPF8   OR  DFHPF20                            G7W2PGM 
00629      THEN                                                         G7W2PGM 
00630          NEXT SENTENCE                                            G7W2PGM 
00631      ELSE                                                         G7W2PGM 
00632          SET WT-01-INDEX TO +01                                   G7W2PGM 
00633          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W2PGM 
00634          PERFORM 9100-000-SEND-THEN-RETURN.                       G7W2PGM 
00635                                                                   G7W2PGM 
00636                                                                   G7W2PGM 
00637                                                                   G7W2PGM 
00638      EXEC CICS  HANDLE CONDITION                                  G7W2PGM 
00639                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7W2PGM 
00640                        END-EXEC.                                  G7W2PGM 
00641                                                                   G7W2PGM 
00642                                                                   G7W2PGM 
00643      EXEC CICS  RECEIVE MAP   ('G7W2I01')                         G7W2PGM 
00644                         MAPSET('G7W2SET')                         G7W2PGM 
00645                         END-EXEC.                                 G7W2PGM 
00646                                                                   G7W2PGM 
00647                                                                   G7W2PGM 
00648      IF  S2FUNCI  NOT = 'G7W2'  OR                                G7W2PGM 
00649          S2SCRNI  NOT = '007W02'                                  G7W2PGM 
00650          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7W2PGM 
00651                                                                   G7W2PGM 
00652                                                                   G7W2PGM 
00653 *--- RETURN TO GCPS MENU? ---------------------------------------*G7W2PGM 
00654                                                                   G7W2PGM 
00655      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7W2PGM 
00656          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7W2PGM 
00657                                                                   G7W2PGM 
00658 *--- PROCESS SCREEN FIELDS --------------------------------------*G7W2PGM 
00659                                                                   G7W2PGM 
00660      PERFORM 2100-000-FIELD-EDITS.                                G7W2PGM 
00661                                                                   G7W2PGM 
00662      IF  WS-02-SCREEN-HAS-ERRORS                                  G7W2PGM 
00663          PERFORM 9100-000-SEND-THEN-RETURN.                       G7W2PGM 
00664                                                                   G7W2PGM 
00665      PERFORM 2200-000-LOGICAL-EDITS.                              G7W2PGM 
00666                                                                   G7W2PGM 
00667      IF  WS-02-SCREEN-HAS-ERRORS                                  G7W2PGM 
00668          PERFORM 9100-000-SEND-THEN-RETURN.                       G7W2PGM 
00669                                                                   G7W2PGM 
00670      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7W2PGM 
00671                                                                   G7W2PGM 
00672      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7W2PGM 
00673                                                                   G7W2PGM 
00674                                                                   G7W2PGM 
00675  2000-900-EXIT.                                                   G7W2PGM 
00676      EXIT.                                                        G7W2PGM 
00677 /***************************************************************  G7W2PGM 
00678 *                                                              *  G7W2PGM 
00679 * 2100  DO SCREEN FIELD EDITS                                  *  G7W2PGM 
00680 *                                                              *  G7W2PGM 
00681 ****************************************************************  G7W2PGM 
00682  2100-000-FIELD-EDITS           SECTION.                          G7W2PGM 
00683  2100-010.                                                        G7W2PGM 
00684                                                                   G7W2PGM 
00685 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7W2PGM 
00686                                                                   G7W2PGM 
00687      MOVE DFHBMUNF TO S2FLPDYA                                    G7W2PGM 
00688                       S2ADALDA                                    G7W2PGM 
00689                       S2RRCERA                                    G7W2PGM 
00690                       S2TRTMIA                                    G7W2PGM 
00691                       S2TRTMFA                                    G7W2PGM 
00692                       S2ELMTIA                                    G7W2PGM 
00693                       S2PHEXIA                                    G7W2PGM 
00694                       S2RHARIA.                                   G7W2PGM 
00695                                                                   G7W2PGM 
00696                                                                   G7W2PGM 
00697      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7W2PGM 
00698                                                                   G7W2PGM 
00699                                                                   G7W2PGM 
00700 *-- VALIDATE ------ FLAT RATE PER-DIEM AMOUNT -------------------*G7W2PGM 
00701 *   D129     DECIMAL CONVERSION                                   G7W2PGM 
00702                                                                   G7W2PGM 
00703      MOVE S2FLPDYI TO D-C-RECEIVE-FIELD.                          G7W2PGM 
00704      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7W2PGM 
00705      MOVE '00' TO D-C-RETURN-CODE.                                G7W2PGM 
00706      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7W2PGM 
00707      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7W2PGM 
00708      IF D-C-RETURN-CODE = '00'                                    G7W2PGM 
00709          IF D-C-RETURN-FIELD-DEC2 > WS-7POS-MAX-AMT               G7W2PGM 
00710              MOVE -1       TO S2FLPDYL                            G7W2PGM 
00711              MOVE DFHBMUBF TO S2FLPDYA                            G7W2PGM 
00712              IF WS-02-SCREEN-HAS-ERRORS                           G7W2PGM 
00713                  NEXT SENTENCE                                    G7W2PGM 
00714              ELSE                                                 G7W2PGM 
00715                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7W2PGM 
00716                  SET WT-01-INDEX TO +11                           G7W2PGM 
00717                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00718          ELSE                                                     G7W2PGM 
00719              MOVE D-C-RETURN-FIELD-DEC2                           G7W2PGM 
00720                TO WS-02-FLAT-RATE-PDM-AMT                         G7W2PGM 
00721              MOVE WS-02-FLAT-RATE-PDM-AMT                         G7W2PGM 
00722                TO WS-02-DISP-7POS-DEC                             G7W2PGM 
00723              MOVE WS-02-DISP-7POS-DEC                             G7W2PGM 
00724                TO S2FLPDYO                                        G7W2PGM 
00725      ELSE                                                         G7W2PGM 
00726          MOVE -1       TO S2FLPDYL                                G7W2PGM 
00727          MOVE DFHBMUBF TO S2FLPDYA                                G7W2PGM 
00728          IF WS-02-SCREEN-HAS-ERRORS                               G7W2PGM 
00729              NEXT SENTENCE                                        G7W2PGM 
00730          ELSE                                                     G7W2PGM 
00731              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7W2PGM 
00732              IF D-C-RETURN-CODE = '10'                            G7W2PGM 
00733                  SET WT-01-INDEX TO +10                           G7W2PGM 
00734                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00735              ELSE                                                 G7W2PGM 
00736                  SET WT-01-INDEX TO +13                           G7W2PGM 
00737                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7W2PGM 
00738                                                                   G7W2PGM 
00739 *    IF  S2FLPDYI IS NUMERIC                                      G7W2PGM 
00740 *    THEN                                                         G7W2PGM 
00741 *        NEXT SENTENCE                                            G7W2PGM 
00742 *    ELSE                                                         G7W2PGM 
00743 *        MOVE  -1        TO  S2FLPDYL                             G7W2PGM 
00744 *        MOVE  DFHBMUBF  TO  S2FLPDYA                             G7W2PGM 
00745 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00746 *        THEN                                                     G7W2PGM 
00747 *            NEXT SENTENCE                                        G7W2PGM 
00748 *        ELSE                                                     G7W2PGM 
00749 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00750 *            SET WT-01-INDEX TO +10                               G7W2PGM 
00751 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00752                                                                   G7W2PGM 
00753 *-- VALIDATE ------ ADDITIONAL ALLOWANCE AMOUNT PER DAY ---------*G7W2PGM 
00754 *   D129     DECIMAL CONVERSION                                   G7W2PGM 
00755                                                                   G7W2PGM 
00756      MOVE S2ADALDI TO D-C-RECEIVE-FIELD.                          G7W2PGM 
00757      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7W2PGM 
00758      MOVE '00' TO D-C-RETURN-CODE.                                G7W2PGM 
00759      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7W2PGM 
00760      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7W2PGM 
00761      IF D-C-RETURN-CODE = '00'                                    G7W2PGM 
00762          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7W2PGM 
00763              MOVE -1       TO S2ADALDL                            G7W2PGM 
00764              MOVE DFHBMUBF TO S2ADALDA                            G7W2PGM 
00765              IF WS-02-SCREEN-HAS-ERRORS                           G7W2PGM 
00766                  NEXT SENTENCE                                    G7W2PGM 
00767              ELSE                                                 G7W2PGM 
00768                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7W2PGM 
00769                  SET WT-01-INDEX TO +12                           G7W2PGM 
00770                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00771          ELSE                                                     G7W2PGM 
00772              MOVE D-C-RETURN-FIELD-DEC2                           G7W2PGM 
00773                TO WS-02-ADDN-ALLOW-AMT-PER-DAY                    G7W2PGM 
00774              MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY                    G7W2PGM 
00775                TO WS-02-DISP-5POS-DEC                             G7W2PGM 
00776              MOVE WS-02-DISP-5POS-DEC                             G7W2PGM 
00777                TO S2ADALDO                                        G7W2PGM 
00778      ELSE                                                         G7W2PGM 
00779          MOVE -1       TO S2ADALDL                                G7W2PGM 
00780          MOVE DFHBMUBF TO S2ADALDA                                G7W2PGM 
00781          IF WS-02-SCREEN-HAS-ERRORS                               G7W2PGM 
00782              NEXT SENTENCE                                        G7W2PGM 
00783          ELSE                                                     G7W2PGM 
00784              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7W2PGM 
00785              IF D-C-RETURN-CODE = '10'                            G7W2PGM 
00786                  SET WT-01-INDEX TO +10                           G7W2PGM 
00787                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00788              ELSE                                                 G7W2PGM 
00789                  SET WT-01-INDEX TO +13                           G7W2PGM 
00790                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7W2PGM 
00791                                                                   G7W2PGM 
00792                                                                   G7W2PGM 
00793 *    IF  S2ADALDI IS NUMERIC                                      G7W2PGM 
00794 *    THEN                                                         G7W2PGM 
00795 *        NEXT SENTENCE                                            G7W2PGM 
00796 *    ELSE                                                         G7W2PGM 
00797 *        MOVE  -1        TO  S2ADALDL                             G7W2PGM 
00798 *        MOVE  DFHBMUBF  TO  S2ADALDA                             G7W2PGM 
00799 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00800 *        THEN                                                     G7W2PGM 
00801 *            NEXT SENTENCE                                        G7W2PGM 
00802 *        ELSE                                                     G7W2PGM 
00803 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00804 *            SET WT-01-INDEX TO +10                               G7W2PGM 
00805 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00806                                                                   G7W2PGM 
00807                                                                   G7W2PGM 
00808 *-- VALIDATE ------ CERTIFICATION REPETITION REQUIREMENT IND ----*G7W2PGM 
00809 *   1. ALPHANUMERIC                                               G7W2PGM 
00810 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W2PGM 
00811                                                                   G7W2PGM 
00812      MOVE  S2RRCERI TO WS-02-CLASS-TEST-AREA.                     G7W2PGM 
00813      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W2PGM 
00814      THEN                                                         G7W2PGM 
00815          MOVE  S2RRCERI TO GCVI-VALUE                             G7W2PGM 
00816          MOVE  'BPAB02' TO GCVI-FIELDS-KEY-ID                     G7W2PGM 
00817          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W2PGM 
00818          IF  GCVI-VALUE-NOT-FOUND                                 G7W2PGM 
00819          THEN                                                     G7W2PGM 
00820              MOVE  -1        TO  S2RRCERL                         G7W2PGM 
00821              MOVE  DFHBMUBF  TO  S2RRCERA                         G7W2PGM 
00822              IF  WS-02-SCREEN-HAS-ERRORS                          G7W2PGM 
00823              THEN                                                 G7W2PGM 
00824                  NEXT SENTENCE                                    G7W2PGM 
00825              ELSE                                                 G7W2PGM 
00826                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W2PGM 
00827                  SET WT-01-INDEX TO +09                           G7W2PGM 
00828                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00829          ELSE                                                     G7W2PGM 
00830              IF  GCVI-VALUE-NOT-LOADED                            G7W2PGM 
00831              THEN                                                 G7W2PGM 
00832                  MOVE  DFHBMUBF  TO  S2RRCERA                     G7W2PGM 
00833              ELSE                                                 G7W2PGM 
00834                  NEXT SENTENCE                                    G7W2PGM 
00835      ELSE                                                         G7W2PGM 
00836          MOVE  -1        TO  S2RRCERL                             G7W2PGM 
00837          MOVE  DFHBMUBF  TO  S2RRCERA                             G7W2PGM 
00838          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00839          THEN                                                     G7W2PGM 
00840              NEXT SENTENCE                                        G7W2PGM 
00841          ELSE                                                     G7W2PGM 
00842              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00843              SET WT-01-INDEX TO +08                               G7W2PGM 
00844              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00845                                                                   G7W2PGM 
00846                                                                   G7W2PGM 
00847 *-- VALIDATE ------ TREATMENT TIME FACTOR IND -------------------*G7W2PGM 
00848 *   1. ALPHANUMERIC                                               G7W2PGM 
00849 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W2PGM 
00850                                                                   G7W2PGM 
00851      MOVE  S2TRTMII TO WS-02-CLASS-TEST-AREA.                     G7W2PGM 
00852      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W2PGM 
00853      THEN                                                         G7W2PGM 
00854          MOVE  S2TRTMII TO GCVI-VALUE                             G7W2PGM 
00855          MOVE  'BPBA07' TO GCVI-FIELDS-KEY-ID                     G7W2PGM 
00856          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W2PGM 
00857          IF  GCVI-VALUE-NOT-FOUND                                 G7W2PGM 
00858          THEN                                                     G7W2PGM 
00859              MOVE  -1        TO  S2TRTMIL                         G7W2PGM 
00860              MOVE  DFHBMUBF  TO  S2TRTMIA                         G7W2PGM 
00861              IF  WS-02-SCREEN-HAS-ERRORS                          G7W2PGM 
00862              THEN                                                 G7W2PGM 
00863                  NEXT SENTENCE                                    G7W2PGM 
00864              ELSE                                                 G7W2PGM 
00865                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W2PGM 
00866                  SET WT-01-INDEX TO +09                           G7W2PGM 
00867                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00868          ELSE                                                     G7W2PGM 
00869              IF  GCVI-VALUE-NOT-LOADED                            G7W2PGM 
00870              THEN                                                 G7W2PGM 
00871                  MOVE  DFHBMUBF  TO  S2TRTMIA                     G7W2PGM 
00872              ELSE                                                 G7W2PGM 
00873                  NEXT SENTENCE                                    G7W2PGM 
00874      ELSE                                                         G7W2PGM 
00875          MOVE  -1        TO  S2TRTMIL                             G7W2PGM 
00876          MOVE  DFHBMUBF  TO  S2TRTMIA                             G7W2PGM 
00877          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00878          THEN                                                     G7W2PGM 
00879              NEXT SENTENCE                                        G7W2PGM 
00880          ELSE                                                     G7W2PGM 
00881              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00882              SET WT-01-INDEX TO +08                               G7W2PGM 
00883              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00884                                                                   G7W2PGM 
00885                                                                   G7W2PGM 
00886 *-- VALIDATE ------ TREATMENT TIME FACTOR -----------------------*G7W2PGM 
00887 *   1. NUMERICS                                                   G7W2PGM 
00888                                                                   G7W2PGM 
00889      IF  S2TRTMFI IS NUMERIC                                      G7W2PGM 
00890      THEN                                                         G7W2PGM 
00891          NEXT SENTENCE                                            G7W2PGM 
00892      ELSE                                                         G7W2PGM 
00893          MOVE  -1        TO  S2TRTMFL                             G7W2PGM 
00894          MOVE  DFHBMUBF  TO  S2TRTMFA                             G7W2PGM 
00895          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00896          THEN                                                     G7W2PGM 
00897              NEXT SENTENCE                                        G7W2PGM 
00898          ELSE                                                     G7W2PGM 
00899              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00900              SET WT-01-INDEX TO +10                               G7W2PGM 
00901              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00902                                                                   G7W2PGM 
00903                                                                   G7W2PGM 
00904 *-- VALIDATE ------ ELIGIBLE METHODS OF TREATMENT INDICATOR -----*G7W2PGM 
00905 *   1. ALPHANUMERIC                                               G7W2PGM 
00906 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W2PGM 
00907                                                                   G7W2PGM 
00908      MOVE  S2ELMTII TO WS-02-CLASS-TEST-AREA.                     G7W2PGM 
00909      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W2PGM 
00910      THEN                                                         G7W2PGM 
00911          MOVE  S2ELMTII TO GCVI-VALUE                             G7W2PGM 
00912          MOVE  'BPBA09' TO GCVI-FIELDS-KEY-ID                     G7W2PGM 
00913          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W2PGM 
00914          IF  GCVI-VALUE-NOT-FOUND                                 G7W2PGM 
00915          THEN                                                     G7W2PGM 
00916              MOVE  -1        TO  S2ELMTIL                         G7W2PGM 
00917              MOVE  DFHBMUBF  TO  S2ELMTIA                         G7W2PGM 
00918              IF  WS-02-SCREEN-HAS-ERRORS                          G7W2PGM 
00919              THEN                                                 G7W2PGM 
00920                  NEXT SENTENCE                                    G7W2PGM 
00921              ELSE                                                 G7W2PGM 
00922                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W2PGM 
00923                  SET WT-01-INDEX TO +09                           G7W2PGM 
00924                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00925          ELSE                                                     G7W2PGM 
00926              IF  GCVI-VALUE-NOT-LOADED                            G7W2PGM 
00927              THEN                                                 G7W2PGM 
00928                  MOVE  DFHBMUBF  TO  S2ELMTIA                     G7W2PGM 
00929              ELSE                                                 G7W2PGM 
00930                  NEXT SENTENCE                                    G7W2PGM 
00931      ELSE                                                         G7W2PGM 
00932          MOVE  -1        TO  S2ELMTIL                             G7W2PGM 
00933          MOVE  DFHBMUBF  TO  S2ELMTIA                             G7W2PGM 
00934          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00935          THEN                                                     G7W2PGM 
00936              NEXT SENTENCE                                        G7W2PGM 
00937          ELSE                                                     G7W2PGM 
00938              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00939              SET WT-01-INDEX TO +08                               G7W2PGM 
00940              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00941                                                                   G7W2PGM 
00942                                                                   G7W2PGM 
00943 *-- VALIDATE ------ PHYSICAL EXAM INDICATOR ---------------------*G7W2PGM 
00944 *   1. ALPHANUMERIC                                               G7W2PGM 
00945 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W2PGM 
00946                                                                   G7W2PGM 
00947      MOVE  S2PHEXII TO WS-02-CLASS-TEST-AREA.                     G7W2PGM 
00948      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W2PGM 
00949      THEN                                                         G7W2PGM 
00950          MOVE  S2PHEXII TO GCVI-VALUE                             G7W2PGM 
00951          MOVE  'BPBB01' TO GCVI-FIELDS-KEY-ID                     G7W2PGM 
00952          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W2PGM 
00953          IF  GCVI-VALUE-NOT-FOUND                                 G7W2PGM 
00954          THEN                                                     G7W2PGM 
00955              MOVE  -1        TO  S2PHEXIL                         G7W2PGM 
00956              MOVE  DFHBMUBF  TO  S2PHEXIA                         G7W2PGM 
00957              IF  WS-02-SCREEN-HAS-ERRORS                          G7W2PGM 
00958              THEN                                                 G7W2PGM 
00959                  NEXT SENTENCE                                    G7W2PGM 
00960              ELSE                                                 G7W2PGM 
00961                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W2PGM 
00962                  SET WT-01-INDEX TO +09                           G7W2PGM 
00963                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
00964          ELSE                                                     G7W2PGM 
00965              IF  GCVI-VALUE-NOT-LOADED                            G7W2PGM 
00966              THEN                                                 G7W2PGM 
00967                  MOVE  DFHBMUBF  TO  S2PHEXIA                     G7W2PGM 
00968              ELSE                                                 G7W2PGM 
00969                  NEXT SENTENCE                                    G7W2PGM 
00970      ELSE                                                         G7W2PGM 
00971          MOVE  -1        TO  S2PHEXIL                             G7W2PGM 
00972          MOVE  DFHBMUBF  TO  S2PHEXIA                             G7W2PGM 
00973          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
00974          THEN                                                     G7W2PGM 
00975              NEXT SENTENCE                                        G7W2PGM 
00976          ELSE                                                     G7W2PGM 
00977              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
00978              SET WT-01-INDEX TO +08                               G7W2PGM 
00979              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
00980                                                                   G7W2PGM 
00981                                                                   G7W2PGM 
00982 *-- VALIDATE ------ REHABILITATION ADMISSION RESTRICTION IND ----*G7W2PGM 
00983 *   1. ALPHANUMERIC                                               G7W2PGM 
00984 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W2PGM 
00985                                                                   G7W2PGM 
00986      MOVE  S2RHARII TO WS-02-CLASS-TEST-AREA.                     G7W2PGM 
00987      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W2PGM 
00988      THEN                                                         G7W2PGM 
00989          MOVE  S2RHARII TO GCVI-VALUE                             G7W2PGM 
00990          MOVE  'BPAA04' TO GCVI-FIELDS-KEY-ID                     G7W2PGM 
00991          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W2PGM 
00992          IF  GCVI-VALUE-NOT-FOUND                                 G7W2PGM 
00993          THEN                                                     G7W2PGM 
00994              MOVE  -1        TO  S2RHARIL                         G7W2PGM 
00995              MOVE  DFHBMUBF  TO  S2RHARIA                         G7W2PGM 
00996              IF  WS-02-SCREEN-HAS-ERRORS                          G7W2PGM 
00997              THEN                                                 G7W2PGM 
00998                  NEXT SENTENCE                                    G7W2PGM 
00999              ELSE                                                 G7W2PGM 
01000                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W2PGM 
01001                  SET WT-01-INDEX TO +09                           G7W2PGM 
01002                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
01003          ELSE                                                     G7W2PGM 
01004              IF  GCVI-VALUE-NOT-LOADED                            G7W2PGM 
01005              THEN                                                 G7W2PGM 
01006                  MOVE  DFHBMUBF  TO  S2RHARIA                     G7W2PGM 
01007              ELSE                                                 G7W2PGM 
01008                  NEXT SENTENCE                                    G7W2PGM 
01009      ELSE                                                         G7W2PGM 
01010          MOVE  -1        TO  S2RHARIL                             G7W2PGM 
01011          MOVE  DFHBMUBF  TO  S2RHARIA                             G7W2PGM 
01012          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
01013          THEN                                                     G7W2PGM 
01014              NEXT SENTENCE                                        G7W2PGM 
01015          ELSE                                                     G7W2PGM 
01016              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
01017              SET WT-01-INDEX TO +08                               G7W2PGM 
01018              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W2PGM 
01019                                                                   G7W2PGM 
01020                                                                   G7W2PGM 
01021  2100-900-EXIT.                                                   G7W2PGM 
01022      EXIT.                                                        G7W2PGM 
01023 /***************************************************************  G7W2PGM 
01024 *                                                              *  G7W2PGM 
01025 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7W2PGM 
01026 *                                                              *  G7W2PGM 
01027 ****************************************************************  G7W2PGM 
01028  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7W2PGM 
01029  2110-010.                                                        G7W2PGM 
01030                                                                   G7W2PGM 
01031      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7W2PGM 
01032                                                                   G7W2PGM 
01033      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7W2PGM 
01034                       COMMAREA(GCVIOPGM-PARM-LIST)                G7W2PGM 
01035                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7W2PGM 
01036                       END-EXEC.                                   G7W2PGM 
01037                                                                   G7W2PGM 
01038      IF  GCVI-VALUE-NOT-LOADED                                    G7W2PGM 
01039          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7W2PGM 
01040                                                                   G7W2PGM 
01041  2110-900-EXIT.                                                   G7W2PGM 
01042      EXIT.                                                        G7W2PGM 
01043 /***************************************************************  G7W2PGM 
01044 *                                                              *  G7W2PGM 
01045 * 2200  DO SCREEN LOGICAL EDITS                                *  G7W2PGM 
01046 *                                                              *  G7W2PGM 
01047 ****************************************************************  G7W2PGM 
01048  2200-000-LOGICAL-EDITS         SECTION.                          G7W2PGM 
01049  2200-010.                                                        G7W2PGM 
01050                                                                   G7W2PGM 
01051                                                                   G7W2PGM 
01052 *----------------------------------------------------------------*G7W2PGM 
01053 *                                                                *G7W2PGM 
01054 *  IF   TREATMENT TIME IND  (S2TRTMI) > ZERO                     *G7W2PGM 
01055 *                                                                *G7W2PGM 
01056 *  THEN TREATMENT TIME FACTOR (S2TRTMF) MUST BE > ZERO           *G7W2PGM 
01057 *                                                                *G7W2PGM 
01058 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7W2PGM 
01059 *                                                                *G7W2PGM 
01060 *----------------------------------------------------------------*G7W2PGM 
01061                                                                   G7W2PGM 
01062      IF  S2TRTMII     > ZEROS                                     G7W2PGM 
01063          AND                                                      G7W2PGM 
01064          S2TRTMFI NOT > ZEROS                                     G7W2PGM 
01065      THEN                                                         G7W2PGM 
01066          MOVE  -1        TO  S2TRTMFL                             G7W2PGM 
01067          MOVE  DFHBMUBF  TO  S2TRTMFA                             G7W2PGM 
01068                              S2TRTMIA                             G7W2PGM 
01069          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
01070          THEN                                                     G7W2PGM 
01071              NEXT SENTENCE                                        G7W2PGM 
01072          ELSE                                                     G7W2PGM 
01073              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
01074              SET WT-01-INDEX TO +02                               G7W2PGM 
01075              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W2PGM 
01076      ELSE                                                         G7W2PGM 
01077          NEXT SENTENCE.                                           G7W2PGM 
01078                                                                   G7W2PGM 
01079      IF  S2TRTMFI     > ZEROS                                     G7W2PGM 
01080          AND                                                      G7W2PGM 
01081          S2TRTMII NOT > ZEROS                                     G7W2PGM 
01082      THEN                                                         G7W2PGM 
01083          MOVE  -1        TO  S2TRTMIL                             G7W2PGM 
01084          MOVE  DFHBMUBF  TO  S2TRTMIA                             G7W2PGM 
01085                              S2TRTMFA                             G7W2PGM 
01086          IF  WS-02-SCREEN-HAS-ERRORS                              G7W2PGM 
01087          THEN                                                     G7W2PGM 
01088              NEXT SENTENCE                                        G7W2PGM 
01089          ELSE                                                     G7W2PGM 
01090              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W2PGM 
01091              SET WT-01-INDEX TO +03                               G7W2PGM 
01092              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W2PGM 
01093      ELSE                                                         G7W2PGM 
01094          NEXT SENTENCE.                                           G7W2PGM 
01095                                                                   G7W2PGM 
01096                                                                   G7W2PGM 
01097                                                                   G7W2PGM 
01098 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7W2PGM 
01099                                                                   G7W2PGM 
01100      IF  WS-02-SCREEN-HAS-ERRORS                                  G7W2PGM 
01101      THEN                                                         G7W2PGM 
01102          NEXT SENTENCE                                            G7W2PGM 
01103      ELSE                                                         G7W2PGM 
01104          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7W2PGM 
01105          THEN                                                     G7W2PGM 
01106              IF EIBAID = DFHPF4 OR DFHPF16                        G7W2PGM 
01107              THEN                                                 G7W2PGM 
01108                  NEXT SENTENCE                                    G7W2PGM 
01109              ELSE                                                 G7W2PGM 
01110                  MOVE  -1        TO S2ERRL                        G7W2PGM 
01111                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W2PGM 
01112                  SET WT-01-INDEX TO +06                           G7W2PGM 
01113                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W2PGM 
01114          ELSE                                                     G7W2PGM 
01115              NEXT SENTENCE.                                       G7W2PGM 
01116                                                                   G7W2PGM 
01117                                                                   G7W2PGM 
01118                                                                   G7W2PGM 
01119                                                                   G7W2PGM 
01120  2200-900-EXIT.                                                   G7W2PGM 
01121      EXIT.                                                        G7W2PGM 
01122 /***************************************************************  G7W2PGM 
01123 *                                                              *  G7W2PGM 
01124 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7W2PGM 
01125 *        REWRITE TO WORKFILE.                                  *  G7W2PGM 
01126 *                                                              *  G7W2PGM 
01127 ****************************************************************  G7W2PGM 
01128  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7W2PGM 
01129  2300-010.                                                        G7W2PGM 
01130                                                                   G7W2PGM 
01131 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7W2PGM 
01132                                                                   G7W2PGM 
01133      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7W2PGM 
01134      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7W2PGM 
01135        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7W2PGM 
01136      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7W2PGM 
01137      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7W2PGM 
01138      IF  NOT GCIO2-GOOD-RETURN                                    G7W2PGM 
01139          MOVE WS-01-ABCODE-W2F2     TO WS-01-ABCODE               G7W2PGM 
01140          MOVE WS-01-ABCODE-W2F2-MSG TO WS-01-ABCODE-MSG           G7W2PGM 
01141          PERFORM  9999-000-ABEND-THE-TASK.                        G7W2PGM 
01142                                                                   G7W2PGM 
01143                                                                   G7W2PGM 
01144 *----- SAVE SCREEN FIELDS THAT CANNOT BE COMPARED DIRECTLY ------*G7W2PGM 
01145 *           WITH FIELDS IN BENEFIT RECORD.                        G7W2PGM 
01146                                                                   G7W2PGM 
01147                                                                   G7W2PGM 
01148 *    MOVE S2FLPDYI   TO WS-02-FLAT-RATE-PDM-AMT-X.                G7W2PGM 
01149 *    MOVE S2ADALDI   TO WS-02-ADDN-ALLOW-AMT-PER-DAY-X.           G7W2PGM 
01150      MOVE S2TRTMFI   TO WS-02-TREAT-TIME-FACTOR-X.                G7W2PGM 
01151                                                                   G7W2PGM 
01152                                                                   G7W2PGM 
01153                                                                   G7W2PGM 
01154 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7W2PGM 
01155                                                                   G7W2PGM 
01156      MOVE GPW2-FLAT-RATE-PDM-AMT TO                               G7W2PGM 
01157        WS-GPW2-FLAT-RATE-PDM-AMT.                                 G7W2PGM 
01158                                                                   G7W2PGM 
01159      MOVE GPW2-ADDN-ALLOW-AMT-PER-DAY TO                          G7W2PGM 
01160        WS-GPW2-ADDN-ALLOW-AMT-PER-DAY.                            G7W2PGM 
01161                                                                   G7W2PGM 
01162      IF   WS-02-FLAT-RATE-PDM-AMT  = WS-GPW2-FLAT-RATE-PDM-AMT    G7W2PGM 
01163          AND WS-02-ADDN-ALLOW-AMT-PER-DAY                         G7W2PGM 
01164                                  = WS-GPW2-ADDN-ALLOW-AMT-PER-DAY G7W2PGM 
01165          AND S2RRCERI               = GPW2-CERTFN-REPETN-REQRM-INDG7W2PGM 
01166          AND S2TRTMII               = GPW2-TREAT-TIME-FACTOR-IND  G7W2PGM 
01167          AND WS-02-TREAT-TIME-FACTOR = GPW2-TREAT-TIME-FACTOR     G7W2PGM 
01168          AND S2ELMTII               = GPW2-ELIG-METHD-OF-TREAT-INDG7W2PGM 
01169          AND S2PHEXII               = GPW2-PHYS-EXAM-IND          G7W2PGM 
01170          AND S2RHARII               = GPW2-REHAB-ADM-RESTRN-IND   G7W2PGM 
01171      THEN                                                         G7W2PGM 
01172          GO TO 2300-900-EXIT                                      G7W2PGM 
01173      ELSE                                                         G7W2PGM 
01174          NEXT SENTENCE.                                           G7W2PGM 
01175                                                                   G7W2PGM 
01176                                                                   G7W2PGM 
01177 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7W2PGM 
01178                                                                   G7W2PGM 
01179      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7W2PGM 
01180      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7W2PGM 
01181      IF  NOT GCIO2-GOOD-RETURN                                    G7W2PGM 
01182          MOVE WS-01-ABCODE-W2F3     TO WS-01-ABCODE               G7W2PGM 
01183          MOVE WS-01-ABCODE-W2F3-MSG TO WS-01-ABCODE-MSG           G7W2PGM 
01184          PERFORM  9999-000-ABEND-THE-TASK.                        G7W2PGM 
01185                                                                   G7W2PGM 
01186                                                                   G7W2PGM 
01187 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7W2PGM 
01188                                                                   G7W2PGM 
01189      MOVE WS-02-FLAT-RATE-PDM-AMT TO GPW2-FLAT-RATE-PDM-AMT.      G7W2PGM 
01190      MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY                            G7W2PGM 
01191                                   TO GPW2-ADDN-ALLOW-AMT-PER-DAY. G7W2PGM 
01192      MOVE S2RRCERI                TO GPW2-CERTFN-REPETN-REQRM-IND.G7W2PGM 
01193      MOVE S2TRTMII                TO GPW2-TREAT-TIME-FACTOR-IND.  G7W2PGM 
01194      MOVE WS-02-TREAT-TIME-FACTOR TO GPW2-TREAT-TIME-FACTOR.      G7W2PGM 
01195      MOVE S2ELMTII                TO GPW2-ELIG-METHD-OF-TREAT-IND.G7W2PGM 
01196      MOVE S2PHEXII                TO GPW2-PHYS-EXAM-IND.          G7W2PGM 
01197      MOVE S2RHARII                TO GPW2-REHAB-ADM-RESTRN-IND.   G7W2PGM 
01198                                                                   G7W2PGM 
01199                                                                   G7W2PGM 
01200 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7W2PGM 
01201                                                                   G7W2PGM 
01202 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7W2PGM 
01203                                                                   G7W2PGM 
01204      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7W2PGM 
01205      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7W2PGM 
01206      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7W2PGM 
01207      IF  NOT GCIO2-GOOD-RETURN                                    G7W2PGM 
01208          MOVE WS-01-ABCODE-W2F4     TO WS-01-ABCODE               G7W2PGM 
01209          MOVE WS-01-ABCODE-W2F4-MSG TO WS-01-ABCODE-MSG           G7W2PGM 
01210          PERFORM  9999-000-ABEND-THE-TASK.                        G7W2PGM 
01211                                                                   G7W2PGM 
01212  2300-900-EXIT.                                                   G7W2PGM 
01213      EXIT.                                                        G7W2PGM 
01214 /***************************************************************  G7W2PGM 
01215 *                                                              *  G7W2PGM 
01216 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7W2PGM 
01217 *                                                              *  G7W2PGM 
01218 ****************************************************************  G7W2PGM 
01219  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7W2PGM 
01220  2310-010.                                                        G7W2PGM 
01221                                                                   G7W2PGM 
01222                                                                   G7W2PGM 
01223 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7W2PGM 
01224                                                                   G7W2PGM 
01225      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7W2PGM 
01226                                         + GC-WORKFILE-KEY-LEN     G7W2PGM 
01227                                         + GC-GCBENPRV-MAX-REC-LEN.G7W2PGM 
01228                                                                   G7W2PGM 
01229      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7W2PGM 
01230                          INITIMG(WS-02-HEX-00)                    G7W2PGM 
01231                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7W2PGM 
01232                          END-EXEC.                                G7W2PGM 
01233                                                                   G7W2PGM 
01234 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7W2PGM 
01235                                                                   G7W2PGM 
01236      MOVE SPACES                 TO GCIO-CONTRACT-FILE-KEY.       G7W2PGM 
01237      MOVE WRK-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           G7W2PGM 
01238      MOVE WRK-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3.        G7W2PGM 
01239      MOVE WRK-SEC-NO-1           TO GCIO-WRK-SEC-NO-1.            G7W2PGM 
01240      MOVE WRK-PKG-CODE           TO GCIO-WRK-PKG-CODE.            G7W2PGM 
01241      MOVE WRK-EFFECTIVE-DATE     TO GCIO-WRK-EFFECTIVE-DT.        G7W2PGM 
01242                                                                   G7W2PGM 
01243      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7W2PGM 
01244      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7W2PGM 
01245      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7W2PGM 
01246      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7W2PGM 
01247      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7W2PGM 
01248      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7W2PGM 
01249      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7W2PGM 
01250      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7W2PGM 
01251      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7W2PGM 
01252                                                                   G7W2PGM 
01253 *    MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7W2PGM 
01254 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7W2PGM 
01255 *    IF  HGADATE-RETURN = ZEROS                                   G7W2PGM 
01256 *    THEN                                                         G7W2PGM 
01257 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7W2PGM 
01258 *    ELSE                                                         G7W2PGM 
01259 *        SET WT-01-INDEX TO +07                                   G7W2PGM 
01260 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W2PGM 
01261 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7W2PGM 
01262                                                                   G7W2PGM 
01263      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7W2PGM 
01264      MOVE S2BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7W2PGM 
01265      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7W2PGM 
01266      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7W2PGM 
01267      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7W2PGM 
01268      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7W2PGM 
01269      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7W2PGM 
01270                                                                   G7W2PGM 
01271                                                                   G7W2PGM 
01272  2310-900-EXIT.                                                   G7W2PGM 
01273      EXIT.                                                        G7W2PGM 
01274 /***************************************************************  G7W2PGM 
01275 *                                                              *  G7W2PGM 
01276 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7W2PGM 
01277 *                                                              *  G7W2PGM 
01278 ****************************************************************  G7W2PGM 
01279  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7W2PGM 
01280  2400-010.                                                        G7W2PGM 
01281                                                                   G7W2PGM 
01282                                                                   G7W2PGM 
01283      IF  EIBAID = DFHPF7  OR DFHPF19                              G7W2PGM 
01284      THEN                                                         G7W2PGM 
01285          MOVE 'G7W1PGM' TO WS-02-NEXT-PROGRAM.                    G7W2PGM 
01286                                                                   G7W2PGM 
01287      IF  EIBAID = DFHENTER OR                                     G7W2PGM 
01288                   DFHPF4   OR DFHPF16 OR                          G7W2PGM 
01289                   DFHPF8   OR DFHPF20                             G7W2PGM 
01290      THEN                                                         G7W2PGM 
01291          MOVE 'GC6APGM' TO WS-02-NEXT-PROGRAM.                    G7W2PGM 
01292                                                                   G7W2PGM 
01293      IF  EIBAID = DFHPF6  OR DFHPF18                              G7W2PGM 
01294      THEN                                                         G7W2PGM 
01295          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7W2PGM 
01296                                                                   G7W2PGM 
01297                                                                   G7W2PGM 
01298      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7W2PGM 
01299                       COMMAREA(WORK-RECORD-2)                     G7W2PGM 
01300                       LENGTH  (GCIO2-RECORD-LENGTH)               G7W2PGM 
01301                       END-EXEC.                                   G7W2PGM 
01302                                                                   G7W2PGM 
01303  2400-900-EXIT.                                                   G7W2PGM 
01304      EXIT.                                                        G7W2PGM 
01305 /***************************************************************  G7W2PGM 
01306 *                                                              *  G7W2PGM 
01307 * 2500   LINK TO GX3APGM FOR CONVERSION                           G7W2PGM 
01308 *                                                              *  G7W2PGM 
01309 ****************************************************************  G7W2PGM 
01310  2500-LINK-TO-GX3APGM.                                            G7W2PGM 
01311                                                                   G7W2PGM 
01312      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7W2PGM 
01313                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7W2PGM 
01314                       LENGTH  (+51)                               G7W2PGM 
01315                       END-EXEC.                                   G7W2PGM 
01316                                                                   G7W2PGM 
01317  2500-EXIT.                                                       G7W2PGM 
01318      EXIT.                                                        G7W2PGM 
01319 /***************************************************************  G7W2PGM 
01320 *                                                              *  G7W2PGM 
01321 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7W2PGM 
01322 *         PROVISION RECORD (TYPE=C4)                           *  G7W2PGM 
01323 *                                                              *  G7W2PGM 
01324 ****************************************************************  G7W2PGM 
01325  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7W2PGM 
01326  5000-010.                                                        G7W2PGM 
01327                                                                   G7W2PGM 
01328      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7W2PGM 
01329                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7W2PGM 
01330                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7W2PGM 
01331                       END-EXEC.                                   G7W2PGM 
01332                                                                   G7W2PGM 
01333                                                                   G7W2PGM 
01334  5000-900-EXIT.                                                   G7W2PGM 
01335      EXIT.                                                        G7W2PGM 
01336 /***************************************************************  G7W2PGM 
01337 *                                                              *  G7W2PGM 
01338 * 5100                                                         *  G7W2PGM 
01339 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7W2PGM 
01340 *                                                              *  G7W2PGM 
01341 ****************************************************************  G7W2PGM 
01342  5100-000-W-F-CONTRACT-IO       SECTION.                          G7W2PGM 
01343  5100-010.                                                        G7W2PGM 
01344                                                                   G7W2PGM 
01345      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7W2PGM 
01346                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7W2PGM 
01347                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7W2PGM 
01348                       END-EXEC.                                   G7W2PGM 
01349                                                                   G7W2PGM 
01350                                                                   G7W2PGM 
01351  5100-900-EXIT.                                                   G7W2PGM 
01352      EXIT.                                                        G7W2PGM 
01353 /***************************************************************  G7W2PGM 
01354 *                                                              *  G7W2PGM 
01355 * 9000   MOVE MESSAGE TO SCREEN                                *  G7W2PGM 
01356 *                                                              *  G7W2PGM 
01357 ****************************************************************  G7W2PGM 
01358  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7W2PGM 
01359  9000-010.                                                        G7W2PGM 
01360                                                                   G7W2PGM 
01361      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S2ERRO.              G7W2PGM 
01362                                                                   G7W2PGM 
01363  9000-900-EXIT.                                                   G7W2PGM 
01364      EXIT.                                                        G7W2PGM 
01365 /***************************************************************  G7W2PGM 
01366 *                                                              *  G7W2PGM 
01367 * 9100 SEND SCREEN AND RETURN                                  *  G7W2PGM 
01368 *                                                              *  G7W2PGM 
01369 ****************************************************************  G7W2PGM 
01370  9100-000-SEND-THEN-RETURN      SECTION.                          G7W2PGM 
01371  9100-010.                                                        G7W2PGM 
01372                                                                   G7W2PGM 
01373                                                                   G7W2PGM 
01374 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7W2PGM 
01375      MOVE  -1 TO  S2ERRL.                                         G7W2PGM 
01376                                                                   G7W2PGM 
01377                                                                   G7W2PGM 
01378      IF  WS-02-MY-EIBTRNID                                        G7W2PGM 
01379      THEN                                                         G7W2PGM 
01380          EXEC CICS  SEND MAP   ('G7W2I01')                        G7W2PGM 
01381                          MAPSET('G7W2SET')                        G7W2PGM 
01382                          DATAONLY                                 G7W2PGM 
01383                          CURSOR                                   G7W2PGM 
01384                          END-EXEC                                 G7W2PGM 
01385      ELSE                                                         G7W2PGM 
01386          EXEC CICS  SEND MAP   ('G7W2I01')                        G7W2PGM 
01387                          MAPSET('G7W2SET')                        G7W2PGM 
01388                          ERASE                                    G7W2PGM 
01389                          CURSOR                                   G7W2PGM 
01390                          END-EXEC.                                G7W2PGM 
01391                                                                   G7W2PGM 
01392      EXEC CICS RETURN                                             G7W2PGM 
01393                TRANSID  ('G7W2')                                  G7W2PGM 
01394                COMMAREA (DFHCOMMAREA)                             G7W2PGM 
01395                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7W2PGM 
01396                END-EXEC.                                          G7W2PGM 
01397                                                                   G7W2PGM 
01398 *    EXEC CICS  RETURN                                            G7W2PGM 
01399 *               END-EXEC.                                         G7W2PGM 
01400 *                                                                 G7W2PGM 
01401 *                                                                 G7W2PGM 
01402  9100-900-EXIT.                                                   G7W2PGM 
01403      EXIT.                                                        G7W2PGM 
01404 /*****************************************************************G7W2PGM 
01405 *                                                                *G7W2PGM 
01406 * 9200    XCTL TO GCPSPGM                                        *G7W2PGM 
01407 *                                                                *G7W2PGM 
01408 *                                                                *G7W2PGM 
01409 ******************************************************************G7W2PGM 
01410  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7W2PGM 
01411  9200-010.                                                        G7W2PGM 
01412                                                                   G7W2PGM 
01413      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7W2PGM 
01414                       END-EXEC.                                   G7W2PGM 
01415                                                                   G7W2PGM 
01416  9200-900-EXIT.                                                   G7W2PGM 
01417      EXIT.                                                        G7W2PGM 
01418 /*****************************************************************G7W2PGM 
01419 *                                                                *G7W2PGM 
01420 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7W2PGM 
01421 *                                                                *G7W2PGM 
01422 *                                                                *G7W2PGM 
01423 ******************************************************************G7W2PGM 
01424  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7W2PGM 
01425  9210-010.                                                        G7W2PGM 
01426                                                                   G7W2PGM 
01427      IF  S2GRPNOI = '000SPS000'                                   G7W2PGM 
01428          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7W2PGM 
01429                           END-EXEC.                               G7W2PGM 
01430                                                                   G7W2PGM 
01431 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7W2PGM 
01432                                                                   G7W2PGM 
01433      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7W2PGM 
01434                                        + GC-WORKFILE-KEY-LEN      G7W2PGM 
01435                                        + GC-GCCONTR-MAX-REC-LEN.  G7W2PGM 
01436                                                                   G7W2PGM 
01437      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7W2PGM 
01438                          INITIMG(WS-02-HEX-00)                    G7W2PGM 
01439                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7W2PGM 
01440                          END-EXEC.                                G7W2PGM 
01441                                                                   G7W2PGM 
01442 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7W2PGM 
01443 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7W2PGM 
01444                                                                   G7W2PGM 
01445 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7W2PGM 
01446                                                                   G7W2PGM 
01447      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7W2PGM 
01448        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7W2PGM 
01449                                                                   G7W2PGM 
01450      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7W2PGM 
01451      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7W2PGM 
01452                                                                   G7W2PGM 
01453      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7W2PGM 
01454      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7W2PGM 
01455      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7W2PGM 
01456      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7W2PGM 
01457      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7W2PGM 
01458      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7W2PGM 
01459      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7W2PGM 
01460      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7W2PGM 
01461      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7W2PGM 
01462      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7W2PGM 
01463      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7W2PGM 
01464      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7W2PGM 
01465      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7W2PGM 
01466      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7W2PGM 
01467                                                                   G7W2PGM 
01468 *    MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7W2PGM 
01469 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7W2PGM 
01470 *    IF  HGADATE-RETURN = ZEROS                                   G7W2PGM 
01471 *    THEN                                                         G7W2PGM 
01472 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7W2PGM 
01473 *    ELSE                                                         G7W2PGM 
01474 *        SET WT-01-INDEX TO +07                                   G7W2PGM 
01475 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W2PGM 
01476 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7W2PGM 
01477                                                                   G7W2PGM 
01478      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7W2PGM 
01479      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7W2PGM 
01480      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7W2PGM 
01481      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7W2PGM 
01482      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7W2PGM 
01483      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7W2PGM 
01484      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7W2PGM 
01485                                                                   G7W2PGM 
01486      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7W2PGM 
01487                                                                   G7W2PGM 
01488      IF  NOT GCIO3-GOOD-RETURN                                    G7W2PGM 
01489          MOVE WS-01-ABCODE-W2F1     TO WS-01-ABCODE               G7W2PGM 
01490          MOVE WS-01-ABCODE-W2F1-MSG TO WS-01-ABCODE-MSG           G7W2PGM 
01491          PERFORM  9999-000-ABEND-THE-TASK.                        G7W2PGM 
01492                                                                   G7W2PGM 
01493      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7W2PGM 
01494                       COMMAREA(WORK-RECORD-3)                     G7W2PGM 
01495                       LENGTH  (GCIO3-RECORD-LENGTH)               G7W2PGM 
01496                       END-EXEC.                                   G7W2PGM 
01497                                                                   G7W2PGM 
01498  9210-900-EXIT.                                                   G7W2PGM 
01499      EXIT.                                                        G7W2PGM 
01500 /*****************************************************************G7W2PGM 
01501 *                                                                *G7W2PGM 
01502 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7W2PGM 
01503 *                                                                *G7W2PGM 
01504 *                                                                *G7W2PGM 
01505 ******************************************************************G7W2PGM 
01506  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7W2PGM 
01507  9220-010.                                                        G7W2PGM 
01508                                                                   G7W2PGM 
01509      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7W2PGM 
01510                       END-EXEC.                                   G7W2PGM 
01511                                                                   G7W2PGM 
01512  9220-900-EXIT.                                                   G7W2PGM 
01513      EXIT.                                                        G7W2PGM 
01514 /*****************************************************************G7W2PGM 
01515 *                                                                *G7W2PGM 
01516 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7W2PGM 
01517 *                                                                *G7W2PGM 
01518 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7W2PGM 
01519 *                                                                *G7W2PGM 
01520 ******************************************************************G7W2PGM 
01521  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7W2PGM 
01522  9800-010.                                                        G7W2PGM 
01523                                                                   G7W2PGM 
01524      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7W2PGM 
01525      MOVE 'M'   TO  HGADATE-FORM1.                                G7W2PGM 
01526      MOVE 'J'   TO  HGADATE-FORM2.                                G7W2PGM 
01527      MOVE ZEROS TO  HGADATE-RETURN                                G7W2PGM 
01528                     HGADATE-AMOUNT.                               G7W2PGM 
01529      EXEC CICS LINK PROGRAM ('HGADATES')                          G7W2PGM 
01530                     COMMAREA(HGADATES-COMMAREA)                   G7W2PGM 
01531                     LENGTH  (24)                                  G7W2PGM 
01532                     END-EXEC.                                     G7W2PGM 
01533                                                                   G7W2PGM 
01534  9800-900-900-EXIT.                                               G7W2PGM 
01535      EXIT.                                                        G7W2PGM 
01536 /*****************************************************************G7W2PGM 
01537 *                                                                *G7W2PGM 
01538 * 9810    J U L I A N    T O    G R E G O R I A N                *G7W2PGM 
01539 *                                                                *G7W2PGM 
01540 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7W2PGM 
01541 *                                                                *G7W2PGM 
01542 ******************************************************************G7W2PGM 
01543  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7W2PGM 
01544  9810-010.                                                        G7W2PGM 
01545                                                                   G7W2PGM 
01546      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7W2PGM 
01547      MOVE 'J'   TO  HGADATE-FORM1.                                G7W2PGM 
01548      MOVE 'M'   TO  HGADATE-FORM2.                                G7W2PGM 
01549      MOVE ZEROS TO  HGADATE-RETURN                                G7W2PGM 
01550                     HGADATE-AMOUNT.                               G7W2PGM 
01551      EXEC CICS LINK PROGRAM ('HGADATES')                          G7W2PGM 
01552                     COMMAREA(HGADATES-COMMAREA)                   G7W2PGM 
01553                     LENGTH  (24)                                  G7W2PGM 
01554                     END-EXEC.                                     G7W2PGM 
01555                                                                   G7W2PGM 
01556  9810-900-900-EXIT.                                               G7W2PGM 
01557      EXIT.                                                        G7W2PGM 
01558 /***************************************************************  G7W2PGM 
01559 *                                                              *  G7W2PGM 
01560 * 9999  ABEND THE TASK                                         *  G7W2PGM 
01561 *                                                              *  G7W2PGM 
01562 ****************************************************************  G7W2PGM 
01563  9999-000-ABEND-THE-TASK SECTION.                                 G7W2PGM 
01564  9999-010.                                                        G7W2PGM 
01565                                                                   G7W2PGM 
01566      EXEC CICS  ABEND                                             G7W2PGM 
01567                 ABCODE(WS-01-ABCODE)                              G7W2PGM 
01568                 END-EXEC.                                         G7W2PGM 
01569                                                                   G7W2PGM 
01570  9900-900-EXIT.                                                   G7W2PGM 
01571      EXIT.                                                        G7W2PGM 
