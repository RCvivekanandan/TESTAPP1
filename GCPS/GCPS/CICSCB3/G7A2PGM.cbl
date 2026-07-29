00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7A2PGM.                                         G7A2PGM 
00003 ***  THIS IA A COBOL II PROGRAM                                      LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7A2PGM 
00005  DATE-WRITTEN.   03/06/87.                                        G7A2PGM 
00006  DATE-COMPILED.                                                   G7A2PGM 
00007 ***************************************************************** G7A2PGM 
00008 *                                                               * G7A2PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7A2PGM 
00010 *                                                               * G7A2PGM 
00011 *                                                               * G7A2PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7A2PGM 
00013 *                                                               * G7A2PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7A2PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7A2PGM 
00016 *                             (SAME AS 'GC5A')                  * G7A2PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7A2PGM 
00018 *                              'GC5A')                          * G7A2PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7A2PGM 
00020 *                              PROVISION)                       * G7A2PGM 
00021 *                                                               * G7A2PGM 
00022 *  D116       7-15-87  FRY   CAUSE GCIOPGM TO CALL GX5ZPGM TO   * G7A2PGM 
00023 *                            UPDATE OPERATOR-ID IN W/F RECORD   * G7A2PGM 
00024 *                            WHEN 'C4' RECORD IS MODIFIED.      * G7A2PGM 
00025 *                                                               * G7A2PGM 
00026 *  D12009     9-30-91  GDM   1. CONVERT TO COBOL II             * G7A2PGM 
00027 *                            2. REMOVE PF12/24 LOGIC            * G7A2PGM 
00028 *                                                               * G7A2PGM 
00029 * 14726/     03/27/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7A2PGM 
00030 * 15057                     INCREASED GROUP AND SECTION ON      * G7A2PGM 
00031 *                           THE SCREEN.                         * G7A2PGM 
00032 *                                                               * G7A2PGM 
00033 *            12/11/02  AKK  COMPILE FOR OPID                    * G7A2PGM 
00034 *                                                               * G7A2PGM 
00035 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7A2PGM 
00036 *                           G7A2SET                              *G7A2PGM 
00037 ***************************************************************** G7A2PGM 
00038                                                                   G7A2PGM 
00039 ***************************************************************** G7A2PGM 
00040 *                                                               * G7A2PGM 
00041 *    G7A2PGM  - PROGRAM 2 OF 2 PROGRAMS TO UPDATE THE FORMAT 'A'* G7A2PGM 
00042 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7A2PGM 
00043 *                                                               * G7A2PGM 
00044 *    TRANSID: G7A2                                              * G7A2PGM 
00045 *    MAPSET:  G7A2SETC    (GIA2PGM WHICH SHARES THIS MAP)       * G7A2PGM 
00046 *    VALGEN:  NONE                                              * G7A2PGM 
00047 *                                                               * G7A2PGM 
00048 *    PROGRAM NARRATIVE:                                         * G7A2PGM 
00049 *                                                               * G7A2PGM 
00050 *        PROGRAM CHECKS FOR TRANS CODE 'G7A2'.  AN INVALID      * G7A2PGM 
00051 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7A2PGM 
00052 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7A2PGM 
00053 *                                                               * G7A2PGM 
00054 *        THE MAIN FUNCTIONS ARE :                               * G7A2PGM 
00055 *        1. HARDCOPY REQUEST,                                   * G7A2PGM 
00056 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7A2PGM 
00057 *           USER,                                               * G7A2PGM 
00058 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7A2PGM 
00059 *                                                               * G7A2PGM 
00060 *        HARDCOPY REQUEST                                       * G7A2PGM 
00061 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7A2PGM 
00062 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7A2PGM 
00063 *           THE SCREEN BUFFER.                                  * G7A2PGM 
00064 *                                                               * G7A2PGM 
00065 *        PROCESS INPUT DATA (UPDATE).                           * G7A2PGM 
00066 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7A2PGM 
00067 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7A2PGM 
00068 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7A2PGM 
00069 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7A2PGM 
00070 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7A2PGM 
00071 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7A2PGM 
00072 *                                                               * G7A2PGM 
00073 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7A2PGM 
00074 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7A2PGM 
00075 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7A2PGM 
00076 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7A2PGM 
00077 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7A2PGM 
00078 *                                                               * G7A2PGM 
00079 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7A2PGM 
00080 *                                     -----------------------   * G7A2PGM 
00081 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7A2PGM 
00082 *           -----------------------------------------------     * G7A2PGM 
00083 *                                                               * G7A2PGM 
00084 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7A2PGM 
00085 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7A2PGM 
00086 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7A2PGM 
00087 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7A2PGM 
00088 *           SENT BACK TO THE USER.                              * G7A2PGM 
00089 *                                                               * G7A2PGM 
00090 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7A2PGM 
00091 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7A2PGM 
00092 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7A2PGM 
00093 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7A2PGM 
00094 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7A2PGM 
00095 *           PF KEY LOGIC,                                       * G7A2PGM 
00096 *              PF6/PF18       GOES TO GC8APGM                   * G7A2PGM 
00097 *              PF8/PF20/ENTER GOES TO GC6APGM                   * G7A2PGM 
00098 *              FOR PF7/PF19   GOES TO G7A1PGM                   * G7A2PGM 
00099 *                                                               * G7A2PGM 
00100 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7A2PGM 
00101 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7A2PGM 
00102 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7A2PGM 
00103 *                                                               * G7A2PGM 
00104 ***************************************************************** G7A2PGM 
00105                                                                   G7A2PGM 
00106  ENVIRONMENT DIVISION.                                            G7A2PGM 
00107  DATA DIVISION.                                                   G7A2PGM 
00108 /                                                                 G7A2PGM 
00109  WORKING-STORAGE SECTION.                                         G7A2PGM 
00110  01  WS-BEGIN                    PIC X(58) VALUE                  G7A2PGM 
00111      '*** G7A2PGM  WORKING-STORAGE BEGINS HERE ***'.              G7A2PGM 
00112                                                                   G7A2PGM 
00113                                                                   G7A2PGM 
00114  01  WS-01-ABEND-AREA.                                            G7A2PGM 
00115      05  FILLER                   PIC X(16)  VALUE                G7A2PGM 
00116          '** ABEND AREA **'.                                      G7A2PGM 
00117                                                                   G7A2PGM 
00118      05  WS-01-ABEND-CODES-AND-MSG.                               G7A2PGM 
00119          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7A2PGM 
00120          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7A2PGM 
00121                                                                   G7A2PGM 
00122          10  WS-01-ABCODE-A2F1          PIC X(04)  VALUE  'A2F1'. G7A2PGM 
00123          10  WS-01-ABCODE-A2F1-MSG      PIC X(44)  VALUE          G7A2PGM 
00124             'W/F CONTRACT CANNOT BE FOUND             '.          G7A2PGM 
00125                                                                   G7A2PGM 
00126          10  WS-01-ABCODE-A2F2          PIC X(04)  VALUE  'A2F2'. G7A2PGM 
00127          10  WS-01-ABCODE-A2F2-MSG      PIC X(44)  VALUE          G7A2PGM 
00128             'W/F BEN PROV CANNOT BE FOUND             '.          G7A2PGM 
00129                                                                   G7A2PGM 
00130          10  WS-01-ABCODE-A2F3          PIC X(04)  VALUE  'A2F3'. G7A2PGM 
00131          10  WS-01-ABCODE-A2F3-MSG      PIC X(44)  VALUE          G7A2PGM 
00132             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7A2PGM 
00133                                                                   G7A2PGM 
00134          10  WS-01-ABCODE-A2F4          PIC X(04)  VALUE  'A2F4'. G7A2PGM 
00135          10  WS-01-ABCODE-A2F4-MSG      PIC X(44)  VALUE          G7A2PGM 
00136             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7A2PGM 
00137                                                                   G7A2PGM 
00138          10  WS-01-ABCODE-A2L1          PIC X(04)  VALUE  'A2L1'. G7A2PGM 
00139          10  WS-01-ABCODE-A2L1-MSG      PIC X(44)  VALUE          G7A2PGM 
00140             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7A2PGM 
00141                                                                   G7A2PGM 
00142          10  WS-01-ABCODE-A2P1          PIC X(04)  VALUE  'A2P1'. G7A2PGM 
00143          10  WS-01-ABCODE-A2P1-MSG      PIC X(44)  VALUE          G7A2PGM 
00144             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7A2PGM 
00145                                                                   G7A2PGM 
00146          10  WS-01-ABCODE-A2P2          PIC X(04)  VALUE  'A2P2'. G7A2PGM 
00147          10  WS-01-ABCODE-A2P2-MSG      PIC X(44)  VALUE          G7A2PGM 
00148             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7A2PGM 
00149                                                                   G7A2PGM 
00150  01  WS-02-AREA.                                                  G7A2PGM 
00151      05  FILLER                   PIC X(16)  VALUE                G7A2PGM 
00152          '** WS-02-AREA **'.                                      G7A2PGM 
00153      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7A2PGM 
00154          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7A2PGM 
00155                                                    'GC6A' 'G7A1'  G7A2PGM 
00156                                                    'G7A2'.        G7A2PGM 
00157          88  WS-02-MY-EIBTRNID                     VALUE  'G7A2'. G7A2PGM 
00158                                                                   G7A2PGM 
00159      05  WS-02-COMPUTED-LENGTHS.                                  G7A2PGM 
00160          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7A2PGM 
00161          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7A2PGM 
00162          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7A2PGM 
00163                                                                   G7A2PGM 
00164      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7A2PGM 
00165                                                                   G7A2PGM 
00166      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7A2PGM 
00167                                                                   G7A2PGM 
00168      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7A2PGM 
00169      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7A2PGM 
00170          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7A2PGM 
00171                                         PIC X.                    G7A2PGM 
00172          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7A2PGM 
00173                                                    '0' THRU '9'   G7A2PGM 
00174                                                    'A' THRU 'Z'   G7A2PGM 
00175                                                    SPACE.         G7A2PGM 
00176                                                                   G7A2PGM 
00177      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7A2PGM 
00178          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7A2PGM 
00179          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7A2PGM 
00180                                                                   G7A2PGM 
00181      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7A2PGM 
00182          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7A2PGM 
00183                                                                   G7A2PGM 
00184      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7A2PGM 
00185 /                                                                 G7A2PGM 
00186  01  WT-00-G7A2PGM-TABLES.                                        G7A2PGM 
00187      05  FILLER                   PIC X(16)  VALUE                G7A2PGM 
00188          '*G7A2PGM TABLES*'.                                      G7A2PGM 
00189                                                                   G7A2PGM 
00190  01  WT-01-TABLE.                                                 G7A2PGM 
00191      05  FILLER                  PIC X(16) VALUE                  G7A2PGM 
00192          '* WT-01-TABLE  *'.                                      G7A2PGM 
00193 ******************************************************************G7A2PGM 
00194 *    WT-01   MESSAGE TABLE                                       *G7A2PGM 
00195 ******************************************************************G7A2PGM 
00196  01  FILLER.                                                      G7A2PGM 
00197      05  WT-01-MESSAGE-VALUES.                                    G7A2PGM 
00198                                                                   G7A2PGM 
00199 *----------------------------------------------------------------*G7A2PGM 
00200          10  WT-01-ENTRY-001.                                     G7A2PGM 
00201              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00202              15  WT-01-MESSAGE-TEXT-001.                          G7A2PGM 
00203                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00204                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00205                  20  FILLER          PIC X(3)  VALUE  '001'.      G7A2PGM 
00206                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00207                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00208                      ' INVALID PFKEY SELECTION                    G7A2PGM 
00209 -                    '                         '.                 G7A2PGM 
00210              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00211                                                                   G7A2PGM 
00212 *----------------------------------------------------------------*G7A2PGM 
00213          10  WT-01-ENTRY-002.                                     G7A2PGM 
00214              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00215              15  WT-01-MESSAGE-TEXT-002.                          G7A2PGM 
00216                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00217                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00218                  20  FILLER          PIC X(3)  VALUE  '002'.      G7A2PGM 
00219                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00220                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00221                      '********** F U T U R E   U S E *************G7A2PGM 
00222 -                    '*************************'.                 G7A2PGM 
00223              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00224                                                                   G7A2PGM 
00225 *----------------------------------------------------------------*G7A2PGM 
00226          10  WT-01-ENTRY-003.                                     G7A2PGM 
00227              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00228              15  WT-01-MESSAGE-TEXT-003.                          G7A2PGM 
00229                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00230                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00231                  20  FILLER          PIC X(3)  VALUE  '003'.      G7A2PGM 
00232                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00233                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00234                      '********** F U T U R E   U S E *************G7A2PGM 
00235 -                    '*************************'.                 G7A2PGM 
00236              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00237                                                                   G7A2PGM 
00238 *----------------------------------------------------------------*G7A2PGM 
00239          10  WT-01-ENTRY-004.                                     G7A2PGM 
00240              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00241              15  WT-01-MESSAGE-TEXT-004.                          G7A2PGM 
00242                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00243                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00244                  20  FILLER          PIC X(3)  VALUE  '004'.      G7A2PGM 
00245                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00246                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00247                      '********** F U T U R E   U S E *************G7A2PGM 
00248 -                    '*************************'.                 G7A2PGM 
00249              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00250                                                                   G7A2PGM 
00251 *----------------------------------------------------------------*G7A2PGM 
00252          10  WT-01-ENTRY-005.                                     G7A2PGM 
00253              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00254              15  WT-01-MESSAGE-TEXT-005.                          G7A2PGM 
00255                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00256                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00257                  20  FILLER          PIC X(3)  VALUE  '005'.      G7A2PGM 
00258                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00259                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00260                      '********** F U T U R E   U S E *************G7A2PGM 
00261 -                    '*************************'.                 G7A2PGM 
00262              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00263                                                                   G7A2PGM 
00264 *----------------------------------------------------------------*G7A2PGM 
00265          10  WT-01-ENTRY-006.                                     G7A2PGM 
00266              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00267              15  WT-01-MESSAGE-TEXT-006.                          G7A2PGM 
00268                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00269                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00270                  20  FILLER          PIC X(3)  VALUE  '006'.      G7A2PGM 
00271                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00272                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00273                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7A2PGM 
00274 -                    ' PF4/PF16 TO CONTINUE    '.                 G7A2PGM 
00275              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00276                                                                   G7A2PGM 
00277 *----------------------------------------------------------------*G7A2PGM 
00278          10  WT-01-ENTRY-007.                                     G7A2PGM 
00279              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00280              15  WT-01-MESSAGE-TEXT-007.                          G7A2PGM 
00281                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00282                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00283                  20  FILLER          PIC X(3)  VALUE  '007'.      G7A2PGM 
00284                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00285                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00286                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7A2PGM 
00287 -                    'E CALL SYSTEMS           '.                 G7A2PGM 
00288              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00289                                                                   G7A2PGM 
00290 *----------------------------------------------------------------*G7A2PGM 
00291          10  WT-01-ENTRY-008.                                     G7A2PGM 
00292              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00293              15  WT-01-MESSAGE-TEXT-008.                          G7A2PGM 
00294                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00295                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00296                  20  FILLER          PIC X(3)  VALUE  '008'.      G7A2PGM 
00297                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00298                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00299                      'FIELD HAS AN INVALID VALUE                  G7A2PGM 
00300 -                    '                         '.                 G7A2PGM 
00301              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00302                                                                   G7A2PGM 
00303 *----------------------------------------------------------------*G7A2PGM 
00304          10  WT-01-ENTRY-009.                                     G7A2PGM 
00305              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00306              15  WT-01-MESSAGE-TEXT-009.                          G7A2PGM 
00307                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00308                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00309                  20  FILLER          PIC X(3)  VALUE  '009'.      G7A2PGM 
00310                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00311                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00312                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7A2PGM 
00313 -                    'YSTEM)                   '.                 G7A2PGM 
00314              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00315                                                                   G7A2PGM 
00316 *----------------------------------------------------------------*G7A2PGM 
00317          10  WT-01-ENTRY-010.                                     G7A2PGM 
00318              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A2PGM 
00319              15  WT-01-MESSAGE-TEXT-010.                          G7A2PGM 
00320                  20  FILLER          PIC X(4)  VALUE  'G7A2'.     G7A2PGM 
00321                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A2PGM 
00322                  20  FILLER          PIC X(3)  VALUE  '010'.      G7A2PGM 
00323                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A2PGM 
00324                  20  FILLER          PIC X(70) VALUE              G7A2PGM 
00325                      '********** F U T U R E   U S E *************G7A2PGM 
00326 -                    '*************************'.                 G7A2PGM 
00327              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A2PGM 
00328 *----------------------------------------------------------------*G7A2PGM 
00329                                                                   G7A2PGM 
00330      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7A2PGM 
00331          WT-01-MESSAGE-VALUES         OCCURS 010 TIMES            G7A2PGM 
00332                                      INDEXED BY WT-01-INDEX.      G7A2PGM 
00333          10  WT-01-ENTRY.                                         G7A2PGM 
00334              15  FILLER              PIC X(02).                   G7A2PGM 
00335              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7A2PGM 
00336              15  FILLER              PIC X(02).                   G7A2PGM 
00337                                                                   G7A2PGM 
00338                                                                   G7A2PGM 
00339 /*** MAP FIELD ATTRIBUTES                                         G7A2PGM 
00340  COPY DFHBMSCA.                                                   G7A2PGM 
00341 *                         AUTOSKIP, BRIGHT, FSET                  G7A2PGM 
00342      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7A2PGM 
00343                                                                   G7A2PGM 
00344 /*** ATTENTION KEYS                                               G7A2PGM 
00345  COPY DFHAID.                                                     G7A2PGM 
00346                                                                   G7A2PGM 
00347 /***  PROVISION MAINTENANCE SCREEN                                G7A2PGM 
00348  COPY  G7A2SETC.                                                  G7A2PGM 
00349                                                                   G7A2PGM 
00350 /*** DATE ROUTINE COMMAREA                                        G7A2PGM 
00351  01  HGADATES-COMMAREA.                                           G7A2PGM 
00352  COPY HGCDAT01.                                                   G7A2PGM 
00353                                                                   G7A2PGM 
00354 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7A2PGM 
00355  01  GCVIOPGM-PARM-LIST.                                          G7A2PGM 
00356  COPY GCVINTRC.                                                   G7A2PGM 
00357                                                                   G7A2PGM 
00358 /*** ALTERNATIVE WORKFILE KEYS                                    G7A2PGM 
00359  01  FILLER.                                                      G7A2PGM 
00360      COPY GCWRKKEY.                                               G7A2PGM 
00361                                                                   G7A2PGM 
00362 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7A2PGM 
00363  01  FILLER.                                                      G7A2PGM 
00364      COPY GCCDRLEN.                                               G7A2PGM 
00365                                                                   G7A2PGM 
00366                                                                   G7A2PGM 
00367  01  WS-END                       PIC X(58) VALUE                 G7A2PGM 
00368      '*** G7A2PGM  WORKING-STORAGE ENDS HERE ***'.                G7A2PGM 
00369 /                                                                 G7A2PGM 
00370  LINKAGE SECTION.                                                 G7A2PGM 
00371 /                                                                 G7A2PGM 
00372  01  DFHCOMMAREA.                                                 G7A2PGM 
00373      COPY  GCWRKDCC.                                              G7A2PGM 
00374      COPY  GCBENPVC.                                              G7A2PGM 
00375 /                                                                 G7A2PGM 
00376 *01  BLL-CELLS.                                                   G7A2PGM 
00377 *    05  FILLER                   PIC S9(08)  COMP.               G7A2PGM 
00378 *    05  BEN-PROV-PNTR            PIC S9(08)  COMP.               G7A2PGM 
00379 *    05  CONTRACT-PNTR            PIC S9(08)  COMP.               G7A2PGM 
00380 *    05  CONTRACT-PNTR-2          PIC S9(08)  COMP.               G7A2PGM 
00381 *                                                                 G7A2PGM 
00382 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7A2PGM 
00383  01  IO-PARM-BEN-PROV-AREA.                                       G7A2PGM 
00384      COPY  GCIOPRM2.                                              G7A2PGM 
00385      COPY  GCWRKDC2.                                              G7A2PGM 
00386      COPY  GCBENPV2.                                              G7A2PGM 
00387                                                                   G7A2PGM 
00388 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7A2PGM 
00389  01  IO-PARM-CONTRACT-AREA.                                       G7A2PGM 
00390      COPY  GCIOPRM3.                                              G7A2PGM 
00391      COPY  GCWRKDC3.                                              G7A2PGM 
00392      COPY  GCCONTR2.                                              G7A2PGM 
00393 /                                                                 G7A2PGM 
00394  PROCEDURE DIVISION.                                              G7A2PGM 
00395                                                                   G7A2PGM 
00396 ****************************************************************  G7A2PGM 
00397 *                                                              *  G7A2PGM 
00398 *           P R O C E S S     C O N T R O L                    *  G7A2PGM 
00399 *                                                              *  G7A2PGM 
00400 ****************************************************************  G7A2PGM 
00401  0000-000-PROCESS-CONTROL       SECTION.                          G7A2PGM 
00402  0000-010.                                                        G7A2PGM 
00403                                                                   G7A2PGM 
00404      IF  EIBAID  =  DFHCLEAR                                      G7A2PGM 
00405          EXEC CICS  RETURN                                        G7A2PGM 
00406                     END-EXEC.                                     G7A2PGM 
00407                                                                   G7A2PGM 
00408      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7A2PGM 
00409                                                                   G7A2PGM 
00410      IF  WS-02-MY-EIBTRNID                                        G7A2PGM 
00411      THEN                                                         G7A2PGM 
00412          PERFORM  2000-000-PROCESS-INPUT                          G7A2PGM 
00413      ELSE                                                         G7A2PGM 
00414          PERFORM  1000-000-DISPLAY-SCREEN.                        G7A2PGM 
00415                                                                   G7A2PGM 
00416                                                                   G7A2PGM 
00417 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7A2PGM 
00418                                                                   G7A2PGM 
00419      MOVE WS-01-ABCODE-A2L1     TO WS-01-ABCODE                   G7A2PGM 
00420      MOVE WS-01-ABCODE-A2L1-MSG TO WS-01-ABCODE-MSG               G7A2PGM 
00421      PERFORM  9999-000-ABEND-THE-TASK.                            G7A2PGM 
00422                                                                   G7A2PGM 
00423      GOBACK.                                                      G7A2PGM 
00424                                                                   G7A2PGM 
00425                                                                   G7A2PGM 
00426  0000-900-EXIT.                                                   G7A2PGM 
00427      EXIT.                                                        G7A2PGM 
00428 /***************************************************************  G7A2PGM 
00429 *                                                              *  G7A2PGM 
00430 * 1000  DISPLAY INITIAL SCREEN                                 *  G7A2PGM 
00431 *                                                              *  G7A2PGM 
00432 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7A2PGM 
00433 *                                                              *  G7A2PGM 
00434 ****************************************************************  G7A2PGM 
00435  1000-000-DISPLAY-SCREEN        SECTION.                          G7A2PGM 
00436  1000-010.                                                        G7A2PGM 
00437                                                                   G7A2PGM 
00438 *------- MOVE LOW-VALUES TO SCREEN                                G7A2PGM 
00439          MOVE LOW-VALUES TO G7A2I01I.                             G7A2PGM 
00440                                                                   G7A2PGM 
00441 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7A2PGM 
00442                                                                   G7A2PGM 
00443      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7A2PGM 
00444          MOVE WS-01-ABCODE-A2P1     TO WS-01-ABCODE               G7A2PGM 
00445          MOVE WS-01-ABCODE-A2P1-MSG TO WS-01-ABCODE-MSG           G7A2PGM 
00446          PERFORM 9999-000-ABEND-THE-TASK.                         G7A2PGM 
00447                                                                   G7A2PGM 
00448                                                                   G7A2PGM 
00449 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7A2PGM 
00450                                                                   G7A2PGM 
00451      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7A2PGM 
00452                                         + GC-GCBENPRV-FIXED-LEN   G7A2PGM 
00453                                         + GC-GCBENPRV-VARY-LEN.   G7A2PGM 
00454                                                                   G7A2PGM 
00455                                                                   G7A2PGM 
00456 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7A2PGM 
00457                                                                   G7A2PGM 
00458      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7A2PGM 
00459          MOVE WS-01-ABCODE-A2P2     TO WS-01-ABCODE               G7A2PGM 
00460          MOVE WS-01-ABCODE-A2P2-MSG TO WS-01-ABCODE-MSG           G7A2PGM 
00461          PERFORM 9999-000-ABEND-THE-TASK.                         G7A2PGM 
00462                                                                   G7A2PGM 
00463                                                                   G7A2PGM 
00464 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7A2PGM 
00465 *          BY CALLER IN COMMAREA.                                 G7A2PGM 
00466                                                                   G7A2PGM 
00467      MOVE WRK-PLAN-CODE                      TO S2PLNCDO.         G7A2PGM 
00468      MOVE WRK-GROUP-NUM                      TO S2GRPNOO.         G7A2PGM 
00469      MOVE WRK-SECTION-NUM                    TO S2SECNOO.         G7A2PGM 
00470      MOVE WRK-PKG-CODE                       TO S2PKGCDO.         G7A2PGM 
00471      MOVE WRK-PROV-CTL                       TO S2PRVO.           G7A2PGM 
00472      MOVE WRK-FAM-REL-LEVEL                  TO S2FRLO.           G7A2PGM 
00473      MOVE WRK-L-O-B                          TO S2LOBO.           G7A2PGM 
00474                                                                   G7A2PGM 
00475      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7A2PGM 
00476      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7A2PGM 
00477      IF  HGADATE-RETURN = ZEROS                                   G7A2PGM 
00478      THEN                                                         G7A2PGM 
00479          MOVE DFHBMASF                       TO S2EFFDTA          G7A2PGM 
00480          MOVE HGADATE-DATE2                  TO S2EFFDTO          G7A2PGM 
00481      ELSE                                                         G7A2PGM 
00482          MOVE DFHBMABF                       TO S2EFFDTA          G7A2PGM 
00483          MOVE HGADATE-JULIAN1                TO S2EFFDTO.         G7A2PGM 
00484                                                                   G7A2PGM 
00485      MOVE GCP-PROVN-ID                       TO S2BPVIDO.         G7A2PGM 
00486      MOVE GPA-CERTFN-REPETN-REQRM-IND        TO S2RRCERO.         G7A2PGM 
00487      MOVE GPA-ALCO-ELIG-MEMB-CLS-OVRD        TO S2AECOIO.         G7A2PGM 
00488      MOVE GPA-DRUG-ELIG-MEMB-CLS-OVRD        TO S2DECOIO.         G7A2PGM 
00489      MOVE GPA-ECF-SNF-OVRD-IND               TO S2ESOVIO.         G7A2PGM 
00490      MOVE GPA-NORM-NWBORN-OVRD-IND           TO S2NNOVIO.         G7A2PGM 
00491      MOVE GPA-TRANSSXL-PMT-RESTR-OVRD        TO S2TXPRIO.         G7A2PGM 
00492                                                                   G7A2PGM 
00493                                                                   G7A2PGM 
00494                                                                   G7A2PGM 
00495                                                                   G7A2PGM 
00496 *------- SEND INITIAL SCREEN ------------------------------------*G7A2PGM 
00497                                                                   G7A2PGM 
00498      MOVE  -1 TO  S2RRCERL.                                       G7A2PGM 
00499      PERFORM 9100-000-SEND-THEN-RETURN.                           G7A2PGM 
00500                                                                   G7A2PGM 
00501                                                                   G7A2PGM 
00502  1000-900-EXIT.                                                   G7A2PGM 
00503      EXIT.                                                        G7A2PGM 
00504 /***************************************************************  G7A2PGM 
00505 *                                                              *  G7A2PGM 
00506 * 2000    P R O C E S S    I N P U T                           *  G7A2PGM 
00507 *                                                              *  G7A2PGM 
00508 ****************************************************************  G7A2PGM 
00509  2000-000-PROCESS-INPUT         SECTION.                          G7A2PGM 
00510  2000-010.                                                        G7A2PGM 
00511                                                                   G7A2PGM 
00512 *------ VALIDATE PFKEY USAGE ------------------------------------*G7A2PGM 
00513                                                                   G7A2PGM 
00514      IF  EIBAID = DFHENTER OR                                     G7A2PGM 
00515                   DFHPF3   OR  DFHPF15 OR                         G7A2PGM 
00516                   DFHPF4   OR  DFHPF16 OR                         G7A2PGM 
00517                   DFHPF6   OR  DFHPF18 OR                         G7A2PGM 
00518                   DFHPF7   OR  DFHPF19 OR                         G7A2PGM 
00519                   DFHPF8   OR  DFHPF20 OR                         G7A2PGM 
00520                   DFHPF24                                         G7A2PGM 
00521      THEN                                                         G7A2PGM 
00522          NEXT SENTENCE                                            G7A2PGM 
00523      ELSE                                                         G7A2PGM 
00524          SET WT-01-INDEX TO +01                                   G7A2PGM 
00525          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A2PGM 
00526          PERFORM 9100-000-SEND-THEN-RETURN.                       G7A2PGM 
00527                                                                   G7A2PGM 
00528                                                                   G7A2PGM 
00529                                                                   G7A2PGM 
00530      EXEC CICS  HANDLE CONDITION                                  G7A2PGM 
00531                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7A2PGM 
00532                        END-EXEC.                                  G7A2PGM 
00533                                                                   G7A2PGM 
00534                                                                   G7A2PGM 
00535      EXEC CICS  RECEIVE MAP   ('G7A2I01')                         G7A2PGM 
00536                         MAPSET('G7A2SET')                         G7A2PGM 
00537                         END-EXEC.                                 G7A2PGM 
00538                                                                   G7A2PGM 
00539                                                                   G7A2PGM 
00540      IF  S2FUNCI  NOT = 'G7A2'  OR                                G7A2PGM 
00541          S2SCRNI  NOT = '007A02'                                  G7A2PGM 
00542          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7A2PGM 
00543                                                                   G7A2PGM 
00544                                                                   G7A2PGM 
00545 *--- RETURN TO GCPS MENU? ---------------------------------------*G7A2PGM 
00546                                                                   G7A2PGM 
00547      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7A2PGM 
00548          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7A2PGM 
00549                                                                   G7A2PGM 
00550 *--- PROCESS SCREEN FIELDS --------------------------------------*G7A2PGM 
00551                                                                   G7A2PGM 
00552      PERFORM 2100-000-FIELD-EDITS.                                G7A2PGM 
00553                                                                   G7A2PGM 
00554      IF  WS-02-SCREEN-HAS-ERRORS                                  G7A2PGM 
00555          PERFORM 9100-000-SEND-THEN-RETURN.                       G7A2PGM 
00556                                                                   G7A2PGM 
00557      PERFORM 2200-000-LOGICAL-EDITS.                              G7A2PGM 
00558                                                                   G7A2PGM 
00559      IF  WS-02-SCREEN-HAS-ERRORS                                  G7A2PGM 
00560          PERFORM 9100-000-SEND-THEN-RETURN.                       G7A2PGM 
00561                                                                   G7A2PGM 
00562      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7A2PGM 
00563                                                                   G7A2PGM 
00564      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7A2PGM 
00565                                                                   G7A2PGM 
00566                                                                   G7A2PGM 
00567  2000-900-EXIT.                                                   G7A2PGM 
00568      EXIT.                                                        G7A2PGM 
00569 /***************************************************************  G7A2PGM 
00570 *                                                              *  G7A2PGM 
00571 * 2100  DO SCREEN FIELD EDITS                                  *  G7A2PGM 
00572 *                                                              *  G7A2PGM 
00573 ****************************************************************  G7A2PGM 
00574  2100-000-FIELD-EDITS           SECTION.                          G7A2PGM 
00575  2100-010.                                                        G7A2PGM 
00576                                                                   G7A2PGM 
00577 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7A2PGM 
00578                                                                   G7A2PGM 
00579      MOVE DFHBMUNF TO  S2RRCERA                                   G7A2PGM 
00580                        S2AECOIA                                   G7A2PGM 
00581                        S2DECOIA                                   G7A2PGM 
00582                        S2ESOVIA                                   G7A2PGM 
00583                        S2NNOVIA                                   G7A2PGM 
00584                        S2TXPRIA.                                  G7A2PGM 
00585                                                                   G7A2PGM 
00586      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7A2PGM 
00587                                                                   G7A2PGM 
00588                                                                   G7A2PGM 
00589 *-- VALIDATE ------ CERTIFICATION REPEAT REQUIREMENT IND --------*G7A2PGM 
00590 *   1. ALPHANUMERIC                                               G7A2PGM 
00591 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A2PGM 
00592                                                                   G7A2PGM 
00593      MOVE  S2RRCERI TO WS-02-CLASS-TEST-AREA.                     G7A2PGM 
00594      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A2PGM 
00595      THEN                                                         G7A2PGM 
00596          MOVE  S2RRCERI TO GCVI-VALUE                             G7A2PGM 
00597          MOVE  'BPAB02' TO GCVI-FIELDS-KEY-ID                     G7A2PGM 
00598          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A2PGM 
00599          IF  GCVI-VALUE-NOT-FOUND                                 G7A2PGM 
00600          THEN                                                     G7A2PGM 
00601              MOVE  -1        TO  S2RRCERL                         G7A2PGM 
00602              MOVE  DFHBMUBF  TO  S2RRCERA                         G7A2PGM 
00603              IF  WS-02-SCREEN-HAS-ERRORS                          G7A2PGM 
00604              THEN                                                 G7A2PGM 
00605                  NEXT SENTENCE                                    G7A2PGM 
00606              ELSE                                                 G7A2PGM 
00607                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00608                  SET WT-01-INDEX TO +09                           G7A2PGM 
00609                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00610          ELSE                                                     G7A2PGM 
00611              IF  GCVI-VALUE-NOT-LOADED                            G7A2PGM 
00612              THEN                                                 G7A2PGM 
00613                  MOVE  DFHBMUBF  TO  S2RRCERA                     G7A2PGM 
00614              ELSE                                                 G7A2PGM 
00615                  NEXT SENTENCE                                    G7A2PGM 
00616      ELSE                                                         G7A2PGM 
00617          MOVE  -1        TO  S2RRCERL                             G7A2PGM 
00618          MOVE  DFHBMUBF  TO  S2RRCERA                             G7A2PGM 
00619          IF  WS-02-SCREEN-HAS-ERRORS                              G7A2PGM 
00620          THEN                                                     G7A2PGM 
00621              NEXT SENTENCE                                        G7A2PGM 
00622          ELSE                                                     G7A2PGM 
00623              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A2PGM 
00624              SET WT-01-INDEX TO +08                               G7A2PGM 
00625              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A2PGM 
00626                                                                   G7A2PGM 
00627                                                                   G7A2PGM 
00628 *-- VALIDATE ------ ALCOHOL ELIGIBLE MEMBER OVERRIDE IND --------*G7A2PGM 
00629 *   1. ALPHANUMERIC                                               G7A2PGM 
00630 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A2PGM 
00631                                                                   G7A2PGM 
00632      MOVE  S2AECOII TO WS-02-CLASS-TEST-AREA.                     G7A2PGM 
00633      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A2PGM 
00634      THEN                                                         G7A2PGM 
00635          MOVE  S2AECOII TO GCVI-VALUE                             G7A2PGM 
00636          MOVE  'BPAB03' TO GCVI-FIELDS-KEY-ID                     G7A2PGM 
00637          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A2PGM 
00638          IF  GCVI-VALUE-NOT-FOUND                                 G7A2PGM 
00639          THEN                                                     G7A2PGM 
00640              MOVE  -1        TO  S2AECOIL                         G7A2PGM 
00641              MOVE  DFHBMUBF  TO  S2AECOIA                         G7A2PGM 
00642              IF  WS-02-SCREEN-HAS-ERRORS                          G7A2PGM 
00643              THEN                                                 G7A2PGM 
00644                  NEXT SENTENCE                                    G7A2PGM 
00645              ELSE                                                 G7A2PGM 
00646                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00647                  SET WT-01-INDEX TO +09                           G7A2PGM 
00648                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00649          ELSE                                                     G7A2PGM 
00650              IF  GCVI-VALUE-NOT-LOADED                            G7A2PGM 
00651              THEN                                                 G7A2PGM 
00652                  MOVE  DFHBMUBF  TO  S2AECOIA                     G7A2PGM 
00653              ELSE                                                 G7A2PGM 
00654                  NEXT SENTENCE                                    G7A2PGM 
00655      ELSE                                                         G7A2PGM 
00656          MOVE  -1        TO  S2AECOIL                             G7A2PGM 
00657          MOVE  DFHBMUBF  TO  S2AECOIA                             G7A2PGM 
00658          IF  WS-02-SCREEN-HAS-ERRORS                              G7A2PGM 
00659          THEN                                                     G7A2PGM 
00660              NEXT SENTENCE                                        G7A2PGM 
00661          ELSE                                                     G7A2PGM 
00662              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A2PGM 
00663              SET WT-01-INDEX TO +08                               G7A2PGM 
00664              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A2PGM 
00665                                                                   G7A2PGM 
00666                                                                   G7A2PGM 
00667 *-- VALIDATE ------ DRUG ELIGIBLE MEMBER OVERRIDE IND -----------*G7A2PGM 
00668 *   1. ALPHANUMERIC                                               G7A2PGM 
00669 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A2PGM 
00670                                                                   G7A2PGM 
00671      MOVE  S2DECOII TO WS-02-CLASS-TEST-AREA.                     G7A2PGM 
00672      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A2PGM 
00673      THEN                                                         G7A2PGM 
00674          MOVE  S2DECOII TO GCVI-VALUE                             G7A2PGM 
00675          MOVE  'BPAB04' TO GCVI-FIELDS-KEY-ID                     G7A2PGM 
00676          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A2PGM 
00677          IF  GCVI-VALUE-NOT-FOUND                                 G7A2PGM 
00678          THEN                                                     G7A2PGM 
00679              MOVE  -1        TO  S2DECOIL                         G7A2PGM 
00680              MOVE  DFHBMUBF  TO  S2DECOIA                         G7A2PGM 
00681              IF  WS-02-SCREEN-HAS-ERRORS                          G7A2PGM 
00682              THEN                                                 G7A2PGM 
00683                  NEXT SENTENCE                                    G7A2PGM 
00684              ELSE                                                 G7A2PGM 
00685                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00686                  SET WT-01-INDEX TO +09                           G7A2PGM 
00687                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00688          ELSE                                                     G7A2PGM 
00689              IF  GCVI-VALUE-NOT-LOADED                            G7A2PGM 
00690              THEN                                                 G7A2PGM 
00691                  MOVE  DFHBMUBF  TO  S2DECOIA                     G7A2PGM 
00692              ELSE                                                 G7A2PGM 
00693                  NEXT SENTENCE                                    G7A2PGM 
00694      ELSE                                                         G7A2PGM 
00695          MOVE  -1        TO  S2DECOIL                             G7A2PGM 
00696          MOVE  DFHBMUBF  TO  S2DECOIA                             G7A2PGM 
00697          IF  WS-02-SCREEN-HAS-ERRORS                              G7A2PGM 
00698          THEN                                                     G7A2PGM 
00699              NEXT SENTENCE                                        G7A2PGM 
00700          ELSE                                                     G7A2PGM 
00701              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A2PGM 
00702              SET WT-01-INDEX TO +08                               G7A2PGM 
00703              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A2PGM 
00704                                                                   G7A2PGM 
00705                                                                   G7A2PGM 
00706 *-- VALIDATE ------ ECF/SNF OVERRIDE INDICATOR ------------------*G7A2PGM 
00707 *   1. ALPHANUMERIC                                               G7A2PGM 
00708 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A2PGM 
00709                                                                   G7A2PGM 
00710      MOVE  S2ESOVII TO WS-02-CLASS-TEST-AREA.                     G7A2PGM 
00711      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A2PGM 
00712      THEN                                                         G7A2PGM 
00713          MOVE  S2ESOVII TO GCVI-VALUE                             G7A2PGM 
00714          MOVE  'BPAB05' TO GCVI-FIELDS-KEY-ID                     G7A2PGM 
00715          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A2PGM 
00716          IF  GCVI-VALUE-NOT-FOUND                                 G7A2PGM 
00717          THEN                                                     G7A2PGM 
00718              MOVE  -1        TO  S2ESOVIL                         G7A2PGM 
00719              MOVE  DFHBMUBF  TO  S2ESOVIA                         G7A2PGM 
00720              IF  WS-02-SCREEN-HAS-ERRORS                          G7A2PGM 
00721              THEN                                                 G7A2PGM 
00722                  NEXT SENTENCE                                    G7A2PGM 
00723              ELSE                                                 G7A2PGM 
00724                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00725                  SET WT-01-INDEX TO +09                           G7A2PGM 
00726                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00727          ELSE                                                     G7A2PGM 
00728              IF  GCVI-VALUE-NOT-LOADED                            G7A2PGM 
00729              THEN                                                 G7A2PGM 
00730                  MOVE  DFHBMUBF  TO  S2ESOVIA                     G7A2PGM 
00731              ELSE                                                 G7A2PGM 
00732                  NEXT SENTENCE                                    G7A2PGM 
00733      ELSE                                                         G7A2PGM 
00734          MOVE  -1        TO  S2ESOVIL                             G7A2PGM 
00735          MOVE  DFHBMUBF  TO  S2ESOVIA                             G7A2PGM 
00736          IF  WS-02-SCREEN-HAS-ERRORS                              G7A2PGM 
00737          THEN                                                     G7A2PGM 
00738              NEXT SENTENCE                                        G7A2PGM 
00739          ELSE                                                     G7A2PGM 
00740              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A2PGM 
00741              SET WT-01-INDEX TO +08                               G7A2PGM 
00742              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A2PGM 
00743                                                                   G7A2PGM 
00744                                                                   G7A2PGM 
00745 *-- VALIDATE ------ NORMAL NEWBORN OVERRIDE IND -----------------*G7A2PGM 
00746 *   1. ALPHANUMERIC                                               G7A2PGM 
00747 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A2PGM 
00748                                                                   G7A2PGM 
00749      MOVE  S2NNOVII TO WS-02-CLASS-TEST-AREA.                     G7A2PGM 
00750      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A2PGM 
00751      THEN                                                         G7A2PGM 
00752          MOVE  S2NNOVII TO GCVI-VALUE                             G7A2PGM 
00753          MOVE  'BPAB05' TO GCVI-FIELDS-KEY-ID                     G7A2PGM 
00754          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A2PGM 
00755          IF  GCVI-VALUE-NOT-FOUND                                 G7A2PGM 
00756          THEN                                                     G7A2PGM 
00757              MOVE  -1        TO  S2NNOVIL                         G7A2PGM 
00758              MOVE  DFHBMUBF  TO  S2NNOVIA                         G7A2PGM 
00759              IF  WS-02-SCREEN-HAS-ERRORS                          G7A2PGM 
00760              THEN                                                 G7A2PGM 
00761                  NEXT SENTENCE                                    G7A2PGM 
00762              ELSE                                                 G7A2PGM 
00763                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00764                  SET WT-01-INDEX TO +09                           G7A2PGM 
00765                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00766          ELSE                                                     G7A2PGM 
00767              IF  GCVI-VALUE-NOT-LOADED                            G7A2PGM 
00768              THEN                                                 G7A2PGM 
00769                  MOVE  DFHBMUBF  TO  S2NNOVIA                     G7A2PGM 
00770              ELSE                                                 G7A2PGM 
00771                  NEXT SENTENCE                                    G7A2PGM 
00772      ELSE                                                         G7A2PGM 
00773          MOVE  -1        TO  S2NNOVIL                             G7A2PGM 
00774          MOVE  DFHBMUBF  TO  S2NNOVIA                             G7A2PGM 
00775          IF  WS-02-SCREEN-HAS-ERRORS                              G7A2PGM 
00776          THEN                                                     G7A2PGM 
00777              NEXT SENTENCE                                        G7A2PGM 
00778          ELSE                                                     G7A2PGM 
00779              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A2PGM 
00780              SET WT-01-INDEX TO +08                               G7A2PGM 
00781              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A2PGM 
00782                                                                   G7A2PGM 
00783                                                                   G7A2PGM 
00784 *-- VALIDATE ------ TRANS-SEXUAL RESTR. OVERRIDE IND ------------*G7A2PGM 
00785 *   1. ALPHANUMERIC                                               G7A2PGM 
00786 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A2PGM 
00787                                                                   G7A2PGM 
00788      MOVE  S2TXPRII TO WS-02-CLASS-TEST-AREA.                     G7A2PGM 
00789      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A2PGM 
00790      THEN                                                         G7A2PGM 
00791          MOVE  S2TXPRII TO GCVI-VALUE                             G7A2PGM 
00792          MOVE  'BPAB04' TO GCVI-FIELDS-KEY-ID                     G7A2PGM 
00793          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A2PGM 
00794          IF  GCVI-VALUE-NOT-FOUND                                 G7A2PGM 
00795          THEN                                                     G7A2PGM 
00796              MOVE  -1        TO  S2TXPRIL                         G7A2PGM 
00797              MOVE  DFHBMUBF  TO  S2TXPRIA                         G7A2PGM 
00798              IF  WS-02-SCREEN-HAS-ERRORS                          G7A2PGM 
00799              THEN                                                 G7A2PGM 
00800                  NEXT SENTENCE                                    G7A2PGM 
00801              ELSE                                                 G7A2PGM 
00802                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00803                  SET WT-01-INDEX TO +09                           G7A2PGM 
00804                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00805          ELSE                                                     G7A2PGM 
00806              IF  GCVI-VALUE-NOT-LOADED                            G7A2PGM 
00807              THEN                                                 G7A2PGM 
00808                  MOVE  DFHBMUBF  TO  S2TXPRIA                     G7A2PGM 
00809              ELSE                                                 G7A2PGM 
00810                  NEXT SENTENCE                                    G7A2PGM 
00811      ELSE                                                         G7A2PGM 
00812          MOVE  -1        TO  S2TXPRIL                             G7A2PGM 
00813          MOVE  DFHBMUBF  TO  S2TXPRIA                             G7A2PGM 
00814          IF  WS-02-SCREEN-HAS-ERRORS                              G7A2PGM 
00815          THEN                                                     G7A2PGM 
00816              NEXT SENTENCE                                        G7A2PGM 
00817          ELSE                                                     G7A2PGM 
00818              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A2PGM 
00819              SET WT-01-INDEX TO +08                               G7A2PGM 
00820              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A2PGM 
00821                                                                   G7A2PGM 
00822                                                                   G7A2PGM 
00823                                                                   G7A2PGM 
00824  2100-900-EXIT.                                                   G7A2PGM 
00825      EXIT.                                                        G7A2PGM 
00826 /***************************************************************  G7A2PGM 
00827 *                                                              *  G7A2PGM 
00828 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7A2PGM 
00829 *                                                              *  G7A2PGM 
00830 ****************************************************************  G7A2PGM 
00831  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7A2PGM 
00832  2110-010.                                                        G7A2PGM 
00833                                                                   G7A2PGM 
00834      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7A2PGM 
00835                                                                   G7A2PGM 
00836      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7A2PGM 
00837                       COMMAREA(GCVIOPGM-PARM-LIST)                G7A2PGM 
00838                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7A2PGM 
00839                       END-EXEC.                                   G7A2PGM 
00840                                                                   G7A2PGM 
00841      IF  GCVI-VALUE-NOT-LOADED                                    G7A2PGM 
00842          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7A2PGM 
00843                                                                   G7A2PGM 
00844  2110-900-EXIT.                                                   G7A2PGM 
00845      EXIT.                                                        G7A2PGM 
00846 /***************************************************************  G7A2PGM 
00847 *                                                              *  G7A2PGM 
00848 * 2200  DO SCREEN LOGICAL EDITS                                *  G7A2PGM 
00849 *                                                              *  G7A2PGM 
00850 ****************************************************************  G7A2PGM 
00851  2200-000-LOGICAL-EDITS         SECTION.                          G7A2PGM 
00852  2200-010.                                                        G7A2PGM 
00853                                                                   G7A2PGM 
00854 *----------------------------------------------------------------*G7A2PGM 
00855 *                                                                *G7A2PGM 
00856 *                                                                *G7A2PGM 
00857 *   THIS PROGRAM HAS NO REQUIREMENT FOR LOGICAL EDITS            *G7A2PGM 
00858 *                                                                *G7A2PGM 
00859 *                                                                *G7A2PGM 
00860 *----------------------------------------------------------------*G7A2PGM 
00861                                                                   G7A2PGM 
00862                                                                   G7A2PGM 
00863 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7A2PGM 
00864                                                                   G7A2PGM 
00865      IF  WS-02-SCREEN-HAS-ERRORS                                  G7A2PGM 
00866      THEN                                                         G7A2PGM 
00867          NEXT SENTENCE                                            G7A2PGM 
00868      ELSE                                                         G7A2PGM 
00869          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7A2PGM 
00870          THEN                                                     G7A2PGM 
00871              IF EIBAID = DFHPF4 OR DFHPF16                        G7A2PGM 
00872              THEN                                                 G7A2PGM 
00873                  NEXT SENTENCE                                    G7A2PGM 
00874              ELSE                                                 G7A2PGM 
00875                  MOVE  -1        TO S2ERRL                        G7A2PGM 
00876                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A2PGM 
00877                  SET WT-01-INDEX TO +06                           G7A2PGM 
00878                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A2PGM 
00879          ELSE                                                     G7A2PGM 
00880              NEXT SENTENCE.                                       G7A2PGM 
00881                                                                   G7A2PGM 
00882                                                                   G7A2PGM 
00883  2200-900-EXIT.                                                   G7A2PGM 
00884      EXIT.                                                        G7A2PGM 
00885 /***************************************************************  G7A2PGM 
00886 *                                                              *  G7A2PGM 
00887 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7A2PGM 
00888 *        REWRITE TO WORKFILE.                                  *  G7A2PGM 
00889 *                                                              *  G7A2PGM 
00890 ****************************************************************  G7A2PGM 
00891  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7A2PGM 
00892  2300-010.                                                        G7A2PGM 
00893                                                                   G7A2PGM 
00894 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7A2PGM 
00895                                                                   G7A2PGM 
00896      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7A2PGM 
00897      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7A2PGM 
00898      TO   GCP2-COUNT-TAB-PROVN-POINTERS.                          G7A2PGM 
00899      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7A2PGM 
00900      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7A2PGM 
00901      IF  NOT GCIO2-GOOD-RETURN                                    G7A2PGM 
00902          MOVE WS-01-ABCODE-A2F2     TO WS-01-ABCODE               G7A2PGM 
00903          MOVE WS-01-ABCODE-A2F2-MSG TO WS-01-ABCODE-MSG           G7A2PGM 
00904          PERFORM  9999-000-ABEND-THE-TASK.                        G7A2PGM 
00905                                                                   G7A2PGM 
00906                                                                   G7A2PGM 
00907 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7A2PGM 
00908                                                                   G7A2PGM 
00909                                                                   G7A2PGM 
00910      IF   GPA2-CERTFN-REPETN-REQRM-IND  = S2RRCERI AND            G7A2PGM 
00911           GPA2-ALCO-ELIG-MEMB-CLS-OVRD  = S2AECOIO AND            G7A2PGM 
00912           GPA2-DRUG-ELIG-MEMB-CLS-OVRD  = S2DECOIO AND            G7A2PGM 
00913           GPA2-ECF-SNF-OVRD-IND         = S2ESOVIO AND            G7A2PGM 
00914           GPA2-NORM-NWBORN-OVRD-IND     = S2NNOVIO AND            G7A2PGM 
00915           GPA2-TRANSSXL-PMT-RESTR-OVRD  = S2TXPRIO                G7A2PGM 
00916      THEN                                                         G7A2PGM 
00917          GO TO 2300-900-EXIT                                      G7A2PGM 
00918      ELSE                                                         G7A2PGM 
00919          NEXT SENTENCE.                                           G7A2PGM 
00920                                                                   G7A2PGM 
00921                                                                   G7A2PGM 
00922 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7A2PGM 
00923                                                                   G7A2PGM 
00924      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7A2PGM 
00925      TO   GCP2-COUNT-TAB-PROVN-POINTERS.                          G7A2PGM 
00926      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7A2PGM 
00927      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7A2PGM 
00928      IF  NOT GCIO2-GOOD-RETURN                                    G7A2PGM 
00929          MOVE WS-01-ABCODE-A2F3     TO WS-01-ABCODE               G7A2PGM 
00930          MOVE WS-01-ABCODE-A2F3-MSG TO WS-01-ABCODE-MSG           G7A2PGM 
00931          PERFORM  9999-000-ABEND-THE-TASK.                        G7A2PGM 
00932                                                                   G7A2PGM 
00933                                                                   G7A2PGM 
00934 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7A2PGM 
00935                                                                   G7A2PGM 
00936      MOVE S2RRCERI TO GPA2-CERTFN-REPETN-REQRM-IND.               G7A2PGM 
00937      MOVE S2AECOII TO GPA2-ALCO-ELIG-MEMB-CLS-OVRD.               G7A2PGM 
00938      MOVE S2DECOII TO GPA2-DRUG-ELIG-MEMB-CLS-OVRD.               G7A2PGM 
00939      MOVE S2ESOVII TO GPA2-ECF-SNF-OVRD-IND.                      G7A2PGM 
00940      MOVE S2NNOVII TO GPA2-NORM-NWBORN-OVRD-IND.                  G7A2PGM 
00941      MOVE S2TXPRII TO GPA2-TRANSSXL-PMT-RESTR-OVRD.               G7A2PGM 
00942                                                                   G7A2PGM 
00943                                                                   G7A2PGM 
00944 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7A2PGM 
00945                                                                   G7A2PGM 
00946 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7A2PGM 
00947                                                                   G7A2PGM 
00948      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7A2PGM 
00949      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7A2PGM 
00950      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7A2PGM 
00951      IF  NOT GCIO2-GOOD-RETURN                                    G7A2PGM 
00952          MOVE WS-01-ABCODE-A2F4     TO WS-01-ABCODE               G7A2PGM 
00953          MOVE WS-01-ABCODE-A2F4-MSG TO WS-01-ABCODE-MSG           G7A2PGM 
00954          PERFORM  9999-000-ABEND-THE-TASK.                        G7A2PGM 
00955                                                                   G7A2PGM 
00956  2300-900-EXIT.                                                   G7A2PGM 
00957      EXIT.                                                        G7A2PGM 
00958 /***************************************************************  G7A2PGM 
00959 *                                                              *  G7A2PGM 
00960 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7A2PGM 
00961 *                                                              *  G7A2PGM 
00962 ****************************************************************  G7A2PGM 
00963  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7A2PGM 
00964  2310-010.                                                        G7A2PGM 
00965                                                                   G7A2PGM 
00966                                                                   G7A2PGM 
00967 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7A2PGM 
00968                                                                   G7A2PGM 
00969      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7A2PGM 
00970                                         + GC-WORKFILE-KEY-LEN     G7A2PGM 
00971                                         + GC-GCBENPRV-MAX-REC-LEN.G7A2PGM 
00972                                                                   G7A2PGM 
00973 ***  EXEC CICS  GETMAIN  SET    (BEN-PROV-PNTR)                   G7A2PGM 
00974      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7A2PGM 
00975                          INITIMG(WS-02-HEX-00)                    G7A2PGM 
00976                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7A2PGM 
00977                          END-EXEC.                                G7A2PGM 
00978                                                                   G7A2PGM 
00979 ***  SERVICE RELOAD  IO-PARM-BEN-PROV-AREA.                       G7A2PGM 
00980                                                                   G7A2PGM 
00981 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7A2PGM 
00982                                                                   G7A2PGM 
00983      MOVE SPACES                 TO GCIO-CONTRACT-FILE-KEY.       G7A2PGM 
00984      MOVE WRK-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           G7A2PGM 
00985      MOVE WRK-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3.        G7A2PGM 
00986      MOVE WRK-SEC-NO-1           TO GCIO-WRK-SEC-NO-1.            G7A2PGM 
00987      MOVE WRK-PKG-CODE           TO GCIO-WRK-PKG-CODE.            G7A2PGM 
00988      MOVE WRK-EFFECTIVE-DATE     TO GCIO-WRK-EFFECTIVE-DT.        G7A2PGM 
00989                                                                   G7A2PGM 
00990      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7A2PGM 
00991      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7A2PGM 
00992      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7A2PGM 
00993      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7A2PGM 
00994      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7A2PGM 
00995      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7A2PGM 
00996      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7A2PGM 
00997      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7A2PGM 
00998      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7A2PGM 
00999                                                                   G7A2PGM 
01000 **   MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7A2PGM 
01001 **   PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7A2PGM 
01002 **   IF  HGADATE-RETURN = ZEROS                                   G7A2PGM 
01003 **   THEN                                                         G7A2PGM 
01004 **       MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7A2PGM 
01005 **   ELSE                                                         G7A2PGM 
01006 **       SET WT-01-INDEX TO +07                                   G7A2PGM 
01007 **       PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A2PGM 
01008 **       PERFORM 9100-000-SEND-THEN-RETURN.                       G7A2PGM 
01009                                                                   G7A2PGM 
01010      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7A2PGM 
01011      MOVE S2BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7A2PGM 
01012      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7A2PGM 
01013      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7A2PGM 
01014      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7A2PGM 
01015      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7A2PGM 
01016      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7A2PGM 
01017                                                                   G7A2PGM 
01018                                                                   G7A2PGM 
01019  2310-900-EXIT.                                                   G7A2PGM 
01020      EXIT.                                                        G7A2PGM 
01021 /***************************************************************  G7A2PGM 
01022 *                                                              *  G7A2PGM 
01023 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7A2PGM 
01024 *                                                              *  G7A2PGM 
01025 ****************************************************************  G7A2PGM 
01026  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7A2PGM 
01027  2400-010.                                                        G7A2PGM 
01028                                                                   G7A2PGM 
01029                                                                   G7A2PGM 
01030      IF  EIBAID = DFHPF7  OR DFHPF19                              G7A2PGM 
01031      THEN                                                         G7A2PGM 
01032          MOVE 'G7A1PGM' TO WS-02-NEXT-PROGRAM.                    G7A2PGM 
01033                                                                   G7A2PGM 
01034      IF  EIBAID = DFHENTER OR                                     G7A2PGM 
01035                   DFHPF4   OR DFHPF16 OR                          G7A2PGM 
01036                   DFHPF8   OR DFHPF20                             G7A2PGM 
01037      THEN                                                         G7A2PGM 
01038          MOVE 'GC6APGM' TO WS-02-NEXT-PROGRAM.                    G7A2PGM 
01039                                                                   G7A2PGM 
01040      IF  EIBAID = DFHPF6  OR DFHPF18                              G7A2PGM 
01041      THEN                                                         G7A2PGM 
01042          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7A2PGM 
01043                                                                   G7A2PGM 
01044                                                                   G7A2PGM 
01045      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7A2PGM 
01046                       COMMAREA(WORK-RECORD-2)                     G7A2PGM 
01047                       LENGTH  (GCIO2-RECORD-LENGTH)               G7A2PGM 
01048                       END-EXEC.                                   G7A2PGM 
01049                                                                   G7A2PGM 
01050  2400-900-EXIT.                                                   G7A2PGM 
01051      EXIT.                                                        G7A2PGM 
01052 /***************************************************************  G7A2PGM 
01053 *                                                              *  G7A2PGM 
01054 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7A2PGM 
01055 *         PROVISION RECORD (TYPE=C4)                           *  G7A2PGM 
01056 *                                                              *  G7A2PGM 
01057 ****************************************************************  G7A2PGM 
01058  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7A2PGM 
01059  5000-010.                                                        G7A2PGM 
01060                                                                   G7A2PGM 
01061      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7A2PGM 
01062                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7A2PGM 
01063                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7A2PGM 
01064                       END-EXEC.                                   G7A2PGM 
01065                                                                   G7A2PGM 
01066                                                                   G7A2PGM 
01067  5000-900-EXIT.                                                   G7A2PGM 
01068      EXIT.                                                        G7A2PGM 
01069 /***************************************************************  G7A2PGM 
01070 *                                                              *  G7A2PGM 
01071 * 5100                                                         *  G7A2PGM 
01072 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7A2PGM 
01073 *                                                              *  G7A2PGM 
01074 ****************************************************************  G7A2PGM 
01075  5100-000-W-F-CONTRACT-IO       SECTION.                          G7A2PGM 
01076  5100-010.                                                        G7A2PGM 
01077                                                                   G7A2PGM 
01078      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7A2PGM 
01079                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7A2PGM 
01080                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7A2PGM 
01081                       END-EXEC.                                   G7A2PGM 
01082                                                                   G7A2PGM 
01083                                                                   G7A2PGM 
01084  5100-900-EXIT.                                                   G7A2PGM 
01085      EXIT.                                                        G7A2PGM 
01086 /***************************************************************  G7A2PGM 
01087 *                                                              *  G7A2PGM 
01088 * 9000   MOVE MESSAGE TO SCREEN                                *  G7A2PGM 
01089 *                                                              *  G7A2PGM 
01090 ****************************************************************  G7A2PGM 
01091  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7A2PGM 
01092  9000-010.                                                        G7A2PGM 
01093                                                                   G7A2PGM 
01094      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S2ERRO.              G7A2PGM 
01095                                                                   G7A2PGM 
01096  9000-900-EXIT.                                                   G7A2PGM 
01097      EXIT.                                                        G7A2PGM 
01098 /***************************************************************  G7A2PGM 
01099 *                                                              *  G7A2PGM 
01100 * 9100 SEND SCREEN AND RETURN                                  *  G7A2PGM 
01101 *                                                              *  G7A2PGM 
01102 ****************************************************************  G7A2PGM 
01103  9100-000-SEND-THEN-RETURN      SECTION.                          G7A2PGM 
01104  9100-010.                                                        G7A2PGM 
01105                                                                   G7A2PGM 
01106                                                                   G7A2PGM 
01107 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7A2PGM 
01108      MOVE  -1 TO  S2ERRL.                                         G7A2PGM 
01109                                                                   G7A2PGM 
01110                                                                   G7A2PGM 
01111      IF  WS-02-MY-EIBTRNID                                        G7A2PGM 
01112      THEN                                                         G7A2PGM 
01113          EXEC CICS  SEND MAP('G7A2I01')                           G7A2PGM 
01114                          MAPSET('G7A2SET')                        G7A2PGM 
01115                          DATAONLY                                 G7A2PGM 
01116                          CURSOR                                   G7A2PGM 
01117                          END-EXEC                                 G7A2PGM 
01118      ELSE                                                         G7A2PGM 
01119          EXEC CICS  SEND MAP('G7A2I01')                           G7A2PGM 
01120                          MAPSET('G7A2SET')                        G7A2PGM 
01121                          ERASE                                    G7A2PGM 
01122                          CURSOR                                   G7A2PGM 
01123                          END-EXEC.                                G7A2PGM 
01124                                                                   G7A2PGM 
01125      EXEC CICS RETURN                                             G7A2PGM 
01126                TRANSID  ('G7A2')                                  G7A2PGM 
01127                COMMAREA (DFHCOMMAREA)                             G7A2PGM 
01128                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7A2PGM 
01129                END-EXEC.                                          G7A2PGM 
01130                                                                   G7A2PGM 
01131 **   EXEC CICS  RETURN                                            G7A2PGM 
01132 **              END-EXEC.                                         G7A2PGM 
01133 *                                                                 G7A2PGM 
01134 *                                                                 G7A2PGM 
01135  9100-900-EXIT.                                                   G7A2PGM 
01136      EXIT.                                                        G7A2PGM 
01137 /*****************************************************************G7A2PGM 
01138 *                                                                *G7A2PGM 
01139 * 9200    XCTL TO GCPSPGM                                        *G7A2PGM 
01140 *                                                                *G7A2PGM 
01141 *                                                                *G7A2PGM 
01142 ******************************************************************G7A2PGM 
01143  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7A2PGM 
01144  9200-010.                                                        G7A2PGM 
01145                                                                   G7A2PGM 
01146      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7A2PGM 
01147                       END-EXEC.                                   G7A2PGM 
01148                                                                   G7A2PGM 
01149  9200-900-EXIT.                                                   G7A2PGM 
01150      EXIT.                                                        G7A2PGM 
01151 /*****************************************************************G7A2PGM 
01152 *                                                                *G7A2PGM 
01153 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7A2PGM 
01154 *                                                                *G7A2PGM 
01155 *                                                                *G7A2PGM 
01156 ******************************************************************G7A2PGM 
01157  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7A2PGM 
01158  9210-010.                                                        G7A2PGM 
01159                                                                   G7A2PGM 
01160      IF  S2GRPNOI = '000SPS000'                                   G7A2PGM 
01161          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7A2PGM 
01162                           END-EXEC.                               G7A2PGM 
01163                                                                   G7A2PGM 
01164 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7A2PGM 
01165                                                                   G7A2PGM 
01166      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7A2PGM 
01167                                        + GC-WORKFILE-KEY-LEN      G7A2PGM 
01168                                        + GC-GCCONTR-MAX-REC-LEN.  G7A2PGM 
01169                                                                   G7A2PGM 
01170 ***  EXEC CICS  GETMAIN  SET    (CONTRACT-PNTR)                   G7A2PGM 
01171      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7A2PGM 
01172                          INITIMG(WS-02-HEX-00)                    G7A2PGM 
01173                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7A2PGM 
01174                          END-EXEC.                                G7A2PGM 
01175                                                                   G7A2PGM 
01176 ***  COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7A2PGM 
01177 ***  SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7A2PGM 
01178                                                                   G7A2PGM 
01179 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7A2PGM 
01180                                                                   G7A2PGM 
01181      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7A2PGM 
01182      TO   GCT2-COUNT-BEN-PROVN-POINTERS.                          G7A2PGM 
01183      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7A2PGM 
01184      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7A2PGM 
01185                                                                   G7A2PGM 
01186      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7A2PGM 
01187      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7A2PGM 
01188      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7A2PGM 
01189      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7A2PGM 
01190      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7A2PGM 
01191      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7A2PGM 
01192      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7A2PGM 
01193      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7A2PGM 
01194      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7A2PGM 
01195      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7A2PGM 
01196      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7A2PGM 
01197      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7A2PGM 
01198      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7A2PGM 
01199      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7A2PGM 
01200                                                                   G7A2PGM 
01201 **   MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7A2PGM 
01202 **   PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7A2PGM 
01203 **   IF  HGADATE-RETURN = ZEROS                                   G7A2PGM 
01204 **   THEN                                                         G7A2PGM 
01205 **       MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7A2PGM 
01206 **   ELSE                                                         G7A2PGM 
01207 **       SET WT-01-INDEX TO +07                                   G7A2PGM 
01208 **       PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A2PGM 
01209 **       PERFORM 9100-000-SEND-THEN-RETURN.                       G7A2PGM 
01210                                                                   G7A2PGM 
01211      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7A2PGM 
01212      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7A2PGM 
01213      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7A2PGM 
01214      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7A2PGM 
01215      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7A2PGM 
01216      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7A2PGM 
01217      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7A2PGM 
01218                                                                   G7A2PGM 
01219      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7A2PGM 
01220                                                                   G7A2PGM 
01221      IF  NOT GCIO3-GOOD-RETURN                                    G7A2PGM 
01222          MOVE WS-01-ABCODE-A2F1     TO WS-01-ABCODE               G7A2PGM 
01223          MOVE WS-01-ABCODE-A2F1-MSG TO WS-01-ABCODE-MSG           G7A2PGM 
01224          PERFORM  9999-000-ABEND-THE-TASK.                        G7A2PGM 
01225                                                                   G7A2PGM 
01226      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7A2PGM 
01227                       COMMAREA(WORK-RECORD-3)                     G7A2PGM 
01228                       LENGTH  (GCIO3-RECORD-LENGTH)               G7A2PGM 
01229                       END-EXEC.                                   G7A2PGM 
01230                                                                   G7A2PGM 
01231  9210-900-EXIT.                                                   G7A2PGM 
01232      EXIT.                                                        G7A2PGM 
01233 /*****************************************************************G7A2PGM 
01234 *                                                                *G7A2PGM 
01235 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7A2PGM 
01236 *                                                                *G7A2PGM 
01237 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7A2PGM 
01238 *                                                                *G7A2PGM 
01239 ******************************************************************G7A2PGM 
01240  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7A2PGM 
01241  9800-010.                                                        G7A2PGM 
01242                                                                   G7A2PGM 
01243      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7A2PGM 
01244      MOVE 'M'   TO  HGADATE-FORM1.                                G7A2PGM 
01245      MOVE 'J'   TO  HGADATE-FORM2.                                G7A2PGM 
01246      MOVE ZEROS TO  HGADATE-RETURN                                G7A2PGM 
01247                     HGADATE-AMOUNT.                               G7A2PGM 
01248      EXEC CICS LINK PROGRAM ('HGADATES')                          G7A2PGM 
01249                     COMMAREA(HGADATES-COMMAREA)                   G7A2PGM 
01250                     LENGTH  (24)                                  G7A2PGM 
01251                     END-EXEC.                                     G7A2PGM 
01252                                                                   G7A2PGM 
01253  9800-900-900-EXIT.                                               G7A2PGM 
01254      EXIT.                                                        G7A2PGM 
01255 /*****************************************************************G7A2PGM 
01256 *                                                                *G7A2PGM 
01257 * 9810    J U L I A N    T O    G R E G O R I A N                *G7A2PGM 
01258 *                                                                *G7A2PGM 
01259 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7A2PGM 
01260 *                                                                *G7A2PGM 
01261 ******************************************************************G7A2PGM 
01262  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7A2PGM 
01263  9810-010.                                                        G7A2PGM 
01264                                                                   G7A2PGM 
01265      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7A2PGM 
01266      MOVE 'J'   TO  HGADATE-FORM1.                                G7A2PGM 
01267      MOVE 'M'   TO  HGADATE-FORM2.                                G7A2PGM 
01268      MOVE ZEROS TO  HGADATE-RETURN                                G7A2PGM 
01269                     HGADATE-AMOUNT.                               G7A2PGM 
01270      EXEC CICS LINK PROGRAM ('HGADATES')                          G7A2PGM 
01271                     COMMAREA(HGADATES-COMMAREA)                   G7A2PGM 
01272                     LENGTH  (24)                                  G7A2PGM 
01273                     END-EXEC.                                     G7A2PGM 
01274                                                                   G7A2PGM 
01275  9810-900-900-EXIT.                                               G7A2PGM 
01276      EXIT.                                                        G7A2PGM 
01277 /***************************************************************  G7A2PGM 
01278 *                                                              *  G7A2PGM 
01279 * 9999  ABEND THE TASK                                         *  G7A2PGM 
01280 *                                                              *  G7A2PGM 
01281 ****************************************************************  G7A2PGM 
01282  9999-000-ABEND-THE-TASK SECTION.                                 G7A2PGM 
01283  9999-010.                                                        G7A2PGM 
01284                                                                   G7A2PGM 
01285      EXEC CICS  ABEND                                             G7A2PGM 
01286                 ABCODE(WS-01-ABCODE)                              G7A2PGM 
01287                 END-EXEC.                                         G7A2PGM 
01288                                                                   G7A2PGM 
01289  9900-900-EXIT.                                                   G7A2PGM 
01290      EXIT.                                                        G7A2PGM 
