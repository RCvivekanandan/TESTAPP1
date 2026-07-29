00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7E2PGM.                                         G7E2PGM 
00003 ***** THIS IS A COBOL/2 PROGRAM.                                     LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7E2PGM 
00005  DATE-WRITTEN.   03/13/87.                                        G7E2PGM 
00006  DATE-COMPILED.                                                   G7E2PGM 
00007 ***************************************************************** G7E2PGM 
00008 *                                                               * G7E2PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7E2PGM 
00010 *                                                               * G7E2PGM 
00011 *                                                               * G7E2PGM 
00012 *                                                               * G7E2PGM 
00013 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7E2PGM 
00014 *                                                               * G7E2PGM 
00015 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7E2PGM 
00016 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7E2PGM 
00017 *                             (SAME AS 'GC5A')                  * G7E2PGM 
00018 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7E2PGM 
00019 *                              'GC5A')                          * G7E2PGM 
00020 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7E2PGM 
00021 *                              PROVISION)                       * G7E2PGM 
00022 *                                                               * G7E2PGM 
00023 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7E2PGM 
00024 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7E2PGM 
00025 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7E2PGM 
00026 *                                                               * G7E2PGM 
00027 *  D247   06/27/90  ENW  CONVERTED TO COBOL/2.                  * G7E2PGM 
00028 *  D247   07/19/90  ENW  CORRECTED ERRANT GETMAIN STATEMENT.    * G7E2PGM 
00029 *  P-XXX  05/04/93  ENW  COMMENTED OUT PROCESSING FOR           * G7E2PGM 
00030 *                        GPE-COST-CONT-PYMT-ELIG-IND.           * G7E2PGM 
00031 *                                                               * G7E2PGM 
00032 *  D14726    11/06/97  GDM 1. ADDED MILLENNIUM PROCESSING FOR   * G7E2PGM 
00033 *                             DATE                              * G7E2PGM 
00034 *                          2. EXPAND THE COMMAREA KEY TO        * G7E2PGM 
00035 *                             SUPPORT THE TEXAS MERGER.         * G7E2PGM 
00036 *                                                               * G7E2PGM 
00037 * 14726/     03/27/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7E2PGM 
00038 * 15057                     INCREASED GROUP AND SECTION ON      * G7E2PGM 
00039 *                           THE SCREEN.                         * G7E2PGM 
00040 *                                                               * G7E2PGM 
00041 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7E2PGM 
00042 *                           G7E2SET                              *G7E2PGM 
00043 ***************************************************************** G7E2PGM 
00044                                                                   G7E2PGM 
00045 ***************************************************************** G7E2PGM 
00046 *                                                               * G7E2PGM 
00047 *    G7E2PGM  - PROGRAM 2 OF 2 PROGRAMS TO UPDATE THE FORMAT 'E'* G7E2PGM 
00048 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7E2PGM 
00049 *                                                               * G7E2PGM 
00050 *    TRANSID: G7E2                                              * G7E2PGM 
00051 *    MAPSET:  G7E2SETC    (GIE2PGM WHICH SHARES THIS MAP)       * G7E2PGM 
00052 *    VALGEN:  NONE                                              * G7E2PGM 
00053 *                                                               * G7E2PGM 
00054 *    PROGRAM NARRATIVE:                                         * G7E2PGM 
00055 *                                                               * G7E2PGM 
00056 *        PROGRAM CHECKS FOR TRANS CODE 'G7E2'.  AN INVALID      * G7E2PGM 
00057 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7E2PGM 
00058 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7E2PGM 
00059 *                                                               * G7E2PGM 
00060 *        THE MAIN FUNCTIONS ARE :                               * G7E2PGM 
00061 *        1. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7E2PGM 
00062 *           USER,                                               * G7E2PGM 
00063 *        2. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7E2PGM 
00064 *                                                               * G7E2PGM 
00065 *        PROCESS INPUT DATA (UPDATE).                           * G7E2PGM 
00066 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7E2PGM 
00067 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7E2PGM 
00068 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7E2PGM 
00069 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7E2PGM 
00070 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7E2PGM 
00071 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7E2PGM 
00072 *                                                               * G7E2PGM 
00073 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7E2PGM 
00074 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7E2PGM 
00075 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7E2PGM 
00076 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7E2PGM 
00077 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7E2PGM 
00078 *                                                               * G7E2PGM 
00079 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7E2PGM 
00080 *                                     -----------------------   * G7E2PGM 
00081 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7E2PGM 
00082 *           -----------------------------------------------     * G7E2PGM 
00083 *                                                               * G7E2PGM 
00084 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7E2PGM 
00085 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7E2PGM 
00086 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7E2PGM 
00087 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7E2PGM 
00088 *           SENT BACK TO THE USER.                              * G7E2PGM 
00089 *                                                               * G7E2PGM 
00090 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7E2PGM 
00091 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7E2PGM 
00092 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7E2PGM 
00093 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7E2PGM 
00094 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7E2PGM 
00095 *           PF KEY LOGIC,                                       * G7E2PGM 
00096 *              PF6/PF18       GOES TO GC8APGM                   * G7E2PGM 
00097 *              PF8/PF20/ENTER GOES TO GC6APGM                   * G7E2PGM 
00098 *              FOR PF7/PF19   GOES TO G7E1PGM                   * G7E2PGM 
00099 *                                                               * G7E2PGM 
00100 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7E2PGM 
00101 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7E2PGM 
00102 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7E2PGM 
00103 *                                                               * G7E2PGM 
00104 ***************************************************************** G7E2PGM 
00105                                                                   G7E2PGM 
00106  ENVIRONMENT DIVISION.                                            G7E2PGM 
00107  DATA DIVISION.                                                   G7E2PGM 
00108 /                                                                 G7E2PGM 
00109  WORKING-STORAGE SECTION.                                         G7E2PGM 
00110  01  WS-BEGIN                    PIC X(58) VALUE                  G7E2PGM 
00111      '*** G7E2PGM  WORKING-STORAGE BEGINS HERE ***'.              G7E2PGM 
00112                                                                   G7E2PGM 
00113                                                                   G7E2PGM 
00114  01  WS-01-ABEND-AREA.                                            G7E2PGM 
00115      05  FILLER                   PIC X(16)  VALUE                G7E2PGM 
00116          '** ABEND AREA **'.                                      G7E2PGM 
00117                                                                   G7E2PGM 
00118      05  WS-01-ABEND-CODES-AND-MSG.                               G7E2PGM 
00119          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7E2PGM 
00120          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7E2PGM 
00121                                                                   G7E2PGM 
00122          10  WS-01-ABCODE-E2F1          PIC X(04)  VALUE  'E2F1'. G7E2PGM 
00123          10  WS-01-ABCODE-E2F1-MSG      PIC X(44)  VALUE          G7E2PGM 
00124             'W/F CONTRACT CANNOT BE FOUND             '.          G7E2PGM 
00125                                                                   G7E2PGM 
00126          10  WS-01-ABCODE-E2F2          PIC X(04)  VALUE  'E2F2'. G7E2PGM 
00127          10  WS-01-ABCODE-E2F2-MSG      PIC X(44)  VALUE          G7E2PGM 
00128             'W/F BEN PROV CANNOT BE FOUND             '.          G7E2PGM 
00129                                                                   G7E2PGM 
00130          10  WS-01-ABCODE-E2F3          PIC X(04)  VALUE  'E2F3'. G7E2PGM 
00131          10  WS-01-ABCODE-E2F3-MSG      PIC X(44)  VALUE          G7E2PGM 
00132             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7E2PGM 
00133                                                                   G7E2PGM 
00134          10  WS-01-ABCODE-E2F4          PIC X(04)  VALUE  'E2F4'. G7E2PGM 
00135          10  WS-01-ABCODE-E2F4-MSG      PIC X(44)  VALUE          G7E2PGM 
00136             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7E2PGM 
00137                                                                   G7E2PGM 
00138          10  WS-01-ABCODE-E2L1          PIC X(04)  VALUE  'E2L1'. G7E2PGM 
00139          10  WS-01-ABCODE-E2L1-MSG      PIC X(44)  VALUE          G7E2PGM 
00140             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7E2PGM 
00141                                                                   G7E2PGM 
00142          10  WS-01-ABCODE-E2P1          PIC X(04)  VALUE  'E2P1'. G7E2PGM 
00143          10  WS-01-ABCODE-E2P1-MSG      PIC X(44)  VALUE          G7E2PGM 
00144             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7E2PGM 
00145                                                                   G7E2PGM 
00146          10  WS-01-ABCODE-E2P2          PIC X(04)  VALUE  'E2P2'. G7E2PGM 
00147          10  WS-01-ABCODE-E2P2-MSG      PIC X(44)  VALUE          G7E2PGM 
00148             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7E2PGM 
00149                                                                   G7E2PGM 
00150  01  WS-02-AREA.                                                  G7E2PGM 
00151      05  FILLER                   PIC X(16)  VALUE                G7E2PGM 
00152          '** WS-02-AREA **'.                                      G7E2PGM 
00153      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7E2PGM 
00154          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7E2PGM 
00155                                                    'GC6A' 'G7E1'  G7E2PGM 
00156                                                    'G7E2'.        G7E2PGM 
00157          88  WS-02-MY-EIBTRNID                     VALUE  'G7E2'. G7E2PGM 
00158                                                                   G7E2PGM 
00159      05  WS-02-COMPUTED-LENGTHS.                                  G7E2PGM 
00160          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7E2PGM 
00161          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7E2PGM 
00162          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7E2PGM 
00163                                                                   G7E2PGM 
00164      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7E2PGM 
00165                                                                   G7E2PGM 
00166      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7E2PGM 
00167                                                                   G7E2PGM 
00168      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7E2PGM 
00169      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7E2PGM 
00170          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7E2PGM 
00171                                         PIC X.                    G7E2PGM 
00172          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7E2PGM 
00173                                                    '0' THRU '9'   G7E2PGM 
00174                                                    'A' THRU 'Z'   G7E2PGM 
00175                                                    SPACE.         G7E2PGM 
00176                                                                   G7E2PGM 
00177      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7E2PGM 
00178          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7E2PGM 
00179          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7E2PGM 
00180                                                                   G7E2PGM 
00181      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7E2PGM 
00182          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7E2PGM 
00183                                                                   G7E2PGM 
00184      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7E2PGM 
00185                                                                   G7E2PGM 
00186 /                                                                 G7E2PGM 
00187  01  WT-00-G7E2PGM-TABLES.                                        G7E2PGM 
00188      05  FILLER                   PIC X(16)  VALUE                G7E2PGM 
00189          '*G7E2PGM TABLES*'.                                      G7E2PGM 
00190                                                                   G7E2PGM 
00191  01  WT-01-TABLE.                                                 G7E2PGM 
00192      05  FILLER                  PIC X(16) VALUE                  G7E2PGM 
00193          '* WT-01-TABLE  *'.                                      G7E2PGM 
00194 ******************************************************************G7E2PGM 
00195 *    WT-01   MESSAGE TABLE                                       *G7E2PGM 
00196 ******************************************************************G7E2PGM 
00197  01  FILLER.                                                      G7E2PGM 
00198      05  WT-01-MESSAGE-VALUES.                                    G7E2PGM 
00199                                                                   G7E2PGM 
00200 *----------------------------------------------------------------*G7E2PGM 
00201          10  WT-01-ENTRY-001.                                     G7E2PGM 
00202              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00203              15  WT-01-MESSAGE-TEXT-001.                          G7E2PGM 
00204                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00205                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00206                  20  FILLER          PIC X(3)  VALUE  '001'.      G7E2PGM 
00207                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00208                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00209                      ' INVALID PFKEY SELECTION                    G7E2PGM 
00210 -                    '                         '.                 G7E2PGM 
00211              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00212                                                                   G7E2PGM 
00213 *----------------------------------------------------------------*G7E2PGM 
00214          10  WT-01-ENTRY-002.                                     G7E2PGM 
00215              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00216              15  WT-01-MESSAGE-TEXT-002.                          G7E2PGM 
00217                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00218                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00219                  20  FILLER          PIC X(3)  VALUE  '002'.      G7E2PGM 
00220                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00221                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00222                      '********** F U T U R E   U S E *************G7E2PGM 
00223 -                    '*************************'.                 G7E2PGM 
00224              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00225                                                                   G7E2PGM 
00226 *----------------------------------------------------------------*G7E2PGM 
00227          10  WT-01-ENTRY-003.                                     G7E2PGM 
00228              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00229              15  WT-01-MESSAGE-TEXT-003.                          G7E2PGM 
00230                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00231                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00232                  20  FILLER          PIC X(3)  VALUE  '003'.      G7E2PGM 
00233                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00234                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00235                      '********** F U T U R E   U S E *************G7E2PGM 
00236 -                    '*************************'.                 G7E2PGM 
00237              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00238                                                                   G7E2PGM 
00239 *----------------------------------------------------------------*G7E2PGM 
00240          10  WT-01-ENTRY-004.                                     G7E2PGM 
00241              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00242              15  WT-01-MESSAGE-TEXT-004.                          G7E2PGM 
00243                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00244                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00245                  20  FILLER          PIC X(3)  VALUE  '004'.      G7E2PGM 
00246                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00247                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00248                      '********** F U T U R E   U S E *************G7E2PGM 
00249 -                    '*************************'.                 G7E2PGM 
00250              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00251                                                                   G7E2PGM 
00252 *----------------------------------------------------------------*G7E2PGM 
00253          10  WT-01-ENTRY-005.                                     G7E2PGM 
00254              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00255              15  WT-01-MESSAGE-TEXT-005.                          G7E2PGM 
00256                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00257                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00258                  20  FILLER          PIC X(3)  VALUE  '005'.      G7E2PGM 
00259                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00260                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00261                      '********** F U T U R E   U S E *************G7E2PGM 
00262 -                    '*************************'.                 G7E2PGM 
00263              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00264                                                                   G7E2PGM 
00265 *----------------------------------------------------------------*G7E2PGM 
00266          10  WT-01-ENTRY-006.                                     G7E2PGM 
00267              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00268              15  WT-01-MESSAGE-TEXT-006.                          G7E2PGM 
00269                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00270                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00271                  20  FILLER          PIC X(3)  VALUE  '006'.      G7E2PGM 
00272                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00273                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00274                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7E2PGM 
00275 -                    ' PF4/PF16 TO CONTINUE    '.                 G7E2PGM 
00276              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00277                                                                   G7E2PGM 
00278 *----------------------------------------------------------------*G7E2PGM 
00279          10  WT-01-ENTRY-007.                                     G7E2PGM 
00280              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00281              15  WT-01-MESSAGE-TEXT-007.                          G7E2PGM 
00282                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00283                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00284                  20  FILLER          PIC X(3)  VALUE  '007'.      G7E2PGM 
00285                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00286                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00287                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7E2PGM 
00288 -                    'E CALL SYSTEMS           '.                 G7E2PGM 
00289              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00290                                                                   G7E2PGM 
00291 *----------------------------------------------------------------*G7E2PGM 
00292          10  WT-01-ENTRY-008.                                     G7E2PGM 
00293              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00294              15  WT-01-MESSAGE-TEXT-008.                          G7E2PGM 
00295                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00296                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00297                  20  FILLER          PIC X(3)  VALUE  '008'.      G7E2PGM 
00298                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00299                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00300                      'FIELD HAS AN INVALID VALUE                  G7E2PGM 
00301 -                    '                         '.                 G7E2PGM 
00302              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00303                                                                   G7E2PGM 
00304 *----------------------------------------------------------------*G7E2PGM 
00305          10  WT-01-ENTRY-009.                                     G7E2PGM 
00306              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00307              15  WT-01-MESSAGE-TEXT-009.                          G7E2PGM 
00308                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00309                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00310                  20  FILLER          PIC X(3)  VALUE  '009'.      G7E2PGM 
00311                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00312                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00313                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7E2PGM 
00314 -                    'YSTEM)                   '.                 G7E2PGM 
00315              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00316                                                                   G7E2PGM 
00317 *----------------------------------------------------------------*G7E2PGM 
00318          10  WT-01-ENTRY-010.                                     G7E2PGM 
00319              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E2PGM 
00320              15  WT-01-MESSAGE-TEXT-010.                          G7E2PGM 
00321                  20  FILLER          PIC X(4)  VALUE  'G7E2'.     G7E2PGM 
00322                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E2PGM 
00323                  20  FILLER          PIC X(3)  VALUE  '010'.      G7E2PGM 
00324                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E2PGM 
00325                  20  FILLER          PIC X(70) VALUE              G7E2PGM 
00326                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7E2PGM 
00327 -                    '                         '.                 G7E2PGM 
00328              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E2PGM 
00329 *----------------------------------------------------------------*G7E2PGM 
00330                                                                   G7E2PGM 
00331      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7E2PGM 
00332          WT-01-MESSAGE-VALUES         OCCURS 010 TIMES            G7E2PGM 
00333                                      INDEXED BY WT-01-INDEX.      G7E2PGM 
00334          10  WT-01-ENTRY.                                         G7E2PGM 
00335              15  FILLER              PIC X(02).                   G7E2PGM 
00336              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7E2PGM 
00337              15  FILLER              PIC X(02).                   G7E2PGM 
00338                                                                   G7E2PGM 
00339                                                                   G7E2PGM 
00340 /*** MAP FIELD ATTRIBUTES                                         G7E2PGM 
00341  COPY DFHBMSCA.                                                   G7E2PGM 
00342 *                         AUTOSKIP, BRIGHT, FSET                  G7E2PGM 
00343      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7E2PGM 
00344                                                                   G7E2PGM 
00345 /*** ATTENTION KEYS                                               G7E2PGM 
00346  COPY DFHAID.                                                     G7E2PGM 
00347                                                                   G7E2PGM 
00348 /***  PROVISION MAINTENANCE SCREEN                                G7E2PGM 
00349  COPY  G7E2SETC.                                                  G7E2PGM 
00350                                                                   G7E2PGM 
00351 /*** DATE ROUTINE COMMAREA                                        G7E2PGM 
00352  01  HGADATES-COMMAREA.                                           G7E2PGM 
00353  COPY HGCDAT01.                                                   G7E2PGM 
00354                                                                   G7E2PGM 
00355 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7E2PGM 
00356  01  GCVIOPGM-PARM-LIST.                                          G7E2PGM 
00357  COPY GCVINTRC.                                                   G7E2PGM 
00358                                                                   G7E2PGM 
00359 /*** ALTERNATIVE WORKFILE KEYS                                    G7E2PGM 
00360  01  FILLER.                                                      G7E2PGM 
00361      COPY GCWRKKEY.                                               G7E2PGM 
00362                                                                   G7E2PGM 
00363 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7E2PGM 
00364  01  FILLER.                                                      G7E2PGM 
00365      COPY GCCDRLEN.                                               G7E2PGM 
00366                                                                   G7E2PGM 
00367                                                                   G7E2PGM 
00368  01  WS-END                       PIC X(58) VALUE                 G7E2PGM 
00369      '*** G7E2PGM  WORKING-STORAGE ENDS HERE ***'.                G7E2PGM 
00370 /                                                                 G7E2PGM 
00371  LINKAGE SECTION.                                                 G7E2PGM 
00372 /                                                                 G7E2PGM 
00373  01  DFHCOMMAREA.                                                 G7E2PGM 
00374      COPY  GCWRKDCC.                                              G7E2PGM 
00375      COPY  GCBENPVC.                                              G7E2PGM 
00376 /                                                                 G7E2PGM 
00377 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7E2PGM 
00378  01  IO-PARM-BEN-PROV-AREA.                                       G7E2PGM 
00379      COPY  GCIOPRM2.                                              G7E2PGM 
00380      COPY  GCWRKDC2.                                              G7E2PGM 
00381      COPY  GCBENPV2.                                              G7E2PGM 
00382                                                                   G7E2PGM 
00383 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7E2PGM 
00384  01  IO-PARM-CONTRACT-AREA.                                       G7E2PGM 
00385      COPY  GCIOPRM3.                                              G7E2PGM 
00386      COPY  GCWRKDC3.                                              G7E2PGM 
00387      COPY  GCCONTR2.                                              G7E2PGM 
00388 /                                                                 G7E2PGM 
00389  PROCEDURE DIVISION.                                              G7E2PGM 
00390                                                                   G7E2PGM 
00391 ****************************************************************  G7E2PGM 
00392 *                                                              *  G7E2PGM 
00393 *           P R O C E S S     C O N T R O L                    *  G7E2PGM 
00394 *                                                              *  G7E2PGM 
00395 ****************************************************************  G7E2PGM 
00396  0000-000-PROCESS-CONTROL       SECTION.                          G7E2PGM 
00397  0000-010.                                                        G7E2PGM 
00398                                                                   G7E2PGM 
00399      IF  EIBAID  =  DFHCLEAR                                      G7E2PGM 
00400          EXEC CICS  RETURN                                        G7E2PGM 
00401                     END-EXEC.                                     G7E2PGM 
00402                                                                   G7E2PGM 
00403      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7E2PGM 
00404                                                                   G7E2PGM 
00405      IF  WS-02-MY-EIBTRNID                                        G7E2PGM 
00406      THEN                                                         G7E2PGM 
00407          PERFORM  2000-000-PROCESS-INPUT                          G7E2PGM 
00408      ELSE                                                         G7E2PGM 
00409          PERFORM  1000-000-DISPLAY-SCREEN.                        G7E2PGM 
00410                                                                   G7E2PGM 
00411                                                                   G7E2PGM 
00412 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7E2PGM 
00413                                                                   G7E2PGM 
00414      MOVE WS-01-ABCODE-E2L1     TO WS-01-ABCODE                   G7E2PGM 
00415      MOVE WS-01-ABCODE-E2L1-MSG TO WS-01-ABCODE-MSG               G7E2PGM 
00416      PERFORM  9999-000-ABEND-THE-TASK.                            G7E2PGM 
00417                                                                   G7E2PGM 
00418      GOBACK.                                                      G7E2PGM 
00419                                                                   G7E2PGM 
00420                                                                   G7E2PGM 
00421  0000-900-EXIT.                                                   G7E2PGM 
00422      EXIT.                                                        G7E2PGM 
00423 /***************************************************************  G7E2PGM 
00424 *                                                              *  G7E2PGM 
00425 * 1000  DISPLAY INITIAL SCREEN                                 *  G7E2PGM 
00426 *                                                              *  G7E2PGM 
00427 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7E2PGM 
00428 *                                                              *  G7E2PGM 
00429 ****************************************************************  G7E2PGM 
00430  1000-000-DISPLAY-SCREEN        SECTION.                          G7E2PGM 
00431  1000-010.                                                        G7E2PGM 
00432                                                                   G7E2PGM 
00433 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7E2PGM 
00434                                                                   G7E2PGM 
00435      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7E2PGM 
00436          MOVE WS-01-ABCODE-E2P1     TO WS-01-ABCODE               G7E2PGM 
00437          MOVE WS-01-ABCODE-E2P1-MSG TO WS-01-ABCODE-MSG           G7E2PGM 
00438          PERFORM 9999-000-ABEND-THE-TASK.                         G7E2PGM 
00439                                                                   G7E2PGM 
00440                                                                   G7E2PGM 
00441 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7E2PGM 
00442                                                                   G7E2PGM 
00443      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7E2PGM 
00444                                         + GC-GCBENPRV-FIXED-LEN   G7E2PGM 
00445                                         + GC-GCBENPRV-VARY-LEN.   G7E2PGM 
00446                                                                   G7E2PGM 
00447                                                                   G7E2PGM 
00448 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7E2PGM 
00449                                                                   G7E2PGM 
00450      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7E2PGM 
00451          MOVE WS-01-ABCODE-E2P2     TO WS-01-ABCODE               G7E2PGM 
00452          MOVE WS-01-ABCODE-E2P2-MSG TO WS-01-ABCODE-MSG           G7E2PGM 
00453          PERFORM 9999-000-ABEND-THE-TASK.                         G7E2PGM 
00454                                                                   G7E2PGM 
00455                                                                   G7E2PGM 
00456 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7E2PGM 
00457 *          BY CALLER IN COMMAREA.                                 G7E2PGM 
00458                                                                   G7E2PGM 
00459      MOVE LOW-VALUES TO G7E2I01O.                                 G7E2PGM 
00460                                                                   G7E2PGM 
00461      MOVE WRK-PLAN-CODE                      TO S2PLNCDO.         G7E2PGM 
00462      MOVE WRK-GROUP-NUM                      TO S2GRPNOO.         G7E2PGM 
00463      MOVE WRK-SECTION-NUM                    TO S2SECNOO.         G7E2PGM 
00464      MOVE WRK-PKG-CODE                       TO S2PKGCDO.         G7E2PGM 
00465      MOVE WRK-PROV-CTL                       TO S2PRVO.           G7E2PGM 
00466      MOVE WRK-FAM-REL-LEVEL                  TO S2FRLO.           G7E2PGM 
00467      MOVE WRK-L-O-B                          TO S2LOBO.           G7E2PGM 
00468                                                                   G7E2PGM 
00469      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7E2PGM 
00470      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7E2PGM 
00471      IF  HGADATE-RETURN = ZEROS                                   G7E2PGM 
00472      THEN                                                         G7E2PGM 
00473          MOVE DFHBMASF                       TO S2EFFDTA          G7E2PGM 
00474          MOVE HGADATE-DATE2                  TO S2EFFDTO          G7E2PGM 
00475      ELSE                                                         G7E2PGM 
00476          MOVE DFHBMABF                       TO S2EFFDTA          G7E2PGM 
00477          MOVE HGADATE-JULIAN1                TO S2EFFDTO.         G7E2PGM 
00478                                                                   G7E2PGM 
00479      MOVE GCP-PROVN-ID                       TO S2BPVIDO.         G7E2PGM 
00480                                                                   G7E2PGM 
00481      MOVE GPE-CERTN-REPETN-REQRD-IND         TO S2RRCERO.         G7E2PGM 
00482      MOVE GPE-REPR-REPLAC-RESTRN-IND         TO S2RRRSTO.         G7E2PGM 
00483      MOVE GPE-PHYS-EXAM-IND                  TO S2PHEXIO.         G7E2PGM 
00484      MOVE GPE-AMBULANCE-ELIG-IND             TO S2AMBELO.         G7E2PGM 
00485 *    MOVE GPE-COST-CONT-PYMT-ELIG-IND        TO S2CCPEIO.         G7E2PGM 
00486                                                                   G7E2PGM 
00487                                                                   G7E2PGM 
00488                                                                   G7E2PGM 
00489 *------- SEND INITIAL SCREEN ------------------------------------*G7E2PGM 
00490                                                                   G7E2PGM 
00491      MOVE  -1 TO  S2RRCERL.                                       G7E2PGM 
00492      PERFORM 9100-000-SEND-THEN-RETURN.                           G7E2PGM 
00493                                                                   G7E2PGM 
00494                                                                   G7E2PGM 
00495  1000-900-EXIT.                                                   G7E2PGM 
00496      EXIT.                                                        G7E2PGM 
00497 /***************************************************************  G7E2PGM 
00498 *                                                              *  G7E2PGM 
00499 * 2000    P R O C E S S    I N P U T                           *  G7E2PGM 
00500 *                                                              *  G7E2PGM 
00501 ****************************************************************  G7E2PGM 
00502  2000-000-PROCESS-INPUT         SECTION.                          G7E2PGM 
00503  2000-010.                                                        G7E2PGM 
00504                                                                   G7E2PGM 
00505 *------ VALIDATE PFKEY USAGE ------------------------------------*G7E2PGM 
00506                                                                   G7E2PGM 
00507      IF  EIBAID = DFHENTER OR                                     G7E2PGM 
00508                   DFHPF3   OR  DFHPF15 OR                         G7E2PGM 
00509                   DFHPF4   OR  DFHPF16 OR                         G7E2PGM 
00510                   DFHPF6   OR  DFHPF18 OR                         G7E2PGM 
00511                   DFHPF7   OR  DFHPF19 OR                         G7E2PGM 
00512                   DFHPF8   OR  DFHPF20                            G7E2PGM 
00513      THEN                                                         G7E2PGM 
00514          NEXT SENTENCE                                            G7E2PGM 
00515      ELSE                                                         G7E2PGM 
00516          SET WT-01-INDEX TO +01                                   G7E2PGM 
00517          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7E2PGM 
00518          PERFORM 9100-000-SEND-THEN-RETURN.                       G7E2PGM 
00519                                                                   G7E2PGM 
00520                                                                   G7E2PGM 
00521                                                                   G7E2PGM 
00522      EXEC CICS  HANDLE CONDITION                                  G7E2PGM 
00523                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7E2PGM 
00524                        END-EXEC.                                  G7E2PGM 
00525                                                                   G7E2PGM 
00526                                                                   G7E2PGM 
00527      EXEC CICS  RECEIVE MAP   ('G7E2I01')                         G7E2PGM 
00528                         MAPSET('G7E2SET')                         G7E2PGM 
00529                         END-EXEC.                                 G7E2PGM 
00530                                                                   G7E2PGM 
00531                                                                   G7E2PGM 
00532      IF  S2FUNCI  NOT = 'G7E2'  OR                                G7E2PGM 
00533          S2SCRNI  NOT = '007E02'                                  G7E2PGM 
00534          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7E2PGM 
00535                                                                   G7E2PGM 
00536                                                                   G7E2PGM 
00537 *--- RETURN TO GCPS MENU? ---------------------------------------*G7E2PGM 
00538                                                                   G7E2PGM 
00539      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7E2PGM 
00540          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7E2PGM 
00541                                                                   G7E2PGM 
00542 *--- SCREEN PRINT REQUESTED? ------------------------------------*G7E2PGM 
00543                                                                   G7E2PGM 
00544                                                                   G7E2PGM 
00545 *--- PROCESS SCREEN FIELDS --------------------------------------*G7E2PGM 
00546                                                                   G7E2PGM 
00547      PERFORM 2100-000-FIELD-EDITS.                                G7E2PGM 
00548                                                                   G7E2PGM 
00549      IF  WS-02-SCREEN-HAS-ERRORS                                  G7E2PGM 
00550          PERFORM 9100-000-SEND-THEN-RETURN.                       G7E2PGM 
00551                                                                   G7E2PGM 
00552      PERFORM 2200-000-LOGICAL-EDITS.                              G7E2PGM 
00553                                                                   G7E2PGM 
00554      IF  WS-02-SCREEN-HAS-ERRORS                                  G7E2PGM 
00555          PERFORM 9100-000-SEND-THEN-RETURN.                       G7E2PGM 
00556                                                                   G7E2PGM 
00557      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7E2PGM 
00558                                                                   G7E2PGM 
00559      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7E2PGM 
00560                                                                   G7E2PGM 
00561                                                                   G7E2PGM 
00562  2000-900-EXIT.                                                   G7E2PGM 
00563      EXIT.                                                        G7E2PGM 
00564 /***************************************************************  G7E2PGM 
00565 *                                                              *  G7E2PGM 
00566 * 2100  DO SCREEN FIELD EDITS                                  *  G7E2PGM 
00567 *                                                              *  G7E2PGM 
00568 ****************************************************************  G7E2PGM 
00569  2100-000-FIELD-EDITS           SECTION.                          G7E2PGM 
00570  2100-010.                                                        G7E2PGM 
00571                                                                   G7E2PGM 
00572 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7E2PGM 
00573                                                                   G7E2PGM 
00574      MOVE DFHBMUNF TO  S2RRCERA                                   G7E2PGM 
00575                        S2RRRSTA                                   G7E2PGM 
00576                        S2PHEXIA                                   G7E2PGM 
00577                        S2AMBELA.                                  G7E2PGM 
00578 *                      S2CCPEIA.                                  G7E2PGM 
00579                                                                   G7E2PGM 
00580      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7E2PGM 
00581                                                                   G7E2PGM 
00582                                                                   G7E2PGM 
00583 *-- VALIDATE ------ CERTIFICATION REPETITION REQUIREMENT IND ----*G7E2PGM 
00584 *   1. ALPHANUMERIC                                               G7E2PGM 
00585 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E2PGM 
00586                                                                   G7E2PGM 
00587      MOVE  S2RRCERI TO WS-02-CLASS-TEST-AREA.                     G7E2PGM 
00588      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E2PGM 
00589      THEN                                                         G7E2PGM 
00590          MOVE  S2RRCERI TO GCVI-VALUE                             G7E2PGM 
00591          MOVE  'BPAB02' TO GCVI-FIELDS-KEY-ID                     G7E2PGM 
00592          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E2PGM 
00593          IF  GCVI-VALUE-NOT-FOUND                                 G7E2PGM 
00594          THEN                                                     G7E2PGM 
00595              MOVE  -1        TO  S2RRCERL                         G7E2PGM 
00596              MOVE  DFHBMUBF  TO  S2RRCERA                         G7E2PGM 
00597              IF  WS-02-SCREEN-HAS-ERRORS                          G7E2PGM 
00598              THEN                                                 G7E2PGM 
00599                  NEXT SENTENCE                                    G7E2PGM 
00600              ELSE                                                 G7E2PGM 
00601                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E2PGM 
00602                  SET WT-01-INDEX TO +09                           G7E2PGM 
00603                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E2PGM 
00604          ELSE                                                     G7E2PGM 
00605              IF  GCVI-VALUE-NOT-LOADED                            G7E2PGM 
00606              THEN                                                 G7E2PGM 
00607                  MOVE  DFHBMUBF  TO  S2RRCERA                     G7E2PGM 
00608              ELSE                                                 G7E2PGM 
00609                  NEXT SENTENCE                                    G7E2PGM 
00610      ELSE                                                         G7E2PGM 
00611          MOVE  -1        TO  S2RRCERL                             G7E2PGM 
00612          MOVE  DFHBMUBF  TO  S2RRCERA                             G7E2PGM 
00613          IF  WS-02-SCREEN-HAS-ERRORS                              G7E2PGM 
00614          THEN                                                     G7E2PGM 
00615              NEXT SENTENCE                                        G7E2PGM 
00616          ELSE                                                     G7E2PGM 
00617              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E2PGM 
00618              SET WT-01-INDEX TO +08                               G7E2PGM 
00619              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E2PGM 
00620                                                                   G7E2PGM 
00621                                                                   G7E2PGM 
00622 *-- VALIDATE ------ REPAIR/REPLACE RESTRICTION IND --------------*G7E2PGM 
00623 *   1. ALPHANUMERIC                                               G7E2PGM 
00624 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E2PGM 
00625                                                                   G7E2PGM 
00626      MOVE  S2RRRSTI TO WS-02-CLASS-TEST-AREA.                     G7E2PGM 
00627      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E2PGM 
00628      THEN                                                         G7E2PGM 
00629          MOVE  S2RRRSTI TO GCVI-VALUE                             G7E2PGM 
00630          MOVE  'BPBA10' TO GCVI-FIELDS-KEY-ID                     G7E2PGM 
00631          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E2PGM 
00632          IF  GCVI-VALUE-NOT-FOUND                                 G7E2PGM 
00633          THEN                                                     G7E2PGM 
00634              MOVE  -1        TO  S2RRRSTL                         G7E2PGM 
00635              MOVE  DFHBMUBF  TO  S2RRRSTA                         G7E2PGM 
00636              IF  WS-02-SCREEN-HAS-ERRORS                          G7E2PGM 
00637              THEN                                                 G7E2PGM 
00638                  NEXT SENTENCE                                    G7E2PGM 
00639              ELSE                                                 G7E2PGM 
00640                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E2PGM 
00641                  SET WT-01-INDEX TO +09                           G7E2PGM 
00642                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E2PGM 
00643          ELSE                                                     G7E2PGM 
00644              IF  GCVI-VALUE-NOT-LOADED                            G7E2PGM 
00645              THEN                                                 G7E2PGM 
00646                  MOVE  DFHBMUBF  TO  S2RRRSTA                     G7E2PGM 
00647              ELSE                                                 G7E2PGM 
00648                  NEXT SENTENCE                                    G7E2PGM 
00649      ELSE                                                         G7E2PGM 
00650          MOVE  -1        TO  S2RRRSTL                             G7E2PGM 
00651          MOVE  DFHBMUBF  TO  S2RRRSTA                             G7E2PGM 
00652          IF  WS-02-SCREEN-HAS-ERRORS                              G7E2PGM 
00653          THEN                                                     G7E2PGM 
00654              NEXT SENTENCE                                        G7E2PGM 
00655          ELSE                                                     G7E2PGM 
00656              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E2PGM 
00657              SET WT-01-INDEX TO +08                               G7E2PGM 
00658              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E2PGM 
00659                                                                   G7E2PGM 
00660                                                                   G7E2PGM 
00661 *-- VALIDATE ------ PHYSICAL EXAM IND ---------------------------*G7E2PGM 
00662 *   1. ALPHANUMERIC                                               G7E2PGM 
00663 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E2PGM 
00664                                                                   G7E2PGM 
00665      MOVE  S2PHEXII TO WS-02-CLASS-TEST-AREA.                     G7E2PGM 
00666      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E2PGM 
00667      THEN                                                         G7E2PGM 
00668          MOVE  S2PHEXII TO GCVI-VALUE                             G7E2PGM 
00669          MOVE  'BPBB01' TO GCVI-FIELDS-KEY-ID                     G7E2PGM 
00670          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E2PGM 
00671          IF  GCVI-VALUE-NOT-FOUND                                 G7E2PGM 
00672          THEN                                                     G7E2PGM 
00673              MOVE  -1        TO  S2PHEXIL                         G7E2PGM 
00674              MOVE  DFHBMUBF  TO  S2PHEXIA                         G7E2PGM 
00675              IF  WS-02-SCREEN-HAS-ERRORS                          G7E2PGM 
00676              THEN                                                 G7E2PGM 
00677                  NEXT SENTENCE                                    G7E2PGM 
00678              ELSE                                                 G7E2PGM 
00679                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E2PGM 
00680                  SET WT-01-INDEX TO +09                           G7E2PGM 
00681                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E2PGM 
00682          ELSE                                                     G7E2PGM 
00683              IF  GCVI-VALUE-NOT-LOADED                            G7E2PGM 
00684              THEN                                                 G7E2PGM 
00685                  MOVE  DFHBMUBF  TO  S2PHEXIA                     G7E2PGM 
00686              ELSE                                                 G7E2PGM 
00687                  NEXT SENTENCE                                    G7E2PGM 
00688      ELSE                                                         G7E2PGM 
00689          MOVE  -1        TO  S2PHEXIL                             G7E2PGM 
00690          MOVE  DFHBMUBF  TO  S2PHEXIA                             G7E2PGM 
00691          IF  WS-02-SCREEN-HAS-ERRORS                              G7E2PGM 
00692          THEN                                                     G7E2PGM 
00693              NEXT SENTENCE                                        G7E2PGM 
00694          ELSE                                                     G7E2PGM 
00695              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E2PGM 
00696              SET WT-01-INDEX TO +08                               G7E2PGM 
00697              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E2PGM 
00698                                                                   G7E2PGM 
00699                                                                   G7E2PGM 
00700 *-- VALIDATE ------ AMBULANCE ELIGIBILITY IND -------------------*G7E2PGM 
00701 *   1. ALPHANUMERIC                                               G7E2PGM 
00702 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E2PGM 
00703                                                                   G7E2PGM 
00704      MOVE  S2AMBELI TO WS-02-CLASS-TEST-AREA.                     G7E2PGM 
00705      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7E2PGM 
00706          WS-02-CLASS-ALPHANUMERIC(2)                              G7E2PGM 
00707      THEN                                                         G7E2PGM 
00708          MOVE  S2AMBELI TO GCVI-VALUE                             G7E2PGM 
00709          MOVE  'BPBB03' TO GCVI-FIELDS-KEY-ID                     G7E2PGM 
00710          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E2PGM 
00711          IF  GCVI-VALUE-NOT-FOUND                                 G7E2PGM 
00712          THEN                                                     G7E2PGM 
00713              MOVE  -1        TO  S2AMBELL                         G7E2PGM 
00714              MOVE  DFHBMUBF  TO  S2AMBELA                         G7E2PGM 
00715              IF  WS-02-SCREEN-HAS-ERRORS                          G7E2PGM 
00716              THEN                                                 G7E2PGM 
00717                  NEXT SENTENCE                                    G7E2PGM 
00718              ELSE                                                 G7E2PGM 
00719                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E2PGM 
00720                  SET WT-01-INDEX TO +09                           G7E2PGM 
00721                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E2PGM 
00722          ELSE                                                     G7E2PGM 
00723              IF  GCVI-VALUE-NOT-LOADED                            G7E2PGM 
00724              THEN                                                 G7E2PGM 
00725                  MOVE  DFHBMUBF  TO  S2AMBELA                     G7E2PGM 
00726              ELSE                                                 G7E2PGM 
00727                  NEXT SENTENCE                                    G7E2PGM 
00728      ELSE                                                         G7E2PGM 
00729          MOVE  -1        TO  S2AMBELL                             G7E2PGM 
00730          MOVE  DFHBMUBF  TO  S2AMBELA                             G7E2PGM 
00731          IF  WS-02-SCREEN-HAS-ERRORS                              G7E2PGM 
00732          THEN                                                     G7E2PGM 
00733              NEXT SENTENCE                                        G7E2PGM 
00734          ELSE                                                     G7E2PGM 
00735              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E2PGM 
00736              SET WT-01-INDEX TO +08                               G7E2PGM 
00737              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E2PGM 
00738                                                                   G7E2PGM 
00739                                                                   G7E2PGM 
00740 *-- VALIDATE ------ COST CONT.PAYMNT ELIGIBILITY IND ------------*G7E2PGM 
00741 *   1. ALPHANUMERIC                                               G7E2PGM 
00742 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E2PGM 
00743                                                                   G7E2PGM 
00744 *    MOVE  S2CCPEII TO WS-02-CLASS-TEST-AREA.                     G7E2PGM 
00745 *    IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E2PGM 
00746 *    THEN                                                         G7E2PGM 
00747 *        MOVE  S2CCPEII TO GCVI-VALUE                             G7E2PGM 
00748 *        MOVE  'BPEB05' TO GCVI-FIELDS-KEY-ID                     G7E2PGM 
00749 *        PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E2PGM 
00750 *        IF  GCVI-VALUE-NOT-FOUND                                 G7E2PGM 
00751 *        THEN                                                     G7E2PGM 
00752 *            MOVE  -1        TO  S2CCPEIL                         G7E2PGM 
00753 *            MOVE  DFHBMUBF  TO  S2CCPEIA                         G7E2PGM 
00754 *            IF  WS-02-SCREEN-HAS-ERRORS                          G7E2PGM 
00755 *            THEN                                                 G7E2PGM 
00756 *                NEXT SENTENCE                                    G7E2PGM 
00757 *            ELSE                                                 G7E2PGM 
00758 *                MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E2PGM 
00759 *                SET WT-01-INDEX TO +09                           G7E2PGM 
00760 *                PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E2PGM 
00761 *        ELSE                                                     G7E2PGM 
00762 *            IF  GCVI-VALUE-NOT-LOADED                            G7E2PGM 
00763 *            THEN                                                 G7E2PGM 
00764 *                MOVE  DFHBMUBF  TO  S2CCPEIA                     G7E2PGM 
00765 *            ELSE                                                 G7E2PGM 
00766 *                NEXT SENTENCE                                    G7E2PGM 
00767 *    ELSE                                                         G7E2PGM 
00768 *        MOVE  -1        TO  S2CCPEIL                             G7E2PGM 
00769 *        MOVE  DFHBMUBF  TO  S2CCPEIA                             G7E2PGM 
00770 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7E2PGM 
00771 *        THEN                                                     G7E2PGM 
00772 *            NEXT SENTENCE                                        G7E2PGM 
00773 *        ELSE                                                     G7E2PGM 
00774 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E2PGM 
00775 *            SET WT-01-INDEX TO +08                               G7E2PGM 
00776 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E2PGM 
00777                                                                   G7E2PGM 
00778                                                                   G7E2PGM 
00779  2100-900-EXIT.                                                   G7E2PGM 
00780      EXIT.                                                        G7E2PGM 
00781 /***************************************************************  G7E2PGM 
00782 *                                                              *  G7E2PGM 
00783 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7E2PGM 
00784 *                                                              *  G7E2PGM 
00785 ****************************************************************  G7E2PGM 
00786  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7E2PGM 
00787  2110-010.                                                        G7E2PGM 
00788                                                                   G7E2PGM 
00789      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7E2PGM 
00790                                                                   G7E2PGM 
00791      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7E2PGM 
00792                       COMMAREA(GCVIOPGM-PARM-LIST)                G7E2PGM 
00793                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7E2PGM 
00794                       END-EXEC.                                   G7E2PGM 
00795                                                                   G7E2PGM 
00796      IF  GCVI-VALUE-NOT-LOADED                                    G7E2PGM 
00797          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7E2PGM 
00798                                                                   G7E2PGM 
00799  2110-900-EXIT.                                                   G7E2PGM 
00800      EXIT.                                                        G7E2PGM 
00801 /***************************************************************  G7E2PGM 
00802 *                                                              *  G7E2PGM 
00803 * 2200  DO SCREEN LOGICAL EDITS                                *  G7E2PGM 
00804 *                                                              *  G7E2PGM 
00805 ****************************************************************  G7E2PGM 
00806  2200-000-LOGICAL-EDITS         SECTION.                          G7E2PGM 
00807  2200-010.                                                        G7E2PGM 
00808                                                                   G7E2PGM 
00809 *----------------------------------------------------------------*G7E2PGM 
00810 *                                                                *G7E2PGM 
00811 *                                                                *G7E2PGM 
00812 *   THIS PROGRAM HAS NO REQUIREMENT FOR LOGICAL EDITS            *G7E2PGM 
00813 *                                                                *G7E2PGM 
00814 *                                                                *G7E2PGM 
00815 *----------------------------------------------------------------*G7E2PGM 
00816                                                                   G7E2PGM 
00817                                                                   G7E2PGM 
00818 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7E2PGM 
00819                                                                   G7E2PGM 
00820      IF  WS-02-SCREEN-HAS-ERRORS                                  G7E2PGM 
00821      THEN                                                         G7E2PGM 
00822          NEXT SENTENCE                                            G7E2PGM 
00823      ELSE                                                         G7E2PGM 
00824          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7E2PGM 
00825          THEN                                                     G7E2PGM 
00826              IF EIBAID = DFHPF4 OR DFHPF16                        G7E2PGM 
00827              THEN                                                 G7E2PGM 
00828                  NEXT SENTENCE                                    G7E2PGM 
00829              ELSE                                                 G7E2PGM 
00830                  MOVE  -1        TO S2ERRL                        G7E2PGM 
00831                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E2PGM 
00832                  SET WT-01-INDEX TO +06                           G7E2PGM 
00833                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E2PGM 
00834          ELSE                                                     G7E2PGM 
00835              NEXT SENTENCE.                                       G7E2PGM 
00836                                                                   G7E2PGM 
00837                                                                   G7E2PGM 
00838  2200-900-EXIT.                                                   G7E2PGM 
00839      EXIT.                                                        G7E2PGM 
00840 /***************************************************************  G7E2PGM 
00841 *                                                              *  G7E2PGM 
00842 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7E2PGM 
00843 *        REWRITE TO WORKFILE.                                  *  G7E2PGM 
00844 *                                                              *  G7E2PGM 
00845 ****************************************************************  G7E2PGM 
00846  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7E2PGM 
00847  2300-010.                                                        G7E2PGM 
00848                                                                   G7E2PGM 
00849 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7E2PGM 
00850                                                                   G7E2PGM 
00851      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7E2PGM 
00852      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7E2PGM 
00853      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7E2PGM 
00854      IF  NOT GCIO2-GOOD-RETURN                                    G7E2PGM 
00855          MOVE WS-01-ABCODE-E2F2     TO WS-01-ABCODE               G7E2PGM 
00856          MOVE WS-01-ABCODE-E2F2-MSG TO WS-01-ABCODE-MSG           G7E2PGM 
00857          PERFORM  9999-000-ABEND-THE-TASK.                        G7E2PGM 
00858                                                                   G7E2PGM 
00859                                                                   G7E2PGM 
00860 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7E2PGM 
00861                                                                   G7E2PGM 
00862      IF      S2RRCERI  = GPE2-CERTN-REPETN-REQRD-IND              G7E2PGM 
00863          AND S2RRRSTI  = GPE2-REPR-REPLAC-RESTRN-IND              G7E2PGM 
00864          AND S2PHEXII  = GPE2-PHYS-EXAM-IND                       G7E2PGM 
00865          AND S2AMBELI  = GPE2-AMBULANCE-ELIG-IND                  G7E2PGM 
00866 *        AND S2CCPEII  = GPE2-COST-CONT-PYMT-ELIG-IND             G7E2PGM 
00867      THEN                                                         G7E2PGM 
00868          GO TO 2300-900-EXIT                                      G7E2PGM 
00869      ELSE                                                         G7E2PGM 
00870          NEXT SENTENCE.                                           G7E2PGM 
00871                                                                   G7E2PGM 
00872                                                                   G7E2PGM 
00873 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7E2PGM 
00874                                                                   G7E2PGM 
00875      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7E2PGM 
00876      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7E2PGM 
00877      IF  NOT GCIO2-GOOD-RETURN                                    G7E2PGM 
00878          MOVE WS-01-ABCODE-E2F3     TO WS-01-ABCODE               G7E2PGM 
00879          MOVE WS-01-ABCODE-E2F3-MSG TO WS-01-ABCODE-MSG           G7E2PGM 
00880          PERFORM  9999-000-ABEND-THE-TASK.                        G7E2PGM 
00881                                                                   G7E2PGM 
00882                                                                   G7E2PGM 
00883 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7E2PGM 
00884                                                                   G7E2PGM 
00885      MOVE S2RRCERI  TO GPE2-CERTN-REPETN-REQRD-IND.               G7E2PGM 
00886      MOVE S2RRRSTI  TO GPE2-REPR-REPLAC-RESTRN-IND.               G7E2PGM 
00887      MOVE S2PHEXII  TO GPE2-PHYS-EXAM-IND.                        G7E2PGM 
00888      MOVE S2AMBELI  TO GPE2-AMBULANCE-ELIG-IND.                   G7E2PGM 
00889 *    MOVE S2CCPEII  TO GPE2-COST-CONT-PYMT-ELIG-IND.              G7E2PGM 
00890                                                                   G7E2PGM 
00891                                                                   G7E2PGM 
00892 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7E2PGM 
00893                                                                   G7E2PGM 
00894 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7E2PGM 
00895                                                                   G7E2PGM 
00896      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7E2PGM 
00897      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7E2PGM 
00898      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7E2PGM 
00899      IF  NOT GCIO2-GOOD-RETURN                                    G7E2PGM 
00900          MOVE WS-01-ABCODE-E2F4     TO WS-01-ABCODE               G7E2PGM 
00901          MOVE WS-01-ABCODE-E2F4-MSG TO WS-01-ABCODE-MSG           G7E2PGM 
00902          PERFORM  9999-000-ABEND-THE-TASK.                        G7E2PGM 
00903                                                                   G7E2PGM 
00904  2300-900-EXIT.                                                   G7E2PGM 
00905      EXIT.                                                        G7E2PGM 
00906 /***************************************************************  G7E2PGM 
00907 *                                                              *  G7E2PGM 
00908 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7E2PGM 
00909 *                                                              *  G7E2PGM 
00910 ****************************************************************  G7E2PGM 
00911  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7E2PGM 
00912  2310-010.                                                        G7E2PGM 
00913                                                                   G7E2PGM 
00914                                                                   G7E2PGM 
00915 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7E2PGM 
00916                                                                   G7E2PGM 
00917      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7E2PGM 
00918                                         + GC-WORKFILE-KEY-LEN     G7E2PGM 
00919                                         + GC-GCBENPRV-MAX-REC-LEN.G7E2PGM 
00920                                                                   G7E2PGM 
00921      EXEC CICS GETMAIN                                            G7E2PGM 
00922                 SET (ADDRESS OF IO-PARM-BEN-PROV-AREA)            G7E2PGM 
00923                 INITIMG(WS-02-HEX-00)                             G7E2PGM 
00924                 LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)               G7E2PGM 
00925                 END-EXEC.                                         G7E2PGM 
00926                                                                   G7E2PGM 
00927 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7E2PGM 
00928                                                                   G7E2PGM 
00929      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7E2PGM 
00930                                                                   G7E2PGM 
00931      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7E2PGM 
00932      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7E2PGM 
00933      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7E2PGM 
00934      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7E2PGM 
00935      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7E2PGM 
00936      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7E2PGM 
00937                                                                   G7E2PGM 
00938      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7E2PGM 
00939      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7E2PGM 
00940      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7E2PGM 
00941      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7E2PGM 
00942      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7E2PGM 
00943      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7E2PGM 
00944      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7E2PGM 
00945      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7E2PGM 
00946                                                                   G7E2PGM 
00947 ***  MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7E2PGM 
00948 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7E2PGM 
00949 ***  IF  HGADATE-RETURN = ZEROS                                   G7E2PGM 
00950 ***  THEN                                                         G7E2PGM 
00951 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7E2PGM 
00952 ***  ELSE                                                         G7E2PGM 
00953 ***      SET WT-01-INDEX TO +07                                   G7E2PGM 
00954 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7E2PGM 
00955 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7E2PGM 
00956                                                                   G7E2PGM 
00957      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7E2PGM 
00958      MOVE S2BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7E2PGM 
00959      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7E2PGM 
00960      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7E2PGM 
00961      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7E2PGM 
00962      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7E2PGM 
00963      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7E2PGM 
00964                                                                   G7E2PGM 
00965 ***** PRIME COUNT FOR COBOL/2.                                    G7E2PGM 
00966      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7E2PGM 
00967        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7E2PGM 
00968                                                                   G7E2PGM 
00969  2310-900-EXIT.                                                   G7E2PGM 
00970      EXIT.                                                        G7E2PGM 
00971 /***************************************************************  G7E2PGM 
00972 *                                                              *  G7E2PGM 
00973 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7E2PGM 
00974 *                                                              *  G7E2PGM 
00975 ****************************************************************  G7E2PGM 
00976  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7E2PGM 
00977  2400-010.                                                        G7E2PGM 
00978                                                                   G7E2PGM 
00979                                                                   G7E2PGM 
00980      IF  EIBAID = DFHPF7  OR DFHPF19                              G7E2PGM 
00981      THEN                                                         G7E2PGM 
00982          MOVE 'G7E1PGM' TO WS-02-NEXT-PROGRAM.                    G7E2PGM 
00983                                                                   G7E2PGM 
00984      IF  EIBAID = DFHENTER OR                                     G7E2PGM 
00985                   DFHPF4   OR DFHPF16 OR                          G7E2PGM 
00986                   DFHPF8   OR DFHPF20                             G7E2PGM 
00987      THEN                                                         G7E2PGM 
00988          MOVE 'GC6APGM' TO WS-02-NEXT-PROGRAM.                    G7E2PGM 
00989                                                                   G7E2PGM 
00990      IF  EIBAID = DFHPF6  OR DFHPF18                              G7E2PGM 
00991      THEN                                                         G7E2PGM 
00992          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7E2PGM 
00993                                                                   G7E2PGM 
00994                                                                   G7E2PGM 
00995      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7E2PGM 
00996                       COMMAREA(WORK-RECORD-2)                     G7E2PGM 
00997                       LENGTH  (GCIO2-RECORD-LENGTH)               G7E2PGM 
00998                       END-EXEC.                                   G7E2PGM 
00999                                                                   G7E2PGM 
01000  2400-900-EXIT.                                                   G7E2PGM 
01001      EXIT.                                                        G7E2PGM 
01002 /***************************************************************  G7E2PGM 
01003 *                                                              *  G7E2PGM 
01004 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7E2PGM 
01005 *         PROVISION RECORD (TYPE=C4)                           *  G7E2PGM 
01006 *                                                              *  G7E2PGM 
01007 ****************************************************************  G7E2PGM 
01008  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7E2PGM 
01009  5000-010.                                                        G7E2PGM 
01010                                                                   G7E2PGM 
01011      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7E2PGM 
01012                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7E2PGM 
01013                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7E2PGM 
01014                       END-EXEC.                                   G7E2PGM 
01015                                                                   G7E2PGM 
01016                                                                   G7E2PGM 
01017  5000-900-EXIT.                                                   G7E2PGM 
01018      EXIT.                                                        G7E2PGM 
01019 /***************************************************************  G7E2PGM 
01020 *                                                              *  G7E2PGM 
01021 * 5100                                                         *  G7E2PGM 
01022 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7E2PGM 
01023 *                                                              *  G7E2PGM 
01024 ****************************************************************  G7E2PGM 
01025  5100-000-W-F-CONTRACT-IO       SECTION.                          G7E2PGM 
01026  5100-010.                                                        G7E2PGM 
01027                                                                   G7E2PGM 
01028      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7E2PGM 
01029                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7E2PGM 
01030                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7E2PGM 
01031                       END-EXEC.                                   G7E2PGM 
01032                                                                   G7E2PGM 
01033                                                                   G7E2PGM 
01034  5100-900-EXIT.                                                   G7E2PGM 
01035      EXIT.                                                        G7E2PGM 
01036 /***************************************************************  G7E2PGM 
01037 *                                                              *  G7E2PGM 
01038 * 9000   MOVE MESSAGE TO SCREEN                                *  G7E2PGM 
01039 *                                                              *  G7E2PGM 
01040 ****************************************************************  G7E2PGM 
01041  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7E2PGM 
01042  9000-010.                                                        G7E2PGM 
01043                                                                   G7E2PGM 
01044      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S2ERRO.              G7E2PGM 
01045                                                                   G7E2PGM 
01046  9000-900-EXIT.                                                   G7E2PGM 
01047      EXIT.                                                        G7E2PGM 
01048 /***************************************************************  G7E2PGM 
01049 *                                                              *  G7E2PGM 
01050 * 9100 SEND SCREEN AND RETURN                                  *  G7E2PGM 
01051 *                                                              *  G7E2PGM 
01052 ****************************************************************  G7E2PGM 
01053  9100-000-SEND-THEN-RETURN      SECTION.                          G7E2PGM 
01054  9100-010.                                                        G7E2PGM 
01055                                                                   G7E2PGM 
01056                                                                   G7E2PGM 
01057 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7E2PGM 
01058      MOVE  -1 TO  S2ERRL.                                         G7E2PGM 
01059                                                                   G7E2PGM 
01060                                                                   G7E2PGM 
01061      IF  WS-02-MY-EIBTRNID                                        G7E2PGM 
01062      THEN                                                         G7E2PGM 
01063          EXEC CICS  SEND MAP   ('G7E2I01')                        G7E2PGM 
01064                          MAPSET('G7E2SET')                        G7E2PGM 
01065                          DATAONLY                                 G7E2PGM 
01066                          CURSOR                                   G7E2PGM 
01067                          END-EXEC                                 G7E2PGM 
01068      ELSE                                                         G7E2PGM 
01069          EXEC CICS  SEND MAP   ('G7E2I01')                        G7E2PGM 
01070                          MAPSET('G7E2SET')                        G7E2PGM 
01071                          ERASE                                    G7E2PGM 
01072                          CURSOR                                   G7E2PGM 
01073                          END-EXEC.                                G7E2PGM 
01074                                                                   G7E2PGM 
01075      EXEC CICS RETURN                                             G7E2PGM 
01076                TRANSID  ('G7E2')                                  G7E2PGM 
01077                COMMAREA (DFHCOMMAREA)                             G7E2PGM 
01078                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7E2PGM 
01079                END-EXEC.                                          G7E2PGM 
01080                                                                   G7E2PGM 
01081 *                                                                 G7E2PGM 
01082  9100-900-EXIT.                                                   G7E2PGM 
01083      EXIT.                                                        G7E2PGM 
01084 /*****************************************************************G7E2PGM 
01085 *                                                                *G7E2PGM 
01086 * 9200    XCTL TO GCPSPGM                                        *G7E2PGM 
01087 *                                                                *G7E2PGM 
01088 *                                                                *G7E2PGM 
01089 ******************************************************************G7E2PGM 
01090  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7E2PGM 
01091  9200-010.                                                        G7E2PGM 
01092                                                                   G7E2PGM 
01093      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7E2PGM 
01094                       END-EXEC.                                   G7E2PGM 
01095                                                                   G7E2PGM 
01096  9200-900-EXIT.                                                   G7E2PGM 
01097      EXIT.                                                        G7E2PGM 
01098 /*****************************************************************G7E2PGM 
01099 *                                                                *G7E2PGM 
01100 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7E2PGM 
01101 *                                                                *G7E2PGM 
01102 *                                                                *G7E2PGM 
01103 ******************************************************************G7E2PGM 
01104  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7E2PGM 
01105  9210-010.                                                        G7E2PGM 
01106                                                                   G7E2PGM 
01107      IF  S2GRPNOI = '000SPS000'                                   G7E2PGM 
01108          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7E2PGM 
01109                           END-EXEC.                               G7E2PGM 
01110                                                                   G7E2PGM 
01111 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7E2PGM 
01112                                                                   G7E2PGM 
01113      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7E2PGM 
01114                                        + GC-WORKFILE-KEY-LEN      G7E2PGM 
01115                                        + GC-GCCONTR-MAX-REC-LEN.  G7E2PGM 
01116                                                                   G7E2PGM 
01117      EXEC CICS GETMAIN                                            G7E2PGM 
01118                SET (ADDRESS OF IO-PARM-CONTRACT-AREA)             G7E2PGM 
01119                INITIMG(WS-02-HEX-00)                              G7E2PGM 
01120                LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)                 G7E2PGM 
01121                END-EXEC.                                          G7E2PGM 
01122                                                                   G7E2PGM 
01123 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7E2PGM 
01124                                                                   G7E2PGM 
01125      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7E2PGM 
01126      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7E2PGM 
01127                                                                   G7E2PGM 
01128      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7E2PGM 
01129      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7E2PGM 
01130      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7E2PGM 
01131      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7E2PGM 
01132      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7E2PGM 
01133      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7E2PGM 
01134                                                                   G7E2PGM 
01135      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7E2PGM 
01136      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7E2PGM 
01137      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7E2PGM 
01138      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7E2PGM 
01139      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7E2PGM 
01140      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7E2PGM 
01141      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7E2PGM 
01142      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7E2PGM 
01143                                                                   G7E2PGM 
01144 ***  MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7E2PGM 
01145 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7E2PGM 
01146 ***  IF  HGADATE-RETURN = ZEROS                                   G7E2PGM 
01147 ***  THEN                                                         G7E2PGM 
01148 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7E2PGM 
01149 ***  ELSE                                                         G7E2PGM 
01150 ***      SET WT-01-INDEX TO +07                                   G7E2PGM 
01151 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7E2PGM 
01152 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7E2PGM 
01153                                                                   G7E2PGM 
01154      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7E2PGM 
01155      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7E2PGM 
01156      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7E2PGM 
01157      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7E2PGM 
01158      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7E2PGM 
01159      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7E2PGM 
01160      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7E2PGM 
01161                                                                   G7E2PGM 
01162 ***  PRIME COUNT FOR COBOL/2.                                     G7E2PGM 
01163      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7E2PGM 
01164        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7E2PGM 
01165                                                                   G7E2PGM 
01166      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7E2PGM 
01167                                                                   G7E2PGM 
01168      IF  NOT GCIO3-GOOD-RETURN                                    G7E2PGM 
01169          MOVE WS-01-ABCODE-E2F1     TO WS-01-ABCODE               G7E2PGM 
01170          MOVE WS-01-ABCODE-E2F1-MSG TO WS-01-ABCODE-MSG           G7E2PGM 
01171          PERFORM  9999-000-ABEND-THE-TASK.                        G7E2PGM 
01172                                                                   G7E2PGM 
01173      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7E2PGM 
01174                       COMMAREA(WORK-RECORD-3)                     G7E2PGM 
01175                       LENGTH  (GCIO3-RECORD-LENGTH)               G7E2PGM 
01176                       END-EXEC.                                   G7E2PGM 
01177                                                                   G7E2PGM 
01178  9210-900-EXIT.                                                   G7E2PGM 
01179      EXIT.                                                        G7E2PGM 
01180 /*****************************************************************G7E2PGM 
01181 *                                                                *G7E2PGM 
01182 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7E2PGM 
01183 *                                                                *G7E2PGM 
01184 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7E2PGM 
01185 *                                                                *G7E2PGM 
01186 ******************************************************************G7E2PGM 
01187  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7E2PGM 
01188  9800-010.                                                        G7E2PGM 
01189                                                                   G7E2PGM 
01190      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7E2PGM 
01191      MOVE 'M'   TO  HGADATE-FORM1.                                G7E2PGM 
01192      MOVE 'J'   TO  HGADATE-FORM2.                                G7E2PGM 
01193      MOVE ZEROS TO  HGADATE-RETURN                                G7E2PGM 
01194                     HGADATE-AMOUNT.                               G7E2PGM 
01195      EXEC CICS LINK PROGRAM ('HGADATES')                          G7E2PGM 
01196                     COMMAREA(HGADATES-COMMAREA)                   G7E2PGM 
01197                     LENGTH  (24)                                  G7E2PGM 
01198                     END-EXEC.                                     G7E2PGM 
01199                                                                   G7E2PGM 
01200  9800-900-900-EXIT.                                               G7E2PGM 
01201      EXIT.                                                        G7E2PGM 
01202 /*****************************************************************G7E2PGM 
01203 *                                                                *G7E2PGM 
01204 * 9810    J U L I A N    T O    G R E G O R I A N                *G7E2PGM 
01205 *                                                                *G7E2PGM 
01206 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7E2PGM 
01207 *                                                                *G7E2PGM 
01208 ******************************************************************G7E2PGM 
01209  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7E2PGM 
01210  9810-010.                                                        G7E2PGM 
01211                                                                   G7E2PGM 
01212      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7E2PGM 
01213      MOVE 'J'   TO  HGADATE-FORM1.                                G7E2PGM 
01214      MOVE 'M'   TO  HGADATE-FORM2.                                G7E2PGM 
01215      MOVE ZEROS TO  HGADATE-RETURN                                G7E2PGM 
01216                     HGADATE-AMOUNT.                               G7E2PGM 
01217      EXEC CICS LINK PROGRAM ('HGADATES')                          G7E2PGM 
01218                     COMMAREA(HGADATES-COMMAREA)                   G7E2PGM 
01219                     LENGTH  (24)                                  G7E2PGM 
01220                     END-EXEC.                                     G7E2PGM 
01221                                                                   G7E2PGM 
01222  9810-900-900-EXIT.                                               G7E2PGM 
01223      EXIT.                                                        G7E2PGM 
01224 /***************************************************************  G7E2PGM 
01225 *                                                              *  G7E2PGM 
01226 * 9999  ABEND THE TASK                                         *  G7E2PGM 
01227 *                                                              *  G7E2PGM 
01228 ****************************************************************  G7E2PGM 
01229  9999-000-ABEND-THE-TASK SECTION.                                 G7E2PGM 
01230  9999-010.                                                        G7E2PGM 
01231                                                                   G7E2PGM 
01232      EXEC CICS  ABEND                                             G7E2PGM 
01233                 ABCODE(WS-01-ABCODE)                              G7E2PGM 
01234                 END-EXEC.                                         G7E2PGM 
01235                                                                   G7E2PGM 
01236  9900-900-EXIT.                                                   G7E2PGM 
01237      EXIT.                                                        G7E2PGM 
