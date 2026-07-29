00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7C1PGM.                                         G7C1PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7C1PGM 
00005  DATE-WRITTEN.   03/10/87.                                        G7C1PGM 
00006  DATE-COMPILED.                                                   G7C1PGM 
00007 ***************************************************************** G7C1PGM 
00008 *                                                               * G7C1PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7C1PGM 
00010 *                                                               * G7C1PGM 
00011 *                                                               * G7C1PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7C1PGM 
00013 *                                                               * G7C1PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7C1PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7C1PGM 
00016 *                             (SAME AS 'GC5A')                  * G7C1PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7C1PGM 
00018 *                              'GC5A')                          * G7C1PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7C1PGM 
00020 *                              PROVISION)                       * G7C1PGM 
00021 *                                                               * G7C1PGM 
00022 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7C1PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7C1PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7C1PGM 
00025 *                                                               * G7C1PGM 
00026 *  M216       1/07/88  FCG    ADDED LOGIC FOR EXCEPTION SCHEDULE* G7C1PGM 
00027 *                             ID TO USE FIELD VALIDATION SUB-   * G7C1PGM 
00028 *                             SYSTEM.                           * G7C1PGM 
00029 *                                                               * G7C1PGM 
00030 *  D129      08/24/89  GDM    CONVERT FOR DECIMALS              * G7C1PGM 
00031 *                                                               * G7C1PGM 
00032 *  D129      09/08/89  GDM    CONVERT TO VS COBOL/2             * G7C1PGM 
00033 *                                                                 G7C1PGM 
00034 *  D12009    08/23/91  BSO   -CORRECT ERR MESSAGE LITERALS IN   * G7C1PGM 
00035 *                             AREA \
00036 *                            -CORRECT ALPHA CLASS TEST AREA       G7C1PGM 
00037 *                                                                 G7C1PGM 
00038 *  XXXXX   09/09/91  ENW  -CORRECTED EDIT FOR BENEFIT-SCOPE-ID    G7C1PGM 
00039 *                                                                 G7C1PGM 
00040 * 14726/   03/26/98  GSP     ADDED PLAN AND PACKAGE CODE AND    * G7C1PGM 
00041 * 15057                      INCREASED GROUP AND SECTION ON     * G7C1PGM 
00042 *                            THE SCREEN.                        * G7C1PGM 
00043 *                                                                 G7C1PGM 
00044 *          12/11/02  AKK     OPID COMPILE                       * G7C1PGM 
00045 *                                                                 G7C1PGM 
00046 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7C1PGM 
00047 *                           G7C1SET                              *G7C1PGM 
00048 ***************************************************************** G7C1PGM 
00049                                                                   G7C1PGM 
00050 ***************************************************************** G7C1PGM 
00051 *                                                               * G7C1PGM 
00052 *    G7C1PGM  - PROGRAM 1 OF 3 PROGRAMS TO UPDATE THE FORMAT 'C'* G7C1PGM 
00053 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7C1PGM 
00054 *                                                               * G7C1PGM 
00055 *    TRANSID: G7C1                                              * G7C1PGM 
00056 *    MAPSET:  G7C1SETC    (GIC1PGM WHICH SHARES THIS MAP)       * G7C1PGM 
00057 *    VALGEN:  NONE                                              * G7C1PGM 
00058 *                                                               * G7C1PGM 
00059 *    PROGRAM NARRATIVE:                                         * G7C1PGM 
00060 *                                                               * G7C1PGM 
00061 *        PROGRAM CHECKS FOR TRANS CODE 'G7C1'.  AN INVALID      * G7C1PGM 
00062 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7C1PGM 
00063 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7C1PGM 
00064 *                                                               * G7C1PGM 
00065 *        THE MAIN FUNCTIONS ARE :                               * G7C1PGM 
00066 *        1. HARDCOPY REQUEST,                                   * G7C1PGM 
00067 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7C1PGM 
00068 *           USER,                                               * G7C1PGM 
00069 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7C1PGM 
00070 *                                                               * G7C1PGM 
00071 *        HARDCOPY REQUEST                                       * G7C1PGM 
00072 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7C1PGM 
00073 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7C1PGM 
00074 *           THE SCREEN BUFFER.                                  * G7C1PGM 
00075 *                                                               * G7C1PGM 
00076 *        PROCESS INPUT DATA (UPDATE).                           * G7C1PGM 
00077 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7C1PGM 
00078 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7C1PGM 
00079 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7C1PGM 
00080 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7C1PGM 
00081 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7C1PGM 
00082 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7C1PGM 
00083 *                                                               * G7C1PGM 
00084 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7C1PGM 
00085 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7C1PGM 
00086 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7C1PGM 
00087 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7C1PGM 
00088 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7C1PGM 
00089 *                                                               * G7C1PGM 
00090 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7C1PGM 
00091 *                                     -----------------------   * G7C1PGM 
00092 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7C1PGM 
00093 *           -----------------------------------------------     * G7C1PGM 
00094 *                                                               * G7C1PGM 
00095 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7C1PGM 
00096 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7C1PGM 
00097 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7C1PGM 
00098 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7C1PGM 
00099 *           SENT BACK TO THE USER.                              * G7C1PGM 
00100 *                                                               * G7C1PGM 
00101 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7C1PGM 
00102 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7C1PGM 
00103 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7C1PGM 
00104 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7C1PGM 
00105 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7C1PGM 
00106 *           PF KEY LOGIC,                                       * G7C1PGM 
00107 *              PF6/PF18       GOES TO GC8APGM                   * G7C1PGM 
00108 *              PF8/PF20/ENTER GOES TO G7C2PGM                   * G7C1PGM 
00109 *              FOR PF7/PF19   GOES TO GC6CPGM                   * G7C1PGM 
00110 *                                                               * G7C1PGM 
00111 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7C1PGM 
00112 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7C1PGM 
00113 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7C1PGM 
00114 *                                                               * G7C1PGM 
00115 ***************************************************************** G7C1PGM 
00116                                                                   G7C1PGM 
00117  ENVIRONMENT DIVISION.                                            G7C1PGM 
00118  DATA DIVISION.                                                   G7C1PGM 
00119 /                                                                 G7C1PGM 
00120  WORKING-STORAGE SECTION.                                         G7C1PGM 
00121  01  WS-BEGIN                    PIC X(58) VALUE                  G7C1PGM 
00122      '*** G7C1PGM  WORKING-STORAGE BEGINS HERE ***'.              G7C1PGM 
00123                                                                   G7C1PGM 
00124                                                                   G7C1PGM 
00125  01  WS-01-ABEND-AREA.                                            G7C1PGM 
00126      05  FILLER                   PIC X(16)  VALUE                G7C1PGM 
00127          '** ABEND AREA **'.                                      G7C1PGM 
00128                                                                   G7C1PGM 
00129      05  WS-01-ABEND-CODES-AND-MSG.                               G7C1PGM 
00130          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7C1PGM 
00131          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7C1PGM 
00132                                                                   G7C1PGM 
00133          10  WS-01-ABCODE-C1F1          PIC X(04)  VALUE  'C1F1'. G7C1PGM 
00134          10  WS-01-ABCODE-C1F1-MSG      PIC X(44)  VALUE          G7C1PGM 
00135             'W/F CONTRACT CANNOT BE FOUND             '.          G7C1PGM 
00136                                                                   G7C1PGM 
00137          10  WS-01-ABCODE-C1F2          PIC X(04)  VALUE  'C1F2'. G7C1PGM 
00138          10  WS-01-ABCODE-C1F2-MSG      PIC X(44)  VALUE          G7C1PGM 
00139             'W/F BEN PROV CANNOT BE FOUND             '.          G7C1PGM 
00140                                                                   G7C1PGM 
00141          10  WS-01-ABCODE-C1F3          PIC X(04)  VALUE  'C1F3'. G7C1PGM 
00142          10  WS-01-ABCODE-C1F3-MSG      PIC X(44)  VALUE          G7C1PGM 
00143             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7C1PGM 
00144                                                                   G7C1PGM 
00145          10  WS-01-ABCODE-C1F4          PIC X(04)  VALUE  'C1F4'. G7C1PGM 
00146          10  WS-01-ABCODE-C1F4-MSG      PIC X(44)  VALUE          G7C1PGM 
00147             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7C1PGM 
00148                                                                   G7C1PGM 
00149          10  WS-01-ABCODE-C1L1          PIC X(04)  VALUE  'C1L1'. G7C1PGM 
00150          10  WS-01-ABCODE-C1L1-MSG      PIC X(44)  VALUE          G7C1PGM 
00151             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7C1PGM 
00152                                                                   G7C1PGM 
00153          10  WS-01-ABCODE-C1P1          PIC X(04)  VALUE  'C1P1'. G7C1PGM 
00154          10  WS-01-ABCODE-C1P1-MSG      PIC X(44)  VALUE          G7C1PGM 
00155             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7C1PGM 
00156                                                                   G7C1PGM 
00157          10  WS-01-ABCODE-C1P2          PIC X(04)  VALUE  'C1P2'. G7C1PGM 
00158          10  WS-01-ABCODE-C1P2-MSG      PIC X(44)  VALUE          G7C1PGM 
00159             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7C1PGM 
00160                                                                   G7C1PGM 
00161  01  WS-02-AREA.                                                  G7C1PGM 
00162      05  FILLER                   PIC X(16)  VALUE                G7C1PGM 
00163          '** WS-02-AREA **'.                                      G7C1PGM 
00164      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7C1PGM 
00165          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7C1PGM 
00166                                                    'GC6C' 'G7C2'  G7C1PGM 
00167                                                    'G7C1'.        G7C1PGM 
00168          88  WS-02-MY-EIBTRNID                     VALUE  'G7C1'. G7C1PGM 
00169                                                                   G7C1PGM 
00170      05  WS-02-COMPUTED-LENGTHS.                                  G7C1PGM 
00171          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7C1PGM 
00172          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7C1PGM 
00173          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7C1PGM 
00174                                                                   G7C1PGM 
00175      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7C1PGM 
00176                                                                   G7C1PGM 
00177      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7C1PGM 
00178                                                                   G7C1PGM 
00179      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7C1PGM 
00180      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7C1PGM 
00181          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7C1PGM 
00182                                         PIC X.                    G7C1PGM 
00183          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7C1PGM 
00184                                                    '0' THRU '9'   G7C1PGM 
00185                                                    'A' THRU 'I'   G7C1PGM 
00186                                                    'J' THRU 'R'   G7C1PGM 
00187                                                    'S' THRU 'Z'   G7C1PGM 
00188                                                    SPACE.         G7C1PGM 
00189          88  WS-02-CLASS-BLANK                     VALUE          G7C1PGM 
00190                                                    SPACE.         G7C1PGM 
00191                                                                   G7C1PGM 
00192      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7C1PGM 
00193          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7C1PGM 
00194          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7C1PGM 
00195                                                                   G7C1PGM 
00196      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7C1PGM 
00197          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7C1PGM 
00198                                                                   G7C1PGM 
00199      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7C1PGM 
00200                                                                   G7C1PGM 
00201      05  WS-02-HEX-F00000.                                        G7C1PGM 
00202          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7C1PGM 
00203          10  FILLER                     PIC  X(09) VALUE          G7C1PGM 
00204                                                    LOW-VALUES.    G7C1PGM 
00205                                                                   G7C1PGM 
00206      05  WS-02-MIN-ELIG-AMT-X.                                    G7C1PGM 
00207          10  WS-02-MIN-ELIG-AMT           PIC 999V99  VALUE ZEROS.G7C1PGM 
00208 *        10  WS-02-S1MNAMT              REDEFINES                 G7C1PGM 
00209 *            WS-02-MIN-ELIG-AMT           PIC X(5).               G7C1PGM 
00210                                                                   G7C1PGM 
00211      05  WS-5POS-MAX-AMT                PIC 999V99 VALUE 999.99.  G7C1PGM 
00212      05  WS-02-DISP-5POS-DEC            PIC 9(3).99.              G7C1PGM 
00213      05  WS-GPC2-MIN-ELIG-AMT           PIC 9(3)V99.              G7C1PGM 
00214 /                                                                 G7C1PGM 
00215  01  WT-00-G7C1PGM-TABLES.                                        G7C1PGM 
00216      05  FILLER                   PIC X(16)  VALUE                G7C1PGM 
00217          '*G7C1PGM TABLES*'.                                      G7C1PGM 
00218                                                                   G7C1PGM 
00219  01  WT-01-TABLE.                                                 G7C1PGM 
00220      05  FILLER                  PIC X(16) VALUE                  G7C1PGM 
00221          '* WT-01-TABLE  *'.                                      G7C1PGM 
00222 ******************************************************************G7C1PGM 
00223 *    WT-01   MESSAGE TABLE                                       *G7C1PGM 
00224 ******************************************************************G7C1PGM 
00225  01  FILLER.                                                      G7C1PGM 
00226      05  WT-01-MESSAGE-VALUES.                                    G7C1PGM 
00227                                                                   G7C1PGM 
00228 *----------------------------------------------------------------*G7C1PGM 
00229          10  WT-01-ENTRY-001.                                     G7C1PGM 
00230              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00231              15  WT-01-MESSAGE-TEXT-001.                          G7C1PGM 
00232                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00233                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00234                  20  FILLER          PIC X(3)  VALUE  '001'.      G7C1PGM 
00235                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00236                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00237                      ' INVALID PFKEY SELECTION                    G7C1PGM 
00238 -                    '                         '.                 G7C1PGM 
00239              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00240                                                                   G7C1PGM 
00241 *----------------------------------------------------------------*G7C1PGM 
00242          10  WT-01-ENTRY-002.                                     G7C1PGM 
00243              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00244              15  WT-01-MESSAGE-TEXT-002.                          G7C1PGM 
00245                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00246                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00247                  20  FILLER          PIC X(3)  VALUE  '002'.      G7C1PGM 
00248                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00249                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00250                      'INVALID DECIMAL DETECTED'.                  G7C1PGM 
00251              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00252                                                                   G7C1PGM 
00253 *----------------------------------------------------------------*G7C1PGM 
00254          10  WT-01-ENTRY-003.                                     G7C1PGM 
00255              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00256              15  WT-01-MESSAGE-TEXT-003.                          G7C1PGM 
00257                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00258                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00259                  20  FILLER          PIC X(3)  VALUE  '003'.      G7C1PGM 
00260                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00261                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00262                      'FIELD EXCEEDS LENGTH OF 5 POSITIONS. FORMAT G7C1PGM 
00263 -                    'IS 999.99'.                                 G7C1PGM 
00264              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00265                                                                   G7C1PGM 
00266 *----------------------------------------------------------------*G7C1PGM 
00267          10  WT-01-ENTRY-004.                                     G7C1PGM 
00268              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00269              15  WT-01-MESSAGE-TEXT-004.                          G7C1PGM 
00270                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00271                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00272                  20  FILLER          PIC X(3)  VALUE  '004'.      G7C1PGM 
00273                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00274                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00275                      'FILL THE FIRST TWO POSITIONS AND SPACE OUT TG7C1PGM 
00276 -                    'HE SECOND TWO.           '.                 G7C1PGM 
00277              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00278                                                                   G7C1PGM 
00279 *----------------------------------------------------------------*G7C1PGM 
00280          10  WT-01-ENTRY-005.                                     G7C1PGM 
00281              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00282              15  WT-01-MESSAGE-TEXT-005.                          G7C1PGM 
00283                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00284                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00285                  20  FILLER          PIC X(3)  VALUE  '005'.      G7C1PGM 
00286                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00287                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00288                      '********** F U T U R E   U S E *************G7C1PGM 
00289 -                    '*************************'.                 G7C1PGM 
00290              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00291                                                                   G7C1PGM 
00292 *----------------------------------------------------------------*G7C1PGM 
00293          10  WT-01-ENTRY-006.                                     G7C1PGM 
00294              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00295              15  WT-01-MESSAGE-TEXT-006.                          G7C1PGM 
00296                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00297                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00298                  20  FILLER          PIC X(3)  VALUE  '006'.      G7C1PGM 
00299                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00300                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00301                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7C1PGM 
00302 -                    ' PF4/PF16 TO CONTINUE    '.                 G7C1PGM 
00303              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00304                                                                   G7C1PGM 
00305 *----------------------------------------------------------------*G7C1PGM 
00306          10  WT-01-ENTRY-007.                                     G7C1PGM 
00307              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00308              15  WT-01-MESSAGE-TEXT-007.                          G7C1PGM 
00309                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00310                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00311                  20  FILLER          PIC X(3)  VALUE  '007'.      G7C1PGM 
00312                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00313                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00314                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7C1PGM 
00315 -                    'E CALL SYSTEMS           '.                 G7C1PGM 
00316              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00317                                                                   G7C1PGM 
00318 *----------------------------------------------------------------*G7C1PGM 
00319          10  WT-01-ENTRY-008.                                     G7C1PGM 
00320              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00321              15  WT-01-MESSAGE-TEXT-008.                          G7C1PGM 
00322                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00323                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00324                  20  FILLER          PIC X(3)  VALUE  '008'.      G7C1PGM 
00325                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00326                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00327                      'FIELD HAS AN INVALID VALUE                  G7C1PGM 
00328 -                    '                         '.                 G7C1PGM 
00329              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00330                                                                   G7C1PGM 
00331 *----------------------------------------------------------------*G7C1PGM 
00332          10  WT-01-ENTRY-009.                                     G7C1PGM 
00333              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00334              15  WT-01-MESSAGE-TEXT-009.                          G7C1PGM 
00335                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00336                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00337                  20  FILLER          PIC X(3)  VALUE  '009'.      G7C1PGM 
00338                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00339                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00340                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7C1PGM 
00341 -                    'YSTEM)                   '.                 G7C1PGM 
00342              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00343                                                                   G7C1PGM 
00344 *----------------------------------------------------------------*G7C1PGM 
00345          10  WT-01-ENTRY-010.                                     G7C1PGM 
00346              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C1PGM 
00347              15  WT-01-MESSAGE-TEXT-010.                          G7C1PGM 
00348                  20  FILLER          PIC X(4)  VALUE  'G7C1'.     G7C1PGM 
00349                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C1PGM 
00350                  20  FILLER          PIC X(3)  VALUE  '010'.      G7C1PGM 
00351                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C1PGM 
00352                  20  FILLER          PIC X(70) VALUE              G7C1PGM 
00353                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7C1PGM 
00354 -                    '                         '.                 G7C1PGM 
00355              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C1PGM 
00356                                                                   G7C1PGM 
00357 *----------------------------------------------------------------*G7C1PGM 
00358                                                                   G7C1PGM 
00359      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7C1PGM 
00360          WT-01-MESSAGE-VALUES         OCCURS 010 TIMES            G7C1PGM 
00361                                      INDEXED BY WT-01-INDEX.      G7C1PGM 
00362          10  WT-01-ENTRY.                                         G7C1PGM 
00363              15  FILLER              PIC X(02).                   G7C1PGM 
00364              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7C1PGM 
00365              15  FILLER              PIC X(02).                   G7C1PGM 
00366                                                                   G7C1PGM 
00367                                                                   G7C1PGM 
00368 /*** MAP FIELD ATTRIBUTES                                         G7C1PGM 
00369  COPY DFHBMSCA.                                                   G7C1PGM 
00370 *                         AUTOSKIP, BRIGHT, FSET                  G7C1PGM 
00371      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7C1PGM 
00372                                                                   G7C1PGM 
00373 /*** ATTENTION KEYS                                               G7C1PGM 
00374  COPY DFHAID.                                                     G7C1PGM 
00375                                                                   G7C1PGM 
00376 /***  PROVISION MAINTENANCE SCREEN                                G7C1PGM 
00377  COPY  G7C1SETC.                                                  G7C1PGM 
00378                                                                   G7C1PGM 
00379 /*** DATE ROUTINE COMMAREA                                        G7C1PGM 
00380  01  HGADATES-COMMAREA.                                           G7C1PGM 
00381  COPY HGCDAT01.                                                   G7C1PGM 
00382                                                                   G7C1PGM 
00383 /*** DECIMAL CONVERSION COMMAREA                                  G7C1PGM 
00384  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7C1PGM 
00385  COPY GCDCCA01.                                                   G7C1PGM 
00386                                                                   G7C1PGM 
00387 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7C1PGM 
00388  01  GCVIOPGM-PARM-LIST.                                          G7C1PGM 
00389  COPY GCVINTRC.                                                   G7C1PGM 
00390                                                                   G7C1PGM 
00391 /*** ALTERNATIVE WORKFILE KEYS                                    G7C1PGM 
00392  01  FILLER.                                                      G7C1PGM 
00393      COPY GCWRKKEY.                                               G7C1PGM 
00394                                                                   G7C1PGM 
00395 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7C1PGM 
00396  01  FILLER.                                                      G7C1PGM 
00397      COPY GCCDRLEN.                                               G7C1PGM 
00398                                                                   G7C1PGM 
00399                                                                   G7C1PGM 
00400  01  WS-END                       PIC X(58) VALUE                 G7C1PGM 
00401      '*** G7C1PGM  WORKING-STORAGE ENDS HERE ***'.                G7C1PGM 
00402 /                                                                 G7C1PGM 
00403  LINKAGE SECTION.                                                 G7C1PGM 
00404 /                                                                 G7C1PGM 
00405  01  DFHCOMMAREA.                                                 G7C1PGM 
00406      COPY  GCWRKDCC.                                              G7C1PGM 
00407      COPY  GCBENPVC.                                              G7C1PGM 
00408 /                                                                 G7C1PGM 
00409 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7C1PGM 
00410  01  IO-PARM-BEN-PROV-AREA.                                       G7C1PGM 
00411      COPY  GCIOPRM2.                                              G7C1PGM 
00412      COPY  GCWRKDC2.                                              G7C1PGM 
00413      COPY  GCBENPV2.                                              G7C1PGM 
00414                                                                   G7C1PGM 
00415 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7C1PGM 
00416  01  IO-PARM-CONTRACT-AREA.                                       G7C1PGM 
00417      COPY  GCIOPRM3.                                              G7C1PGM 
00418      COPY  GCWRKDC3.                                              G7C1PGM 
00419      COPY  GCCONTR2.                                              G7C1PGM 
00420 /                                                                 G7C1PGM 
00421  PROCEDURE DIVISION.                                              G7C1PGM 
00422                                                                   G7C1PGM 
00423 ****************************************************************  G7C1PGM 
00424 *                                                              *  G7C1PGM 
00425 *           P R O C E S S     C O N T R O L                    *  G7C1PGM 
00426 *                                                              *  G7C1PGM 
00427 ****************************************************************  G7C1PGM 
00428  0000-000-PROCESS-CONTROL       SECTION.                          G7C1PGM 
00429  0000-010.                                                        G7C1PGM 
00430                                                                   G7C1PGM 
00431      IF  EIBAID  =  DFHCLEAR                                      G7C1PGM 
00432          EXEC CICS  RETURN                                        G7C1PGM 
00433                     END-EXEC.                                     G7C1PGM 
00434                                                                   G7C1PGM 
00435      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7C1PGM 
00436                                                                   G7C1PGM 
00437      IF  WS-02-MY-EIBTRNID                                        G7C1PGM 
00438      THEN                                                         G7C1PGM 
00439          PERFORM  2000-000-PROCESS-INPUT                          G7C1PGM 
00440      ELSE                                                         G7C1PGM 
00441          PERFORM  1000-000-DISPLAY-SCREEN.                        G7C1PGM 
00442                                                                   G7C1PGM 
00443                                                                   G7C1PGM 
00444 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7C1PGM 
00445                                                                   G7C1PGM 
00446      MOVE WS-01-ABCODE-C1L1     TO WS-01-ABCODE                   G7C1PGM 
00447      MOVE WS-01-ABCODE-C1L1-MSG TO WS-01-ABCODE-MSG               G7C1PGM 
00448      PERFORM  9999-000-ABEND-THE-TASK.                            G7C1PGM 
00449                                                                   G7C1PGM 
00450      GOBACK.                                                      G7C1PGM 
00451                                                                   G7C1PGM 
00452                                                                   G7C1PGM 
00453  0000-900-EXIT.                                                   G7C1PGM 
00454      EXIT.                                                        G7C1PGM 
00455 /***************************************************************  G7C1PGM 
00456 *                                                              *  G7C1PGM 
00457 * 1000  DISPLAY INITIAL SCREEN                                 *  G7C1PGM 
00458 *                                                              *  G7C1PGM 
00459 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7C1PGM 
00460 *                                                              *  G7C1PGM 
00461 ****************************************************************  G7C1PGM 
00462  1000-000-DISPLAY-SCREEN        SECTION.                          G7C1PGM 
00463  1000-010.                                                        G7C1PGM 
00464                                                                   G7C1PGM 
00465 *------- SET SCREEN TO LOW VALUES FOR FIRST SEND                  G7C1PGM 
00466 *                                                                 G7C1PGM 
00467      MOVE LOW-VALUES TO G7C1I01I.                                 G7C1PGM 
00468                                                                   G7C1PGM 
00469 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7C1PGM 
00470                                                                   G7C1PGM 
00471      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7C1PGM 
00472          MOVE WS-01-ABCODE-C1P1     TO WS-01-ABCODE               G7C1PGM 
00473          MOVE WS-01-ABCODE-C1P1-MSG TO WS-01-ABCODE-MSG           G7C1PGM 
00474          PERFORM 9999-000-ABEND-THE-TASK.                         G7C1PGM 
00475                                                                   G7C1PGM 
00476                                                                   G7C1PGM 
00477 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7C1PGM 
00478                                                                   G7C1PGM 
00479      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7C1PGM 
00480                                         + GC-GCBENPRV-FIXED-LEN   G7C1PGM 
00481                                         + GC-GCBENPRV-VARY-LEN.   G7C1PGM 
00482                                                                   G7C1PGM 
00483                                                                   G7C1PGM 
00484 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7C1PGM 
00485                                                                   G7C1PGM 
00486      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7C1PGM 
00487          MOVE WS-01-ABCODE-C1P2     TO WS-01-ABCODE               G7C1PGM 
00488          MOVE WS-01-ABCODE-C1P2-MSG TO WS-01-ABCODE-MSG           G7C1PGM 
00489          PERFORM 9999-000-ABEND-THE-TASK.                         G7C1PGM 
00490                                                                   G7C1PGM 
00491                                                                   G7C1PGM 
00492 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7C1PGM 
00493 *          BY CALLER IN COMMAREA.                                 G7C1PGM 
00494                                                                   G7C1PGM 
00495      MOVE WRK-PLAN-CODE                      TO S1PLNCDO.         G7C1PGM 
00496      MOVE WRK-GROUP-NUM                      TO S1GRPNOO.         G7C1PGM 
00497      MOVE WRK-SECTION-NUM                    TO S1SECNOO.         G7C1PGM 
00498      MOVE WRK-PKG-CODE                       TO S1PKGCDO.         G7C1PGM 
00499      MOVE WRK-PROV-CTL                       TO S1PRVO.           G7C1PGM 
00500      MOVE WRK-FAM-REL-LEVEL                  TO S1FRLO.           G7C1PGM 
00501      MOVE WRK-L-O-B                          TO S1LOBO.           G7C1PGM 
00502                                                                   G7C1PGM 
00503      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7C1PGM 
00504      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7C1PGM 
00505      IF  HGADATE-RETURN = ZEROS                                   G7C1PGM 
00506      THEN                                                         G7C1PGM 
00507          MOVE DFHBMASF                       TO S1EFFDTA          G7C1PGM 
00508          MOVE HGADATE-DATE2                  TO S1EFFDTO          G7C1PGM 
00509      ELSE                                                         G7C1PGM 
00510          MOVE DFHBMABF                       TO S1EFFDTA          G7C1PGM 
00511          MOVE HGADATE-JULIAN1                TO S1EFFDTO.         G7C1PGM 
00512                                                                   G7C1PGM 
00513      MOVE GCP-PROVN-ID                       TO S1BPVIDO.         G7C1PGM 
00514                                                                   G7C1PGM 
00515      MOVE GPC-BEN-SCOPE-ID                   TO S1BESCIO.         G7C1PGM 
00516      MOVE GPC-EXCP-SCHED-ID                  TO S1EXCSCO.         G7C1PGM 
00517      MOVE GPC-ELIG-METHD-OF-TREAT-IND        TO S1ELMTIO.         G7C1PGM 
00518      MOVE GPC-MULT-REL-PROC-IND              TO S1MRPIO.          G7C1PGM 
00519      MOVE GPC-PRIM-SURG-DEPEND-IND           TO S1PSUDIO.         G7C1PGM 
00520      MOVE GPC-HOSP-STAFF-PROV-IND            TO S1HSTFPO.         G7C1PGM 
00521      MOVE GPC-REPEAT-PROC-IND                TO S1RPTPRO.         G7C1PGM 
00522                                                                   G7C1PGM 
00523 *    IF  GPC-MIN-ELIG-AMT = ZEROS                                 G7C1PGM 
00524 *        MOVE WS-02-HEX-F00000               TO S1MNAMTO          G7C1PGM 
00525 *    ELSE                                                         G7C1PGM 
00526 *        MOVE GPC-MIN-ELIG-AMT               TO WS-02-MIN-ELIG-AMTG7C1PGM 
00527 *        MOVE WS-02-MIN-ELIG-AMT-X           TO S1MNAMTO.         G7C1PGM 
00528                                                                   G7C1PGM 
00529 * D129     DECIMAL CONVERSION.                                    G7C1PGM 
00530 *                                                                 G7C1PGM 
00531          MOVE GPC-MIN-ELIG-AMT               TO                   G7C1PGM 
00532             WS-02-MIN-ELIG-AMT.                                   G7C1PGM 
00533          MOVE WS-02-MIN-ELIG-AMT             TO                   G7C1PGM 
00534               WS-02-DISP-5POS-DEC.                                G7C1PGM 
00535          MOVE WS-02-DISP-5POS-DEC            TO S1MNAMTO.         G7C1PGM 
00536                                                                   G7C1PGM 
00537      MOVE GPC-CORRIDOR-OVERRIDE              TO S1CORORO.         G7C1PGM 
00538      MOVE GPC-SURG-MULT-PROC-PRICE-IND       TO S1SMPPIO.         G7C1PGM 
00539      MOVE GPC-MULT-INJ-PRICING-MOD-IND       TO S1MIJPMO.         G7C1PGM 
00540                                                                   G7C1PGM 
00541                                                                   G7C1PGM 
00542                                                                   G7C1PGM 
00543 *------- SEND INITIAL SCREEN ------------------------------------*G7C1PGM 
00544                                                                   G7C1PGM 
00545      MOVE  -1 TO  S1BESCIL.                                       G7C1PGM 
00546      PERFORM 9100-000-SEND-THEN-RETURN.                           G7C1PGM 
00547                                                                   G7C1PGM 
00548                                                                   G7C1PGM 
00549  1000-900-EXIT.                                                   G7C1PGM 
00550      EXIT.                                                        G7C1PGM 
00551 /***************************************************************  G7C1PGM 
00552 *                                                              *  G7C1PGM 
00553 * 2000    P R O C E S S    I N P U T                           *  G7C1PGM 
00554 *                                                              *  G7C1PGM 
00555 ****************************************************************  G7C1PGM 
00556  2000-000-PROCESS-INPUT         SECTION.                          G7C1PGM 
00557  2000-010.                                                        G7C1PGM 
00558                                                                   G7C1PGM 
00559 *------ VALIDATE PFKEY USAGE ------------------------------------*G7C1PGM 
00560                                                                   G7C1PGM 
00561      IF  EIBAID = DFHENTER OR                                     G7C1PGM 
00562                   DFHPF3   OR  DFHPF15 OR                         G7C1PGM 
00563                   DFHPF4   OR  DFHPF16 OR                         G7C1PGM 
00564                   DFHPF6   OR  DFHPF18 OR                         G7C1PGM 
00565                   DFHPF7   OR  DFHPF19 OR                         G7C1PGM 
00566                   DFHPF8   OR  DFHPF20                            G7C1PGM 
00567      THEN                                                         G7C1PGM 
00568          NEXT SENTENCE                                            G7C1PGM 
00569      ELSE                                                         G7C1PGM 
00570          SET WT-01-INDEX TO +01                                   G7C1PGM 
00571          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7C1PGM 
00572          PERFORM 9100-000-SEND-THEN-RETURN.                       G7C1PGM 
00573                                                                   G7C1PGM 
00574                                                                   G7C1PGM 
00575                                                                   G7C1PGM 
00576      EXEC CICS  HANDLE CONDITION                                  G7C1PGM 
00577                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7C1PGM 
00578                        END-EXEC.                                  G7C1PGM 
00579                                                                   G7C1PGM 
00580                                                                   G7C1PGM 
00581      EXEC CICS  RECEIVE MAP   ('G7C1I01')                         G7C1PGM 
00582                         MAPSET('G7C1SET')                         G7C1PGM 
00583                         END-EXEC.                                 G7C1PGM 
00584                                                                   G7C1PGM 
00585                                                                   G7C1PGM 
00586      IF  S1FUNCI  NOT = 'G7C1'  OR                                G7C1PGM 
00587          S1SCRNI  NOT = '007C01'                                  G7C1PGM 
00588          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7C1PGM 
00589                                                                   G7C1PGM 
00590                                                                   G7C1PGM 
00591 *--- RETURN TO GCPS MENU? ---------------------------------------*G7C1PGM 
00592                                                                   G7C1PGM 
00593      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7C1PGM 
00594          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7C1PGM 
00595                                                                   G7C1PGM 
00596 *--- PROCESS SCREEN FIELDS --------------------------------------*G7C1PGM 
00597                                                                   G7C1PGM 
00598      PERFORM 2100-000-FIELD-EDITS.                                G7C1PGM 
00599                                                                   G7C1PGM 
00600      IF  WS-02-SCREEN-HAS-ERRORS                                  G7C1PGM 
00601          PERFORM 9100-000-SEND-THEN-RETURN.                       G7C1PGM 
00602                                                                   G7C1PGM 
00603      PERFORM 2200-000-LOGICAL-EDITS.                              G7C1PGM 
00604                                                                   G7C1PGM 
00605      IF  WS-02-SCREEN-HAS-ERRORS                                  G7C1PGM 
00606          PERFORM 9100-000-SEND-THEN-RETURN.                       G7C1PGM 
00607                                                                   G7C1PGM 
00608      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7C1PGM 
00609                                                                   G7C1PGM 
00610      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7C1PGM 
00611                                                                   G7C1PGM 
00612                                                                   G7C1PGM 
00613  2000-900-EXIT.                                                   G7C1PGM 
00614      EXIT.                                                        G7C1PGM 
00615 /***************************************************************  G7C1PGM 
00616 *                                                              *  G7C1PGM 
00617 * 2100  DO SCREEN FIELD EDITS                                  *  G7C1PGM 
00618 *                                                              *  G7C1PGM 
00619 ****************************************************************  G7C1PGM 
00620  2100-000-FIELD-EDITS           SECTION.                          G7C1PGM 
00621  2100-010.                                                        G7C1PGM 
00622                                                                   G7C1PGM 
00623 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7C1PGM 
00624                                                                   G7C1PGM 
00625      MOVE DFHBMUNF TO  S1BESCIA                                   G7C1PGM 
00626                        S1EXCSCA                                   G7C1PGM 
00627                        S1ELMTIA                                   G7C1PGM 
00628                        S1MRPIA                                    G7C1PGM 
00629                        S1PSUDIA                                   G7C1PGM 
00630                        S1HSTFPA                                   G7C1PGM 
00631                        S1RPTPRA                                   G7C1PGM 
00632                        S1MNAMTA                                   G7C1PGM 
00633                        S1CORORA                                   G7C1PGM 
00634                        S1SMPPIA                                   G7C1PGM 
00635                        S1MIJPMA.                                  G7C1PGM 
00636                                                                   G7C1PGM 
00637      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7C1PGM 
00638                                                                   G7C1PGM 
00639                                                                   G7C1PGM 
00640 *-- VALIDATE ------ BENEFIT SCOPE ID ----------------------------*G7C1PGM 
00641 *   1. ALPHANUMERIC                                               G7C1PGM 
00642 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00643                                                                   G7C1PGM 
00644      MOVE  S1BESCII TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00645      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7C1PGM 
00646          WS-02-CLASS-ALPHANUMERIC(2) AND                          G7C1PGM 
00647          WS-02-CLASS-BLANK       (3) AND                          G7C1PGM 
00648          WS-02-CLASS-BLANK       (4)                              G7C1PGM 
00649      THEN                                                         G7C1PGM 
00650          MOVE  S1BESCII TO GCVI-VALUE                             G7C1PGM 
00651          MOVE  'BPCA01' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00652          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00653          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00654          THEN                                                     G7C1PGM 
00655              MOVE  -1        TO  S1BESCIL                         G7C1PGM 
00656              MOVE  DFHBMUBF  TO  S1BESCIA                         G7C1PGM 
00657              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00658              THEN                                                 G7C1PGM 
00659                  NEXT SENTENCE                                    G7C1PGM 
00660              ELSE                                                 G7C1PGM 
00661                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00662                  SET WT-01-INDEX TO +08                           G7C1PGM 
00663                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00664          ELSE                                                     G7C1PGM 
00665              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00666              THEN                                                 G7C1PGM 
00667                  MOVE  DFHBMUBF  TO  S1BESCIA                     G7C1PGM 
00668              ELSE                                                 G7C1PGM 
00669                  NEXT SENTENCE                                    G7C1PGM 
00670      ELSE                                                         G7C1PGM 
00671          MOVE  -1        TO  S1BESCIL                             G7C1PGM 
00672          MOVE  DFHBMUBF  TO  S1BESCIA                             G7C1PGM 
00673          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00674          THEN                                                     G7C1PGM 
00675              NEXT SENTENCE                                        G7C1PGM 
00676          ELSE                                                     G7C1PGM 
00677              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00678              SET WT-01-INDEX TO +04                               G7C1PGM 
00679              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00680                                                                   G7C1PGM 
00681                                                                   G7C1PGM 
00682 *-- VALIDATE ------ EXCEPTION SCHED. ID -------------------------*G7C1PGM 
00683 *   1. ALPHANUMERIC                                               G7C1PGM 
00684 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00685                                                                   G7C1PGM 
00686      MOVE  S1EXCSCI TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00687      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7C1PGM 
00688          WS-02-CLASS-ALPHANUMERIC(2) AND                          G7C1PGM 
00689          WS-02-CLASS-ALPHANUMERIC(3) AND                          G7C1PGM 
00690          WS-02-CLASS-ALPHANUMERIC(4)                              G7C1PGM 
00691      THEN                                                         G7C1PGM 
00692          MOVE  S1EXCSCI TO GCVI-VALUE                             G7C1PGM 
00693          MOVE  'BPCA10' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00694          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00695          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00696          THEN                                                     G7C1PGM 
00697              MOVE  -1        TO  S1EXCSCL                         G7C1PGM 
00698              MOVE  DFHBMUBF  TO  S1EXCSCA                         G7C1PGM 
00699              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00700              THEN                                                 G7C1PGM 
00701                  NEXT SENTENCE                                    G7C1PGM 
00702              ELSE                                                 G7C1PGM 
00703                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00704                  SET WT-01-INDEX TO +09                           G7C1PGM 
00705                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00706          ELSE                                                     G7C1PGM 
00707              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00708              THEN                                                 G7C1PGM 
00709                  MOVE  DFHBMUBF  TO  S1EXCSCA                     G7C1PGM 
00710              ELSE                                                 G7C1PGM 
00711                  NEXT SENTENCE                                    G7C1PGM 
00712      ELSE                                                         G7C1PGM 
00713          MOVE  -1        TO  S1EXCSCL                             G7C1PGM 
00714          MOVE  DFHBMUBF  TO  S1EXCSCA                             G7C1PGM 
00715          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00716          THEN                                                     G7C1PGM 
00717              NEXT SENTENCE                                        G7C1PGM 
00718          ELSE                                                     G7C1PGM 
00719              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00720              SET WT-01-INDEX TO +08                               G7C1PGM 
00721              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00722                                                                   G7C1PGM 
00723                                                                   G7C1PGM 
00724 *-- VALIDATE ------ ELIG. METHOD OF TREATMENT IND ---------------*G7C1PGM 
00725 *   1. ALPHANUMERIC                                               G7C1PGM 
00726 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00727                                                                   G7C1PGM 
00728      MOVE  S1ELMTII TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00729      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
00730      THEN                                                         G7C1PGM 
00731          MOVE  S1ELMTII TO GCVI-VALUE                             G7C1PGM 
00732          MOVE  'BPBA09' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00733          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00734          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00735          THEN                                                     G7C1PGM 
00736              MOVE  -1        TO  S1ELMTIL                         G7C1PGM 
00737              MOVE  DFHBMUBF  TO  S1ELMTIA                         G7C1PGM 
00738              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00739              THEN                                                 G7C1PGM 
00740                  NEXT SENTENCE                                    G7C1PGM 
00741              ELSE                                                 G7C1PGM 
00742                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00743                  SET WT-01-INDEX TO +09                           G7C1PGM 
00744                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00745          ELSE                                                     G7C1PGM 
00746              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00747              THEN                                                 G7C1PGM 
00748                  MOVE  DFHBMUBF  TO  S1ELMTIA                     G7C1PGM 
00749              ELSE                                                 G7C1PGM 
00750                  NEXT SENTENCE                                    G7C1PGM 
00751      ELSE                                                         G7C1PGM 
00752          MOVE  -1        TO  S1ELMTIL                             G7C1PGM 
00753          MOVE  DFHBMUBF  TO  S1ELMTIA                             G7C1PGM 
00754          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00755          THEN                                                     G7C1PGM 
00756              NEXT SENTENCE                                        G7C1PGM 
00757          ELSE                                                     G7C1PGM 
00758              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00759              SET WT-01-INDEX TO +08                               G7C1PGM 
00760              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00761                                                                   G7C1PGM 
00762                                                                   G7C1PGM 
00763 *-- VALIDATE ------ MULTI-RELATED-PROCEDURES PRICING IND --------*G7C1PGM 
00764 *   1. ALPHANUMERIC                                               G7C1PGM 
00765 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00766                                                                   G7C1PGM 
00767      MOVE  S1MRPII TO WS-02-CLASS-TEST-AREA.                      G7C1PGM 
00768      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7C1PGM 
00769          WS-02-CLASS-ALPHANUMERIC(2)                              G7C1PGM 
00770      THEN                                                         G7C1PGM 
00771          MOVE  S1MRPII  TO GCVI-VALUE                             G7C1PGM 
00772          MOVE  'BPCB06' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00773          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00774          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00775          THEN                                                     G7C1PGM 
00776              MOVE  -1        TO  S1MRPIL                          G7C1PGM 
00777              MOVE  DFHBMUBF  TO  S1MRPIA                          G7C1PGM 
00778              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00779              THEN                                                 G7C1PGM 
00780                  NEXT SENTENCE                                    G7C1PGM 
00781              ELSE                                                 G7C1PGM 
00782                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00783                  SET WT-01-INDEX TO +09                           G7C1PGM 
00784                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00785          ELSE                                                     G7C1PGM 
00786              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00787              THEN                                                 G7C1PGM 
00788                  MOVE  DFHBMUBF  TO  S1MRPIA                      G7C1PGM 
00789              ELSE                                                 G7C1PGM 
00790                  NEXT SENTENCE                                    G7C1PGM 
00791      ELSE                                                         G7C1PGM 
00792          MOVE  -1        TO  S1MRPIL                              G7C1PGM 
00793          MOVE  DFHBMUBF  TO  S1MRPIA                              G7C1PGM 
00794          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00795          THEN                                                     G7C1PGM 
00796              NEXT SENTENCE                                        G7C1PGM 
00797          ELSE                                                     G7C1PGM 
00798              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00799              SET WT-01-INDEX TO +08                               G7C1PGM 
00800              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00801                                                                   G7C1PGM 
00802                                                                   G7C1PGM 
00803 *-- VALIDATE ------ PRIMARY SURGEON DEPENDENCY IND --------------*G7C1PGM 
00804 *   1. ALPHANUMERIC                                               G7C1PGM 
00805 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00806                                                                   G7C1PGM 
00807      MOVE  S1PSUDII TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00808      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
00809      THEN                                                         G7C1PGM 
00810          MOVE  S1PSUDII TO GCVI-VALUE                             G7C1PGM 
00811          MOVE  'BPCA03' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00812          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00813          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00814          THEN                                                     G7C1PGM 
00815              MOVE  -1        TO  S1PSUDIL                         G7C1PGM 
00816              MOVE  DFHBMUBF  TO  S1PSUDIA                         G7C1PGM 
00817              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00818              THEN                                                 G7C1PGM 
00819                  NEXT SENTENCE                                    G7C1PGM 
00820              ELSE                                                 G7C1PGM 
00821                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00822                  SET WT-01-INDEX TO +09                           G7C1PGM 
00823                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00824          ELSE                                                     G7C1PGM 
00825              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00826              THEN                                                 G7C1PGM 
00827                  MOVE  DFHBMUBF  TO  S1PSUDIA                     G7C1PGM 
00828              ELSE                                                 G7C1PGM 
00829                  NEXT SENTENCE                                    G7C1PGM 
00830      ELSE                                                         G7C1PGM 
00831          MOVE  -1        TO  S1PSUDIL                             G7C1PGM 
00832          MOVE  DFHBMUBF  TO  S1PSUDIA                             G7C1PGM 
00833          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00834          THEN                                                     G7C1PGM 
00835              NEXT SENTENCE                                        G7C1PGM 
00836          ELSE                                                     G7C1PGM 
00837              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00838              SET WT-01-INDEX TO +08                               G7C1PGM 
00839              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00840                                                                   G7C1PGM 
00841                                                                   G7C1PGM 
00842 *-- VALIDATE ------ HOSPITAL STAFF PROVIDER IND -----------------*G7C1PGM 
00843 *   1. ALPHANUMERIC                                               G7C1PGM 
00844 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00845                                                                   G7C1PGM 
00846      MOVE  S1HSTFPI TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00847      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
00848      THEN                                                         G7C1PGM 
00849          MOVE  S1HSTFPI TO GCVI-VALUE                             G7C1PGM 
00850          MOVE  'BPCA05' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00851          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00852          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00853          THEN                                                     G7C1PGM 
00854              MOVE  -1        TO  S1HSTFPL                         G7C1PGM 
00855              MOVE  DFHBMUBF  TO  S1HSTFPA                         G7C1PGM 
00856              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00857              THEN                                                 G7C1PGM 
00858                  NEXT SENTENCE                                    G7C1PGM 
00859              ELSE                                                 G7C1PGM 
00860                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00861                  SET WT-01-INDEX TO +09                           G7C1PGM 
00862                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00863          ELSE                                                     G7C1PGM 
00864              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00865              THEN                                                 G7C1PGM 
00866                  MOVE  DFHBMUBF  TO  S1HSTFPA                     G7C1PGM 
00867              ELSE                                                 G7C1PGM 
00868                  NEXT SENTENCE                                    G7C1PGM 
00869      ELSE                                                         G7C1PGM 
00870          MOVE  -1        TO  S1HSTFPL                             G7C1PGM 
00871          MOVE  DFHBMUBF  TO  S1HSTFPA                             G7C1PGM 
00872          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00873          THEN                                                     G7C1PGM 
00874              NEXT SENTENCE                                        G7C1PGM 
00875          ELSE                                                     G7C1PGM 
00876              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00877              SET WT-01-INDEX TO +08                               G7C1PGM 
00878              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00879                                                                   G7C1PGM 
00880                                                                   G7C1PGM 
00881 *-- VALIDATE ------ REPEAT PROCEDURE IND ------------------------*G7C1PGM 
00882 *   1. ALPHANUMERIC                                               G7C1PGM 
00883 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00884                                                                   G7C1PGM 
00885      MOVE  S1RPTPRI TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00886      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
00887      THEN                                                         G7C1PGM 
00888          MOVE  S1RPTPRI TO GCVI-VALUE                             G7C1PGM 
00889          MOVE  'BPCA09' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00890          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00891          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00892          THEN                                                     G7C1PGM 
00893              MOVE  -1        TO  S1RPTPRL                         G7C1PGM 
00894              MOVE  DFHBMUBF  TO  S1RPTPRA                         G7C1PGM 
00895              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00896              THEN                                                 G7C1PGM 
00897                  NEXT SENTENCE                                    G7C1PGM 
00898              ELSE                                                 G7C1PGM 
00899                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00900                  SET WT-01-INDEX TO +09                           G7C1PGM 
00901                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00902          ELSE                                                     G7C1PGM 
00903              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00904              THEN                                                 G7C1PGM 
00905                  MOVE  DFHBMUBF  TO  S1RPTPRA                     G7C1PGM 
00906              ELSE                                                 G7C1PGM 
00907                  NEXT SENTENCE                                    G7C1PGM 
00908      ELSE                                                         G7C1PGM 
00909          MOVE  -1        TO  S1RPTPRL                             G7C1PGM 
00910          MOVE  DFHBMUBF  TO  S1RPTPRA                             G7C1PGM 
00911          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00912          THEN                                                     G7C1PGM 
00913              NEXT SENTENCE                                        G7C1PGM 
00914          ELSE                                                     G7C1PGM 
00915              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
00916              SET WT-01-INDEX TO +08                               G7C1PGM 
00917              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00918                                                                   G7C1PGM 
00919                                                                   G7C1PGM 
00920 *-- VALIDATE ------ MINIMUM ELIGIBLE AMOUNT ---------------------*G7C1PGM 
00921 *  D129     CONVERSION                                            G7C1PGM 
00922                                                                   G7C1PGM 
00923      MOVE S1MNAMTI TO D-C-RECEIVE-FIELD.                          G7C1PGM 
00924      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C1PGM 
00925      MOVE '00' TO D-C-RETURN-CODE.                                G7C1PGM 
00926      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C1PGM 
00927      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C1PGM 
00928      IF D-C-RETURN-CODE = '00'                                    G7C1PGM 
00929          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C1PGM 
00930              MOVE -1       TO S1MNAMTL                            G7C1PGM 
00931              MOVE DFHBMUBF TO S1MNAMTA                            G7C1PGM 
00932              IF WS-02-SCREEN-HAS-ERRORS                           G7C1PGM 
00933                  NEXT SENTENCE                                    G7C1PGM 
00934              ELSE                                                 G7C1PGM 
00935                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C1PGM 
00936                  SET WT-01-INDEX TO +3                            G7C1PGM 
00937                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00938          ELSE                                                     G7C1PGM 
00939              MOVE D-C-RETURN-FIELD-DEC2                           G7C1PGM 
00940                TO WS-02-MIN-ELIG-AMT                              G7C1PGM 
00941              MOVE WS-02-MIN-ELIG-AMT                              G7C1PGM 
00942                TO WS-02-DISP-5POS-DEC                             G7C1PGM 
00943              MOVE WS-02-DISP-5POS-DEC                             G7C1PGM 
00944                TO S1MNAMTO                                        G7C1PGM 
00945      ELSE                                                         G7C1PGM 
00946          MOVE -1       TO S1MNAMTL                                G7C1PGM 
00947          MOVE DFHBMUBF TO S1MNAMTA                                G7C1PGM 
00948          IF WS-02-SCREEN-HAS-ERRORS                               G7C1PGM 
00949              NEXT SENTENCE                                        G7C1PGM 
00950          ELSE                                                     G7C1PGM 
00951              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C1PGM 
00952              IF D-C-RETURN-CODE = '10'                            G7C1PGM 
00953                  SET WT-01-INDEX TO +10                           G7C1PGM 
00954                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00955              ELSE                                                 G7C1PGM 
00956                  SET WT-01-INDEX TO +2                            G7C1PGM 
00957                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C1PGM 
00958                                                                   G7C1PGM 
00959 *    IF  S1MNAMTI IS NUMERIC                                      G7C1PGM 
00960 *    THEN                                                         G7C1PGM 
00961 *        NEXT SENTENCE                                            G7C1PGM 
00962 *    ELSE                                                         G7C1PGM 
00963 *        MOVE  -1        TO  S1MNAMTL                             G7C1PGM 
00964 *        MOVE  DFHBMUBF  TO  S1MNAMTA                             G7C1PGM 
00965 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
00966 *        THEN                                                     G7C1PGM 
00967 *            NEXT SENTENCE                                        G7C1PGM 
00968 *        ELSE                                                     G7C1PGM 
00969 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7C1PGM 
00970 *            SET WT-01-INDEX TO +10                               G7C1PGM 
00971 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
00972                                                                   G7C1PGM 
00973                                                                   G7C1PGM 
00974 *-- VALIDATE ------ CORRIDOR OVERRIDE ---------------------------*G7C1PGM 
00975 *   1. ALPHANUMERIC                                               G7C1PGM 
00976 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
00977                                                                   G7C1PGM 
00978      MOVE  S1CORORI TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
00979      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
00980      THEN                                                         G7C1PGM 
00981          MOVE  S1CORORI TO GCVI-VALUE                             G7C1PGM 
00982          MOVE  'BPCA02' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
00983          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
00984          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
00985          THEN                                                     G7C1PGM 
00986              MOVE  -1        TO  S1CORORL                         G7C1PGM 
00987              MOVE  DFHBMUBF  TO  S1CORORA                         G7C1PGM 
00988              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
00989              THEN                                                 G7C1PGM 
00990                  NEXT SENTENCE                                    G7C1PGM 
00991              ELSE                                                 G7C1PGM 
00992                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
00993                  SET WT-01-INDEX TO +09                           G7C1PGM 
00994                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
00995          ELSE                                                     G7C1PGM 
00996              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
00997              THEN                                                 G7C1PGM 
00998                  MOVE  DFHBMUBF  TO  S1CORORA                     G7C1PGM 
00999              ELSE                                                 G7C1PGM 
01000                  NEXT SENTENCE                                    G7C1PGM 
01001      ELSE                                                         G7C1PGM 
01002          MOVE  -1        TO  S1CORORL                             G7C1PGM 
01003          MOVE  DFHBMUBF  TO  S1CORORA                             G7C1PGM 
01004          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
01005          THEN                                                     G7C1PGM 
01006              NEXT SENTENCE                                        G7C1PGM 
01007          ELSE                                                     G7C1PGM 
01008              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
01009              SET WT-01-INDEX TO +08                               G7C1PGM 
01010              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
01011                                                                   G7C1PGM 
01012                                                                   G7C1PGM 
01013 *-- VALIDATE ------ MULTI-UNRELATED PROCEDURES PRICING IND ------*G7C1PGM 
01014 *   1. ALPHANUMERIC                                               G7C1PGM 
01015 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
01016                                                                   G7C1PGM 
01017      MOVE  S1SMPPII TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
01018      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
01019      THEN                                                         G7C1PGM 
01020          MOVE  S1SMPPII TO GCVI-VALUE                             G7C1PGM 
01021          MOVE  'BPCB01' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
01022          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
01023          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
01024          THEN                                                     G7C1PGM 
01025              MOVE  -1        TO  S1SMPPIL                         G7C1PGM 
01026              MOVE  DFHBMUBF  TO  S1SMPPIA                         G7C1PGM 
01027              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
01028              THEN                                                 G7C1PGM 
01029                  NEXT SENTENCE                                    G7C1PGM 
01030              ELSE                                                 G7C1PGM 
01031                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
01032                  SET WT-01-INDEX TO +09                           G7C1PGM 
01033                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
01034          ELSE                                                     G7C1PGM 
01035              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
01036              THEN                                                 G7C1PGM 
01037                  MOVE  DFHBMUBF  TO  S1SMPPIA                     G7C1PGM 
01038              ELSE                                                 G7C1PGM 
01039                  NEXT SENTENCE                                    G7C1PGM 
01040      ELSE                                                         G7C1PGM 
01041          MOVE  -1        TO  S1SMPPIL                             G7C1PGM 
01042          MOVE  DFHBMUBF  TO  S1SMPPIA                             G7C1PGM 
01043          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
01044          THEN                                                     G7C1PGM 
01045              NEXT SENTENCE                                        G7C1PGM 
01046          ELSE                                                     G7C1PGM 
01047              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
01048              SET WT-01-INDEX TO +08                               G7C1PGM 
01049              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
01050                                                                   G7C1PGM 
01051                                                                   G7C1PGM 
01052 *-- VALIDATE ------ MULTI-INJURY PRICING MODIFICATION IND -------*G7C1PGM 
01053 *   1. ALPHANUMERIC                                               G7C1PGM 
01054 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C1PGM 
01055                                                                   G7C1PGM 
01056      MOVE  S1MIJPMI TO WS-02-CLASS-TEST-AREA.                     G7C1PGM 
01057      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C1PGM 
01058      THEN                                                         G7C1PGM 
01059          MOVE  S1MIJPMI TO GCVI-VALUE                             G7C1PGM 
01060          MOVE  'BPCB11' TO GCVI-FIELDS-KEY-ID                     G7C1PGM 
01061          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C1PGM 
01062          IF  GCVI-VALUE-NOT-FOUND                                 G7C1PGM 
01063          THEN                                                     G7C1PGM 
01064              MOVE  -1        TO  S1MIJPML                         G7C1PGM 
01065              MOVE  DFHBMUBF  TO  S1MIJPMA                         G7C1PGM 
01066              IF  WS-02-SCREEN-HAS-ERRORS                          G7C1PGM 
01067              THEN                                                 G7C1PGM 
01068                  NEXT SENTENCE                                    G7C1PGM 
01069              ELSE                                                 G7C1PGM 
01070                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
01071                  SET WT-01-INDEX TO +09                           G7C1PGM 
01072                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
01073          ELSE                                                     G7C1PGM 
01074              IF  GCVI-VALUE-NOT-LOADED                            G7C1PGM 
01075              THEN                                                 G7C1PGM 
01076                  MOVE  DFHBMUBF  TO  S1MIJPMA                     G7C1PGM 
01077              ELSE                                                 G7C1PGM 
01078                  NEXT SENTENCE                                    G7C1PGM 
01079      ELSE                                                         G7C1PGM 
01080          MOVE  -1        TO  S1MIJPML                             G7C1PGM 
01081          MOVE  DFHBMUBF  TO  S1MIJPMA                             G7C1PGM 
01082          IF  WS-02-SCREEN-HAS-ERRORS                              G7C1PGM 
01083          THEN                                                     G7C1PGM 
01084              NEXT SENTENCE                                        G7C1PGM 
01085          ELSE                                                     G7C1PGM 
01086              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C1PGM 
01087              SET WT-01-INDEX TO +08                               G7C1PGM 
01088              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C1PGM 
01089                                                                   G7C1PGM 
01090                                                                   G7C1PGM 
01091  2100-900-EXIT.                                                   G7C1PGM 
01092      EXIT.                                                        G7C1PGM 
01093 /***************************************************************  G7C1PGM 
01094 *                                                              *  G7C1PGM 
01095 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7C1PGM 
01096 *                                                              *  G7C1PGM 
01097 ****************************************************************  G7C1PGM 
01098  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7C1PGM 
01099  2110-010.                                                        G7C1PGM 
01100                                                                   G7C1PGM 
01101      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7C1PGM 
01102                                                                   G7C1PGM 
01103      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7C1PGM 
01104                       COMMAREA(GCVIOPGM-PARM-LIST)                G7C1PGM 
01105                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7C1PGM 
01106                       END-EXEC.                                   G7C1PGM 
01107                                                                   G7C1PGM 
01108      IF  GCVI-VALUE-NOT-LOADED                                    G7C1PGM 
01109          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7C1PGM 
01110                                                                   G7C1PGM 
01111  2110-900-EXIT.                                                   G7C1PGM 
01112      EXIT.                                                        G7C1PGM 
01113 /***************************************************************  G7C1PGM 
01114 *                                                              *  G7C1PGM 
01115 * 2200  DO SCREEN LOGICAL EDITS                                *  G7C1PGM 
01116 *                                                              *  G7C1PGM 
01117 ****************************************************************  G7C1PGM 
01118  2200-000-LOGICAL-EDITS         SECTION.                          G7C1PGM 
01119  2200-010.                                                        G7C1PGM 
01120                                                                   G7C1PGM 
01121 *----------------------------------------------------------------*G7C1PGM 
01122 *                                                                *G7C1PGM 
01123 *                                                                *G7C1PGM 
01124 *  THIS MODULE HAS NO LOGICAL EDIT REQUIREMENTS                  *G7C1PGM 
01125 *                                                                *G7C1PGM 
01126 *                                                                *G7C1PGM 
01127 *----------------------------------------------------------------*G7C1PGM 
01128                                                                   G7C1PGM 
01129                                                                   G7C1PGM 
01130 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7C1PGM 
01131                                                                   G7C1PGM 
01132      IF  WS-02-SCREEN-HAS-ERRORS                                  G7C1PGM 
01133      THEN                                                         G7C1PGM 
01134          NEXT SENTENCE                                            G7C1PGM 
01135      ELSE                                                         G7C1PGM 
01136          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7C1PGM 
01137          THEN                                                     G7C1PGM 
01138              IF EIBAID = DFHPF4 OR DFHPF16                        G7C1PGM 
01139              THEN                                                 G7C1PGM 
01140                  NEXT SENTENCE                                    G7C1PGM 
01141              ELSE                                                 G7C1PGM 
01142                  MOVE  -1        TO S1ERRL                        G7C1PGM 
01143                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C1PGM 
01144                  SET WT-01-INDEX TO +06                           G7C1PGM 
01145                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C1PGM 
01146          ELSE                                                     G7C1PGM 
01147              NEXT SENTENCE.                                       G7C1PGM 
01148                                                                   G7C1PGM 
01149                                                                   G7C1PGM 
01150  2200-900-EXIT.                                                   G7C1PGM 
01151      EXIT.                                                        G7C1PGM 
01152 /***************************************************************  G7C1PGM 
01153 *                                                              *  G7C1PGM 
01154 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7C1PGM 
01155 *        REWRITE TO WORKFILE.                                  *  G7C1PGM 
01156 *                                                              *  G7C1PGM 
01157 ****************************************************************  G7C1PGM 
01158  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7C1PGM 
01159  2300-010.                                                        G7C1PGM 
01160                                                                   G7C1PGM 
01161 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7C1PGM 
01162                                                                   G7C1PGM 
01163      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7C1PGM 
01164      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7C1PGM 
01165        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7C1PGM 
01166      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7C1PGM 
01167      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7C1PGM 
01168      IF  NOT GCIO2-GOOD-RETURN                                    G7C1PGM 
01169          MOVE WS-01-ABCODE-C1F2     TO WS-01-ABCODE               G7C1PGM 
01170          MOVE WS-01-ABCODE-C1F2-MSG TO WS-01-ABCODE-MSG           G7C1PGM 
01171          PERFORM  9999-000-ABEND-THE-TASK.                        G7C1PGM 
01172                                                                   G7C1PGM 
01173                                                                   G7C1PGM 
01174 *----- SAVE FIELDS FROM SCREEN THAT CANNOT BE DIRECTLY ----------*G7C1PGM 
01175 *        COMPARED TO THE RECORD                                   G7C1PGM 
01176                                                                   G7C1PGM 
01177 *    MOVE S1MNAMTI    TO WS-02-MIN-ELIG-AMT-X.                    G7C1PGM 
01178                                                                   G7C1PGM 
01179                                                                   G7C1PGM 
01180 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7C1PGM 
01181                                                                   G7C1PGM 
01182      MOVE GPC2-MIN-ELIG-AMT          TO                           G7C1PGM 
01183          WS-GPC2-MIN-ELIG-AMT.                                    G7C1PGM 
01184      IF      S1BESCII           = GPC2-BEN-SCOPE-ID               G7C1PGM 
01185          AND S1EXCSCI           = GPC2-EXCP-SCHED-ID              G7C1PGM 
01186          AND S1ELMTII           = GPC2-ELIG-METHD-OF-TREAT-IND    G7C1PGM 
01187          AND S1MRPII            = GPC2-MULT-REL-PROC-IND          G7C1PGM 
01188          AND S1PSUDII           = GPC2-PRIM-SURG-DEPEND-IND       G7C1PGM 
01189          AND S1HSTFPI           = GPC2-HOSP-STAFF-PROV-IND        G7C1PGM 
01190          AND S1RPTPRI           = GPC2-REPEAT-PROC-IND            G7C1PGM 
01191          AND WS-02-MIN-ELIG-AMT = WS-GPC2-MIN-ELIG-AMT            G7C1PGM 
01192          AND S1CORORI           = GPC2-CORRIDOR-OVERRIDE          G7C1PGM 
01193          AND S1SMPPII           = GPC2-SURG-MULT-PROC-PRICE-IND   G7C1PGM 
01194          AND S1MIJPMI           = GPC2-MULT-INJ-PRICING-MOD-IND   G7C1PGM 
01195      THEN                                                         G7C1PGM 
01196          GO TO 2300-900-EXIT                                      G7C1PGM 
01197      ELSE                                                         G7C1PGM 
01198          NEXT SENTENCE.                                           G7C1PGM 
01199                                                                   G7C1PGM 
01200                                                                   G7C1PGM 
01201 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7C1PGM 
01202                                                                   G7C1PGM 
01203      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7C1PGM 
01204      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7C1PGM 
01205      IF  NOT GCIO2-GOOD-RETURN                                    G7C1PGM 
01206          MOVE WS-01-ABCODE-C1F3     TO WS-01-ABCODE               G7C1PGM 
01207          MOVE WS-01-ABCODE-C1F3-MSG TO WS-01-ABCODE-MSG           G7C1PGM 
01208          PERFORM  9999-000-ABEND-THE-TASK.                        G7C1PGM 
01209                                                                   G7C1PGM 
01210                                                                   G7C1PGM 
01211 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7C1PGM 
01212                                                                   G7C1PGM 
01213      MOVE S1BESCII           TO GPC2-BEN-SCOPE-ID.                G7C1PGM 
01214      MOVE S1EXCSCI           TO GPC2-EXCP-SCHED-ID.               G7C1PGM 
01215      MOVE S1ELMTII           TO GPC2-ELIG-METHD-OF-TREAT-IND.     G7C1PGM 
01216      MOVE S1MRPII            TO GPC2-MULT-REL-PROC-IND.           G7C1PGM 
01217      MOVE S1PSUDII           TO GPC2-PRIM-SURG-DEPEND-IND.        G7C1PGM 
01218      MOVE S1HSTFPI           TO GPC2-HOSP-STAFF-PROV-IND.         G7C1PGM 
01219      MOVE S1RPTPRI           TO GPC2-REPEAT-PROC-IND.             G7C1PGM 
01220      MOVE WS-02-MIN-ELIG-AMT TO GPC2-MIN-ELIG-AMT.                G7C1PGM 
01221      MOVE S1CORORI           TO GPC2-CORRIDOR-OVERRIDE.           G7C1PGM 
01222      MOVE S1SMPPII           TO GPC2-SURG-MULT-PROC-PRICE-IND.    G7C1PGM 
01223      MOVE S1MIJPMI           TO GPC2-MULT-INJ-PRICING-MOD-IND.    G7C1PGM 
01224                                                                   G7C1PGM 
01225                                                                   G7C1PGM 
01226 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7C1PGM 
01227                                                                   G7C1PGM 
01228 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7C1PGM 
01229                                                                   G7C1PGM 
01230      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7C1PGM 
01231      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7C1PGM 
01232      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7C1PGM 
01233      IF  NOT GCIO2-GOOD-RETURN                                    G7C1PGM 
01234          MOVE WS-01-ABCODE-C1F4     TO WS-01-ABCODE               G7C1PGM 
01235          MOVE WS-01-ABCODE-C1F4-MSG TO WS-01-ABCODE-MSG           G7C1PGM 
01236          PERFORM  9999-000-ABEND-THE-TASK.                        G7C1PGM 
01237                                                                   G7C1PGM 
01238  2300-900-EXIT.                                                   G7C1PGM 
01239      EXIT.                                                        G7C1PGM 
01240 /***************************************************************  G7C1PGM 
01241 *                                                              *  G7C1PGM 
01242 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7C1PGM 
01243 *                                                              *  G7C1PGM 
01244 ****************************************************************  G7C1PGM 
01245  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7C1PGM 
01246  2310-010.                                                        G7C1PGM 
01247                                                                   G7C1PGM 
01248                                                                   G7C1PGM 
01249 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7C1PGM 
01250                                                                   G7C1PGM 
01251      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7C1PGM 
01252                                         + GC-WORKFILE-KEY-LEN     G7C1PGM 
01253                                         + GC-GCBENPRV-MAX-REC-LEN.G7C1PGM 
01254                                                                   G7C1PGM 
01255      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7C1PGM 
01256                          INITIMG(WS-02-HEX-00)                    G7C1PGM 
01257                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7C1PGM 
01258                          END-EXEC.                                G7C1PGM 
01259                                                                   G7C1PGM 
01260 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7C1PGM 
01261                                                                   G7C1PGM 
01262      MOVE SPACES                 TO GCIO-CONTRACT-FILE-KEY.       G7C1PGM 
01263      MOVE WRK-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           G7C1PGM 
01264      MOVE WRK-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3.        G7C1PGM 
01265      MOVE WRK-SEC-NO-1           TO GCIO-WRK-SEC-NO-1.            G7C1PGM 
01266      MOVE WRK-PKG-CODE           TO GCIO-WRK-PKG-CODE.            G7C1PGM 
01267      MOVE WRK-EFFECTIVE-DATE     TO GCIO-WRK-EFFECTIVE-DT.        G7C1PGM 
01268                                                                   G7C1PGM 
01269      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7C1PGM 
01270      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7C1PGM 
01271      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7C1PGM 
01272      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7C1PGM 
01273      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7C1PGM 
01274      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7C1PGM 
01275      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7C1PGM 
01276      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7C1PGM 
01277      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7C1PGM 
01278                                                                   G7C1PGM 
01279 *    MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7C1PGM 
01280 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7C1PGM 
01281 *    IF  HGADATE-RETURN = ZEROS                                   G7C1PGM 
01282 *    THEN                                                         G7C1PGM 
01283 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7C1PGM 
01284 *    ELSE                                                         G7C1PGM 
01285 *        SET WT-01-INDEX TO +07                                   G7C1PGM 
01286 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7C1PGM 
01287 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7C1PGM 
01288                                                                   G7C1PGM 
01289      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7C1PGM 
01290      MOVE S1BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7C1PGM 
01291      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7C1PGM 
01292      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7C1PGM 
01293      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7C1PGM 
01294      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7C1PGM 
01295      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7C1PGM 
01296                                                                   G7C1PGM 
01297                                                                   G7C1PGM 
01298  2310-900-EXIT.                                                   G7C1PGM 
01299      EXIT.                                                        G7C1PGM 
01300 /***************************************************************  G7C1PGM 
01301 *                                                              *  G7C1PGM 
01302 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7C1PGM 
01303 *                                                              *  G7C1PGM 
01304 ****************************************************************  G7C1PGM 
01305  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7C1PGM 
01306  2400-010.                                                        G7C1PGM 
01307                                                                   G7C1PGM 
01308                                                                   G7C1PGM 
01309      IF  EIBAID = DFHPF7  OR DFHPF19                              G7C1PGM 
01310      THEN                                                         G7C1PGM 
01311          MOVE 'GC6CPGM' TO WS-02-NEXT-PROGRAM.                    G7C1PGM 
01312                                                                   G7C1PGM 
01313      IF  EIBAID = DFHENTER OR                                     G7C1PGM 
01314                   DFHPF4   OR DFHPF16 OR                          G7C1PGM 
01315                   DFHPF8   OR DFHPF20                             G7C1PGM 
01316      THEN                                                         G7C1PGM 
01317          MOVE 'G7C2PGM' TO WS-02-NEXT-PROGRAM.                    G7C1PGM 
01318                                                                   G7C1PGM 
01319      IF  EIBAID = DFHPF6  OR DFHPF18                              G7C1PGM 
01320      THEN                                                         G7C1PGM 
01321          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7C1PGM 
01322                                                                   G7C1PGM 
01323                                                                   G7C1PGM 
01324      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7C1PGM 
01325                       COMMAREA(WORK-RECORD-2)                     G7C1PGM 
01326                       LENGTH  (GCIO2-RECORD-LENGTH)               G7C1PGM 
01327                       END-EXEC.                                   G7C1PGM 
01328                                                                   G7C1PGM 
01329  2400-900-EXIT.                                                   G7C1PGM 
01330      EXIT.                                                        G7C1PGM 
01331 /***************************************************************  G7C1PGM 
01332 *                                                              *  G7C1PGM 
01333 * 2500   LINK TO GX3APGM FOR CONVERSION.                          G7C1PGM 
01334 *                                                              *  G7C1PGM 
01335 ****************************************************************  G7C1PGM 
01336  2500-LINK-TO-GX3APGM.                                            G7C1PGM 
01337                                                                   G7C1PGM 
01338      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7C1PGM 
01339                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7C1PGM 
01340                       LENGTH  (+51)                               G7C1PGM 
01341                       END-EXEC.                                   G7C1PGM 
01342                                                                   G7C1PGM 
01343                                                                   G7C1PGM 
01344  2500-EXIT.                                                       G7C1PGM 
01345      EXIT.                                                        G7C1PGM 
01346 /***************************************************************  G7C1PGM 
01347 *                                                              *  G7C1PGM 
01348 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7C1PGM 
01349 *         PROVISION RECORD (TYPE=C4)                           *  G7C1PGM 
01350 *                                                              *  G7C1PGM 
01351 ****************************************************************  G7C1PGM 
01352  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7C1PGM 
01353  5000-010.                                                        G7C1PGM 
01354                                                                   G7C1PGM 
01355      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7C1PGM 
01356                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7C1PGM 
01357                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7C1PGM 
01358                       END-EXEC.                                   G7C1PGM 
01359                                                                   G7C1PGM 
01360                                                                   G7C1PGM 
01361  5000-900-EXIT.                                                   G7C1PGM 
01362      EXIT.                                                        G7C1PGM 
01363 /***************************************************************  G7C1PGM 
01364 *                                                              *  G7C1PGM 
01365 * 5100                                                         *  G7C1PGM 
01366 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7C1PGM 
01367 *                                                              *  G7C1PGM 
01368 ****************************************************************  G7C1PGM 
01369  5100-000-W-F-CONTRACT-IO       SECTION.                          G7C1PGM 
01370  5100-010.                                                        G7C1PGM 
01371                                                                   G7C1PGM 
01372      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7C1PGM 
01373                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7C1PGM 
01374                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7C1PGM 
01375                       END-EXEC.                                   G7C1PGM 
01376                                                                   G7C1PGM 
01377                                                                   G7C1PGM 
01378  5100-900-EXIT.                                                   G7C1PGM 
01379      EXIT.                                                        G7C1PGM 
01380 /***************************************************************  G7C1PGM 
01381 *                                                              *  G7C1PGM 
01382 * 9000   MOVE MESSAGE TO SCREEN                                *  G7C1PGM 
01383 *                                                              *  G7C1PGM 
01384 ****************************************************************  G7C1PGM 
01385  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7C1PGM 
01386  9000-010.                                                        G7C1PGM 
01387                                                                   G7C1PGM 
01388      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S1ERRO.              G7C1PGM 
01389                                                                   G7C1PGM 
01390  9000-900-EXIT.                                                   G7C1PGM 
01391      EXIT.                                                        G7C1PGM 
01392 /***************************************************************  G7C1PGM 
01393 *                                                              *  G7C1PGM 
01394 * 9100 SEND SCREEN AND RETURN                                  *  G7C1PGM 
01395 *                                                              *  G7C1PGM 
01396 ****************************************************************  G7C1PGM 
01397  9100-000-SEND-THEN-RETURN      SECTION.                          G7C1PGM 
01398  9100-010.                                                        G7C1PGM 
01399                                                                   G7C1PGM 
01400                                                                   G7C1PGM 
01401 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7C1PGM 
01402      MOVE  -1 TO  S1ERRL.                                         G7C1PGM 
01403                                                                   G7C1PGM 
01404                                                                   G7C1PGM 
01405      IF  WS-02-MY-EIBTRNID                                        G7C1PGM 
01406      THEN                                                         G7C1PGM 
01407          EXEC CICS  SEND MAP   ('G7C1I01')                        G7C1PGM 
01408                          MAPSET('G7C1SET')                        G7C1PGM 
01409                          DATAONLY                                 G7C1PGM 
01410                          CURSOR                                   G7C1PGM 
01411                          END-EXEC                                 G7C1PGM 
01412      ELSE                                                         G7C1PGM 
01413          EXEC CICS  SEND MAP   ('G7C1I01')                        G7C1PGM 
01414                          MAPSET('G7C1SET')                        G7C1PGM 
01415                          ERASE                                    G7C1PGM 
01416                          CURSOR                                   G7C1PGM 
01417                          END-EXEC.                                G7C1PGM 
01418                                                                   G7C1PGM 
01419      EXEC CICS RETURN                                             G7C1PGM 
01420                TRANSID  ('G7C1')                                  G7C1PGM 
01421                COMMAREA (DFHCOMMAREA)                             G7C1PGM 
01422                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7C1PGM 
01423                END-EXEC.                                          G7C1PGM 
01424                                                                   G7C1PGM 
01425 *    EXEC CICS  RETURN                                            G7C1PGM 
01426 *               END-EXEC.                                         G7C1PGM 
01427                                                                   G7C1PGM 
01428  9100-900-EXIT.                                                   G7C1PGM 
01429      EXIT.                                                        G7C1PGM 
01430 /*****************************************************************G7C1PGM 
01431 *                                                                *G7C1PGM 
01432 * 9200    XCTL TO GCPSPGM                                        *G7C1PGM 
01433 *                                                                *G7C1PGM 
01434 *                                                                *G7C1PGM 
01435 ******************************************************************G7C1PGM 
01436  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7C1PGM 
01437  9200-010.                                                        G7C1PGM 
01438                                                                   G7C1PGM 
01439      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7C1PGM 
01440                       END-EXEC.                                   G7C1PGM 
01441                                                                   G7C1PGM 
01442  9200-900-EXIT.                                                   G7C1PGM 
01443      EXIT.                                                        G7C1PGM 
01444 /*****************************************************************G7C1PGM 
01445 *                                                                *G7C1PGM 
01446 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7C1PGM 
01447 *                                                                *G7C1PGM 
01448 *                                                                *G7C1PGM 
01449 ******************************************************************G7C1PGM 
01450  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7C1PGM 
01451  9210-010.                                                        G7C1PGM 
01452                                                                   G7C1PGM 
01453      IF  S1GRPNOI = '000SPS000'                                   G7C1PGM 
01454          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7C1PGM 
01455                           END-EXEC.                               G7C1PGM 
01456                                                                   G7C1PGM 
01457 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7C1PGM 
01458                                                                   G7C1PGM 
01459      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7C1PGM 
01460                                        + GC-WORKFILE-KEY-LEN      G7C1PGM 
01461                                        + GC-GCCONTR-MAX-REC-LEN.  G7C1PGM 
01462                                                                   G7C1PGM 
01463      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7C1PGM 
01464                          INITIMG(WS-02-HEX-00)                    G7C1PGM 
01465                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7C1PGM 
01466                          END-EXEC.                                G7C1PGM 
01467                                                                   G7C1PGM 
01468 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7C1PGM 
01469 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7C1PGM 
01470                                                                   G7C1PGM 
01471 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7C1PGM 
01472                                                                   G7C1PGM 
01473      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7C1PGM 
01474        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7C1PGM 
01475                                                                   G7C1PGM 
01476      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7C1PGM 
01477      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7C1PGM 
01478                                                                   G7C1PGM 
01479      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7C1PGM 
01480      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7C1PGM 
01481      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7C1PGM 
01482      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7C1PGM 
01483      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7C1PGM 
01484      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7C1PGM 
01485      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7C1PGM 
01486      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7C1PGM 
01487      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7C1PGM 
01488      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7C1PGM 
01489      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7C1PGM 
01490      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7C1PGM 
01491      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7C1PGM 
01492      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7C1PGM 
01493                                                                   G7C1PGM 
01494 *    MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7C1PGM 
01495 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7C1PGM 
01496 *    IF  HGADATE-RETURN = ZEROS                                   G7C1PGM 
01497 *    THEN                                                         G7C1PGM 
01498 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7C1PGM 
01499 *    ELSE                                                         G7C1PGM 
01500 *        SET WT-01-INDEX TO +07                                   G7C1PGM 
01501 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7C1PGM 
01502 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7C1PGM 
01503                                                                   G7C1PGM 
01504      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7C1PGM 
01505      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7C1PGM 
01506      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7C1PGM 
01507      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7C1PGM 
01508      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7C1PGM 
01509      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7C1PGM 
01510      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7C1PGM 
01511                                                                   G7C1PGM 
01512      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7C1PGM 
01513                                                                   G7C1PGM 
01514      IF  NOT GCIO3-GOOD-RETURN                                    G7C1PGM 
01515          MOVE WS-01-ABCODE-C1F1     TO WS-01-ABCODE               G7C1PGM 
01516          MOVE WS-01-ABCODE-C1F1-MSG TO WS-01-ABCODE-MSG           G7C1PGM 
01517          PERFORM  9999-000-ABEND-THE-TASK.                        G7C1PGM 
01518                                                                   G7C1PGM 
01519      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7C1PGM 
01520                       COMMAREA(WORK-RECORD-3)                     G7C1PGM 
01521                       LENGTH  (GCIO3-RECORD-LENGTH)               G7C1PGM 
01522                       END-EXEC.                                   G7C1PGM 
01523                                                                   G7C1PGM 
01524  9210-900-EXIT.                                                   G7C1PGM 
01525      EXIT.                                                        G7C1PGM 
01526 /*****************************************************************G7C1PGM 
01527 *                                                                *G7C1PGM 
01528 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7C1PGM 
01529 *                                                                *G7C1PGM 
01530 *                                                                *G7C1PGM 
01531 ******************************************************************G7C1PGM 
01532  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7C1PGM 
01533  9220-010.                                                        G7C1PGM 
01534                                                                   G7C1PGM 
01535      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7C1PGM 
01536                       END-EXEC.                                   G7C1PGM 
01537                                                                   G7C1PGM 
01538  9220-900-EXIT.                                                   G7C1PGM 
01539      EXIT.                                                        G7C1PGM 
01540 /*****************************************************************G7C1PGM 
01541 *                                                                *G7C1PGM 
01542 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7C1PGM 
01543 *                                                                *G7C1PGM 
01544 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7C1PGM 
01545 *                                                                *G7C1PGM 
01546 ******************************************************************G7C1PGM 
01547  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7C1PGM 
01548  9800-010.                                                        G7C1PGM 
01549                                                                   G7C1PGM 
01550      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7C1PGM 
01551      MOVE 'M'   TO  HGADATE-FORM1.                                G7C1PGM 
01552      MOVE 'J'   TO  HGADATE-FORM2.                                G7C1PGM 
01553      MOVE ZEROS TO  HGADATE-RETURN                                G7C1PGM 
01554                     HGADATE-AMOUNT.                               G7C1PGM 
01555      EXEC CICS LINK PROGRAM ('HGADATES')                          G7C1PGM 
01556                     COMMAREA(HGADATES-COMMAREA)                   G7C1PGM 
01557                     LENGTH  (24)                                  G7C1PGM 
01558                     END-EXEC.                                     G7C1PGM 
01559                                                                   G7C1PGM 
01560  9800-900-900-EXIT.                                               G7C1PGM 
01561      EXIT.                                                        G7C1PGM 
01562 /*****************************************************************G7C1PGM 
01563 *                                                                *G7C1PGM 
01564 * 9810    J U L I A N    T O    G R E G O R I A N                *G7C1PGM 
01565 *                                                                *G7C1PGM 
01566 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7C1PGM 
01567 *                                                                *G7C1PGM 
01568 ******************************************************************G7C1PGM 
01569  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7C1PGM 
01570  9810-010.                                                        G7C1PGM 
01571                                                                   G7C1PGM 
01572      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7C1PGM 
01573      MOVE 'J'   TO  HGADATE-FORM1.                                G7C1PGM 
01574      MOVE 'M'   TO  HGADATE-FORM2.                                G7C1PGM 
01575      MOVE ZEROS TO  HGADATE-RETURN                                G7C1PGM 
01576                     HGADATE-AMOUNT.                               G7C1PGM 
01577      EXEC CICS LINK PROGRAM ('HGADATES')                          G7C1PGM 
01578                     COMMAREA(HGADATES-COMMAREA)                   G7C1PGM 
01579                     LENGTH  (24)                                  G7C1PGM 
01580                     END-EXEC.                                     G7C1PGM 
01581                                                                   G7C1PGM 
01582  9810-900-900-EXIT.                                               G7C1PGM 
01583      EXIT.                                                        G7C1PGM 
01584 /***************************************************************  G7C1PGM 
01585 *                                                              *  G7C1PGM 
01586 * 9999  ABEND THE TASK                                         *  G7C1PGM 
01587 *                                                              *  G7C1PGM 
01588 ****************************************************************  G7C1PGM 
01589  9999-000-ABEND-THE-TASK SECTION.                                 G7C1PGM 
01590  9999-010.                                                        G7C1PGM 
01591                                                                   G7C1PGM 
01592      EXEC CICS  ABEND                                             G7C1PGM 
01593                 ABCODE(WS-01-ABCODE)                              G7C1PGM 
01594                 END-EXEC.                                         G7C1PGM 
01595                                                                   G7C1PGM 
01596  9900-900-EXIT.                                                   G7C1PGM 
01597      EXIT.                                                        G7C1PGM 
