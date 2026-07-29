00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7E1PGM.                                         G7E1PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7E1PGM 
00005  DATE-WRITTEN.   03/13/87.                                        G7E1PGM 
00006  DATE-COMPILED.                                                   G7E1PGM 
00007 ***************************************************************** G7E1PGM 
00008 *                                                               * G7E1PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7E1PGM 
00010 *                                                               * G7E1PGM 
00011 *                                                               * G7E1PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7E1PGM 
00013 *                                                               * G7E1PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7E1PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7E1PGM 
00016 *                             (SAME AS 'GC5A')                  * G7E1PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7E1PGM 
00018 *                              'GC5A')                          * G7E1PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7E1PGM 
00020 *                              PROVISION)                       * G7E1PGM 
00021 *                                                               * G7E1PGM 
00022 *  D116       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7E1PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7E1PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7E1PGM 
00025 *                                                               * G7E1PGM 
00026 *  M216       1/07/88  FCG    ADDED LOGIC FOR EXCEPTION SCHEDULE* G7E1PGM 
00027 *                             ID TO USE FIELD VALIDATION SUB-   * G7E1PGM 
00028 *                             SYSTEM.                           * G7E1PGM 
00029 *  P????      4/12/88  ENW    FIXED LOGICAL EDIT FOR TREATMENT  * G7E1PGM 
00030 *                             TIME IND AND TIME FACTOR. WAS     * G7E1PGM 
00031 *                             SKIPPING NUMERIC CHECK WITH       * G7E1PGM 
00032 *                             > ZEROS CHECK.                    * G7E1PGM 
00033 *                                                               * G7E1PGM 
00034 *  D129      08/28/89  GDM    CONVERT FOR DECIMALS.             * G7E1PGM 
00035 *                                                               * G7E1PGM 
00036 *  D129      09/08/89  GDM    CONVERT TO VS COBOL/2             * G7E1PGM 
00037 *                                                               * G7E1PGM 
00038 *  D12009    08/23/91  BSO   -CORRECT ERR MESSAGES IN AREA \
00039 *                            -CORRECT ALPHA CLASS TEST AREA     * G7E1PGM 
00040 *                                                               * G7E1PGM 
00041 *  XXXXX     09/10/91  ENW    CORRECTED BENEFIT-SCOPE-IND EDIT  * G7E1PGM 
00042 *                                                               * G7E1PGM 
00043 *  D14726    11/06/97  GDM 1. ADDED MILLENNIUM PROCESSING FOR   * G7E1PGM 
00044 *                             DATE                              * G7E1PGM 
00045 *                          2. EXPAND THE COMMAREA KEY TO        * G7E1PGM 
00046 *                             SUPPORT THE TEXAS MERGER.         * G7E1PGM 
00047 *                                                               * G7E1PGM 
00048 * 14726/     03/27/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7E1PGM 
00049 * 15057                     INCREASED GROUP AND SECTION ON      * G7E1PGM 
00050 *                           THE SCREEN.                         * G7E1PGM 
00051 *                                                               * G7E1PGM 
00052 *            12/11/02  AKK   OPID COMPILE                       * G7E1PGM 
00053 *                                                               * G7E1PGM 
00054 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7E1PGM 
00055 *                           G7E1SET                              *G7E1PGM 
00056 ***************************************************************** G7E1PGM 
00057                                                                   G7E1PGM 
00058 ***************************************************************** G7E1PGM 
00059 *                                                               * G7E1PGM 
00060 *    G7E1PGM  - PROGRAM 1 OF 2 PROGRAMS TO UPDATE THE FORMAT 'E'* G7E1PGM 
00061 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7E1PGM 
00062 *                                                               * G7E1PGM 
00063 *    TRANSID: G7E1                                              * G7E1PGM 
00064 *    MAPSET:  G7E1SETC    (GIE1PGM WHICH SHARES THIS MAP)       * G7E1PGM 
00065 *    VALGEN:  NONE                                              * G7E1PGM 
00066 *                                                               * G7E1PGM 
00067 *    PROGRAM NARRATIVE:                                         * G7E1PGM 
00068 *                                                               * G7E1PGM 
00069 *        PROGRAM CHECKS FOR TRANS CODE 'G7E1'.  AN INVALID      * G7E1PGM 
00070 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7E1PGM 
00071 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7E1PGM 
00072 *                                                               * G7E1PGM 
00073 *        THE MAIN FUNCTIONS ARE :                               * G7E1PGM 
00074 *        1. HARDCOPY REQUEST,                                   * G7E1PGM 
00075 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7E1PGM 
00076 *           USER,                                               * G7E1PGM 
00077 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7E1PGM 
00078 *                                                               * G7E1PGM 
00079 *        HARDCOPY REQUEST                                       * G7E1PGM 
00080 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7E1PGM 
00081 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7E1PGM 
00082 *           THE SCREEN BUFFER.                                  * G7E1PGM 
00083 *                                                               * G7E1PGM 
00084 *        PROCESS INPUT DATA (UPDATE).                           * G7E1PGM 
00085 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7E1PGM 
00086 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7E1PGM 
00087 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7E1PGM 
00088 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7E1PGM 
00089 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7E1PGM 
00090 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7E1PGM 
00091 *                                                               * G7E1PGM 
00092 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7E1PGM 
00093 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7E1PGM 
00094 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7E1PGM 
00095 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7E1PGM 
00096 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7E1PGM 
00097 *                                                               * G7E1PGM 
00098 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7E1PGM 
00099 *                                     -----------------------   * G7E1PGM 
00100 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7E1PGM 
00101 *           -----------------------------------------------     * G7E1PGM 
00102 *                                                               * G7E1PGM 
00103 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7E1PGM 
00104 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7E1PGM 
00105 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7E1PGM 
00106 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7E1PGM 
00107 *           SENT BACK TO THE USER.                              * G7E1PGM 
00108 *                                                               * G7E1PGM 
00109 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7E1PGM 
00110 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7E1PGM 
00111 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7E1PGM 
00112 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7E1PGM 
00113 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7E1PGM 
00114 *           PF KEY LOGIC,                                       * G7E1PGM 
00115 *              PF6/PF18       GOES TO GC8APGM                   * G7E1PGM 
00116 *              PF8/PF20/ENTER GOES TO G7E2PGM                   * G7E1PGM 
00117 *              FOR PF7/PF19   GOES TO GC6CPGM                   * G7E1PGM 
00118 *                                                               * G7E1PGM 
00119 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7E1PGM 
00120 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7E1PGM 
00121 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7E1PGM 
00122 *                                                               * G7E1PGM 
00123 ***************************************************************** G7E1PGM 
00124                                                                   G7E1PGM 
00125  ENVIRONMENT DIVISION.                                            G7E1PGM 
00126  DATA DIVISION.                                                   G7E1PGM 
00127 /                                                                 G7E1PGM 
00128  WORKING-STORAGE SECTION.                                         G7E1PGM 
00129  01  WS-BEGIN                    PIC X(58) VALUE                  G7E1PGM 
00130      '*** G7E1PGM  WORKING-STORAGE BEGINS HERE ***'.              G7E1PGM 
00131                                                                   G7E1PGM 
00132                                                                   G7E1PGM 
00133  01  WS-01-ABEND-AREA.                                            G7E1PGM 
00134      05  FILLER                   PIC X(16)  VALUE                G7E1PGM 
00135          '** ABEND AREA **'.                                      G7E1PGM 
00136                                                                   G7E1PGM 
00137      05  WS-01-ABEND-CODES-AND-MSG.                               G7E1PGM 
00138          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7E1PGM 
00139          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7E1PGM 
00140                                                                   G7E1PGM 
00141          10  WS-01-ABCODE-E1F1          PIC X(04)  VALUE  'E1F1'. G7E1PGM 
00142          10  WS-01-ABCODE-E1F1-MSG      PIC X(44)  VALUE          G7E1PGM 
00143             'W/F CONTRACT CANNOT BE FOUND             '.          G7E1PGM 
00144                                                                   G7E1PGM 
00145          10  WS-01-ABCODE-E1F2          PIC X(04)  VALUE  'E1F2'. G7E1PGM 
00146          10  WS-01-ABCODE-E1F2-MSG      PIC X(44)  VALUE          G7E1PGM 
00147             'W/F BEN PROV CANNOT BE FOUND             '.          G7E1PGM 
00148                                                                   G7E1PGM 
00149          10  WS-01-ABCODE-E1F3          PIC X(04)  VALUE  'E1F3'. G7E1PGM 
00150          10  WS-01-ABCODE-E1F3-MSG      PIC X(44)  VALUE          G7E1PGM 
00151             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7E1PGM 
00152                                                                   G7E1PGM 
00153          10  WS-01-ABCODE-E1F4          PIC X(04)  VALUE  'E1F4'. G7E1PGM 
00154          10  WS-01-ABCODE-E1F4-MSG      PIC X(44)  VALUE          G7E1PGM 
00155             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7E1PGM 
00156                                                                   G7E1PGM 
00157          10  WS-01-ABCODE-E1L1          PIC X(04)  VALUE  'E1L1'. G7E1PGM 
00158          10  WS-01-ABCODE-E1L1-MSG      PIC X(44)  VALUE          G7E1PGM 
00159             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7E1PGM 
00160                                                                   G7E1PGM 
00161          10  WS-01-ABCODE-E1P1          PIC X(04)  VALUE  'E1P1'. G7E1PGM 
00162          10  WS-01-ABCODE-E1P1-MSG      PIC X(44)  VALUE          G7E1PGM 
00163             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7E1PGM 
00164                                                                   G7E1PGM 
00165          10  WS-01-ABCODE-E1P2          PIC X(04)  VALUE  'E1P2'. G7E1PGM 
00166          10  WS-01-ABCODE-E1P2-MSG      PIC X(44)  VALUE          G7E1PGM 
00167             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7E1PGM 
00168                                                                   G7E1PGM 
00169  01  WS-02-AREA.                                                  G7E1PGM 
00170      05  FILLER                   PIC X(16)  VALUE                G7E1PGM 
00171          '** WS-02-AREA **'.                                      G7E1PGM 
00172      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7E1PGM 
00173          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7E1PGM 
00174                                                    'GC6C' 'G7E2'  G7E1PGM 
00175                                                    'G7E1'.        G7E1PGM 
00176          88  WS-02-MY-EIBTRNID                     VALUE  'G7E1'. G7E1PGM 
00177                                                                   G7E1PGM 
00178      05  WS-02-COMPUTED-LENGTHS.                                  G7E1PGM 
00179          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7E1PGM 
00180          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7E1PGM 
00181          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7E1PGM 
00182                                                                   G7E1PGM 
00183      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7E1PGM 
00184                                                                   G7E1PGM 
00185      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7E1PGM 
00186                                                                   G7E1PGM 
00187      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7E1PGM 
00188      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7E1PGM 
00189          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7E1PGM 
00190                                         PIC X.                    G7E1PGM 
00191          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7E1PGM 
00192                                                    '0' THRU '9'   G7E1PGM 
00193                                                    'A' THRU 'I'   G7E1PGM 
00194                                                    'J' THRU 'R'   G7E1PGM 
00195                                                    'S' THRU 'Z'   G7E1PGM 
00196                                                    SPACE.         G7E1PGM 
00197          88  WS-02-CLASS-BLANK                     VALUES         G7E1PGM 
00198                                                    SPACE.         G7E1PGM 
00199                                                                   G7E1PGM 
00200      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7E1PGM 
00201          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7E1PGM 
00202          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7E1PGM 
00203                                                                   G7E1PGM 
00204      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7E1PGM 
00205          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7E1PGM 
00206                                                                   G7E1PGM 
00207      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7E1PGM 
00208                                                                   G7E1PGM 
00209      05  WS-02-HEX-F00000.                                        G7E1PGM 
00210          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7E1PGM 
00211          10  FILLER                     PIC  X(09) VALUE          G7E1PGM 
00212                                                    LOW-VALUES.    G7E1PGM 
00213                                                                   G7E1PGM 
00214      05  WS-02-HSP-ADM-RESTRN-DAYS-X.                             G7E1PGM 
00215          10  WS-02-HSP-ADM-RESTRN-DAYS  PIC  9(3)    VALUE ZEROS. G7E1PGM 
00216          10  WS-02-S1HADRD              REDEFINES                 G7E1PGM 
00217              WS-02-HSP-ADM-RESTRN-DAYS  PIC  X(3).                G7E1PGM 
00218                                                                   G7E1PGM 
00219      05  WS-02-BEN-MAX-VISITS-DAYS-X.                             G7E1PGM 
00220          10  WS-02-BEN-MAX-VISITS-DAYS    PIC 9(3)    VALUE ZEROS.G7E1PGM 
00221          10  WS-02-S1BMVST              REDEFINES                 G7E1PGM 
00222              WS-02-BEN-MAX-VISITS-DAYS    PIC X(3).               G7E1PGM 
00223                                                                   G7E1PGM 
00224      05  WS-02-MAX-AMT-PER-VISIT-X.                               G7E1PGM 
00225          10  WS-02-MAX-AMT-PER-VISIT      PIC 9(3)V99 VALUE ZEROS.G7E1PGM 
00226 *        10  WS-02-S1XAV                REDEFINES                 G7E1PGM 
00227 *            WS-02-MAX-AMT-PER-VISIT      PIC X(5).               G7E1PGM 
00228                                                                   G7E1PGM 
00229      05  WS-02-TREAT-TIME-FACTOR-X.                               G7E1PGM 
00230          10  WS-02-TREAT-TIME-FACTOR      PIC 9(3)    VALUE ZEROS.G7E1PGM 
00231          10  WS-02-S1TRTMF              REDEFINES                 G7E1PGM 
00232              WS-02-TREAT-TIME-FACTOR      PIC X(3).               G7E1PGM 
00233                                                                   G7E1PGM 
00234      05  WS-5POS-MAX-AMT          PIC 999V99 VALUE 999.99.        G7E1PGM 
00235      05  WS-02-DISP-5POS-DEC      PIC 999.99.                     G7E1PGM 
00236      05  WS-GPE2-MAX-AMT-PER-VISIT        PIC 9(3)V99.            G7E1PGM 
00237 /                                                                 G7E1PGM 
00238  01  WT-00-G7E1PGM-TABLES.                                        G7E1PGM 
00239      05  FILLER                   PIC X(16)  VALUE                G7E1PGM 
00240          '*G7E1PGM TABLES*'.                                      G7E1PGM 
00241                                                                   G7E1PGM 
00242  01  WT-01-TABLE.                                                 G7E1PGM 
00243      05  FILLER                  PIC X(16) VALUE                  G7E1PGM 
00244          '* WT-01-TABLE  *'.                                      G7E1PGM 
00245 ******************************************************************G7E1PGM 
00246 *    WT-01   MESSAGE TABLE                                       *G7E1PGM 
00247 ******************************************************************G7E1PGM 
00248  01  FILLER.                                                      G7E1PGM 
00249      05  WT-01-MESSAGE-VALUES.                                    G7E1PGM 
00250                                                                   G7E1PGM 
00251 *----------------------------------------------------------------*G7E1PGM 
00252          10  WT-01-ENTRY-001.                                     G7E1PGM 
00253              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00254              15  WT-01-MESSAGE-TEXT-001.                          G7E1PGM 
00255                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00256                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00257                  20  FILLER          PIC X(3)  VALUE  '001'.      G7E1PGM 
00258                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00259                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00260                      ' INVALID PFKEY SELECTION                    G7E1PGM 
00261 -                    '                         '.                 G7E1PGM 
00262              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00263                                                                   G7E1PGM 
00264 *----------------------------------------------------------------*G7E1PGM 
00265          10  WT-01-ENTRY-002.                                     G7E1PGM 
00266              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00267              15  WT-01-MESSAGE-TEXT-002.                          G7E1PGM 
00268                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00269                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00270                  20  FILLER          PIC X(3)  VALUE  '002'.      G7E1PGM 
00271                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00272                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00273                      'HOSP ADMISSION RESTRICTION DAYS REQUIRED WHEG7E1PGM 
00274 -                    'N INDICATOR IS CODED     '.                 G7E1PGM 
00275              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00276                                                                   G7E1PGM 
00277 *----------------------------------------------------------------*G7E1PGM 
00278          10  WT-01-ENTRY-003.                                     G7E1PGM 
00279              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00280              15  WT-01-MESSAGE-TEXT-003.                          G7E1PGM 
00281                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00282                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00283                  20  FILLER          PIC X(3)  VALUE  '003'.      G7E1PGM 
00284                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00285                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00286                      'INDICATOR REQUIRED WHEN HOSPITAL ADMISSION RG7E1PGM 
00287 -                    'ESTRICTION DAYS CODED    '.                 G7E1PGM 
00288              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00289                                                                   G7E1PGM 
00290 *----------------------------------------------------------------*G7E1PGM 
00291          10  WT-01-ENTRY-004.                                     G7E1PGM 
00292              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00293              15  WT-01-MESSAGE-TEXT-004.                          G7E1PGM 
00294                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00295                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00296                  20  FILLER          PIC X(3)  VALUE  '004'.      G7E1PGM 
00297                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00298                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00299                      'BENEFIT MAXIMUM VISITS REQUIRED WHEN INDICATG7E1PGM 
00300 -                    'OR IS CODED              '.                 G7E1PGM 
00301              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00302                                                                   G7E1PGM 
00303 *----------------------------------------------------------------*G7E1PGM 
00304          10  WT-01-ENTRY-005.                                     G7E1PGM 
00305              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00306              15  WT-01-MESSAGE-TEXT-005.                          G7E1PGM 
00307                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00308                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00309                  20  FILLER          PIC X(3)  VALUE  '005'.      G7E1PGM 
00310                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00311                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00312                      'INDICATOR REQUIRED WHEN BENEFIT MAXIMUM VISIG7E1PGM 
00313 -                    'TS IS CODED              '.                 G7E1PGM 
00314              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00315                                                                   G7E1PGM 
00316 *----------------------------------------------------------------*G7E1PGM 
00317          10  WT-01-ENTRY-006.                                     G7E1PGM 
00318              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00319              15  WT-01-MESSAGE-TEXT-006.                          G7E1PGM 
00320                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00321                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00322                  20  FILLER          PIC X(3)  VALUE  '006'.      G7E1PGM 
00323                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00324                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00325                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7E1PGM 
00326 -                    ' PF4/PF16 TO CONTINUE    '.                 G7E1PGM 
00327              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00328                                                                   G7E1PGM 
00329 *----------------------------------------------------------------*G7E1PGM 
00330          10  WT-01-ENTRY-007.                                     G7E1PGM 
00331              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00332              15  WT-01-MESSAGE-TEXT-007.                          G7E1PGM 
00333                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00334                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00335                  20  FILLER          PIC X(3)  VALUE  '007'.      G7E1PGM 
00336                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00337                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00338                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7E1PGM 
00339 -                    'E CALL SYSTEMS           '.                 G7E1PGM 
00340              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00341                                                                   G7E1PGM 
00342 *----------------------------------------------------------------*G7E1PGM 
00343          10  WT-01-ENTRY-008.                                     G7E1PGM 
00344              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00345              15  WT-01-MESSAGE-TEXT-008.                          G7E1PGM 
00346                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00347                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00348                  20  FILLER          PIC X(3)  VALUE  '008'.      G7E1PGM 
00349                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00350                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00351                      'FIELD HAS AN INVALID VALUE                  G7E1PGM 
00352 -                    '                         '.                 G7E1PGM 
00353              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00354                                                                   G7E1PGM 
00355 *----------------------------------------------------------------*G7E1PGM 
00356          10  WT-01-ENTRY-009.                                     G7E1PGM 
00357              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00358              15  WT-01-MESSAGE-TEXT-009.                          G7E1PGM 
00359                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00360                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00361                  20  FILLER          PIC X(3)  VALUE  '009'.      G7E1PGM 
00362                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00363                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00364                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7E1PGM 
00365 -                    'YSTEM)                   '.                 G7E1PGM 
00366              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00367                                                                   G7E1PGM 
00368 *----------------------------------------------------------------*G7E1PGM 
00369          10  WT-01-ENTRY-010.                                     G7E1PGM 
00370              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00371              15  WT-01-MESSAGE-TEXT-010.                          G7E1PGM 
00372                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00373                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00374                  20  FILLER          PIC X(3)  VALUE  '010'.      G7E1PGM 
00375                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00376                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00377                      'TREATMENT TIME FACTOR REQUIRED WHEN INDICATOG7E1PGM 
00378 -                    'R IS CODED               '.                 G7E1PGM 
00379              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00380                                                                   G7E1PGM 
00381 *----------------------------------------------------------------*G7E1PGM 
00382          10  WT-01-ENTRY-011.                                     G7E1PGM 
00383              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00384              15  WT-01-MESSAGE-TEXT-011.                          G7E1PGM 
00385                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00386                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00387                  20  FILLER          PIC X(3)  VALUE  '011'.      G7E1PGM 
00388                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00389                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00390                      'INDICATOR REQUIRED WHEN TREATMENT TIME FACTOG7E1PGM 
00391 -                    'R IS CODED               '.                 G7E1PGM 
00392              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00393                                                                   G7E1PGM 
00394 *----------------------------------------------------------------*G7E1PGM 
00395          10  WT-01-ENTRY-012.                                     G7E1PGM 
00396              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00397              15  WT-01-MESSAGE-TEXT-012.                          G7E1PGM 
00398                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00399                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00400                  20  FILLER          PIC X(3)  VALUE  '012'.      G7E1PGM 
00401                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00402                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00403                      'FIELD EXCEEDS LENGTH OF 5 POSITIONS. FORMAT G7E1PGM 
00404 -                    'IS 999.99                '.                 G7E1PGM 
00405              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00406                                                                   G7E1PGM 
00407 *----------------------------------------------------------------*G7E1PGM 
00408          10  WT-01-ENTRY-013.                                     G7E1PGM 
00409              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00410              15  WT-01-MESSAGE-TEXT-013.                          G7E1PGM 
00411                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00412                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00413                  20  FILLER          PIC X(3)  VALUE  '013'.      G7E1PGM 
00414                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00415                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00416                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7E1PGM 
00417 -                    '                         '.                 G7E1PGM 
00418              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00419                                                                   G7E1PGM 
00420 *----------------------------------------------------------------*G7E1PGM 
00421          10  WT-01-ENTRY-014.                                     G7E1PGM 
00422              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00423              15  WT-01-MESSAGE-TEXT-014.                          G7E1PGM 
00424                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00425                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00426                  20  FILLER          PIC X(3)  VALUE  '014'.      G7E1PGM 
00427                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00428                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00429                      ' INVALID DECIMAL DETECTED'.                 G7E1PGM 
00430              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00431                                                                   G7E1PGM 
00432 *----------------------------------------------------------------*G7E1PGM 
00433          10  WT-01-ENTRY-015.                                     G7E1PGM 
00434              15  FILLER              PIC X(2)  VALUE '¬>'.        G7E1PGM 
00435              15  WT-01-MESSAGE-TEXT-015.                          G7E1PGM 
00436                  20  FILLER          PIC X(4)  VALUE  'G7E1'.     G7E1PGM 
00437                  20  FILLER          PIC X(1)  VALUE  '-'.        G7E1PGM 
00438                  20  FILLER          PIC X(3)  VALUE  '015'.      G7E1PGM 
00439                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7E1PGM 
00440                  20  FILLER          PIC X(70) VALUE              G7E1PGM 
00441                      'FILL THE FIRST TWO POSITIONS AND SPACE OUT TG7E1PGM 
00442 -                    'HE SECOND TWO.           '.                 G7E1PGM 
00443              15  FILLER              PIC X(2)  VALUE '<¬'.        G7E1PGM 
00444                                                                   G7E1PGM 
00445 *----------------------------------------------------------------*G7E1PGM 
00446                                                                   G7E1PGM 
00447      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7E1PGM 
00448          WT-01-MESSAGE-VALUES         OCCURS 015 TIMES            G7E1PGM 
00449                                      INDEXED BY WT-01-INDEX.      G7E1PGM 
00450          10  WT-01-ENTRY.                                         G7E1PGM 
00451              15  FILLER              PIC X(02).                   G7E1PGM 
00452              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7E1PGM 
00453              15  FILLER              PIC X(02).                   G7E1PGM 
00454                                                                   G7E1PGM 
00455                                                                   G7E1PGM 
00456 /*** MAP FIELD ATTRIBUTES                                         G7E1PGM 
00457  COPY DFHBMSCA.                                                   G7E1PGM 
00458 *                         AUTOSKIP, BRIGHT, FSET                  G7E1PGM 
00459      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7E1PGM 
00460                                                                   G7E1PGM 
00461 /*** ATTENTION KEYS                                               G7E1PGM 
00462  COPY DFHAID.                                                     G7E1PGM 
00463                                                                   G7E1PGM 
00464 /***  PROVISION MAINTENANCE SCREEN                                G7E1PGM 
00465  COPY  G7E1SETC.                                                  G7E1PGM 
00466                                                                   G7E1PGM 
00467 /*** DATE ROUTINE COMMAREA                                        G7E1PGM 
00468  01  HGADATES-COMMAREA.                                           G7E1PGM 
00469  COPY HGCDAT01.                                                   G7E1PGM 
00470                                                                   G7E1PGM 
00471 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7E1PGM 
00472  01  GCVIOPGM-PARM-LIST.                                          G7E1PGM 
00473  COPY GCVINTRC.                                                   G7E1PGM 
00474                                                                   G7E1PGM 
00475 /*** DECIMAL CONVERSION COMMAREA                                  G7E1PGM 
00476  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7E1PGM 
00477  COPY GCDCCA01.                                                   G7E1PGM 
00478                                                                   G7E1PGM 
00479 /*** ALTERNATIVE WORKFILE KEYS                                    G7E1PGM 
00480  01  FILLER.                                                      G7E1PGM 
00481      COPY GCWRKKEY.                                               G7E1PGM 
00482                                                                   G7E1PGM 
00483 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7E1PGM 
00484  01  FILLER.                                                      G7E1PGM 
00485      COPY GCCDRLEN.                                               G7E1PGM 
00486                                                                   G7E1PGM 
00487                                                                   G7E1PGM 
00488  01  WS-END                       PIC X(58) VALUE                 G7E1PGM 
00489      '*** G7E1PGM  WORKING-STORAGE ENDS HERE ***'.                G7E1PGM 
00490 /                                                                 G7E1PGM 
00491  LINKAGE SECTION.                                                 G7E1PGM 
00492 /                                                                 G7E1PGM 
00493  01  DFHCOMMAREA.                                                 G7E1PGM 
00494      COPY  GCWRKDCC.                                              G7E1PGM 
00495      COPY  GCBENPVC.                                              G7E1PGM 
00496 /                                                                 G7E1PGM 
00497 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7E1PGM 
00498  01  IO-PARM-BEN-PROV-AREA.                                       G7E1PGM 
00499      COPY  GCIOPRM2.                                              G7E1PGM 
00500      COPY  GCWRKDC2.                                              G7E1PGM 
00501      COPY  GCBENPV2.                                              G7E1PGM 
00502                                                                   G7E1PGM 
00503 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7E1PGM 
00504  01  IO-PARM-CONTRACT-AREA.                                       G7E1PGM 
00505      COPY  GCIOPRM3.                                              G7E1PGM 
00506      COPY  GCWRKDC3.                                              G7E1PGM 
00507      COPY  GCCONTR2.                                              G7E1PGM 
00508 /                                                                 G7E1PGM 
00509  PROCEDURE DIVISION.                                              G7E1PGM 
00510                                                                   G7E1PGM 
00511 ****************************************************************  G7E1PGM 
00512 *                                                              *  G7E1PGM 
00513 *           P R O C E S S     C O N T R O L                    *  G7E1PGM 
00514 *                                                              *  G7E1PGM 
00515 ****************************************************************  G7E1PGM 
00516  0000-000-PROCESS-CONTROL       SECTION.                          G7E1PGM 
00517  0000-010.                                                        G7E1PGM 
00518                                                                   G7E1PGM 
00519      IF  EIBAID  =  DFHCLEAR                                      G7E1PGM 
00520          EXEC CICS  RETURN                                        G7E1PGM 
00521                     END-EXEC.                                     G7E1PGM 
00522                                                                   G7E1PGM 
00523      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7E1PGM 
00524                                                                   G7E1PGM 
00525      IF  WS-02-MY-EIBTRNID                                        G7E1PGM 
00526      THEN                                                         G7E1PGM 
00527          PERFORM  2000-000-PROCESS-INPUT                          G7E1PGM 
00528      ELSE                                                         G7E1PGM 
00529          PERFORM  1000-000-DISPLAY-SCREEN.                        G7E1PGM 
00530                                                                   G7E1PGM 
00531                                                                   G7E1PGM 
00532 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7E1PGM 
00533                                                                   G7E1PGM 
00534      MOVE WS-01-ABCODE-E1L1     TO WS-01-ABCODE                   G7E1PGM 
00535      MOVE WS-01-ABCODE-E1L1-MSG TO WS-01-ABCODE-MSG               G7E1PGM 
00536      PERFORM  9999-000-ABEND-THE-TASK.                            G7E1PGM 
00537                                                                   G7E1PGM 
00538      GOBACK.                                                      G7E1PGM 
00539                                                                   G7E1PGM 
00540                                                                   G7E1PGM 
00541  0000-900-EXIT.                                                   G7E1PGM 
00542      EXIT.                                                        G7E1PGM 
00543 /***************************************************************  G7E1PGM 
00544 *                                                              *  G7E1PGM 
00545 * 1000  DISPLAY INITIAL SCREEN                                 *  G7E1PGM 
00546 *                                                              *  G7E1PGM 
00547 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7E1PGM 
00548 *                                                              *  G7E1PGM 
00549 ****************************************************************  G7E1PGM 
00550  1000-000-DISPLAY-SCREEN        SECTION.                          G7E1PGM 
00551  1000-010.                                                        G7E1PGM 
00552                                                                   G7E1PGM 
00553 *------- MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY              G7E1PGM 
00554 *                                                                 G7E1PGM 
00555      MOVE LOW-VALUES TO G7E1I01I.                                 G7E1PGM 
00556                                                                   G7E1PGM 
00557 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7E1PGM 
00558                                                                   G7E1PGM 
00559      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7E1PGM 
00560          MOVE WS-01-ABCODE-E1P1     TO WS-01-ABCODE               G7E1PGM 
00561          MOVE WS-01-ABCODE-E1P1-MSG TO WS-01-ABCODE-MSG           G7E1PGM 
00562          PERFORM 9999-000-ABEND-THE-TASK.                         G7E1PGM 
00563                                                                   G7E1PGM 
00564                                                                   G7E1PGM 
00565 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7E1PGM 
00566                                                                   G7E1PGM 
00567      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7E1PGM 
00568                                         + GC-GCBENPRV-FIXED-LEN   G7E1PGM 
00569                                         + GC-GCBENPRV-VARY-LEN.   G7E1PGM 
00570                                                                   G7E1PGM 
00571                                                                   G7E1PGM 
00572 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7E1PGM 
00573                                                                   G7E1PGM 
00574      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7E1PGM 
00575          MOVE WS-01-ABCODE-E1P2     TO WS-01-ABCODE               G7E1PGM 
00576          MOVE WS-01-ABCODE-E1P2-MSG TO WS-01-ABCODE-MSG           G7E1PGM 
00577          PERFORM 9999-000-ABEND-THE-TASK.                         G7E1PGM 
00578                                                                   G7E1PGM 
00579                                                                   G7E1PGM 
00580 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7E1PGM 
00581 *          BY CALLER IN COMMAREA.                                 G7E1PGM 
00582                                                                   G7E1PGM 
00583      MOVE WRK-PLAN-CODE                      TO S1PLNCDO.         G7E1PGM 
00584      MOVE WRK-GROUP-NUM                      TO S1GRPNOO.         G7E1PGM 
00585      MOVE WRK-SECTION-NUM                    TO S1SECNOO.         G7E1PGM 
00586      MOVE WRK-PKG-CODE                       TO S1PKGCDO.         G7E1PGM 
00587      MOVE WRK-PROV-CTL                       TO S1PRVO.           G7E1PGM 
00588      MOVE WRK-FAM-REL-LEVEL                  TO S1FRLO.           G7E1PGM 
00589      MOVE WRK-L-O-B                          TO S1LOBO.           G7E1PGM 
00590                                                                   G7E1PGM 
00591      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7E1PGM 
00592      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7E1PGM 
00593      IF  HGADATE-RETURN = ZEROS                                   G7E1PGM 
00594      THEN                                                         G7E1PGM 
00595          MOVE DFHBMASF                       TO S1EFFDTA          G7E1PGM 
00596          MOVE HGADATE-DATE2                  TO S1EFFDTO          G7E1PGM 
00597      ELSE                                                         G7E1PGM 
00598          MOVE DFHBMABF                       TO S1EFFDTA          G7E1PGM 
00599          MOVE HGADATE-JULIAN1                TO S1EFFDTO.         G7E1PGM 
00600                                                                   G7E1PGM 
00601      MOVE GCP-PROVN-ID                       TO S1BPVIDO.         G7E1PGM 
00602                                                                   G7E1PGM 
00603      MOVE GPE-BEN-SCOPE-ID                   TO S1BESCIO.         G7E1PGM 
00604      MOVE GPE-EXCP-SCHED-ID                  TO S1EXCSCO.         G7E1PGM 
00605      MOVE GPE-HOSP-ADM-RESTRN-IND            TO S1HADMRO.         G7E1PGM 
00606                                                                   G7E1PGM 
00607      IF  GPE-HSP-ADM-RESTRN-DAYS = ZEROS                          G7E1PGM 
00608          MOVE WS-02-HEX-F00000               TO S1HADRDO          G7E1PGM 
00609      ELSE                                                         G7E1PGM 
00610          MOVE   GPE-HSP-ADM-RESTRN-DAYS      TO                   G7E1PGM 
00611               WS-02-HSP-ADM-RESTRN-DAYS                           G7E1PGM 
00612          MOVE WS-02-HSP-ADM-RESTRN-DAYS-X    TO S1HADRDO.         G7E1PGM 
00613                                                                   G7E1PGM 
00614      MOVE  GPE-BEN-MAX-VISITS-IND            TO S1BMVSIO.         G7E1PGM 
00615                                                                   G7E1PGM 
00616      IF  GPE-BEN-MAX-VISITS-DAYS = ZEROS                          G7E1PGM 
00617          MOVE WS-02-HEX-F00000               TO S1BMVSTO          G7E1PGM 
00618      ELSE                                                         G7E1PGM 
00619          MOVE   GPE-BEN-MAX-VISITS-DAYS      TO                   G7E1PGM 
00620               WS-02-BEN-MAX-VISITS-DAYS                           G7E1PGM 
00621          MOVE WS-02-BEN-MAX-VISITS-DAYS-X    TO S1BMVSTO.         G7E1PGM 
00622                                                                   G7E1PGM 
00623      MOVE  GPE-CORRIDOR-OVERRIDE             TO S1CORORO.         G7E1PGM 
00624                                                                   G7E1PGM 
00625 *    IF  GPE-MAX-AMT-PER-VISIT = ZEROS                            G7E1PGM 
00626 *        MOVE WS-02-HEX-F00000               TO S1XAVO            G7E1PGM 
00627 *    ELSE                                                         G7E1PGM 
00628 *        MOVE   GPE-MAX-AMT-PER-VISIT        TO                   G7E1PGM 
00629 *             WS-02-MAX-AMT-PER-VISIT                             G7E1PGM 
00630 *        MOVE WS-02-MAX-AMT-PER-VISIT-X      TO S1XAVO.           G7E1PGM 
00631                                                                   G7E1PGM 
00632 *    D129          CONVERSION FOR DECIMALS.                       G7E1PGM 
00633 *                                                                 G7E1PGM 
00634          MOVE   GPE-MAX-AMT-PER-VISIT        TO                   G7E1PGM 
00635               WS-02-MAX-AMT-PER-VISIT.                            G7E1PGM 
00636          MOVE WS-02-MAX-AMT-PER-VISIT        TO                   G7E1PGM 
00637               WS-02-DISP-5POS-DEC.                                G7E1PGM 
00638          MOVE WS-02-DISP-5POS-DEC            TO S1XAVO.           G7E1PGM 
00639                                                                   G7E1PGM 
00640      MOVE GPE-TREAT-TIME-FACTOR-IND          TO S1TRTMIO.         G7E1PGM 
00641                                                                   G7E1PGM 
00642      IF  GPE-TREAT-TIME-FACTOR = ZEROS                            G7E1PGM 
00643          MOVE WS-02-HEX-F00000               TO S1TRTMFO          G7E1PGM 
00644      ELSE                                                         G7E1PGM 
00645          MOVE   GPE-TREAT-TIME-FACTOR        TO                   G7E1PGM 
00646               WS-02-TREAT-TIME-FACTOR                             G7E1PGM 
00647          MOVE WS-02-TREAT-TIME-FACTOR-X      TO S1TRTMFO.         G7E1PGM 
00648                                                                   G7E1PGM 
00649                                                                   G7E1PGM 
00650                                                                   G7E1PGM 
00651 *------- SEND INITIAL SCREEN ------------------------------------*G7E1PGM 
00652                                                                   G7E1PGM 
00653      MOVE  -1 TO  S1BESCIL.                                       G7E1PGM 
00654      PERFORM 9100-000-SEND-THEN-RETURN.                           G7E1PGM 
00655                                                                   G7E1PGM 
00656                                                                   G7E1PGM 
00657  1000-900-EXIT.                                                   G7E1PGM 
00658      EXIT.                                                        G7E1PGM 
00659 /***************************************************************  G7E1PGM 
00660 *                                                              *  G7E1PGM 
00661 * 2000    P R O C E S S    I N P U T                           *  G7E1PGM 
00662 *                                                              *  G7E1PGM 
00663 ****************************************************************  G7E1PGM 
00664  2000-000-PROCESS-INPUT         SECTION.                          G7E1PGM 
00665  2000-010.                                                        G7E1PGM 
00666                                                                   G7E1PGM 
00667 *------ VALIDATE PFKEY USAGE ------------------------------------*G7E1PGM 
00668                                                                   G7E1PGM 
00669      IF  EIBAID = DFHENTER OR                                     G7E1PGM 
00670                   DFHPF3   OR  DFHPF15 OR                         G7E1PGM 
00671                   DFHPF4   OR  DFHPF16 OR                         G7E1PGM 
00672                   DFHPF6   OR  DFHPF18 OR                         G7E1PGM 
00673                   DFHPF7   OR  DFHPF19 OR                         G7E1PGM 
00674                   DFHPF8   OR  DFHPF20                            G7E1PGM 
00675      THEN                                                         G7E1PGM 
00676          NEXT SENTENCE                                            G7E1PGM 
00677      ELSE                                                         G7E1PGM 
00678          SET WT-01-INDEX TO +01                                   G7E1PGM 
00679          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7E1PGM 
00680          PERFORM 9100-000-SEND-THEN-RETURN.                       G7E1PGM 
00681                                                                   G7E1PGM 
00682                                                                   G7E1PGM 
00683                                                                   G7E1PGM 
00684      EXEC CICS  HANDLE CONDITION                                  G7E1PGM 
00685                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7E1PGM 
00686                        END-EXEC.                                  G7E1PGM 
00687                                                                   G7E1PGM 
00688                                                                   G7E1PGM 
00689      EXEC CICS  RECEIVE MAP   ('G7E1I01')                         G7E1PGM 
00690                         MAPSET('G7E1SET')                         G7E1PGM 
00691                         END-EXEC.                                 G7E1PGM 
00692                                                                   G7E1PGM 
00693                                                                   G7E1PGM 
00694      IF  S1FUNCI  NOT = 'G7E1'  OR                                G7E1PGM 
00695          S1SCRNI  NOT = '007E01'                                  G7E1PGM 
00696          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7E1PGM 
00697                                                                   G7E1PGM 
00698                                                                   G7E1PGM 
00699 *--- RETURN TO GCPS MENU? ---------------------------------------*G7E1PGM 
00700                                                                   G7E1PGM 
00701      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7E1PGM 
00702          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7E1PGM 
00703                                                                   G7E1PGM 
00704 *--- PROCESS SCREEN FIELDS --------------------------------------*G7E1PGM 
00705                                                                   G7E1PGM 
00706      PERFORM 2100-000-FIELD-EDITS.                                G7E1PGM 
00707                                                                   G7E1PGM 
00708      IF  WS-02-SCREEN-HAS-ERRORS                                  G7E1PGM 
00709          PERFORM 9100-000-SEND-THEN-RETURN.                       G7E1PGM 
00710                                                                   G7E1PGM 
00711      PERFORM 2200-000-LOGICAL-EDITS.                              G7E1PGM 
00712                                                                   G7E1PGM 
00713      IF  WS-02-SCREEN-HAS-ERRORS                                  G7E1PGM 
00714          PERFORM 9100-000-SEND-THEN-RETURN.                       G7E1PGM 
00715                                                                   G7E1PGM 
00716      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7E1PGM 
00717                                                                   G7E1PGM 
00718      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7E1PGM 
00719                                                                   G7E1PGM 
00720                                                                   G7E1PGM 
00721  2000-900-EXIT.                                                   G7E1PGM 
00722      EXIT.                                                        G7E1PGM 
00723 /***************************************************************  G7E1PGM 
00724 *                                                              *  G7E1PGM 
00725 * 2100  DO SCREEN FIELD EDITS                                  *  G7E1PGM 
00726 *                                                              *  G7E1PGM 
00727 ****************************************************************  G7E1PGM 
00728  2100-000-FIELD-EDITS           SECTION.                          G7E1PGM 
00729  2100-010.                                                        G7E1PGM 
00730                                                                   G7E1PGM 
00731 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7E1PGM 
00732                                                                   G7E1PGM 
00733      MOVE DFHBMUNF TO  S1BESCIA                                   G7E1PGM 
00734                        S1EXCSCA                                   G7E1PGM 
00735                        S1HADMRA                                   G7E1PGM 
00736                        S1HADRDA                                   G7E1PGM 
00737                        S1BMVSIA                                   G7E1PGM 
00738                        S1BMVSTA                                   G7E1PGM 
00739                        S1XAVA                                     G7E1PGM 
00740                        S1CORORA                                   G7E1PGM 
00741                        S1TRTMIA                                   G7E1PGM 
00742                        S1TRTMFA.                                  G7E1PGM 
00743                                                                   G7E1PGM 
00744      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7E1PGM 
00745                                                                   G7E1PGM 
00746                                                                   G7E1PGM 
00747 *-- VALIDATE ------ BENEFIT SCOPE -------------------------------*G7E1PGM 
00748 *   1. ALPHANUMERIC                                               G7E1PGM 
00749 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E1PGM 
00750                                                                   G7E1PGM 
00751      MOVE  S1BESCII TO WS-02-CLASS-TEST-AREA.                     G7E1PGM 
00752      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7E1PGM 
00753          WS-02-CLASS-ALPHANUMERIC(2) AND                          G7E1PGM 
00754          WS-02-CLASS-BLANK       (3) AND                          G7E1PGM 
00755          WS-02-CLASS-BLANK       (4)                              G7E1PGM 
00756      THEN                                                         G7E1PGM 
00757          MOVE  S1BESCII TO GCVI-VALUE                             G7E1PGM 
00758          MOVE  'BPCA01' TO GCVI-FIELDS-KEY-ID                     G7E1PGM 
00759          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E1PGM 
00760          IF  GCVI-VALUE-NOT-FOUND                                 G7E1PGM 
00761          THEN                                                     G7E1PGM 
00762              MOVE  -1        TO  S1BESCIL                         G7E1PGM 
00763              MOVE  DFHBMUBF  TO  S1BESCIA                         G7E1PGM 
00764              IF  WS-02-SCREEN-HAS-ERRORS                          G7E1PGM 
00765              THEN                                                 G7E1PGM 
00766                  NEXT SENTENCE                                    G7E1PGM 
00767              ELSE                                                 G7E1PGM 
00768                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
00769                  SET WT-01-INDEX TO +09                           G7E1PGM 
00770                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
00771          ELSE                                                     G7E1PGM 
00772              IF  GCVI-VALUE-NOT-LOADED                            G7E1PGM 
00773              THEN                                                 G7E1PGM 
00774                  MOVE  DFHBMUBF  TO  S1BESCIA                     G7E1PGM 
00775              ELSE                                                 G7E1PGM 
00776                  NEXT SENTENCE                                    G7E1PGM 
00777      ELSE                                                         G7E1PGM 
00778          MOVE  -1        TO  S1BESCIL                             G7E1PGM 
00779          MOVE  DFHBMUBF  TO  S1BESCIA                             G7E1PGM 
00780          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00781          THEN                                                     G7E1PGM 
00782              NEXT SENTENCE                                        G7E1PGM 
00783          ELSE                                                     G7E1PGM 
00784              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00785              SET WT-01-INDEX TO +15                               G7E1PGM 
00786              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00787                                                                   G7E1PGM 
00788                                                                   G7E1PGM 
00789 *-- VALIDATE ------ EXCEPTION SCHEDULE ID -----------------------*G7E1PGM 
00790 *   1. ALPHANUMERIC                                               G7E1PGM 
00791 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E1PGM 
00792                                                                   G7E1PGM 
00793      MOVE  S1EXCSCI TO WS-02-CLASS-TEST-AREA.                     G7E1PGM 
00794      IF  WS-02-CLASS-ALPHANUMERIC(1) AND                          G7E1PGM 
00795          WS-02-CLASS-ALPHANUMERIC(2) AND                          G7E1PGM 
00796          WS-02-CLASS-ALPHANUMERIC(3) AND                          G7E1PGM 
00797          WS-02-CLASS-ALPHANUMERIC(4)                              G7E1PGM 
00798      THEN                                                         G7E1PGM 
00799          MOVE  S1EXCSCI TO GCVI-VALUE                             G7E1PGM 
00800          MOVE  'BPCA10' TO GCVI-FIELDS-KEY-ID                     G7E1PGM 
00801          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E1PGM 
00802          IF  GCVI-VALUE-NOT-FOUND                                 G7E1PGM 
00803          THEN                                                     G7E1PGM 
00804              MOVE  -1        TO  S1EXCSCL                         G7E1PGM 
00805              MOVE  DFHBMUBF  TO  S1EXCSCA                         G7E1PGM 
00806              IF  WS-02-SCREEN-HAS-ERRORS                          G7E1PGM 
00807              THEN                                                 G7E1PGM 
00808                  NEXT SENTENCE                                    G7E1PGM 
00809              ELSE                                                 G7E1PGM 
00810                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
00811                  SET WT-01-INDEX TO +09                           G7E1PGM 
00812                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
00813          ELSE                                                     G7E1PGM 
00814              IF  GCVI-VALUE-NOT-LOADED                            G7E1PGM 
00815              THEN                                                 G7E1PGM 
00816                  MOVE  DFHBMUBF  TO  S1EXCSCA                     G7E1PGM 
00817              ELSE                                                 G7E1PGM 
00818                  NEXT SENTENCE                                    G7E1PGM 
00819      ELSE                                                         G7E1PGM 
00820          MOVE  -1        TO  S1BESCIL                             G7E1PGM 
00821          MOVE  DFHBMUBF  TO  S1EXCSCA                             G7E1PGM 
00822          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00823          THEN                                                     G7E1PGM 
00824              NEXT SENTENCE                                        G7E1PGM 
00825          ELSE                                                     G7E1PGM 
00826              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00827              SET WT-01-INDEX TO +08                               G7E1PGM 
00828              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00829                                                                   G7E1PGM 
00830                                                                   G7E1PGM 
00831 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION IND ----------*G7E1PGM 
00832 *   1. ALPHANUMERIC                                               G7E1PGM 
00833 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E1PGM 
00834                                                                   G7E1PGM 
00835      MOVE  S1HADMRI TO WS-02-CLASS-TEST-AREA.                     G7E1PGM 
00836      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E1PGM 
00837      THEN                                                         G7E1PGM 
00838          MOVE  S1HADMRI TO GCVI-VALUE                             G7E1PGM 
00839          MOVE  'BPAA01' TO GCVI-FIELDS-KEY-ID                     G7E1PGM 
00840          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E1PGM 
00841          IF  GCVI-VALUE-NOT-FOUND                                 G7E1PGM 
00842          THEN                                                     G7E1PGM 
00843              MOVE  -1        TO  S1HADMRL                         G7E1PGM 
00844              MOVE  DFHBMUBF  TO  S1HADMRA                         G7E1PGM 
00845              IF  WS-02-SCREEN-HAS-ERRORS                          G7E1PGM 
00846              THEN                                                 G7E1PGM 
00847                  NEXT SENTENCE                                    G7E1PGM 
00848              ELSE                                                 G7E1PGM 
00849                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
00850                  SET WT-01-INDEX TO +09                           G7E1PGM 
00851                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
00852          ELSE                                                     G7E1PGM 
00853              IF  GCVI-VALUE-NOT-LOADED                            G7E1PGM 
00854              THEN                                                 G7E1PGM 
00855                  MOVE  DFHBMUBF  TO  S1HADMRA                     G7E1PGM 
00856              ELSE                                                 G7E1PGM 
00857                  NEXT SENTENCE                                    G7E1PGM 
00858      ELSE                                                         G7E1PGM 
00859          MOVE  -1        TO  S1HADMRL                             G7E1PGM 
00860          MOVE  DFHBMUBF  TO  S1HADMRA                             G7E1PGM 
00861          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00862          THEN                                                     G7E1PGM 
00863              NEXT SENTENCE                                        G7E1PGM 
00864          ELSE                                                     G7E1PGM 
00865              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00866              SET WT-01-INDEX TO +08                               G7E1PGM 
00867              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00868                                                                   G7E1PGM 
00869                                                                   G7E1PGM 
00870                                                                   G7E1PGM 
00871 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION DAYS ---------*G7E1PGM 
00872 *   1. NUMERICS                                                   G7E1PGM 
00873                                                                   G7E1PGM 
00874      IF  S1HADRDI IS NUMERIC                                      G7E1PGM 
00875      THEN                                                         G7E1PGM 
00876          NEXT SENTENCE                                            G7E1PGM 
00877      ELSE                                                         G7E1PGM 
00878          MOVE  -1        TO  S1HADRDL                             G7E1PGM 
00879          MOVE  DFHBMUBF  TO  S1HADRDA                             G7E1PGM 
00880          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00881          THEN                                                     G7E1PGM 
00882              NEXT SENTENCE                                        G7E1PGM 
00883          ELSE                                                     G7E1PGM 
00884              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00885              SET WT-01-INDEX TO +13                               G7E1PGM 
00886              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00887                                                                   G7E1PGM 
00888                                                                   G7E1PGM 
00889 *-- VALIDATE ------ BENEFIT MAXIMUM VISIT IND -------------------*G7E1PGM 
00890 *   1. ALPHANUMERIC                                               G7E1PGM 
00891 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E1PGM 
00892                                                                   G7E1PGM 
00893      MOVE  S1BMVSII TO WS-02-CLASS-TEST-AREA.                     G7E1PGM 
00894      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E1PGM 
00895      THEN                                                         G7E1PGM 
00896          MOVE  S1BMVSII TO GCVI-VALUE                             G7E1PGM 
00897          MOVE  'BPDB01' TO GCVI-FIELDS-KEY-ID                     G7E1PGM 
00898          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E1PGM 
00899          IF  GCVI-VALUE-NOT-FOUND                                 G7E1PGM 
00900          THEN                                                     G7E1PGM 
00901              MOVE  -1        TO  S1BMVSIL                         G7E1PGM 
00902              MOVE  DFHBMUBF  TO  S1BMVSIA                         G7E1PGM 
00903              IF  WS-02-SCREEN-HAS-ERRORS                          G7E1PGM 
00904              THEN                                                 G7E1PGM 
00905                  NEXT SENTENCE                                    G7E1PGM 
00906              ELSE                                                 G7E1PGM 
00907                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
00908                  SET WT-01-INDEX TO +09                           G7E1PGM 
00909                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
00910          ELSE                                                     G7E1PGM 
00911              IF  GCVI-VALUE-NOT-LOADED                            G7E1PGM 
00912              THEN                                                 G7E1PGM 
00913                  MOVE  DFHBMUBF  TO  S1BMVSIA                     G7E1PGM 
00914              ELSE                                                 G7E1PGM 
00915                  NEXT SENTENCE                                    G7E1PGM 
00916      ELSE                                                         G7E1PGM 
00917          MOVE  -1        TO  S1BMVSIL                             G7E1PGM 
00918          MOVE  DFHBMUBF  TO  S1BMVSIA                             G7E1PGM 
00919          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00920          THEN                                                     G7E1PGM 
00921              NEXT SENTENCE                                        G7E1PGM 
00922          ELSE                                                     G7E1PGM 
00923              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00924              SET WT-01-INDEX TO +08                               G7E1PGM 
00925              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00926                                                                   G7E1PGM 
00927                                                                   G7E1PGM 
00928 *-- VALIDATE ------ BENEFIT MAXIMUM VISITS ----------------------*G7E1PGM 
00929 *   1. NUMERICS                                                   G7E1PGM 
00930                                                                   G7E1PGM 
00931      IF  S1BMVSTI IS NUMERIC                                      G7E1PGM 
00932      THEN                                                         G7E1PGM 
00933          NEXT SENTENCE                                            G7E1PGM 
00934      ELSE                                                         G7E1PGM 
00935          MOVE  -1        TO  S1BMVSTL                             G7E1PGM 
00936          MOVE  DFHBMUBF  TO  S1BMVSTA                             G7E1PGM 
00937          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00938          THEN                                                     G7E1PGM 
00939              NEXT SENTENCE                                        G7E1PGM 
00940          ELSE                                                     G7E1PGM 
00941              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00942              SET WT-01-INDEX TO +13                               G7E1PGM 
00943              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00944                                                                   G7E1PGM 
00945                                                                   G7E1PGM 
00946 *-- VALIDATE ------ MAXIMUM AMOUNT PER VISIT --------------------*G7E1PGM 
00947 *   D129     DECIMAL CONVERSION                                   G7E1PGM 
00948                                                                   G7E1PGM 
00949      MOVE S1XAVI TO D-C-RECEIVE-FIELD.                            G7E1PGM 
00950      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7E1PGM 
00951      MOVE '00' TO D-C-RETURN-CODE.                                G7E1PGM 
00952      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7E1PGM 
00953      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7E1PGM 
00954      IF D-C-RETURN-CODE = '00'                                    G7E1PGM 
00955          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7E1PGM 
00956              MOVE -1       TO S1XAVL                              G7E1PGM 
00957              MOVE DFHBMUBF TO S1XAVA                              G7E1PGM 
00958              IF WS-02-SCREEN-HAS-ERRORS                           G7E1PGM 
00959                  NEXT SENTENCE                                    G7E1PGM 
00960              ELSE                                                 G7E1PGM 
00961                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7E1PGM 
00962                  SET WT-01-INDEX TO +12                           G7E1PGM 
00963                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
00964          ELSE                                                     G7E1PGM 
00965              MOVE D-C-RETURN-FIELD-DEC2                           G7E1PGM 
00966                TO WS-02-MAX-AMT-PER-VISIT                         G7E1PGM 
00967              MOVE WS-02-MAX-AMT-PER-VISIT                         G7E1PGM 
00968                TO WS-02-DISP-5POS-DEC                             G7E1PGM 
00969              MOVE WS-02-DISP-5POS-DEC                             G7E1PGM 
00970                TO S1XAVO                                          G7E1PGM 
00971      ELSE                                                         G7E1PGM 
00972          MOVE -1       TO S1XAVL                                  G7E1PGM 
00973          MOVE DFHBMUBF TO S1XAVA                                  G7E1PGM 
00974          IF WS-02-SCREEN-HAS-ERRORS                               G7E1PGM 
00975              NEXT SENTENCE                                        G7E1PGM 
00976          ELSE                                                     G7E1PGM 
00977              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7E1PGM 
00978              IF D-C-RETURN-CODE = '10'                            G7E1PGM 
00979                  SET WT-01-INDEX TO +13                           G7E1PGM 
00980                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
00981              ELSE                                                 G7E1PGM 
00982                  SET WT-01-INDEX TO +14                           G7E1PGM 
00983                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7E1PGM 
00984                                                                   G7E1PGM 
00985 *    IF  S1XAVI IS NUMERIC                                        G7E1PGM 
00986 *    THEN                                                         G7E1PGM 
00987 *        NEXT SENTENCE                                            G7E1PGM 
00988 *    ELSE                                                         G7E1PGM 
00989 *        MOVE  -1        TO  S1XAVL                               G7E1PGM 
00990 *        MOVE  DFHBMUBF  TO  S1XAVA                               G7E1PGM 
00991 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
00992 *        THEN                                                     G7E1PGM 
00993 *            NEXT SENTENCE                                        G7E1PGM 
00994 *        ELSE                                                     G7E1PGM 
00995 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
00996 *            SET WT-01-INDEX TO +13                               G7E1PGM 
00997 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
00998                                                                   G7E1PGM 
00999                                                                   G7E1PGM 
01000 *-- VALIDATE ------ CORRIDOR OVERRIDE ---------------------------*G7E1PGM 
01001 *   1. ALPHANUMERIC                                               G7E1PGM 
01002 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E1PGM 
01003                                                                   G7E1PGM 
01004      MOVE  S1CORORI TO WS-02-CLASS-TEST-AREA.                     G7E1PGM 
01005      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E1PGM 
01006      THEN                                                         G7E1PGM 
01007          MOVE  S1CORORI TO GCVI-VALUE                             G7E1PGM 
01008          MOVE  'BPCA02' TO GCVI-FIELDS-KEY-ID                     G7E1PGM 
01009          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E1PGM 
01010          IF  GCVI-VALUE-NOT-FOUND                                 G7E1PGM 
01011          THEN                                                     G7E1PGM 
01012              MOVE  -1        TO  S1CORORL                         G7E1PGM 
01013              MOVE  DFHBMUBF  TO  S1CORORA                         G7E1PGM 
01014              IF  WS-02-SCREEN-HAS-ERRORS                          G7E1PGM 
01015              THEN                                                 G7E1PGM 
01016                  NEXT SENTENCE                                    G7E1PGM 
01017              ELSE                                                 G7E1PGM 
01018                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
01019                  SET WT-01-INDEX TO +09                           G7E1PGM 
01020                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
01021          ELSE                                                     G7E1PGM 
01022              IF  GCVI-VALUE-NOT-LOADED                            G7E1PGM 
01023              THEN                                                 G7E1PGM 
01024                  MOVE  DFHBMUBF  TO  S1CORORA                     G7E1PGM 
01025              ELSE                                                 G7E1PGM 
01026                  NEXT SENTENCE                                    G7E1PGM 
01027      ELSE                                                         G7E1PGM 
01028          MOVE  -1        TO  S1CORORL                             G7E1PGM 
01029          MOVE  DFHBMUBF  TO  S1CORORA                             G7E1PGM 
01030          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01031          THEN                                                     G7E1PGM 
01032              NEXT SENTENCE                                        G7E1PGM 
01033          ELSE                                                     G7E1PGM 
01034              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01035              SET WT-01-INDEX TO +08                               G7E1PGM 
01036              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
01037                                                                   G7E1PGM 
01038                                                                   G7E1PGM 
01039 *-- VALIDATE ------ TREATMENT TIME FACTOR IND -------------------*G7E1PGM 
01040 *   1. ALPHANUMERIC                                               G7E1PGM 
01041 *   2. FIELD VALIDATION SUB-SYSTEM                                G7E1PGM 
01042                                                                   G7E1PGM 
01043      MOVE  S1TRTMII TO WS-02-CLASS-TEST-AREA.                     G7E1PGM 
01044      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7E1PGM 
01045      THEN                                                         G7E1PGM 
01046          MOVE  S1TRTMII TO GCVI-VALUE                             G7E1PGM 
01047          MOVE  'BPBA07' TO GCVI-FIELDS-KEY-ID                     G7E1PGM 
01048          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7E1PGM 
01049          IF  GCVI-VALUE-NOT-FOUND                                 G7E1PGM 
01050          THEN                                                     G7E1PGM 
01051              MOVE  -1        TO  S1TRTMIL                         G7E1PGM 
01052              MOVE  DFHBMUBF  TO  S1TRTMIA                         G7E1PGM 
01053              IF  WS-02-SCREEN-HAS-ERRORS                          G7E1PGM 
01054              THEN                                                 G7E1PGM 
01055                  NEXT SENTENCE                                    G7E1PGM 
01056              ELSE                                                 G7E1PGM 
01057                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
01058                  SET WT-01-INDEX TO +09                           G7E1PGM 
01059                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
01060          ELSE                                                     G7E1PGM 
01061              IF  GCVI-VALUE-NOT-LOADED                            G7E1PGM 
01062              THEN                                                 G7E1PGM 
01063                  MOVE  DFHBMUBF  TO  S1TRTMIA                     G7E1PGM 
01064              ELSE                                                 G7E1PGM 
01065                  NEXT SENTENCE                                    G7E1PGM 
01066      ELSE                                                         G7E1PGM 
01067          MOVE  -1        TO  S1TRTMIL                             G7E1PGM 
01068          MOVE  DFHBMUBF  TO  S1TRTMIA                             G7E1PGM 
01069          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01070          THEN                                                     G7E1PGM 
01071              NEXT SENTENCE                                        G7E1PGM 
01072          ELSE                                                     G7E1PGM 
01073              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01074              SET WT-01-INDEX TO +08                               G7E1PGM 
01075              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
01076                                                                   G7E1PGM 
01077                                                                   G7E1PGM 
01078 *-- VALIDATE ------ TREATMENT TIME FACTOR -----------------------*G7E1PGM 
01079 *   1. NUMERICS                                                   G7E1PGM 
01080                                                                   G7E1PGM 
01081      IF  S1TRTMFI IS NUMERIC                                      G7E1PGM 
01082      THEN                                                         G7E1PGM 
01083          NEXT SENTENCE                                            G7E1PGM 
01084      ELSE                                                         G7E1PGM 
01085          MOVE  -1        TO  S1TRTMFL                             G7E1PGM 
01086          MOVE  DFHBMUBF  TO  S1TRTMFA                             G7E1PGM 
01087          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01088          THEN                                                     G7E1PGM 
01089              NEXT SENTENCE                                        G7E1PGM 
01090          ELSE                                                     G7E1PGM 
01091              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01092              SET WT-01-INDEX TO +13                               G7E1PGM 
01093              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
01094                                                                   G7E1PGM 
01095                                                                   G7E1PGM 
01096  2100-900-EXIT.                                                   G7E1PGM 
01097      EXIT.                                                        G7E1PGM 
01098 /***************************************************************  G7E1PGM 
01099 *                                                              *  G7E1PGM 
01100 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7E1PGM 
01101 *                                                              *  G7E1PGM 
01102 ****************************************************************  G7E1PGM 
01103  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7E1PGM 
01104  2110-010.                                                        G7E1PGM 
01105                                                                   G7E1PGM 
01106      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7E1PGM 
01107                                                                   G7E1PGM 
01108      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7E1PGM 
01109                       COMMAREA(GCVIOPGM-PARM-LIST)                G7E1PGM 
01110                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7E1PGM 
01111                       END-EXEC.                                   G7E1PGM 
01112                                                                   G7E1PGM 
01113      IF  GCVI-VALUE-NOT-LOADED                                    G7E1PGM 
01114          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7E1PGM 
01115                                                                   G7E1PGM 
01116  2110-900-EXIT.                                                   G7E1PGM 
01117      EXIT.                                                        G7E1PGM 
01118 /***************************************************************  G7E1PGM 
01119 *                                                              *  G7E1PGM 
01120 * 2200  DO SCREEN LOGICAL EDITS                                *  G7E1PGM 
01121 *                                                              *  G7E1PGM 
01122 ****************************************************************  G7E1PGM 
01123  2200-000-LOGICAL-EDITS         SECTION.                          G7E1PGM 
01124  2200-010.                                                        G7E1PGM 
01125                                                                   G7E1PGM 
01126 *----------------------------------------------------------------*G7E1PGM 
01127 *                                                                *G7E1PGM 
01128 *  IF   HOSPITAL ADMISSION RESTRICTION IND (S1HADMR) > ZERO      *G7E1PGM 
01129 *                                                                *G7E1PGM 
01130 *  THEN HOSPITAL ADMISSION RESTRICTION DAYS(S1HADRD):            *G7E1PGM 
01131 *                      MUST BE > ZERO.                           *G7E1PGM 
01132 *                                                                *G7E1PGM 
01133 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7E1PGM 
01134 *                                                                *G7E1PGM 
01135 *----------------------------------------------------------------*G7E1PGM 
01136                                                                   G7E1PGM 
01137      IF  S1HADMRI     > ZEROS                                     G7E1PGM 
01138          AND                                                      G7E1PGM 
01139          S1HADRDI NOT > ZEROS                                     G7E1PGM 
01140      THEN                                                         G7E1PGM 
01141          MOVE  -1        TO  S1HADRDL                             G7E1PGM 
01142          MOVE  DFHBMUBF  TO  S1HADRDA                             G7E1PGM 
01143                              S1HADMRA                             G7E1PGM 
01144          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01145          THEN                                                     G7E1PGM 
01146              NEXT SENTENCE                                        G7E1PGM 
01147          ELSE                                                     G7E1PGM 
01148              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01149              SET WT-01-INDEX TO +02                               G7E1PGM 
01150              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7E1PGM 
01151      ELSE                                                         G7E1PGM 
01152          NEXT SENTENCE.                                           G7E1PGM 
01153                                                                   G7E1PGM 
01154      IF  S1HADRDI     > ZEROS                                     G7E1PGM 
01155          AND                                                      G7E1PGM 
01156          S1HADMRI NOT > ZEROS                                     G7E1PGM 
01157      THEN                                                         G7E1PGM 
01158          MOVE  -1        TO  S1HADMRL                             G7E1PGM 
01159          MOVE  DFHBMUBF  TO  S1HADMRA                             G7E1PGM 
01160                              S1HADRDA                             G7E1PGM 
01161          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01162          THEN                                                     G7E1PGM 
01163              NEXT SENTENCE                                        G7E1PGM 
01164          ELSE                                                     G7E1PGM 
01165              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01166              SET WT-01-INDEX TO +03                               G7E1PGM 
01167              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7E1PGM 
01168      ELSE                                                         G7E1PGM 
01169          NEXT SENTENCE.                                           G7E1PGM 
01170                                                                   G7E1PGM 
01171                                                                   G7E1PGM 
01172 *----------------------------------------------------------------*G7E1PGM 
01173 *                                                                *G7E1PGM 
01174 *  IF   BENEFIT MAXIMUM VISITS IND         (S1BMVSI) > ZERO      *G7E1PGM 
01175 *                                                                *G7E1PGM 
01176 *  THEN BENEFIT MAXIMUM VISITS             (S1BMVST):            *G7E1PGM 
01177 *                      MUST BE > ZERO.                           *G7E1PGM 
01178 *                                                                *G7E1PGM 
01179 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7E1PGM 
01180 *                                                                *G7E1PGM 
01181 *----------------------------------------------------------------*G7E1PGM 
01182                                                                   G7E1PGM 
01183      IF  S1BMVSII     > ZEROS                                     G7E1PGM 
01184          AND                                                      G7E1PGM 
01185          S1BMVSTI NOT > ZEROS                                     G7E1PGM 
01186      THEN                                                         G7E1PGM 
01187          MOVE  -1        TO  S1BMVSTL                             G7E1PGM 
01188          MOVE  DFHBMUBF  TO  S1BMVSTA                             G7E1PGM 
01189                              S1BMVSIA                             G7E1PGM 
01190          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01191          THEN                                                     G7E1PGM 
01192              NEXT SENTENCE                                        G7E1PGM 
01193          ELSE                                                     G7E1PGM 
01194              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01195              SET WT-01-INDEX TO +04                               G7E1PGM 
01196              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7E1PGM 
01197      ELSE                                                         G7E1PGM 
01198          NEXT SENTENCE.                                           G7E1PGM 
01199                                                                   G7E1PGM 
01200      IF  S1BMVSTI     > ZEROS                                     G7E1PGM 
01201          AND                                                      G7E1PGM 
01202          S1BMVSII NOT > ZEROS                                     G7E1PGM 
01203      THEN                                                         G7E1PGM 
01204          MOVE  -1        TO  S1BMVSIL                             G7E1PGM 
01205          MOVE  DFHBMUBF  TO  S1BMVSIA                             G7E1PGM 
01206                              S1BMVSTA                             G7E1PGM 
01207          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01208          THEN                                                     G7E1PGM 
01209              NEXT SENTENCE                                        G7E1PGM 
01210          ELSE                                                     G7E1PGM 
01211              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01212              SET WT-01-INDEX TO +05                               G7E1PGM 
01213              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7E1PGM 
01214      ELSE                                                         G7E1PGM 
01215          NEXT SENTENCE.                                           G7E1PGM 
01216                                                                   G7E1PGM 
01217                                                                   G7E1PGM 
01218 *----------------------------------------------------------------*G7E1PGM 
01219 *                                                                *G7E1PGM 
01220 *  IF   TREATMENT TIME FACTOR IND          (S1TRTMI) > ZERO      *G7E1PGM 
01221 *                                                                *G7E1PGM 
01222 *  THEN TREATMENT TIME FACTOR              (S1TRTMF):            *G7E1PGM 
01223 *                      MUST BE > ZERO.                           *G7E1PGM 
01224 *                                                                *G7E1PGM 
01225 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7E1PGM 
01226 *                                                                *G7E1PGM 
01227 *----------------------------------------------------------------*G7E1PGM 
01228                                                                   G7E1PGM 
01229      IF  S1TRTMII     > ZEROS                                     G7E1PGM 
01230          AND                                                      G7E1PGM 
01231          S1TRTMFI NOT > ZEROS                                     G7E1PGM 
01232      THEN                                                         G7E1PGM 
01233          MOVE  -1        TO  S1TRTMFL                             G7E1PGM 
01234          MOVE  DFHBMUBF  TO  S1TRTMFA                             G7E1PGM 
01235                              S1TRTMIA                             G7E1PGM 
01236          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01237          THEN                                                     G7E1PGM 
01238              NEXT SENTENCE                                        G7E1PGM 
01239          ELSE                                                     G7E1PGM 
01240              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01241              SET WT-01-INDEX TO +10                               G7E1PGM 
01242              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7E1PGM 
01243      ELSE                                                         G7E1PGM 
01244          NEXT SENTENCE.                                           G7E1PGM 
01245                                                                   G7E1PGM 
01246      IF S1TRTMFI = ZEROS AND S1TRTMII = ZEROS                     G7E1PGM 
01247          NEXT SENTENCE                                            G7E1PGM 
01248      ELSE                                                         G7E1PGM 
01249      IF (S1TRTMFI > SPACES AND NOT = ZEROS)                       G7E1PGM 
01250        AND                                                        G7E1PGM 
01251         (S1TRTMII > SPACES AND NOT = ZEROS)                       G7E1PGM 
01252          NEXT SENTENCE                                            G7E1PGM 
01253      ELSE                                                         G7E1PGM 
01254          MOVE  -1        TO  S1TRTMIL                             G7E1PGM 
01255          MOVE  DFHBMUBF  TO  S1TRTMIA                             G7E1PGM 
01256                              S1TRTMFA                             G7E1PGM 
01257          IF  WS-02-SCREEN-HAS-ERRORS                              G7E1PGM 
01258          THEN                                                     G7E1PGM 
01259              NEXT SENTENCE                                        G7E1PGM 
01260          ELSE                                                     G7E1PGM 
01261              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7E1PGM 
01262              SET WT-01-INDEX TO +11                               G7E1PGM 
01263              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7E1PGM 
01264                                                                   G7E1PGM 
01265                                                                   G7E1PGM 
01266 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7E1PGM 
01267                                                                   G7E1PGM 
01268      IF  WS-02-SCREEN-HAS-ERRORS                                  G7E1PGM 
01269      THEN                                                         G7E1PGM 
01270          NEXT SENTENCE                                            G7E1PGM 
01271      ELSE                                                         G7E1PGM 
01272          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7E1PGM 
01273          THEN                                                     G7E1PGM 
01274              IF EIBAID = DFHPF4 OR DFHPF16                        G7E1PGM 
01275              THEN                                                 G7E1PGM 
01276                  NEXT SENTENCE                                    G7E1PGM 
01277              ELSE                                                 G7E1PGM 
01278                  MOVE  -1        TO S1ERRL                        G7E1PGM 
01279                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7E1PGM 
01280                  SET WT-01-INDEX TO +06                           G7E1PGM 
01281                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7E1PGM 
01282          ELSE                                                     G7E1PGM 
01283              NEXT SENTENCE.                                       G7E1PGM 
01284                                                                   G7E1PGM 
01285                                                                   G7E1PGM 
01286  2200-900-EXIT.                                                   G7E1PGM 
01287      EXIT.                                                        G7E1PGM 
01288 /***************************************************************  G7E1PGM 
01289 *                                                              *  G7E1PGM 
01290 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7E1PGM 
01291 *        REWRITE TO WORKFILE.                                  *  G7E1PGM 
01292 *                                                              *  G7E1PGM 
01293 ****************************************************************  G7E1PGM 
01294  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7E1PGM 
01295  2300-010.                                                        G7E1PGM 
01296                                                                   G7E1PGM 
01297 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7E1PGM 
01298                                                                   G7E1PGM 
01299      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7E1PGM 
01300      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7E1PGM 
01301        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7E1PGM 
01302      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7E1PGM 
01303      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7E1PGM 
01304      IF  NOT GCIO2-GOOD-RETURN                                    G7E1PGM 
01305          MOVE WS-01-ABCODE-E1F2     TO WS-01-ABCODE               G7E1PGM 
01306          MOVE WS-01-ABCODE-E1F2-MSG TO WS-01-ABCODE-MSG           G7E1PGM 
01307          PERFORM  9999-000-ABEND-THE-TASK.                        G7E1PGM 
01308                                                                   G7E1PGM 
01309                                                                   G7E1PGM 
01310 *----- SAVE FIELDS FROM SCREEN THAT CANNOT BE DIRECTLY ----------*G7E1PGM 
01311 *        COMPARED TO THE RECORD                                   G7E1PGM 
01312                                                                   G7E1PGM 
01313      MOVE S1HADRDI    TO WS-02-HSP-ADM-RESTRN-DAYS-X.             G7E1PGM 
01314      MOVE S1BMVSTI    TO WS-02-BEN-MAX-VISITS-DAYS-X.             G7E1PGM 
01315 *    MOVE S1XAVI      TO WS-02-MAX-AMT-PER-VISIT-X.               G7E1PGM 
01316      MOVE S1TRTMFI    TO WS-02-TREAT-TIME-FACTOR-X.               G7E1PGM 
01317                                                                   G7E1PGM 
01318                                                                   G7E1PGM 
01319 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7E1PGM 
01320                                                                   G7E1PGM 
01321      MOVE GPE2-MAX-AMT-PER-VISIT  TO                              G7E1PGM 
01322        WS-GPE2-MAX-AMT-PER-VISIT.                                 G7E1PGM 
01323                                                                   G7E1PGM 
01324      IF      S1BESCII                  = GPE2-BEN-SCOPE-ID        G7E1PGM 
01325          AND S1EXCSCI                  = GPE2-EXCP-SCHED-ID       G7E1PGM 
01326          AND S1HADMRI                  = GPE2-HOSP-ADM-RESTRN-IND G7E1PGM 
01327          AND WS-02-HSP-ADM-RESTRN-DAYS = GPE2-HSP-ADM-RESTRN-DAYS G7E1PGM 
01328          AND S1BMVSII                  = GPE2-BEN-MAX-VISITS-IND  G7E1PGM 
01329          AND WS-02-BEN-MAX-VISITS-DAYS = GPE2-BEN-MAX-VISITS-DAYS G7E1PGM 
01330          AND S1CORORI                  = GPE2-CORRIDOR-OVERRIDE   G7E1PGM 
01331          AND WS-02-MAX-AMT-PER-VISIT  = WS-GPE2-MAX-AMT-PER-VISIT G7E1PGM 
01332          AND S1TRTMII                 = GPE2-TREAT-TIME-FACTOR-INDG7E1PGM 
01333          AND WS-02-TREAT-TIME-FACTOR   = GPE2-TREAT-TIME-FACTOR   G7E1PGM 
01334      THEN                                                         G7E1PGM 
01335          GO TO 2300-900-EXIT                                      G7E1PGM 
01336      ELSE                                                         G7E1PGM 
01337          NEXT SENTENCE.                                           G7E1PGM 
01338                                                                   G7E1PGM 
01339                                                                   G7E1PGM 
01340 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7E1PGM 
01341                                                                   G7E1PGM 
01342      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7E1PGM 
01343      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7E1PGM 
01344      IF  NOT GCIO2-GOOD-RETURN                                    G7E1PGM 
01345          MOVE WS-01-ABCODE-E1F3     TO WS-01-ABCODE               G7E1PGM 
01346          MOVE WS-01-ABCODE-E1F3-MSG TO WS-01-ABCODE-MSG           G7E1PGM 
01347          PERFORM  9999-000-ABEND-THE-TASK.                        G7E1PGM 
01348                                                                   G7E1PGM 
01349                                                                   G7E1PGM 
01350 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7E1PGM 
01351                                                                   G7E1PGM 
01352      MOVE S1BESCII                  TO GPE2-BEN-SCOPE-ID.         G7E1PGM 
01353      MOVE S1EXCSCI                  TO GPE2-EXCP-SCHED-ID.        G7E1PGM 
01354      MOVE S1HADMRI                  TO GPE2-HOSP-ADM-RESTRN-IND.  G7E1PGM 
01355      MOVE WS-02-HSP-ADM-RESTRN-DAYS TO GPE2-HSP-ADM-RESTRN-DAYS.  G7E1PGM 
01356      MOVE S1BMVSII                  TO GPE2-BEN-MAX-VISITS-IND.   G7E1PGM 
01357      MOVE WS-02-BEN-MAX-VISITS-DAYS TO GPE2-BEN-MAX-VISITS-DAYS.  G7E1PGM 
01358      MOVE S1CORORI                  TO GPE2-CORRIDOR-OVERRIDE.    G7E1PGM 
01359      MOVE WS-02-MAX-AMT-PER-VISIT   TO GPE2-MAX-AMT-PER-VISIT.    G7E1PGM 
01360      MOVE S1TRTMII                  TO GPE2-TREAT-TIME-FACTOR-IND.G7E1PGM 
01361      MOVE WS-02-TREAT-TIME-FACTOR   TO GPE2-TREAT-TIME-FACTOR.    G7E1PGM 
01362                                                                   G7E1PGM 
01363                                                                   G7E1PGM 
01364 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7E1PGM 
01365                                                                   G7E1PGM 
01366 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7E1PGM 
01367                                                                   G7E1PGM 
01368      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7E1PGM 
01369      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7E1PGM 
01370      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7E1PGM 
01371      IF  NOT GCIO2-GOOD-RETURN                                    G7E1PGM 
01372          MOVE WS-01-ABCODE-E1F4     TO WS-01-ABCODE               G7E1PGM 
01373          MOVE WS-01-ABCODE-E1F4-MSG TO WS-01-ABCODE-MSG           G7E1PGM 
01374          PERFORM  9999-000-ABEND-THE-TASK.                        G7E1PGM 
01375                                                                   G7E1PGM 
01376  2300-900-EXIT.                                                   G7E1PGM 
01377      EXIT.                                                        G7E1PGM 
01378 /***************************************************************  G7E1PGM 
01379 *                                                              *  G7E1PGM 
01380 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7E1PGM 
01381 *                                                              *  G7E1PGM 
01382 ****************************************************************  G7E1PGM 
01383  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7E1PGM 
01384  2310-010.                                                        G7E1PGM 
01385                                                                   G7E1PGM 
01386                                                                   G7E1PGM 
01387 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7E1PGM 
01388                                                                   G7E1PGM 
01389      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7E1PGM 
01390                                         + GC-WORKFILE-KEY-LEN     G7E1PGM 
01391                                         + GC-GCBENPRV-MAX-REC-LEN.G7E1PGM 
01392                                                                   G7E1PGM 
01393      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7E1PGM 
01394                          INITIMG(WS-02-HEX-00)                    G7E1PGM 
01395                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7E1PGM 
01396                          END-EXEC.                                G7E1PGM 
01397                                                                   G7E1PGM 
01398 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7E1PGM 
01399                                                                   G7E1PGM 
01400      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7E1PGM 
01401                                                                   G7E1PGM 
01402      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7E1PGM 
01403      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7E1PGM 
01404      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7E1PGM 
01405      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7E1PGM 
01406      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7E1PGM 
01407      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7E1PGM 
01408                                                                   G7E1PGM 
01409      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7E1PGM 
01410      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7E1PGM 
01411      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7E1PGM 
01412      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7E1PGM 
01413      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7E1PGM 
01414      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7E1PGM 
01415      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7E1PGM 
01416      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7E1PGM 
01417                                                                   G7E1PGM 
01418 ***  MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7E1PGM 
01419 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7E1PGM 
01420 ***  IF  HGADATE-RETURN = ZEROS                                   G7E1PGM 
01421 ***  THEN                                                         G7E1PGM 
01422 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7E1PGM 
01423 ***  ELSE                                                         G7E1PGM 
01424 ***      SET WT-01-INDEX TO +07                                   G7E1PGM 
01425 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7E1PGM 
01426 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7E1PGM 
01427                                                                   G7E1PGM 
01428      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7E1PGM 
01429      MOVE S1BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7E1PGM 
01430      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7E1PGM 
01431      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7E1PGM 
01432      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7E1PGM 
01433      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7E1PGM 
01434      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7E1PGM 
01435                                                                   G7E1PGM 
01436                                                                   G7E1PGM 
01437  2310-900-EXIT.                                                   G7E1PGM 
01438      EXIT.                                                        G7E1PGM 
01439 /***************************************************************  G7E1PGM 
01440 *                                                              *  G7E1PGM 
01441 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7E1PGM 
01442 *                                                              *  G7E1PGM 
01443 ****************************************************************  G7E1PGM 
01444  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7E1PGM 
01445  2400-010.                                                        G7E1PGM 
01446                                                                   G7E1PGM 
01447                                                                   G7E1PGM 
01448      IF  EIBAID = DFHPF7  OR DFHPF19                              G7E1PGM 
01449      THEN                                                         G7E1PGM 
01450          MOVE 'GC6CPGM' TO WS-02-NEXT-PROGRAM.                    G7E1PGM 
01451                                                                   G7E1PGM 
01452      IF  EIBAID = DFHENTER OR                                     G7E1PGM 
01453                   DFHPF4   OR DFHPF16 OR                          G7E1PGM 
01454                   DFHPF8   OR DFHPF20                             G7E1PGM 
01455      THEN                                                         G7E1PGM 
01456          MOVE 'G7E2PGM' TO WS-02-NEXT-PROGRAM.                    G7E1PGM 
01457                                                                   G7E1PGM 
01458      IF  EIBAID = DFHPF6  OR DFHPF18                              G7E1PGM 
01459      THEN                                                         G7E1PGM 
01460          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7E1PGM 
01461                                                                   G7E1PGM 
01462                                                                   G7E1PGM 
01463      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7E1PGM 
01464                       COMMAREA(WORK-RECORD-2)                     G7E1PGM 
01465                       LENGTH  (GCIO2-RECORD-LENGTH)               G7E1PGM 
01466                       END-EXEC.                                   G7E1PGM 
01467                                                                   G7E1PGM 
01468  2400-900-EXIT.                                                   G7E1PGM 
01469      EXIT.                                                        G7E1PGM 
01470 /***************************************************************  G7E1PGM 
01471 *                                                              *  G7E1PGM 
01472 * 2500   LINK TO GX3APGM FOR CONVERSION.                          G7E1PGM 
01473 *                                                              *  G7E1PGM 
01474 ****************************************************************  G7E1PGM 
01475  2500-LINK-TO-GX3APGM.                                            G7E1PGM 
01476                                                                   G7E1PGM 
01477      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7E1PGM 
01478                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7E1PGM 
01479                       LENGTH  (+51)                               G7E1PGM 
01480                       END-EXEC.                                   G7E1PGM 
01481                                                                   G7E1PGM 
01482                                                                   G7E1PGM 
01483  2500-EXIT.                                                       G7E1PGM 
01484      EXIT.                                                        G7E1PGM 
01485 /***************************************************************  G7E1PGM 
01486 *                                                              *  G7E1PGM 
01487 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7E1PGM 
01488 *         PROVISION RECORD (TYPE=C4)                           *  G7E1PGM 
01489 *                                                              *  G7E1PGM 
01490 ****************************************************************  G7E1PGM 
01491  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7E1PGM 
01492  5000-010.                                                        G7E1PGM 
01493                                                                   G7E1PGM 
01494      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7E1PGM 
01495                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7E1PGM 
01496                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7E1PGM 
01497                       END-EXEC.                                   G7E1PGM 
01498                                                                   G7E1PGM 
01499                                                                   G7E1PGM 
01500  5000-900-EXIT.                                                   G7E1PGM 
01501      EXIT.                                                        G7E1PGM 
01502 /***************************************************************  G7E1PGM 
01503 *                                                              *  G7E1PGM 
01504 * 5100                                                         *  G7E1PGM 
01505 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7E1PGM 
01506 *                                                              *  G7E1PGM 
01507 ****************************************************************  G7E1PGM 
01508  5100-000-W-F-CONTRACT-IO       SECTION.                          G7E1PGM 
01509  5100-010.                                                        G7E1PGM 
01510                                                                   G7E1PGM 
01511      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7E1PGM 
01512                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7E1PGM 
01513                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7E1PGM 
01514                       END-EXEC.                                   G7E1PGM 
01515                                                                   G7E1PGM 
01516                                                                   G7E1PGM 
01517  5100-900-EXIT.                                                   G7E1PGM 
01518      EXIT.                                                        G7E1PGM 
01519 /***************************************************************  G7E1PGM 
01520 *                                                              *  G7E1PGM 
01521 * 9000   MOVE MESSAGE TO SCREEN                                *  G7E1PGM 
01522 *                                                              *  G7E1PGM 
01523 ****************************************************************  G7E1PGM 
01524  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7E1PGM 
01525  9000-010.                                                        G7E1PGM 
01526                                                                   G7E1PGM 
01527      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S1ERRO.              G7E1PGM 
01528                                                                   G7E1PGM 
01529  9000-900-EXIT.                                                   G7E1PGM 
01530      EXIT.                                                        G7E1PGM 
01531 /***************************************************************  G7E1PGM 
01532 *                                                              *  G7E1PGM 
01533 * 9100 SEND SCREEN AND RETURN                                  *  G7E1PGM 
01534 *                                                              *  G7E1PGM 
01535 ****************************************************************  G7E1PGM 
01536  9100-000-SEND-THEN-RETURN      SECTION.                          G7E1PGM 
01537  9100-010.                                                        G7E1PGM 
01538                                                                   G7E1PGM 
01539                                                                   G7E1PGM 
01540 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7E1PGM 
01541      MOVE  -1 TO  S1ERRL.                                         G7E1PGM 
01542                                                                   G7E1PGM 
01543                                                                   G7E1PGM 
01544      IF  WS-02-MY-EIBTRNID                                        G7E1PGM 
01545      THEN                                                         G7E1PGM 
01546          EXEC CICS  SEND MAP   ('G7E1I01')                        G7E1PGM 
01547                          MAPSET('G7E1SET')                        G7E1PGM 
01548                          DATAONLY                                 G7E1PGM 
01549                          CURSOR                                   G7E1PGM 
01550                          END-EXEC                                 G7E1PGM 
01551      ELSE                                                         G7E1PGM 
01552          EXEC CICS  SEND MAP   ('G7E1I01')                        G7E1PGM 
01553                          MAPSET('G7E1SET')                        G7E1PGM 
01554                          ERASE                                    G7E1PGM 
01555                          CURSOR                                   G7E1PGM 
01556                          END-EXEC.                                G7E1PGM 
01557                                                                   G7E1PGM 
01558      EXEC CICS RETURN                                             G7E1PGM 
01559                TRANSID  ('G7E1')                                  G7E1PGM 
01560                COMMAREA (DFHCOMMAREA)                             G7E1PGM 
01561                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7E1PGM 
01562                END-EXEC.                                          G7E1PGM 
01563 *                                                                 G7E1PGM 
01564  9100-900-EXIT.                                                   G7E1PGM 
01565      EXIT.                                                        G7E1PGM 
01566 /*****************************************************************G7E1PGM 
01567 *                                                                *G7E1PGM 
01568 * 9200    XCTL TO GCPSPGM                                        *G7E1PGM 
01569 *                                                                *G7E1PGM 
01570 *                                                                *G7E1PGM 
01571 ******************************************************************G7E1PGM 
01572  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7E1PGM 
01573  9200-010.                                                        G7E1PGM 
01574                                                                   G7E1PGM 
01575      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7E1PGM 
01576                       END-EXEC.                                   G7E1PGM 
01577                                                                   G7E1PGM 
01578  9200-900-EXIT.                                                   G7E1PGM 
01579      EXIT.                                                        G7E1PGM 
01580 /*****************************************************************G7E1PGM 
01581 *                                                                *G7E1PGM 
01582 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7E1PGM 
01583 *                                                                *G7E1PGM 
01584 *                                                                *G7E1PGM 
01585 ******************************************************************G7E1PGM 
01586  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7E1PGM 
01587  9210-010.                                                        G7E1PGM 
01588                                                                   G7E1PGM 
01589      IF  S1GRPNOI = '000SPS000'                                   G7E1PGM 
01590          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7E1PGM 
01591                           END-EXEC.                               G7E1PGM 
01592                                                                   G7E1PGM 
01593 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7E1PGM 
01594                                                                   G7E1PGM 
01595      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7E1PGM 
01596                                        + GC-WORKFILE-KEY-LEN      G7E1PGM 
01597                                        + GC-GCCONTR-MAX-REC-LEN.  G7E1PGM 
01598                                                                   G7E1PGM 
01599      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7E1PGM 
01600                          INITIMG(WS-02-HEX-00)                    G7E1PGM 
01601                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7E1PGM 
01602                          END-EXEC.                                G7E1PGM 
01603                                                                   G7E1PGM 
01604 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7E1PGM 
01605 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7E1PGM 
01606                                                                   G7E1PGM 
01607 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7E1PGM 
01608                                                                   G7E1PGM 
01609      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7E1PGM 
01610        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7E1PGM 
01611                                                                   G7E1PGM 
01612      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7E1PGM 
01613      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7E1PGM 
01614                                                                   G7E1PGM 
01615      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7E1PGM 
01616      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7E1PGM 
01617      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7E1PGM 
01618      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7E1PGM 
01619      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7E1PGM 
01620      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7E1PGM 
01621                                                                   G7E1PGM 
01622      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7E1PGM 
01623      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7E1PGM 
01624      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7E1PGM 
01625      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7E1PGM 
01626      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7E1PGM 
01627      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7E1PGM 
01628      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7E1PGM 
01629      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7E1PGM 
01630                                                                   G7E1PGM 
01631 ***  MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7E1PGM 
01632 ***  PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7E1PGM 
01633 ***  IF  HGADATE-RETURN = ZEROS                                   G7E1PGM 
01634 ***  THEN                                                         G7E1PGM 
01635 ***      MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7E1PGM 
01636 ***  ELSE                                                         G7E1PGM 
01637 ***      SET WT-01-INDEX TO +07                                   G7E1PGM 
01638 ***      PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7E1PGM 
01639 ***      PERFORM 9100-000-SEND-THEN-RETURN.                       G7E1PGM 
01640                                                                   G7E1PGM 
01641      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7E1PGM 
01642      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7E1PGM 
01643      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7E1PGM 
01644      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7E1PGM 
01645      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7E1PGM 
01646      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7E1PGM 
01647      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7E1PGM 
01648                                                                   G7E1PGM 
01649      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7E1PGM 
01650                                                                   G7E1PGM 
01651      IF  NOT GCIO3-GOOD-RETURN                                    G7E1PGM 
01652          MOVE WS-01-ABCODE-E1F1     TO WS-01-ABCODE               G7E1PGM 
01653          MOVE WS-01-ABCODE-E1F1-MSG TO WS-01-ABCODE-MSG           G7E1PGM 
01654          PERFORM  9999-000-ABEND-THE-TASK.                        G7E1PGM 
01655                                                                   G7E1PGM 
01656      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7E1PGM 
01657                       COMMAREA(WORK-RECORD-3)                     G7E1PGM 
01658                       LENGTH  (GCIO3-RECORD-LENGTH)               G7E1PGM 
01659                       END-EXEC.                                   G7E1PGM 
01660                                                                   G7E1PGM 
01661  9210-900-EXIT.                                                   G7E1PGM 
01662      EXIT.                                                        G7E1PGM 
01663 /*****************************************************************G7E1PGM 
01664 *                                                                *G7E1PGM 
01665 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7E1PGM 
01666 *                                                                *G7E1PGM 
01667 *                                                                *G7E1PGM 
01668 ******************************************************************G7E1PGM 
01669  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7E1PGM 
01670  9220-010.                                                        G7E1PGM 
01671                                                                   G7E1PGM 
01672      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7E1PGM 
01673                       END-EXEC.                                   G7E1PGM 
01674                                                                   G7E1PGM 
01675  9220-900-EXIT.                                                   G7E1PGM 
01676      EXIT.                                                        G7E1PGM 
01677 /*****************************************************************G7E1PGM 
01678 *                                                                *G7E1PGM 
01679 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7E1PGM 
01680 *                                                                *G7E1PGM 
01681 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7E1PGM 
01682 *                                                                *G7E1PGM 
01683 ******************************************************************G7E1PGM 
01684  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7E1PGM 
01685  9800-010.                                                        G7E1PGM 
01686                                                                   G7E1PGM 
01687      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7E1PGM 
01688      MOVE 'M'   TO  HGADATE-FORM1.                                G7E1PGM 
01689      MOVE 'J'   TO  HGADATE-FORM2.                                G7E1PGM 
01690      MOVE ZEROS TO  HGADATE-RETURN                                G7E1PGM 
01691                     HGADATE-AMOUNT.                               G7E1PGM 
01692      EXEC CICS LINK PROGRAM ('HGADATES')                          G7E1PGM 
01693                     COMMAREA(HGADATES-COMMAREA)                   G7E1PGM 
01694                     LENGTH  (24)                                  G7E1PGM 
01695                     END-EXEC.                                     G7E1PGM 
01696                                                                   G7E1PGM 
01697  9800-900-900-EXIT.                                               G7E1PGM 
01698      EXIT.                                                        G7E1PGM 
01699 /*****************************************************************G7E1PGM 
01700 *                                                                *G7E1PGM 
01701 * 9810    J U L I A N    T O    G R E G O R I A N                *G7E1PGM 
01702 *                                                                *G7E1PGM 
01703 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7E1PGM 
01704 *                                                                *G7E1PGM 
01705 ******************************************************************G7E1PGM 
01706  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7E1PGM 
01707  9810-010.                                                        G7E1PGM 
01708                                                                   G7E1PGM 
01709      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7E1PGM 
01710      MOVE 'J'   TO  HGADATE-FORM1.                                G7E1PGM 
01711      MOVE 'M'   TO  HGADATE-FORM2.                                G7E1PGM 
01712      MOVE ZEROS TO  HGADATE-RETURN                                G7E1PGM 
01713                     HGADATE-AMOUNT.                               G7E1PGM 
01714      EXEC CICS LINK PROGRAM ('HGADATES')                          G7E1PGM 
01715                     COMMAREA(HGADATES-COMMAREA)                   G7E1PGM 
01716                     LENGTH  (24)                                  G7E1PGM 
01717                     END-EXEC.                                     G7E1PGM 
01718                                                                   G7E1PGM 
01719  9810-900-900-EXIT.                                               G7E1PGM 
01720      EXIT.                                                        G7E1PGM 
01721 /***************************************************************  G7E1PGM 
01722 *                                                              *  G7E1PGM 
01723 * 9999  ABEND THE TASK                                         *  G7E1PGM 
01724 *                                                              *  G7E1PGM 
01725 ****************************************************************  G7E1PGM 
01726  9999-000-ABEND-THE-TASK SECTION.                                 G7E1PGM 
01727  9999-010.                                                        G7E1PGM 
01728                                                                   G7E1PGM 
01729      EXEC CICS  ABEND                                             G7E1PGM 
01730                 ABCODE(WS-01-ABCODE)                              G7E1PGM 
01731                 END-EXEC.                                         G7E1PGM 
01732                                                                   G7E1PGM 
01733  9900-900-EXIT.                                                   G7E1PGM 
01734      EXIT.                                                        G7E1PGM 
