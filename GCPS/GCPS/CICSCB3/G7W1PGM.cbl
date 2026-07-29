00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7W1PGM.                                         G7W1PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7W1PGM 
00005  DATE-WRITTEN.   03/13/87.                                        G7W1PGM 
00006  DATE-COMPILED.                                                   G7W1PGM 
00007 ***************************************************************** G7W1PGM 
00008 *                                                               * G7W1PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7W1PGM 
00010 *                                                               * G7W1PGM 
00011 *                                                               * G7W1PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7W1PGM 
00013 *                                                               * G7W1PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7W1PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7W1PGM 
00016 *                             (SAME AS 'GC5A')                  * G7W1PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7W1PGM 
00018 *                              'GC5A')                          * G7W1PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7W1PGM 
00020 *                              PROVISION)                       * G7W1PGM 
00021 *                                                               * G7W1PGM 
00022 *  D115       7/15/87  FRY    CAUSE GCIOPGM TO CALL GX5ZPGM TO  * G7W1PGM 
00023 *                             UPDATE OPERATOR-ID IN W/F RECORD  * G7W1PGM 
00024 *                             WHEN 'C4' RECORD IS MODIFIED.     * G7W1PGM 
00025 *                                                               * G7W1PGM 
00026 *  D129      09/18/89  GDM    CONVERT FOR DECIMALS                G7W1PGM 
00027 *                                                               * G7W1PGM 
00028 *  D129      09/19/89  GDM    CONVERT TO VS COBOL/2             * G7W1PGM 
00029 *                                                               * G7W1PGM 
00030 *  D12009    08/23/91  BSO   -CORRECT ERR MESSAGES IN AREA \
00031 *                            -CORRECT ALPHA CLASS TEST AREA     * G7W1PGM 
00032 *                                                               * G7W1PGM 
00033 * 14726/     03/30/98  GSP    ADDED PLAN AND PACKAGE CODE AND   * G7W1PGM 
00034 * 15057                       INCREASED GROUP AND SECTION ON    * G7W1PGM 
00035 *                             THE SCREEN.                       * G7W1PGM 
00036 *                                                               * G7W1PGM 
00037 *            12/11/02  AKK    OPID GENERATE                     * G7W1PGM 
00038 *                                                               * G7W1PGM 
00039 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7W1PGM 
00040 *                           G7W1SET                              *G7W1PGM 
00041 ***************************************************************** G7W1PGM 
00042                                                                   G7W1PGM 
00043 ***************************************************************** G7W1PGM 
00044 *                                                               * G7W1PGM 
00045 *    G7W1PGM  - PROGRAM 1 OF 2 PROGRAMS TO UPDATE THE FORMAT 'W'* G7W1PGM 
00046 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7W1PGM 
00047 *                                                               * G7W1PGM 
00048 *    TRANSID: G7W1                                              * G7W1PGM 
00049 *    MAPSET:  G7W1SETC    (GIW1PGM WHICH SHARES THIS MAP)       * G7W1PGM 
00050 *    VALGEN:  NONE                                              * G7W1PGM 
00051 *                                                               * G7W1PGM 
00052 *    PROGRAM NARRATIVE:                                         * G7W1PGM 
00053 *                                                               * G7W1PGM 
00054 *        PROGRAM CHECKS FOR TRANS CODE 'G7W1'.  AN INVALID      * G7W1PGM 
00055 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7W1PGM 
00056 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7W1PGM 
00057 *                                                               * G7W1PGM 
00058 *        THE MAIN FUNCTIONS ARE :                               * G7W1PGM 
00059 *        1. HARDCOPY REQUEST,                                   * G7W1PGM 
00060 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7W1PGM 
00061 *           USER,                                               * G7W1PGM 
00062 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7W1PGM 
00063 *                                                               * G7W1PGM 
00064 *        HARDCOPY REQUEST                                       * G7W1PGM 
00065 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7W1PGM 
00066 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7W1PGM 
00067 *           THE SCREEN BUFFER.                                  * G7W1PGM 
00068 *                                                               * G7W1PGM 
00069 *        PROCESS INPUT DATA (UPDATE).                           * G7W1PGM 
00070 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7W1PGM 
00071 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7W1PGM 
00072 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7W1PGM 
00073 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7W1PGM 
00074 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7W1PGM 
00075 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7W1PGM 
00076 *                                                               * G7W1PGM 
00077 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7W1PGM 
00078 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7W1PGM 
00079 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7W1PGM 
00080 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7W1PGM 
00081 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7W1PGM 
00082 *                                                               * G7W1PGM 
00083 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7W1PGM 
00084 *                                     -----------------------   * G7W1PGM 
00085 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7W1PGM 
00086 *           -----------------------------------------------     * G7W1PGM 
00087 *                                                               * G7W1PGM 
00088 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7W1PGM 
00089 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7W1PGM 
00090 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7W1PGM 
00091 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7W1PGM 
00092 *           SENT BACK TO THE USER.                              * G7W1PGM 
00093 *                                                               * G7W1PGM 
00094 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7W1PGM 
00095 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7W1PGM 
00096 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7W1PGM 
00097 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7W1PGM 
00098 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7W1PGM 
00099 *           PF KEY LOGIC,                                       * G7W1PGM 
00100 *              PF6/PF18       GOES TO GC8APGM                   * G7W1PGM 
00101 *              PF8/PF20/ENTER GOES TO G7W2PGM                   * G7W1PGM 
00102 *              FOR PF7/PF19   GOES TO GC6CPGM                   * G7W1PGM 
00103 *                                                               * G7W1PGM 
00104 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7W1PGM 
00105 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7W1PGM 
00106 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7W1PGM 
00107 *                                                               * G7W1PGM 
00108 ***************************************************************** G7W1PGM 
00109                                                                   G7W1PGM 
00110  ENVIRONMENT DIVISION.                                            G7W1PGM 
00111  DATA DIVISION.                                                   G7W1PGM 
00112 /                                                                 G7W1PGM 
00113  WORKING-STORAGE SECTION.                                         G7W1PGM 
00114  01  WS-BEGIN                    PIC X(58) VALUE                  G7W1PGM 
00115      '*** G7W1PGM  WORKING-STORAGE BEGINS HERE ***'.              G7W1PGM 
00116                                                                   G7W1PGM 
00117                                                                   G7W1PGM 
00118  01  WS-01-ABEND-AREA.                                            G7W1PGM 
00119      05  FILLER                   PIC X(16)  VALUE                G7W1PGM 
00120          '** ABEND AREA **'.                                      G7W1PGM 
00121                                                                   G7W1PGM 
00122      05  WS-01-ABEND-CODES-AND-MSG.                               G7W1PGM 
00123          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7W1PGM 
00124          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7W1PGM 
00125                                                                   G7W1PGM 
00126          10  WS-01-ABCODE-W1F1          PIC X(04)  VALUE  'W1F1'. G7W1PGM 
00127          10  WS-01-ABCODE-W1F1-MSG      PIC X(44)  VALUE          G7W1PGM 
00128             'W/F CONTRACT CANNOT BE FOUND             '.          G7W1PGM 
00129                                                                   G7W1PGM 
00130          10  WS-01-ABCODE-W1F2          PIC X(04)  VALUE  'W1F2'. G7W1PGM 
00131          10  WS-01-ABCODE-W1F2-MSG      PIC X(44)  VALUE          G7W1PGM 
00132             'W/F BEN PROV CANNOT BE FOUND             '.          G7W1PGM 
00133                                                                   G7W1PGM 
00134          10  WS-01-ABCODE-W1F3          PIC X(04)  VALUE  'W1F3'. G7W1PGM 
00135          10  WS-01-ABCODE-W1F3-MSG      PIC X(44)  VALUE          G7W1PGM 
00136             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7W1PGM 
00137                                                                   G7W1PGM 
00138          10  WS-01-ABCODE-W1F4          PIC X(04)  VALUE  'W1F4'. G7W1PGM 
00139          10  WS-01-ABCODE-W1F4-MSG      PIC X(44)  VALUE          G7W1PGM 
00140             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7W1PGM 
00141                                                                   G7W1PGM 
00142          10  WS-01-ABCODE-W1L1          PIC X(04)  VALUE  'W1L1'. G7W1PGM 
00143          10  WS-01-ABCODE-W1L1-MSG      PIC X(44)  VALUE          G7W1PGM 
00144             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7W1PGM 
00145                                                                   G7W1PGM 
00146          10  WS-01-ABCODE-W1P1          PIC X(04)  VALUE  'W1P1'. G7W1PGM 
00147          10  WS-01-ABCODE-W1P1-MSG      PIC X(44)  VALUE          G7W1PGM 
00148             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7W1PGM 
00149                                                                   G7W1PGM 
00150          10  WS-01-ABCODE-W1P2          PIC X(04)  VALUE  'W1P2'. G7W1PGM 
00151          10  WS-01-ABCODE-W1P2-MSG      PIC X(44)  VALUE          G7W1PGM 
00152             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7W1PGM 
00153                                                                   G7W1PGM 
00154  01  WS-02-AREA.                                                  G7W1PGM 
00155      05  FILLER                   PIC X(16)  VALUE                G7W1PGM 
00156          '** WS-02-AREA **'.                                      G7W1PGM 
00157      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7W1PGM 
00158          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7W1PGM 
00159                                                    'GC6C' 'G7W2'  G7W1PGM 
00160                                                    'G7W1'.        G7W1PGM 
00161          88  WS-02-MY-EIBTRNID                     VALUE  'G7W1'. G7W1PGM 
00162                                                                   G7W1PGM 
00163      05  WS-02-COMPUTED-LENGTHS.                                  G7W1PGM 
00164          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7W1PGM 
00165          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7W1PGM 
00166          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7W1PGM 
00167                                                                   G7W1PGM 
00168      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7W1PGM 
00169                                                                   G7W1PGM 
00170      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7W1PGM 
00171                                                                   G7W1PGM 
00172      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7W1PGM 
00173      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7W1PGM 
00174          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7W1PGM 
00175                                         PIC X.                    G7W1PGM 
00176          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7W1PGM 
00177                                                    '0' THRU '9'   G7W1PGM 
00178                                                    'A' THRU 'I'   G7W1PGM 
00179                                                    'J' THRU 'R'   G7W1PGM 
00180                                                    'S' THRU 'Z'   G7W1PGM 
00181                                                    SPACE.         G7W1PGM 
00182                                                                   G7W1PGM 
00183      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7W1PGM 
00184          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7W1PGM 
00185          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7W1PGM 
00186                                                                   G7W1PGM 
00187      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7W1PGM 
00188          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7W1PGM 
00189                                                                   G7W1PGM 
00190      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7W1PGM 
00191                                                                   G7W1PGM 
00192      05  WS-02-HEX-F00000.                                        G7W1PGM 
00193          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7W1PGM 
00194          10  FILLER                     PIC  X(09) VALUE          G7W1PGM 
00195                                                    LOW-VALUES.    G7W1PGM 
00196                                                                   G7W1PGM 
00197      05  WS-02-RATIO                    PIC  9(2)V9 VALUE  ZEROS. G7W1PGM 
00198 *    05  FILLER    REDEFINES  WS-02-RATIO.                        G7W1PGM 
00199 *        10  FILLER                     PIC  X.                   G7W1PGM 
00200 *        10  WS-02-RATIO-DIVISOR        PIC  X.                   G7W1PGM 
00201 *        10  WS-02-RATIO-BASE           PIC  X.                   G7W1PGM 
00202                                                                   G7W1PGM 
00203      05  WS-02-STAY-CD-X.                                         G7W1PGM 
00204          10  WS-02-STAY-CD                PIC 9(3)    VALUE ZEROS.G7W1PGM 
00205          10  WS-02-S1STYCD              REDEFINES                 G7W1PGM 
00206              WS-02-STAY-CD                PIC X(3).               G7W1PGM 
00207                                                                   G7W1PGM 
00208      05  WS-02-DAYS-RDCN-RAT-BASIC-AP-X.                          G7W1PGM 
00209          10  WS-02-DAYS-RDCN-RAT-BASIC-APL PIC 99V9  VALUE ZEROS. G7W1PGM 
00210                                                                   G7W1PGM 
00211      05  WS-02-DAYS-RDCN-RAT-BASIC-BA-X.                          G7W1PGM 
00212          10  WS-02-DAYS-RDCN-RAT-BASIC-BASE PIC 99V9  VALUE ZEROS.G7W1PGM 
00213                                                                   G7W1PGM 
00214      05  WS-02-DAYS-RDCN-RAT-SEC-AP-X.                            G7W1PGM 
00215          10  WS-02-DAYS-RDCN-RAT-SEC-APL    PIC 99V9  VALUE ZEROS.G7W1PGM 
00216                                                                   G7W1PGM 
00217      05  WS-02-DAYS-RDCN-RAT-SEC-BA-X.                            G7W1PGM 
00218          10  WS-02-DAYS-RDCN-RAT-SEC-BASE   PIC 99V9  VALUE ZEROS.G7W1PGM 
00219                                                                   G7W1PGM 
00220      05  WS-02-HSP-ADM-RESTRN-DAYS-X.                             G7W1PGM 
00221          10  WS-02-HSP-ADM-RESTRN-DAYS  PIC  9(3)    VALUE ZEROS. G7W1PGM 
00222          10  WS-02-S1HADRD              REDEFINES                 G7W1PGM 
00223              WS-02-HSP-ADM-RESTRN-DAYS  PIC  X(3).                G7W1PGM 
00224                                                                   G7W1PGM 
00225      05  WS-3POS-MAX-AMT          PIC 99V9 VALUE 99.9.            G7W1PGM 
00226      05  WS-02-DISP-3POS-DEC      PIC 99.9.                       G7W1PGM 
00227      05  WS-GPW2-DAYS-RDCN-RAT-BAS-APL           PIC 99V9.        G7W1PGM 
00228      05  WS-GPW2-DAYS-RDCN-RAT-BAS-BASE          PIC 99V9.        G7W1PGM 
00229      05  WS-GPW2-DAYS-RDCN-RAT-SEC-APL           PIC 99V9.        G7W1PGM 
00230      05  WS-GPW2-DAYS-RDCN-RAT-SEC-BASE          PIC 99V9.        G7W1PGM 
00231 /                                                                 G7W1PGM 
00232  01  WT-00-G7W1PGM-TABLES.                                        G7W1PGM 
00233      05  FILLER                   PIC X(16)  VALUE                G7W1PGM 
00234          '*G7W1PGM TABLES*'.                                      G7W1PGM 
00235                                                                   G7W1PGM 
00236  01  WT-01-TABLE.                                                 G7W1PGM 
00237      05  FILLER                  PIC X(16) VALUE                  G7W1PGM 
00238          '* WT-01-TABLE  *'.                                      G7W1PGM 
00239 ******************************************************************G7W1PGM 
00240 *    WT-01   MESSAGE TABLE                                       *G7W1PGM 
00241 ******************************************************************G7W1PGM 
00242  01  FILLER.                                                      G7W1PGM 
00243      05  WT-01-MESSAGE-VALUES.                                    G7W1PGM 
00244                                                                   G7W1PGM 
00245 *----------------------------------------------------------------*G7W1PGM 
00246          10  WT-01-ENTRY-001.                                     G7W1PGM 
00247              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00248              15  WT-01-MESSAGE-TEXT-001.                          G7W1PGM 
00249                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00250                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00251                  20  FILLER          PIC X(3)  VALUE  '001'.      G7W1PGM 
00252                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00253                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00254                      ' INVALID PFKEY SELECTION                    G7W1PGM 
00255 -                    '                         '.                 G7W1PGM 
00256              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00257                                                                   G7W1PGM 
00258 *----------------------------------------------------------------*G7W1PGM 
00259          10  WT-01-ENTRY-002.                                     G7W1PGM 
00260              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00261              15  WT-01-MESSAGE-TEXT-002.                          G7W1PGM 
00262                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00263                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00264                  20  FILLER          PIC X(3)  VALUE  '002'.      G7W1PGM 
00265                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00266                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00267                      'HOSP ADMISSION RESTRICTION DAYS REQUIRED WHEG7W1PGM 
00268 -                    'N INDICATOR IS CODED     '.                 G7W1PGM 
00269              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00270                                                                   G7W1PGM 
00271 *----------------------------------------------------------------*G7W1PGM 
00272          10  WT-01-ENTRY-003.                                     G7W1PGM 
00273              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00274              15  WT-01-MESSAGE-TEXT-003.                          G7W1PGM 
00275                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00276                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00277                  20  FILLER          PIC X(3)  VALUE  '003'.      G7W1PGM 
00278                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00279                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00280                      'INDICATOR REQUIRED WHEN HOSPITAL ADMISSION RG7W1PGM 
00281 -                    'ESTRICTION DAYS CODED    '.                 G7W1PGM 
00282              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00283                                                                   G7W1PGM 
00284 *----------------------------------------------------------------*G7W1PGM 
00285          10  WT-01-ENTRY-004.                                     G7W1PGM 
00286              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00287              15  WT-01-MESSAGE-TEXT-004.                          G7W1PGM 
00288                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00289                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00290                  20  FILLER          PIC X(3)  VALUE  '004'.      G7W1PGM 
00291                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00292                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00293                      'STAY CODE DAYS REQUIRED WHEN INDICATOR IS COG7W1PGM 
00294 -                    'DED                      '.                 G7W1PGM 
00295              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00296                                                                   G7W1PGM 
00297 *----------------------------------------------------------------*G7W1PGM 
00298          10  WT-01-ENTRY-005.                                     G7W1PGM 
00299              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00300              15  WT-01-MESSAGE-TEXT-005.                          G7W1PGM 
00301                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00302                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00303                  20  FILLER          PIC X(3)  VALUE  '005'.      G7W1PGM 
00304                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00305                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00306                      'INDICATOR REQUIRED WHEN STAY CODE DAYS IS COG7W1PGM 
00307 -                    'DED                      '.                 G7W1PGM 
00308              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00309                                                                   G7W1PGM 
00310 *----------------------------------------------------------------*G7W1PGM 
00311          10  WT-01-ENTRY-006.                                     G7W1PGM 
00312              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00313              15  WT-01-MESSAGE-TEXT-006.                          G7W1PGM 
00314                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00315                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00316                  20  FILLER          PIC X(3)  VALUE  '006'.      G7W1PGM 
00317                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00318                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00319                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7W1PGM 
00320 -                    ' PF4/PF16 TO CONTINUE    '.                 G7W1PGM 
00321              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00322                                                                   G7W1PGM 
00323 *----------------------------------------------------------------*G7W1PGM 
00324          10  WT-01-ENTRY-007.                                     G7W1PGM 
00325              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00326              15  WT-01-MESSAGE-TEXT-007.                          G7W1PGM 
00327                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00328                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00329                  20  FILLER          PIC X(3)  VALUE  '007'.      G7W1PGM 
00330                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00331                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00332                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7W1PGM 
00333 -                    'E CALL SYSTEMS           '.                 G7W1PGM 
00334              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00335                                                                   G7W1PGM 
00336 *----------------------------------------------------------------*G7W1PGM 
00337          10  WT-01-ENTRY-008.                                     G7W1PGM 
00338              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00339              15  WT-01-MESSAGE-TEXT-008.                          G7W1PGM 
00340                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00341                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00342                  20  FILLER          PIC X(3)  VALUE  '008'.      G7W1PGM 
00343                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00344                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00345                      'FIELD HAS AN INVALID VALUE                  G7W1PGM 
00346 -                    '                         '.                 G7W1PGM 
00347              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00348                                                                   G7W1PGM 
00349 *----------------------------------------------------------------*G7W1PGM 
00350          10  WT-01-ENTRY-009.                                     G7W1PGM 
00351              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00352              15  WT-01-MESSAGE-TEXT-009.                          G7W1PGM 
00353                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00354                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00355                  20  FILLER          PIC X(3)  VALUE  '009'.      G7W1PGM 
00356                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00357                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00358                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7W1PGM 
00359 -                    'YSTEM)                   '.                 G7W1PGM 
00360              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00361                                                                   G7W1PGM 
00362 *----------------------------------------------------------------*G7W1PGM 
00363          10  WT-01-ENTRY-010.                                     G7W1PGM 
00364              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00365              15  WT-01-MESSAGE-TEXT-010.                          G7W1PGM 
00366                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00367                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00368                  20  FILLER          PIC X(3)  VALUE  '010'.      G7W1PGM 
00369                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00370                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00371                      '********** F U T U R E   U S E *************G7W1PGM 
00372 -                    '*************************'.                 G7W1PGM 
00373              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00374                                                                   G7W1PGM 
00375 *----------------------------------------------------------------*G7W1PGM 
00376          10  WT-01-ENTRY-011.                                     G7W1PGM 
00377              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00378              15  WT-01-MESSAGE-TEXT-011.                          G7W1PGM 
00379                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00380                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00381                  20  FILLER          PIC X(3)  VALUE  '011'.      G7W1PGM 
00382                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00383                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00384                      '********** F U T U R E   U S E *************G7W1PGM 
00385 -                    '*************************'.                 G7W1PGM 
00386              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00387                                                                   G7W1PGM 
00388 *----------------------------------------------------------------*G7W1PGM 
00389          10  WT-01-ENTRY-012.                                     G7W1PGM 
00390              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00391              15  WT-01-MESSAGE-TEXT-012.                          G7W1PGM 
00392                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00393                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00394                  20  FILLER          PIC X(3)  VALUE  '012'.      G7W1PGM 
00395                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00396                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00397                      'RATIO BASE CANNOT EQUAL ZEROES              G7W1PGM 
00398 -                    '                         '.                 G7W1PGM 
00399              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00400                                                                   G7W1PGM 
00401 *----------------------------------------------------------------*G7W1PGM 
00402          10  WT-01-ENTRY-013.                                     G7W1PGM 
00403              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00404              15  WT-01-MESSAGE-TEXT-013.                          G7W1PGM 
00405                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00406                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00407                  20  FILLER          PIC X(3)  VALUE  '013'.      G7W1PGM 
00408                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00409                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00410                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7W1PGM 
00411 -                    '                         '.                 G7W1PGM 
00412              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00413                                                                   G7W1PGM 
00414 *----------------------------------------------------------------*G7W1PGM 
00415          10  WT-01-ENTRY-014.                                     G7W1PGM 
00416              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00417              15  WT-01-MESSAGE-TEXT-014.                          G7W1PGM 
00418                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00419                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00420                  20  FILLER          PIC X(3)  VALUE  '014'.      G7W1PGM 
00421                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00422                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00423       'FIELD EXCEEDS LENGTH OF 3 POSITIONS   FORMAT IS 99.9'.     G7W1PGM 
00424              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00425                                                                   G7W1PGM 
00426 *----------------------------------------------------------------*G7W1PGM 
00427          10  WT-01-ENTRY-015.                                     G7W1PGM 
00428              15  FILLER              PIC X(2)  VALUE '¬>'.        G7W1PGM 
00429              15  WT-01-MESSAGE-TEXT-015.                          G7W1PGM 
00430                  20  FILLER          PIC X(4)  VALUE  'G7W1'.     G7W1PGM 
00431                  20  FILLER          PIC X(1)  VALUE  '-'.        G7W1PGM 
00432                  20  FILLER          PIC X(3)  VALUE  '015'.      G7W1PGM 
00433                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7W1PGM 
00434                  20  FILLER          PIC X(70) VALUE              G7W1PGM 
00435                      ' INVALID DECIMAL DETECTED '.                G7W1PGM 
00436              15  FILLER              PIC X(2)  VALUE '<¬'.        G7W1PGM 
00437 *----------------------------------------------------------------*G7W1PGM 
00438                                                                   G7W1PGM 
00439      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7W1PGM 
00440          WT-01-MESSAGE-VALUES         OCCURS 015 TIMES            G7W1PGM 
00441                                      INDEXED BY WT-01-INDEX.      G7W1PGM 
00442          10  WT-01-ENTRY.                                         G7W1PGM 
00443              15  FILLER              PIC X(02).                   G7W1PGM 
00444              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7W1PGM 
00445              15  FILLER              PIC X(02).                   G7W1PGM 
00446                                                                   G7W1PGM 
00447                                                                   G7W1PGM 
00448 /*** MAP FIELD ATTRIBUTES                                         G7W1PGM 
00449  COPY DFHBMSCA.                                                   G7W1PGM 
00450 *                         AUTOSKIP, BRIGHT, FSET                  G7W1PGM 
00451      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7W1PGM 
00452                                                                   G7W1PGM 
00453 /*** ATTENTION KEYS                                               G7W1PGM 
00454  COPY DFHAID.                                                     G7W1PGM 
00455                                                                   G7W1PGM 
00456 /***  PROVISION MAINTENANCE SCREEN                                G7W1PGM 
00457  COPY  G7W1SETC.                                                  G7W1PGM 
00458                                                                   G7W1PGM 
00459 /*** DATE ROUTINE COMMAREA                                        G7W1PGM 
00460  01  HGADATES-COMMAREA.                                           G7W1PGM 
00461  COPY HGCDAT01.                                                   G7W1PGM 
00462                                                                   G7W1PGM 
00463 /*** DECIMAL CONVERT COMMAREA                                     G7W1PGM 
00464  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7W1PGM 
00465  COPY GCDCCA01.                                                   G7W1PGM 
00466                                                                   G7W1PGM 
00467 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7W1PGM 
00468  01  GCVIOPGM-PARM-LIST.                                          G7W1PGM 
00469  COPY GCVINTRC.                                                   G7W1PGM 
00470                                                                   G7W1PGM 
00471 /*** ALTERNATIVE WORKFILE KEYS                                    G7W1PGM 
00472  01  FILLER.                                                      G7W1PGM 
00473      COPY GCWRKKEY.                                               G7W1PGM 
00474                                                                   G7W1PGM 
00475 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7W1PGM 
00476  01  FILLER.                                                      G7W1PGM 
00477      COPY GCCDRLEN.                                               G7W1PGM 
00478                                                                   G7W1PGM 
00479                                                                   G7W1PGM 
00480  01  WS-END                       PIC X(58) VALUE                 G7W1PGM 
00481      '*** G7W1PGM  WORKING-STORAGE ENDS HERE ***'.                G7W1PGM 
00482 /                                                                 G7W1PGM 
00483  LINKAGE SECTION.                                                 G7W1PGM 
00484 /                                                                 G7W1PGM 
00485  01  DFHCOMMAREA.                                                 G7W1PGM 
00486      COPY  GCWRKDCC.                                              G7W1PGM 
00487      COPY  GCBENPVC.                                              G7W1PGM 
00488 /                                                                 G7W1PGM 
00489 *01  BLL-CELLS.                                                   G7W1PGM 
00490 *    05  FILLER                   PIC S9(08)  COMP.               G7W1PGM 
00491 *    05  BEN-PROV-PNTR            PIC S9(08)  COMP.               G7W1PGM 
00492 *    05  CONTRACT-PNTR            PIC S9(08)  COMP.               G7W1PGM 
00493 *    05  CONTRACT-PNTR-2          PIC S9(08)  COMP.               G7W1PGM 
00494                                                                   G7W1PGM 
00495 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7W1PGM 
00496  01  IO-PARM-BEN-PROV-AREA.                                       G7W1PGM 
00497      COPY  GCIOPRM2.                                              G7W1PGM 
00498      COPY  GCWRKDC2.                                              G7W1PGM 
00499      COPY  GCBENPV2.                                              G7W1PGM 
00500                                                                   G7W1PGM 
00501 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7W1PGM 
00502  01  IO-PARM-CONTRACT-AREA.                                       G7W1PGM 
00503      COPY  GCIOPRM3.                                              G7W1PGM 
00504      COPY  GCWRKDC3.                                              G7W1PGM 
00505      COPY  GCCONTR2.                                              G7W1PGM 
00506 /                                                                 G7W1PGM 
00507  PROCEDURE DIVISION.                                              G7W1PGM 
00508                                                                   G7W1PGM 
00509 ****************************************************************  G7W1PGM 
00510 *                                                              *  G7W1PGM 
00511 *           P R O C E S S     C O N T R O L                    *  G7W1PGM 
00512 *                                                              *  G7W1PGM 
00513 ****************************************************************  G7W1PGM 
00514  0000-000-PROCESS-CONTROL       SECTION.                          G7W1PGM 
00515  0000-010.                                                        G7W1PGM 
00516                                                                   G7W1PGM 
00517 *    SERVICE RELOAD  BLL-CELLS.                                   G7W1PGM 
00518                                                                   G7W1PGM 
00519      IF  EIBAID  =  DFHCLEAR                                      G7W1PGM 
00520          EXEC CICS  RETURN                                        G7W1PGM 
00521                     END-EXEC.                                     G7W1PGM 
00522                                                                   G7W1PGM 
00523      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7W1PGM 
00524                                                                   G7W1PGM 
00525      IF  WS-02-MY-EIBTRNID                                        G7W1PGM 
00526      THEN                                                         G7W1PGM 
00527          PERFORM  2000-000-PROCESS-INPUT                          G7W1PGM 
00528      ELSE                                                         G7W1PGM 
00529          PERFORM  1000-000-DISPLAY-SCREEN.                        G7W1PGM 
00530                                                                   G7W1PGM 
00531                                                                   G7W1PGM 
00532 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7W1PGM 
00533                                                                   G7W1PGM 
00534      MOVE WS-01-ABCODE-W1L1     TO WS-01-ABCODE                   G7W1PGM 
00535      MOVE WS-01-ABCODE-W1L1-MSG TO WS-01-ABCODE-MSG               G7W1PGM 
00536      PERFORM  9999-000-ABEND-THE-TASK.                            G7W1PGM 
00537                                                                   G7W1PGM 
00538      GOBACK.                                                      G7W1PGM 
00539                                                                   G7W1PGM 
00540                                                                   G7W1PGM 
00541  0000-900-EXIT.                                                   G7W1PGM 
00542      EXIT.                                                        G7W1PGM 
00543 /***************************************************************  G7W1PGM 
00544 *                                                              *  G7W1PGM 
00545 * 1000  DISPLAY INITIAL SCREEN                                 *  G7W1PGM 
00546 *                                                              *  G7W1PGM 
00547 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7W1PGM 
00548 *                                                              *  G7W1PGM 
00549 ****************************************************************  G7W1PGM 
00550  1000-000-DISPLAY-SCREEN        SECTION.                          G7W1PGM 
00551  1000-010.                                                        G7W1PGM 
00552                                                                   G7W1PGM 
00553 *------- D129     MOVE LOW VALUES TO SCREEN FOR FIRST DISPLAY     G7W1PGM 
00554 *                                                                 G7W1PGM 
00555      MOVE LOW-VALUES TO G7W1I01I.                                 G7W1PGM 
00556                                                                   G7W1PGM 
00557 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7W1PGM 
00558                                                                   G7W1PGM 
00559      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7W1PGM 
00560          MOVE WS-01-ABCODE-W1P1     TO WS-01-ABCODE               G7W1PGM 
00561          MOVE WS-01-ABCODE-W1P1-MSG TO WS-01-ABCODE-MSG           G7W1PGM 
00562          PERFORM 9999-000-ABEND-THE-TASK.                         G7W1PGM 
00563                                                                   G7W1PGM 
00564                                                                   G7W1PGM 
00565 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7W1PGM 
00566                                                                   G7W1PGM 
00567      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7W1PGM 
00568                                         + GC-GCBENPRV-FIXED-LEN   G7W1PGM 
00569                                         + GC-GCBENPRV-VARY-LEN.   G7W1PGM 
00570                                                                   G7W1PGM 
00571                                                                   G7W1PGM 
00572 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7W1PGM 
00573                                                                   G7W1PGM 
00574      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7W1PGM 
00575          MOVE WS-01-ABCODE-W1P2     TO WS-01-ABCODE               G7W1PGM 
00576          MOVE WS-01-ABCODE-W1P2-MSG TO WS-01-ABCODE-MSG           G7W1PGM 
00577          PERFORM 9999-000-ABEND-THE-TASK.                         G7W1PGM 
00578                                                                   G7W1PGM 
00579                                                                   G7W1PGM 
00580 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7W1PGM 
00581 *          BY CALLER IN COMMAREA.                                 G7W1PGM 
00582                                                                   G7W1PGM 
00583      MOVE WRK-PLAN-CODE                      TO S1PLNCDO.         G7W1PGM 
00584      MOVE WRK-GROUP-NUM                      TO S1GRPNOO.         G7W1PGM 
00585      MOVE WRK-SECTION-NUM                    TO S1SECNOO.         G7W1PGM 
00586      MOVE WRK-PKG-CODE                       TO S1PKGCDO.         G7W1PGM 
00587      MOVE WRK-PROV-CTL                       TO S1PRVO.           G7W1PGM 
00588      MOVE WRK-FAM-REL-LEVEL                  TO S1FRLO.           G7W1PGM 
00589      MOVE WRK-L-O-B                          TO S1LOBO.           G7W1PGM 
00590                                                                   G7W1PGM 
00591      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7W1PGM 
00592      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7W1PGM 
00593      IF  HGADATE-RETURN = ZEROS                                   G7W1PGM 
00594      THEN                                                         G7W1PGM 
00595          MOVE DFHBMASF                       TO S1EFFDTA          G7W1PGM 
00596          MOVE HGADATE-DATE2                  TO S1EFFDTO          G7W1PGM 
00597      ELSE                                                         G7W1PGM 
00598          MOVE DFHBMABF                       TO S1EFFDTA          G7W1PGM 
00599          MOVE HGADATE-JULIAN1                TO S1EFFDTO.         G7W1PGM 
00600                                                                   G7W1PGM 
00601      MOVE GCP-PROVN-ID                       TO S1BPVIDO.         G7W1PGM 
00602                                                                   G7W1PGM 
00603      MOVE GPW-HOSP-ADM-RESTRN-IND            TO S1HADMRO.         G7W1PGM 
00604                                                                   G7W1PGM 
00605      IF  GPW-STAY-CD = ZEROS                                      G7W1PGM 
00606          MOVE WS-02-HEX-F00000               TO S1STYCDO          G7W1PGM 
00607      ELSE                                                         G7W1PGM 
00608          MOVE GPW-STAY-CD                    TO WS-02-STAY-CD     G7W1PGM 
00609          MOVE WS-02-STAY-CD-X                TO S1STYCDO.         G7W1PGM 
00610                                                                   G7W1PGM 
00611      MOVE  GPW-HOSP-COND-RELATSP-IND         TO S1HCNDRO.         G7W1PGM 
00612      MOVE  GPW-DAYS-RDCN-RAT-IND             TO S1RDDYIO.         G7W1PGM 
00613                                                                   G7W1PGM 
00614 *    MOVE  GPW-DAYS-RDCN-RAT-BASIC-APL       TO WS-02-RATIO.      G7W1PGM 
00615 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV1BO.         G7W1PGM 
00616 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS1BO.         G7W1PGM 
00617                                                                   G7W1PGM 
00618 *    D129     ADDED FOR CONVERSION.                               G7W1PGM 
00619 *                                                                 G7W1PGM 
00620      MOVE  GPW-DAYS-RDCN-RAT-BASIC-APL    TO                      G7W1PGM 
00621            WS-02-DAYS-RDCN-RAT-BASIC-APL.                         G7W1PGM 
00622      MOVE  WS-02-DAYS-RDCN-RAT-BASIC-APL  TO WS-02-DISP-3POS-DEC. G7W1PGM 
00623      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRB1O.            G7W1PGM 
00624                                                                   G7W1PGM 
00625 *    MOVE  GPW-DAYS-RDCN-RAT-BASIC-BASE      TO WS-02-RATIO.      G7W1PGM 
00626 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV2BO.         G7W1PGM 
00627 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS2BO.         G7W1PGM 
00628                                                                   G7W1PGM 
00629 *    D129     ADDED FOR CONVERSION.                               G7W1PGM 
00630 *                                                                 G7W1PGM 
00631      MOVE  GPW-DAYS-RDCN-RAT-BASIC-BASE   TO                      G7W1PGM 
00632            WS-02-DAYS-RDCN-RAT-BASIC-BASE.                        G7W1PGM 
00633      MOVE  WS-02-DAYS-RDCN-RAT-BASIC-BASE TO WS-02-DISP-3POS-DEC. G7W1PGM 
00634      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRB2O.            G7W1PGM 
00635                                                                   G7W1PGM 
00636 *    MOVE  GPW-DAYS-RDCN-RAT-SEC-APL         TO WS-02-RATIO.      G7W1PGM 
00637 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV1SO.         G7W1PGM 
00638 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS1SO.         G7W1PGM 
00639                                                                   G7W1PGM 
00640 *    D129     ADDED FOR CONVERSION.                               G7W1PGM 
00641 *                                                                 G7W1PGM 
00642      MOVE  GPW-DAYS-RDCN-RAT-SEC-APL      TO                      G7W1PGM 
00643            WS-02-DAYS-RDCN-RAT-SEC-APL.                           G7W1PGM 
00644      MOVE  WS-02-DAYS-RDCN-RAT-SEC-APL    TO WS-02-DISP-3POS-DEC. G7W1PGM 
00645      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRS1O.            G7W1PGM 
00646                                                                   G7W1PGM 
00647 *    MOVE  GPW-DAYS-RDCN-RAT-SEC-BASE        TO WS-02-RATIO.      G7W1PGM 
00648 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV2SO.         G7W1PGM 
00649 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS2SO.         G7W1PGM 
00650                                                                   G7W1PGM 
00651 *    D129     ADDED FOR CONVERSION.                               G7W1PGM 
00652 *                                                                 G7W1PGM 
00653      MOVE  GPW-DAYS-RDCN-RAT-SEC-BASE     TO                      G7W1PGM 
00654            WS-02-DAYS-RDCN-RAT-SEC-BASE.                          G7W1PGM 
00655      MOVE  WS-02-DAYS-RDCN-RAT-SEC-BASE   TO WS-02-DISP-3POS-DEC. G7W1PGM 
00656      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRS2O.            G7W1PGM 
00657                                                                   G7W1PGM 
00658      IF  GPW-HSP-ADM-RESTRN-DAYS = ZEROS                          G7W1PGM 
00659          MOVE WS-02-HEX-F00000               TO S1HADRDO          G7W1PGM 
00660      ELSE                                                         G7W1PGM 
00661          MOVE   GPW-HSP-ADM-RESTRN-DAYS      TO                   G7W1PGM 
00662               WS-02-HSP-ADM-RESTRN-DAYS                           G7W1PGM 
00663          MOVE WS-02-HSP-ADM-RESTRN-DAYS-X    TO S1HADRDO.         G7W1PGM 
00664                                                                   G7W1PGM 
00665      MOVE  GPW-DRUG-ELIG-MEMB-CLS-OVRD       TO S1DECOIO.         G7W1PGM 
00666      MOVE  GPW-ALCO-ELIG-MEMB-CLS-OVRD       TO S1AECOIO.         G7W1PGM 
00667      MOVE  GPW-NORM-NWBORN-OVRD-IND          TO S1NNOIO.          G7W1PGM 
00668      MOVE  GPW-STAY-CODE-IND                 TO S1SCDIO.          G7W1PGM 
00669      MOVE  GPW-TRNS-SEX-REST-OVRD-IND        TO S1TRSIO.          G7W1PGM 
00670                                                                   G7W1PGM 
00671                                                                   G7W1PGM 
00672                                                                   G7W1PGM 
00673 *------- SEND INITIAL SCREEN ------------------------------------*G7W1PGM 
00674                                                                   G7W1PGM 
00675      MOVE  -1 TO  S1HADMRL.                                       G7W1PGM 
00676      PERFORM 9100-000-SEND-THEN-RETURN.                           G7W1PGM 
00677                                                                   G7W1PGM 
00678                                                                   G7W1PGM 
00679  1000-900-EXIT.                                                   G7W1PGM 
00680      EXIT.                                                        G7W1PGM 
00681 /***************************************************************  G7W1PGM 
00682 *                                                              *  G7W1PGM 
00683 * 2000    P R O C E S S    I N P U T                           *  G7W1PGM 
00684 *                                                              *  G7W1PGM 
00685 ****************************************************************  G7W1PGM 
00686  2000-000-PROCESS-INPUT         SECTION.                          G7W1PGM 
00687  2000-010.                                                        G7W1PGM 
00688                                                                   G7W1PGM 
00689 *------ VALIDATE PFKEY USAGE ------------------------------------*G7W1PGM 
00690                                                                   G7W1PGM 
00691      IF  EIBAID = DFHENTER OR                                     G7W1PGM 
00692                   DFHPF3   OR  DFHPF15 OR                         G7W1PGM 
00693                   DFHPF4   OR  DFHPF16 OR                         G7W1PGM 
00694                   DFHPF6   OR  DFHPF18 OR                         G7W1PGM 
00695                   DFHPF7   OR  DFHPF19 OR                         G7W1PGM 
00696                   DFHPF8   OR  DFHPF20                            G7W1PGM 
00697      THEN                                                         G7W1PGM 
00698          NEXT SENTENCE                                            G7W1PGM 
00699      ELSE                                                         G7W1PGM 
00700          SET WT-01-INDEX TO +01                                   G7W1PGM 
00701          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W1PGM 
00702          PERFORM 9100-000-SEND-THEN-RETURN.                       G7W1PGM 
00703                                                                   G7W1PGM 
00704                                                                   G7W1PGM 
00705                                                                   G7W1PGM 
00706      EXEC CICS  HANDLE CONDITION                                  G7W1PGM 
00707                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7W1PGM 
00708                        END-EXEC.                                  G7W1PGM 
00709                                                                   G7W1PGM 
00710                                                                   G7W1PGM 
00711      EXEC CICS  RECEIVE MAP   ('G7W1I01')                         G7W1PGM 
00712                         MAPSET('G7W1SET')                         G7W1PGM 
00713                         END-EXEC.                                 G7W1PGM 
00714                                                                   G7W1PGM 
00715                                                                   G7W1PGM 
00716      IF  S1FUNCI  NOT = 'G7W1'  OR                                G7W1PGM 
00717          S1SCRNI  NOT = '007W01'                                  G7W1PGM 
00718          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7W1PGM 
00719                                                                   G7W1PGM 
00720                                                                   G7W1PGM 
00721 *--- RETURN TO GCPS MENU? ---------------------------------------*G7W1PGM 
00722                                                                   G7W1PGM 
00723      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7W1PGM 
00724          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7W1PGM 
00725                                                                   G7W1PGM 
00726 *--- PROCESS SCREEN FIELDS --------------------------------------*G7W1PGM 
00727                                                                   G7W1PGM 
00728      PERFORM 2100-000-FIELD-EDITS.                                G7W1PGM 
00729                                                                   G7W1PGM 
00730      IF  WS-02-SCREEN-HAS-ERRORS                                  G7W1PGM 
00731          PERFORM 9100-000-SEND-THEN-RETURN.                       G7W1PGM 
00732                                                                   G7W1PGM 
00733      PERFORM 2200-000-LOGICAL-EDITS.                              G7W1PGM 
00734                                                                   G7W1PGM 
00735      IF  WS-02-SCREEN-HAS-ERRORS                                  G7W1PGM 
00736          PERFORM 9100-000-SEND-THEN-RETURN.                       G7W1PGM 
00737                                                                   G7W1PGM 
00738      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7W1PGM 
00739                                                                   G7W1PGM 
00740      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7W1PGM 
00741                                                                   G7W1PGM 
00742                                                                   G7W1PGM 
00743  2000-900-EXIT.                                                   G7W1PGM 
00744      EXIT.                                                        G7W1PGM 
00745 /***************************************************************  G7W1PGM 
00746 *                                                              *  G7W1PGM 
00747 * 2100  DO SCREEN FIELD EDITS                                  *  G7W1PGM 
00748 *                                                              *  G7W1PGM 
00749 ****************************************************************  G7W1PGM 
00750  2100-000-FIELD-EDITS           SECTION.                          G7W1PGM 
00751  2100-010.                                                        G7W1PGM 
00752                                                                   G7W1PGM 
00753 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7W1PGM 
00754                                                                   G7W1PGM 
00755      MOVE DFHBMUNF TO  S1HADMRA                                   G7W1PGM 
00756                        S1STYCDA                                   G7W1PGM 
00757                        S1HCNDRA                                   G7W1PGM 
00758                        S1RDDYIA                                   G7W1PGM 
00759                        S1DRRB1A                                   G7W1PGM 
00760                        S1DRRB2A                                   G7W1PGM 
00761                        S1DRRS1A                                   G7W1PGM 
00762                        S1DRRS2A                                   G7W1PGM 
00763                        S1HADRDA                                   G7W1PGM 
00764                        S1DECOIA                                   G7W1PGM 
00765                        S1AECOIA                                   G7W1PGM 
00766                        S1NNOIA                                    G7W1PGM 
00767                        S1NNOIA                                    G7W1PGM 
00768                        S1SCDIA                                    G7W1PGM 
00769                        S1TRSIA.                                   G7W1PGM 
00770                                                                   G7W1PGM 
00771      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7W1PGM 
00772                                                                   G7W1PGM 
00773                                                                   G7W1PGM 
00774 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION IND ----------*G7W1PGM 
00775 *   1. ALPHANUMERIC                                               G7W1PGM 
00776 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
00777                                                                   G7W1PGM 
00778      MOVE  S1HADMRI TO WS-02-CLASS-TEST-AREA.                     G7W1PGM 
00779      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
00780      THEN                                                         G7W1PGM 
00781          MOVE  S1HADMRI TO GCVI-VALUE                             G7W1PGM 
00782          MOVE  'BPAA01' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
00783          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
00784          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
00785          THEN                                                     G7W1PGM 
00786              MOVE  -1        TO  S1HADMRL                         G7W1PGM 
00787              MOVE  DFHBMUBF  TO  S1HADMRA                         G7W1PGM 
00788              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
00789              THEN                                                 G7W1PGM 
00790                  NEXT SENTENCE                                    G7W1PGM 
00791              ELSE                                                 G7W1PGM 
00792                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
00793                  SET WT-01-INDEX TO +09                           G7W1PGM 
00794                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00795          ELSE                                                     G7W1PGM 
00796              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
00797              THEN                                                 G7W1PGM 
00798                  MOVE  DFHBMUBF  TO  S1HADMRA                     G7W1PGM 
00799              ELSE                                                 G7W1PGM 
00800                  NEXT SENTENCE                                    G7W1PGM 
00801      ELSE                                                         G7W1PGM 
00802          MOVE  -1        TO  S1HADMRL                             G7W1PGM 
00803          MOVE  DFHBMUBF  TO  S1HADMRA                             G7W1PGM 
00804          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
00805          THEN                                                     G7W1PGM 
00806              NEXT SENTENCE                                        G7W1PGM 
00807          ELSE                                                     G7W1PGM 
00808              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
00809              SET WT-01-INDEX TO +08                               G7W1PGM 
00810              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
00811                                                                   G7W1PGM 
00812                                                                   G7W1PGM 
00813 *-- VALIDATE ------ STAY CODE -----------------------------------*G7W1PGM 
00814 *   1. NUMERICS                                                   G7W1PGM 
00815                                                                   G7W1PGM 
00816      IF  S1STYCDI IS NUMERIC                                      G7W1PGM 
00817      THEN                                                         G7W1PGM 
00818          NEXT SENTENCE                                            G7W1PGM 
00819      ELSE                                                         G7W1PGM 
00820          MOVE  -1        TO  S1STYCDL                             G7W1PGM 
00821          MOVE  DFHBMUBF  TO  S1STYCDA                             G7W1PGM 
00822          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
00823          THEN                                                     G7W1PGM 
00824              NEXT SENTENCE                                        G7W1PGM 
00825          ELSE                                                     G7W1PGM 
00826              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
00827              SET WT-01-INDEX TO +13                               G7W1PGM 
00828              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
00829                                                                   G7W1PGM 
00830                                                                   G7W1PGM 
00831 *-- VALIDATE ------ HOSP. COND. RELATIONSHIP IND ----------------*G7W1PGM 
00832 *   1. ALPHANUMERIC                                               G7W1PGM 
00833 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
00834                                                                   G7W1PGM 
00835      MOVE  S1HCNDRI TO WS-02-CLASS-TEST-AREA.                     G7W1PGM 
00836      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
00837      THEN                                                         G7W1PGM 
00838          MOVE  S1HCNDRI TO GCVI-VALUE                             G7W1PGM 
00839          MOVE  'BPAA03' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
00840          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
00841          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
00842          THEN                                                     G7W1PGM 
00843              MOVE  -1        TO  S1HCNDRL                         G7W1PGM 
00844              MOVE  DFHBMUBF  TO  S1HCNDRA                         G7W1PGM 
00845              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
00846              THEN                                                 G7W1PGM 
00847                  NEXT SENTENCE                                    G7W1PGM 
00848              ELSE                                                 G7W1PGM 
00849                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
00850                  SET WT-01-INDEX TO +09                           G7W1PGM 
00851                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00852          ELSE                                                     G7W1PGM 
00853              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
00854              THEN                                                 G7W1PGM 
00855                  MOVE  DFHBMUBF  TO  S1HCNDRA                     G7W1PGM 
00856              ELSE                                                 G7W1PGM 
00857                  NEXT SENTENCE                                    G7W1PGM 
00858      ELSE                                                         G7W1PGM 
00859          MOVE  -1        TO  S1HCNDRL                             G7W1PGM 
00860          MOVE  DFHBMUBF  TO  S1HCNDRA                             G7W1PGM 
00861          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
00862          THEN                                                     G7W1PGM 
00863              NEXT SENTENCE                                        G7W1PGM 
00864          ELSE                                                     G7W1PGM 
00865              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
00866              SET WT-01-INDEX TO +08                               G7W1PGM 
00867              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
00868                                                                   G7W1PGM 
00869                                                                   G7W1PGM 
00870 *-- VALIDATE ------ DAYS REDUCTION RATIO IND --------------------*G7W1PGM 
00871 *   1. ALPHANUMERIC                                               G7W1PGM 
00872 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
00873                                                                   G7W1PGM 
00874      MOVE  S1RDDYII TO WS-02-CLASS-TEST-AREA.                     G7W1PGM 
00875      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
00876      THEN                                                         G7W1PGM 
00877          MOVE  S1RDDYII TO GCVI-VALUE                             G7W1PGM 
00878          MOVE  'BPAA05' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
00879          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
00880          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
00881          THEN                                                     G7W1PGM 
00882              MOVE  -1        TO  S1RDDYIL                         G7W1PGM 
00883              MOVE  DFHBMUBF  TO  S1RDDYIA                         G7W1PGM 
00884              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
00885              THEN                                                 G7W1PGM 
00886                  NEXT SENTENCE                                    G7W1PGM 
00887              ELSE                                                 G7W1PGM 
00888                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
00889                  SET WT-01-INDEX TO +09                           G7W1PGM 
00890                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00891          ELSE                                                     G7W1PGM 
00892              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
00893              THEN                                                 G7W1PGM 
00894                  MOVE  DFHBMUBF  TO  S1RDDYIA                     G7W1PGM 
00895              ELSE                                                 G7W1PGM 
00896                  NEXT SENTENCE                                    G7W1PGM 
00897      ELSE                                                         G7W1PGM 
00898          MOVE  -1        TO  S1RDDYIL                             G7W1PGM 
00899          MOVE  DFHBMUBF  TO  S1RDDYIA                             G7W1PGM 
00900          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
00901          THEN                                                     G7W1PGM 
00902              NEXT SENTENCE                                        G7W1PGM 
00903          ELSE                                                     G7W1PGM 
00904              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
00905              SET WT-01-INDEX TO +08                               G7W1PGM 
00906              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
00907                                                                   G7W1PGM 
00908                                                                   G7W1PGM 
00909 *-- VALIDATE ------ DAYS REDUCTION RATIO BASIC (2 FIELDS) -------*G7W1PGM 
00910 *  D129                                                           G7W1PGM 
00911                                                                   G7W1PGM 
00912      MOVE S1DRRB1I TO D-C-RECEIVE-FIELD.                          G7W1PGM 
00913      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7W1PGM 
00914      MOVE '00' TO D-C-RETURN-CODE.                                G7W1PGM 
00915      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7W1PGM 
00916      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7W1PGM 
00917      IF D-C-RETURN-CODE = '00'                                    G7W1PGM 
00918          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7W1PGM 
00919              MOVE -1       TO S1DRRB1L                            G7W1PGM 
00920              MOVE DFHBMUBF TO S1DRRB1A                            G7W1PGM 
00921              IF WS-02-SCREEN-HAS-ERRORS                           G7W1PGM 
00922                  NEXT SENTENCE                                    G7W1PGM 
00923              ELSE                                                 G7W1PGM 
00924                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7W1PGM 
00925                  SET WT-01-INDEX TO +14                           G7W1PGM 
00926                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00927          ELSE                                                     G7W1PGM 
00928              MOVE D-C-RETURN-FIELD-DEC1                           G7W1PGM 
00929                TO WS-02-DAYS-RDCN-RAT-BASIC-APL                   G7W1PGM 
00930              MOVE WS-02-DAYS-RDCN-RAT-BASIC-APL                   G7W1PGM 
00931                TO WS-02-DISP-3POS-DEC                             G7W1PGM 
00932              MOVE WS-02-DISP-3POS-DEC                             G7W1PGM 
00933                TO S1DRRB1O                                        G7W1PGM 
00934      ELSE                                                         G7W1PGM 
00935          MOVE -1       TO S1DRRB1L                                G7W1PGM 
00936          MOVE DFHBMUBF TO S1DRRB1A                                G7W1PGM 
00937          IF WS-02-SCREEN-HAS-ERRORS                               G7W1PGM 
00938              NEXT SENTENCE                                        G7W1PGM 
00939          ELSE                                                     G7W1PGM 
00940              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7W1PGM 
00941              IF D-C-RETURN-CODE = '10'                            G7W1PGM 
00942                  SET WT-01-INDEX TO +13                           G7W1PGM 
00943                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00944              ELSE                                                 G7W1PGM 
00945                  SET WT-01-INDEX TO +15                           G7W1PGM 
00946                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7W1PGM 
00947                                                                   G7W1PGM 
00948                                                                   G7W1PGM 
00949      MOVE S1DRRB2I TO D-C-RECEIVE-FIELD.                          G7W1PGM 
00950      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7W1PGM 
00951      MOVE '00' TO D-C-RETURN-CODE.                                G7W1PGM 
00952      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7W1PGM 
00953      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7W1PGM 
00954      IF D-C-RETURN-CODE = '00'                                    G7W1PGM 
00955          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7W1PGM 
00956              MOVE -1       TO S1DRRB2L                            G7W1PGM 
00957              MOVE DFHBMUBF TO S1DRRB2A                            G7W1PGM 
00958              IF WS-02-SCREEN-HAS-ERRORS                           G7W1PGM 
00959                  NEXT SENTENCE                                    G7W1PGM 
00960              ELSE                                                 G7W1PGM 
00961                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7W1PGM 
00962                  SET WT-01-INDEX TO +14                           G7W1PGM 
00963                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00964          ELSE                                                     G7W1PGM 
00965              MOVE D-C-RETURN-FIELD-DEC1                           G7W1PGM 
00966                TO WS-02-DAYS-RDCN-RAT-BASIC-BASE                  G7W1PGM 
00967              MOVE WS-02-DAYS-RDCN-RAT-BASIC-BASE                  G7W1PGM 
00968                TO WS-02-DISP-3POS-DEC                             G7W1PGM 
00969              MOVE WS-02-DISP-3POS-DEC                             G7W1PGM 
00970                TO S1DRRB2O                                        G7W1PGM 
00971      ELSE                                                         G7W1PGM 
00972          MOVE -1       TO S1DRRB2L                                G7W1PGM 
00973          MOVE DFHBMUBF TO S1DRRB2A                                G7W1PGM 
00974          IF WS-02-SCREEN-HAS-ERRORS                               G7W1PGM 
00975              NEXT SENTENCE                                        G7W1PGM 
00976          ELSE                                                     G7W1PGM 
00977              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7W1PGM 
00978              IF D-C-RETURN-CODE = '10'                            G7W1PGM 
00979                  SET WT-01-INDEX TO +13                           G7W1PGM 
00980                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
00981              ELSE                                                 G7W1PGM 
00982                  SET WT-01-INDEX TO +15                           G7W1PGM 
00983                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7W1PGM 
00984                                                                   G7W1PGM 
00985 *    D-I-V-I-S-O-R                                                G7W1PGM 
00986 *    IF  S1DIV1BI IS NUMERIC                                      G7W1PGM 
00987 *    THEN                                                         G7W1PGM 
00988 *        NEXT SENTENCE                                            G7W1PGM 
00989 *    ELSE                                                         G7W1PGM 
00990 *        MOVE  -1        TO  S1DIV1BL                             G7W1PGM 
00991 *        MOVE  DFHBMUBF  TO  S1DIV1BA                             G7W1PGM 
00992 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
00993 *        THEN                                                     G7W1PGM 
00994 *            NEXT SENTENCE                                        G7W1PGM 
00995 *        ELSE                                                     G7W1PGM 
00996 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
00997 *            SET WT-01-INDEX TO +13                               G7W1PGM 
00998 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
00999                                                                   G7W1PGM 
01000 *    B-A-S-E                                                      G7W1PGM 
01001 *    IF  S1BAS1BI IS NUMERIC                                      G7W1PGM 
01002 *    THEN                                                         G7W1PGM 
01003 *        NEXT SENTENCE                                            G7W1PGM 
01004 *    ELSE                                                         G7W1PGM 
01005 *        MOVE  -1        TO  S1BAS1BL                             G7W1PGM 
01006 *        MOVE  DFHBMUBF  TO  S1BAS1BA                             G7W1PGM 
01007 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01008 *        THEN                                                     G7W1PGM 
01009 *            NEXT SENTENCE                                        G7W1PGM 
01010 *        ELSE                                                     G7W1PGM 
01011 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01012 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01013 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01014                                                                   G7W1PGM 
01015 *    D-I-V-I-S-O-R                                                G7W1PGM 
01016 *    IF  S1DIV2BI IS NUMERIC                                      G7W1PGM 
01017 *    THEN                                                         G7W1PGM 
01018 *        NEXT SENTENCE                                            G7W1PGM 
01019 *    ELSE                                                         G7W1PGM 
01020 *        MOVE  -1        TO  S1DIV2BL                             G7W1PGM 
01021 *        MOVE  DFHBMUBF  TO  S1DIV2BA                             G7W1PGM 
01022 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01023 *        THEN                                                     G7W1PGM 
01024 *            NEXT SENTENCE                                        G7W1PGM 
01025 *        ELSE                                                     G7W1PGM 
01026 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01027 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01028 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01029                                                                   G7W1PGM 
01030 *    B-A-S-E                                                      G7W1PGM 
01031 *    IF  S1BAS2BI IS NUMERIC                                      G7W1PGM 
01032 *    THEN                                                         G7W1PGM 
01033 *        NEXT SENTENCE                                            G7W1PGM 
01034 *    ELSE                                                         G7W1PGM 
01035 *        MOVE  -1        TO  S1BAS2BL                             G7W1PGM 
01036 *        MOVE  DFHBMUBF  TO  S1BAS2BA                             G7W1PGM 
01037 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01038 *        THEN                                                     G7W1PGM 
01039 *            NEXT SENTENCE                                        G7W1PGM 
01040 *        ELSE                                                     G7W1PGM 
01041 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01042 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01043 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01044                                                                   G7W1PGM 
01045                                                                   G7W1PGM 
01046 *-- VALIDATE ------ DAYS REDUCTION RATIO SECONDARY (2 FIELDS) ---*G7W1PGM 
01047 *  D129                                                           G7W1PGM 
01048                                                                   G7W1PGM 
01049      MOVE S1DRRS1I TO D-C-RECEIVE-FIELD.                          G7W1PGM 
01050      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7W1PGM 
01051      MOVE '00' TO D-C-RETURN-CODE.                                G7W1PGM 
01052      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7W1PGM 
01053      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7W1PGM 
01054      IF D-C-RETURN-CODE = '00'                                    G7W1PGM 
01055          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7W1PGM 
01056              MOVE -1       TO S1DRRS1L                            G7W1PGM 
01057              MOVE DFHBMUBF TO S1DRRS1A                            G7W1PGM 
01058              IF WS-02-SCREEN-HAS-ERRORS                           G7W1PGM 
01059                  NEXT SENTENCE                                    G7W1PGM 
01060              ELSE                                                 G7W1PGM 
01061                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7W1PGM 
01062                  SET WT-01-INDEX TO +14                           G7W1PGM 
01063                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01064          ELSE                                                     G7W1PGM 
01065              MOVE D-C-RETURN-FIELD-DEC1                           G7W1PGM 
01066                TO WS-02-DAYS-RDCN-RAT-SEC-APL                     G7W1PGM 
01067              MOVE WS-02-DAYS-RDCN-RAT-SEC-APL                     G7W1PGM 
01068                TO WS-02-DISP-3POS-DEC                             G7W1PGM 
01069              MOVE WS-02-DISP-3POS-DEC                             G7W1PGM 
01070                TO S1DRRS1O                                        G7W1PGM 
01071      ELSE                                                         G7W1PGM 
01072          MOVE -1       TO S1DRRS1L                                G7W1PGM 
01073          MOVE DFHBMUBF TO S1DRRS1A                                G7W1PGM 
01074          IF WS-02-SCREEN-HAS-ERRORS                               G7W1PGM 
01075              NEXT SENTENCE                                        G7W1PGM 
01076          ELSE                                                     G7W1PGM 
01077              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7W1PGM 
01078              IF D-C-RETURN-CODE = '10'                            G7W1PGM 
01079                  SET WT-01-INDEX TO +13                           G7W1PGM 
01080                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01081              ELSE                                                 G7W1PGM 
01082                  SET WT-01-INDEX TO +15                           G7W1PGM 
01083                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7W1PGM 
01084                                                                   G7W1PGM 
01085                                                                   G7W1PGM 
01086      MOVE S1DRRS2I TO D-C-RECEIVE-FIELD.                          G7W1PGM 
01087      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7W1PGM 
01088      MOVE '00' TO D-C-RETURN-CODE.                                G7W1PGM 
01089      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7W1PGM 
01090      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7W1PGM 
01091      IF D-C-RETURN-CODE = '00'                                    G7W1PGM 
01092          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7W1PGM 
01093              MOVE -1       TO S1DRRS2L                            G7W1PGM 
01094              MOVE DFHBMUBF TO S1DRRS2A                            G7W1PGM 
01095              IF WS-02-SCREEN-HAS-ERRORS                           G7W1PGM 
01096                  NEXT SENTENCE                                    G7W1PGM 
01097              ELSE                                                 G7W1PGM 
01098                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7W1PGM 
01099                  SET WT-01-INDEX TO +14                           G7W1PGM 
01100                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01101          ELSE                                                     G7W1PGM 
01102              MOVE D-C-RETURN-FIELD-DEC1                           G7W1PGM 
01103                TO WS-02-DAYS-RDCN-RAT-SEC-BASE                    G7W1PGM 
01104              MOVE WS-02-DAYS-RDCN-RAT-SEC-BASE                    G7W1PGM 
01105                TO WS-02-DISP-3POS-DEC                             G7W1PGM 
01106              MOVE WS-02-DISP-3POS-DEC                             G7W1PGM 
01107                TO S1DRRS2O                                        G7W1PGM 
01108      ELSE                                                         G7W1PGM 
01109          MOVE -1       TO S1DRRS2L                                G7W1PGM 
01110          MOVE DFHBMUBF TO S1DRRS2A                                G7W1PGM 
01111          IF WS-02-SCREEN-HAS-ERRORS                               G7W1PGM 
01112              NEXT SENTENCE                                        G7W1PGM 
01113          ELSE                                                     G7W1PGM 
01114              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7W1PGM 
01115              IF D-C-RETURN-CODE = '10'                            G7W1PGM 
01116                  SET WT-01-INDEX TO +13                           G7W1PGM 
01117                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01118              ELSE                                                 G7W1PGM 
01119                  SET WT-01-INDEX TO +15                           G7W1PGM 
01120                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7W1PGM 
01121                                                                   G7W1PGM 
01122                                                                   G7W1PGM 
01123 *    D-I-V-I-S-O-R                                                G7W1PGM 
01124 *    IF  S1DIV1SI IS NUMERIC                                      G7W1PGM 
01125 *    THEN                                                         G7W1PGM 
01126 *        NEXT SENTENCE                                            G7W1PGM 
01127 *    ELSE                                                         G7W1PGM 
01128 *        MOVE  -1        TO  S1DIV1SL                             G7W1PGM 
01129 *        MOVE  DFHBMUBF  TO  S1DIV1SA                             G7W1PGM 
01130 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01131 *        THEN                                                     G7W1PGM 
01132 *            NEXT SENTENCE                                        G7W1PGM 
01133 *        ELSE                                                     G7W1PGM 
01134 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01135 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01136 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01137                                                                   G7W1PGM 
01138 *    B-A-S-E                                                      G7W1PGM 
01139 *    IF  S1BAS1SI IS NUMERIC                                      G7W1PGM 
01140 *    THEN                                                         G7W1PGM 
01141 *        NEXT SENTENCE                                            G7W1PGM 
01142 *    ELSE                                                         G7W1PGM 
01143 *        MOVE  -1        TO  S1BAS1SL                             G7W1PGM 
01144 *        MOVE  DFHBMUBF  TO  S1BAS1SA                             G7W1PGM 
01145 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01146 *        THEN                                                     G7W1PGM 
01147 *            NEXT SENTENCE                                        G7W1PGM 
01148 *        ELSE                                                     G7W1PGM 
01149 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01150 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01151 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01152                                                                   G7W1PGM 
01153 *    D-I-V-I-S-O-R                                                G7W1PGM 
01154 *    IF  S1DIV2SI IS NUMERIC                                      G7W1PGM 
01155 *    THEN                                                         G7W1PGM 
01156 *        NEXT SENTENCE                                            G7W1PGM 
01157 *    ELSE                                                         G7W1PGM 
01158 *        MOVE  -1        TO  S1DIV2SL                             G7W1PGM 
01159 *        MOVE  DFHBMUBF  TO  S1DIV2SA                             G7W1PGM 
01160 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01161 *        THEN                                                     G7W1PGM 
01162 *            NEXT SENTENCE                                        G7W1PGM 
01163 *        ELSE                                                     G7W1PGM 
01164 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01165 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01166 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01167                                                                   G7W1PGM 
01168 *    B-A-S-E                                                      G7W1PGM 
01169 *    IF  S1BAS2SI IS NUMERIC                                      G7W1PGM 
01170 *    THEN                                                         G7W1PGM 
01171 *        NEXT SENTENCE                                            G7W1PGM 
01172 *    ELSE                                                         G7W1PGM 
01173 *        MOVE  -1        TO  S1BAS2SL                             G7W1PGM 
01174 *        MOVE  DFHBMUBF  TO  S1BAS2SA                             G7W1PGM 
01175 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01176 *        THEN                                                     G7W1PGM 
01177 *            NEXT SENTENCE                                        G7W1PGM 
01178 *        ELSE                                                     G7W1PGM 
01179 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01180 *            SET WT-01-INDEX TO +13                               G7W1PGM 
01181 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01182                                                                   G7W1PGM 
01183                                                                   G7W1PGM 
01184 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION DAYS ---------*G7W1PGM 
01185 *   1. NUMERICS                                                   G7W1PGM 
01186                                                                   G7W1PGM 
01187      IF  S1HADRDI IS NUMERIC                                      G7W1PGM 
01188      THEN                                                         G7W1PGM 
01189          NEXT SENTENCE                                            G7W1PGM 
01190      ELSE                                                         G7W1PGM 
01191          MOVE  -1        TO  S1HADRDL                             G7W1PGM 
01192          MOVE  DFHBMUBF  TO  S1HADRDA                             G7W1PGM 
01193          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01194          THEN                                                     G7W1PGM 
01195              NEXT SENTENCE                                        G7W1PGM 
01196          ELSE                                                     G7W1PGM 
01197              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01198              SET WT-01-INDEX TO +13                               G7W1PGM 
01199              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01200                                                                   G7W1PGM 
01201                                                                   G7W1PGM 
01202 *-- VALIDATE ------ DRUG ELIGIBLE MEMBER OVERRIED IND -----------*G7W1PGM 
01203 *   1. ALPHANUMERIC                                               G7W1PGM 
01204 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
01205                                                                   G7W1PGM 
01206      MOVE  S1DECOII TO WS-02-CLASS-TEST-AREA.                     G7W1PGM 
01207      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
01208      THEN                                                         G7W1PGM 
01209          MOVE  S1DECOII TO GCVI-VALUE                             G7W1PGM 
01210          MOVE  'BPAB04' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
01211          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
01212          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
01213          THEN                                                     G7W1PGM 
01214              MOVE  -1        TO  S1DECOIL                         G7W1PGM 
01215              MOVE  DFHBMUBF  TO  S1DECOIA                         G7W1PGM 
01216              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
01217              THEN                                                 G7W1PGM 
01218                  NEXT SENTENCE                                    G7W1PGM 
01219              ELSE                                                 G7W1PGM 
01220                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01221                  SET WT-01-INDEX TO +09                           G7W1PGM 
01222                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01223          ELSE                                                     G7W1PGM 
01224              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
01225              THEN                                                 G7W1PGM 
01226                  MOVE  DFHBMUBF  TO  S1DECOIA                     G7W1PGM 
01227              ELSE                                                 G7W1PGM 
01228                  NEXT SENTENCE                                    G7W1PGM 
01229      ELSE                                                         G7W1PGM 
01230          MOVE  -1        TO  S1DECOIL                             G7W1PGM 
01231          MOVE  DFHBMUBF  TO  S1DECOIA                             G7W1PGM 
01232          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01233          THEN                                                     G7W1PGM 
01234              NEXT SENTENCE                                        G7W1PGM 
01235          ELSE                                                     G7W1PGM 
01236              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01237              SET WT-01-INDEX TO +08                               G7W1PGM 
01238              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01239                                                                   G7W1PGM 
01240                                                                   G7W1PGM 
01241 *-- VALIDATE ------ ALCOHOL ELIGIBLE MEMBER OVERRIED IND --------*G7W1PGM 
01242 *   1. ALPHANUMERIC                                               G7W1PGM 
01243 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
01244                                                                   G7W1PGM 
01245      MOVE  S1AECOII TO WS-02-CLASS-TEST-AREA.                     G7W1PGM 
01246      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
01247      THEN                                                         G7W1PGM 
01248          MOVE  S1AECOII TO GCVI-VALUE                             G7W1PGM 
01249          MOVE  'BPAB03' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
01250          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
01251          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
01252          THEN                                                     G7W1PGM 
01253              MOVE  -1        TO  S1AECOIL                         G7W1PGM 
01254              MOVE  DFHBMUBF  TO  S1AECOIA                         G7W1PGM 
01255              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
01256              THEN                                                 G7W1PGM 
01257                  NEXT SENTENCE                                    G7W1PGM 
01258              ELSE                                                 G7W1PGM 
01259                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01260                  SET WT-01-INDEX TO +09                           G7W1PGM 
01261                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01262          ELSE                                                     G7W1PGM 
01263              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
01264              THEN                                                 G7W1PGM 
01265                  MOVE  DFHBMUBF  TO  S1AECOIA                     G7W1PGM 
01266              ELSE                                                 G7W1PGM 
01267                  NEXT SENTENCE                                    G7W1PGM 
01268      ELSE                                                         G7W1PGM 
01269          MOVE  -1        TO  S1AECOIL                             G7W1PGM 
01270          MOVE  DFHBMUBF  TO  S1AECOIA                             G7W1PGM 
01271          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01272          THEN                                                     G7W1PGM 
01273              NEXT SENTENCE                                        G7W1PGM 
01274          ELSE                                                     G7W1PGM 
01275              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01276              SET WT-01-INDEX TO +08                               G7W1PGM 
01277              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01278                                                                   G7W1PGM 
01279                                                                   G7W1PGM 
01280 *-- VALIDATE ------ NORMAL NEWBORN OVERRIDE INDICATOR -----------*G7W1PGM 
01281 *   1. ALPHANUMERIC                                               G7W1PGM 
01282 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
01283                                                                   G7W1PGM 
01284      MOVE  S1NNOII TO WS-02-CLASS-TEST-AREA.                      G7W1PGM 
01285      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
01286      THEN                                                         G7W1PGM 
01287          MOVE  S1NNOII TO GCVI-VALUE                              G7W1PGM 
01288          MOVE  'BPAB05' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
01289          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
01290          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
01291          THEN                                                     G7W1PGM 
01292              MOVE  -1        TO  S1NNOIL                          G7W1PGM 
01293              MOVE  DFHBMUBF  TO  S1NNOIA                          G7W1PGM 
01294              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
01295              THEN                                                 G7W1PGM 
01296                  NEXT SENTENCE                                    G7W1PGM 
01297              ELSE                                                 G7W1PGM 
01298                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01299                  SET WT-01-INDEX TO +09                           G7W1PGM 
01300                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01301          ELSE                                                     G7W1PGM 
01302              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
01303              THEN                                                 G7W1PGM 
01304                  MOVE  DFHBMUBF  TO  S1NNOIA                      G7W1PGM 
01305              ELSE                                                 G7W1PGM 
01306                  NEXT SENTENCE                                    G7W1PGM 
01307      ELSE                                                         G7W1PGM 
01308          MOVE  -1        TO  S1NNOIL                              G7W1PGM 
01309          MOVE  DFHBMUBF  TO  S1NNOIA                              G7W1PGM 
01310          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01311          THEN                                                     G7W1PGM 
01312              NEXT SENTENCE                                        G7W1PGM 
01313          ELSE                                                     G7W1PGM 
01314              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01315              SET WT-01-INDEX TO +08                               G7W1PGM 
01316              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01317                                                                   G7W1PGM 
01318                                                                   G7W1PGM 
01319 *-- VALIDATE ------ STAY CODE INDICATOR -------------------------*G7W1PGM 
01320 *   1. ALPHANUMERIC                                               G7W1PGM 
01321 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
01322                                                                   G7W1PGM 
01323      MOVE  S1SCDII TO WS-02-CLASS-TEST-AREA.                      G7W1PGM 
01324      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
01325      THEN                                                         G7W1PGM 
01326          MOVE  S1SCDII TO GCVI-VALUE                              G7W1PGM 
01327          MOVE  'BPAA12' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
01328          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
01329          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
01330          THEN                                                     G7W1PGM 
01331              MOVE  -1        TO  S1SCDIL                          G7W1PGM 
01332              MOVE  DFHBMUBF  TO  S1SCDIA                          G7W1PGM 
01333              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
01334              THEN                                                 G7W1PGM 
01335                  NEXT SENTENCE                                    G7W1PGM 
01336              ELSE                                                 G7W1PGM 
01337                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01338                  SET WT-01-INDEX TO +09                           G7W1PGM 
01339                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01340          ELSE                                                     G7W1PGM 
01341              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
01342              THEN                                                 G7W1PGM 
01343                  MOVE  DFHBMUBF  TO  S1SCDIA                      G7W1PGM 
01344              ELSE                                                 G7W1PGM 
01345                  NEXT SENTENCE                                    G7W1PGM 
01346      ELSE                                                         G7W1PGM 
01347          MOVE  -1        TO  S1SCDIL                              G7W1PGM 
01348          MOVE  DFHBMUBF  TO  S1SCDIA                              G7W1PGM 
01349          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01350          THEN                                                     G7W1PGM 
01351              NEXT SENTENCE                                        G7W1PGM 
01352          ELSE                                                     G7W1PGM 
01353              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01354              SET WT-01-INDEX TO +08                               G7W1PGM 
01355              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01356                                                                   G7W1PGM 
01357                                                                   G7W1PGM 
01358 *-- VALIDATE ------ TRANS-SEXUAL RESTR. OVERRIDE IND. -----------*G7W1PGM 
01359 *   1. ALPHANUMERIC                                               G7W1PGM 
01360 *   2. FIELD VALIDATION SUB-SYSTEM                                G7W1PGM 
01361                                                                   G7W1PGM 
01362      MOVE  S1TRSII TO WS-02-CLASS-TEST-AREA.                      G7W1PGM 
01363      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7W1PGM 
01364      THEN                                                         G7W1PGM 
01365          MOVE  S1TRSII TO GCVI-VALUE                              G7W1PGM 
01366          MOVE  'BPAB04' TO GCVI-FIELDS-KEY-ID                     G7W1PGM 
01367          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7W1PGM 
01368          IF  GCVI-VALUE-NOT-FOUND                                 G7W1PGM 
01369          THEN                                                     G7W1PGM 
01370              MOVE  -1        TO  S1TRSIL                          G7W1PGM 
01371              MOVE  DFHBMUBF  TO  S1TRSIA                          G7W1PGM 
01372              IF  WS-02-SCREEN-HAS-ERRORS                          G7W1PGM 
01373              THEN                                                 G7W1PGM 
01374                  NEXT SENTENCE                                    G7W1PGM 
01375              ELSE                                                 G7W1PGM 
01376                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01377                  SET WT-01-INDEX TO +09                           G7W1PGM 
01378                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01379          ELSE                                                     G7W1PGM 
01380              IF  GCVI-VALUE-NOT-LOADED                            G7W1PGM 
01381              THEN                                                 G7W1PGM 
01382                  MOVE  DFHBMUBF  TO  S1TRSIA                      G7W1PGM 
01383              ELSE                                                 G7W1PGM 
01384                  NEXT SENTENCE                                    G7W1PGM 
01385      ELSE                                                         G7W1PGM 
01386          MOVE  -1        TO  S1TRSIL                              G7W1PGM 
01387          MOVE  DFHBMUBF  TO  S1TRSIA                              G7W1PGM 
01388          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01389          THEN                                                     G7W1PGM 
01390              NEXT SENTENCE                                        G7W1PGM 
01391          ELSE                                                     G7W1PGM 
01392              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01393              SET WT-01-INDEX TO +08                               G7W1PGM 
01394              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7W1PGM 
01395                                                                   G7W1PGM 
01396                                                                   G7W1PGM 
01397                                                                   G7W1PGM 
01398  2100-900-EXIT.                                                   G7W1PGM 
01399      EXIT.                                                        G7W1PGM 
01400 /***************************************************************  G7W1PGM 
01401 *                                                              *  G7W1PGM 
01402 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7W1PGM 
01403 *                                                              *  G7W1PGM 
01404 ****************************************************************  G7W1PGM 
01405  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7W1PGM 
01406  2110-010.                                                        G7W1PGM 
01407                                                                   G7W1PGM 
01408      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7W1PGM 
01409                                                                   G7W1PGM 
01410      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7W1PGM 
01411                       COMMAREA(GCVIOPGM-PARM-LIST)                G7W1PGM 
01412                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7W1PGM 
01413                       END-EXEC.                                   G7W1PGM 
01414                                                                   G7W1PGM 
01415      IF  GCVI-VALUE-NOT-LOADED                                    G7W1PGM 
01416          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7W1PGM 
01417                                                                   G7W1PGM 
01418  2110-900-EXIT.                                                   G7W1PGM 
01419      EXIT.                                                        G7W1PGM 
01420 /***************************************************************  G7W1PGM 
01421 *                                                              *  G7W1PGM 
01422 * 2200  DO SCREEN LOGICAL EDITS                                *  G7W1PGM 
01423 *                                                              *  G7W1PGM 
01424 ****************************************************************  G7W1PGM 
01425  2200-000-LOGICAL-EDITS         SECTION.                          G7W1PGM 
01426  2200-010.                                                        G7W1PGM 
01427                                                                   G7W1PGM 
01428 *----------------------------------------------------------------*G7W1PGM 
01429 *                                                                *G7W1PGM 
01430 *  IF   HOSPITAL ADMISSION RESTRICTION IND (S1HADMR) > ZERO      *G7W1PGM 
01431 *                                                                *G7W1PGM 
01432 *  THEN HOSPITAL ADMISSION RESTRICTION DAYS(S1HADRD):            *G7W1PGM 
01433 *                      MUST BE > ZERO.                           *G7W1PGM 
01434 *                                                                *G7W1PGM 
01435 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7W1PGM 
01436 *                                                                *G7W1PGM 
01437 *----------------------------------------------------------------*G7W1PGM 
01438                                                                   G7W1PGM 
01439      IF  S1HADMRI     > ZEROS                                     G7W1PGM 
01440          AND                                                      G7W1PGM 
01441          S1HADRDI NOT > ZEROS                                     G7W1PGM 
01442      THEN                                                         G7W1PGM 
01443          MOVE  -1        TO  S1HADRDL                             G7W1PGM 
01444          MOVE  DFHBMUBF  TO  S1HADRDA                             G7W1PGM 
01445                              S1HADMRA                             G7W1PGM 
01446          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01447          THEN                                                     G7W1PGM 
01448              NEXT SENTENCE                                        G7W1PGM 
01449          ELSE                                                     G7W1PGM 
01450              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01451              SET WT-01-INDEX TO +02                               G7W1PGM 
01452              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W1PGM 
01453      ELSE                                                         G7W1PGM 
01454          NEXT SENTENCE.                                           G7W1PGM 
01455                                                                   G7W1PGM 
01456      IF  S1HADRDI     > ZEROS                                     G7W1PGM 
01457          AND                                                      G7W1PGM 
01458          S1HADMRI NOT > ZEROS                                     G7W1PGM 
01459      THEN                                                         G7W1PGM 
01460          MOVE  -1        TO  S1HADMRL                             G7W1PGM 
01461          MOVE  DFHBMUBF  TO  S1HADMRA                             G7W1PGM 
01462                              S1HADRDA                             G7W1PGM 
01463          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01464          THEN                                                     G7W1PGM 
01465              NEXT SENTENCE                                        G7W1PGM 
01466          ELSE                                                     G7W1PGM 
01467              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01468              SET WT-01-INDEX TO +03                               G7W1PGM 
01469              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W1PGM 
01470      ELSE                                                         G7W1PGM 
01471          NEXT SENTENCE.                                           G7W1PGM 
01472                                                                   G7W1PGM 
01473                                                                   G7W1PGM 
01474 *----------------------------------------------------------------*G7W1PGM 
01475 *                                                                *G7W1PGM 
01476 *  IF   STAY CODE INDICATOR (S1SCDI) > ZERO                      *G7W1PGM 
01477 *                                                                *G7W1PGM 
01478 *  THEN STAY CODE (S1STYCD) MUST BE > ZERO                       *G7W1PGM 
01479 *                                                                *G7W1PGM 
01480 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7W1PGM 
01481 *                                                                *G7W1PGM 
01482 *----------------------------------------------------------------*G7W1PGM 
01483                                                                   G7W1PGM 
01484      IF  S1SCDII      > ZEROS                                     G7W1PGM 
01485          AND                                                      G7W1PGM 
01486          S1STYCDI NOT > ZEROS                                     G7W1PGM 
01487      THEN                                                         G7W1PGM 
01488          MOVE  -1        TO  S1STYCDL                             G7W1PGM 
01489          MOVE  DFHBMUBF  TO  S1STYCDA                             G7W1PGM 
01490                              S1SCDIA                              G7W1PGM 
01491          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01492          THEN                                                     G7W1PGM 
01493              NEXT SENTENCE                                        G7W1PGM 
01494          ELSE                                                     G7W1PGM 
01495              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01496              SET WT-01-INDEX TO +04                               G7W1PGM 
01497              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W1PGM 
01498      ELSE                                                         G7W1PGM 
01499          NEXT SENTENCE.                                           G7W1PGM 
01500                                                                   G7W1PGM 
01501      IF  S1STYCDI     > ZEROS                                     G7W1PGM 
01502          AND                                                      G7W1PGM 
01503          S1SCDII NOT > ZEROS                                      G7W1PGM 
01504      THEN                                                         G7W1PGM 
01505          MOVE  -1        TO  S1SCDIL                              G7W1PGM 
01506          MOVE  DFHBMUBF  TO  S1SCDIA                              G7W1PGM 
01507                              S1STYCDA                             G7W1PGM 
01508          IF  WS-02-SCREEN-HAS-ERRORS                              G7W1PGM 
01509          THEN                                                     G7W1PGM 
01510              NEXT SENTENCE                                        G7W1PGM 
01511          ELSE                                                     G7W1PGM 
01512              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01513              SET WT-01-INDEX TO +05                               G7W1PGM 
01514              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W1PGM 
01515      ELSE                                                         G7W1PGM 
01516          NEXT SENTENCE.                                           G7W1PGM 
01517                                                                   G7W1PGM 
01518                                                                   G7W1PGM 
01519 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7W1PGM 
01520                                                                   G7W1PGM 
01521      IF  WS-02-SCREEN-HAS-ERRORS                                  G7W1PGM 
01522      THEN                                                         G7W1PGM 
01523          NEXT SENTENCE                                            G7W1PGM 
01524      ELSE                                                         G7W1PGM 
01525          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7W1PGM 
01526          THEN                                                     G7W1PGM 
01527              IF EIBAID = DFHPF4 OR DFHPF16                        G7W1PGM 
01528              THEN                                                 G7W1PGM 
01529                  NEXT SENTENCE                                    G7W1PGM 
01530              ELSE                                                 G7W1PGM 
01531                  MOVE  -1        TO S1ERRL                        G7W1PGM 
01532                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7W1PGM 
01533                  SET WT-01-INDEX TO +06                           G7W1PGM 
01534                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7W1PGM 
01535          ELSE                                                     G7W1PGM 
01536              NEXT SENTENCE.                                       G7W1PGM 
01537                                                                   G7W1PGM 
01538                                                                   G7W1PGM 
01539  2200-900-EXIT.                                                   G7W1PGM 
01540      EXIT.                                                        G7W1PGM 
01541 /***************************************************************  G7W1PGM 
01542 *                                                              *  G7W1PGM 
01543 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7W1PGM 
01544 *        REWRITE TO WORKFILE.                                  *  G7W1PGM 
01545 *                                                              *  G7W1PGM 
01546 ****************************************************************  G7W1PGM 
01547  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7W1PGM 
01548  2300-010.                                                        G7W1PGM 
01549                                                                   G7W1PGM 
01550 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7W1PGM 
01551                                                                   G7W1PGM 
01552      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7W1PGM 
01553      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7W1PGM 
01554        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7W1PGM 
01555      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7W1PGM 
01556      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7W1PGM 
01557      IF  NOT GCIO2-GOOD-RETURN                                    G7W1PGM 
01558          MOVE WS-01-ABCODE-W1F2     TO WS-01-ABCODE               G7W1PGM 
01559          MOVE WS-01-ABCODE-W1F2-MSG TO WS-01-ABCODE-MSG           G7W1PGM 
01560          PERFORM  9999-000-ABEND-THE-TASK.                        G7W1PGM 
01561                                                                   G7W1PGM 
01562                                                                   G7W1PGM 
01563 *----- SAVE FIELDS FROM SCREEN THAT CANNOT BE DIRECTLY ----------*G7W1PGM 
01564 *        COMPARED TO THE RECORD                                   G7W1PGM 
01565                                                                   G7W1PGM 
01566      MOVE S1STYCDI    TO WS-02-STAY-CD-X.                         G7W1PGM 
01567                                                                   G7W1PGM 
01568 *    MOVE ZEROS       TO WS-02-RATIO.                             G7W1PGM 
01569 *    MOVE S1DIV1BI    TO WS-02-RATIO-DIVISOR.                     G7W1PGM 
01570 *    MOVE S1BAS1BI    TO WS-02-RATIO-BASE.                        G7W1PGM 
01571 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-BASIC-AP-X.          G7W1PGM 
01572                                                                   G7W1PGM 
01573 *    MOVE ZEROS       TO WS-02-RATIO.                             G7W1PGM 
01574 *    MOVE S1DIV2BI    TO WS-02-RATIO-DIVISOR.                     G7W1PGM 
01575 *    MOVE S1BAS2BI    TO WS-02-RATIO-BASE.                        G7W1PGM 
01576 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-BASIC-BA-X.          G7W1PGM 
01577                                                                   G7W1PGM 
01578 *    MOVE ZEROS       TO WS-02-RATIO.                             G7W1PGM 
01579 *    MOVE S1DIV1SI    TO WS-02-RATIO-DIVISOR.                     G7W1PGM 
01580 *    MOVE S1BAS1SI    TO WS-02-RATIO-BASE.                        G7W1PGM 
01581 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-SEC-AP-X.            G7W1PGM 
01582                                                                   G7W1PGM 
01583 *    MOVE ZEROS       TO WS-02-RATIO.                             G7W1PGM 
01584 *    MOVE S1DIV2SI    TO WS-02-RATIO-DIVISOR.                     G7W1PGM 
01585 *    MOVE S1BAS2SI    TO WS-02-RATIO-BASE.                        G7W1PGM 
01586 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-SEC-BA-X.            G7W1PGM 
01587                                                                   G7W1PGM 
01588      MOVE S1HADRDI    TO WS-02-HSP-ADM-RESTRN-DAYS-X.             G7W1PGM 
01589                                                                   G7W1PGM 
01590                                                                   G7W1PGM 
01591                                                                   G7W1PGM 
01592 *------- DEFAULT RATIOS TO 1.0 : 1.0 IF RATIO IND = 0 -----------*G7W1PGM 
01593                                                                   G7W1PGM 
01594 *    IF  S1RDDYII = '0' OR ' '                                    G7W1PGM 
01595 *    THEN                                                         G7W1PGM 
01596 *        MOVE 01.0 TO WS-02-DAYS-RDCN-RAT-BASIC-APL               G7W1PGM 
01597 *                     WS-02-DAYS-RDCN-RAT-BASIC-BASE              G7W1PGM 
01598 *                     WS-02-DAYS-RDCN-RAT-SEC-APL                 G7W1PGM 
01599 *                     WS-02-DAYS-RDCN-RAT-SEC-BASE                G7W1PGM 
01600 *        MOVE '1'  TO S1DIV1BI                                    G7W1PGM 
01601 *                     S1DIV2BI                                    G7W1PGM 
01602 *                     S1DIV1SI                                    G7W1PGM 
01603 *                     S1DIV2SI                                    G7W1PGM 
01604 *        MOVE '0'  TO S1BAS1BI                                    G7W1PGM 
01605 *                     S1BAS2BI                                    G7W1PGM 
01606 *                     S1BAS1SI                                    G7W1PGM 
01607 *                     S1BAS2SI                                    G7W1PGM 
01608 *    ELSE                                                         G7W1PGM 
01609 *        NEXT SENTENCE.                                           G7W1PGM 
01610                                                                   G7W1PGM 
01611                                                                   G7W1PGM 
01612 *------- BASE OF RATIOS CANNOT BE 0.0 ---------------------------*G7W1PGM 
01613                                                                   G7W1PGM 
01614 *    IF  WS-02-DAYS-RDCN-RAT-BASIC-BASE = ZEROS                   G7W1PGM 
01615 *    THEN                                                         G7W1PGM 
01616 *        MOVE  -1        TO S1DIV2BL                              G7W1PGM 
01617 *        MOVE  DFHBMUBF  TO S1DIV2BA                              G7W1PGM 
01618 *                           S1BAS2BA                              G7W1PGM 
01619 *        SET WT-01-INDEX TO +12                                   G7W1PGM 
01620 *        MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH             G7W1PGM 
01621 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W1PGM 
01622 *        IF  WS-02-DAYS-RDCN-RAT-SEC-BASE = ZEROS                 G7W1PGM 
01623 *        THEN                                                     G7W1PGM 
01624 *            MOVE  DFHBMUBF  TO S1DIV2SA                          G7W1PGM 
01625 *                               S1BAS2SA                          G7W1PGM 
01626 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7W1PGM 
01627 *        ELSE                                                     G7W1PGM 
01628 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7W1PGM 
01629 *    ELSE                                                         G7W1PGM 
01630 *        IF  WS-02-DAYS-RDCN-RAT-SEC-BASE = ZEROS                 G7W1PGM 
01631 *        THEN                                                     G7W1PGM 
01632 *            MOVE  -1        TO S1DIV2SL                          G7W1PGM 
01633 *            MOVE  DFHBMUBF  TO S1DIV2SA                          G7W1PGM 
01634 *                               S1BAS2SA                          G7W1PGM 
01635 *            SET WT-01-INDEX TO +12                               G7W1PGM 
01636 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7W1PGM 
01637 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7W1PGM 
01638 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7W1PGM 
01639 *        ELSE                                                     G7W1PGM 
01640 *            NEXT SENTENCE.                                       G7W1PGM 
01641                                                                   G7W1PGM 
01642                                                                   G7W1PGM 
01643 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7W1PGM 
01644                                                                   G7W1PGM 
01645         MOVE GPW2-DAYS-RDCN-RAT-BASIC-APL TO                      G7W1PGM 
01646           WS-GPW2-DAYS-RDCN-RAT-BAS-APL.                          G7W1PGM 
01647                                                                   G7W1PGM 
01648         MOVE GPW2-DAYS-RDCN-RAT-BASIC-BASE TO                     G7W1PGM 
01649           WS-GPW2-DAYS-RDCN-RAT-BAS-BASE.                         G7W1PGM 
01650                                                                   G7W1PGM 
01651         MOVE GPW2-DAYS-RDCN-RAT-SEC-APL TO                        G7W1PGM 
01652           WS-GPW2-DAYS-RDCN-RAT-SEC-APL.                          G7W1PGM 
01653                                                                   G7W1PGM 
01654         MOVE GPW2-DAYS-RDCN-RAT-SEC-BASE TO                       G7W1PGM 
01655           WS-GPW2-DAYS-RDCN-RAT-SEC-BASE.                         G7W1PGM 
01656                                                                   G7W1PGM 
01657      IF      S1HADMRI            =  GPW2-HOSP-ADM-RESTRN-IND      G7W1PGM 
01658          AND WS-02-STAY-CD       =  GPW2-STAY-CD                  G7W1PGM 
01659          AND S1HCNDRI            =  GPW2-HOSP-COND-RELATSP-IND    G7W1PGM 
01660          AND S1RDDYII            =  GPW2-DAYS-RDCN-RAT-IND        G7W1PGM 
01661          AND WS-02-DAYS-RDCN-RAT-BASIC-APL                        G7W1PGM 
01662                                = WS-GPW2-DAYS-RDCN-RAT-BAS-APL    G7W1PGM 
01663          AND WS-02-DAYS-RDCN-RAT-BASIC-BASE                       G7W1PGM 
01664                                = WS-GPW2-DAYS-RDCN-RAT-BAS-BASE   G7W1PGM 
01665          AND WS-02-DAYS-RDCN-RAT-SEC-APL                          G7W1PGM 
01666                                = WS-GPW2-DAYS-RDCN-RAT-SEC-APL    G7W1PGM 
01667          AND WS-02-DAYS-RDCN-RAT-SEC-BASE                         G7W1PGM 
01668                                = WS-GPW2-DAYS-RDCN-RAT-SEC-BASE   G7W1PGM 
01669          AND WS-02-HSP-ADM-RESTRN-DAYS                            G7W1PGM 
01670                                  =  GPW2-HSP-ADM-RESTRN-DAYS      G7W1PGM 
01671          AND S1DECOII            =  GPW2-DRUG-ELIG-MEMB-CLS-OVRD  G7W1PGM 
01672          AND S1AECOII            =  GPW2-ALCO-ELIG-MEMB-CLS-OVRD  G7W1PGM 
01673          AND S1NNOII             =  GPW2-NORM-NWBORN-OVRD-IND     G7W1PGM 
01674          AND S1SCDII             =  GPW2-STAY-CODE-IND            G7W1PGM 
01675          AND S1TRSII             =  GPW2-TRNS-SEX-REST-OVRD-IND   G7W1PGM 
01676      THEN                                                         G7W1PGM 
01677          GO TO 2300-900-EXIT                                      G7W1PGM 
01678      ELSE                                                         G7W1PGM 
01679          NEXT SENTENCE.                                           G7W1PGM 
01680                                                                   G7W1PGM 
01681                                                                   G7W1PGM 
01682 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7W1PGM 
01683                                                                   G7W1PGM 
01684      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7W1PGM 
01685      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7W1PGM 
01686      IF  NOT GCIO2-GOOD-RETURN                                    G7W1PGM 
01687          MOVE WS-01-ABCODE-W1F3     TO WS-01-ABCODE               G7W1PGM 
01688          MOVE WS-01-ABCODE-W1F3-MSG TO WS-01-ABCODE-MSG           G7W1PGM 
01689          PERFORM  9999-000-ABEND-THE-TASK.                        G7W1PGM 
01690                                                                   G7W1PGM 
01691                                                                   G7W1PGM 
01692 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7W1PGM 
01693                                                                   G7W1PGM 
01694      MOVE S1HADMRI            TO GPW2-HOSP-ADM-RESTRN-IND.        G7W1PGM 
01695      MOVE WS-02-STAY-CD       TO GPW2-STAY-CD.                    G7W1PGM 
01696      MOVE S1HCNDRI            TO GPW2-HOSP-COND-RELATSP-IND.      G7W1PGM 
01697      MOVE S1RDDYII            TO GPW2-DAYS-RDCN-RAT-IND.          G7W1PGM 
01698      MOVE WS-02-DAYS-RDCN-RAT-BASIC-APL                           G7W1PGM 
01699                               TO GPW2-DAYS-RDCN-RAT-BASIC-APL.    G7W1PGM 
01700      MOVE WS-02-DAYS-RDCN-RAT-BASIC-BASE                          G7W1PGM 
01701                               TO GPW2-DAYS-RDCN-RAT-BASIC-BASE.   G7W1PGM 
01702      MOVE WS-02-DAYS-RDCN-RAT-SEC-APL                             G7W1PGM 
01703                               TO GPW2-DAYS-RDCN-RAT-SEC-APL.      G7W1PGM 
01704      MOVE WS-02-DAYS-RDCN-RAT-SEC-BASE                            G7W1PGM 
01705                               TO GPW2-DAYS-RDCN-RAT-SEC-BASE.     G7W1PGM 
01706      MOVE WS-02-HSP-ADM-RESTRN-DAYS                               G7W1PGM 
01707                               TO GPW2-HSP-ADM-RESTRN-DAYS.        G7W1PGM 
01708      MOVE S1DECOII            TO GPW2-DRUG-ELIG-MEMB-CLS-OVRD.    G7W1PGM 
01709      MOVE S1AECOII            TO GPW2-ALCO-ELIG-MEMB-CLS-OVRD.    G7W1PGM 
01710      MOVE S1NNOII             TO GPW2-NORM-NWBORN-OVRD-IND.       G7W1PGM 
01711      MOVE S1SCDII             TO GPW2-STAY-CODE-IND.              G7W1PGM 
01712      MOVE S1TRSII             TO GPW2-TRNS-SEX-REST-OVRD-IND.     G7W1PGM 
01713                                                                   G7W1PGM 
01714                                                                   G7W1PGM 
01715 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7W1PGM 
01716                                                                   G7W1PGM 
01717 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7W1PGM 
01718                                                                   G7W1PGM 
01719      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7W1PGM 
01720      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7W1PGM 
01721      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7W1PGM 
01722      IF  NOT GCIO2-GOOD-RETURN                                    G7W1PGM 
01723          MOVE WS-01-ABCODE-W1F4     TO WS-01-ABCODE               G7W1PGM 
01724          MOVE WS-01-ABCODE-W1F4-MSG TO WS-01-ABCODE-MSG           G7W1PGM 
01725          PERFORM  9999-000-ABEND-THE-TASK.                        G7W1PGM 
01726                                                                   G7W1PGM 
01727  2300-900-EXIT.                                                   G7W1PGM 
01728      EXIT.                                                        G7W1PGM 
01729 /***************************************************************  G7W1PGM 
01730 *                                                              *  G7W1PGM 
01731 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7W1PGM 
01732 *                                                              *  G7W1PGM 
01733 ****************************************************************  G7W1PGM 
01734  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7W1PGM 
01735  2310-010.                                                        G7W1PGM 
01736                                                                   G7W1PGM 
01737                                                                   G7W1PGM 
01738 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7W1PGM 
01739                                                                   G7W1PGM 
01740      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7W1PGM 
01741                                         + GC-WORKFILE-KEY-LEN     G7W1PGM 
01742                                         + GC-GCBENPRV-MAX-REC-LEN.G7W1PGM 
01743                                                                   G7W1PGM 
01744      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7W1PGM 
01745                          INITIMG(WS-02-HEX-00)                    G7W1PGM 
01746                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7W1PGM 
01747                          END-EXEC.                                G7W1PGM 
01748                                                                   G7W1PGM 
01749 *    SERVICE RELOAD  IO-PARM-BEN-PROV-AREA.                       G7W1PGM 
01750                                                                   G7W1PGM 
01751 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7W1PGM 
01752                                                                   G7W1PGM 
01753      MOVE SPACES                 TO GCIO-CONTRACT-FILE-KEY.       G7W1PGM 
01754      MOVE WRK-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           G7W1PGM 
01755      MOVE WRK-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3.        G7W1PGM 
01756      MOVE WRK-SEC-NO-1           TO GCIO-WRK-SEC-NO-1.            G7W1PGM 
01757      MOVE WRK-PKG-CODE           TO GCIO-WRK-PKG-CODE.            G7W1PGM 
01758      MOVE WRK-EFFECTIVE-DATE     TO GCIO-WRK-EFFECTIVE-DT.        G7W1PGM 
01759                                                                   G7W1PGM 
01760      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7W1PGM 
01761      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7W1PGM 
01762      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7W1PGM 
01763      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7W1PGM 
01764      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7W1PGM 
01765      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7W1PGM 
01766      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7W1PGM 
01767      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7W1PGM 
01768      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7W1PGM 
01769                                                                   G7W1PGM 
01770 *    MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7W1PGM 
01771 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7W1PGM 
01772 *    IF  HGADATE-RETURN = ZEROS                                   G7W1PGM 
01773 *    THEN                                                         G7W1PGM 
01774 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7W1PGM 
01775 *    ELSE                                                         G7W1PGM 
01776 *        SET WT-01-INDEX TO +07                                   G7W1PGM 
01777 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W1PGM 
01778 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7W1PGM 
01779                                                                   G7W1PGM 
01780      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7W1PGM 
01781      MOVE S1BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7W1PGM 
01782      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7W1PGM 
01783      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7W1PGM 
01784      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7W1PGM 
01785      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7W1PGM 
01786      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7W1PGM 
01787                                                                   G7W1PGM 
01788                                                                   G7W1PGM 
01789  2310-900-EXIT.                                                   G7W1PGM 
01790      EXIT.                                                        G7W1PGM 
01791 /***************************************************************  G7W1PGM 
01792 *                                                              *  G7W1PGM 
01793 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7W1PGM 
01794 *                                                              *  G7W1PGM 
01795 ****************************************************************  G7W1PGM 
01796  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7W1PGM 
01797  2400-010.                                                        G7W1PGM 
01798                                                                   G7W1PGM 
01799                                                                   G7W1PGM 
01800      IF  EIBAID = DFHPF7  OR DFHPF19                              G7W1PGM 
01801      THEN                                                         G7W1PGM 
01802          MOVE 'GC6CPGM' TO WS-02-NEXT-PROGRAM.                    G7W1PGM 
01803                                                                   G7W1PGM 
01804      IF  EIBAID = DFHENTER OR                                     G7W1PGM 
01805                   DFHPF4   OR DFHPF16 OR                          G7W1PGM 
01806                   DFHPF8   OR DFHPF20                             G7W1PGM 
01807      THEN                                                         G7W1PGM 
01808          MOVE 'G7W2PGM' TO WS-02-NEXT-PROGRAM.                    G7W1PGM 
01809                                                                   G7W1PGM 
01810      IF  EIBAID = DFHPF6  OR DFHPF18                              G7W1PGM 
01811      THEN                                                         G7W1PGM 
01812          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7W1PGM 
01813                                                                   G7W1PGM 
01814                                                                   G7W1PGM 
01815      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7W1PGM 
01816                       COMMAREA(WORK-RECORD-2)                     G7W1PGM 
01817                       LENGTH  (GCIO2-RECORD-LENGTH)               G7W1PGM 
01818                       END-EXEC.                                   G7W1PGM 
01819                                                                   G7W1PGM 
01820  2400-900-EXIT.                                                   G7W1PGM 
01821      EXIT.                                                        G7W1PGM 
01822 /***************************************************************  G7W1PGM 
01823 *                                                              *  G7W1PGM 
01824 * 2500   LINK TO GX3APGM FOR CONVERSION                           G7W1PGM 
01825 *                                                              *  G7W1PGM 
01826 ****************************************************************  G7W1PGM 
01827  2500-LINK-TO-GX3APGM.                                            G7W1PGM 
01828                                                                   G7W1PGM 
01829      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7W1PGM 
01830                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7W1PGM 
01831                       LENGTH  (+51)                               G7W1PGM 
01832                       END-EXEC.                                   G7W1PGM 
01833                                                                   G7W1PGM 
01834                                                                   G7W1PGM 
01835  2500-EXIT.                                                       G7W1PGM 
01836      EXIT.                                                        G7W1PGM 
01837 /***************************************************************  G7W1PGM 
01838 *                                                              *  G7W1PGM 
01839 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7W1PGM 
01840 *         PROVISION RECORD (TYPE=C4)                           *  G7W1PGM 
01841 *                                                              *  G7W1PGM 
01842 ****************************************************************  G7W1PGM 
01843  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7W1PGM 
01844  5000-010.                                                        G7W1PGM 
01845                                                                   G7W1PGM 
01846      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7W1PGM 
01847                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7W1PGM 
01848                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7W1PGM 
01849                       END-EXEC.                                   G7W1PGM 
01850                                                                   G7W1PGM 
01851                                                                   G7W1PGM 
01852  5000-900-EXIT.                                                   G7W1PGM 
01853      EXIT.                                                        G7W1PGM 
01854 /***************************************************************  G7W1PGM 
01855 *                                                              *  G7W1PGM 
01856 * 5100                                                         *  G7W1PGM 
01857 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7W1PGM 
01858 *                                                              *  G7W1PGM 
01859 ****************************************************************  G7W1PGM 
01860  5100-000-W-F-CONTRACT-IO       SECTION.                          G7W1PGM 
01861  5100-010.                                                        G7W1PGM 
01862                                                                   G7W1PGM 
01863      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7W1PGM 
01864                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7W1PGM 
01865                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7W1PGM 
01866                       END-EXEC.                                   G7W1PGM 
01867                                                                   G7W1PGM 
01868                                                                   G7W1PGM 
01869  5100-900-EXIT.                                                   G7W1PGM 
01870      EXIT.                                                        G7W1PGM 
01871 /***************************************************************  G7W1PGM 
01872 *                                                              *  G7W1PGM 
01873 * 9000   MOVE MESSAGE TO SCREEN                                *  G7W1PGM 
01874 *                                                              *  G7W1PGM 
01875 ****************************************************************  G7W1PGM 
01876  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7W1PGM 
01877  9000-010.                                                        G7W1PGM 
01878                                                                   G7W1PGM 
01879      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S1ERRO.              G7W1PGM 
01880                                                                   G7W1PGM 
01881  9000-900-EXIT.                                                   G7W1PGM 
01882      EXIT.                                                        G7W1PGM 
01883 /***************************************************************  G7W1PGM 
01884 *                                                              *  G7W1PGM 
01885 * 9100 SEND SCREEN AND RETURN                                  *  G7W1PGM 
01886 *                                                              *  G7W1PGM 
01887 ****************************************************************  G7W1PGM 
01888  9100-000-SEND-THEN-RETURN      SECTION.                          G7W1PGM 
01889  9100-010.                                                        G7W1PGM 
01890                                                                   G7W1PGM 
01891                                                                   G7W1PGM 
01892 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7W1PGM 
01893      MOVE  -1 TO  S1ERRL.                                         G7W1PGM 
01894                                                                   G7W1PGM 
01895                                                                   G7W1PGM 
01896      IF  WS-02-MY-EIBTRNID                                        G7W1PGM 
01897      THEN                                                         G7W1PGM 
01898          EXEC CICS  SEND MAP('G7W1I01')                           G7W1PGM 
01899                          MAPSET('G7W1SET')                        G7W1PGM 
01900                          DATAONLY                                 G7W1PGM 
01901                          CURSOR                                   G7W1PGM 
01902                          END-EXEC                                 G7W1PGM 
01903      ELSE                                                         G7W1PGM 
01904          EXEC CICS  SEND MAP('G7W1I01')                           G7W1PGM 
01905                          MAPSET('G7W1SET')                        G7W1PGM 
01906                          ERASE                                    G7W1PGM 
01907                          CURSOR                                   G7W1PGM 
01908                          END-EXEC.                                G7W1PGM 
01909                                                                   G7W1PGM 
01910      EXEC CICS RETURN                                             G7W1PGM 
01911                TRANSID  ('G7W1')                                  G7W1PGM 
01912                COMMAREA (DFHCOMMAREA)                             G7W1PGM 
01913                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7W1PGM 
01914                END-EXEC.                                          G7W1PGM 
01915                                                                   G7W1PGM 
01916 *    EXEC CICS  RETURN                                            G7W1PGM 
01917 *               END-EXEC.                                         G7W1PGM 
01918 *                                                                 G7W1PGM 
01919 *                                                                 G7W1PGM 
01920  9100-900-EXIT.                                                   G7W1PGM 
01921      EXIT.                                                        G7W1PGM 
01922 /*****************************************************************G7W1PGM 
01923 *                                                                *G7W1PGM 
01924 * 9200    XCTL TO GCPSPGM                                        *G7W1PGM 
01925 *                                                                *G7W1PGM 
01926 *                                                                *G7W1PGM 
01927 ******************************************************************G7W1PGM 
01928  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7W1PGM 
01929  9200-010.                                                        G7W1PGM 
01930                                                                   G7W1PGM 
01931      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7W1PGM 
01932                       END-EXEC.                                   G7W1PGM 
01933                                                                   G7W1PGM 
01934  9200-900-EXIT.                                                   G7W1PGM 
01935      EXIT.                                                        G7W1PGM 
01936 /*****************************************************************G7W1PGM 
01937 *                                                                *G7W1PGM 
01938 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7W1PGM 
01939 *                                                                *G7W1PGM 
01940 *                                                                *G7W1PGM 
01941 ******************************************************************G7W1PGM 
01942  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7W1PGM 
01943  9210-010.                                                        G7W1PGM 
01944                                                                   G7W1PGM 
01945      IF  S1GRPNOI = '000SPS000'                                   G7W1PGM 
01946          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7W1PGM 
01947                           END-EXEC.                               G7W1PGM 
01948                                                                   G7W1PGM 
01949 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7W1PGM 
01950                                                                   G7W1PGM 
01951      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7W1PGM 
01952                                        + GC-WORKFILE-KEY-LEN      G7W1PGM 
01953                                        + GC-GCCONTR-MAX-REC-LEN.  G7W1PGM 
01954                                                                   G7W1PGM 
01955      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7W1PGM 
01956                          INITIMG(WS-02-HEX-00)                    G7W1PGM 
01957                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7W1PGM 
01958                          END-EXEC.                                G7W1PGM 
01959                                                                   G7W1PGM 
01960 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7W1PGM 
01961 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7W1PGM 
01962                                                                   G7W1PGM 
01963 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7W1PGM 
01964                                                                   G7W1PGM 
01965      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7W1PGM 
01966        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7W1PGM 
01967      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7W1PGM 
01968      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7W1PGM 
01969                                                                   G7W1PGM 
01970      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7W1PGM 
01971      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7W1PGM 
01972      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7W1PGM 
01973      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7W1PGM 
01974      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7W1PGM 
01975      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7W1PGM 
01976      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7W1PGM 
01977      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7W1PGM 
01978      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7W1PGM 
01979      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7W1PGM 
01980      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7W1PGM 
01981      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7W1PGM 
01982      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7W1PGM 
01983      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7W1PGM 
01984                                                                   G7W1PGM 
01985 *    MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7W1PGM 
01986 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7W1PGM 
01987 *    IF  HGADATE-RETURN = ZEROS                                   G7W1PGM 
01988 *    THEN                                                         G7W1PGM 
01989 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7W1PGM 
01990 *    ELSE                                                         G7W1PGM 
01991 *        SET WT-01-INDEX TO +07                                   G7W1PGM 
01992 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7W1PGM 
01993 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7W1PGM 
01994                                                                   G7W1PGM 
01995      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7W1PGM 
01996      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7W1PGM 
01997      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7W1PGM 
01998      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7W1PGM 
01999      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7W1PGM 
02000      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7W1PGM 
02001      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7W1PGM 
02002                                                                   G7W1PGM 
02003      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7W1PGM 
02004                                                                   G7W1PGM 
02005      IF  NOT GCIO3-GOOD-RETURN                                    G7W1PGM 
02006          MOVE WS-01-ABCODE-W1F1     TO WS-01-ABCODE               G7W1PGM 
02007          MOVE WS-01-ABCODE-W1F1-MSG TO WS-01-ABCODE-MSG           G7W1PGM 
02008          PERFORM  9999-000-ABEND-THE-TASK.                        G7W1PGM 
02009                                                                   G7W1PGM 
02010      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7W1PGM 
02011                       COMMAREA(WORK-RECORD-3)                     G7W1PGM 
02012                       LENGTH  (GCIO3-RECORD-LENGTH)               G7W1PGM 
02013                       END-EXEC.                                   G7W1PGM 
02014                                                                   G7W1PGM 
02015  9210-900-EXIT.                                                   G7W1PGM 
02016      EXIT.                                                        G7W1PGM 
02017 /*****************************************************************G7W1PGM 
02018 *                                                                *G7W1PGM 
02019 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7W1PGM 
02020 *                                                                *G7W1PGM 
02021 *                                                                *G7W1PGM 
02022 ******************************************************************G7W1PGM 
02023  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7W1PGM 
02024  9220-010.                                                        G7W1PGM 
02025                                                                   G7W1PGM 
02026      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7W1PGM 
02027                       END-EXEC.                                   G7W1PGM 
02028                                                                   G7W1PGM 
02029  9220-900-EXIT.                                                   G7W1PGM 
02030      EXIT.                                                        G7W1PGM 
02031 /*****************************************************************G7W1PGM 
02032 *                                                                *G7W1PGM 
02033 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7W1PGM 
02034 *                                                                *G7W1PGM 
02035 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7W1PGM 
02036 *                                                                *G7W1PGM 
02037 ******************************************************************G7W1PGM 
02038  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7W1PGM 
02039  9800-010.                                                        G7W1PGM 
02040                                                                   G7W1PGM 
02041      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7W1PGM 
02042      MOVE 'M'   TO  HGADATE-FORM1.                                G7W1PGM 
02043      MOVE 'J'   TO  HGADATE-FORM2.                                G7W1PGM 
02044      MOVE ZEROS TO  HGADATE-RETURN                                G7W1PGM 
02045                     HGADATE-AMOUNT.                               G7W1PGM 
02046      EXEC CICS LINK PROGRAM ('HGADATES')                          G7W1PGM 
02047                     COMMAREA(HGADATES-COMMAREA)                   G7W1PGM 
02048                     LENGTH  (24)                                  G7W1PGM 
02049                     END-EXEC.                                     G7W1PGM 
02050                                                                   G7W1PGM 
02051  9800-900-900-EXIT.                                               G7W1PGM 
02052      EXIT.                                                        G7W1PGM 
02053 /*****************************************************************G7W1PGM 
02054 *                                                                *G7W1PGM 
02055 * 9810    J U L I A N    T O    G R E G O R I A N                *G7W1PGM 
02056 *                                                                *G7W1PGM 
02057 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7W1PGM 
02058 *                                                                *G7W1PGM 
02059 ******************************************************************G7W1PGM 
02060  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7W1PGM 
02061  9810-010.                                                        G7W1PGM 
02062                                                                   G7W1PGM 
02063      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7W1PGM 
02064      MOVE 'J'   TO  HGADATE-FORM1.                                G7W1PGM 
02065      MOVE 'M'   TO  HGADATE-FORM2.                                G7W1PGM 
02066      MOVE ZEROS TO  HGADATE-RETURN                                G7W1PGM 
02067                     HGADATE-AMOUNT.                               G7W1PGM 
02068      EXEC CICS LINK PROGRAM ('HGADATES')                          G7W1PGM 
02069                     COMMAREA(HGADATES-COMMAREA)                   G7W1PGM 
02070                     LENGTH  (24)                                  G7W1PGM 
02071                     END-EXEC.                                     G7W1PGM 
02072                                                                   G7W1PGM 
02073  9810-900-900-EXIT.                                               G7W1PGM 
02074      EXIT.                                                        G7W1PGM 
02075 /***************************************************************  G7W1PGM 
02076 *                                                              *  G7W1PGM 
02077 * 9999  ABEND THE TASK                                         *  G7W1PGM 
02078 *                                                              *  G7W1PGM 
02079 ****************************************************************  G7W1PGM 
02080  9999-000-ABEND-THE-TASK SECTION.                                 G7W1PGM 
02081  9999-010.                                                        G7W1PGM 
02082                                                                   G7W1PGM 
02083      EXEC CICS  ABEND                                             G7W1PGM 
02084                 ABCODE(WS-01-ABCODE)                              G7W1PGM 
02085                 END-EXEC.                                         G7W1PGM 
02086                                                                   G7W1PGM 
02087  9900-900-EXIT.                                                   G7W1PGM 
02088      EXIT.                                                        G7W1PGM 
