00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7A1PGM.                                         G7A1PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7A1PGM 
00005  DATE-WRITTEN.   03/05/87.                                        G7A1PGM 
00006  DATE-COMPILED.                                                   G7A1PGM 
00007 ***************************************************************** G7A1PGM 
00008 *                                                               * G7A1PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7A1PGM 
00010 *                                                               * G7A1PGM 
00011 *                                                               * G7A1PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7A1PGM 
00013 *                                                               * G7A1PGM 
00014 *  D0120     01/20/87  TCM  LOGIC FOR SINGLE PROVISION SUPPORT: * G7A1PGM 
00015 *                          1) TREAT 'GPM1' AS A VALID TRANS CODE* G7A1PGM 
00016 *                             (SAME AS 'GC5A')                  * G7A1PGM 
00017 *                          2)  RETURN TO 'GPM1' (INSTEAD OF     * G7A1PGM 
00018 *                              'GC5A')                          * G7A1PGM 
00019 *                              IF GROUP NO. IS 'SPS000' (SINGLE * G7A1PGM 
00020 *                              PROVISION)                       * G7A1PGM 
00021 *                                                               * G7A1PGM 
00022 *  D116       7-15-87  FRY   CAUSE GCIOPGM TO CALL GX5ZPGM TO   * G7A1PGM 
00023 *                            UPDATE OPERATOR-ID IN W/F RECORD   * G7A1PGM 
00024 *                            WHEN 'C4' RECORD IS MODIFIED.      * G7A1PGM 
00025 *                                                               * G7A1PGM 
00026 *  D129      08/17/89  GDM   CONVERT FOR DECIMALS                 G7A1PGM 
00027 *                                                                 G7A1PGM 
00028 *  D129      09/01/89  GDM   CONVERT TO VS COBOL/2                G7A1PGM 
00029 *                                                                 G7A1PGM 
00030 *  D12009    08/23/91  BSO   CORRECT ERR MESSAGE LITERALS STARTINGG7A1PGM 
00031 *                            IN AREA \
00032 *                            CORRECT ALPHA CLASS TEST AREA        G7A1PGM 
00033 *                                                                 G7A1PGM 
00034 * 14726/     03/27/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7A1PGM 
00035 * 15057                     INCREASED GROUP AND SECTION ON      * G7A1PGM 
00036 *                           THE SCREEN.                         * G7A1PGM 
00037 *                                                               * G7A1PGM 
00038 *            12/11/02  AKK  COMPILE FOR OPID                    * G7A1PGM 
00039 *                                                               * G7A1PGM 
00040 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7A1PGM 
00041 *                           G7A1SET                              *G7A1PGM 
00042 ***************************************************************** G7A1PGM 
00043                                                                   G7A1PGM 
00044 ***************************************************************** G7A1PGM 
00045 *                                                               * G7A1PGM 
00046 *    G7A1PGM  - PROGRAM 1 OF 2 PROGRAMS TO UPDATE THE FORMAT 'A'* G7A1PGM 
00047 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7A1PGM 
00048 *                                                               * G7A1PGM 
00049 *    TRANSID: G7A1                                              * G7A1PGM 
00050 *    MAPSET:  G7A1SETC    (GIA1PGM WHICH SHARES THIS MAP)       * G7A1PGM 
00051 *    VALGEN:  NONE                                              * G7A1PGM 
00052 *                                                               * G7A1PGM 
00053 *    PROGRAM NARRATIVE:                                         * G7A1PGM 
00054 *                                                               * G7A1PGM 
00055 *        PROGRAM CHECKS FOR TRANS CODE 'G7A1'.  AN INVALID      * G7A1PGM 
00056 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7A1PGM 
00057 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7A1PGM 
00058 *                                                               * G7A1PGM 
00059 *        THE MAIN FUNCTIONS ARE :                               * G7A1PGM 
00060 *        1. HARDCOPY REQUEST,                                   * G7A1PGM 
00061 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7A1PGM 
00062 *           USER,                                               * G7A1PGM 
00063 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7A1PGM 
00064 *                                                               * G7A1PGM 
00065 *        HARDCOPY REQUEST                                       * G7A1PGM 
00066 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7A1PGM 
00067 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7A1PGM 
00068 *           THE SCREEN BUFFER.                                  * G7A1PGM 
00069 *                                                               * G7A1PGM 
00070 *        PROCESS INPUT DATA (UPDATE).                           * G7A1PGM 
00071 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7A1PGM 
00072 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7A1PGM 
00073 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7A1PGM 
00074 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7A1PGM 
00075 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7A1PGM 
00076 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7A1PGM 
00077 *                                                               * G7A1PGM 
00078 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7A1PGM 
00079 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7A1PGM 
00080 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7A1PGM 
00081 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7A1PGM 
00082 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7A1PGM 
00083 *                                                               * G7A1PGM 
00084 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7A1PGM 
00085 *                                     -----------------------   * G7A1PGM 
00086 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7A1PGM 
00087 *           -----------------------------------------------     * G7A1PGM 
00088 *                                                               * G7A1PGM 
00089 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7A1PGM 
00090 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7A1PGM 
00091 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7A1PGM 
00092 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7A1PGM 
00093 *           SENT BACK TO THE USER.                              * G7A1PGM 
00094 *                                                               * G7A1PGM 
00095 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7A1PGM 
00096 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7A1PGM 
00097 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7A1PGM 
00098 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7A1PGM 
00099 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7A1PGM 
00100 *           PF KEY LOGIC,                                       * G7A1PGM 
00101 *              PF6/PF18       GOES TO GC8APGM                   * G7A1PGM 
00102 *              PF8/PF20/ENTER GOES TO G7A2PGM                   * G7A1PGM 
00103 *              FOR PF7/PF19   GOES TO GC6CPGM                   * G7A1PGM 
00104 *                                                               * G7A1PGM 
00105 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7A1PGM 
00106 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7A1PGM 
00107 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7A1PGM 
00108 *                                                               * G7A1PGM 
00109 ***************************************************************** G7A1PGM 
00110                                                                   G7A1PGM 
00111  ENVIRONMENT DIVISION.                                            G7A1PGM 
00112  DATA DIVISION.                                                   G7A1PGM 
00113 /                                                                 G7A1PGM 
00114  WORKING-STORAGE SECTION.                                         G7A1PGM 
00115  01  WS-BEGIN                    PIC X(58) VALUE                  G7A1PGM 
00116      '*** G7A1PGM  WORKING-STORAGE BEGINS HERE ***'.              G7A1PGM 
00117                                                                   G7A1PGM 
00118                                                                   G7A1PGM 
00119  01  WS-01-ABEND-AREA.                                            G7A1PGM 
00120      05  FILLER                   PIC X(16)  VALUE                G7A1PGM 
00121          '** ABEND AREA **'.                                      G7A1PGM 
00122                                                                   G7A1PGM 
00123      05  WS-01-ABEND-CODES-AND-MSG.                               G7A1PGM 
00124          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7A1PGM 
00125          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7A1PGM 
00126                                                                   G7A1PGM 
00127          10  WS-01-ABCODE-A1F1          PIC X(04)  VALUE  'A1F1'. G7A1PGM 
00128          10  WS-01-ABCODE-A1F1-MSG      PIC X(44)  VALUE          G7A1PGM 
00129             'W/F CONTRACT CANNOT BE FOUND             '.          G7A1PGM 
00130                                                                   G7A1PGM 
00131          10  WS-01-ABCODE-A1F2          PIC X(04)  VALUE  'A1F2'. G7A1PGM 
00132          10  WS-01-ABCODE-A1F2-MSG      PIC X(44)  VALUE          G7A1PGM 
00133             'W/F BEN PROV CANNOT BE FOUND             '.          G7A1PGM 
00134                                                                   G7A1PGM 
00135          10  WS-01-ABCODE-A1F3          PIC X(04)  VALUE  'A1F3'. G7A1PGM 
00136          10  WS-01-ABCODE-A1F3-MSG      PIC X(44)  VALUE          G7A1PGM 
00137             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7A1PGM 
00138                                                                   G7A1PGM 
00139          10  WS-01-ABCODE-A1F4          PIC X(04)  VALUE  'A1F4'. G7A1PGM 
00140          10  WS-01-ABCODE-A1F4-MSG      PIC X(44)  VALUE          G7A1PGM 
00141             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7A1PGM 
00142                                                                   G7A1PGM 
00143          10  WS-01-ABCODE-A1L1          PIC X(04)  VALUE  'A1L1'. G7A1PGM 
00144          10  WS-01-ABCODE-A1L1-MSG      PIC X(44)  VALUE          G7A1PGM 
00145             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7A1PGM 
00146                                                                   G7A1PGM 
00147          10  WS-01-ABCODE-A1P1          PIC X(04)  VALUE  'A1P1'. G7A1PGM 
00148          10  WS-01-ABCODE-A1P1-MSG      PIC X(44)  VALUE          G7A1PGM 
00149             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7A1PGM 
00150                                                                   G7A1PGM 
00151          10  WS-01-ABCODE-A1P2          PIC X(04)  VALUE  'A1P2'. G7A1PGM 
00152          10  WS-01-ABCODE-A1P2-MSG      PIC X(44)  VALUE          G7A1PGM 
00153             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7A1PGM 
00154                                                                   G7A1PGM 
00155  01  WS-02-AREA.                                                  G7A1PGM 
00156      05  FILLER                   PIC X(16)  VALUE                G7A1PGM 
00157          '** WS-02-AREA **'.                                      G7A1PGM 
00158                                                                   G7A1PGM 
00159      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7A1PGM 
00160          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7A1PGM 
00161                                                    'GC6C' 'G7A2'  G7A1PGM 
00162                                                    'G7A1'.        G7A1PGM 
00163                                                                   G7A1PGM 
00164          88  WS-02-MY-EIBTRNID                     VALUE  'G7A1'. G7A1PGM 
00165                                                                   G7A1PGM 
00166      05  WS-02-COMPUTED-LENGTHS.                                  G7A1PGM 
00167          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7A1PGM 
00168          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7A1PGM 
00169          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7A1PGM 
00170                                                                   G7A1PGM 
00171      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7A1PGM 
00172                                                                   G7A1PGM 
00173      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7A1PGM 
00174                                                                   G7A1PGM 
00175      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7A1PGM 
00176      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7A1PGM 
00177          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7A1PGM 
00178                                         PIC X.                    G7A1PGM 
00179          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7A1PGM 
00180                                                    '0' THRU '9'   G7A1PGM 
00181                                                    'A' THRU 'I'   G7A1PGM 
00182                                                    'J' THRU 'R'   G7A1PGM 
00183                                                    'S' THRU 'Z'   G7A1PGM 
00184                                                    SPACE.         G7A1PGM 
00185                                                                   G7A1PGM 
00186      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7A1PGM 
00187          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7A1PGM 
00188          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7A1PGM 
00189                                                                   G7A1PGM 
00190      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7A1PGM 
00191          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7A1PGM 
00192                                                                   G7A1PGM 
00193      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7A1PGM 
00194                                                                   G7A1PGM 
00195      05  WS-02-HEX-F00000.                                        G7A1PGM 
00196          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7A1PGM 
00197          10  FILLER                     PIC  X(09) VALUE          G7A1PGM 
00198                                                    LOW-VALUES.    G7A1PGM 
00199                                                                   G7A1PGM 
00200      05  WS-02-RATIO                    PIC  9(2)V9 VALUE  ZEROS. G7A1PGM 
00201 *    05  FILLER    REDEFINES  WS-02-RATIO.                        G7A1PGM 
00202 *        10  FILLER                     PIC  X.                   G7A1PGM 
00203 *        10  WS-02-RATIO-DIVISOR        PIC  X.                   G7A1PGM 
00204 *        10  WS-02-RATIO-BASE           PIC  X.                   G7A1PGM 
00205                                                                   G7A1PGM 
00206      05  WS-02-HSP-ADM-RESTRN-DAYS-X.                             G7A1PGM 
00207          10  WS-02-HSP-ADM-RESTRN-DAYS  PIC  9(3)    VALUE ZEROS. G7A1PGM 
00208          10  WS-02-S1HADRD              REDEFINES                 G7A1PGM 
00209              WS-02-HSP-ADM-RESTRN-DAYS  PIC  X(3).                G7A1PGM 
00210                                                                   G7A1PGM 
00211      05  WS-02-DAYS-RDCN-RAT-BASIC-AP-X.                          G7A1PGM 
00212          10  WS-02-DAYS-RDCN-RAT-BASIC-APL PIC 99V9  VALUE ZEROS. G7A1PGM 
00213                                                                   G7A1PGM 
00214      05  WS-02-DAYS-RDCN-RAT-BASIC-BA-X.                          G7A1PGM 
00215          10  WS-02-DAYS-RDCN-RAT-BASIC-BASE PIC 99V9  VALUE ZEROS.G7A1PGM 
00216                                                                   G7A1PGM 
00217      05  WS-02-DAYS-RDCN-RAT-SEC-AP-X.                            G7A1PGM 
00218          10  WS-02-DAYS-RDCN-RAT-SEC-APL    PIC 99V9  VALUE ZEROS.G7A1PGM 
00219                                                                   G7A1PGM 
00220      05  WS-02-DAYS-RDCN-RAT-SEC-BA-X.                            G7A1PGM 
00221          10  WS-02-DAYS-RDCN-RAT-SEC-BASE   PIC 99V9  VALUE ZEROS.G7A1PGM 
00222                                                                   G7A1PGM 
00223      05  WS-02-FLAT-RATE-PDM-AMT-X.                               G7A1PGM 
00224          10  WS-02-FLAT-RATE-PDM-AMT    PIC  9(5)V99 VALUE ZEROS. G7A1PGM 
00225 *        10  WS-02-S1FLPDY              REDEFINES                 G7A1PGM 
00226 *            WS-02-FLAT-RATE-PDM-AMT    PIC  X(7).                G7A1PGM 
00227                                                                   G7A1PGM 
00228      05  WS-02-ECF-F-RAT-PER-DIEM-AMT-X.                          G7A1PGM 
00229          10  WS-02-ECF-F-RAT-PER-DIEM-AMT PIC 9(5)V99 VALUE ZEROS.G7A1PGM 
00230 *        10  WS-02-S1EFLPD              REDEFINES                 G7A1PGM 
00231 *            WS-02-ECF-F-RAT-PER-DIEM-AMT PIC  X(7).              G7A1PGM 
00232                                                                   G7A1PGM 
00233      05  WS-02-ADDN-ALLOW-AMT-PER-DAY-X.                          G7A1PGM 
00234          10  WS-02-ADDN-ALLOW-AMT-PER-DAY PIC 9(3)V99 VALUE ZEROS.G7A1PGM 
00235 *        10  WS-02-S1ADALD              REDEFINES                 G7A1PGM 
00236 *            WS-02-ADDN-ALLOW-AMT-PER-DAY PIC X(5).               G7A1PGM 
00237                                                                   G7A1PGM 
00238      05  WS-02-STAY-CD-X.                                         G7A1PGM 
00239          10  WS-02-STAY-CD                PIC 9(3)    VALUE ZEROS.G7A1PGM 
00240          10  WS-02-S1STYCD              REDEFINES                 G7A1PGM 
00241              WS-02-STAY-CD                PIC X(3).               G7A1PGM 
00242                                                                   G7A1PGM 
00243      05  WS-7POS-MAX-AMT          PIC 99999V99 VALUE 99999.99.    G7A1PGM 
00244      05  WS-02-DISP-7POS-DEC      PIC 9(5).99.                    G7A1PGM 
00245      05  WS-GPA2-ECF-F-RAT-PER-DIEM-AMT   PIC 9(5)V99.            G7A1PGM 
00246      05  WS-GPA2-FLAT-RATE-PDM-AMT        PIC 9(5)V99.            G7A1PGM 
00247                                                                   G7A1PGM 
00248      05  WS-5POS-MAX-AMT          PIC 999V99 VALUE 999.99.        G7A1PGM 
00249      05  WS-02-DISP-5POS-DEC      PIC 9(3).99.                    G7A1PGM 
00250      05  WS-GPA2-ADDN-ALLOW-AMT-PER-DAY   PIC 9(3)V99.            G7A1PGM 
00251                                                                   G7A1PGM 
00252      05  WS-3POS-MAX-AMT          PIC 99V9 VALUE 99.9.            G7A1PGM 
00253      05  WS-02-DISP-3POS-DEC      PIC 99.9.                       G7A1PGM 
00254      05  WS-GPA2-DAYS-RDCN-RAT-BAS-APL           PIC 99V9.        G7A1PGM 
00255      05  WS-GPA2-DAYS-RDCN-RAT-BAS-BASE          PIC 99V9.        G7A1PGM 
00256      05  WS-GPA2-DAYS-RDCN-RAT-SEC-APL           PIC 99V9.        G7A1PGM 
00257      05  WS-GPA2-DAYS-RDCN-RAT-SEC-BASE          PIC 99V9.        G7A1PGM 
00258 /                                                                 G7A1PGM 
00259  01  WT-00-G7A1PGM-TABLES.                                        G7A1PGM 
00260      05  FILLER                   PIC X(16)  VALUE                G7A1PGM 
00261          '*G7A1PGM TABLES*'.                                      G7A1PGM 
00262                                                                   G7A1PGM 
00263  01  WT-01-TABLE.                                                 G7A1PGM 
00264      05  FILLER                  PIC X(16) VALUE                  G7A1PGM 
00265          '* WT-01-TABLE  *'.                                      G7A1PGM 
00266 ******************************************************************G7A1PGM 
00267 *    WT-01   MESSAGE TABLE                                       *G7A1PGM 
00268 ******************************************************************G7A1PGM 
00269  01  FILLER.                                                      G7A1PGM 
00270      05  WT-01-MESSAGE-VALUES.                                    G7A1PGM 
00271                                                                   G7A1PGM 
00272 *----------------------------------------------------------------*G7A1PGM 
00273          10  WT-01-ENTRY-001.                                     G7A1PGM 
00274              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00275              15  WT-01-MESSAGE-TEXT-001.                          G7A1PGM 
00276                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00277                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00278                  20  FILLER          PIC X(3)  VALUE  '001'.      G7A1PGM 
00279                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00280                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00281                      ' INVALID PFKEY SELECTION                    G7A1PGM 
00282 -                    '                         '.                 G7A1PGM 
00283              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00284                                                                   G7A1PGM 
00285 *----------------------------------------------------------------*G7A1PGM 
00286          10  WT-01-ENTRY-002.                                     G7A1PGM 
00287              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00288              15  WT-01-MESSAGE-TEXT-002.                          G7A1PGM 
00289                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00290                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00291                  20  FILLER          PIC X(3)  VALUE  '002'.      G7A1PGM 
00292                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00293                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00294                      'HOSP ADMISSION RESTRICTION DAYS REQUIRED WHEG7A1PGM 
00295 -                    'N INDICATOR IS CODED     '.                 G7A1PGM 
00296              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00297                                                                   G7A1PGM 
00298 *----------------------------------------------------------------*G7A1PGM 
00299          10  WT-01-ENTRY-003.                                     G7A1PGM 
00300              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00301              15  WT-01-MESSAGE-TEXT-003.                          G7A1PGM 
00302                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00303                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00304                  20  FILLER          PIC X(3)  VALUE  '003'.      G7A1PGM 
00305                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00306                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00307                      'INDICATOR REQUIRED WHEN HOSPITAL ADMISSION RG7A1PGM 
00308 -                    'ESTRICTION DAYS CODED    '.                 G7A1PGM 
00309              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00310                                                                   G7A1PGM 
00311 *----------------------------------------------------------------*G7A1PGM 
00312          10  WT-01-ENTRY-004.                                     G7A1PGM 
00313              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00314              15  WT-01-MESSAGE-TEXT-004.                          G7A1PGM 
00315                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00316                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00317                  20  FILLER          PIC X(3)  VALUE  '004'.      G7A1PGM 
00318                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00319                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00320                      'STAY CODE DAYS REQUIRED WHEN INDICATOR IS C G7A1PGM 
00321 -                    'ODED                     '.                 G7A1PGM 
00322              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00323                                                                   G7A1PGM 
00324 *----------------------------------------------------------------*G7A1PGM 
00325          10  WT-01-ENTRY-005.                                     G7A1PGM 
00326              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00327              15  WT-01-MESSAGE-TEXT-005.                          G7A1PGM 
00328                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00329                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00330                  20  FILLER          PIC X(3)  VALUE  '005'.      G7A1PGM 
00331                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00332                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00333                      'INDICATOR REQUIRED WHEN STAY CODE DAYS IS COG7A1PGM 
00334 -                    'DED                      '.                 G7A1PGM 
00335              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00336                                                                   G7A1PGM 
00337 *----------------------------------------------------------------*G7A1PGM 
00338          10  WT-01-ENTRY-006.                                     G7A1PGM 
00339              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00340              15  WT-01-MESSAGE-TEXT-006.                          G7A1PGM 
00341                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00342                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00343                  20  FILLER          PIC X(3)  VALUE  '006'.      G7A1PGM 
00344                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00345                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00346                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7A1PGM 
00347 -                    ' PF4/PF16 TO CONTINUE    '.                 G7A1PGM 
00348              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00349                                                                   G7A1PGM 
00350 *----------------------------------------------------------------*G7A1PGM 
00351          10  WT-01-ENTRY-007.                                     G7A1PGM 
00352              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00353              15  WT-01-MESSAGE-TEXT-007.                          G7A1PGM 
00354                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00355                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00356                  20  FILLER          PIC X(3)  VALUE  '007'.      G7A1PGM 
00357                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00358                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00359                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7A1PGM 
00360 -                    'E CALL SYSTEMS           '.                 G7A1PGM 
00361              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00362                                                                   G7A1PGM 
00363 *----------------------------------------------------------------*G7A1PGM 
00364          10  WT-01-ENTRY-008.                                     G7A1PGM 
00365              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00366              15  WT-01-MESSAGE-TEXT-008.                          G7A1PGM 
00367                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00368                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00369                  20  FILLER          PIC X(3)  VALUE  '008'.      G7A1PGM 
00370                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00371                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00372                      'FIELD HAS AN INVALID VALUE                  G7A1PGM 
00373 -                    '                         '.                 G7A1PGM 
00374              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00375                                                                   G7A1PGM 
00376 *----------------------------------------------------------------*G7A1PGM 
00377          10  WT-01-ENTRY-009.                                     G7A1PGM 
00378              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00379              15  WT-01-MESSAGE-TEXT-009.                          G7A1PGM 
00380                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00381                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00382                  20  FILLER          PIC X(3)  VALUE  '009'.      G7A1PGM 
00383                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00384                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00385                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7A1PGM 
00386 -                    'YSTEM)                   '.                 G7A1PGM 
00387              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00388                                                                   G7A1PGM 
00389 *----------------------------------------------------------------*G7A1PGM 
00390          10  WT-01-ENTRY-010.                                     G7A1PGM 
00391              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00392              15  WT-01-MESSAGE-TEXT-010.                          G7A1PGM 
00393                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00394                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00395                  20  FILLER          PIC X(3)  VALUE  '010'.      G7A1PGM 
00396                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00397                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00398                      'PROVISION PRICING METHOD REQUIRES FLAT RATE G7A1PGM 
00399 -                    'PER DIEM BE > ZERO       '.                 G7A1PGM 
00400              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00401                                                                   G7A1PGM 
00402 *----------------------------------------------------------------*G7A1PGM 
00403          10  WT-01-ENTRY-011.                                     G7A1PGM 
00404              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00405              15  WT-01-MESSAGE-TEXT-011.                          G7A1PGM 
00406                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00407                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00408                  20  FILLER          PIC X(3)  VALUE  '011'.      G7A1PGM 
00409                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00410                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00411                      'PROVISION PRICING METHOD REQUIRES ECF FLAT RG7A1PGM 
00412 -                    'ATE PER DIEM > ZERO      '.                 G7A1PGM 
00413              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00414                                                                   G7A1PGM 
00415 *----------------------------------------------------------------*G7A1PGM 
00416          10  WT-01-ENTRY-012.                                     G7A1PGM 
00417              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00418              15  WT-01-MESSAGE-TEXT-012.                          G7A1PGM 
00419                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00420                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00421                  20  FILLER          PIC X(3)  VALUE  '012'.      G7A1PGM 
00422                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00423                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00424                      'RATIO BASE CANNOT EQUAL ZEROES              G7A1PGM 
00425 -                    '                         '.                 G7A1PGM 
00426              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00427                                                                   G7A1PGM 
00428 *----------------------------------------------------------------*G7A1PGM 
00429          10  WT-01-ENTRY-013.                                     G7A1PGM 
00430              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00431              15  WT-01-MESSAGE-TEXT-013.                          G7A1PGM 
00432                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00433                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00434                  20  FILLER          PIC X(3)  VALUE  '013'.      G7A1PGM 
00435                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00436                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00437                      'FIELD MUST HAVE NUMERIC VALUES ONLY         G7A1PGM 
00438 -                    '                         '.                 G7A1PGM 
00439              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00440                                                                   G7A1PGM 
00441 *----------------------------------------------------------------*G7A1PGM 
00442          10  WT-01-ENTRY-014.                                     G7A1PGM 
00443              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00444              15  WT-01-MESSAGE-TEXT-014.                          G7A1PGM 
00445                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00446                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00447                  20  FILLER          PIC X(3)  VALUE  '014'.      G7A1PGM 
00448                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00449                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00450                      'FIELD EXCEEDS LENGTH OF 7 POSITIONS  FORMAT G7A1PGM 
00451 -                    'IS 99999.99'.                               G7A1PGM 
00452              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00453                                                                   G7A1PGM 
00454 *----------------------------------------------------------------*G7A1PGM 
00455          10  WT-01-ENTRY-015.                                     G7A1PGM 
00456              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00457              15  WT-01-MESSAGE-TEXT-014.                          G7A1PGM 
00458                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00459                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00460                  20  FILLER          PIC X(3)  VALUE  '015'.      G7A1PGM 
00461                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00462                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00463                      ' INVALID DECIMAL DETECTED'.                 G7A1PGM 
00464              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00465                                                                   G7A1PGM 
00466 *----------------------------------------------------------------*G7A1PGM 
00467          10  WT-01-ENTRY-016.                                     G7A1PGM 
00468              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00469              15  WT-01-MESSAGE-TEXT-014.                          G7A1PGM 
00470                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00471                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00472                  20  FILLER          PIC X(3)  VALUE  '016'.      G7A1PGM 
00473                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00474                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00475                   'FIELD EXCEEDS LENGTH OF 5 POSITIONS   FORMAT ISG7A1PGM 
00476 -                 ' 999.99'.                                      G7A1PGM 
00477              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00478                                                                   G7A1PGM 
00479 *----------------------------------------------------------------*G7A1PGM 
00480          10  WT-01-ENTRY-017.                                     G7A1PGM 
00481              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00482              15  WT-01-MESSAGE-TEXT-017.                          G7A1PGM 
00483                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00484                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00485                  20  FILLER          PIC X(3)  VALUE  '017'.      G7A1PGM 
00486                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00487                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00488                  'FIELD EXCEEDS LENGTH OF 3 POSITIONS   FORMAT IS G7A1PGM 
00489 -                '  99.9'.                                        G7A1PGM 
00490              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00491                                                                   G7A1PGM 
00492 *----------------------------------------------------------------*G7A1PGM 
00493          10  WT-01-ENTRY-018.                                     G7A1PGM 
00494              15  FILLER              PIC X(2)  VALUE '¬>'.        G7A1PGM 
00495              15  WT-01-MESSAGE-TEXT-017.                          G7A1PGM 
00496                  20  FILLER          PIC X(4)  VALUE  'G7A1'.     G7A1PGM 
00497                  20  FILLER          PIC X(1)  VALUE  '-'.        G7A1PGM 
00498                  20  FILLER          PIC X(3)  VALUE  '018'.      G7A1PGM 
00499                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7A1PGM 
00500                  20  FILLER          PIC X(70) VALUE              G7A1PGM 
00501                      '********** F U T U R E   U S E *************G7A1PGM 
00502 -                    '*************************'.                 G7A1PGM 
00503              15  FILLER              PIC X(2)  VALUE '<¬'.        G7A1PGM 
00504                                                                   G7A1PGM 
00505 *----------------------------------------------------------------*G7A1PGM 
00506      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7A1PGM 
00507          WT-01-MESSAGE-VALUES         OCCURS 018 TIMES            G7A1PGM 
00508                                      INDEXED BY WT-01-INDEX.      G7A1PGM 
00509          10  WT-01-ENTRY.                                         G7A1PGM 
00510              15  FILLER              PIC X(02).                   G7A1PGM 
00511              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7A1PGM 
00512              15  FILLER              PIC X(02).                   G7A1PGM 
00513                                                                   G7A1PGM 
00514                                                                   G7A1PGM 
00515 /*** MAP FIELD ATTRIBUTES                                         G7A1PGM 
00516  COPY DFHBMSCA.                                                   G7A1PGM 
00517 *                         AUTOSKIP, BRIGHT, FSET                  G7A1PGM 
00518      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7A1PGM 
00519                                                                   G7A1PGM 
00520 /*** ATTENTION KEYS                                               G7A1PGM 
00521  COPY DFHAID.                                                     G7A1PGM 
00522                                                                   G7A1PGM 
00523 /*** DECIMAL CONVERT COMMAREA                                     G7A1PGM 
00524  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7A1PGM 
00525  COPY GCDCCA01.                                                   G7A1PGM 
00526                                                                   G7A1PGM 
00527 /***  PROVISION MAINTENANCE SCREEN                                G7A1PGM 
00528  COPY  G7A1SETC.                                                  G7A1PGM 
00529                                                                   G7A1PGM 
00530 /*** DATE ROUTINE COMMAREA                                        G7A1PGM 
00531  01  HGADATES-COMMAREA.                                           G7A1PGM 
00532  COPY HGCDAT01.                                                   G7A1PGM 
00533                                                                   G7A1PGM 
00534 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7A1PGM 
00535  01  GCVIOPGM-PARM-LIST.                                          G7A1PGM 
00536  COPY GCVINTRC.                                                   G7A1PGM 
00537                                                                   G7A1PGM 
00538 /*** ALTERNATIVE WORKFILE KEYS                                    G7A1PGM 
00539  01  FILLER.                                                      G7A1PGM 
00540      COPY GCWRKKEY.                                               G7A1PGM 
00541                                                                   G7A1PGM 
00542 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7A1PGM 
00543  01  FILLER.                                                      G7A1PGM 
00544      COPY GCCDRLEN.                                               G7A1PGM 
00545                                                                   G7A1PGM 
00546                                                                   G7A1PGM 
00547  01  WS-END                       PIC X(58) VALUE                 G7A1PGM 
00548      '*** G7A1PGM  WORKING-STORAGE ENDS HERE ***'.                G7A1PGM 
00549 /                                                                 G7A1PGM 
00550  LINKAGE SECTION.                                                 G7A1PGM 
00551 /                                                                 G7A1PGM 
00552  01  DFHCOMMAREA.                                                 G7A1PGM 
00553      COPY  GCWRKDCC.                                              G7A1PGM 
00554      COPY  GCBENPVC.                                              G7A1PGM 
00555 /                                                                 G7A1PGM 
00556 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7A1PGM 
00557  01  IO-PARM-BEN-PROV-AREA.                                       G7A1PGM 
00558      COPY  GCIOPRM2.                                              G7A1PGM 
00559      COPY  GCWRKDC2.                                              G7A1PGM 
00560      COPY  GCBENPV2.                                              G7A1PGM 
00561                                                                   G7A1PGM 
00562 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7A1PGM 
00563  01  IO-PARM-CONTRACT-AREA.                                       G7A1PGM 
00564      COPY  GCIOPRM3.                                              G7A1PGM 
00565      COPY  GCWRKDC3.                                              G7A1PGM 
00566      COPY  GCCONTR2.                                              G7A1PGM 
00567 /                                                                 G7A1PGM 
00568  PROCEDURE DIVISION.                                              G7A1PGM 
00569                                                                   G7A1PGM 
00570 ****************************************************************  G7A1PGM 
00571 *                                                              *  G7A1PGM 
00572 *           P R O C E S S     C O N T R O L                    *  G7A1PGM 
00573 *                                                              *  G7A1PGM 
00574 ****************************************************************  G7A1PGM 
00575  0000-000-PROCESS-CONTROL       SECTION.                          G7A1PGM 
00576  0000-010.                                                        G7A1PGM 
00577                                                                   G7A1PGM 
00578      IF  EIBAID  =  DFHCLEAR                                      G7A1PGM 
00579          EXEC CICS  RETURN                                        G7A1PGM 
00580                     END-EXEC.                                     G7A1PGM 
00581                                                                   G7A1PGM 
00582      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7A1PGM 
00583                                                                   G7A1PGM 
00584      IF  WS-02-MY-EIBTRNID                                        G7A1PGM 
00585      THEN                                                         G7A1PGM 
00586          PERFORM  2000-000-PROCESS-INPUT                          G7A1PGM 
00587      ELSE                                                         G7A1PGM 
00588          PERFORM  1000-000-DISPLAY-SCREEN.                        G7A1PGM 
00589                                                                   G7A1PGM 
00590 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7A1PGM 
00591                                                                   G7A1PGM 
00592      MOVE WS-01-ABCODE-A1L1     TO WS-01-ABCODE                   G7A1PGM 
00593      MOVE WS-01-ABCODE-A1L1-MSG TO WS-01-ABCODE-MSG               G7A1PGM 
00594      PERFORM  9999-000-ABEND-THE-TASK.                            G7A1PGM 
00595                                                                   G7A1PGM 
00596                                                                   G7A1PGM 
00597      GOBACK.                                                      G7A1PGM 
00598                                                                   G7A1PGM 
00599                                                                   G7A1PGM 
00600  0000-900-EXIT.                                                   G7A1PGM 
00601      EXIT.                                                        G7A1PGM 
00602 /***************************************************************  G7A1PGM 
00603 *                                                              *  G7A1PGM 
00604 * 1000  DISPLAY INITIAL SCREEN                                 *  G7A1PGM 
00605 *                                                              *  G7A1PGM 
00606 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7A1PGM 
00607 *                                                              *  G7A1PGM 
00608 ****************************************************************  G7A1PGM 
00609  1000-000-DISPLAY-SCREEN        SECTION.                          G7A1PGM 
00610  1000-010.                                                        G7A1PGM 
00611                                                                   G7A1PGM 
00612 *------- D129    CONVERSION FOR COBOL/2                           G7A1PGM 
00613 *------- SET SCREEN TO LOW VALUES FOR FIRST DISPLAY               G7A1PGM 
00614 *                                                                 G7A1PGM 
00615      MOVE LOW-VALUES TO G7A1I01I.                                 G7A1PGM 
00616                                                                   G7A1PGM 
00617 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7A1PGM 
00618                                                                   G7A1PGM 
00619      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7A1PGM 
00620          MOVE WS-01-ABCODE-A1P1     TO WS-01-ABCODE               G7A1PGM 
00621          MOVE WS-01-ABCODE-A1P1-MSG TO WS-01-ABCODE-MSG           G7A1PGM 
00622          PERFORM 9999-000-ABEND-THE-TASK.                         G7A1PGM 
00623                                                                   G7A1PGM 
00624                                                                   G7A1PGM 
00625 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7A1PGM 
00626                                                                   G7A1PGM 
00627      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7A1PGM 
00628                                         + GC-GCBENPRV-FIXED-LEN   G7A1PGM 
00629                                         + GC-GCBENPRV-VARY-LEN.   G7A1PGM 
00630                                                                   G7A1PGM 
00631                                                                   G7A1PGM 
00632 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7A1PGM 
00633                                                                   G7A1PGM 
00634      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7A1PGM 
00635          MOVE WS-01-ABCODE-A1P2     TO WS-01-ABCODE               G7A1PGM 
00636          MOVE WS-01-ABCODE-A1P2-MSG TO WS-01-ABCODE-MSG           G7A1PGM 
00637          PERFORM 9999-000-ABEND-THE-TASK.                         G7A1PGM 
00638                                                                   G7A1PGM 
00639                                                                   G7A1PGM 
00640 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7A1PGM 
00641 *          BY CALLER IN COMMAREA.                                 G7A1PGM 
00642                                                                   G7A1PGM 
00643      MOVE WRK-PLAN-CODE                      TO S1PLNCDO.         G7A1PGM 
00644      MOVE WRK-GROUP-NUM                      TO S1GRPNOO.         G7A1PGM 
00645      MOVE WRK-SECTION-NUM                    TO S1SECNOO.         G7A1PGM 
00646      MOVE WRK-PKG-CODE                       TO S1PKGCDO.         G7A1PGM 
00647      MOVE WRK-PROV-CTL                       TO S1PRVO.           G7A1PGM 
00648      MOVE WRK-FAM-REL-LEVEL                  TO S1FRLO.           G7A1PGM 
00649      MOVE WRK-L-O-B                          TO S1LOBO.           G7A1PGM 
00650                                                                   G7A1PGM 
00651      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7A1PGM 
00652      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7A1PGM 
00653      IF  HGADATE-RETURN = ZEROS                                   G7A1PGM 
00654      THEN                                                         G7A1PGM 
00655          MOVE DFHBMASF                       TO S1EFFDTA          G7A1PGM 
00656          MOVE HGADATE-DATE2                  TO S1EFFDTO          G7A1PGM 
00657      ELSE                                                         G7A1PGM 
00658          MOVE DFHBMABF                       TO S1EFFDTA          G7A1PGM 
00659          MOVE HGADATE-JULIAN1                TO S1EFFDTO.         G7A1PGM 
00660                                                                   G7A1PGM 
00661      MOVE GCP-PROVN-ID                       TO S1BPVIDO.         G7A1PGM 
00662      MOVE GPA-HOSP-ADM-RESTRN-IND            TO S1HADMRO.         G7A1PGM 
00663                                                                   G7A1PGM 
00664      IF  GPA-HSP-ADM-RESTRN-DAYS = ZEROS                          G7A1PGM 
00665          MOVE WS-02-HEX-F00000               TO S1HADRDO          G7A1PGM 
00666      ELSE                                                         G7A1PGM 
00667          MOVE   GPA-HSP-ADM-RESTRN-DAYS      TO                   G7A1PGM 
00668               WS-02-HSP-ADM-RESTRN-DAYS                           G7A1PGM 
00669          MOVE WS-02-HSP-ADM-RESTRN-DAYS-X    TO S1HADRDO.         G7A1PGM 
00670                                                                   G7A1PGM 
00671      MOVE  GPA-HOSP-COND-RELATSP-IND         TO S1HCNDRO.         G7A1PGM 
00672      MOVE  GPA-REHAB-ADM-RESTRN-IND          TO S1RHADRO.         G7A1PGM 
00673      MOVE  GPA-DAYS-RDCN-RAT-IND             TO S1RDDYIO.         G7A1PGM 
00674                                                                   G7A1PGM 
00675 *    MOVE  GPA-DAYS-RDCN-RAT-BASIC-APL       TO WS-02-RATIO.      G7A1PGM 
00676 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV1BO.         G7A1PGM 
00677 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS1BO.         G7A1PGM 
00678                                                                   G7A1PGM 
00679 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00680 *                                                                 G7A1PGM 
00681      MOVE  GPA-DAYS-RDCN-RAT-BASIC-APL    TO                      G7A1PGM 
00682            WS-02-DAYS-RDCN-RAT-BASIC-APL.                         G7A1PGM 
00683      MOVE  WS-02-DAYS-RDCN-RAT-BASIC-APL  TO WS-02-DISP-3POS-DEC. G7A1PGM 
00684      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRB1O.            G7A1PGM 
00685                                                                   G7A1PGM 
00686 *    MOVE  GPA-DAYS-RDCN-RAT-BASIC-BASE      TO WS-02-RATIO.      G7A1PGM 
00687 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV2BO.         G7A1PGM 
00688 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS2BO.         G7A1PGM 
00689                                                                   G7A1PGM 
00690 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00691 *                                                                 G7A1PGM 
00692      MOVE  GPA-DAYS-RDCN-RAT-BASIC-BASE   TO                      G7A1PGM 
00693            WS-02-DAYS-RDCN-RAT-BASIC-BASE.                        G7A1PGM 
00694      MOVE  WS-02-DAYS-RDCN-RAT-BASIC-BASE TO WS-02-DISP-3POS-DEC. G7A1PGM 
00695      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRB2O.            G7A1PGM 
00696                                                                   G7A1PGM 
00697 *    MOVE  GPA-DAYS-RDCN-RAT-SEC-APL         TO WS-02-RATIO.      G7A1PGM 
00698 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV1SO.         G7A1PGM 
00699 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS1SO.         G7A1PGM 
00700                                                                   G7A1PGM 
00701 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00702 *                                                                 G7A1PGM 
00703      MOVE  GPA-DAYS-RDCN-RAT-SEC-APL      TO                      G7A1PGM 
00704            WS-02-DAYS-RDCN-RAT-SEC-APL.                           G7A1PGM 
00705      MOVE  WS-02-DAYS-RDCN-RAT-SEC-APL    TO WS-02-DISP-3POS-DEC. G7A1PGM 
00706      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRS1O.            G7A1PGM 
00707                                                                   G7A1PGM 
00708 *    MOVE  GPA-DAYS-RDCN-RAT-SEC-BASE        TO WS-02-RATIO.      G7A1PGM 
00709 *    MOVE  WS-02-RATIO-DIVISOR               TO S1DIV2SO.         G7A1PGM 
00710 *    MOVE  WS-02-RATIO-BASE                  TO S1BAS2SO.         G7A1PGM 
00711                                                                   G7A1PGM 
00712 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00713 *                                                                 G7A1PGM 
00714      MOVE  GPA-DAYS-RDCN-RAT-SEC-BASE   TO                        G7A1PGM 
00715            WS-02-DAYS-RDCN-RAT-SEC-BASE.                          G7A1PGM 
00716      MOVE  WS-02-DAYS-RDCN-RAT-SEC-BASE TO WS-02-DISP-3POS-DEC.   G7A1PGM 
00717      MOVE  WS-02-DISP-3POS-DEC            TO S1DRRS2O.            G7A1PGM 
00718                                                                   G7A1PGM 
00719 *    IF  GPA-FLAT-RATE-PDM-AMT = ZEROS                            G7A1PGM 
00720 *        MOVE WS-02-HEX-F00000               TO S1FLPDYO          G7A1PGM 
00721 *    ELSE                                                         G7A1PGM 
00722 *        MOVE   GPA-FLAT-RATE-PDM-AMT        TO                   G7A1PGM 
00723 *             WS-02-FLAT-RATE-PDM-AMT                             G7A1PGM 
00724 *        MOVE WS-02-FLAT-RATE-PDM-AMT-X      TO S1FLPDYO.         G7A1PGM 
00725                                                                   G7A1PGM 
00726 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00727                                                                   G7A1PGM 
00728          MOVE   GPA-FLAT-RATE-PDM-AMT        TO                   G7A1PGM 
00729               WS-02-FLAT-RATE-PDM-AMT.                            G7A1PGM 
00730          MOVE WS-02-FLAT-RATE-PDM-AMT        TO                   G7A1PGM 
00731               WS-02-DISP-7POS-DEC.                                G7A1PGM 
00732          MOVE WS-02-DISP-7POS-DEC TO S1FLPDYO.                    G7A1PGM 
00733                                                                   G7A1PGM 
00734 *    IF  GPA-ECF-F-RAT-PER-DIEM-AMT = ZEROS                       G7A1PGM 
00735 *        MOVE WS-02-HEX-F00000               TO S1EFLPDO          G7A1PGM 
00736 *    ELSE                                                         G7A1PGM 
00737 *        MOVE   GPA-ECF-F-RAT-PER-DIEM-AMT   TO                   G7A1PGM 
00738 *             WS-02-ECF-F-RAT-PER-DIEM-AMT                        G7A1PGM 
00739 *        MOVE WS-02-ECF-F-RAT-PER-DIEM-AMT-X TO S1EFLPDO.         G7A1PGM 
00740                                                                   G7A1PGM 
00741 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00742                                                                   G7A1PGM 
00743          MOVE   GPA-ECF-F-RAT-PER-DIEM-AMT   TO                   G7A1PGM 
00744               WS-02-ECF-F-RAT-PER-DIEM-AMT.                       G7A1PGM 
00745          MOVE WS-02-ECF-F-RAT-PER-DIEM-AMT   TO                   G7A1PGM 
00746               WS-02-DISP-7POS-DEC.                                G7A1PGM 
00747          MOVE WS-02-DISP-7POS-DEC TO S1EFLPDO.                    G7A1PGM 
00748                                                                   G7A1PGM 
00749 *    IF  GPA-ADDN-ALLOW-AMT-PER-DAY = ZEROS                       G7A1PGM 
00750 *        MOVE WS-02-HEX-F00000               TO S1ADALDO          G7A1PGM 
00751 *    ELSE                                                         G7A1PGM 
00752 *        MOVE   GPA-ADDN-ALLOW-AMT-PER-DAY   TO                   G7A1PGM 
00753 *             WS-02-ADDN-ALLOW-AMT-PER-DAY                        G7A1PGM 
00754 *        MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY-X TO S1ADALDO.         G7A1PGM 
00755                                                                   G7A1PGM 
00756 *    D129.    ADDED FOR CONVERSION.                               G7A1PGM 
00757                                                                   G7A1PGM 
00758          MOVE   GPA-ADDN-ALLOW-AMT-PER-DAY   TO                   G7A1PGM 
00759               WS-02-ADDN-ALLOW-AMT-PER-DAY.                       G7A1PGM 
00760          MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY   TO                   G7A1PGM 
00761               WS-02-DISP-5POS-DEC.                                G7A1PGM 
00762          MOVE WS-02-DISP-5POS-DEC TO S1ADALDO.                    G7A1PGM 
00763                                                                   G7A1PGM 
00764      MOVE  GPA-PHYS-EXAM-IND                 TO S1PHEXIO.         G7A1PGM 
00765      MOVE  GPA-STAY-CODE-IND                 TO S1STCDIO.         G7A1PGM 
00766                                                                   G7A1PGM 
00767      IF  GPA-STAY-CD = ZEROS                                      G7A1PGM 
00768          MOVE WS-02-HEX-F00000               TO S1STYCDO          G7A1PGM 
00769      ELSE                                                         G7A1PGM 
00770          MOVE GPA-STAY-CD                    TO WS-02-STAY-CD     G7A1PGM 
00771          MOVE WS-02-STAY-CD-X                TO S1STYCDO.         G7A1PGM 
00772                                                                   G7A1PGM 
00773                                                                   G7A1PGM 
00774                                                                   G7A1PGM 
00775 *------- SEND INITIAL SCREEN ------------------------------------*G7A1PGM 
00776                                                                   G7A1PGM 
00777      MOVE  -1 TO  S1HADMRL.                                       G7A1PGM 
00778      PERFORM 9100-000-SEND-THEN-RETURN.                           G7A1PGM 
00779                                                                   G7A1PGM 
00780                                                                   G7A1PGM 
00781  1000-900-EXIT.                                                   G7A1PGM 
00782      EXIT.                                                        G7A1PGM 
00783 /***************************************************************  G7A1PGM 
00784 *                                                              *  G7A1PGM 
00785 * 2000    P R O C E S S    I N P U T                           *  G7A1PGM 
00786 *                                                              *  G7A1PGM 
00787 ****************************************************************  G7A1PGM 
00788  2000-000-PROCESS-INPUT         SECTION.                          G7A1PGM 
00789  2000-010.                                                        G7A1PGM 
00790                                                                   G7A1PGM 
00791 *------ VALIDATE PFKEY USAGE ------------------------------------*G7A1PGM 
00792                                                                   G7A1PGM 
00793      IF  EIBAID = DFHENTER OR                                     G7A1PGM 
00794                   DFHPF3   OR  DFHPF15 OR                         G7A1PGM 
00795                   DFHPF4   OR  DFHPF16 OR                         G7A1PGM 
00796                   DFHPF6   OR  DFHPF18 OR                         G7A1PGM 
00797                   DFHPF7   OR  DFHPF19 OR                         G7A1PGM 
00798                   DFHPF8   OR  DFHPF20                            G7A1PGM 
00799      THEN                                                         G7A1PGM 
00800          NEXT SENTENCE                                            G7A1PGM 
00801      ELSE                                                         G7A1PGM 
00802          SET WT-01-INDEX TO +01                                   G7A1PGM 
00803          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A1PGM 
00804          PERFORM 9100-000-SEND-THEN-RETURN.                       G7A1PGM 
00805                                                                   G7A1PGM 
00806                                                                   G7A1PGM 
00807                                                                   G7A1PGM 
00808      EXEC CICS  HANDLE CONDITION                                  G7A1PGM 
00809                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7A1PGM 
00810                        END-EXEC.                                  G7A1PGM 
00811                                                                   G7A1PGM 
00812                                                                   G7A1PGM 
00813      EXEC CICS  RECEIVE MAP   ('G7A1I01')                         G7A1PGM 
00814                         MAPSET('G7A1SET')                         G7A1PGM 
00815                         END-EXEC.                                 G7A1PGM 
00816                                                                   G7A1PGM 
00817                                                                   G7A1PGM 
00818      IF  S1FUNCI  NOT = 'G7A1'  OR                                G7A1PGM 
00819          S1SCRNI  NOT = '007A01'                                  G7A1PGM 
00820          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7A1PGM 
00821                                                                   G7A1PGM 
00822                                                                   G7A1PGM 
00823 *--- RETURN TO GCPS MENU? ---------------------------------------*G7A1PGM 
00824                                                                   G7A1PGM 
00825      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7A1PGM 
00826          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7A1PGM 
00827                                                                   G7A1PGM 
00828 *--- PROCESS SCREEN FIELDS --------------------------------------*G7A1PGM 
00829                                                                   G7A1PGM 
00830      PERFORM 2100-000-FIELD-EDITS.                                G7A1PGM 
00831                                                                   G7A1PGM 
00832      IF  WS-02-SCREEN-HAS-ERRORS                                  G7A1PGM 
00833          PERFORM 9100-000-SEND-THEN-RETURN.                       G7A1PGM 
00834                                                                   G7A1PGM 
00835      PERFORM 2200-000-LOGICAL-EDITS.                              G7A1PGM 
00836                                                                   G7A1PGM 
00837      IF  WS-02-SCREEN-HAS-ERRORS                                  G7A1PGM 
00838          PERFORM 9100-000-SEND-THEN-RETURN.                       G7A1PGM 
00839                                                                   G7A1PGM 
00840      PERFORM 2300-000-APPLY-RECORD-CHANGES.                       G7A1PGM 
00841                                                                   G7A1PGM 
00842      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7A1PGM 
00843                                                                   G7A1PGM 
00844                                                                   G7A1PGM 
00845  2000-900-EXIT.                                                   G7A1PGM 
00846      EXIT.                                                        G7A1PGM 
00847 /***************************************************************  G7A1PGM 
00848 *                                                              *  G7A1PGM 
00849 * 2100  DO SCREEN FIELD EDITS                                  *  G7A1PGM 
00850 *                                                              *  G7A1PGM 
00851 ****************************************************************  G7A1PGM 
00852  2100-000-FIELD-EDITS           SECTION.                          G7A1PGM 
00853  2100-010.                                                        G7A1PGM 
00854                                                                   G7A1PGM 
00855 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7A1PGM 
00856                                                                   G7A1PGM 
00857      MOVE DFHBMUNF TO  S1HADMRA                                   G7A1PGM 
00858                        S1HADRDA                                   G7A1PGM 
00859                        S1HCNDRA                                   G7A1PGM 
00860                        S1RHADRA                                   G7A1PGM 
00861                        S1RDDYIA                                   G7A1PGM 
00862                        S1DRRB1A                                   G7A1PGM 
00863                        S1DRRB2A                                   G7A1PGM 
00864                        S1DRRS1A                                   G7A1PGM 
00865                        S1DRRS2A                                   G7A1PGM 
00866                        S1FLPDYA                                   G7A1PGM 
00867                        S1EFLPDA                                   G7A1PGM 
00868                        S1ADALDA                                   G7A1PGM 
00869                        S1PHEXIA                                   G7A1PGM 
00870                        S1STCDIA                                   G7A1PGM 
00871                        S1STYCDA.                                  G7A1PGM 
00872                                                                   G7A1PGM 
00873      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7A1PGM 
00874                                                                   G7A1PGM 
00875                                                                   G7A1PGM 
00876 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION IND ----------*G7A1PGM 
00877 *   1. ALPHANUMERIC                                               G7A1PGM 
00878 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A1PGM 
00879                                                                   G7A1PGM 
00880      MOVE  S1HADMRI TO WS-02-CLASS-TEST-AREA.                     G7A1PGM 
00881      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A1PGM 
00882      THEN                                                         G7A1PGM 
00883          MOVE  S1HADMRI TO GCVI-VALUE                             G7A1PGM 
00884          MOVE  'BPAA01' TO GCVI-FIELDS-KEY-ID                     G7A1PGM 
00885          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A1PGM 
00886          IF  GCVI-VALUE-NOT-FOUND                                 G7A1PGM 
00887          THEN                                                     G7A1PGM 
00888              MOVE  -1        TO  S1HADMRL                         G7A1PGM 
00889              MOVE  DFHBMUBF  TO  S1HADMRA                         G7A1PGM 
00890              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
00891              THEN                                                 G7A1PGM 
00892                  NEXT SENTENCE                                    G7A1PGM 
00893              ELSE                                                 G7A1PGM 
00894                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
00895                  SET WT-01-INDEX TO +09                           G7A1PGM 
00896                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
00897          ELSE                                                     G7A1PGM 
00898              IF  GCVI-VALUE-NOT-LOADED                            G7A1PGM 
00899              THEN                                                 G7A1PGM 
00900                  MOVE  DFHBMUBF  TO  S1HADMRA                     G7A1PGM 
00901              ELSE                                                 G7A1PGM 
00902                  NEXT SENTENCE                                    G7A1PGM 
00903      ELSE                                                         G7A1PGM 
00904          MOVE  -1        TO  S1HADMRL                             G7A1PGM 
00905          MOVE  DFHBMUBF  TO  S1HADMRA                             G7A1PGM 
00906          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
00907          THEN                                                     G7A1PGM 
00908              NEXT SENTENCE                                        G7A1PGM 
00909          ELSE                                                     G7A1PGM 
00910              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
00911              SET WT-01-INDEX TO +08                               G7A1PGM 
00912              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
00913                                                                   G7A1PGM 
00914                                                                   G7A1PGM 
00915                                                                   G7A1PGM 
00916 *-- VALIDATE ------ HOSPITAL ADMISSION RESTRICTION DAYS ---------*G7A1PGM 
00917 *   1. NUMERICS                                                   G7A1PGM 
00918                                                                   G7A1PGM 
00919      IF  S1HADRDI IS NUMERIC                                      G7A1PGM 
00920      THEN                                                         G7A1PGM 
00921          NEXT SENTENCE                                            G7A1PGM 
00922      ELSE                                                         G7A1PGM 
00923          MOVE  -1        TO  S1HADRDL                             G7A1PGM 
00924          MOVE  DFHBMUBF  TO  S1HADRDA                             G7A1PGM 
00925          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
00926          THEN                                                     G7A1PGM 
00927              NEXT SENTENCE                                        G7A1PGM 
00928          ELSE                                                     G7A1PGM 
00929              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
00930              SET WT-01-INDEX TO +13                               G7A1PGM 
00931              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
00932                                                                   G7A1PGM 
00933                                                                   G7A1PGM 
00934 *-- VALIDATE ------ HOSP. COND. RELATIONSHIP IND ----------------*G7A1PGM 
00935 *   1. ALPHANUMERIC                                               G7A1PGM 
00936 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A1PGM 
00937                                                                   G7A1PGM 
00938      MOVE  S1HCNDRI TO WS-02-CLASS-TEST-AREA.                     G7A1PGM 
00939      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A1PGM 
00940      THEN                                                         G7A1PGM 
00941          MOVE  S1HCNDRI TO GCVI-VALUE                             G7A1PGM 
00942          MOVE  'BPAA03' TO GCVI-FIELDS-KEY-ID                     G7A1PGM 
00943          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A1PGM 
00944          IF  GCVI-VALUE-NOT-FOUND                                 G7A1PGM 
00945          THEN                                                     G7A1PGM 
00946              MOVE  -1        TO  S1HCNDRL                         G7A1PGM 
00947              MOVE  DFHBMUBF  TO  S1HCNDRA                         G7A1PGM 
00948              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
00949              THEN                                                 G7A1PGM 
00950                  NEXT SENTENCE                                    G7A1PGM 
00951              ELSE                                                 G7A1PGM 
00952                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
00953                  SET WT-01-INDEX TO +09                           G7A1PGM 
00954                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
00955          ELSE                                                     G7A1PGM 
00956              IF  GCVI-VALUE-NOT-LOADED                            G7A1PGM 
00957              THEN                                                 G7A1PGM 
00958                  MOVE  DFHBMUBF  TO  S1HCNDRA                     G7A1PGM 
00959              ELSE                                                 G7A1PGM 
00960                  NEXT SENTENCE                                    G7A1PGM 
00961      ELSE                                                         G7A1PGM 
00962          MOVE  -1        TO  S1HCNDRL                             G7A1PGM 
00963          MOVE  DFHBMUBF  TO  S1HCNDRA                             G7A1PGM 
00964          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
00965          THEN                                                     G7A1PGM 
00966              NEXT SENTENCE                                        G7A1PGM 
00967          ELSE                                                     G7A1PGM 
00968              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
00969              SET WT-01-INDEX TO +08                               G7A1PGM 
00970              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
00971                                                                   G7A1PGM 
00972                                                                   G7A1PGM 
00973 *-- VALIDATE ------ REHAB. AD. RESTRICTION IND ------------------*G7A1PGM 
00974 *   1. ALPHANUMERIC                                               G7A1PGM 
00975 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A1PGM 
00976                                                                   G7A1PGM 
00977      MOVE  S1RHADRI TO WS-02-CLASS-TEST-AREA.                     G7A1PGM 
00978      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A1PGM 
00979      THEN                                                         G7A1PGM 
00980          MOVE  S1RHADRI TO GCVI-VALUE                             G7A1PGM 
00981          MOVE  'BPAA04' TO GCVI-FIELDS-KEY-ID                     G7A1PGM 
00982          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A1PGM 
00983          IF  GCVI-VALUE-NOT-FOUND                                 G7A1PGM 
00984          THEN                                                     G7A1PGM 
00985              MOVE  -1        TO  S1RHADRL                         G7A1PGM 
00986              MOVE  DFHBMUBF  TO  S1RHADRA                         G7A1PGM 
00987              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
00988              THEN                                                 G7A1PGM 
00989                  NEXT SENTENCE                                    G7A1PGM 
00990              ELSE                                                 G7A1PGM 
00991                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
00992                  SET WT-01-INDEX TO +09                           G7A1PGM 
00993                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
00994          ELSE                                                     G7A1PGM 
00995              IF  GCVI-VALUE-NOT-LOADED                            G7A1PGM 
00996              THEN                                                 G7A1PGM 
00997                  MOVE  DFHBMUBF  TO  S1RHADRA                     G7A1PGM 
00998              ELSE                                                 G7A1PGM 
00999                  NEXT SENTENCE                                    G7A1PGM 
01000      ELSE                                                         G7A1PGM 
01001          MOVE  -1        TO  S1RHADRL                             G7A1PGM 
01002          MOVE  DFHBMUBF  TO  S1RHADRA                             G7A1PGM 
01003          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01004          THEN                                                     G7A1PGM 
01005              NEXT SENTENCE                                        G7A1PGM 
01006          ELSE                                                     G7A1PGM 
01007              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01008              SET WT-01-INDEX TO +08                               G7A1PGM 
01009              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01010                                                                   G7A1PGM 
01011                                                                   G7A1PGM 
01012 *-- VALIDATE ------ DAYS REDUCTION RATIO IND --------------------*G7A1PGM 
01013 *   1. ALPHANUMERIC                                               G7A1PGM 
01014 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A1PGM 
01015                                                                   G7A1PGM 
01016      MOVE  S1RDDYII TO WS-02-CLASS-TEST-AREA.                     G7A1PGM 
01017      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A1PGM 
01018      THEN                                                         G7A1PGM 
01019          MOVE  S1RDDYII TO GCVI-VALUE                             G7A1PGM 
01020          MOVE  'BPAA05' TO GCVI-FIELDS-KEY-ID                     G7A1PGM 
01021          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A1PGM 
01022          IF  GCVI-VALUE-NOT-FOUND                                 G7A1PGM 
01023          THEN                                                     G7A1PGM 
01024              MOVE  -1        TO  S1RDDYIL                         G7A1PGM 
01025              MOVE  DFHBMUBF  TO  S1RDDYIA                         G7A1PGM 
01026              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
01027              THEN                                                 G7A1PGM 
01028                  NEXT SENTENCE                                    G7A1PGM 
01029              ELSE                                                 G7A1PGM 
01030                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01031                  SET WT-01-INDEX TO +09                           G7A1PGM 
01032                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01033          ELSE                                                     G7A1PGM 
01034              IF  GCVI-VALUE-NOT-LOADED                            G7A1PGM 
01035              THEN                                                 G7A1PGM 
01036                  MOVE  DFHBMUBF  TO  S1RDDYIA                     G7A1PGM 
01037              ELSE                                                 G7A1PGM 
01038                  NEXT SENTENCE                                    G7A1PGM 
01039      ELSE                                                         G7A1PGM 
01040          MOVE  -1        TO  S1RDDYIL                             G7A1PGM 
01041          MOVE  DFHBMUBF  TO  S1RDDYIA                             G7A1PGM 
01042          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01043          THEN                                                     G7A1PGM 
01044              NEXT SENTENCE                                        G7A1PGM 
01045          ELSE                                                     G7A1PGM 
01046              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01047              SET WT-01-INDEX TO +08                               G7A1PGM 
01048              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01049                                                                   G7A1PGM 
01050                                                                   G7A1PGM 
01051 *-- VALIDATE ------ DAYS REDUCTION RATIO BASIC (2 FIELDS) -------*G7A1PGM 
01052 *  D129                                                           G7A1PGM 
01053                                                                   G7A1PGM 
01054      MOVE S1DRRB1I TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01055      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01056      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01057      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01058      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01059      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01060          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7A1PGM 
01061              MOVE -1       TO S1DRRB1L                            G7A1PGM 
01062              MOVE DFHBMUBF TO S1DRRB1A                            G7A1PGM 
01063              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01064                  NEXT SENTENCE                                    G7A1PGM 
01065              ELSE                                                 G7A1PGM 
01066                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01067                  SET WT-01-INDEX TO +17                           G7A1PGM 
01068                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01069          ELSE                                                     G7A1PGM 
01070              MOVE D-C-RETURN-FIELD-DEC1                           G7A1PGM 
01071                TO WS-02-DAYS-RDCN-RAT-BASIC-APL                   G7A1PGM 
01072              MOVE WS-02-DAYS-RDCN-RAT-BASIC-APL                   G7A1PGM 
01073                TO WS-02-DISP-3POS-DEC                             G7A1PGM 
01074              MOVE WS-02-DISP-3POS-DEC                             G7A1PGM 
01075                TO S1DRRB1O                                        G7A1PGM 
01076      ELSE                                                         G7A1PGM 
01077          MOVE -1       TO S1DRRB1L                                G7A1PGM 
01078          MOVE DFHBMUBF TO S1DRRB1A                                G7A1PGM 
01079          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01080              NEXT SENTENCE                                        G7A1PGM 
01081          ELSE                                                     G7A1PGM 
01082              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01083              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01084                  SET WT-01-INDEX TO +13                           G7A1PGM 
01085                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01086              ELSE                                                 G7A1PGM 
01087                  SET WT-01-INDEX TO +15                           G7A1PGM 
01088                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01089                                                                   G7A1PGM 
01090                                                                   G7A1PGM 
01091      MOVE S1DRRB2I TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01092      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01093      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01094      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01095      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01096      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01097          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7A1PGM 
01098              MOVE -1       TO S1DRRB2L                            G7A1PGM 
01099              MOVE DFHBMUBF TO S1DRRB2A                            G7A1PGM 
01100              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01101                  NEXT SENTENCE                                    G7A1PGM 
01102              ELSE                                                 G7A1PGM 
01103                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01104                  SET WT-01-INDEX TO +17                           G7A1PGM 
01105                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01106          ELSE                                                     G7A1PGM 
01107              MOVE D-C-RETURN-FIELD-DEC1                           G7A1PGM 
01108                TO WS-02-DAYS-RDCN-RAT-BASIC-BASE                  G7A1PGM 
01109              MOVE WS-02-DAYS-RDCN-RAT-BASIC-BASE                  G7A1PGM 
01110                TO WS-02-DISP-3POS-DEC                             G7A1PGM 
01111              MOVE WS-02-DISP-3POS-DEC                             G7A1PGM 
01112                TO S1DRRB2O                                        G7A1PGM 
01113      ELSE                                                         G7A1PGM 
01114          MOVE -1       TO S1DRRB2L                                G7A1PGM 
01115          MOVE DFHBMUBF TO S1DRRB2A                                G7A1PGM 
01116          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01117              NEXT SENTENCE                                        G7A1PGM 
01118          ELSE                                                     G7A1PGM 
01119              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01120              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01121                  SET WT-01-INDEX TO +13                           G7A1PGM 
01122                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01123              ELSE                                                 G7A1PGM 
01124                  SET WT-01-INDEX TO +15                           G7A1PGM 
01125                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01126                                                                   G7A1PGM 
01127                                                                   G7A1PGM 
01128 *    D-I-V-I-S-O-R                                                G7A1PGM 
01129 *    IF  S1DIV1BI IS NUMERIC                                      G7A1PGM 
01130 *    THEN                                                         G7A1PGM 
01131 *        NEXT SENTENCE                                            G7A1PGM 
01132 *    ELSE                                                         G7A1PGM 
01133 *        MOVE  -1        TO  S1DIV1BL                             G7A1PGM 
01134 *        MOVE  DFHBMUBF  TO  S1DIV1BA                             G7A1PGM 
01135 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01136 *        THEN                                                     G7A1PGM 
01137 *            NEXT SENTENCE                                        G7A1PGM 
01138 *        ELSE                                                     G7A1PGM 
01139 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01140 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01141 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01142                                                                   G7A1PGM 
01143 *    B-A-S-E                                                      G7A1PGM 
01144 *    IF  S1BAS1BI IS NUMERIC                                      G7A1PGM 
01145 *    THEN                                                         G7A1PGM 
01146 *        NEXT SENTENCE                                            G7A1PGM 
01147 *    ELSE                                                         G7A1PGM 
01148 *        MOVE  -1        TO  S1BAS1BL                             G7A1PGM 
01149 *        MOVE  DFHBMUBF  TO  S1BAS1BA                             G7A1PGM 
01150 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01151 *        THEN                                                     G7A1PGM 
01152 *            NEXT SENTENCE                                        G7A1PGM 
01153 *        ELSE                                                     G7A1PGM 
01154 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01155 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01156 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01157                                                                   G7A1PGM 
01158 *    D-I-V-I-S-O-R                                                G7A1PGM 
01159 *    IF  S1DIV2BI IS NUMERIC                                      G7A1PGM 
01160 *    THEN                                                         G7A1PGM 
01161 *        NEXT SENTENCE                                            G7A1PGM 
01162 *    ELSE                                                         G7A1PGM 
01163 *        MOVE  -1        TO  S1DIV2BL                             G7A1PGM 
01164 *        MOVE  DFHBMUBF  TO  S1DIV2BA                             G7A1PGM 
01165 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01166 *        THEN                                                     G7A1PGM 
01167 *            NEXT SENTENCE                                        G7A1PGM 
01168 *        ELSE                                                     G7A1PGM 
01169 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01170 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01171 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01172                                                                   G7A1PGM 
01173 *    B-A-S-E                                                      G7A1PGM 
01174 *    IF  S1BAS2BI IS NUMERIC                                      G7A1PGM 
01175 *    THEN                                                         G7A1PGM 
01176 *        NEXT SENTENCE                                            G7A1PGM 
01177 *    ELSE                                                         G7A1PGM 
01178 *        MOVE  -1        TO  S1BAS2BL                             G7A1PGM 
01179 *        MOVE  DFHBMUBF  TO  S1BAS2BA                             G7A1PGM 
01180 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01181 *        THEN                                                     G7A1PGM 
01182 *            NEXT SENTENCE                                        G7A1PGM 
01183 *        ELSE                                                     G7A1PGM 
01184 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01185 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01186 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01187                                                                   G7A1PGM 
01188                                                                   G7A1PGM 
01189 *-- VALIDATE ------ DAYS REDUCTION RATIO SECONDARY (2 FIELDS) ---*G7A1PGM 
01190 *  D129                                                           G7A1PGM 
01191                                                                   G7A1PGM 
01192      MOVE S1DRRS1I TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01193      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01194      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01195      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01196      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01197      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01198          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7A1PGM 
01199              MOVE -1       TO S1DRRS1L                            G7A1PGM 
01200              MOVE DFHBMUBF TO S1DRRS1A                            G7A1PGM 
01201              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01202                  NEXT SENTENCE                                    G7A1PGM 
01203              ELSE                                                 G7A1PGM 
01204                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01205                  SET WT-01-INDEX TO +17                           G7A1PGM 
01206                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01207          ELSE                                                     G7A1PGM 
01208              MOVE D-C-RETURN-FIELD-DEC1                           G7A1PGM 
01209                TO WS-02-DAYS-RDCN-RAT-SEC-APL                     G7A1PGM 
01210              MOVE WS-02-DAYS-RDCN-RAT-SEC-APL                     G7A1PGM 
01211                TO WS-02-DISP-3POS-DEC                             G7A1PGM 
01212              MOVE WS-02-DISP-3POS-DEC                             G7A1PGM 
01213                TO S1DRRS1O                                        G7A1PGM 
01214      ELSE                                                         G7A1PGM 
01215          MOVE -1       TO S1DRRS1L                                G7A1PGM 
01216          MOVE DFHBMUBF TO S1DRRS1A                                G7A1PGM 
01217          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01218              NEXT SENTENCE                                        G7A1PGM 
01219          ELSE                                                     G7A1PGM 
01220              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01221              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01222                  SET WT-01-INDEX TO +13                           G7A1PGM 
01223                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01224              ELSE                                                 G7A1PGM 
01225                  SET WT-01-INDEX TO +15                           G7A1PGM 
01226                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01227                                                                   G7A1PGM 
01228                                                                   G7A1PGM 
01229      MOVE S1DRRS2I TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01230      MOVE +1 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01231      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01232      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01233      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01234      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01235          IF D-C-RETURN-FIELD-DEC1 > WS-3POS-MAX-AMT               G7A1PGM 
01236              MOVE -1       TO S1DRRS2L                            G7A1PGM 
01237              MOVE DFHBMUBF TO S1DRRS2A                            G7A1PGM 
01238              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01239                  NEXT SENTENCE                                    G7A1PGM 
01240              ELSE                                                 G7A1PGM 
01241                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01242                  SET WT-01-INDEX TO +17                           G7A1PGM 
01243                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01244          ELSE                                                     G7A1PGM 
01245              MOVE D-C-RETURN-FIELD-DEC1                           G7A1PGM 
01246                TO WS-02-DAYS-RDCN-RAT-SEC-BASE                    G7A1PGM 
01247              MOVE WS-02-DAYS-RDCN-RAT-SEC-BASE                    G7A1PGM 
01248                TO WS-02-DISP-3POS-DEC                             G7A1PGM 
01249              MOVE WS-02-DISP-3POS-DEC                             G7A1PGM 
01250                TO S1DRRS2O                                        G7A1PGM 
01251      ELSE                                                         G7A1PGM 
01252          MOVE -1       TO S1DRRS2L                                G7A1PGM 
01253          MOVE DFHBMUBF TO S1DRRS2A                                G7A1PGM 
01254          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01255              NEXT SENTENCE                                        G7A1PGM 
01256          ELSE                                                     G7A1PGM 
01257              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01258              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01259                  SET WT-01-INDEX TO +13                           G7A1PGM 
01260                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01261              ELSE                                                 G7A1PGM 
01262                  SET WT-01-INDEX TO +15                           G7A1PGM 
01263                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01264                                                                   G7A1PGM 
01265 *    D-I-V-I-S-O-R                                                G7A1PGM 
01266 *    IF  S1DIV1SI IS NUMERIC                                      G7A1PGM 
01267 *    THEN                                                         G7A1PGM 
01268 *        NEXT SENTENCE                                            G7A1PGM 
01269 *    ELSE                                                         G7A1PGM 
01270 *        MOVE  -1        TO  S1DIV1SL                             G7A1PGM 
01271 *        MOVE  DFHBMUBF  TO  S1DIV1SA                             G7A1PGM 
01272 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01273 *        THEN                                                     G7A1PGM 
01274 *            NEXT SENTENCE                                        G7A1PGM 
01275 *        ELSE                                                     G7A1PGM 
01276 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01277 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01278 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01279                                                                   G7A1PGM 
01280 *    B-A-S-E                                                      G7A1PGM 
01281 *    IF  S1BAS1SI IS NUMERIC                                      G7A1PGM 
01282 *    THEN                                                         G7A1PGM 
01283 *        NEXT SENTENCE                                            G7A1PGM 
01284 *    ELSE                                                         G7A1PGM 
01285 *        MOVE  -1        TO  S1BAS1SL                             G7A1PGM 
01286 *        MOVE  DFHBMUBF  TO  S1BAS1SA                             G7A1PGM 
01287 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01288 *        THEN                                                     G7A1PGM 
01289 *            NEXT SENTENCE                                        G7A1PGM 
01290 *        ELSE                                                     G7A1PGM 
01291 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01292 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01293 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01294                                                                   G7A1PGM 
01295 *    D-I-V-I-S-O-R                                                G7A1PGM 
01296 *    IF  S1DIV2SI IS NUMERIC                                      G7A1PGM 
01297 *    THEN                                                         G7A1PGM 
01298 *        NEXT SENTENCE                                            G7A1PGM 
01299 *    ELSE                                                         G7A1PGM 
01300 *        MOVE  -1        TO  S1DIV2SL                             G7A1PGM 
01301 *        MOVE  DFHBMUBF  TO  S1DIV2SA                             G7A1PGM 
01302 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01303 *        THEN                                                     G7A1PGM 
01304 *            NEXT SENTENCE                                        G7A1PGM 
01305 *        ELSE                                                     G7A1PGM 
01306 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01307 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01308 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01309                                                                   G7A1PGM 
01310 *    B-A-S-E                                                      G7A1PGM 
01311 *    IF  S1BAS2SI IS NUMERIC                                      G7A1PGM 
01312 *    THEN                                                         G7A1PGM 
01313 *        NEXT SENTENCE                                            G7A1PGM 
01314 *    ELSE                                                         G7A1PGM 
01315 *        MOVE  -1        TO  S1BAS2SL                             G7A1PGM 
01316 *        MOVE  DFHBMUBF  TO  S1BAS2SA                             G7A1PGM 
01317 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01318 *        THEN                                                     G7A1PGM 
01319 *            NEXT SENTENCE                                        G7A1PGM 
01320 *        ELSE                                                     G7A1PGM 
01321 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01322 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01323 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01324                                                                   G7A1PGM 
01325                                                                   G7A1PGM 
01326 *-- VALIDATE ------ FLAT RATE PER-DIEM AMOUNT -------------------*G7A1PGM 
01327 *  D129                                                           G7A1PGM 
01328                                                                   G7A1PGM 
01329      MOVE S1FLPDYI TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01330      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01331      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01332      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01333      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01334      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01335          IF D-C-RETURN-FIELD-DEC2 > WS-7POS-MAX-AMT               G7A1PGM 
01336              MOVE -1       TO S1FLPDYL                            G7A1PGM 
01337              MOVE DFHBMUBF TO S1FLPDYA                            G7A1PGM 
01338              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01339                  NEXT SENTENCE                                    G7A1PGM 
01340              ELSE                                                 G7A1PGM 
01341                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01342                  SET WT-01-INDEX TO +14                           G7A1PGM 
01343                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01344          ELSE                                                     G7A1PGM 
01345              MOVE D-C-RETURN-FIELD-DEC2                           G7A1PGM 
01346                TO WS-02-FLAT-RATE-PDM-AMT                         G7A1PGM 
01347              MOVE WS-02-FLAT-RATE-PDM-AMT                         G7A1PGM 
01348                TO WS-02-DISP-7POS-DEC                             G7A1PGM 
01349              MOVE WS-02-DISP-7POS-DEC                             G7A1PGM 
01350                TO S1FLPDYO                                        G7A1PGM 
01351      ELSE                                                         G7A1PGM 
01352          MOVE -1       TO S1FLPDYL                                G7A1PGM 
01353          MOVE DFHBMUBF TO S1FLPDYA                                G7A1PGM 
01354          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01355              NEXT SENTENCE                                        G7A1PGM 
01356          ELSE                                                     G7A1PGM 
01357              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01358              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01359                  SET WT-01-INDEX TO +13                           G7A1PGM 
01360                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01361              ELSE                                                 G7A1PGM 
01362                  SET WT-01-INDEX TO +15                           G7A1PGM 
01363                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01364                                                                   G7A1PGM 
01365 *    IF  S1FLPDYI IS NUMERIC                                      G7A1PGM 
01366 *    THEN                                                         G7A1PGM 
01367 *        NEXT SENTENCE                                            G7A1PGM 
01368 *    ELSE                                                         G7A1PGM 
01369 *        MOVE  -1        TO  S1FLPDYL                             G7A1PGM 
01370 *        MOVE  DFHBMUBF  TO  S1FLPDYA                             G7A1PGM 
01371 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01372 *        THEN                                                     G7A1PGM 
01373 *            NEXT SENTENCE                                        G7A1PGM 
01374 *        ELSE                                                     G7A1PGM 
01375 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01376 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01377 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01378                                                                   G7A1PGM 
01379                                                                   G7A1PGM 
01380 *-- VALIDATE ------ ECF FLAT RATE PER-DIEM AMOUNT ---------------*G7A1PGM 
01381 *  D129                                                           G7A1PGM 
01382                                                                   G7A1PGM 
01383      MOVE S1EFLPDI TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01384      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01385      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01386      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01387      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01388      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01389          IF D-C-RETURN-FIELD-DEC2 > WS-7POS-MAX-AMT               G7A1PGM 
01390              MOVE -1       TO S1EFLPDL                            G7A1PGM 
01391              MOVE DFHBMUBF TO S1EFLPDA                            G7A1PGM 
01392              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01393                  NEXT SENTENCE                                    G7A1PGM 
01394              ELSE                                                 G7A1PGM 
01395                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01396                  SET WT-01-INDEX TO +14                           G7A1PGM 
01397                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01398          ELSE                                                     G7A1PGM 
01399              MOVE D-C-RETURN-FIELD-DEC2                           G7A1PGM 
01400                TO WS-02-ECF-F-RAT-PER-DIEM-AMT                    G7A1PGM 
01401              MOVE WS-02-ECF-F-RAT-PER-DIEM-AMT                    G7A1PGM 
01402                TO WS-02-DISP-7POS-DEC                             G7A1PGM 
01403              MOVE WS-02-DISP-7POS-DEC                             G7A1PGM 
01404                TO S1EFLPDO                                        G7A1PGM 
01405      ELSE                                                         G7A1PGM 
01406          MOVE -1       TO S1EFLPDL                                G7A1PGM 
01407          MOVE DFHBMUBF TO S1EFLPDA                                G7A1PGM 
01408          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01409              NEXT SENTENCE                                        G7A1PGM 
01410          ELSE                                                     G7A1PGM 
01411              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01412              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01413                  SET WT-01-INDEX TO +13                           G7A1PGM 
01414                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01415              ELSE                                                 G7A1PGM 
01416                  SET WT-01-INDEX TO +15                           G7A1PGM 
01417                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01418                                                                   G7A1PGM 
01419 *    IF  S1EFLPDI IS NUMERIC                                      G7A1PGM 
01420 *    THEN                                                         G7A1PGM 
01421 *        NEXT SENTENCE                                            G7A1PGM 
01422 *    ELSE                                                         G7A1PGM 
01423 *        MOVE  -1        TO  S1EFLPDL                             G7A1PGM 
01424 *        MOVE  DFHBMUBF  TO  S1EFLPDA                             G7A1PGM 
01425 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01426 *        THEN                                                     G7A1PGM 
01427 *            NEXT SENTENCE                                        G7A1PGM 
01428 *        ELSE                                                     G7A1PGM 
01429 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01430 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01431 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01432                                                                   G7A1PGM 
01433                                                                   G7A1PGM 
01434 *-- VALIDATE ------ ADDITIONAL ALLOWANCE AMT PER DAY ------------*G7A1PGM 
01435 *  D129                                                           G7A1PGM 
01436                                                                   G7A1PGM 
01437      MOVE S1ADALDI TO D-C-RECEIVE-FIELD.                          G7A1PGM 
01438      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7A1PGM 
01439      MOVE '00' TO D-C-RETURN-CODE.                                G7A1PGM 
01440      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7A1PGM 
01441      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7A1PGM 
01442      IF D-C-RETURN-CODE = '00'                                    G7A1PGM 
01443          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7A1PGM 
01444              MOVE -1       TO S1ADALDL                            G7A1PGM 
01445              MOVE DFHBMUBF TO S1ADALDA                            G7A1PGM 
01446              IF WS-02-SCREEN-HAS-ERRORS                           G7A1PGM 
01447                  NEXT SENTENCE                                    G7A1PGM 
01448              ELSE                                                 G7A1PGM 
01449                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7A1PGM 
01450                  SET WT-01-INDEX TO +16                           G7A1PGM 
01451                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01452          ELSE                                                     G7A1PGM 
01453              MOVE D-C-RETURN-FIELD-DEC2                           G7A1PGM 
01454                TO WS-02-ADDN-ALLOW-AMT-PER-DAY                    G7A1PGM 
01455              MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY                    G7A1PGM 
01456                TO WS-02-DISP-5POS-DEC                             G7A1PGM 
01457              MOVE WS-02-DISP-5POS-DEC                             G7A1PGM 
01458                TO S1ADALDO                                        G7A1PGM 
01459      ELSE                                                         G7A1PGM 
01460          MOVE -1       TO S1ADALDL                                G7A1PGM 
01461          MOVE DFHBMUBF TO S1ADALDA                                G7A1PGM 
01462          IF WS-02-SCREEN-HAS-ERRORS                               G7A1PGM 
01463              NEXT SENTENCE                                        G7A1PGM 
01464          ELSE                                                     G7A1PGM 
01465              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7A1PGM 
01466              IF D-C-RETURN-CODE = '10'                            G7A1PGM 
01467                  SET WT-01-INDEX TO +13                           G7A1PGM 
01468                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01469              ELSE                                                 G7A1PGM 
01470                  SET WT-01-INDEX TO +15                           G7A1PGM 
01471                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7A1PGM 
01472                                                                   G7A1PGM 
01473 *    IF  S1ADALDI IS NUMERIC                                      G7A1PGM 
01474 *    THEN                                                         G7A1PGM 
01475 *        NEXT SENTENCE                                            G7A1PGM 
01476 *    ELSE                                                         G7A1PGM 
01477 *        MOVE  -1        TO  S1ADALDL                             G7A1PGM 
01478 *        MOVE  DFHBMUBF  TO  S1ADALDA                             G7A1PGM 
01479 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01480 *        THEN                                                     G7A1PGM 
01481 *            NEXT SENTENCE                                        G7A1PGM 
01482 *        ELSE                                                     G7A1PGM 
01483 *            MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01484 *            SET WT-01-INDEX TO +13                               G7A1PGM 
01485 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01486                                                                   G7A1PGM 
01487                                                                   G7A1PGM 
01488 *-- VALIDATE ------ PHYS EXAM INDICATOR -------------------------*G7A1PGM 
01489 *   1. ALPHANUMERIC                                               G7A1PGM 
01490 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A1PGM 
01491                                                                   G7A1PGM 
01492      MOVE  S1PHEXII TO WS-02-CLASS-TEST-AREA.                     G7A1PGM 
01493      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A1PGM 
01494      THEN                                                         G7A1PGM 
01495          MOVE  S1PHEXII TO GCVI-VALUE                             G7A1PGM 
01496          MOVE  'BPBB01' TO GCVI-FIELDS-KEY-ID                     G7A1PGM 
01497          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A1PGM 
01498          IF  GCVI-VALUE-NOT-FOUND                                 G7A1PGM 
01499          THEN                                                     G7A1PGM 
01500              MOVE  -1        TO  S1PHEXIL                         G7A1PGM 
01501              MOVE  DFHBMUBF  TO  S1PHEXIA                         G7A1PGM 
01502              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
01503              THEN                                                 G7A1PGM 
01504                  NEXT SENTENCE                                    G7A1PGM 
01505              ELSE                                                 G7A1PGM 
01506                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01507                  SET WT-01-INDEX TO +09                           G7A1PGM 
01508                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01509          ELSE                                                     G7A1PGM 
01510              IF  GCVI-VALUE-NOT-LOADED                            G7A1PGM 
01511              THEN                                                 G7A1PGM 
01512                  MOVE  DFHBMUBF  TO  S1PHEXIA                     G7A1PGM 
01513              ELSE                                                 G7A1PGM 
01514                  NEXT SENTENCE                                    G7A1PGM 
01515      ELSE                                                         G7A1PGM 
01516          MOVE  -1        TO  S1PHEXIL                             G7A1PGM 
01517          MOVE  DFHBMUBF  TO  S1PHEXIA                             G7A1PGM 
01518          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01519          THEN                                                     G7A1PGM 
01520              NEXT SENTENCE                                        G7A1PGM 
01521          ELSE                                                     G7A1PGM 
01522              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01523              SET WT-01-INDEX TO +08                               G7A1PGM 
01524              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01525                                                                   G7A1PGM 
01526                                                                   G7A1PGM 
01527 *-- VALIDATE ------ STAY CODE INDICATOR -------------------------*G7A1PGM 
01528 *   1. ALPHANUMERIC                                               G7A1PGM 
01529 *   2. FIELD VALIDATION SUB-SYSTEM                                G7A1PGM 
01530                                                                   G7A1PGM 
01531      MOVE  S1STCDII TO WS-02-CLASS-TEST-AREA.                     G7A1PGM 
01532      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7A1PGM 
01533      THEN                                                         G7A1PGM 
01534          MOVE  S1STCDII TO GCVI-VALUE                             G7A1PGM 
01535          MOVE  'BPAA12' TO GCVI-FIELDS-KEY-ID                     G7A1PGM 
01536          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7A1PGM 
01537          IF  GCVI-VALUE-NOT-FOUND                                 G7A1PGM 
01538          THEN                                                     G7A1PGM 
01539              MOVE  -1        TO  S1STCDIL                         G7A1PGM 
01540              MOVE  DFHBMUBF  TO  S1STCDIA                         G7A1PGM 
01541              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
01542              THEN                                                 G7A1PGM 
01543                  NEXT SENTENCE                                    G7A1PGM 
01544              ELSE                                                 G7A1PGM 
01545                  MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01546                  SET WT-01-INDEX TO +09                           G7A1PGM 
01547                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01548          ELSE                                                     G7A1PGM 
01549              IF  GCVI-VALUE-NOT-LOADED                            G7A1PGM 
01550              THEN                                                 G7A1PGM 
01551                  MOVE  DFHBMUBF  TO  S1STCDIA                     G7A1PGM 
01552              ELSE                                                 G7A1PGM 
01553                  NEXT SENTENCE                                    G7A1PGM 
01554      ELSE                                                         G7A1PGM 
01555          MOVE  -1        TO  S1STCDIL                             G7A1PGM 
01556          MOVE  DFHBMUBF  TO  S1STCDIA                             G7A1PGM 
01557          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01558          THEN                                                     G7A1PGM 
01559              NEXT SENTENCE                                        G7A1PGM 
01560          ELSE                                                     G7A1PGM 
01561              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01562              SET WT-01-INDEX TO +08                               G7A1PGM 
01563              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01564                                                                   G7A1PGM 
01565                                                                   G7A1PGM 
01566 *-- VALIDATE ------ STAY CODE -----------------------------------*G7A1PGM 
01567 *   1. NUMERICS                                                   G7A1PGM 
01568                                                                   G7A1PGM 
01569      IF  S1STYCDI IS NUMERIC                                      G7A1PGM 
01570      THEN                                                         G7A1PGM 
01571          NEXT SENTENCE                                            G7A1PGM 
01572      ELSE                                                         G7A1PGM 
01573          MOVE  -1        TO  S1STYCDL                             G7A1PGM 
01574          MOVE  DFHBMUBF  TO  S1STYCDA                             G7A1PGM 
01575          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01576          THEN                                                     G7A1PGM 
01577              NEXT SENTENCE                                        G7A1PGM 
01578          ELSE                                                     G7A1PGM 
01579              MOVE '1'    TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01580              SET WT-01-INDEX TO +13                               G7A1PGM 
01581              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7A1PGM 
01582                                                                   G7A1PGM 
01583                                                                   G7A1PGM 
01584  2100-900-EXIT.                                                   G7A1PGM 
01585      EXIT.                                                        G7A1PGM 
01586 /***************************************************************  G7A1PGM 
01587 *                                                              *  G7A1PGM 
01588 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7A1PGM 
01589 *                                                              *  G7A1PGM 
01590 ****************************************************************  G7A1PGM 
01591  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7A1PGM 
01592  2110-010.                                                        G7A1PGM 
01593                                                                   G7A1PGM 
01594      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7A1PGM 
01595                                                                   G7A1PGM 
01596      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7A1PGM 
01597                       COMMAREA(GCVIOPGM-PARM-LIST)                G7A1PGM 
01598                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7A1PGM 
01599                       END-EXEC.                                   G7A1PGM 
01600                                                                   G7A1PGM 
01601      IF  GCVI-VALUE-NOT-LOADED                                    G7A1PGM 
01602          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7A1PGM 
01603                                                                   G7A1PGM 
01604  2110-900-EXIT.                                                   G7A1PGM 
01605      EXIT.                                                        G7A1PGM 
01606 /***************************************************************  G7A1PGM 
01607 *                                                              *  G7A1PGM 
01608 * 2200  DO SCREEN LOGICAL EDITS                                *  G7A1PGM 
01609 *                                                              *  G7A1PGM 
01610 ****************************************************************  G7A1PGM 
01611  2200-000-LOGICAL-EDITS         SECTION.                          G7A1PGM 
01612  2200-010.                                                        G7A1PGM 
01613                                                                   G7A1PGM 
01614 *----------------------------------------------------------------*G7A1PGM 
01615 *                                                                *G7A1PGM 
01616 *  IF   HOSPITAL ADMISSION RESTRICTION IND (S1HADMR) > ZERO      *G7A1PGM 
01617 *                                                                *G7A1PGM 
01618 *  THEN HOSPITAL ADMISSION RESTRICTION DAYS(S1HADRD):            *G7A1PGM 
01619 *                      MUST BE > ZERO.                           *G7A1PGM 
01620 *                                                                *G7A1PGM 
01621 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7A1PGM 
01622 *                                                                *G7A1PGM 
01623 *----------------------------------------------------------------*G7A1PGM 
01624                                                                   G7A1PGM 
01625      IF  S1HADMRI     > ZEROS                                     G7A1PGM 
01626          AND                                                      G7A1PGM 
01627          S1HADRDI NOT > ZEROS                                     G7A1PGM 
01628      THEN                                                         G7A1PGM 
01629          MOVE  -1        TO  S1HADRDL                             G7A1PGM 
01630          MOVE  DFHBMUBF  TO  S1HADRDA                             G7A1PGM 
01631                              S1HADMRA                             G7A1PGM 
01632          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01633          THEN                                                     G7A1PGM 
01634              NEXT SENTENCE                                        G7A1PGM 
01635          ELSE                                                     G7A1PGM 
01636              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01637              SET WT-01-INDEX TO +02                               G7A1PGM 
01638              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7A1PGM 
01639      ELSE                                                         G7A1PGM 
01640          NEXT SENTENCE.                                           G7A1PGM 
01641                                                                   G7A1PGM 
01642      IF  S1HADRDI     > ZEROS                                     G7A1PGM 
01643          AND                                                      G7A1PGM 
01644          S1HADMRI NOT > ZEROS                                     G7A1PGM 
01645      THEN                                                         G7A1PGM 
01646          MOVE  -1        TO  S1HADMRL                             G7A1PGM 
01647          MOVE  DFHBMUBF  TO  S1HADMRA                             G7A1PGM 
01648                              S1HADRDA                             G7A1PGM 
01649          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01650          THEN                                                     G7A1PGM 
01651              NEXT SENTENCE                                        G7A1PGM 
01652          ELSE                                                     G7A1PGM 
01653              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01654              SET WT-01-INDEX TO +03                               G7A1PGM 
01655              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7A1PGM 
01656      ELSE                                                         G7A1PGM 
01657          NEXT SENTENCE.                                           G7A1PGM 
01658                                                                   G7A1PGM 
01659                                                                   G7A1PGM 
01660 *----------------------------------------------------------------*G7A1PGM 
01661 *                                                                *G7A1PGM 
01662 *  IF   STAY CODE INDICATOR (S1STCDI) > ZERO                     *G7A1PGM 
01663 *                                                                *G7A1PGM 
01664 *  THEN STAY CODE (S1STYCD) MUST BE > ZERO                       *G7A1PGM 
01665 *                                                                *G7A1PGM 
01666 *  THE CONVERSE CONDITION ALSO APPLIES.                          *G7A1PGM 
01667 *                                                                *G7A1PGM 
01668 *----------------------------------------------------------------*G7A1PGM 
01669                                                                   G7A1PGM 
01670      IF  S1STCDII     > ZEROS                                     G7A1PGM 
01671          AND                                                      G7A1PGM 
01672          S1STYCDI NOT > ZEROS                                     G7A1PGM 
01673      THEN                                                         G7A1PGM 
01674          MOVE  -1        TO  S1STYCDL                             G7A1PGM 
01675          MOVE  DFHBMUBF  TO  S1STYCDA                             G7A1PGM 
01676                              S1STCDIA                             G7A1PGM 
01677          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01678          THEN                                                     G7A1PGM 
01679              NEXT SENTENCE                                        G7A1PGM 
01680          ELSE                                                     G7A1PGM 
01681              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01682              SET WT-01-INDEX TO +04                               G7A1PGM 
01683              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7A1PGM 
01684      ELSE                                                         G7A1PGM 
01685          NEXT SENTENCE.                                           G7A1PGM 
01686                                                                   G7A1PGM 
01687      IF  S1STYCDI     > ZEROS                                     G7A1PGM 
01688          AND                                                      G7A1PGM 
01689          S1STCDII NOT > ZEROS                                     G7A1PGM 
01690      THEN                                                         G7A1PGM 
01691          MOVE  -1        TO  S1STCDIL                             G7A1PGM 
01692          MOVE  DFHBMUBF  TO  S1STCDIA                             G7A1PGM 
01693                              S1STYCDA                             G7A1PGM 
01694          IF  WS-02-SCREEN-HAS-ERRORS                              G7A1PGM 
01695          THEN                                                     G7A1PGM 
01696              NEXT SENTENCE                                        G7A1PGM 
01697          ELSE                                                     G7A1PGM 
01698              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01699              SET WT-01-INDEX TO +05                               G7A1PGM 
01700              PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7A1PGM 
01701      ELSE                                                         G7A1PGM 
01702          NEXT SENTENCE.                                           G7A1PGM 
01703                                                                   G7A1PGM 
01704                                                                   G7A1PGM 
01705 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7A1PGM 
01706                                                                   G7A1PGM 
01707      IF  WS-02-SCREEN-HAS-ERRORS                                  G7A1PGM 
01708      THEN                                                         G7A1PGM 
01709          NEXT SENTENCE                                            G7A1PGM 
01710      ELSE                                                         G7A1PGM 
01711          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7A1PGM 
01712          THEN                                                     G7A1PGM 
01713              IF EIBAID = DFHPF4 OR DFHPF16                        G7A1PGM 
01714              THEN                                                 G7A1PGM 
01715                  NEXT SENTENCE                                    G7A1PGM 
01716              ELSE                                                 G7A1PGM 
01717                  MOVE  -1        TO S1ERRL                        G7A1PGM 
01718                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A1PGM 
01719                  SET WT-01-INDEX TO +06                           G7A1PGM 
01720                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01721          ELSE                                                     G7A1PGM 
01722              NEXT SENTENCE.                                       G7A1PGM 
01723                                                                   G7A1PGM 
01724                                                                   G7A1PGM 
01725  2200-900-EXIT.                                                   G7A1PGM 
01726      EXIT.                                                        G7A1PGM 
01727 /***************************************************************  G7A1PGM 
01728 *                                                              *  G7A1PGM 
01729 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7A1PGM 
01730 *        REWRITE TO WORKFILE.                                  *  G7A1PGM 
01731 *                                                              *  G7A1PGM 
01732 ****************************************************************  G7A1PGM 
01733  2300-000-APPLY-RECORD-CHANGES  SECTION.                          G7A1PGM 
01734  2300-010.                                                        G7A1PGM 
01735                                                                   G7A1PGM 
01736 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7A1PGM 
01737                                                                   G7A1PGM 
01738      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7A1PGM 
01739      MOVE GC-GCBENPRV-VARY-MAX-OCUR TO                            G7A1PGM 
01740           GCP2-COUNT-TAB-PROVN-POINTERS.                          G7A1PGM 
01741      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7A1PGM 
01742      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7A1PGM 
01743      IF  NOT GCIO2-GOOD-RETURN                                    G7A1PGM 
01744          MOVE WS-01-ABCODE-A1F2     TO WS-01-ABCODE               G7A1PGM 
01745          MOVE WS-01-ABCODE-A1F2-MSG TO WS-01-ABCODE-MSG           G7A1PGM 
01746          PERFORM  9999-000-ABEND-THE-TASK.                        G7A1PGM 
01747                                                                   G7A1PGM 
01748                                                                   G7A1PGM 
01749 *----- SAVE FIELDS FROM SCREEN THAT CANNOT BE DIRECTLY ----------*G7A1PGM 
01750 *        COMPARED TO THE RECORD                                   G7A1PGM 
01751                                                                   G7A1PGM 
01752      MOVE S1HADRDI    TO WS-02-HSP-ADM-RESTRN-DAYS-X.             G7A1PGM 
01753                                                                   G7A1PGM 
01754 *    MOVE ZEROS       TO WS-02-RATIO.                             G7A1PGM 
01755 *    MOVE S1DIV1BI    TO WS-02-RATIO-DIVISOR.                     G7A1PGM 
01756 *    MOVE S1BAS1BI    TO WS-02-RATIO-BASE.                        G7A1PGM 
01757 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-BASIC-AP-X.          G7A1PGM 
01758                                                                   G7A1PGM 
01759 *    MOVE ZEROS       TO WS-02-RATIO.                             G7A1PGM 
01760 *    MOVE S1DIV2BI    TO WS-02-RATIO-DIVISOR.                     G7A1PGM 
01761 *    MOVE S1BAS2BI    TO WS-02-RATIO-BASE.                        G7A1PGM 
01762 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-BASIC-BA-X.          G7A1PGM 
01763                                                                   G7A1PGM 
01764 *    MOVE ZEROS       TO WS-02-RATIO.                             G7A1PGM 
01765 *    MOVE S1DIV1SI    TO WS-02-RATIO-DIVISOR.                     G7A1PGM 
01766 *    MOVE S1BAS1SI    TO WS-02-RATIO-BASE.                        G7A1PGM 
01767 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-SEC-AP-X.            G7A1PGM 
01768                                                                   G7A1PGM 
01769 *    MOVE ZEROS       TO WS-02-RATIO.                             G7A1PGM 
01770 *    MOVE S1DIV2SI    TO WS-02-RATIO-DIVISOR.                     G7A1PGM 
01771 *    MOVE S1BAS2SI    TO WS-02-RATIO-BASE.                        G7A1PGM 
01772 *    MOVE WS-02-RATIO TO WS-02-DAYS-RDCN-RAT-SEC-BA-X.            G7A1PGM 
01773                                                                   G7A1PGM 
01774 *    MOVE S1FLPDYI    TO WS-02-FLAT-RATE-PDM-AMT-X.               G7A1PGM 
01775 *    MOVE S1EFLPDI    TO WS-02-ECF-F-RAT-PER-DIEM-AMT-X.          G7A1PGM 
01776 *    MOVE S1ADALDI    TO WS-02-ADDN-ALLOW-AMT-PER-DAY-X.          G7A1PGM 
01777      MOVE S1STYCDI    TO WS-02-STAY-CD-X.                         G7A1PGM 
01778                                                                   G7A1PGM 
01779 *----------------------------------------------------------------*G7A1PGM 
01780 *                                                                *G7A1PGM 
01781 *  IF   PROVISION PRICING METHOD = 04, 14, 21, OR 22             *G7A1PGM 
01782 *                                                                *G7A1PGM 
01783 *  THEN BOTH FLAT-RATE-PER-DIEM AND ECF-FLAT-RATE-PER-DIEM       *G7A1PGM 
01784 *                      MUST BE > ZERO.                           *G7A1PGM 
01785 *                                                                *G7A1PGM 
01786 *----------------------------------------------------------------*G7A1PGM 
01787                                                                   G7A1PGM 
01788      IF  GCP2-PROVN-PRICING-METHD = '04' OR '14' OR '21' OR '22'  G7A1PGM 
01789      THEN                                                         G7A1PGM 
01790          IF  S1FLPDYI = ZEROS                                     G7A1PGM 
01791          THEN                                                     G7A1PGM 
01792              MOVE  -1        TO  S1FLPDYL                         G7A1PGM 
01793              MOVE  DFHBMUBF  TO  S1FLPDYA                         G7A1PGM 
01794              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
01795              THEN                                                 G7A1PGM 
01796                  NEXT SENTENCE                                    G7A1PGM 
01797              ELSE                                                 G7A1PGM 
01798                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A1PGM 
01799                  SET WT-01-INDEX TO +10                           G7A1PGM 
01800                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01801                  PERFORM 9100-000-SEND-THEN-RETURN                G7A1PGM 
01802          ELSE                                                     G7A1PGM 
01803              NEXT SENTENCE                                        G7A1PGM 
01804      ELSE                                                         G7A1PGM 
01805          NEXT SENTENCE.                                           G7A1PGM 
01806                                                                   G7A1PGM 
01807      IF  GCP2-PROVN-PRICING-METHD = '04' OR '14' OR '21' OR '22'  G7A1PGM 
01808      THEN                                                         G7A1PGM 
01809          IF  S1EFLPDI = ZEROS                                     G7A1PGM 
01810          THEN                                                     G7A1PGM 
01811              MOVE  -1        TO  S1EFLPDL                         G7A1PGM 
01812              MOVE  DFHBMUBF  TO  S1EFLPDA                         G7A1PGM 
01813              IF  WS-02-SCREEN-HAS-ERRORS                          G7A1PGM 
01814              THEN                                                 G7A1PGM 
01815                  NEXT SENTENCE                                    G7A1PGM 
01816              ELSE                                                 G7A1PGM 
01817                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7A1PGM 
01818                  SET WT-01-INDEX TO +11                           G7A1PGM 
01819                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7A1PGM 
01820                  PERFORM 9100-000-SEND-THEN-RETURN                G7A1PGM 
01821          ELSE                                                     G7A1PGM 
01822              NEXT SENTENCE                                        G7A1PGM 
01823      ELSE                                                         G7A1PGM 
01824          NEXT SENTENCE.                                           G7A1PGM 
01825                                                                   G7A1PGM 
01826                                                                   G7A1PGM 
01827 *------- DEFAULT RATIOS TO 1.0 : 1.0 IF RATIO IND = 0 -----------*G7A1PGM 
01828                                                                   G7A1PGM 
01829 *    IF  S1RDDYII = '0' OR ' '                                    G7A1PGM 
01830 *    THEN                                                         G7A1PGM 
01831 *        MOVE 01.0 TO WS-02-DAYS-RDCN-RAT-BASIC-APL               G7A1PGM 
01832 *                     WS-02-DAYS-RDCN-RAT-BASIC-BASE              G7A1PGM 
01833 *                     WS-02-DAYS-RDCN-RAT-SEC-APL                 G7A1PGM 
01834 *                     WS-02-DAYS-RDCN-RAT-SEC-BASE                G7A1PGM 
01835 *        MOVE '1'  TO S1DIV1BI                                    G7A1PGM 
01836 *                     S1DIV2BI                                    G7A1PGM 
01837 *                     S1DIV1SI                                    G7A1PGM 
01838 *                     S1DIV2SI                                    G7A1PGM 
01839 *        MOVE '0'  TO S1BAS1BI                                    G7A1PGM 
01840 *                     S1BAS2BI                                    G7A1PGM 
01841 *                     S1BAS1SI                                    G7A1PGM 
01842 *                     S1BAS2SI                                    G7A1PGM 
01843 *    ELSE                                                         G7A1PGM 
01844 *        NEXT SENTENCE.                                           G7A1PGM 
01845 *                                                                 G7A1PGM 
01846                                                                   G7A1PGM 
01847 *------- BASE OF RATIOS CANNOT BE 0.0 ---------------------------*G7A1PGM 
01848                                                                   G7A1PGM 
01849 *    IF  WS-02-DAYS-RDCN-RAT-BASIC-BASE = ZEROS                   G7A1PGM 
01850 *    THEN                                                         G7A1PGM 
01851 *        MOVE  -1        TO S1DIV2BL                              G7A1PGM 
01852 *        MOVE  DFHBMUBF  TO S1DIV2BA                              G7A1PGM 
01853 *                           S1BAS2BA                              G7A1PGM 
01854 *        SET WT-01-INDEX TO +12                                   G7A1PGM 
01855 *        MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH             G7A1PGM 
01856 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A1PGM 
01857 *        IF  WS-02-DAYS-RDCN-RAT-SEC-BASE = ZEROS                 G7A1PGM 
01858 *        THEN                                                     G7A1PGM 
01859 *            MOVE  DFHBMUBF  TO S1DIV2SA                          G7A1PGM 
01860 *                               S1BAS2SA                          G7A1PGM 
01861 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7A1PGM 
01862 *        ELSE                                                     G7A1PGM 
01863 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7A1PGM 
01864 *    ELSE                                                         G7A1PGM 
01865 *        IF  WS-02-DAYS-RDCN-RAT-SEC-BASE = ZEROS                 G7A1PGM 
01866 *        THEN                                                     G7A1PGM 
01867 *            MOVE  -1        TO S1DIV2SL                          G7A1PGM 
01868 *            MOVE  DFHBMUBF  TO S1DIV2SA                          G7A1PGM 
01869 *                               S1BAS2SA                          G7A1PGM 
01870 *            SET WT-01-INDEX TO +12                               G7A1PGM 
01871 *            MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7A1PGM 
01872 *            PERFORM 9000-000-MOVE-MSG-TO-SCREEN                  G7A1PGM 
01873 *            PERFORM 9100-000-SEND-THEN-RETURN                    G7A1PGM 
01874 *        ELSE                                                     G7A1PGM 
01875 *            NEXT SENTENCE.                                       G7A1PGM 
01876                                                                   G7A1PGM 
01877                                                                   G7A1PGM 
01878 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7A1PGM 
01879                                                                   G7A1PGM 
01880         MOVE GPA2-DAYS-RDCN-RAT-BASIC-APL TO                      G7A1PGM 
01881           WS-GPA2-DAYS-RDCN-RAT-BAS-APL.                          G7A1PGM 
01882                                                                   G7A1PGM 
01883         MOVE GPA2-DAYS-RDCN-RAT-BASIC-BASE TO                     G7A1PGM 
01884           WS-GPA2-DAYS-RDCN-RAT-BAS-BASE.                         G7A1PGM 
01885                                                                   G7A1PGM 
01886         MOVE GPA2-DAYS-RDCN-RAT-SEC-APL TO                        G7A1PGM 
01887           WS-GPA2-DAYS-RDCN-RAT-SEC-APL.                          G7A1PGM 
01888                                                                   G7A1PGM 
01889         MOVE GPA2-DAYS-RDCN-RAT-SEC-BASE TO                       G7A1PGM 
01890           WS-GPA2-DAYS-RDCN-RAT-SEC-BASE.                         G7A1PGM 
01891                                                                   G7A1PGM 
01892         MOVE GPA2-ECF-F-RAT-PER-DIEM-AMT TO                       G7A1PGM 
01893                               WS-GPA2-ECF-F-RAT-PER-DIEM-AMT.     G7A1PGM 
01894                                                                   G7A1PGM 
01895         MOVE GPA2-FLAT-RATE-PDM-AMT      TO                       G7A1PGM 
01896                               WS-GPA2-FLAT-RATE-PDM-AMT.          G7A1PGM 
01897                                                                   G7A1PGM 
01898         MOVE GPA2-ADDN-ALLOW-AMT-PER-DAY TO                       G7A1PGM 
01899                               WS-GPA2-ADDN-ALLOW-AMT-PER-DAY.     G7A1PGM 
01900                                                                   G7A1PGM 
01901      IF     S1HADMRI              =  GPA2-HOSP-ADM-RESTRN-IND     G7A1PGM 
01902         AND WS-02-HSP-ADM-RESTRN-DAYS                             G7A1PGM 
01903                                   =  GPA2-HSP-ADM-RESTRN-DAYS     G7A1PGM 
01904         AND S1HCNDRI              =  GPA2-HOSP-COND-RELATSP-IND   G7A1PGM 
01905         AND S1RHADRI              =  GPA2-REHAB-ADM-RESTRN-IND    G7A1PGM 
01906         AND S1RDDYII              =  GPA2-DAYS-RDCN-RAT-IND       G7A1PGM 
01907         AND WS-02-DAYS-RDCN-RAT-BASIC-APL                         G7A1PGM 
01908                                 = WS-GPA2-DAYS-RDCN-RAT-BAS-APL   G7A1PGM 
01909         AND WS-02-DAYS-RDCN-RAT-BASIC-BASE                        G7A1PGM 
01910                                 = WS-GPA2-DAYS-RDCN-RAT-BAS-BASE  G7A1PGM 
01911         AND WS-02-DAYS-RDCN-RAT-SEC-APL                           G7A1PGM 
01912                                 = WS-GPA2-DAYS-RDCN-RAT-SEC-APL   G7A1PGM 
01913         AND WS-02-DAYS-RDCN-RAT-SEC-BASE                          G7A1PGM 
01914                                 = WS-GPA2-DAYS-RDCN-RAT-SEC-BASE  G7A1PGM 
01915         AND WS-02-FLAT-RATE-PDM-AMT                               G7A1PGM 
01916                                   =  WS-GPA2-FLAT-RATE-PDM-AMT    G7A1PGM 
01917         AND WS-02-ECF-F-RAT-PER-DIEM-AMT                          G7A1PGM 
01918                                  =  WS-GPA2-ECF-F-RAT-PER-DIEM-AMTG7A1PGM 
01919         AND WS-02-ADDN-ALLOW-AMT-PER-DAY                          G7A1PGM 
01920                                   = WS-GPA2-ADDN-ALLOW-AMT-PER-DAYG7A1PGM 
01921         AND S1PHEXII              =  GPA2-PHYS-EXAM-IND           G7A1PGM 
01922         AND S1STCDII              =  GPA2-STAY-CODE-IND           G7A1PGM 
01923         AND WS-02-STAY-CD         =  GPA2-STAY-CD                 G7A1PGM 
01924      THEN                                                         G7A1PGM 
01925          GO TO 2300-900-EXIT                                      G7A1PGM 
01926      ELSE                                                         G7A1PGM 
01927          NEXT SENTENCE.                                           G7A1PGM 
01928                                                                   G7A1PGM 
01929                                                                   G7A1PGM 
01930 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7A1PGM 
01931                                                                   G7A1PGM 
01932      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7A1PGM 
01933      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7A1PGM 
01934      IF  NOT GCIO2-GOOD-RETURN                                    G7A1PGM 
01935          MOVE WS-01-ABCODE-A1F3     TO WS-01-ABCODE               G7A1PGM 
01936          MOVE WS-01-ABCODE-A1F3-MSG TO WS-01-ABCODE-MSG           G7A1PGM 
01937          PERFORM  9999-000-ABEND-THE-TASK.                        G7A1PGM 
01938                                                                   G7A1PGM 
01939                                                                   G7A1PGM 
01940 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7A1PGM 
01941                                                                   G7A1PGM 
01942      MOVE S1HADMRI              TO GPA2-HOSP-ADM-RESTRN-IND.      G7A1PGM 
01943      MOVE WS-02-HSP-ADM-RESTRN-DAYS                               G7A1PGM 
01944                                 TO GPA2-HSP-ADM-RESTRN-DAYS.      G7A1PGM 
01945      MOVE S1HCNDRI              TO GPA2-HOSP-COND-RELATSP-IND.    G7A1PGM 
01946      MOVE S1RHADRI              TO GPA2-REHAB-ADM-RESTRN-IND.     G7A1PGM 
01947      MOVE S1RDDYII              TO GPA2-DAYS-RDCN-RAT-IND.        G7A1PGM 
01948      MOVE WS-02-DAYS-RDCN-RAT-BASIC-APL                           G7A1PGM 
01949                                 TO GPA2-DAYS-RDCN-RAT-BASIC-APL.  G7A1PGM 
01950      MOVE WS-02-DAYS-RDCN-RAT-BASIC-BASE                          G7A1PGM 
01951                                 TO GPA2-DAYS-RDCN-RAT-BASIC-BASE. G7A1PGM 
01952      MOVE WS-02-DAYS-RDCN-RAT-SEC-APL                             G7A1PGM 
01953                                 TO GPA2-DAYS-RDCN-RAT-SEC-APL.    G7A1PGM 
01954      MOVE WS-02-DAYS-RDCN-RAT-SEC-BASE                            G7A1PGM 
01955                                 TO GPA2-DAYS-RDCN-RAT-SEC-BASE.   G7A1PGM 
01956      MOVE WS-02-FLAT-RATE-PDM-AMT                                 G7A1PGM 
01957                                 TO GPA2-FLAT-RATE-PDM-AMT.        G7A1PGM 
01958      MOVE WS-02-ECF-F-RAT-PER-DIEM-AMT                            G7A1PGM 
01959                                 TO GPA2-ECF-F-RAT-PER-DIEM-AMT.   G7A1PGM 
01960      MOVE WS-02-ADDN-ALLOW-AMT-PER-DAY                            G7A1PGM 
01961                                 TO GPA2-ADDN-ALLOW-AMT-PER-DAY.   G7A1PGM 
01962      MOVE S1PHEXII              TO GPA2-PHYS-EXAM-IND.            G7A1PGM 
01963      MOVE S1STCDII              TO GPA2-STAY-CODE-IND.            G7A1PGM 
01964      MOVE WS-02-STAY-CD         TO GPA2-STAY-CD.                  G7A1PGM 
01965                                                                   G7A1PGM 
01966                                                                   G7A1PGM 
01967 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7A1PGM 
01968                                                                   G7A1PGM 
01969 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7A1PGM 
01970                                                                   G7A1PGM 
01971      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7A1PGM 
01972      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7A1PGM 
01973      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7A1PGM 
01974      IF  NOT GCIO2-GOOD-RETURN                                    G7A1PGM 
01975          MOVE WS-01-ABCODE-A1F4     TO WS-01-ABCODE               G7A1PGM 
01976          MOVE WS-01-ABCODE-A1F4-MSG TO WS-01-ABCODE-MSG           G7A1PGM 
01977          PERFORM  9999-000-ABEND-THE-TASK.                        G7A1PGM 
01978                                                                   G7A1PGM 
01979  2300-900-EXIT.                                                   G7A1PGM 
01980      EXIT.                                                        G7A1PGM 
01981 /***************************************************************  G7A1PGM 
01982 *                                                              *  G7A1PGM 
01983 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7A1PGM 
01984 *                                                              *  G7A1PGM 
01985 ****************************************************************  G7A1PGM 
01986  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7A1PGM 
01987  2310-010.                                                        G7A1PGM 
01988                                                                   G7A1PGM 
01989                                                                   G7A1PGM 
01990 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7A1PGM 
01991                                                                   G7A1PGM 
01992      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7A1PGM 
01993                                         + GC-WORKFILE-KEY-LEN     G7A1PGM 
01994                                         + GC-GCBENPRV-MAX-REC-LEN.G7A1PGM 
01995                                                                   G7A1PGM 
01996      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7A1PGM 
01997                          INITIMG(WS-02-HEX-00)                    G7A1PGM 
01998                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7A1PGM 
01999                          END-EXEC.                                G7A1PGM 
02000                                                                   G7A1PGM 
02001                                                                   G7A1PGM 
02002 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7A1PGM 
02003                                                                   G7A1PGM 
02004      MOVE SPACES                 TO GCIO-CONTRACT-FILE-KEY.       G7A1PGM 
02005      MOVE WRK-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           G7A1PGM 
02006      MOVE WRK-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3.        G7A1PGM 
02007      MOVE WRK-SEC-NO-1           TO GCIO-WRK-SEC-NO-1.            G7A1PGM 
02008      MOVE WRK-PKG-CODE           TO GCIO-WRK-PKG-CODE.            G7A1PGM 
02009      MOVE WRK-EFFECTIVE-DATE     TO GCIO-WRK-EFFECTIVE-DT.        G7A1PGM 
02010                                                                   G7A1PGM 
02011      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7A1PGM 
02012      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7A1PGM 
02013      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7A1PGM 
02014      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7A1PGM 
02015      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7A1PGM 
02016      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7A1PGM 
02017      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7A1PGM 
02018      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7A1PGM 
02019      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7A1PGM 
02020                                                                   G7A1PGM 
02021 *    MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7A1PGM 
02022 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7A1PGM 
02023 *    IF  HGADATE-RETURN = ZEROS                                   G7A1PGM 
02024 *    THEN                                                         G7A1PGM 
02025 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7A1PGM 
02026 *    ELSE                                                         G7A1PGM 
02027 *        SET WT-01-INDEX TO +07                                   G7A1PGM 
02028 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A1PGM 
02029 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7A1PGM 
02030                                                                   G7A1PGM 
02031      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7A1PGM 
02032      MOVE S1BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7A1PGM 
02033      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7A1PGM 
02034      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7A1PGM 
02035      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7A1PGM 
02036      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7A1PGM 
02037      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7A1PGM 
02038                                                                   G7A1PGM 
02039                                                                   G7A1PGM 
02040  2310-900-EXIT.                                                   G7A1PGM 
02041      EXIT.                                                        G7A1PGM 
02042 /***************************************************************  G7A1PGM 
02043 *                                                              *  G7A1PGM 
02044 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7A1PGM 
02045 *                                                              *  G7A1PGM 
02046 ****************************************************************  G7A1PGM 
02047  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7A1PGM 
02048  2400-010.                                                        G7A1PGM 
02049                                                                   G7A1PGM 
02050                                                                   G7A1PGM 
02051      IF  EIBAID = DFHPF7  OR DFHPF19                              G7A1PGM 
02052      THEN                                                         G7A1PGM 
02053          MOVE 'GC6CPGM' TO WS-02-NEXT-PROGRAM.                    G7A1PGM 
02054                                                                   G7A1PGM 
02055      IF  EIBAID = DFHENTER OR                                     G7A1PGM 
02056                   DFHPF4   OR DFHPF16 OR                          G7A1PGM 
02057                   DFHPF8   OR DFHPF20                             G7A1PGM 
02058      THEN                                                         G7A1PGM 
02059          MOVE 'G7A2PGM' TO WS-02-NEXT-PROGRAM.                    G7A1PGM 
02060                                                                   G7A1PGM 
02061      IF  EIBAID = DFHPF6  OR DFHPF18                              G7A1PGM 
02062      THEN                                                         G7A1PGM 
02063          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7A1PGM 
02064                                                                   G7A1PGM 
02065                                                                   G7A1PGM 
02066      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7A1PGM 
02067                       COMMAREA(WORK-RECORD-2)                     G7A1PGM 
02068                       LENGTH  (GCIO2-RECORD-LENGTH)               G7A1PGM 
02069                       END-EXEC.                                   G7A1PGM 
02070                                                                   G7A1PGM 
02071  2400-900-EXIT.                                                   G7A1PGM 
02072      EXIT.                                                        G7A1PGM 
02073 /***************************************************************  G7A1PGM 
02074 *                                                              *  G7A1PGM 
02075 * 2500    LINK TO GX3APGM FOR CONVERSION                       *  G7A1PGM 
02076 *                                                              *  G7A1PGM 
02077 ****************************************************************  G7A1PGM 
02078  2500-LINK-TO-GX3APGM.                                            G7A1PGM 
02079                                                                   G7A1PGM 
02080      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7A1PGM 
02081                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7A1PGM 
02082                       LENGTH  (+51)                               G7A1PGM 
02083                       END-EXEC.                                   G7A1PGM 
02084                                                                   G7A1PGM 
02085                                                                   G7A1PGM 
02086  2500-EXIT.                                                       G7A1PGM 
02087      EXIT.                                                        G7A1PGM 
02088 /***************************************************************  G7A1PGM 
02089 *                                                              *  G7A1PGM 
02090 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7A1PGM 
02091 *         PROVISION RECORD (TYPE=C4)                           *  G7A1PGM 
02092 *                                                              *  G7A1PGM 
02093 ****************************************************************  G7A1PGM 
02094  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7A1PGM 
02095  5000-010.                                                        G7A1PGM 
02096                                                                   G7A1PGM 
02097      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7A1PGM 
02098                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7A1PGM 
02099                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7A1PGM 
02100                       END-EXEC.                                   G7A1PGM 
02101                                                                   G7A1PGM 
02102                                                                   G7A1PGM 
02103  5000-900-EXIT.                                                   G7A1PGM 
02104      EXIT.                                                        G7A1PGM 
02105 /***************************************************************  G7A1PGM 
02106 *                                                              *  G7A1PGM 
02107 * 5100                                                         *  G7A1PGM 
02108 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7A1PGM 
02109 *                                                              *  G7A1PGM 
02110 ****************************************************************  G7A1PGM 
02111  5100-000-W-F-CONTRACT-IO       SECTION.                          G7A1PGM 
02112  5100-010.                                                        G7A1PGM 
02113                                                                   G7A1PGM 
02114      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7A1PGM 
02115                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7A1PGM 
02116                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7A1PGM 
02117                       END-EXEC.                                   G7A1PGM 
02118                                                                   G7A1PGM 
02119                                                                   G7A1PGM 
02120  5100-900-EXIT.                                                   G7A1PGM 
02121      EXIT.                                                        G7A1PGM 
02122 /***************************************************************  G7A1PGM 
02123 *                                                              *  G7A1PGM 
02124 * 9000   MOVE MESSAGE TO SCREEN                                *  G7A1PGM 
02125 *                                                              *  G7A1PGM 
02126 ****************************************************************  G7A1PGM 
02127  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7A1PGM 
02128  9000-010.                                                        G7A1PGM 
02129                                                                   G7A1PGM 
02130      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S1ERRO.              G7A1PGM 
02131                                                                   G7A1PGM 
02132  9000-900-EXIT.                                                   G7A1PGM 
02133      EXIT.                                                        G7A1PGM 
02134 /***************************************************************  G7A1PGM 
02135 *                                                              *  G7A1PGM 
02136 * 9100 SEND SCREEN AND RETURN                                  *  G7A1PGM 
02137 *                                                              *  G7A1PGM 
02138 ****************************************************************  G7A1PGM 
02139  9100-000-SEND-THEN-RETURN      SECTION.                          G7A1PGM 
02140  9100-010.                                                        G7A1PGM 
02141                                                                   G7A1PGM 
02142                                                                   G7A1PGM 
02143 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7A1PGM 
02144      MOVE  -1 TO  S1ERRL.                                         G7A1PGM 
02145                                                                   G7A1PGM 
02146                                                                   G7A1PGM 
02147      IF  WS-02-MY-EIBTRNID                                        G7A1PGM 
02148      THEN                                                         G7A1PGM 
02149          EXEC CICS  SEND MAP('G7A1I01')                           G7A1PGM 
02150                          MAPSET('G7A1SET')                        G7A1PGM 
02151                          DATAONLY                                 G7A1PGM 
02152                          CURSOR                                   G7A1PGM 
02153                          END-EXEC                                 G7A1PGM 
02154      ELSE                                                         G7A1PGM 
02155          EXEC CICS  SEND MAP('G7A1I01')                           G7A1PGM 
02156                          MAPSET('G7A1SET')                        G7A1PGM 
02157                          ERASE                                    G7A1PGM 
02158                          CURSOR                                   G7A1PGM 
02159                          END-EXEC.                                G7A1PGM 
02160                                                                   G7A1PGM 
02161      EXEC CICS RETURN                                             G7A1PGM 
02162                TRANSID  ('G7A1')                                  G7A1PGM 
02163                COMMAREA (DFHCOMMAREA)                             G7A1PGM 
02164                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7A1PGM 
02165                END-EXEC.                                          G7A1PGM 
02166 *                                                                 G7A1PGM 
02167 *    EXEC CICS  RETURN                                            G7A1PGM 
02168 *               END-EXEC.                                         G7A1PGM 
02169 *                                                                 G7A1PGM 
02170 *                                                                 G7A1PGM 
02171  9100-900-EXIT.                                                   G7A1PGM 
02172      EXIT.                                                        G7A1PGM 
02173 /*****************************************************************G7A1PGM 
02174 *                                                                *G7A1PGM 
02175 * 9200    XCTL TO GCPSPGM                                        *G7A1PGM 
02176 *                                                                *G7A1PGM 
02177 *                                                                *G7A1PGM 
02178 ******************************************************************G7A1PGM 
02179  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7A1PGM 
02180  9200-010.                                                        G7A1PGM 
02181                                                                   G7A1PGM 
02182      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7A1PGM 
02183                       END-EXEC.                                   G7A1PGM 
02184                                                                   G7A1PGM 
02185  9200-900-EXIT.                                                   G7A1PGM 
02186      EXIT.                                                        G7A1PGM 
02187 /*****************************************************************G7A1PGM 
02188 *                                                                *G7A1PGM 
02189 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7A1PGM 
02190 *                                                                *G7A1PGM 
02191 *                                                                *G7A1PGM 
02192 ******************************************************************G7A1PGM 
02193  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7A1PGM 
02194  9210-010.                                                        G7A1PGM 
02195                                                                   G7A1PGM 
02196      IF  S1GRPNOI = '000SPS000'                                   G7A1PGM 
02197          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7A1PGM 
02198                           END-EXEC.                               G7A1PGM 
02199                                                                   G7A1PGM 
02200 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7A1PGM 
02201                                                                   G7A1PGM 
02202      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7A1PGM 
02203                                        + GC-WORKFILE-KEY-LEN      G7A1PGM 
02204                                        + GC-GCCONTR-MAX-REC-LEN.  G7A1PGM 
02205                                                                   G7A1PGM 
02206      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7A1PGM 
02207                          INITIMG(WS-02-HEX-00)                    G7A1PGM 
02208                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7A1PGM 
02209                          END-EXEC.                                G7A1PGM 
02210                                                                   G7A1PGM 
02211 **   COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7A1PGM 
02212 **   SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7A1PGM 
02213                                                                   G7A1PGM 
02214 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7A1PGM 
02215                                                                   G7A1PGM 
02216      MOVE GC-GCCONTR-VARY-MAX-OCUR TO                             G7A1PGM 
02217           GCT2-COUNT-BEN-PROVN-POINTERS.                          G7A1PGM 
02218                                                                   G7A1PGM 
02219      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7A1PGM 
02220      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7A1PGM 
02221                                                                   G7A1PGM 
02222      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7A1PGM 
02223      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7A1PGM 
02224      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7A1PGM 
02225      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7A1PGM 
02226      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7A1PGM 
02227      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7A1PGM 
02228      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7A1PGM 
02229      MOVE S1PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7A1PGM 
02230      MOVE S1GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7A1PGM 
02231      MOVE S1SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7A1PGM 
02232      MOVE S1PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7A1PGM 
02233      MOVE S1LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7A1PGM 
02234      MOVE S1PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7A1PGM 
02235      MOVE S1FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7A1PGM 
02236                                                                   G7A1PGM 
02237 *    MOVE S1EFFDTI               TO  HGADATE-DATE1.               G7A1PGM 
02238 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7A1PGM 
02239 *    IF  HGADATE-RETURN = ZEROS                                   G7A1PGM 
02240 *    THEN                                                         G7A1PGM 
02241 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7A1PGM 
02242 *    ELSE                                                         G7A1PGM 
02243 *        SET WT-01-INDEX TO +07                                   G7A1PGM 
02244 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7A1PGM 
02245 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7A1PGM 
02246                                                                   G7A1PGM 
02247      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7A1PGM 
02248      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7A1PGM 
02249      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7A1PGM 
02250      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7A1PGM 
02251      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7A1PGM 
02252      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7A1PGM 
02253      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7A1PGM 
02254                                                                   G7A1PGM 
02255      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7A1PGM 
02256                                                                   G7A1PGM 
02257      IF  NOT GCIO3-GOOD-RETURN                                    G7A1PGM 
02258          MOVE WS-01-ABCODE-A1F1     TO WS-01-ABCODE               G7A1PGM 
02259          MOVE WS-01-ABCODE-A1F1-MSG TO WS-01-ABCODE-MSG           G7A1PGM 
02260          PERFORM  9999-000-ABEND-THE-TASK.                        G7A1PGM 
02261                                                                   G7A1PGM 
02262      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7A1PGM 
02263                       COMMAREA(WORK-RECORD-3)                     G7A1PGM 
02264                       LENGTH  (GCIO3-RECORD-LENGTH)               G7A1PGM 
02265                       END-EXEC.                                   G7A1PGM 
02266                                                                   G7A1PGM 
02267  9210-900-EXIT.                                                   G7A1PGM 
02268      EXIT.                                                        G7A1PGM 
02269 /*****************************************************************G7A1PGM 
02270 *                                                                *G7A1PGM 
02271 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7A1PGM 
02272 *                                                                *G7A1PGM 
02273 *                                                                *G7A1PGM 
02274 ******************************************************************G7A1PGM 
02275  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7A1PGM 
02276  9220-010.                                                        G7A1PGM 
02277                                                                   G7A1PGM 
02278      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7A1PGM 
02279                       END-EXEC.                                   G7A1PGM 
02280                                                                   G7A1PGM 
02281  9220-900-EXIT.                                                   G7A1PGM 
02282      EXIT.                                                        G7A1PGM 
02283 /*****************************************************************G7A1PGM 
02284 *                                                                *G7A1PGM 
02285 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7A1PGM 
02286 *                                                                *G7A1PGM 
02287 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7A1PGM 
02288 *                                                                *G7A1PGM 
02289 ******************************************************************G7A1PGM 
02290  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7A1PGM 
02291  9800-010.                                                        G7A1PGM 
02292                                                                   G7A1PGM 
02293      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7A1PGM 
02294      MOVE 'M'   TO  HGADATE-FORM1.                                G7A1PGM 
02295      MOVE 'J'   TO  HGADATE-FORM2.                                G7A1PGM 
02296      MOVE ZEROS TO  HGADATE-RETURN                                G7A1PGM 
02297                     HGADATE-AMOUNT.                               G7A1PGM 
02298      EXEC CICS LINK PROGRAM ('HGADATES')                          G7A1PGM 
02299                     COMMAREA(HGADATES-COMMAREA)                   G7A1PGM 
02300                     LENGTH  (24)                                  G7A1PGM 
02301                     END-EXEC.                                     G7A1PGM 
02302                                                                   G7A1PGM 
02303  9800-900-900-EXIT.                                               G7A1PGM 
02304      EXIT.                                                        G7A1PGM 
02305 /*****************************************************************G7A1PGM 
02306 *                                                                *G7A1PGM 
02307 * 9810    J U L I A N    T O    G R E G O R I A N                *G7A1PGM 
02308 *                                                                *G7A1PGM 
02309 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7A1PGM 
02310 *                                                                *G7A1PGM 
02311 ******************************************************************G7A1PGM 
02312  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7A1PGM 
02313  9810-010.                                                        G7A1PGM 
02314                                                                   G7A1PGM 
02315      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7A1PGM 
02316      MOVE 'J'   TO  HGADATE-FORM1.                                G7A1PGM 
02317      MOVE 'M'   TO  HGADATE-FORM2.                                G7A1PGM 
02318      MOVE ZEROS TO  HGADATE-RETURN                                G7A1PGM 
02319                     HGADATE-AMOUNT.                               G7A1PGM 
02320      EXEC CICS LINK PROGRAM ('HGADATES')                          G7A1PGM 
02321                     COMMAREA(HGADATES-COMMAREA)                   G7A1PGM 
02322                     LENGTH  (24)                                  G7A1PGM 
02323                     END-EXEC.                                     G7A1PGM 
02324                                                                   G7A1PGM 
02325  9810-900-900-EXIT.                                               G7A1PGM 
02326      EXIT.                                                        G7A1PGM 
02327 /***************************************************************  G7A1PGM 
02328 *                                                              *  G7A1PGM 
02329 * 9999  ABEND THE TASK                                         *  G7A1PGM 
02330 *                                                              *  G7A1PGM 
02331 ****************************************************************  G7A1PGM 
02332  9999-000-ABEND-THE-TASK SECTION.                                 G7A1PGM 
02333  9999-010.                                                        G7A1PGM 
02334                                                                   G7A1PGM 
02335      EXEC CICS  ABEND                                             G7A1PGM 
02336                 ABCODE(WS-01-ABCODE)                              G7A1PGM 
02337                 END-EXEC.                                         G7A1PGM 
02338                                                                   G7A1PGM 
02339  9900-900-EXIT.                                                   G7A1PGM 
02340      EXIT.                                                        G7A1PGM 
