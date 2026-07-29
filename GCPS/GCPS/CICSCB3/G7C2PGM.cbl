00001  ID DIVISION.                                                     12/08/04
00002  PROGRAM-ID.     G7C2PGM.                                         G7C2PGM 
00003 *** THIS IS A COBOL/2 PROGRAM.                                       LV003
00004  AUTHOR.         J.L.ARKEMA.                                      G7C2PGM 
00005  DATE-WRITTEN.   03/11/87.                                        G7C2PGM 
00006  DATE-COMPILED.                                                   G7C2PGM 
00007 ***************************************************************** G7C2PGM 
00008 *                                                               * G7C2PGM 
00009 *       M A I N T E N A N C E     L O G                         * G7C2PGM 
00010 *                                                               * G7C2PGM 
00011 *                                                               * G7C2PGM 
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------* G7C2PGM 
00013 *                                                               * G7C2PGM 
00014 *  N 129      JUNE 88  RKH  COMBINE G7C2 & G7C3 PROGRAMS INTO A * G7C2PGM 
00015 *                           SINGLE PROGRAM AND ADD 6 NEW FIELDS,* G7C2PGM 
00016 *                           AND FIELD VALIDATION TO THE NUMBER  * G7C2PGM 
00017 *                           OF OCCURRANCES FIELDS.              * G7C2PGM 
00018 *                                                               * G7C2PGM 
00019 *  D129       SEPT 89  GDM  CONVERT FOR DECIMAL                 * G7C2PGM 
00020 *                                                               * G7C2PGM 
00021 *  D129       SEPT 89  GDM  CONVERT TO VS COBOL/2               * G7C2PGM 
00022 *                                                               * G7C2PGM 
00023 *  D12009    08/23/91  BSO  -CORRECT ERR MESSAGES IN AREA \
00024 *                           -CORRECT ALPHA CLASS TEST           * G7C2PGM 
00025 *                                                               * G7C2PGM 
00026 *14726/15057 03/25/98  GSP  ADDED PLAN AND PACKAGE CODE AND     * G7C2PGM 
00027 *                           INCREASED GROUP AND SECTION ON      * G7C2PGM 
00028 *                           THE SCREEN.                         * G7C2PGM 
00029 *                                                               * G7C2PGM 
00030 *            12/11/02  AKK  OPID RECOMPILE                      * G7C2PGM 
00031 *                                                               * G7C2PGM 
00032 * P00148     09-02-03 KIKI  RECOMPILE TO CAPTURE RESEQUENCED    * G7C2PGM 
00033 *                           G7C2SET                              *G7C2PGM 
00034 ***************************************************************** G7C2PGM 
00035                                                                   G7C2PGM 
00036 ***************************************************************** G7C2PGM 
00037 *                                                               * G7C2PGM 
00038 *    G7C2PGM  - PROGRAM 2 OF 2 PROGRAMS TO UPDATE THE FORMAT 'C'* G7C2PGM 
00039 *               PORTION OF THE BENEFIT PROVISION RECORD.        * G7C2PGM 
00040 *                                                               * G7C2PGM 
00041 *    TRANSID: G7C2                                              * G7C2PGM 
00042 *    MAPSET:  G7C2SETC    (GIC2PGM WHICH SHARES THIS MAP)       * G7C2PGM 
00043 *    VALGEN:  NONE                                              * G7C2PGM 
00044 *                                                               * G7C2PGM 
00045 *    PROGRAM NARRATIVE:                                         * G7C2PGM 
00046 *                                                               * G7C2PGM 
00047 *        PROGRAM CHECKS FOR TRANS CODE 'G7C2'.  AN INVALID      * G7C2PGM 
00048 *        TRANS CODE CAUSES A SCREEN TO BE BUILT FROM THE COMM   * G7C2PGM 
00049 *        AREA, SENT TO THE USER, AND TO EXIT THE PROGRAM.       * G7C2PGM 
00050 *                                                               * G7C2PGM 
00051 *        THE MAIN FUNCTIONS ARE :                               * G7C2PGM 
00052 *        1. HARDCOPY REQUEST,                                   * G7C2PGM 
00053 *        2. PROCESS INPUT DATA (UPDATE) FIELDS SELECTED BY      * G7C2PGM 
00054 *           USER,                                               * G7C2PGM 
00055 *        3. TEST FOR AN INVALID REQUEST (WRONG PF KEY).         * G7C2PGM 
00056 *                                                               * G7C2PGM 
00057 *        HARDCOPY REQUEST                                       * G7C2PGM 
00058 *           A USER HAS ENTERED EITHER A PF12 OR PF24 KEY.       * G7C2PGM 
00059 *           THIS PROGRAM XCTLS TO PROGRAM HGACOPYP TO PRINT     * G7C2PGM 
00060 *           THE SCREEN BUFFER.                                  * G7C2PGM 
00061 *                                                               * G7C2PGM 
00062 *        PROCESS INPUT DATA (UPDATE).                           * G7C2PGM 
00063 *           A USER HAS ENTERED EITHER A PF6, PF7, PF8, PF18,    * G7C2PGM 
00064 *           PF19, PF20, PF3, PF15, PF4, PF16, OR ENTER KEY TO   * G7C2PGM 
00065 *           GET HERE.  THE PROGRAM RECEIVES A MAP FROM THE      * G7C2PGM 
00066 *           TERMINAL AND CHECKS ITS MAPID.  IF OK, PROCESSING   * G7C2PGM 
00067 *           CONTINUES, OTHERWISE MAPFAIL ACTION IS TAKEN        * G7C2PGM 
00068 *           CONSISTING OF AN XCTL TO 'GCPSPGM'.                 * G7C2PGM 
00069 *                                                               * G7C2PGM 
00070 *           PF3, PF15 ARE REQUESTS FOR A PREVIOUS MENU.  THE    * G7C2PGM 
00071 *           PROGRAM FORMATS A CONTRACT CONTROL WORKFILE KEY AND * G7C2PGM 
00072 *           READS THE WORKFILE FOR THE C2 RECORD WHICH IS USED  * G7C2PGM 
00073 *           AS A DFHCOMMAREA. ONCE COMPLETED CONTROL IS         * G7C2PGM 
00074 *           TRANSFERED VIA XCTL TO PGM 'GC5APGM'.               * G7C2PGM 
00075 *                                                               * G7C2PGM 
00076 *           PF4, PF16 ARE REQUESTS TO OVERRIDE THE VALIDATION   * G7C2PGM 
00077 *                                     -----------------------   * G7C2PGM 
00078 *           TABLE EMPTY ERROR MESSAGE AND THAT MESSAGE ONLY.    * G7C2PGM 
00079 *           -----------------------------------------------     * G7C2PGM 
00080 *                                                               * G7C2PGM 
00081 *           PF4, PF6, PF7, PF8, PF16, PF18, PF19, PF20, OR ENTER* G7C2PGM 
00082 *           WILL CAUSE THIS PROGRAM TO VALIDATE THE SELECTED    * G7C2PGM 
00083 *           INPUT FIELDS FROM THE RECEIVED MAP.  ANY ERRORS WILL* G7C2PGM 
00084 *           CAUSE AN ERROR MESSAGE AND CURSOR POSITION TO BE    * G7C2PGM 
00085 *           SENT BACK TO THE USER.                              * G7C2PGM 
00086 *                                                               * G7C2PGM 
00087 *           IF THE SELECTED FIELDS ARE OK, A WORKFILE RECORD IS * G7C2PGM 
00088 *           READ FOR UPDATE.  THE SELECTED FIELDS ARE MERGED, A * G7C2PGM 
00089 *           NEW DFHCOMMAREA IS BUILT, AND THE UPDATED RECORD IS * G7C2PGM 
00090 *           WRITTEN BACK TO THE FILE.  THE PROGRAM THEN EXITS   * G7C2PGM 
00091 *           VIA XCTL TO A PROGRAM SELECTED BY THE OPERATOR THRU * G7C2PGM 
00092 *           PF KEY LOGIC,                                       * G7C2PGM 
00093 *              PF6/PF18       GOES TO GC8APGM                   * G7C2PGM 
00094 *              PF8/PF20/ENTER GOES TO GC6APGM                   * G7C2PGM 
00095 *              FOR PF7/PF19   GOES TO G7C1PGM                   * G7C2PGM 
00096 *                                                               * G7C2PGM 
00097 *        TEST FOR AN INVALID REQUEST (WRONG PF KEY).            * G7C2PGM 
00098 *           A DISPLAY IS BUILT FROM DFHCOMMAREA AND SENT BACK   * G7C2PGM 
00099 *           TO THE USER.   PROGRAM THEN EXITS.                  * G7C2PGM 
00100 *                                                               * G7C2PGM 
00101 ***************************************************************** G7C2PGM 
00102                                                                   G7C2PGM 
00103  ENVIRONMENT DIVISION.                                            G7C2PGM 
00104  DATA DIVISION.                                                   G7C2PGM 
00105 /                                                                 G7C2PGM 
00106  WORKING-STORAGE SECTION.                                         G7C2PGM 
00107  01  WS-BEGIN                    PIC X(58) VALUE                  G7C2PGM 
00108      '*** G7C2PGM  WORKING-STORAGE BEGINS HERE ***'.              G7C2PGM 
00109                                                                   G7C2PGM 
00110                                                                   G7C2PGM 
00111  01  WS-01-ABEND-AREA.                                            G7C2PGM 
00112      05  FILLER                   PIC X(16)  VALUE                G7C2PGM 
00113          '** ABEND AREA **'.                                      G7C2PGM 
00114                                                                   G7C2PGM 
00115      05  WS-01-ABEND-CODES-AND-MSG.                               G7C2PGM 
00116          10  WS-01-ABCODE               PIC X(04)  VALUE  SPACES. G7C2PGM 
00117          10  WS-01-ABCODE-MSG           PIC X(44)  VALUE  SPACES. G7C2PGM 
00118                                                                   G7C2PGM 
00119          10  WS-01-ABCODE-C2F1          PIC X(04)  VALUE  'C2F1'. G7C2PGM 
00120          10  WS-01-ABCODE-C2F1-MSG      PIC X(44)  VALUE          G7C2PGM 
00121             'W/F CONTRACT CANNOT BE FOUND             '.          G7C2PGM 
00122                                                                   G7C2PGM 
00123          10  WS-01-ABCODE-C2F2          PIC X(04)  VALUE  'C2F2'. G7C2PGM 
00124          10  WS-01-ABCODE-C2F2-MSG      PIC X(44)  VALUE          G7C2PGM 
00125             'W/F BEN PROV CANNOT BE FOUND             '.          G7C2PGM 
00126                                                                   G7C2PGM 
00127          10  WS-01-ABCODE-C2F3          PIC X(04)  VALUE  'C2F3'. G7C2PGM 
00128          10  WS-01-ABCODE-C2F3-MSG      PIC X(44)  VALUE          G7C2PGM 
00129             'W/F BEN PROV CANNOT BE READ FOR UPDATE   '.          G7C2PGM 
00130                                                                   G7C2PGM 
00131          10  WS-01-ABCODE-C2F4          PIC X(04)  VALUE  'C2F4'. G7C2PGM 
00132          10  WS-01-ABCODE-C2F4-MSG      PIC X(44)  VALUE          G7C2PGM 
00133             'W/F BEN PROV CANNOT BE REWRITTEN         '.          G7C2PGM 
00134                                                                   G7C2PGM 
00135          10  WS-01-ABCODE-C3L1          PIC X(04)  VALUE  'C3L1'. G7C2PGM 
00136          10  WS-01-ABCODE-C3L1-MSG      PIC X(44)  VALUE          G7C2PGM 
00137             'THIS PROGRAM SHOULD NEVER RETURN TO HERE '.          G7C2PGM 
00138                                                                   G7C2PGM 
00139          10  WS-01-ABCODE-C3P1          PIC X(04)  VALUE  'C3P1'. G7C2PGM 
00140          10  WS-01-ABCODE-C3P1-MSG      PIC X(44)  VALUE          G7C2PGM 
00141             'ENTRY GAINED FROM UNKNOWN PROGRAM        '.          G7C2PGM 
00142                                                                   G7C2PGM 
00143          10  WS-01-ABCODE-C3P2          PIC X(04)  VALUE  'C3P2'. G7C2PGM 
00144          10  WS-01-ABCODE-C3P2-MSG      PIC X(44)  VALUE          G7C2PGM 
00145             'INVALID COMMAREA RECEIVED FROM CALLER    '.          G7C2PGM 
00146                                                                   G7C2PGM 
00147  01  WS-02-AREA.                                                  G7C2PGM 
00148      05  FILLER                   PIC X(16)  VALUE                G7C2PGM 
00149          '** WS-02-AREA **'.                                      G7C2PGM 
00150      05  WS-02-EIBTRNID                 PIC X(04)  VALUE  SPACES. G7C2PGM 
00151          88  WS-02-VALID-ENTRY-EIBTRNID            VALUES         G7C2PGM 
00152                                                    'GC6A' 'G7C1'. G7C2PGM 
00153          88  WS-02-MY-EIBTRNID                     VALUE  'G7C2'. G7C2PGM 
00154                                                                   G7C2PGM 
00155      05  WS-02-COMPUTED-LENGTHS.                                  G7C2PGM 
00156          10  WS-02-MINIMUM-COMMAREA-LEN PIC S9(4)  COMP VALUE +0. G7C2PGM 
00157          10  WS-02-W-F-GCCONTR-MAX-LEN  PIC S9(4)  COMP VALUE +0. G7C2PGM 
00158          10  WS-02-W-F-GCBENPRV-MAX-LEN PIC S9(4)  COMP VALUE +0. G7C2PGM 
00159                                                                   G7C2PGM 
00160      05  WS-02-HEX-00             PIC X(01)  VALUE  LOW-VALUES.   G7C2PGM 
00161                                                                   G7C2PGM 
00162      05  WS-02-GCVI-PARM-AREA-LEN PIC S9(04) COMP VALUE +19.      G7C2PGM 
00163                                                                   G7C2PGM 
00164      05  WS-02-CLASS-TEST-AREA          PIC X(10)  VALUE  ZEROS.  G7C2PGM 
00165      05  WS-02-CLASS-TEST-DIGIT     REDEFINES                     G7C2PGM 
00166          WS-02-CLASS-TEST-AREA      OCCURS 10 TIMES               G7C2PGM 
00167                                         PIC X.                    G7C2PGM 
00168          88  WS-02-CLASS-ALPHANUMERIC              VALUES         G7C2PGM 
00169                                                    '0' THRU '9'   G7C2PGM 
00170                                                    'A' THRU 'I'   G7C2PGM 
00171                                                    'J' THRU 'R'   G7C2PGM 
00172                                                    'S' THRU 'Z'   G7C2PGM 
00173                                                    SPACE.         G7C2PGM 
00174                                                                   G7C2PGM 
00175      05  WS-02-SCREEN-ERROR-SWITCH      PIC X(01)  VALUE  '0'.    G7C2PGM 
00176          88  WS-02-SCREEN-HAS-NO-ERRORS            VALUE  '0'.    G7C2PGM 
00177          88  WS-02-SCREEN-HAS-ERRORS               VALUE  '1'.    G7C2PGM 
00178                                                                   G7C2PGM 
00179      05  WS-02-GCVI-RETURN-CODE         PIC X(02)  VALUE  '00'.   G7C2PGM 
00180          88  WS-02-GCVI-VALUE-NOT-LOADED           VALUE  '20'.   G7C2PGM 
00181                                                                   G7C2PGM 
00182      05  WS-02-NEXT-PROGRAM             PIC X(08)  VALUE  SPACES. G7C2PGM 
00183                                                                   G7C2PGM 
00184      05  WS-02-HEX-F00000.                                        G7C2PGM 
00185          10  FILLER                     PIC  X(01) VALUE  ZERO.   G7C2PGM 
00186          10  FILLER                     PIC  X(09) VALUE          G7C2PGM 
00187                                                    LOW-VALUES.    G7C2PGM 
00188                                                                   G7C2PGM 
00189 /*****************************************************************G7C2PGM 
00190 *    HOLD AREA FOR NUMERICAL AREA COMPARISION                    *G7C2PGM 
00191 *         REQUIRED BECAUSE DISPLAY USES A DECIMAL POINT          *G7C2PGM 
00192 ******************************************************************G7C2PGM 
00193  01  FILLER.                                                      G7C2PGM 
00194      05  WS-02-MULT-LVL-PROC-PCT-X.                               G7C2PGM 
00195          10  WS-02-MULT-LVL-PROC-PCT    PIC 999V99  VALUE ZEROS.  G7C2PGM 
00196          10  WS-02-PCT-LVL-X            REDEFINES                 G7C2PGM 
00197              WS-02-MULT-LVL-PROC-PCT.                             G7C2PGM 
00198              15  WS-02-IND-LVL-F3         PIC XXX.                G7C2PGM 
00199              15  WS-02-IND-LVL-L2         PIC XX.                 G7C2PGM 
00200                                                                   G7C2PGM 
00201     05   WS-02-MULT-LVL-INPUT-PCT-X.                              G7C2PGM 
00202          10  WS-02-MULT-PCT-C1           PIC XXX.                 G7C2PGM 
00203          10  WS-02-MULT-PCT-C4           PIC X.                   G7C2PGM 
00204          10  WS-02-MULT-PCT-C5           PIC XX.                  G7C2PGM 
00205                                                                   G7C2PGM 
00206 ***** REQUIRED FOR PRIMARY SURGEON DEP PAY PERCENT                G7C2PGM 
00207                                                                   G7C2PGM 
00208      05  WS-02-PRI-SURG-DEP-PCT-X.                                G7C2PGM 
00209          10  WS-02-PRI-SURG-DEPD-PCT    PIC   999.                G7C2PGM 
00210 *        10  WS-02-PRI-SURG-X           REDEFINES                 G7C2PGM 
00211 *            WS-02-PRI-SURG-DEPD-PCT.                             G7C2PGM 
00212 *            15  WS-02-SURG-F1            PIC X.                  G7C2PGM 
00213 *            15  WS-02-SURG-L2            PIC XX.                 G7C2PGM 
00214                                                                   G7C2PGM 
00215     05   WS-02-PRI-SURG-DEP-INPUT-X.                              G7C2PGM 
00216          10  WS-02-PRI-SURG-C1           PIC X.                   G7C2PGM 
00217          10  WS-02-PRI-SURG-C3           PIC XX.                  G7C2PGM 
00218                                                                   G7C2PGM 
00219     05  WS-02-PRI-SUR-DEP-PAY-PCT     PIC   999   VALUE ZEROS.    G7C2PGM 
00220                                                                   G7C2PGM 
00221     05  WS-02-MULT-UNR-PROC-PCT-L1    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00222     05  WS-02-MULT-UNR-PROC-PCT-L2    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00223     05  WS-02-MULT-UNR-PROC-PCT-L3    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00224                                                                   G7C2PGM 
00225     05  WS-02-MULT-REL-PROC-PCT-L1    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00226     05  WS-02-MULT-REL-PROC-PCT-L2    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00227     05  WS-02-MULT-REL-PROC-PCT-L3    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00228                                                                   G7C2PGM 
00229     05  WS-02-MULT-INJ-LVL-PCT-L1     PIC 999V99  VALUE ZEROS.    G7C2PGM 
00230     05  WS-02-MULT-INJ-LVL-PCT-L2     PIC 999V99  VALUE ZEROS.    G7C2PGM 
00231     05  WS-02-MULT-INJ-LVL-PCT-L3     PIC 999V99  VALUE ZEROS.    G7C2PGM 
00232                                                                   G7C2PGM 
00233     05  WS-02-MULT-POD-PROC-PCT-L1    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00234     05  WS-02-MULT-POD-PROC-PCT-L2    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00235     05  WS-02-MULT-POD-PROC-PCT-L3    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00236     05  WS-02-MULT-POD-PROC-PCT-L4    PIC 999V99  VALUE ZEROS.    G7C2PGM 
00237                                                                   G7C2PGM 
00238     05  WS-02-DISP-3POS-DEC           PIC 999.                    G7C2PGM 
00239     05  WS-3POS-MAX-AMT               PIC 9(3) VALUE 999.         G7C2PGM 
00240     05  WS-GPC2-PRIM-SURG-DPD-PAY-PCT PIC 999.                    G7C2PGM 
00241                                                                   G7C2PGM 
00242     05  WS-02-DISP-5POS-DEC           PIC 999.99.                 G7C2PGM 
00243     05  WS-5POS-MAX-AMT               PIC 999V99 VALUE 999.99.    G7C2PGM 
00244     05  WS-GPC2-MULT-UNRL-PROC-1-PCT  PIC 999V99.                 G7C2PGM 
00245     05  WS-GPC2-MULT-UNRL-PROC-2-PCT  PIC 999V99.                 G7C2PGM 
00246     05  WS-GPC2-MULT-UNRL-PROC-3-PCT  PIC 999V99.                 G7C2PGM 
00247     05  WS-GPC2-MULT-RL-PROC-1-PCT    PIC 999V99.                 G7C2PGM 
00248     05  WS-GPC2-MULT-RL-PROC-2-PCT    PIC 999V99.                 G7C2PGM 
00249     05  WS-GPC2-MULT-RL-PROC-3-PCT    PIC 999V99.                 G7C2PGM 
00250     05  WS-GPC2-MULT-INJ-LVL-1-PCT    PIC 999V99.                 G7C2PGM 
00251     05  WS-GPC2-MULT-INJ-LVL-2-PCT    PIC 999V99.                 G7C2PGM 
00252     05  WS-GPC2-MULT-INJ-LVL-3-PCT    PIC 999V99.                 G7C2PGM 
00253     05  WS-GPC2-MULT-POD-PROC-1-PCT   PIC 999V99.                 G7C2PGM 
00254     05  WS-GPC2-MULT-POD-PROC-2-PCT   PIC 999V99.                 G7C2PGM 
00255     05  WS-GPC2-MULT-POD-PROC-3-PCT   PIC 999V99.                 G7C2PGM 
00256     05  WS-GPC2-MULT-POD-PROC-4-PCT   PIC 999V99.                 G7C2PGM 
00257 /                                                                 G7C2PGM 
00258  01  WT-00-G7C2PGM-TABLES.                                        G7C2PGM 
00259      05  FILLER                   PIC X(16)  VALUE                G7C2PGM 
00260          '*G7C2PGM TABLES*'.                                      G7C2PGM 
00261                                                                   G7C2PGM 
00262  01  WT-01-TABLE.                                                 G7C2PGM 
00263      05  FILLER                  PIC X(16) VALUE                  G7C2PGM 
00264          '* WT-01-TABLE  *'.                                      G7C2PGM 
00265 ******************************************************************G7C2PGM 
00266 *    WT-01   MESSAGE TABLE                                       *G7C2PGM 
00267 ******************************************************************G7C2PGM 
00268  01  FILLER.                                                      G7C2PGM 
00269      05  WT-01-MESSAGE-VALUES.                                    G7C2PGM 
00270                                                                   G7C2PGM 
00271 *----------------------------------------------------------------*G7C2PGM 
00272          10  WT-01-ENTRY-001.                                     G7C2PGM 
00273              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00274              15  WT-01-MESSAGE-TEXT-001.                          G7C2PGM 
00275                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00276                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00277                  20  FILLER          PIC X(3)  VALUE  '001'.      G7C2PGM 
00278                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00279                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00280                      ' INVALID PFKEY SELECTION                    G7C2PGM 
00281 -                    '                         '.                 G7C2PGM 
00282              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00283                                                                   G7C2PGM 
00284 *----------------------------------------------------------------*G7C2PGM 
00285          10  WT-01-ENTRY-002.                                     G7C2PGM 
00286              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00287              15  WT-01-MESSAGE-TEXT-002.                          G7C2PGM 
00288                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00289                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00290                  20  FILLER          PIC X(3)  VALUE  '002'.      G7C2PGM 
00291                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00292                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00293                      'PERCENTAGE IS INCONSISTENT WITH OCCURRANCES G7C2PGM 
00294 -                    '                         '.                 G7C2PGM 
00295              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00296                                                                   G7C2PGM 
00297 *----------------------------------------------------------------*G7C2PGM 
00298          10  WT-01-ENTRY-003.                                     G7C2PGM 
00299              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00300              15  WT-01-MESSAGE-TEXT-003.                          G7C2PGM 
00301                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00302                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00303                  20  FILLER          PIC X(3)  VALUE  '003'.      G7C2PGM 
00304                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00305                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00306                      'ONE LEVEL OF PERCENT AND # OF OCCURS MUST BEG7C2PGM 
00307 -                    ' SELECTED WHEN IND IS SET'.                 G7C2PGM 
00308              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00309                                                                   G7C2PGM 
00310 *----------------------------------------------------------------*G7C2PGM 
00311          10  WT-01-ENTRY-004.                                     G7C2PGM 
00312              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00313              15  WT-01-MESSAGE-TEXT-004.                          G7C2PGM 
00314                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00315                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00316                  20  FILLER          PIC X(3)  VALUE  '004'.      G7C2PGM 
00317                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00318                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00319                      '********** F U T U R E   U S E *************G7C2PGM 
00320 -                    '*************************'.                 G7C2PGM 
00321              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00322                                                                   G7C2PGM 
00323 *----------------------------------------------------------------*G7C2PGM 
00324          10  WT-01-ENTRY-005.                                     G7C2PGM 
00325              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00326              15  WT-01-MESSAGE-TEXT-005.                          G7C2PGM 
00327                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00328                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00329                  20  FILLER          PIC X(3)  VALUE  '005'.      G7C2PGM 
00330                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00331                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00332                      'FIELD MUST CONTAIN 2 DECIMIAL PLACES TO BE AG7C2PGM 
00333 -                    ' VALID ENTRY             '.                 G7C2PGM 
00334              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00335                                                                   G7C2PGM 
00336 *----------------------------------------------------------------*G7C2PGM 
00337          10  WT-01-ENTRY-006.                                     G7C2PGM 
00338              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00339              15  WT-01-MESSAGE-TEXT-006.                          G7C2PGM 
00340                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00341                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00342                  20  FILLER          PIC X(3)  VALUE  '006'.      G7C2PGM 
00343                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00344                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00345                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSG7C2PGM 
00346 -                    ' PF4/PF16 TO CONTINUE    '.                 G7C2PGM 
00347              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00348                                                                   G7C2PGM 
00349 *----------------------------------------------------------------*G7C2PGM 
00350          10  WT-01-ENTRY-007.                                     G7C2PGM 
00351              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00352              15  WT-01-MESSAGE-TEXT-007.                          G7C2PGM 
00353                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00354                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00355                  20  FILLER          PIC X(3)  VALUE  '007'.      G7C2PGM 
00356                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00357                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00358                      'EFFECTIVE DATE ON SCREEN IS INVALID - PLEAS G7C2PGM 
00359 -                    'E CALL SYSTEMS           '.                 G7C2PGM 
00360              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00361                                                                   G7C2PGM 
00362 *----------------------------------------------------------------*G7C2PGM 
00363          10  WT-01-ENTRY-008.                                     G7C2PGM 
00364              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00365              15  WT-01-MESSAGE-TEXT-008.                          G7C2PGM 
00366                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00367                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00368                  20  FILLER          PIC X(3)  VALUE  '008'.      G7C2PGM 
00369                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00370                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00371                      'FIELD HAS AN INVALID VALUE                  G7C2PGM 
00372 -                    '                         '.                 G7C2PGM 
00373              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00374                                                                   G7C2PGM 
00375 *----------------------------------------------------------------*G7C2PGM 
00376          10  WT-01-ENTRY-009.                                     G7C2PGM 
00377              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00378              15  WT-01-MESSAGE-TEXT-009.                          G7C2PGM 
00379                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00380                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00381                  20  FILLER          PIC X(3)  VALUE  '009'.      G7C2PGM 
00382                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00383                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00384                      'FIELD HAS AN INVALID VALUE (VALIDATION SUB-SG7C2PGM 
00385 -                    'YSTEM)                   '.                 G7C2PGM 
00386              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00387                                                                   G7C2PGM 
00388 *----------------------------------------------------------------*G7C2PGM 
00389          10  WT-01-ENTRY-010.                                     G7C2PGM 
00390              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00391              15  WT-01-MESSAGE-TEXT-010.                          G7C2PGM 
00392                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00393                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00394                  20  FILLER          PIC X(3)  VALUE  '010'.      G7C2PGM 
00395                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00396                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00397              '    FIELD MUST HAVE NUMERIC VALUES ONLY '.          G7C2PGM 
00398              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00399 *----------------------------------------------------------------*G7C2PGM 
00400          10  WT-01-ENTRY-011.                                     G7C2PGM 
00401              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00402              15  WT-01-MESSAGE-TEXT-011.                          G7C2PGM 
00403                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00404                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00405                  20  FILLER          PIC X(3)  VALUE  '011'.      G7C2PGM 
00406                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00407                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00408                ' INVALID DECIMAL DETECTED'.                       G7C2PGM 
00409              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00410 *----------------------------------------------------------------*G7C2PGM 
00411          10  WT-01-ENTRY-012.                                     G7C2PGM 
00412              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00413              15  WT-01-MESSAGE-TEXT-012.                          G7C2PGM 
00414                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00415                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00416                  20  FILLER          PIC X(3)  VALUE  '012'.      G7C2PGM 
00417                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00418                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00419         'FIELD EXCEEDS LENGTH OF 5 POSITIONS   FORMAT IS 999.99'. G7C2PGM 
00420              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00421 *----------------------------------------------------------------*G7C2PGM 
00422          10  WT-01-ENTRY-013.                                     G7C2PGM 
00423              15  FILLER              PIC X(2)  VALUE '¬>'.        G7C2PGM 
00424              15  WT-01-MESSAGE-TEXT-013.                          G7C2PGM 
00425                  20  FILLER          PIC X(4)  VALUE  'G7C2'.     G7C2PGM 
00426                  20  FILLER          PIC X(1)  VALUE  '-'.        G7C2PGM 
00427                  20  FILLER          PIC X(3)  VALUE  '013'.      G7C2PGM 
00428                  20  FILLER          PIC X(1)  VALUE  SPACE.      G7C2PGM 
00429                  20  FILLER          PIC X(70) VALUE              G7C2PGM 
00430          ' INVALID DECIMAL DETECTED       FORMAT IS 999'.         G7C2PGM 
00431              15  FILLER              PIC X(2)  VALUE '<¬'.        G7C2PGM 
00432 *----------------------------------------------------------------*G7C2PGM 
00433      05  WT-01-MESSAGE-TABLE         REDEFINES                    G7C2PGM 
00434          WT-01-MESSAGE-VALUES         OCCURS 013 TIMES            G7C2PGM 
00435                                      INDEXED BY WT-01-INDEX.      G7C2PGM 
00436          10  WT-01-ENTRY.                                         G7C2PGM 
00437              15  FILLER              PIC X(02).                   G7C2PGM 
00438              15  WT-01-MESSAGE-TEXT  PIC X(79).                   G7C2PGM 
00439              15  FILLER              PIC X(02).                   G7C2PGM 
00440                                                                   G7C2PGM 
00441                                                                   G7C2PGM 
00442 /*** MAP FIELD ATTRIBUTES                                         G7C2PGM 
00443  COPY DFHBMSCA.                                                   G7C2PGM 
00444 *                         AUTOSKIP, BRIGHT, FSET                  G7C2PGM 
00445      02  DFHBMABF         PIC X  VALUE 'Z'.                       G7C2PGM 
00446                                                                   G7C2PGM 
00447 /*** ATTENTION KEYS                                               G7C2PGM 
00448  COPY DFHAID.                                                     G7C2PGM 
00449                                                                   G7C2PGM 
00450 /***  PROVISION MAINTENANCE SCREEN                                G7C2PGM 
00451  COPY  G7C2SETC.                                                  G7C2PGM 
00452                                                                   G7C2PGM 
00453 /*** DATE ROUTINE COMMAREA                                        G7C2PGM 
00454  01  HGADATES-COMMAREA.                                           G7C2PGM 
00455  COPY HGCDAT01.                                                   G7C2PGM 
00456                                                                   G7C2PGM 
00457 /*** DECIMAL CONVERT COMMAREA                                     G7C2PGM 
00458  01  WS-DECIMAL-CONVERT-COMMAREA.                                 G7C2PGM 
00459  COPY GCDCCA01.                                                   G7C2PGM 
00460                                                                   G7C2PGM 
00461 /*** VALIDATION SUB-SYSTEM PARM LIST                              G7C2PGM 
00462  01  GCVIOPGM-PARM-LIST.                                          G7C2PGM 
00463  COPY GCVINTRC.                                                   G7C2PGM 
00464                                                                   G7C2PGM 
00465 /*** ALTERNATIVE WORKFILE KEYS                                    G7C2PGM 
00466  01  FILLER.                                                      G7C2PGM 
00467      COPY GCWRKKEY.                                               G7C2PGM 
00468                                                                   G7C2PGM 
00469 /*** GENERIC CONTRACT GLOBALLY DEFINED LENGTHS                    G7C2PGM 
00470  01  FILLER.                                                      G7C2PGM 
00471      COPY GCCDRLEN.                                               G7C2PGM 
00472                                                                   G7C2PGM 
00473                                                                   G7C2PGM 
00474  01  WS-END                       PIC X(58) VALUE                 G7C2PGM 
00475      '*** G7C2PGM  WORKING-STORAGE ENDS HERE ***'.                G7C2PGM 
00476 /                                                                 G7C2PGM 
00477  LINKAGE SECTION.                                                 G7C2PGM 
00478 /                                                                 G7C2PGM 
00479  01  DFHCOMMAREA.                                                 G7C2PGM 
00480      COPY  GCWRKDCC.                                              G7C2PGM 
00481      COPY  GCBENPVC.                                              G7C2PGM 
00482 /                                                                 G7C2PGM 
00483 *01  BLL-CELLS.                                                   G7C2PGM 
00484 *    05  FILLER                   PIC S9(08)  COMP.               G7C2PGM 
00485 *    05  BEN-PROV-PNTR            PIC S9(08)  COMP.               G7C2PGM 
00486 *    05  CONTRACT-PNTR            PIC S9(08)  COMP.               G7C2PGM 
00487 *    05  CONTRACT-PNTR-2          PIC S9(08)  COMP.               G7C2PGM 
00488                                                                   G7C2PGM 
00489 **** IO PARM, WORKFILE KEY, BENEFIT PROVISION RECORD              G7C2PGM 
00490  01  IO-PARM-BEN-PROV-AREA.                                       G7C2PGM 
00491      COPY  GCIOPRM2.                                              G7C2PGM 
00492      COPY  GCWRKDC2.                                              G7C2PGM 
00493      COPY  GCBENPV2.                                              G7C2PGM 
00494                                                                   G7C2PGM 
00495 /*** IO PARM, WORKFILE KEY, CONTRACT RECORD                       G7C2PGM 
00496  01  IO-PARM-CONTRACT-AREA.                                       G7C2PGM 
00497      COPY  GCIOPRM3.                                              G7C2PGM 
00498      COPY  GCWRKDC3.                                              G7C2PGM 
00499      COPY  GCCONTR2.                                              G7C2PGM 
00500 /                                                                 G7C2PGM 
00501  PROCEDURE DIVISION.                                              G7C2PGM 
00502                                                                   G7C2PGM 
00503 ****************************************************************  G7C2PGM 
00504 *                                                              *  G7C2PGM 
00505 *           P R O C E S S     C O N T R O L                    *  G7C2PGM 
00506 *                                                              *  G7C2PGM 
00507 ****************************************************************  G7C2PGM 
00508  0000-000-PROCESS-CONTROL       SECTION.                          G7C2PGM 
00509  0000-010.                                                        G7C2PGM 
00510                                                                   G7C2PGM 
00511 *    SERVICE RELOAD  BLL-CELLS.                                   G7C2PGM 
00512                                                                   G7C2PGM 
00513      IF  EIBAID  =  DFHCLEAR                                      G7C2PGM 
00514          EXEC CICS  RETURN                                        G7C2PGM 
00515                     END-EXEC.                                     G7C2PGM 
00516                                                                   G7C2PGM 
00517      MOVE EIBTRNID TO WS-02-EIBTRNID.                             G7C2PGM 
00518                                                                   G7C2PGM 
00519      IF  WS-02-MY-EIBTRNID                                        G7C2PGM 
00520      THEN                                                         G7C2PGM 
00521          PERFORM  2000-000-PROCESS-INPUT                          G7C2PGM 
00522      ELSE                                                         G7C2PGM 
00523          PERFORM  1000-000-DISPLAY-SCREEN.                        G7C2PGM 
00524                                                                   G7C2PGM 
00525                                                                   G7C2PGM 
00526 *---- THIS PROGRAM SHOULD NEVER RETURN TO HERE, ABEND -----------*G7C2PGM 
00527                                                                   G7C2PGM 
00528      MOVE WS-01-ABCODE-C3L1     TO WS-01-ABCODE                   G7C2PGM 
00529      MOVE WS-01-ABCODE-C3L1-MSG TO WS-01-ABCODE-MSG               G7C2PGM 
00530      PERFORM  9999-000-ABEND-THE-TASK.                            G7C2PGM 
00531                                                                   G7C2PGM 
00532      GOBACK.                                                      G7C2PGM 
00533                                                                   G7C2PGM 
00534                                                                   G7C2PGM 
00535  0000-900-EXIT.                                                   G7C2PGM 
00536      EXIT.                                                        G7C2PGM 
00537 /***************************************************************  G7C2PGM 
00538 *                                                              *  G7C2PGM 
00539 * 1000  DISPLAY INITIAL SCREEN                                 *  G7C2PGM 
00540 *                                                              *  G7C2PGM 
00541 *     BUILD AND DISPLAY INITIAL SCREEN                         *  G7C2PGM 
00542 *                                                              *  G7C2PGM 
00543 ****************************************************************  G7C2PGM 
00544  1000-000-DISPLAY-SCREEN        SECTION.                          G7C2PGM 
00545  1000-010.                                                        G7C2PGM 
00546                                                                   G7C2PGM 
00547 *------- MOVE LOW VALUES TO SCREEN FOR FIRST SEND                 G7C2PGM 
00548 *                                                                 G7C2PGM 
00549      MOVE LOW-VALUES TO G7C2I01I.                                 G7C2PGM 
00550                                                                   G7C2PGM 
00551 *------- IF ENTRY IS NOT FROM A LEGITIMATE MODULE, ABEND --------*G7C2PGM 
00552                                                                   G7C2PGM 
00553      IF  NOT WS-02-VALID-ENTRY-EIBTRNID                           G7C2PGM 
00554          MOVE WS-01-ABCODE-C3P1     TO WS-01-ABCODE               G7C2PGM 
00555          MOVE WS-01-ABCODE-C3P1-MSG TO WS-01-ABCODE-MSG           G7C2PGM 
00556          PERFORM 9999-000-ABEND-THE-TASK.                         G7C2PGM 
00557                                                                   G7C2PGM 
00558                                                                   G7C2PGM 
00559 *------- COMPUTE MIMIMUM ACCEPTABLE COMMAREA LENGTH -------------*G7C2PGM 
00560                                                                   G7C2PGM 
00561      COMPUTE WS-02-MINIMUM-COMMAREA-LEN = GC-WORKFILE-KEY-LEN     G7C2PGM 
00562                                         + GC-GCBENPRV-FIXED-LEN   G7C2PGM 
00563                                         + GC-GCBENPRV-VARY-LEN.   G7C2PGM 
00564                                                                   G7C2PGM 
00565                                                                   G7C2PGM 
00566 *------- IF NOT MIMIMUM ACCEPTABLE COMMAREA LENGTH, ABEND -------*G7C2PGM 
00567                                                                   G7C2PGM 
00568      IF  EIBCALEN < WS-02-MINIMUM-COMMAREA-LEN                    G7C2PGM 
00569          MOVE WS-01-ABCODE-C3P2     TO WS-01-ABCODE               G7C2PGM 
00570          MOVE WS-01-ABCODE-C3P2-MSG TO WS-01-ABCODE-MSG           G7C2PGM 
00571          PERFORM 9999-000-ABEND-THE-TASK.                         G7C2PGM 
00572                                                                   G7C2PGM 
00573                                                                   G7C2PGM 
00574 *------- BUILD SCREEN FROM W/F BENEFIT PROVISION RECORD PASSED --*G7C2PGM 
00575 *          BY CALLER IN COMMAREA.                                 G7C2PGM 
00576                                                                   G7C2PGM 
00577      MOVE WRK-PLAN-CODE                      TO S2PLNCDO.         G7C2PGM 
00578      MOVE WRK-GROUP-NUM                      TO S2GRPNOO.         G7C2PGM 
00579      MOVE WRK-SECTION-NUM                    TO S2SECNOO.         G7C2PGM 
00580      MOVE WRK-PKG-CODE                       TO S2PKGCDO.         G7C2PGM 
00581      MOVE WRK-PROV-CTL                       TO S2PRVO.           G7C2PGM 
00582      MOVE WRK-FAM-REL-LEVEL                  TO S2FRLO.           G7C2PGM 
00583      MOVE WRK-L-O-B                          TO S2LOBO.           G7C2PGM 
00584                                                                   G7C2PGM 
00585      MOVE WRK-EFF-DATE                       TO HGADATE-JULIAN1.  G7C2PGM 
00586      PERFORM 9810-000-JULIAN-TO-GREGORIAN.                        G7C2PGM 
00587      IF  HGADATE-RETURN = ZEROS                                   G7C2PGM 
00588      THEN                                                         G7C2PGM 
00589          MOVE DFHBMASF                       TO S2EFFDTA          G7C2PGM 
00590          MOVE HGADATE-DATE2                  TO S2EFFDTO          G7C2PGM 
00591      ELSE                                                         G7C2PGM 
00592          MOVE DFHBMABF                       TO S2EFFDTA          G7C2PGM 
00593          MOVE HGADATE-JULIAN1                TO S2EFFDTO.         G7C2PGM 
00594                                                                   G7C2PGM 
00595      MOVE GCP-PROVN-ID                       TO S2BPVIDO.         G7C2PGM 
00596                                                                   G7C2PGM 
00597 * ---- MULT UNRELATED PROCEDURE LEVEL PERCENT 1 - 3               G7C2PGM 
00598 * ---- D129                                                       G7C2PGM 
00599                                                                   G7C2PGM 
00600      MOVE  GPC-MULT-UNRL-PROC-1-PCT     TO                        G7C2PGM 
00601            WS-02-MULT-UNR-PROC-PCT-L1.                            G7C2PGM 
00602      MOVE  WS-02-MULT-UNR-PROC-PCT-L1   TO                        G7C2PGM 
00603            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00604      MOVE  WS-02-DISP-5POS-DEC          TO S2UPLP1O.              G7C2PGM 
00605                                                                   G7C2PGM 
00606      MOVE  GPC-MULT-UNRL-PROC-2-PCT     TO                        G7C2PGM 
00607            WS-02-MULT-UNR-PROC-PCT-L2.                            G7C2PGM 
00608      MOVE  WS-02-MULT-UNR-PROC-PCT-L2   TO                        G7C2PGM 
00609            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00610      MOVE  WS-02-DISP-5POS-DEC          TO S2UPLP2O.              G7C2PGM 
00611                                                                   G7C2PGM 
00612      MOVE  GPC-MULT-UNRL-PROC-3-PCT     TO                        G7C2PGM 
00613            WS-02-MULT-UNR-PROC-PCT-L3.                            G7C2PGM 
00614      MOVE  WS-02-MULT-UNR-PROC-PCT-L3   TO                        G7C2PGM 
00615            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00616      MOVE  WS-02-DISP-5POS-DEC          TO S2UPLP3O.              G7C2PGM 
00617                                                                   G7C2PGM 
00618 * ---- MULT UNRELATED NUMBER OF OCCURRANCES 1 - 3                 G7C2PGM 
00619                                                                   G7C2PGM 
00620      MOVE GPC-MULT-UNRL-1-NO-OCCUR           TO S2UNOC1O.         G7C2PGM 
00621      MOVE GPC-MULT-UNRL-2-NO-OCCUR           TO S2UNOC2O.         G7C2PGM 
00622      MOVE GPC-MULT-UNRL-3-NO-OCCUR           TO S2UNOC3O.         G7C2PGM 
00623                                                                   G7C2PGM 
00624 * ---- MULT RELATED PROCEDURE LEVEL PERCENT 1 - 3                 G7C2PGM 
00625                                                                   G7C2PGM 
00626      MOVE  GPC-MULT-RL-PROC-1-PCT       TO                        G7C2PGM 
00627            WS-02-MULT-REL-PROC-PCT-L1.                            G7C2PGM 
00628      MOVE  WS-02-MULT-REL-PROC-PCT-L1   TO                        G7C2PGM 
00629            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00630      MOVE  WS-02-DISP-5POS-DEC          TO S2RPLP1O.              G7C2PGM 
00631                                                                   G7C2PGM 
00632      MOVE  GPC-MULT-RL-PROC-2-PCT       TO                        G7C2PGM 
00633            WS-02-MULT-REL-PROC-PCT-L2.                            G7C2PGM 
00634      MOVE  WS-02-MULT-REL-PROC-PCT-L2   TO                        G7C2PGM 
00635            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00636      MOVE  WS-02-DISP-5POS-DEC          TO S2RPLP2O.              G7C2PGM 
00637                                                                   G7C2PGM 
00638      MOVE  GPC-MULT-RL-PROC-3-PCT       TO                        G7C2PGM 
00639            WS-02-MULT-REL-PROC-PCT-L3.                            G7C2PGM 
00640      MOVE  WS-02-MULT-REL-PROC-PCT-L3   TO                        G7C2PGM 
00641            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00642      MOVE  WS-02-DISP-5POS-DEC          TO S2RPLP3O.              G7C2PGM 
00643                                                                   G7C2PGM 
00644                                                                   G7C2PGM 
00645 * ---- MULT RELATED NUMBER OF OCCURRANCES 1 - 3                   G7C2PGM 
00646                                                                   G7C2PGM 
00647      MOVE GPC-MULT-RL-1-NO-OCCUR             TO S2RNOC1O.         G7C2PGM 
00648      MOVE GPC-MULT-RL-2-NO-OCCUR             TO S2RNOC2O.         G7C2PGM 
00649      MOVE GPC-MULT-RL-3-NO-OCCUR             TO S2RNOC3O.         G7C2PGM 
00650                                                                   G7C2PGM 
00651 * ---- MULT INJURY PROCEDURE LEVEL PERCENT 1 - 3                  G7C2PGM 
00652                                                                   G7C2PGM 
00653      MOVE  GPC-MULT-INJ-LVL-1-PCT       TO                        G7C2PGM 
00654            WS-02-MULT-INJ-LVL-PCT-L1.                             G7C2PGM 
00655      MOVE  WS-02-MULT-INJ-LVL-PCT-L1    TO                        G7C2PGM 
00656            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00657      MOVE  WS-02-DISP-5POS-DEC          TO S2IPLP1O.              G7C2PGM 
00658                                                                   G7C2PGM 
00659      MOVE  GPC-MULT-INJ-LVL-2-PCT       TO                        G7C2PGM 
00660            WS-02-MULT-INJ-LVL-PCT-L2.                             G7C2PGM 
00661      MOVE  WS-02-MULT-INJ-LVL-PCT-L2    TO                        G7C2PGM 
00662            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00663      MOVE  WS-02-DISP-5POS-DEC          TO S2IPLP2O.              G7C2PGM 
00664                                                                   G7C2PGM 
00665      MOVE  GPC-MULT-INJ-LVL-3-PCT       TO                        G7C2PGM 
00666            WS-02-MULT-INJ-LVL-PCT-L3.                             G7C2PGM 
00667      MOVE  WS-02-MULT-INJ-LVL-PCT-L3    TO                        G7C2PGM 
00668            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00669      MOVE  WS-02-DISP-5POS-DEC          TO S2IPLP3O.              G7C2PGM 
00670                                                                   G7C2PGM 
00671                                                                   G7C2PGM 
00672 * ---- MULT INJURY NUMBER OF OCCURRANCES 1 - 3                    G7C2PGM 
00673                                                                   G7C2PGM 
00674      MOVE GPC-MULT-INJ-1-NO-OCCUR            TO S2INOC1O.         G7C2PGM 
00675      MOVE GPC-MULT-INJ-2-NO-OCCUR            TO S2INOC2O.         G7C2PGM 
00676      MOVE GPC-MULT-INJ-3-NO-OCCUR            TO S2INOC3O.         G7C2PGM 
00677                                                                   G7C2PGM 
00678 * ---- MULT PODIATRY PROCEDURE LEVEL PERCENT 1 - 4                G7C2PGM 
00679                                                                   G7C2PGM 
00680      MOVE  GPC-MULT-POD-PROC-1-PCT      TO                        G7C2PGM 
00681            WS-02-MULT-POD-PROC-PCT-L1.                            G7C2PGM 
00682      MOVE  WS-02-MULT-POD-PROC-PCT-L1   TO                        G7C2PGM 
00683            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00684      MOVE  WS-02-DISP-5POS-DEC          TO S2PPLP1O.              G7C2PGM 
00685                                                                   G7C2PGM 
00686      MOVE  GPC-MULT-POD-PROC-2-PCT      TO                        G7C2PGM 
00687            WS-02-MULT-POD-PROC-PCT-L2.                            G7C2PGM 
00688      MOVE  WS-02-MULT-POD-PROC-PCT-L2   TO                        G7C2PGM 
00689            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00690      MOVE  WS-02-DISP-5POS-DEC          TO S2PPLP2O.              G7C2PGM 
00691                                                                   G7C2PGM 
00692      MOVE  GPC-MULT-POD-PROC-3-PCT      TO                        G7C2PGM 
00693            WS-02-MULT-POD-PROC-PCT-L3.                            G7C2PGM 
00694      MOVE  WS-02-MULT-POD-PROC-PCT-L3   TO                        G7C2PGM 
00695            WS-02-DISP-5POS-DEC.                                   G7C2PGM 
00696      MOVE  WS-02-DISP-5POS-DEC          TO S2PPLP3O.              G7C2PGM 
00697                                                                   G7C2PGM 
00698      MOVE   GPC-MULT-POD-PROC-4-PCT      TO                       G7C2PGM 
00699             WS-02-MULT-POD-PROC-PCT-L4.                           G7C2PGM 
00700      MOVE WS-02-MULT-POD-PROC-PCT-L4     TO                       G7C2PGM 
00701           WS-02-DISP-5POS-DEC.                                    G7C2PGM 
00702      MOVE WS-02-DISP-5POS-DEC            TO S2PPLP4O.             G7C2PGM 
00703                                                                   G7C2PGM 
00704 * ---- MULT PODIATRY NUMBER OF OCCURRANCES 1 - 4                  G7C2PGM 
00705                                                                   G7C2PGM 
00706      MOVE GPC-MULT-POD-1-NO-OCCUR            TO S2PNOC1O.         G7C2PGM 
00707      MOVE GPC-MULT-POD-2-NO-OCCUR            TO S2PNOC2O.         G7C2PGM 
00708      MOVE GPC-MULT-POD-3-NO-OCCUR            TO S2PNOC3O.         G7C2PGM 
00709      MOVE GPC-MULT-POD-4-NO-OCCUR            TO S2PNOC4O.         G7C2PGM 
00710                                                                   G7C2PGM 
00711 * --- MULTIPLE PODIATRY PROCEDURE PRICING INDICATOR               G7C2PGM 
00712                                                                   G7C2PGM 
00713      MOVE GPC-MULT-POD-PROC-PRICE-IND        TO S2MPPPIO.         G7C2PGM 
00714                                                                   G7C2PGM 
00715 * --- PRIMARY SURGEON DEPENDENCY PAYMENT PERCENT                  G7C2PGM 
00716                                                                   G7C2PGM 
00717      MOVE GPC-PRIM-SURG-DPD-PAY-PCT         TO                    G7C2PGM 
00718           WS-02-PRI-SUR-DEP-PAY-PCT.                              G7C2PGM 
00719      MOVE WS-02-PRI-SUR-DEP-PAY-PCT         TO                    G7C2PGM 
00720           WS-02-DISP-3POS-DEC.                                    G7C2PGM 
00721      MOVE WS-02-DISP-3POS-DEC               TO  S2PSDPPO.         G7C2PGM 
00722                                                                   G7C2PGM 
00723 *------- SEND INITIAL SCREEN ------------------------------------*G7C2PGM 
00724                                                                   G7C2PGM 
00725      MOVE  -1 TO  S2UPLP1L.                                       G7C2PGM 
00726      PERFORM 9100-000-SEND-THEN-RETURN.                           G7C2PGM 
00727                                                                   G7C2PGM 
00728                                                                   G7C2PGM 
00729  1000-900-EXIT.                                                   G7C2PGM 
00730      EXIT.                                                        G7C2PGM 
00731 /***************************************************************  G7C2PGM 
00732 *                                                              *  G7C2PGM 
00733 * 2000    P R O C E S S    I N P U T                           *  G7C2PGM 
00734 *                                                              *  G7C2PGM 
00735 ****************************************************************  G7C2PGM 
00736  2000-000-PROCESS-INPUT         SECTION.                          G7C2PGM 
00737  2000-010.                                                        G7C2PGM 
00738                                                                   G7C2PGM 
00739 *------ VALIDATE PFKEY USAGE ------------------------------------*G7C2PGM 
00740                                                                   G7C2PGM 
00741      IF  EIBAID = DFHENTER OR                                     G7C2PGM 
00742                   DFHPF3   OR  DFHPF15 OR                         G7C2PGM 
00743                   DFHPF4   OR  DFHPF16 OR                         G7C2PGM 
00744                   DFHPF6   OR  DFHPF18 OR                         G7C2PGM 
00745                   DFHPF7   OR  DFHPF19 OR                         G7C2PGM 
00746                   DFHPF8   OR  DFHPF20                            G7C2PGM 
00747      THEN                                                         G7C2PGM 
00748          NEXT SENTENCE                                            G7C2PGM 
00749      ELSE                                                         G7C2PGM 
00750          SET WT-01-INDEX TO +01                                   G7C2PGM 
00751          PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7C2PGM 
00752          PERFORM 9100-000-SEND-THEN-RETURN.                       G7C2PGM 
00753                                                                   G7C2PGM 
00754                                                                   G7C2PGM 
00755                                                                   G7C2PGM 
00756      EXEC CICS  HANDLE CONDITION                                  G7C2PGM 
00757                        MAPFAIL(9200-000-XCTL-TO-GCPSPGM)          G7C2PGM 
00758                        END-EXEC.                                  G7C2PGM 
00759                                                                   G7C2PGM 
00760                                                                   G7C2PGM 
00761      EXEC CICS  RECEIVE MAP   ('G7C2I01')                         G7C2PGM 
00762                         MAPSET('G7C2SET')                         G7C2PGM 
00763                         END-EXEC.                                 G7C2PGM 
00764                                                                   G7C2PGM 
00765                                                                   G7C2PGM 
00766      IF  S2FUNCI  NOT = 'G7C2'  OR                                G7C2PGM 
00767          S2SCRNI  NOT = '007C02'                                  G7C2PGM 
00768          PERFORM 9200-000-XCTL-TO-GCPSPGM.                        G7C2PGM 
00769                                                                   G7C2PGM 
00770                                                                   G7C2PGM 
00771 *--- RETURN TO GCPS MENU? ---------------------------------------*G7C2PGM 
00772                                                                   G7C2PGM 
00773      IF  EIBAID  =  DFHPF3  OR DFHPF15                            G7C2PGM 
00774          PERFORM 9210-000-XCTL-TO-PREVIOUS-MENU.                  G7C2PGM 
00775                                                                   G7C2PGM 
00776 *--- PROCESS SCREEN FIELDS --------------------------------------*G7C2PGM 
00777                                                                   G7C2PGM 
00778      PERFORM 2100-000-FIELD-EDITS THRU 2100-900-EXIT.             G7C2PGM 
00779                                                                   G7C2PGM 
00780      IF  WS-02-SCREEN-HAS-ERRORS                                  G7C2PGM 
00781          PERFORM 9100-000-SEND-THEN-RETURN.                       G7C2PGM 
00782                                                                   G7C2PGM 
00783      PERFORM 2200-000-LOGICAL-EDITS.                              G7C2PGM 
00784                                                                   G7C2PGM 
00785      IF  WS-02-SCREEN-HAS-ERRORS                                  G7C2PGM 
00786          PERFORM 9100-000-SEND-THEN-RETURN.                       G7C2PGM 
00787                                                                   G7C2PGM 
00788      PERFORM 2300-000-APPLY-RECORD-CHANGES  THRU                  G7C2PGM 
00789              2300-900-EXIT.                                       G7C2PGM 
00790                                                                   G7C2PGM 
00791      PERFORM 2400-000-XCTL-TO-NEXT-PGM.                           G7C2PGM 
00792                                                                   G7C2PGM 
00793                                                                   G7C2PGM 
00794  2000-900-EXIT.                                                   G7C2PGM 
00795      EXIT.                                                        G7C2PGM 
00796 /***************************************************************  G7C2PGM 
00797 *                                                              *  G7C2PGM 
00798 * 2100  DO SCREEN FIELD EDITS                                  *  G7C2PGM 
00799 *                                                              *  G7C2PGM 
00800 ****************************************************************  G7C2PGM 
00801  2100-000-FIELD-EDITS.                                            G7C2PGM 
00802  2100-010.                                                        G7C2PGM 
00803                                                                   G7C2PGM 
00804 *--------------- SET FIELD ATTRIBUTES (UNPROT, FSET, NORMAL) ----*G7C2PGM 
00805                                                                   G7C2PGM 
00806      MOVE DFHBMUNF TO  S2MPPPIA   S2PSDPPA                        G7C2PGM 
00807           S2UPLP1A     S2UPLP2A   S2UPLP3A                        G7C2PGM 
00808           S2UNOC1A     S2UNOC2A   S2UNOC3A                        G7C2PGM 
00809 *                                                                 G7C2PGM 
00810           S2RPLP1A     S2RPLP2A   S2RPLP3A                        G7C2PGM 
00811           S2RNOC1A     S2RNOC2A   S2RNOC3A                        G7C2PGM 
00812 *                                                                 G7C2PGM 
00813           S2IPLP1A     S2IPLP2A   S2IPLP3A                        G7C2PGM 
00814           S2INOC1A     S2INOC2A   S2INOC3A                        G7C2PGM 
00815 *                                                                 G7C2PGM 
00816           S2PPLP1A     S2PPLP2A   S2PPLP3A    S2PPLP4A            G7C2PGM 
00817           S2PNOC1A     S2PNOC2A   S2PNOC3A    S2PNOC4A.           G7C2PGM 
00818                                                                   G7C2PGM 
00819                                                                   G7C2PGM 
00820      MOVE ZEROS            TO WS-02-GCVI-RETURN-CODE.             G7C2PGM 
00821                                                                   G7C2PGM 
00822  2100-011.                                                        G7C2PGM 
00823 /*****************************************************************G7C2PGM 
00824 *                                                                *G7C2PGM 
00825 *-- VALIDATE ------ MULT UNRELATED PROCEDURE LEVEL PCT # 1  -----*G7C2PGM 
00826 *                                                                *G7C2PGM 
00827 ******************************************************************G7C2PGM 
00828 *   D129                                                          G7C2PGM 
00829                                                                   G7C2PGM 
00830      MOVE S2UPLP1I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
00831      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
00832      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
00833      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
00834      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
00835      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
00836          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
00837              MOVE -1       TO S2UPLP1L                            G7C2PGM 
00838              MOVE DFHBMUBF TO S2UPLP1A                            G7C2PGM 
00839              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
00840                  NEXT SENTENCE                                    G7C2PGM 
00841              ELSE                                                 G7C2PGM 
00842                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
00843                  SET WT-01-INDEX TO +12                           G7C2PGM 
00844                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
00845          ELSE                                                     G7C2PGM 
00846              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
00847                TO WS-02-MULT-UNR-PROC-PCT-L1                      G7C2PGM 
00848              MOVE WS-02-MULT-UNR-PROC-PCT-L1                      G7C2PGM 
00849                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
00850              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
00851                TO S2UPLP1O                                        G7C2PGM 
00852      ELSE                                                         G7C2PGM 
00853          MOVE -1       TO S2UPLP1L                                G7C2PGM 
00854          MOVE DFHBMUBF TO S2UPLP1A                                G7C2PGM 
00855          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
00856              NEXT SENTENCE                                        G7C2PGM 
00857          ELSE                                                     G7C2PGM 
00858              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
00859              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
00860                  SET WT-01-INDEX TO +10                           G7C2PGM 
00861                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
00862              ELSE                                                 G7C2PGM 
00863                  SET WT-01-INDEX TO +11                           G7C2PGM 
00864                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
00865                                                                   G7C2PGM 
00866                                                                   G7C2PGM 
00867 *    MOVE S2UPLP1I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
00868 *                                                                 G7C2PGM 
00869 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
00870 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
00871 *                                                                 G7C2PGM 
00872 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
00873 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
00874 *         WS-02-MULT-PCT-C4 EQUAL '.' )                           G7C2PGM 
00875 *    THEN                                                         G7C2PGM 
00876 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
00877 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
00878 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
00879 *                 WS-02-MULT-UNR-PROC-PCT-L1                      G7C2PGM 
00880 *    ELSE                                                         G7C2PGM 
00881 *        MOVE  DFHBMUBF  TO  S2UPLP1A                             G7C2PGM 
00882 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
00883 *        THEN                                                     G7C2PGM 
00884 *            NEXT SENTENCE                                        G7C2PGM 
00885 *        ELSE                                                     G7C2PGM 
00886 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
00887 *               MOVE  -1        TO  S2UPLP1L                      G7C2PGM 
00888 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
00889 *               SET WT-01-INDEX TO +10                            G7C2PGM 
00890 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
00891 *            ELSE                                                 G7C2PGM 
00892 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
00893 *               MOVE  -1        TO  S2UPLP1L                      G7C2PGM 
00894 *               SET WT-01-INDEX TO +05                            G7C2PGM 
00895 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
00896                                                                   G7C2PGM 
00897  2100-012.                                                        G7C2PGM 
00898 *-- VALIDATE ------ MULT UNRELATED LEVEL 1 NO. OF OCCURRENCES ---*G7C2PGM 
00899 *   1. ALPHANUMERIC                                               G7C2PGM 
00900 *   2. FIELD VALIDATION                                           G7C2PGM 
00901                                                                   G7C2PGM 
00902      MOVE  S2UNOC1I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
00903      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
00904      THEN                                                         G7C2PGM 
00905          MOVE  S2UNOC1I TO GCVI-VALUE                             G7C2PGM 
00906          MOVE  'BPCB12' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
00907          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
00908          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
00909          THEN                                                     G7C2PGM 
00910              MOVE  DFHBMUBF  TO  S2UNOC1A                         G7C2PGM 
00911              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
00912              THEN                                                 G7C2PGM 
00913                  NEXT SENTENCE                                    G7C2PGM 
00914              ELSE                                                 G7C2PGM 
00915                  MOVE  -1        TO  S2UNOC1L                     G7C2PGM 
00916                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
00917                  SET WT-01-INDEX TO +09                           G7C2PGM 
00918                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
00919          ELSE                                                     G7C2PGM 
00920              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
00921              THEN                                                 G7C2PGM 
00922                  MOVE  DFHBMUBF  TO  S2UNOC1A                     G7C2PGM 
00923              ELSE                                                 G7C2PGM 
00924                  NEXT SENTENCE                                    G7C2PGM 
00925      ELSE                                                         G7C2PGM 
00926          MOVE  -1        TO  S2UNOC1L                             G7C2PGM 
00927          MOVE  DFHBMUBF  TO  S2UNOC1A                             G7C2PGM 
00928          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
00929          THEN                                                     G7C2PGM 
00930              NEXT SENTENCE                                        G7C2PGM 
00931          ELSE                                                     G7C2PGM 
00932              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
00933              SET WT-01-INDEX TO +08                               G7C2PGM 
00934              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
00935                                                                   G7C2PGM 
00936                                                                   G7C2PGM 
00937  2100-013.                                                        G7C2PGM 
00938 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
00939 *                                                                 G7C2PGM 
00940 *   IF LEVEL PERCENT IS CODEED > 0 THE OCCURRANCES MUST BE > 0.   G7C2PGM 
00941 *   IF LEVEL PERCENT IS CODEED = 0 THE OCCURRANCES MUST BE = 0.   G7C2PGM 
00942                                                                   G7C2PGM 
00943      IF ((WS-02-MULT-UNR-PROC-PCT-L1 NOT = ZEROS AND              G7C2PGM 
00944          S2UNOC1I NOT = ZERO )    OR                              G7C2PGM 
00945          (WS-02-MULT-UNR-PROC-PCT-L1 = ZEROS AND                  G7C2PGM 
00946          S2UNOC1I = ZERO ))                                       G7C2PGM 
00947          NEXT SENTENCE                                            G7C2PGM 
00948      ELSE                                                         G7C2PGM 
00949          MOVE  DFHBMUBF  TO  S2UNOC1A                             G7C2PGM 
00950                              S2UPLP1A                             G7C2PGM 
00951          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
00952          THEN                                                     G7C2PGM 
00953              NEXT SENTENCE                                        G7C2PGM 
00954          ELSE                                                     G7C2PGM 
00955              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
00956              MOVE  -1        TO  S2UPLP1L                         G7C2PGM 
00957              SET WT-01-INDEX TO +02                               G7C2PGM 
00958              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
00959                                                                   G7C2PGM 
00960 /*****************************************************************G7C2PGM 
00961 *                                                                *G7C2PGM 
00962 *-- VALIDATE ------ MULT UNRELATED PROCEDURE LEVEL PCT # 2  -----*G7C2PGM 
00963 *                                                                *G7C2PGM 
00964 ******************************************************************G7C2PGM 
00965 *   D129                                                          G7C2PGM 
00966                                                                   G7C2PGM 
00967      MOVE S2UPLP2I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
00968      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
00969      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
00970      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
00971      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
00972      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
00973          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
00974              MOVE -1       TO S2UPLP2L                            G7C2PGM 
00975              MOVE DFHBMUBF TO S2UPLP2A                            G7C2PGM 
00976              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
00977                  NEXT SENTENCE                                    G7C2PGM 
00978              ELSE                                                 G7C2PGM 
00979                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
00980                  SET WT-01-INDEX TO +12                           G7C2PGM 
00981                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
00982          ELSE                                                     G7C2PGM 
00983              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
00984                TO WS-02-MULT-UNR-PROC-PCT-L2                      G7C2PGM 
00985              MOVE WS-02-MULT-UNR-PROC-PCT-L2                      G7C2PGM 
00986                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
00987              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
00988                TO S2UPLP2O                                        G7C2PGM 
00989      ELSE                                                         G7C2PGM 
00990          MOVE -1       TO S2UPLP2L                                G7C2PGM 
00991          MOVE DFHBMUBF TO S2UPLP2A                                G7C2PGM 
00992          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
00993              NEXT SENTENCE                                        G7C2PGM 
00994          ELSE                                                     G7C2PGM 
00995              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
00996              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
00997                  SET WT-01-INDEX TO +10                           G7C2PGM 
00998                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
00999              ELSE                                                 G7C2PGM 
01000                  SET WT-01-INDEX TO +11                           G7C2PGM 
01001                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01002                                                                   G7C2PGM 
01003                                                                   G7C2PGM 
01004 *    MOVE S2UPLP2I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01005                                                                   G7C2PGM 
01006 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01007 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01008 *                                                                 G7C2PGM 
01009 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01010 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01011 *         WS-02-MULT-PCT-C4 EQUAL '.' )                           G7C2PGM 
01012 *    THEN                                                         G7C2PGM 
01013 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01014 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01015 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01016 *                 WS-02-MULT-UNR-PROC-PCT-L2                      G7C2PGM 
01017 *    ELSE                                                         G7C2PGM 
01018 *        MOVE  DFHBMUBF  TO  S2UPLP2A                             G7C2PGM 
01019 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01020 *        THEN                                                     G7C2PGM 
01021 *            NEXT SENTENCE                                        G7C2PGM 
01022 *        ELSE                                                     G7C2PGM 
01023 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01024 *               MOVE  -1        TO  S2UPLP2L                      G7C2PGM 
01025 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01026 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01027 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01028 *            ELSE                                                 G7C2PGM 
01029 *               MOVE  -1        TO  S2UPLP2L                      G7C2PGM 
01030 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01031 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01032 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01033                                                                   G7C2PGM 
01034 *-- VALIDATE ------ MULT UNRELATED LEVEL 2 NO. OF OCCURRENCES ---*G7C2PGM 
01035 *   1. ALPHANUMERIC                                               G7C2PGM 
01036 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01037                                                                   G7C2PGM 
01038      MOVE  S2UNOC2I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01039      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01040      THEN                                                         G7C2PGM 
01041          MOVE  S2UNOC2I TO GCVI-VALUE                             G7C2PGM 
01042          MOVE  'BPCB12' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01043          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01044          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01045          THEN                                                     G7C2PGM 
01046              MOVE  DFHBMUBF  TO  S2UNOC2A                         G7C2PGM 
01047              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01048              THEN                                                 G7C2PGM 
01049                  NEXT SENTENCE                                    G7C2PGM 
01050              ELSE                                                 G7C2PGM 
01051                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01052                  MOVE  -1        TO  S2UNOC2L                     G7C2PGM 
01053                  SET WT-01-INDEX TO +09                           G7C2PGM 
01054                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01055          ELSE                                                     G7C2PGM 
01056              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01057              THEN                                                 G7C2PGM 
01058                  MOVE  DFHBMUBF  TO  S2UNOC2A                     G7C2PGM 
01059              ELSE                                                 G7C2PGM 
01060                  NEXT SENTENCE                                    G7C2PGM 
01061      ELSE                                                         G7C2PGM 
01062          MOVE  DFHBMUBF  TO  S2UNOC2A                             G7C2PGM 
01063          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01064          THEN                                                     G7C2PGM 
01065              NEXT SENTENCE                                        G7C2PGM 
01066          ELSE                                                     G7C2PGM 
01067              MOVE  -1        TO  S2UNOC2L                         G7C2PGM 
01068              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01069              SET WT-01-INDEX TO +08                               G7C2PGM 
01070              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01071                                                                   G7C2PGM 
01072                                                                   G7C2PGM 
01073 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01074 *                                                                 G7C2PGM 
01075 *   IF LEVEL PERCENT IS CODEED > 0 THE OCCURRANCES MUST BE > 0.   G7C2PGM 
01076 *   IF LEVEL PERCENT IS CODEED = 0 THE OCCURRANCES MUST BE = 0.   G7C2PGM 
01077                                                                   G7C2PGM 
01078      IF ((WS-02-MULT-UNR-PROC-PCT-L2 NOT = ZEROS   AND            G7C2PGM 
01079           S2UNOC2I NOT = ZERO)     OR                             G7C2PGM 
01080          (WS-02-MULT-UNR-PROC-PCT-L2    = ZEROS   AND             G7C2PGM 
01081           S2UNOC2I = ZERO ))                                      G7C2PGM 
01082      THEN                                                         G7C2PGM 
01083          NEXT SENTENCE                                            G7C2PGM 
01084      ELSE                                                         G7C2PGM 
01085          MOVE  DFHBMUBF  TO  S2UNOC2A                             G7C2PGM 
01086                              S2UPLP2A                             G7C2PGM 
01087          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01088          THEN                                                     G7C2PGM 
01089              NEXT SENTENCE                                        G7C2PGM 
01090          ELSE                                                     G7C2PGM 
01091              MOVE  -1        TO  S2UPLP2L                         G7C2PGM 
01092              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01093              SET WT-01-INDEX TO +02                               G7C2PGM 
01094              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01095                                                                   G7C2PGM 
01096 /*****************************************************************G7C2PGM 
01097 *                                                                *G7C2PGM 
01098 *-- VALIDATE ------ MULT UNRELATED PROCEDURE LEVEL PCT # 3  -----*G7C2PGM 
01099 *                                                                *G7C2PGM 
01100 ******************************************************************G7C2PGM 
01101 *   D129                                                          G7C2PGM 
01102                                                                   G7C2PGM 
01103      MOVE S2UPLP3I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01104      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01105      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01106      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01107      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01108      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01109          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01110              MOVE -1       TO S2UPLP3L                            G7C2PGM 
01111              MOVE DFHBMUBF TO S2UPLP3A                            G7C2PGM 
01112              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01113                  NEXT SENTENCE                                    G7C2PGM 
01114              ELSE                                                 G7C2PGM 
01115                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01116                  SET WT-01-INDEX TO +12                           G7C2PGM 
01117                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01118          ELSE                                                     G7C2PGM 
01119              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01120                TO WS-02-MULT-UNR-PROC-PCT-L3                      G7C2PGM 
01121              MOVE WS-02-MULT-UNR-PROC-PCT-L3                      G7C2PGM 
01122                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01123              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01124                TO S2UPLP3O                                        G7C2PGM 
01125      ELSE                                                         G7C2PGM 
01126          MOVE -1       TO S2UPLP3L                                G7C2PGM 
01127          MOVE DFHBMUBF TO S2UPLP3A                                G7C2PGM 
01128          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01129              NEXT SENTENCE                                        G7C2PGM 
01130          ELSE                                                     G7C2PGM 
01131              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01132              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01133                  SET WT-01-INDEX TO +10                           G7C2PGM 
01134                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01135              ELSE                                                 G7C2PGM 
01136                  SET WT-01-INDEX TO +11                           G7C2PGM 
01137                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01138                                                                   G7C2PGM 
01139                                                                   G7C2PGM 
01140 *    MOVE S2UPLP3I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01141 *                                                                 G7C2PGM 
01142 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01143 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01144 *                                                                 G7C2PGM 
01145 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01146 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01147 *         WS-02-MULT-PCT-C4 EQUAL '.' )                           G7C2PGM 
01148 *    THEN                                                         G7C2PGM 
01149 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01150 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01151 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01152 *                 WS-02-MULT-UNR-PROC-PCT-L3                      G7C2PGM 
01153 *    ELSE                                                         G7C2PGM 
01154 *        MOVE  DFHBMUBF  TO  S2UPLP3A                             G7C2PGM 
01155 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01156 *        THEN                                                     G7C2PGM 
01157 *            NEXT SENTENCE                                        G7C2PGM 
01158 *        ELSE                                                     G7C2PGM 
01159 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01160 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01161 *               MOVE  -1        TO  S2UPLP3L                      G7C2PGM 
01162 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01163 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01164 *            ELSE                                                 G7C2PGM 
01165 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01166 *               MOVE  -1        TO  S2UPLP3L                      G7C2PGM 
01167 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01168 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01169                                                                   G7C2PGM 
01170                                                                   G7C2PGM 
01171 *-- VALIDATE ------ MULT UNRELATED LEVEL 3 NO. OF OCCURRENCES ---*G7C2PGM 
01172 *   1. ALPHANUMERIC                                               G7C2PGM 
01173 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01174                                                                   G7C2PGM 
01175      MOVE  S2UNOC3I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01176      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01177      THEN                                                         G7C2PGM 
01178          MOVE  S2UNOC3I TO GCVI-VALUE                             G7C2PGM 
01179          MOVE  'BPCB12' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01180          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01181          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01182          THEN                                                     G7C2PGM 
01183              MOVE  DFHBMUBF  TO  S2UNOC3A                         G7C2PGM 
01184              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01185              THEN                                                 G7C2PGM 
01186                  NEXT SENTENCE                                    G7C2PGM 
01187              ELSE                                                 G7C2PGM 
01188                  MOVE  -1        TO  S2UNOC3L                     G7C2PGM 
01189                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01190                  SET WT-01-INDEX TO +09                           G7C2PGM 
01191                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01192          ELSE                                                     G7C2PGM 
01193              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01194              THEN                                                 G7C2PGM 
01195                  MOVE  DFHBMUBF  TO  S2UNOC3A                     G7C2PGM 
01196              ELSE                                                 G7C2PGM 
01197                  NEXT SENTENCE                                    G7C2PGM 
01198      ELSE                                                         G7C2PGM 
01199          MOVE  DFHBMUBF  TO  S2UNOC3A                             G7C2PGM 
01200          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01201          THEN                                                     G7C2PGM 
01202              NEXT SENTENCE                                        G7C2PGM 
01203          ELSE                                                     G7C2PGM 
01204              MOVE  -1        TO  S2UNOC3L                         G7C2PGM 
01205              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01206              SET WT-01-INDEX TO +08                               G7C2PGM 
01207              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01208                                                                   G7C2PGM 
01209                                                                   G7C2PGM 
01210 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01211 *                                                                 G7C2PGM 
01212 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
01213 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
01214                                                                   G7C2PGM 
01215      IF ((WS-02-MULT-UNR-PROC-PCT-L3  NOT = ZEROS    AND          G7C2PGM 
01216           S2UNOC3I NOT = ZERO) OR                                 G7C2PGM 
01217          (WS-02-MULT-UNR-PROC-PCT-L3  =  ZEROS    AND             G7C2PGM 
01218           S2UNOC3I = ZERO))                                       G7C2PGM 
01219      THEN                                                         G7C2PGM 
01220          NEXT SENTENCE                                            G7C2PGM 
01221      ELSE                                                         G7C2PGM 
01222          MOVE  DFHBMUBF  TO  S2UNOC3A                             G7C2PGM 
01223                              S2UPLP3A                             G7C2PGM 
01224          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01225          THEN                                                     G7C2PGM 
01226              NEXT SENTENCE                                        G7C2PGM 
01227          ELSE                                                     G7C2PGM 
01228              MOVE  -1        TO  S2UPLP3L                         G7C2PGM 
01229              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01230              SET WT-01-INDEX TO +02                               G7C2PGM 
01231              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01232                                                                   G7C2PGM 
01233                                                                   G7C2PGM 
01234 /*****************************************************************G7C2PGM 
01235 *                                                                *G7C2PGM 
01236 *-- VALIDATE ------ MULT RELATED PROCEDURE LEVEL PCT # 1    -----*G7C2PGM 
01237 *                                                                *G7C2PGM 
01238 ******************************************************************G7C2PGM 
01239 *   D129                                                          G7C2PGM 
01240                                                                   G7C2PGM 
01241      MOVE S2RPLP1I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01242      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01243      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01244      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01245      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01246      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01247          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01248              MOVE -1       TO S2RPLP1L                            G7C2PGM 
01249              MOVE DFHBMUBF TO S2RPLP1A                            G7C2PGM 
01250              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01251                  NEXT SENTENCE                                    G7C2PGM 
01252              ELSE                                                 G7C2PGM 
01253                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01254                  SET WT-01-INDEX TO +12                           G7C2PGM 
01255                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01256          ELSE                                                     G7C2PGM 
01257              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01258                TO WS-02-MULT-REL-PROC-PCT-L1                      G7C2PGM 
01259              MOVE WS-02-MULT-REL-PROC-PCT-L1                      G7C2PGM 
01260                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01261              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01262                TO S2RPLP1O                                        G7C2PGM 
01263      ELSE                                                         G7C2PGM 
01264          MOVE -1       TO S2RPLP1L                                G7C2PGM 
01265          MOVE DFHBMUBF TO S2RPLP1A                                G7C2PGM 
01266          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01267              NEXT SENTENCE                                        G7C2PGM 
01268          ELSE                                                     G7C2PGM 
01269              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01270              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01271                  SET WT-01-INDEX TO +10                           G7C2PGM 
01272                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01273              ELSE                                                 G7C2PGM 
01274                  SET WT-01-INDEX TO +11                           G7C2PGM 
01275                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01276                                                                   G7C2PGM 
01277                                                                   G7C2PGM 
01278 *    MOVE S2RPLP1I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01279 *                                                                 G7C2PGM 
01280 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01281 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01282 *                                                                 G7C2PGM 
01283 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01284 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01285 *         WS-02-MULT-PCT-C4 EQUAL '.' )                           G7C2PGM 
01286 *    THEN                                                         G7C2PGM 
01287 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01288 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01289 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01290 *                 WS-02-MULT-REL-PROC-PCT-L1                      G7C2PGM 
01291 *    ELSE                                                         G7C2PGM 
01292 *        MOVE  DFHBMUBF  TO  S2RPLP1A                             G7C2PGM 
01293 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01294 *        THEN                                                     G7C2PGM 
01295 *            NEXT SENTENCE                                        G7C2PGM 
01296 *        ELSE                                                     G7C2PGM 
01297 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01298 *               MOVE  -1        TO  S2RPLP1L                      G7C2PGM 
01299 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01300 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01301 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01302 *            ELSE                                                 G7C2PGM 
01303 *               MOVE  -1        TO  S2RPLP1L                      G7C2PGM 
01304 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01305 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01306 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01307                                                                   G7C2PGM 
01308 *-- VALIDATE ------ MULT RELATED LEVEL 1 NO. OF OCCURRENCES ---*  G7C2PGM 
01309 *   1. ALPHANUMERIC                                               G7C2PGM 
01310 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01311                                                                   G7C2PGM 
01312      MOVE  S2RNOC1I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01313      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01314      THEN                                                         G7C2PGM 
01315          MOVE  S2RNOC1I TO GCVI-VALUE                             G7C2PGM 
01316          MOVE  'BPCB13' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01317          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01318          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01319          THEN                                                     G7C2PGM 
01320              MOVE  DFHBMUBF  TO  S2RNOC1A                         G7C2PGM 
01321              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01322              THEN                                                 G7C2PGM 
01323                  NEXT SENTENCE                                    G7C2PGM 
01324              ELSE                                                 G7C2PGM 
01325                  MOVE  -1        TO  S2RNOC1L                     G7C2PGM 
01326                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01327                  SET WT-01-INDEX TO +09                           G7C2PGM 
01328                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01329          ELSE                                                     G7C2PGM 
01330              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01331              THEN                                                 G7C2PGM 
01332                  MOVE  DFHBMUBF  TO  S2RNOC1A                     G7C2PGM 
01333              ELSE                                                 G7C2PGM 
01334                  NEXT SENTENCE                                    G7C2PGM 
01335      ELSE                                                         G7C2PGM 
01336          MOVE  DFHBMUBF  TO  S2RNOC1A                             G7C2PGM 
01337          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01338          THEN                                                     G7C2PGM 
01339              NEXT SENTENCE                                        G7C2PGM 
01340          ELSE                                                     G7C2PGM 
01341              MOVE  -1        TO  S2RNOC1L                         G7C2PGM 
01342              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01343              SET WT-01-INDEX TO +08                               G7C2PGM 
01344              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01345                                                                   G7C2PGM 
01346                                                                   G7C2PGM 
01347 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01348 *                                                                 G7C2PGM 
01349 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
01350 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
01351                                                                   G7C2PGM 
01352      IF  ((WS-02-MULT-REL-PROC-PCT-L1 NOT = ZEROS    AND          G7C2PGM 
01353            S2RNOC1I NOT = ZERO)                       OR          G7C2PGM 
01354           (WS-02-MULT-REL-PROC-PCT-L1   =   ZEROS    AND          G7C2PGM 
01355            S2RNOC1I = ZERO))                                      G7C2PGM 
01356      THEN                                                         G7C2PGM 
01357          NEXT SENTENCE                                            G7C2PGM 
01358      ELSE                                                         G7C2PGM 
01359          MOVE  DFHBMUBF  TO  S2RNOC1A                             G7C2PGM 
01360                              S2RPLP1A                             G7C2PGM 
01361          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01362          THEN                                                     G7C2PGM 
01363              NEXT SENTENCE                                        G7C2PGM 
01364          ELSE                                                     G7C2PGM 
01365              MOVE  -1        TO  S2RPLP1L                         G7C2PGM 
01366              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01367              SET WT-01-INDEX TO +02                               G7C2PGM 
01368              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01369                                                                   G7C2PGM 
01370 /*****************************************************************G7C2PGM 
01371 *                                                                *G7C2PGM 
01372 *-- VALIDATE ------ MULT   RELATED PROCEDURE LEVEL PCT # 2  -----*G7C2PGM 
01373 *                                                                *G7C2PGM 
01374 ******************************************************************G7C2PGM 
01375 *   D129                                                          G7C2PGM 
01376                                                                   G7C2PGM 
01377      MOVE S2RPLP2I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01378      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01379      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01380      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01381      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01382      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01383          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01384              MOVE -1       TO S2RPLP2L                            G7C2PGM 
01385              MOVE DFHBMUBF TO S2RPLP2A                            G7C2PGM 
01386              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01387                  NEXT SENTENCE                                    G7C2PGM 
01388              ELSE                                                 G7C2PGM 
01389                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01390                  SET WT-01-INDEX TO +12                           G7C2PGM 
01391                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01392          ELSE                                                     G7C2PGM 
01393              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01394                TO WS-02-MULT-REL-PROC-PCT-L2                      G7C2PGM 
01395              MOVE WS-02-MULT-REL-PROC-PCT-L2                      G7C2PGM 
01396                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01397              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01398                TO S2RPLP2O                                        G7C2PGM 
01399      ELSE                                                         G7C2PGM 
01400          MOVE -1       TO S2RPLP2L                                G7C2PGM 
01401          MOVE DFHBMUBF TO S2RPLP2A                                G7C2PGM 
01402          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01403              NEXT SENTENCE                                        G7C2PGM 
01404          ELSE                                                     G7C2PGM 
01405              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01406              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01407                  SET WT-01-INDEX TO +10                           G7C2PGM 
01408                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01409              ELSE                                                 G7C2PGM 
01410                  SET WT-01-INDEX TO +11                           G7C2PGM 
01411                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01412                                                                   G7C2PGM 
01413                                                                   G7C2PGM 
01414 *    MOVE S2RPLP2I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01415 *                                                                 G7C2PGM 
01416 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01417 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01418 *                                                                 G7C2PGM 
01419 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01420 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01421 *         WS-02-MULT-PCT-C4 EQUAL '.' )                           G7C2PGM 
01422 *    THEN                                                         G7C2PGM 
01423 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01424 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01425 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01426 *                 WS-02-MULT-REL-PROC-PCT-L2                      G7C2PGM 
01427 *    ELSE                                                         G7C2PGM 
01428 *        MOVE  DFHBMUBF  TO  S2RPLP2A                             G7C2PGM 
01429 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01430 *        THEN                                                     G7C2PGM 
01431 *            NEXT SENTENCE                                        G7C2PGM 
01432 *        ELSE                                                     G7C2PGM 
01433 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01434 *               MOVE  -1        TO  S2RPLP2L                      G7C2PGM 
01435 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01436 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01437 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01438 *            ELSE                                                 G7C2PGM 
01439 *               MOVE  -1        TO  S2RPLP2L                      G7C2PGM 
01440 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01441 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01442 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01443                                                                   G7C2PGM 
01444 *-- VALIDATE ------ MULT   RELATED LEVEL 2 NO. OF OCCURRENCES ---*G7C2PGM 
01445 *   1. ALPHANUMERIC                                               G7C2PGM 
01446 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01447                                                                   G7C2PGM 
01448      MOVE  S2RNOC2I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01449      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01450      THEN                                                         G7C2PGM 
01451          MOVE  S2RNOC2I TO GCVI-VALUE                             G7C2PGM 
01452          MOVE  'BPCB13' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01453          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01454          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01455          THEN                                                     G7C2PGM 
01456              MOVE  DFHBMUBF  TO  S2RNOC2A                         G7C2PGM 
01457              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01458              THEN                                                 G7C2PGM 
01459                  NEXT SENTENCE                                    G7C2PGM 
01460              ELSE                                                 G7C2PGM 
01461                  MOVE  -1        TO  S2RNOC2L                     G7C2PGM 
01462                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01463                  SET WT-01-INDEX TO +09                           G7C2PGM 
01464                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01465          ELSE                                                     G7C2PGM 
01466              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01467              THEN                                                 G7C2PGM 
01468                  MOVE  DFHBMUBF  TO  S2RNOC2A                     G7C2PGM 
01469              ELSE                                                 G7C2PGM 
01470                  NEXT SENTENCE                                    G7C2PGM 
01471      ELSE                                                         G7C2PGM 
01472          MOVE  DFHBMUBF  TO  S2RNOC2A                             G7C2PGM 
01473          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01474          THEN                                                     G7C2PGM 
01475              NEXT SENTENCE                                        G7C2PGM 
01476          ELSE                                                     G7C2PGM 
01477              MOVE  -1        TO  S2RNOC2L                         G7C2PGM 
01478              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01479              SET WT-01-INDEX TO +08                               G7C2PGM 
01480              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01481                                                                   G7C2PGM 
01482                                                                   G7C2PGM 
01483 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01484 *                                                                 G7C2PGM 
01485 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
01486 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
01487                                                                   G7C2PGM 
01488      IF (( WS-02-MULT-REL-PROC-PCT-L2   NOT = ZEROS AND           G7C2PGM 
01489            S2RNOC2I NOT = ZERO)                     OR            G7C2PGM 
01490           (WS-02-MULT-REL-PROC-PCT-L2   =   ZEROS   AND           G7C2PGM 
01491            S2RNOC2I = ZERO))                                      G7C2PGM 
01492      THEN                                                         G7C2PGM 
01493          NEXT SENTENCE                                            G7C2PGM 
01494      ELSE                                                         G7C2PGM 
01495          MOVE  DFHBMUBF  TO  S2RNOC2A                             G7C2PGM 
01496                              S2RPLP2A                             G7C2PGM 
01497          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01498          THEN                                                     G7C2PGM 
01499              NEXT SENTENCE                                        G7C2PGM 
01500          ELSE                                                     G7C2PGM 
01501              MOVE  -1        TO  S2RPLP2L                         G7C2PGM 
01502              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01503              SET WT-01-INDEX TO +02                               G7C2PGM 
01504              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01505                                                                   G7C2PGM 
01506 /*****************************************************************G7C2PGM 
01507 *                                                                *G7C2PGM 
01508 *-- VALIDATE ------ MULT RELATED PROCEDURE LEVEL PCT # 3    -----*G7C2PGM 
01509 *                                                                *G7C2PGM 
01510 ******************************************************************G7C2PGM 
01511 *   D129                                                          G7C2PGM 
01512                                                                   G7C2PGM 
01513      MOVE S2RPLP3I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01514      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01515      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01516      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01517      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01518      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01519          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01520              MOVE -1       TO S2RPLP3L                            G7C2PGM 
01521              MOVE DFHBMUBF TO S2RPLP3A                            G7C2PGM 
01522              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01523                  NEXT SENTENCE                                    G7C2PGM 
01524              ELSE                                                 G7C2PGM 
01525                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01526                  SET WT-01-INDEX TO +12                           G7C2PGM 
01527                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01528          ELSE                                                     G7C2PGM 
01529              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01530                TO WS-02-MULT-REL-PROC-PCT-L3                      G7C2PGM 
01531              MOVE WS-02-MULT-REL-PROC-PCT-L3                      G7C2PGM 
01532                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01533              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01534                TO S2RPLP3O                                        G7C2PGM 
01535      ELSE                                                         G7C2PGM 
01536          MOVE -1       TO S2RPLP3L                                G7C2PGM 
01537          MOVE DFHBMUBF TO S2RPLP3A                                G7C2PGM 
01538          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01539              NEXT SENTENCE                                        G7C2PGM 
01540          ELSE                                                     G7C2PGM 
01541              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01542              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01543                  SET WT-01-INDEX TO +10                           G7C2PGM 
01544                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01545              ELSE                                                 G7C2PGM 
01546                  SET WT-01-INDEX TO +11                           G7C2PGM 
01547                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01548                                                                   G7C2PGM 
01549                                                                   G7C2PGM 
01550 *    MOVE S2RPLP3I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01551 *                                                                 G7C2PGM 
01552 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01553 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01554 *                                                                 G7C2PGM 
01555 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01556 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01557 *         WS-02-MULT-PCT-C4 EQUAL '.' )                           G7C2PGM 
01558 *    THEN                                                         G7C2PGM 
01559 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01560 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01561 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01562 *                 WS-02-MULT-REL-PROC-PCT-L3                      G7C2PGM 
01563 *    ELSE                                                         G7C2PGM 
01564 *        MOVE  DFHBMUBF  TO  S2RPLP3A                             G7C2PGM 
01565 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01566 *        THEN                                                     G7C2PGM 
01567 *            NEXT SENTENCE                                        G7C2PGM 
01568 *        ELSE                                                     G7C2PGM 
01569 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01570 *               MOVE  -1        TO  S2RPLP3L                      G7C2PGM 
01571 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01572 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01573 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01574 *            ELSE                                                 G7C2PGM 
01575 *               MOVE  -1        TO  S2RPLP3L                      G7C2PGM 
01576 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01577 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01578 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01579                                                                   G7C2PGM 
01580                                                                   G7C2PGM 
01581 *-- VALIDATE ------ MULT   RELATED LEVEL 3 NO. OF OCCURRENCES ---*G7C2PGM 
01582 *   1. ALPHANUMERIC                                               G7C2PGM 
01583 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01584                                                                   G7C2PGM 
01585      MOVE  S2RNOC3I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01586      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01587      THEN                                                         G7C2PGM 
01588          MOVE  S2RNOC3I TO GCVI-VALUE                             G7C2PGM 
01589          MOVE  'BPCB13' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01590          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01591          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01592          THEN                                                     G7C2PGM 
01593              MOVE  DFHBMUBF  TO  S2RNOC3A                         G7C2PGM 
01594              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01595              THEN                                                 G7C2PGM 
01596                  NEXT SENTENCE                                    G7C2PGM 
01597              ELSE                                                 G7C2PGM 
01598                  MOVE  -1        TO  S2RNOC3L                     G7C2PGM 
01599                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01600                  SET WT-01-INDEX TO +09                           G7C2PGM 
01601                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01602          ELSE                                                     G7C2PGM 
01603              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01604              THEN                                                 G7C2PGM 
01605                  MOVE  DFHBMUBF  TO  S2RNOC3A                     G7C2PGM 
01606              ELSE                                                 G7C2PGM 
01607                  NEXT SENTENCE                                    G7C2PGM 
01608      ELSE                                                         G7C2PGM 
01609          MOVE  DFHBMUBF  TO  S2RNOC3A                             G7C2PGM 
01610          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01611          THEN                                                     G7C2PGM 
01612              NEXT SENTENCE                                        G7C2PGM 
01613          ELSE                                                     G7C2PGM 
01614              MOVE  -1        TO  S2RNOC3L                         G7C2PGM 
01615              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01616              SET WT-01-INDEX TO +08                               G7C2PGM 
01617              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01618                                                                   G7C2PGM 
01619                                                                   G7C2PGM 
01620 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01621 *                                                                 G7C2PGM 
01622 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
01623 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
01624                                                                   G7C2PGM 
01625      IF ((WS-02-MULT-REL-PROC-PCT-L3   NOT = ZEROS AND            G7C2PGM 
01626           S2RNOC3I NOT = ZERO)                      OR            G7C2PGM 
01627          (WS-02-MULT-REL-PROC-PCT-L3   =   ZEROS   AND            G7C2PGM 
01628           S2RNOC3I = ZERO))                                       G7C2PGM 
01629      THEN                                                         G7C2PGM 
01630          NEXT SENTENCE                                            G7C2PGM 
01631      ELSE                                                         G7C2PGM 
01632          MOVE  DFHBMUBF  TO  S2RNOC3A                             G7C2PGM 
01633                              S2RPLP3A                             G7C2PGM 
01634          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01635          THEN                                                     G7C2PGM 
01636              NEXT SENTENCE                                        G7C2PGM 
01637          ELSE                                                     G7C2PGM 
01638              MOVE  -1        TO  S2RPLP3L                         G7C2PGM 
01639              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01640              SET WT-01-INDEX TO +02                               G7C2PGM 
01641              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01642                                                                   G7C2PGM 
01643                                                                   G7C2PGM 
01644 /*****************************************************************G7C2PGM 
01645 *                                                                *G7C2PGM 
01646 *-- VALIDATE ------ MULT INJURY PROCEDURE LEVEL PCT # 1     -----*G7C2PGM 
01647 *                                                                *G7C2PGM 
01648 ******************************************************************G7C2PGM 
01649 *   D129                                                          G7C2PGM 
01650                                                                   G7C2PGM 
01651      MOVE S2IPLP1I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01652      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01653      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01654      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01655      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01656      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01657          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01658              MOVE -1       TO S2IPLP1L                            G7C2PGM 
01659              MOVE DFHBMUBF TO S2IPLP1A                            G7C2PGM 
01660              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01661                  NEXT SENTENCE                                    G7C2PGM 
01662              ELSE                                                 G7C2PGM 
01663                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01664                  SET WT-01-INDEX TO +12                           G7C2PGM 
01665                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01666          ELSE                                                     G7C2PGM 
01667              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01668                TO WS-02-MULT-INJ-LVL-PCT-L1                       G7C2PGM 
01669              MOVE WS-02-MULT-INJ-LVL-PCT-L1                       G7C2PGM 
01670                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01671              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01672                TO S2IPLP1O                                        G7C2PGM 
01673      ELSE                                                         G7C2PGM 
01674          MOVE -1       TO S2IPLP1L                                G7C2PGM 
01675          MOVE DFHBMUBF TO S2IPLP1A                                G7C2PGM 
01676          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01677              NEXT SENTENCE                                        G7C2PGM 
01678          ELSE                                                     G7C2PGM 
01679              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01680              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01681                  SET WT-01-INDEX TO +10                           G7C2PGM 
01682                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01683              ELSE                                                 G7C2PGM 
01684                  SET WT-01-INDEX TO +11                           G7C2PGM 
01685                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01686                                                                   G7C2PGM 
01687                                                                   G7C2PGM 
01688 *    MOVE S2IPLP1I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01689 *                                                                 G7C2PGM 
01690 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01691 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01692 *                                                                 G7C2PGM 
01693 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01694 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01695 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
01696 *    THEN                                                         G7C2PGM 
01697 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01698 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01699 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01700 *                 WS-02-MULT-INJ-LVL-PCT-L1                       G7C2PGM 
01701 *    ELSE                                                         G7C2PGM 
01702 *        MOVE  DFHBMUBF  TO  S2IPLP1A                             G7C2PGM 
01703 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01704 *        THEN                                                     G7C2PGM 
01705 *            NEXT SENTENCE                                        G7C2PGM 
01706 *        ELSE                                                     G7C2PGM 
01707 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01708 *               MOVE  -1        TO  S2IPLP1L                      G7C2PGM 
01709 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01710 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01711 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01712 *            ELSE                                                 G7C2PGM 
01713 *               MOVE  -1        TO  S2IPLP1L                      G7C2PGM 
01714 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01715 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01716 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01717                                                                   G7C2PGM 
01718 *-- VALIDATE ------ MULT INJURY LEVEL 1 NO. OF OCCURRENCES ---*   G7C2PGM 
01719 *   1. ALPHANUMERIC                                               G7C2PGM 
01720 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01721                                                                   G7C2PGM 
01722      MOVE  S2INOC1I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01723      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01724      THEN                                                         G7C2PGM 
01725          MOVE  S2INOC1I TO GCVI-VALUE                             G7C2PGM 
01726          MOVE  'BPCB14' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01727          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01728          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01729          THEN                                                     G7C2PGM 
01730              MOVE  DFHBMUBF  TO  S2INOC1A                         G7C2PGM 
01731              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01732              THEN                                                 G7C2PGM 
01733                  NEXT SENTENCE                                    G7C2PGM 
01734              ELSE                                                 G7C2PGM 
01735                  MOVE  -1        TO  S2INOC1L                     G7C2PGM 
01736                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01737                  SET WT-01-INDEX TO +09                           G7C2PGM 
01738                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01739          ELSE                                                     G7C2PGM 
01740              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01741              THEN                                                 G7C2PGM 
01742                  MOVE  DFHBMUBF  TO  S2INOC1A                     G7C2PGM 
01743              ELSE                                                 G7C2PGM 
01744                  NEXT SENTENCE                                    G7C2PGM 
01745      ELSE                                                         G7C2PGM 
01746          MOVE  DFHBMUBF  TO  S2INOC1A                             G7C2PGM 
01747          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01748          THEN                                                     G7C2PGM 
01749              NEXT SENTENCE                                        G7C2PGM 
01750          ELSE                                                     G7C2PGM 
01751              MOVE  -1        TO  S2INOC1L                         G7C2PGM 
01752              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01753              SET WT-01-INDEX TO +08                               G7C2PGM 
01754              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01755                                                                   G7C2PGM 
01756                                                                   G7C2PGM 
01757 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01758 *                                                                 G7C2PGM 
01759 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
01760 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
01761                                                                   G7C2PGM 
01762      IF  ((WS-02-MULT-INJ-LVL-PCT-L1   NOT = ZEROS   AND          G7C2PGM 
01763            S2INOC1I NOT = ZERO)                      OR           G7C2PGM 
01764           (WS-02-MULT-INJ-LVL-PCT-L1   =   ZEROS     AND          G7C2PGM 
01765            S2INOC1I = ZERO))                                      G7C2PGM 
01766      THEN                                                         G7C2PGM 
01767          NEXT SENTENCE                                            G7C2PGM 
01768      ELSE                                                         G7C2PGM 
01769          MOVE  DFHBMUBF  TO  S2INOC1A                             G7C2PGM 
01770                              S2IPLP1A                             G7C2PGM 
01771          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01772          THEN                                                     G7C2PGM 
01773              NEXT SENTENCE                                        G7C2PGM 
01774          ELSE                                                     G7C2PGM 
01775              MOVE  -1        TO  S2IPLP1L                         G7C2PGM 
01776              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01777              SET WT-01-INDEX TO +02                               G7C2PGM 
01778              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01779                                                                   G7C2PGM 
01780 /*****************************************************************G7C2PGM 
01781 *                                                                *G7C2PGM 
01782 *-- VALIDATE ------ MULT   INJURY PROCEDURE LEVEL PCT # 2   -----*G7C2PGM 
01783 *                                                                *G7C2PGM 
01784 ******************************************************************G7C2PGM 
01785 *   D129                                                          G7C2PGM 
01786                                                                   G7C2PGM 
01787      MOVE S2IPLP2I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01788      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01789      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01790      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01791      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01792      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01793          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01794              MOVE -1       TO S2IPLP2L                            G7C2PGM 
01795              MOVE DFHBMUBF TO S2IPLP2A                            G7C2PGM 
01796              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01797                  NEXT SENTENCE                                    G7C2PGM 
01798              ELSE                                                 G7C2PGM 
01799                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01800                  SET WT-01-INDEX TO +12                           G7C2PGM 
01801                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01802          ELSE                                                     G7C2PGM 
01803              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01804                TO WS-02-MULT-INJ-LVL-PCT-L2                       G7C2PGM 
01805              MOVE WS-02-MULT-INJ-LVL-PCT-L2                       G7C2PGM 
01806                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01807              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01808                TO S2IPLP2O                                        G7C2PGM 
01809      ELSE                                                         G7C2PGM 
01810          MOVE -1       TO S2IPLP2L                                G7C2PGM 
01811          MOVE DFHBMUBF TO S2IPLP2A                                G7C2PGM 
01812          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01813              NEXT SENTENCE                                        G7C2PGM 
01814          ELSE                                                     G7C2PGM 
01815              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01816              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01817                  SET WT-01-INDEX TO +10                           G7C2PGM 
01818                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01819              ELSE                                                 G7C2PGM 
01820                  SET WT-01-INDEX TO +11                           G7C2PGM 
01821                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01822                                                                   G7C2PGM 
01823                                                                   G7C2PGM 
01824 *    MOVE S2IPLP2I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01825 *                                                                 G7C2PGM 
01826 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01827 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01828 *                                                                 G7C2PGM 
01829 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01830 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01831 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
01832 *    THEN                                                         G7C2PGM 
01833 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01834 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01835 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01836 *                 WS-02-MULT-INJ-LVL-PCT-L2                       G7C2PGM 
01837 *    ELSE                                                         G7C2PGM 
01838 *        MOVE  DFHBMUBF  TO  S2IPLP2A                             G7C2PGM 
01839 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01840 *        THEN                                                     G7C2PGM 
01841 *            NEXT SENTENCE                                        G7C2PGM 
01842 *        ELSE                                                     G7C2PGM 
01843 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01844 *               MOVE  -1        TO  S2IPLP2L                      G7C2PGM 
01845 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01846 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01847 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01848 *            ELSE                                                 G7C2PGM 
01849 *               MOVE  -1        TO  S2IPLP2L                      G7C2PGM 
01850 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01851 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01852 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01853                                                                   G7C2PGM 
01854 *-- VALIDATE ------ MULT   INJURY LEVEL 2 NO. OF OCCURRENCES ---* G7C2PGM 
01855 *   1. ALPHANUMERIC                                               G7C2PGM 
01856 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01857                                                                   G7C2PGM 
01858      MOVE  S2INOC2I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01859      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01860      THEN                                                         G7C2PGM 
01861          MOVE  S2INOC2I TO GCVI-VALUE                             G7C2PGM 
01862          MOVE  'BPCB14' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
01863          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
01864          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
01865          THEN                                                     G7C2PGM 
01866              MOVE  DFHBMUBF  TO  S2INOC2A                         G7C2PGM 
01867              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
01868              THEN                                                 G7C2PGM 
01869                  NEXT SENTENCE                                    G7C2PGM 
01870              ELSE                                                 G7C2PGM 
01871                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
01872                  MOVE  -1        TO  S2INOC2L                     G7C2PGM 
01873                  SET WT-01-INDEX TO +09                           G7C2PGM 
01874                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01875          ELSE                                                     G7C2PGM 
01876              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
01877              THEN                                                 G7C2PGM 
01878                  MOVE  DFHBMUBF  TO  S2INOC2A                     G7C2PGM 
01879              ELSE                                                 G7C2PGM 
01880                  NEXT SENTENCE                                    G7C2PGM 
01881      ELSE                                                         G7C2PGM 
01882          MOVE  DFHBMUBF  TO  S2INOC2A                             G7C2PGM 
01883          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01884          THEN                                                     G7C2PGM 
01885              NEXT SENTENCE                                        G7C2PGM 
01886          ELSE                                                     G7C2PGM 
01887              MOVE  -1        TO  S2INOC2L                         G7C2PGM 
01888              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01889              SET WT-01-INDEX TO +08                               G7C2PGM 
01890              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01891                                                                   G7C2PGM 
01892                                                                   G7C2PGM 
01893 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
01894 *                                                                 G7C2PGM 
01895 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
01896 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
01897                                                                   G7C2PGM 
01898      IF  ((WS-02-MULT-INJ-LVL-PCT-L2   NOT = ZEROS  AND           G7C2PGM 
01899            S2INOC2I NOT = ZERO)                     OR            G7C2PGM 
01900           (WS-02-MULT-INJ-LVL-PCT-L2   =   ZEROS    AND           G7C2PGM 
01901            S2INOC2I = ZERO))                                      G7C2PGM 
01902      THEN                                                         G7C2PGM 
01903          NEXT SENTENCE                                            G7C2PGM 
01904      ELSE                                                         G7C2PGM 
01905          MOVE  DFHBMUBF  TO  S2INOC2A                             G7C2PGM 
01906                              S2IPLP2A                             G7C2PGM 
01907          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01908          THEN                                                     G7C2PGM 
01909              NEXT SENTENCE                                        G7C2PGM 
01910          ELSE                                                     G7C2PGM 
01911              MOVE  -1        TO  S2IPLP2L                         G7C2PGM 
01912              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
01913              SET WT-01-INDEX TO +02                               G7C2PGM 
01914              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
01915                                                                   G7C2PGM 
01916 /*****************************************************************G7C2PGM 
01917 *                                                                *G7C2PGM 
01918 *-- VALIDATE ------ MULT INJURY PROCEDURE LEVEL PCT # 3     -----*G7C2PGM 
01919 *                                                                *G7C2PGM 
01920 ******************************************************************G7C2PGM 
01921 *   D129                                                          G7C2PGM 
01922                                                                   G7C2PGM 
01923      MOVE S2IPLP3I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
01924      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
01925      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
01926      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
01927      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
01928      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
01929          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
01930              MOVE -1       TO S2IPLP3L                            G7C2PGM 
01931              MOVE DFHBMUBF TO S2IPLP3A                            G7C2PGM 
01932              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
01933                  NEXT SENTENCE                                    G7C2PGM 
01934              ELSE                                                 G7C2PGM 
01935                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
01936                  SET WT-01-INDEX TO +12                           G7C2PGM 
01937                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01938          ELSE                                                     G7C2PGM 
01939              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
01940                TO WS-02-MULT-INJ-LVL-PCT-L3                       G7C2PGM 
01941              MOVE WS-02-MULT-INJ-LVL-PCT-L3                       G7C2PGM 
01942                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
01943              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
01944                TO S2IPLP3O                                        G7C2PGM 
01945      ELSE                                                         G7C2PGM 
01946          MOVE -1       TO S2IPLP3L                                G7C2PGM 
01947          MOVE DFHBMUBF TO S2IPLP3A                                G7C2PGM 
01948          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
01949              NEXT SENTENCE                                        G7C2PGM 
01950          ELSE                                                     G7C2PGM 
01951              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
01952              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
01953                  SET WT-01-INDEX TO +10                           G7C2PGM 
01954                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
01955              ELSE                                                 G7C2PGM 
01956                  SET WT-01-INDEX TO +11                           G7C2PGM 
01957                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
01958                                                                   G7C2PGM 
01959                                                                   G7C2PGM 
01960 *    MOVE S2IPLP3I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
01961 *                                                                 G7C2PGM 
01962 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
01963 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
01964 *                                                                 G7C2PGM 
01965 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
01966 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
01967 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
01968 *    THEN                                                         G7C2PGM 
01969 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
01970 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
01971 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
01972 *                 WS-02-MULT-INJ-LVL-PCT-L3                       G7C2PGM 
01973 *    ELSE                                                         G7C2PGM 
01974 *        MOVE  DFHBMUBF  TO  S2IPLP3A                             G7C2PGM 
01975 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
01976 *        THEN                                                     G7C2PGM 
01977 *            NEXT SENTENCE                                        G7C2PGM 
01978 *        ELSE                                                     G7C2PGM 
01979 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
01980 *               MOVE  -1        TO  S2IPLP3L                      G7C2PGM 
01981 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01982 *               SET WT-01-INDEX TO +10                            G7C2PGM 
01983 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
01984 *            ELSE                                                 G7C2PGM 
01985 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
01986 *               MOVE  -1        TO  S2IPLP3L                      G7C2PGM 
01987 *               SET WT-01-INDEX TO +05                            G7C2PGM 
01988 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
01989                                                                   G7C2PGM 
01990                                                                   G7C2PGM 
01991 *-- VALIDATE ------ MULT   INJURY LEVEL 3 NO. OF OCCURRENCES ---* G7C2PGM 
01992 *   1. ALPHANUMERIC                                               G7C2PGM 
01993 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
01994                                                                   G7C2PGM 
01995      MOVE  S2INOC3I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
01996      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
01997      THEN                                                         G7C2PGM 
01998          MOVE  S2INOC3I TO GCVI-VALUE                             G7C2PGM 
01999          MOVE  'BPCB14' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
02000          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
02001          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
02002          THEN                                                     G7C2PGM 
02003              MOVE  DFHBMUBF  TO  S2INOC3A                         G7C2PGM 
02004              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
02005              THEN                                                 G7C2PGM 
02006                  NEXT SENTENCE                                    G7C2PGM 
02007              ELSE                                                 G7C2PGM 
02008                 MOVE  -1        TO  S2INOC3L                      G7C2PGM 
02009                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02010                  SET WT-01-INDEX TO +09                           G7C2PGM 
02011                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02012          ELSE                                                     G7C2PGM 
02013              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
02014              THEN                                                 G7C2PGM 
02015                  MOVE  DFHBMUBF  TO  S2INOC3A                     G7C2PGM 
02016              ELSE                                                 G7C2PGM 
02017                  NEXT SENTENCE                                    G7C2PGM 
02018      ELSE                                                         G7C2PGM 
02019          MOVE  DFHBMUBF  TO  S2INOC3A                             G7C2PGM 
02020          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02021          THEN                                                     G7C2PGM 
02022              NEXT SENTENCE                                        G7C2PGM 
02023          ELSE                                                     G7C2PGM 
02024              MOVE  -1        TO  S2INOC3L                         G7C2PGM 
02025              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02026              SET WT-01-INDEX TO +08                               G7C2PGM 
02027              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02028                                                                   G7C2PGM 
02029                                                                   G7C2PGM 
02030 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
02031 *                                                                 G7C2PGM 
02032 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
02033 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
02034                                                                   G7C2PGM 
02035      IF  ((WS-02-MULT-INJ-LVL-PCT-L3  NOT = ZEROS AND             G7C2PGM 
02036            S2INOC3I NOT = ZERO)                   OR              G7C2PGM 
02037           (WS-02-MULT-INJ-LVL-PCT-L3  = ZEROS     AND             G7C2PGM 
02038            S2INOC3I = ZERO))                                      G7C2PGM 
02039      THEN                                                         G7C2PGM 
02040          NEXT SENTENCE                                            G7C2PGM 
02041      ELSE                                                         G7C2PGM 
02042          MOVE  DFHBMUBF  TO  S2INOC3A                             G7C2PGM 
02043                              S2IPLP3A                             G7C2PGM 
02044          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02045          THEN                                                     G7C2PGM 
02046              NEXT SENTENCE                                        G7C2PGM 
02047          ELSE                                                     G7C2PGM 
02048              MOVE  -1        TO  S2IPLP3L                         G7C2PGM 
02049              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02050              SET WT-01-INDEX TO +02                               G7C2PGM 
02051              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02052                                                                   G7C2PGM 
02053 /*****************************************************************G7C2PGM 
02054 *                                                                *G7C2PGM 
02055 *-- VALIDATE ------ MULT PODIATRY PROCEDURE  LEVEL PCT # 1  -----*G7C2PGM 
02056 *                                                                *G7C2PGM 
02057 ******************************************************************G7C2PGM 
02058 *   D129                                                          G7C2PGM 
02059                                                                   G7C2PGM 
02060      MOVE S2PPLP1I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
02061      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
02062      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
02063      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
02064      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
02065      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
02066          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
02067              MOVE -1       TO S2PPLP1L                            G7C2PGM 
02068              MOVE DFHBMUBF TO S2PPLP1A                            G7C2PGM 
02069              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
02070                  NEXT SENTENCE                                    G7C2PGM 
02071              ELSE                                                 G7C2PGM 
02072                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
02073                  SET WT-01-INDEX TO +12                           G7C2PGM 
02074                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02075          ELSE                                                     G7C2PGM 
02076              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
02077                TO WS-02-MULT-POD-PROC-PCT-L1                      G7C2PGM 
02078              MOVE WS-02-MULT-POD-PROC-PCT-L1                      G7C2PGM 
02079                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
02080              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
02081                TO S2PPLP1O                                        G7C2PGM 
02082      ELSE                                                         G7C2PGM 
02083          MOVE -1       TO S2PPLP1L                                G7C2PGM 
02084          MOVE DFHBMUBF TO S2PPLP1A                                G7C2PGM 
02085          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
02086              NEXT SENTENCE                                        G7C2PGM 
02087          ELSE                                                     G7C2PGM 
02088              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
02089              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
02090                  SET WT-01-INDEX TO +10                           G7C2PGM 
02091                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02092              ELSE                                                 G7C2PGM 
02093                  SET WT-01-INDEX TO +11                           G7C2PGM 
02094                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
02095                                                                   G7C2PGM 
02096                                                                   G7C2PGM 
02097 *    MOVE S2PPLP1I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
02098 *                                                                 G7C2PGM 
02099 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
02100 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
02101 *                                                                 G7C2PGM 
02102 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
02103 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
02104 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
02105 *    THEN                                                         G7C2PGM 
02106 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
02107 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
02108 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
02109 *                 WS-02-MULT-POD-PROC-PCT-L1                      G7C2PGM 
02110 *    ELSE                                                         G7C2PGM 
02111 *        MOVE  DFHBMUBF  TO  S2PPLP1A                             G7C2PGM 
02112 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02113 *        THEN                                                     G7C2PGM 
02114 *            NEXT SENTENCE                                        G7C2PGM 
02115 *        ELSE                                                     G7C2PGM 
02116 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
02117 *               MOVE  -1        TO  S2PPLP1L                      G7C2PGM 
02118 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02119 *               SET WT-01-INDEX TO +10                            G7C2PGM 
02120 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
02121 *            ELSE                                                 G7C2PGM 
02122 *               MOVE  -1        TO  S2PPLP1L                      G7C2PGM 
02123 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02124 *               SET WT-01-INDEX TO +05                            G7C2PGM 
02125 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
02126                                                                   G7C2PGM 
02127 *-- VALIDATE ------ MULT PODIATRY LEVEL 1 NO. OF OCCURRENCES ---* G7C2PGM 
02128 *   1. ALPHANUMERIC                                               G7C2PGM 
02129 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
02130                                                                   G7C2PGM 
02131      MOVE  S2PNOC1I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
02132      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
02133      THEN                                                         G7C2PGM 
02134          MOVE  S2PNOC1I TO GCVI-VALUE                             G7C2PGM 
02135          MOVE  'BPCB15' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
02136          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
02137          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
02138          THEN                                                     G7C2PGM 
02139              MOVE  DFHBMUBF  TO  S2PNOC1A                         G7C2PGM 
02140              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
02141              THEN                                                 G7C2PGM 
02142                  NEXT SENTENCE                                    G7C2PGM 
02143              ELSE                                                 G7C2PGM 
02144                  MOVE  -1        TO  S2PNOC1L                     G7C2PGM 
02145                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02146                  SET WT-01-INDEX TO +09                           G7C2PGM 
02147                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02148          ELSE                                                     G7C2PGM 
02149              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
02150              THEN                                                 G7C2PGM 
02151                  MOVE  DFHBMUBF  TO  S2PNOC1A                     G7C2PGM 
02152              ELSE                                                 G7C2PGM 
02153                  NEXT SENTENCE                                    G7C2PGM 
02154      ELSE                                                         G7C2PGM 
02155          MOVE  DFHBMUBF  TO  S2PNOC1A                             G7C2PGM 
02156          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02157          THEN                                                     G7C2PGM 
02158              NEXT SENTENCE                                        G7C2PGM 
02159          ELSE                                                     G7C2PGM 
02160              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02161              MOVE  -1        TO  S2PNOC1L                         G7C2PGM 
02162              SET WT-01-INDEX TO +08                               G7C2PGM 
02163              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02164                                                                   G7C2PGM 
02165 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
02166 *                                                                 G7C2PGM 
02167 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
02168 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
02169                                                                   G7C2PGM 
02170      IF  ((WS-02-MULT-POD-PROC-PCT-L1   NOT = ZEROS AND           G7C2PGM 
02171            S2PNOC1I NOT = ZERO)                     OR            G7C2PGM 
02172           (WS-02-MULT-POD-PROC-PCT-L1   =   ZEROS   AND           G7C2PGM 
02173            S2PNOC1I = ZERO))                                      G7C2PGM 
02174      THEN                                                         G7C2PGM 
02175          NEXT SENTENCE                                            G7C2PGM 
02176      ELSE                                                         G7C2PGM 
02177          MOVE  DFHBMUBF  TO  S2PPLP1A                             G7C2PGM 
02178                              S2PNOC1A                             G7C2PGM 
02179          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02180          THEN                                                     G7C2PGM 
02181              NEXT SENTENCE                                        G7C2PGM 
02182          ELSE                                                     G7C2PGM 
02183             MOVE  -1        TO  S2PPLP1L                          G7C2PGM 
02184              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02185              SET WT-01-INDEX TO +02                               G7C2PGM 
02186              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02187                                                                   G7C2PGM 
02188                                                                   G7C2PGM 
02189                                                                   G7C2PGM 
02190 /*****************************************************************G7C2PGM 
02191 *                                                                *G7C2PGM 
02192 *-- VALIDATE ------ MULT PODIATRY PROCEDURE  LEVEL PCT # 2  -----*G7C2PGM 
02193 *                                                                *G7C2PGM 
02194 ******************************************************************G7C2PGM 
02195 *   D129                                                          G7C2PGM 
02196                                                                   G7C2PGM 
02197      MOVE S2PPLP2I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
02198      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
02199      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
02200      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
02201      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
02202      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
02203          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
02204              MOVE -1       TO S2PPLP2L                            G7C2PGM 
02205              MOVE DFHBMUBF TO S2PPLP2A                            G7C2PGM 
02206              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
02207                  NEXT SENTENCE                                    G7C2PGM 
02208              ELSE                                                 G7C2PGM 
02209                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
02210                  SET WT-01-INDEX TO +12                           G7C2PGM 
02211                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02212          ELSE                                                     G7C2PGM 
02213              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
02214                TO WS-02-MULT-POD-PROC-PCT-L2                      G7C2PGM 
02215              MOVE WS-02-MULT-POD-PROC-PCT-L2                      G7C2PGM 
02216                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
02217              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
02218                TO S2PPLP2O                                        G7C2PGM 
02219      ELSE                                                         G7C2PGM 
02220          MOVE -1       TO S2PPLP2L                                G7C2PGM 
02221          MOVE DFHBMUBF TO S2PPLP2A                                G7C2PGM 
02222          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
02223              NEXT SENTENCE                                        G7C2PGM 
02224          ELSE                                                     G7C2PGM 
02225              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
02226              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
02227                  SET WT-01-INDEX TO +10                           G7C2PGM 
02228                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02229              ELSE                                                 G7C2PGM 
02230                  SET WT-01-INDEX TO +11                           G7C2PGM 
02231                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
02232                                                                   G7C2PGM 
02233                                                                   G7C2PGM 
02234 *    MOVE S2PPLP2I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
02235 *                                                                 G7C2PGM 
02236 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
02237 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
02238 *                                                                 G7C2PGM 
02239 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
02240 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
02241 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
02242 *    THEN                                                         G7C2PGM 
02243 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
02244 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
02245 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
02246 *                 WS-02-MULT-POD-PROC-PCT-L2                      G7C2PGM 
02247 *    ELSE                                                         G7C2PGM 
02248 *        MOVE  DFHBMUBF  TO  S2PPLP2A                             G7C2PGM 
02249 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02250 *        THEN                                                     G7C2PGM 
02251 *            NEXT SENTENCE                                        G7C2PGM 
02252 *        ELSE                                                     G7C2PGM 
02253 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
02254 *               MOVE  -1        TO  S2PPLP2L                      G7C2PGM 
02255 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02256 *               SET WT-01-INDEX TO +10                            G7C2PGM 
02257 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
02258 *            ELSE                                                 G7C2PGM 
02259 *               MOVE  -1        TO  S2PPLP2L                      G7C2PGM 
02260 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02261 *               SET WT-01-INDEX TO +05                            G7C2PGM 
02262 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
02263                                                                   G7C2PGM 
02264 *-- VALIDATE ------ MULT PODIATRY LEVEL 2 NO. OF OCCURRENCES ---* G7C2PGM 
02265 *   1. ALPHANUMERIC                                               G7C2PGM 
02266 *   2. FIELD VALIDATION SUB SYSTEM                                G7C2PGM 
02267                                                                   G7C2PGM 
02268      MOVE  S2PNOC2I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
02269      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
02270      THEN                                                         G7C2PGM 
02271          MOVE  S2PNOC2I TO GCVI-VALUE                             G7C2PGM 
02272          MOVE  'BPCB15' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
02273          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
02274          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
02275          THEN                                                     G7C2PGM 
02276              MOVE  DFHBMUBF  TO  S2PNOC2A                         G7C2PGM 
02277              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
02278              THEN                                                 G7C2PGM 
02279                  NEXT SENTENCE                                    G7C2PGM 
02280              ELSE                                                 G7C2PGM 
02281                  MOVE  -1        TO  S2PNOC2L                     G7C2PGM 
02282                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02283                  SET WT-01-INDEX TO +09                           G7C2PGM 
02284                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02285          ELSE                                                     G7C2PGM 
02286              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
02287              THEN                                                 G7C2PGM 
02288                  MOVE  DFHBMUBF  TO  S2PNOC2A                     G7C2PGM 
02289              ELSE                                                 G7C2PGM 
02290                  NEXT SENTENCE                                    G7C2PGM 
02291      ELSE                                                         G7C2PGM 
02292          MOVE  DFHBMUBF  TO  S2PNOC2A                             G7C2PGM 
02293          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02294          THEN                                                     G7C2PGM 
02295              NEXT SENTENCE                                        G7C2PGM 
02296          ELSE                                                     G7C2PGM 
02297              MOVE  -1        TO  S2PNOC2L                         G7C2PGM 
02298              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02299              SET WT-01-INDEX TO +08                               G7C2PGM 
02300              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02301                                                                   G7C2PGM 
02302 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
02303 *                                                                 G7C2PGM 
02304 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
02305 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
02306                                                                   G7C2PGM 
02307      IF  ((WS-02-MULT-POD-PROC-PCT-L2   NOT = ZEROS AND           G7C2PGM 
02308            S2PNOC2I NOT = ZERO)                     OR            G7C2PGM 
02309           (WS-02-MULT-POD-PROC-PCT-L2   =   ZEROS   AND           G7C2PGM 
02310            S2PNOC2I = ZERO))                                      G7C2PGM 
02311      THEN                                                         G7C2PGM 
02312          NEXT SENTENCE                                            G7C2PGM 
02313      ELSE                                                         G7C2PGM 
02314          MOVE  DFHBMUBF  TO  S2PPLP2A                             G7C2PGM 
02315                              S2PNOC2A                             G7C2PGM 
02316          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02317          THEN                                                     G7C2PGM 
02318              NEXT SENTENCE                                        G7C2PGM 
02319          ELSE                                                     G7C2PGM 
02320              MOVE  -1        TO  S2PPLP2L                         G7C2PGM 
02321              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02322              SET WT-01-INDEX TO +02                               G7C2PGM 
02323              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02324                                                                   G7C2PGM 
02325                                                                   G7C2PGM 
02326                                                                   G7C2PGM 
02327 /*****************************************************************G7C2PGM 
02328 *                                                                *G7C2PGM 
02329 *-- VALIDATE ------ MULT PODIATRY PROCEDURE  LEVEL PCT # 3  -----*G7C2PGM 
02330 *                                                                *G7C2PGM 
02331 ******************************************************************G7C2PGM 
02332 *   D129                                                          G7C2PGM 
02333                                                                   G7C2PGM 
02334      MOVE S2PPLP3I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
02335      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
02336      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
02337      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
02338      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
02339      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
02340          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
02341              MOVE -1       TO S2PPLP3L                            G7C2PGM 
02342              MOVE DFHBMUBF TO S2PPLP3A                            G7C2PGM 
02343              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
02344                  NEXT SENTENCE                                    G7C2PGM 
02345              ELSE                                                 G7C2PGM 
02346                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
02347                  SET WT-01-INDEX TO +12                           G7C2PGM 
02348                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02349          ELSE                                                     G7C2PGM 
02350              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
02351                TO WS-02-MULT-POD-PROC-PCT-L3                      G7C2PGM 
02352              MOVE WS-02-MULT-POD-PROC-PCT-L3                      G7C2PGM 
02353                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
02354              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
02355                TO S2PPLP3O                                        G7C2PGM 
02356      ELSE                                                         G7C2PGM 
02357          MOVE -1       TO S2PPLP3L                                G7C2PGM 
02358          MOVE DFHBMUBF TO S2PPLP3A                                G7C2PGM 
02359          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
02360              NEXT SENTENCE                                        G7C2PGM 
02361          ELSE                                                     G7C2PGM 
02362              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
02363              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
02364                  SET WT-01-INDEX TO +10                           G7C2PGM 
02365                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02366              ELSE                                                 G7C2PGM 
02367                  SET WT-01-INDEX TO +11                           G7C2PGM 
02368                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
02369                                                                   G7C2PGM 
02370                                                                   G7C2PGM 
02371 *    MOVE S2PPLP3I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
02372 *                                                                 G7C2PGM 
02373 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
02374 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
02375 *                                                                 G7C2PGM 
02376 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
02377 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
02378 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
02379 *    THEN                                                         G7C2PGM 
02380 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
02381 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
02382 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
02383 *                 WS-02-MULT-POD-PROC-PCT-L3                      G7C2PGM 
02384 *    ELSE                                                         G7C2PGM 
02385 *        MOVE  DFHBMUBF  TO  S2PPLP3A                             G7C2PGM 
02386 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02387 *        THEN                                                     G7C2PGM 
02388 *            NEXT SENTENCE                                        G7C2PGM 
02389 *        ELSE                                                     G7C2PGM 
02390 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
02391 *               MOVE  -1        TO  S2PPLP3L                      G7C2PGM 
02392 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02393 *               SET WT-01-INDEX TO +10                            G7C2PGM 
02394 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
02395 *            ELSE                                                 G7C2PGM 
02396 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02397 *               MOVE  -1        TO  S2PPLP3L                      G7C2PGM 
02398 *               SET WT-01-INDEX TO +05                            G7C2PGM 
02399 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
02400                                                                   G7C2PGM 
02401 *-- VALIDATE ------ MULT PODIATRY LEVEL 3 NO. OF OCCURRENCES ---* G7C2PGM 
02402 *   1. ALPHANUMERIC                                               G7C2PGM 
02403                                                                   G7C2PGM 
02404      MOVE  S2PNOC3I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
02405      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
02406      THEN                                                         G7C2PGM 
02407          MOVE  S2PNOC3I TO GCVI-VALUE                             G7C2PGM 
02408          MOVE  'BPCB15' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
02409          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
02410          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
02411          THEN                                                     G7C2PGM 
02412              MOVE  DFHBMUBF  TO  S2PNOC3A                         G7C2PGM 
02413              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
02414              THEN                                                 G7C2PGM 
02415                  NEXT SENTENCE                                    G7C2PGM 
02416              ELSE                                                 G7C2PGM 
02417                  MOVE  -1        TO  S2PNOC3L                     G7C2PGM 
02418                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02419                  SET WT-01-INDEX TO +09                           G7C2PGM 
02420                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02421          ELSE                                                     G7C2PGM 
02422              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
02423              THEN                                                 G7C2PGM 
02424                  MOVE  DFHBMUBF  TO  S2PNOC3A                     G7C2PGM 
02425              ELSE                                                 G7C2PGM 
02426                  NEXT SENTENCE                                    G7C2PGM 
02427      ELSE                                                         G7C2PGM 
02428          MOVE  DFHBMUBF  TO  S2PNOC3A                             G7C2PGM 
02429          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02430          THEN                                                     G7C2PGM 
02431              NEXT SENTENCE                                        G7C2PGM 
02432          ELSE                                                     G7C2PGM 
02433          MOVE  -1        TO  S2PNOC3L                             G7C2PGM 
02434              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02435              SET WT-01-INDEX TO +08                               G7C2PGM 
02436              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02437                                                                   G7C2PGM 
02438 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
02439 *                                                                 G7C2PGM 
02440 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
02441 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
02442                                                                   G7C2PGM 
02443      IF  ((WS-02-MULT-POD-PROC-PCT-L3 NOT EQUAL ZEROS AND         G7C2PGM 
02444            S2PNOC3I NOT EQUAL ZERO)                 OR            G7C2PGM 
02445           (WS-02-MULT-POD-PROC-PCT-L3   =   ZEROS   AND           G7C2PGM 
02446            S2PNOC3I = ZERO))                                      G7C2PGM 
02447      THEN                                                         G7C2PGM 
02448          NEXT SENTENCE                                            G7C2PGM 
02449      ELSE                                                         G7C2PGM 
02450          MOVE  DFHBMUBF  TO  S2PPLP3A                             G7C2PGM 
02451                              S2PNOC3A                             G7C2PGM 
02452          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02453          THEN                                                     G7C2PGM 
02454              NEXT SENTENCE                                        G7C2PGM 
02455          ELSE                                                     G7C2PGM 
02456              MOVE  -1        TO  S2PPLP3L                         G7C2PGM 
02457              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02458              SET WT-01-INDEX TO +02                               G7C2PGM 
02459              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02460                                                                   G7C2PGM 
02461 /*****************************************************************G7C2PGM 
02462 *                                                                *G7C2PGM 
02463 *-- VALIDATE ------ MULT PODIATRY PROCEDURE  LEVEL PCT # 4  -----*G7C2PGM 
02464 *                                                                *G7C2PGM 
02465 ******************************************************************G7C2PGM 
02466 *   D129                                                          G7C2PGM 
02467                                                                   G7C2PGM 
02468      MOVE S2PPLP4I TO D-C-RECEIVE-FIELD.                          G7C2PGM 
02469      MOVE +2 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
02470      MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
02471      MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
02472      PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
02473      IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
02474          IF D-C-RETURN-FIELD-DEC2 > WS-5POS-MAX-AMT               G7C2PGM 
02475              MOVE -1       TO S2PPLP4L                            G7C2PGM 
02476              MOVE DFHBMUBF TO S2PPLP4A                            G7C2PGM 
02477              IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
02478                  NEXT SENTENCE                                    G7C2PGM 
02479              ELSE                                                 G7C2PGM 
02480                  MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
02481                  SET WT-01-INDEX TO +12                           G7C2PGM 
02482                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02483          ELSE                                                     G7C2PGM 
02484              MOVE D-C-RETURN-FIELD-DEC2                           G7C2PGM 
02485                TO WS-02-MULT-POD-PROC-PCT-L4                      G7C2PGM 
02486              MOVE WS-02-MULT-POD-PROC-PCT-L4                      G7C2PGM 
02487                TO WS-02-DISP-5POS-DEC                             G7C2PGM 
02488              MOVE WS-02-DISP-5POS-DEC                             G7C2PGM 
02489                TO S2PPLP4O                                        G7C2PGM 
02490      ELSE                                                         G7C2PGM 
02491          MOVE -1       TO S2PPLP4L                                G7C2PGM 
02492          MOVE DFHBMUBF TO S2PPLP4A                                G7C2PGM 
02493          IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
02494              NEXT SENTENCE                                        G7C2PGM 
02495          ELSE                                                     G7C2PGM 
02496              MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
02497              IF D-C-RETURN-CODE = '10'                            G7C2PGM 
02498                  SET WT-01-INDEX TO +10                           G7C2PGM 
02499                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02500              ELSE                                                 G7C2PGM 
02501                  SET WT-01-INDEX TO +11                           G7C2PGM 
02502                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
02503                                                                   G7C2PGM 
02504                                                                   G7C2PGM 
02505 *    MOVE S2PPLP4I TO   WS-02-MULT-LVL-INPUT-PCT-X.               G7C2PGM 
02506 *                                                                 G7C2PGM 
02507 *    INSPECT WS-02-MULT-LVL-INPUT-PCT-X REPLACING                 G7C2PGM 
02508 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
02509 *                                                                 G7C2PGM 
02510 *    IF ( WS-02-MULT-PCT-C1 IS NUMERIC  AND                       G7C2PGM 
02511 *         WS-02-MULT-PCT-C5 IS NUMERIC  AND                       G7C2PGM 
02512 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
02513 *    THEN                                                         G7C2PGM 
02514 *        MOVE WS-02-MULT-PCT-C1       TO WS-02-IND-LVL-F3         G7C2PGM 
02515 *        MOVE WS-02-MULT-PCT-C5       TO WS-02-IND-LVL-L2         G7C2PGM 
02516 *        MOVE WS-02-MULT-LVL-PROC-PCT TO                          G7C2PGM 
02517 *                 WS-02-MULT-POD-PROC-PCT-L4                      G7C2PGM 
02518 *    ELSE                                                         G7C2PGM 
02519 *        MOVE  DFHBMUBF  TO  S2PPLP4A                             G7C2PGM 
02520 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02521 *        THEN                                                     G7C2PGM 
02522 *            NEXT SENTENCE                                        G7C2PGM 
02523 *        ELSE                                                     G7C2PGM 
02524 *            IF WS-02-MULT-PCT-C4 EQUAL '.'                       G7C2PGM 
02525 *               MOVE  -1        TO  S2PPLP4L                      G7C2PGM 
02526 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02527 *               SET WT-01-INDEX TO +10                            G7C2PGM 
02528 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN               G7C2PGM 
02529 *            ELSE                                                 G7C2PGM 
02530 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02531 *               MOVE  -1        TO  S2PPLP4L                      G7C2PGM 
02532 *               SET WT-01-INDEX TO +05                            G7C2PGM 
02533 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
02534                                                                   G7C2PGM 
02535 *-- VALIDATE ------ MULT PODIATRY LEVEL 4 NO. OF OCCURRENCES ---* G7C2PGM 
02536 *   1. ALPHANUMERIC                                               G7C2PGM 
02537 *   2. FIELDVALIDATION                                            G7C2PGM 
02538                                                                   G7C2PGM 
02539      MOVE  S2PNOC4I TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
02540                                                                   G7C2PGM 
02541      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
02542      THEN                                                         G7C2PGM 
02543          MOVE  S2PNOC4I TO GCVI-VALUE                             G7C2PGM 
02544          MOVE  'BPCB15' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
02545          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
02546          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
02547          THEN                                                     G7C2PGM 
02548              MOVE  DFHBMUBF  TO  S2PNOC4A                         G7C2PGM 
02549              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
02550              THEN                                                 G7C2PGM 
02551                  NEXT SENTENCE                                    G7C2PGM 
02552              ELSE                                                 G7C2PGM 
02553                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02554                  MOVE  -1        TO  S2PNOC4L                     G7C2PGM 
02555                  SET WT-01-INDEX TO +09                           G7C2PGM 
02556                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02557          ELSE                                                     G7C2PGM 
02558              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
02559              THEN                                                 G7C2PGM 
02560                  MOVE  DFHBMUBF  TO  S2PNOC4A                     G7C2PGM 
02561              ELSE                                                 G7C2PGM 
02562                  NEXT SENTENCE                                    G7C2PGM 
02563      ELSE                                                         G7C2PGM 
02564          MOVE  DFHBMUBF  TO  S2PNOC4A                             G7C2PGM 
02565          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02566          THEN                                                     G7C2PGM 
02567              NEXT SENTENCE                                        G7C2PGM 
02568          ELSE                                                     G7C2PGM 
02569              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02570             MOVE  -1        TO  S2PNOC4L                          G7C2PGM 
02571              SET WT-01-INDEX TO +08                               G7C2PGM 
02572              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02573                                                                   G7C2PGM 
02574 *-- VALIDATE ----------------------------------------------------*G7C2PGM 
02575 *                                                                 G7C2PGM 
02576 *   IF LEVEL PERCENT IS > 0 THE OCCURRANCES MUST BE > 0.          G7C2PGM 
02577 *   IF LEVEL PERCENT IS = 0 THE OCCURRANCES MUST BE = 0.          G7C2PGM 
02578                                                                   G7C2PGM 
02579      IF  ((WS-02-MULT-POD-PROC-PCT-L4   NOT = ZEROS AND           G7C2PGM 
02580            S2PNOC4I NOT = ZERO)                      OR           G7C2PGM 
02581           (WS-02-MULT-POD-PROC-PCT-L4   =   ZEROS   AND           G7C2PGM 
02582            S2PNOC4I = ZERO))                                      G7C2PGM 
02583      THEN                                                         G7C2PGM 
02584          NEXT SENTENCE                                            G7C2PGM 
02585      ELSE                                                         G7C2PGM 
02586          MOVE  DFHBMUBF  TO  S2PPLP4A                             G7C2PGM 
02587                              S2PNOC4A                             G7C2PGM 
02588          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02589          THEN                                                     G7C2PGM 
02590              NEXT SENTENCE                                        G7C2PGM 
02591          ELSE                                                     G7C2PGM 
02592              MOVE  -1        TO  S2PPLP4L                         G7C2PGM 
02593              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02594              SET WT-01-INDEX TO +02                               G7C2PGM 
02595              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02596                                                                   G7C2PGM 
02597 /*****************************************************************G7C2PGM 
02598 *                                                                *G7C2PGM 
02599 *-- VALIDATE ------ MULT PODIATRY PROCEDURE PRICING INDICATOR  --*G7C2PGM 
02600 *                                                                *G7C2PGM 
02601 ******************************************************************G7C2PGM 
02602 *                                                                 G7C2PGM 
02603 *   IF (MULT PODIATRY PROCEDURE PRICING INDICATOR IS NON-ZERO     G7C2PGM 
02604 *   THEN                                                          G7C2PGM 
02605 *      (MULT PODIATRY PROCEDURE LEVEL 1 PERCENT) MUST BE > ZERO   G7C2PGM 
02606 *        OR                                                       G7C2PGM 
02607 *      (MULT PODIATRY PROCEDURE LEVEL 2 PERCENT) MUST BE > ZERO   G7C2PGM 
02608 *        OR                                                       G7C2PGM 
02609 *      (MULT PODIATRY PROCEDURE LEVEL 3 PERCENT) MUST BE > ZERO   G7C2PGM 
02610 *        OR                                                       G7C2PGM 
02611 *      (MULT PODIATRY PROCEDURE LEVEL 4 PERCENT) MUST BE > ZERO   G7C2PGM 
02612                                                                   G7C2PGM 
02613      IF  S2MPPPIA = DFHBMUBF                                      G7C2PGM 
02614      THEN                                                         G7C2PGM 
02615          NEXT SENTENCE                                            G7C2PGM 
02616      ELSE                                                         G7C2PGM 
02617          IF  S2MPPPII = '0'                                       G7C2PGM 
02618          THEN                                                     G7C2PGM 
02619              NEXT SENTENCE                                        G7C2PGM 
02620          ELSE                                                     G7C2PGM 
02621              IF  S2PPLP1I = '000.00' AND                          G7C2PGM 
02622                  S2PPLP2I = '000.00' AND                          G7C2PGM 
02623                  S2PPLP3I = '000.00' AND                          G7C2PGM 
02624                  S2PPLP4I = '000.00'                              G7C2PGM 
02625              THEN                                                 G7C2PGM 
02626                  MOVE  -1        TO  S2MPPPIL                     G7C2PGM 
02627                  MOVE  DFHBMUBF  TO  S2MPPPIA                     G7C2PGM 
02628                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02629                  SET WT-01-INDEX TO +03                           G7C2PGM 
02630                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02631              ELSE                                                 G7C2PGM 
02632                  NEXT SENTENCE.                                   G7C2PGM 
02633                                                                   G7C2PGM 
02634 *-- VALIDATE ------ MULTIPLE PODIATRY PROCEDURE PRICING INDIC ---*G7C2PGM 
02635 *   1. ALPHANUMERIC                                               G7C2PGM 
02636 *   2. FIELD VALIDATION SUB-SYSTEM                                G7C2PGM 
02637                                                                   G7C2PGM 
02638      MOVE  S2MPPPII TO WS-02-CLASS-TEST-AREA.                     G7C2PGM 
02639      IF  WS-02-CLASS-ALPHANUMERIC(1)                              G7C2PGM 
02640      THEN                                                         G7C2PGM 
02641          MOVE  S2MPPPII TO GCVI-VALUE                             G7C2PGM 
02642          MOVE  'BPCC01' TO GCVI-FIELDS-KEY-ID                     G7C2PGM 
02643          PERFORM 2110-000-LINK-TO-GCVIOPGM                        G7C2PGM 
02644          IF  GCVI-VALUE-NOT-FOUND                                 G7C2PGM 
02645          THEN                                                     G7C2PGM 
02646              MOVE  -1        TO  S2MPPPIL                         G7C2PGM 
02647              MOVE  DFHBMUBF  TO  S2MPPPIA                         G7C2PGM 
02648              IF  WS-02-SCREEN-HAS-ERRORS                          G7C2PGM 
02649              THEN                                                 G7C2PGM 
02650                  NEXT SENTENCE                                    G7C2PGM 
02651              ELSE                                                 G7C2PGM 
02652                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02653                  SET WT-01-INDEX TO +09                           G7C2PGM 
02654                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02655          ELSE                                                     G7C2PGM 
02656              IF  GCVI-VALUE-NOT-LOADED                            G7C2PGM 
02657              THEN                                                 G7C2PGM 
02658                  MOVE  DFHBMUBF  TO  S2MPPPIA                     G7C2PGM 
02659              ELSE                                                 G7C2PGM 
02660                  NEXT SENTENCE                                    G7C2PGM 
02661      ELSE                                                         G7C2PGM 
02662          MOVE  -1        TO  S2MPPPIL                             G7C2PGM 
02663          MOVE  DFHBMUBF  TO  S2MPPPIA                             G7C2PGM 
02664          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02665          THEN                                                     G7C2PGM 
02666              NEXT SENTENCE                                        G7C2PGM 
02667          ELSE                                                     G7C2PGM 
02668              MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH         G7C2PGM 
02669              SET WT-01-INDEX TO +08                               G7C2PGM 
02670              PERFORM 9000-000-MOVE-MSG-TO-SCREEN.                 G7C2PGM 
02671                                                                   G7C2PGM 
02672                                                                   G7C2PGM 
02673 /*****************************************************************G7C2PGM 
02674 *                                                                *G7C2PGM 
02675 *-- VALIDATE ------ PRIMARY SURGEON DEPENDENCY PAYMENT PERCENT --*G7C2PGM 
02676 *                                                                *G7C2PGM 
02677 ******************************************************************G7C2PGM 
02678 *   D129                                                          G7C2PGM 
02679                                                                   G7C2PGM 
02680 *    MOVE S2PSDPPI TO D-C-RECEIVE-FIELD.                          G7C2PGM 
02681 *    MOVE +0 TO D-C-DECIMAL-POSITIONS.                            G7C2PGM 
02682 *    MOVE '00' TO D-C-RETURN-CODE.                                G7C2PGM 
02683 *    MOVE ZEROES TO D-C-RETURN-FIELD.                             G7C2PGM 
02684 *    PERFORM 2500-LINK-TO-GX3APGM THRU 2500-EXIT.                 G7C2PGM 
02685 *    IF D-C-RETURN-CODE = '00'                                    G7C2PGM 
02686 *        IF D-C-RETURN-FIELD > WS-3POS-MAX-AMT                    G7C2PGM 
02687 *            MOVE -1       TO S2PSDPPL                            G7C2PGM 
02688 *            MOVE DFHBMUBF TO S2PSDPPA                            G7C2PGM 
02689 *            IF WS-02-SCREEN-HAS-ERRORS                           G7C2PGM 
02690 *                NEXT SENTENCE                                    G7C2PGM 
02691 *            ELSE                                                 G7C2PGM 
02692 *                MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH            G7C2PGM 
02693 *                SET WT-01-INDEX TO +13                           G7C2PGM 
02694 *                PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02695 *        ELSE                                                     G7C2PGM 
02696 *            MOVE D-C-RETURN-FIELD                                G7C2PGM 
02697 *              TO WS-02-PRI-SUR-DEP-PAY-PCT                       G7C2PGM 
02698 *            MOVE WS-02-PRI-SUR-DEP-PAY-PCT                       G7C2PGM 
02699 *              TO WS-02-DISP-3POS-DEC                             G7C2PGM 
02700 *            MOVE WS-02-DISP-3POS-DEC                             G7C2PGM 
02701 *              TO S2PSDPPO                                        G7C2PGM 
02702 *    ELSE                                                         G7C2PGM 
02703 *        MOVE -1       TO S2PSDPPL                                G7C2PGM 
02704 *        MOVE DFHBMUBF TO S2PSDPPA                                G7C2PGM 
02705 *        IF WS-02-SCREEN-HAS-ERRORS                               G7C2PGM 
02706 *            NEXT SENTENCE                                        G7C2PGM 
02707 *        ELSE                                                     G7C2PGM 
02708 *            MOVE '1' TO WS-02-SCREEN-ERROR-SWITCH                G7C2PGM 
02709 *            IF D-C-RETURN-CODE = '10'                            G7C2PGM 
02710 *                SET WT-01-INDEX TO +10                           G7C2PGM 
02711 *                PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
02712 *            ELSE                                                 G7C2PGM 
02713 *                SET WT-01-INDEX TO +13                           G7C2PGM 
02714 *                PERFORM 9000-000-MOVE-MSG-TO-SCREEN.             G7C2PGM 
02715                                                                   G7C2PGM 
02716                                                                   G7C2PGM 
02717 *    MOVE S2PSDPPI TO   WS-02-PRI-SURG-DEP-INPUT-X.               G7C2PGM 
02718 *                                                                 G7C2PGM 
02719 *    INSPECT WS-02-PRI-SURG-DEP-INPUT-X REPLACING                 G7C2PGM 
02720 *       LEADING SPACES BY ZEROS.                                  G7C2PGM 
02721 *                                                                 G7C2PGM 
02722 *    IF ( WS-02-PRI-SURG-C1 IS NUMERIC  AND                       G7C2PGM 
02723 *         WS-02-PRI-SURG-C3 IS NUMERIC  AND                       G7C2PGM 
02724 *         WS-02-MULT-PCT-C4 EQUAL '.'  )                          G7C2PGM 
02725 *    THEN                                                         G7C2PGM 
02726 *        MOVE WS-02-PRI-SURG-C1       TO WS-02-SURG-F1            G7C2PGM 
02727 *        MOVE WS-02-PRI-SURG-C3       TO WS-02-SURG-L2            G7C2PGM 
02728 *        MOVE WS-02-PRI-SURG-DEPD-PCT TO                          G7C2PGM 
02729 *                 WS-02-PRI-SUR-DEP-PAY-PCT                       G7C2PGM 
02730 *    ELSE                                                         G7C2PGM 
02731 *        MOVE  -1        TO  S2PSDPPL                             G7C2PGM 
02732 *        MOVE  DFHBMUBF  TO  S2PSDPPA                             G7C2PGM 
02733 *        IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02734 *        THEN                                                     G7C2PGM 
02735 *            NEXT SENTENCE                                        G7C2PGM 
02736 *        ELSE                                                     G7C2PGM 
02737 *               MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02738 *               SET WT-01-INDEX TO +10                            G7C2PGM 
02739 *               PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
02740 *                                                                 G7C2PGM 
02741                                                                   G7C2PGM 
02742      MOVE S2PSDPPI TO   WS-02-PRI-SUR-DEP-PAY-PCT.                G7C2PGM 
02743                                                                   G7C2PGM 
02744      INSPECT WS-02-PRI-SUR-DEP-PAY-PCT REPLACING                  G7C2PGM 
02745         LEADING SPACES BY ZEROS.                                  G7C2PGM 
02746                                                                   G7C2PGM 
02747      IF  WS-02-PRI-SUR-DEP-PAY-PCT IS NUMERIC                     G7C2PGM 
02748      THEN                                                         G7C2PGM 
02749          MOVE WS-02-PRI-SUR-DEP-PAY-PCT TO                        G7C2PGM 
02750               WS-02-PRI-SURG-DEPD-PCT                             G7C2PGM 
02751          MOVE WS-02-PRI-SURG-DEPD-PCT TO                          G7C2PGM 
02752               S2PSDPPO                                            G7C2PGM 
02753      ELSE                                                         G7C2PGM 
02754          MOVE  -1        TO  S2PSDPPL                             G7C2PGM 
02755          MOVE  DFHBMUBF  TO  S2PSDPPA                             G7C2PGM 
02756          IF  WS-02-SCREEN-HAS-ERRORS                              G7C2PGM 
02757          THEN                                                     G7C2PGM 
02758              NEXT SENTENCE                                        G7C2PGM 
02759          ELSE                                                     G7C2PGM 
02760                 MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH      G7C2PGM 
02761                 SET WT-01-INDEX TO +10                            G7C2PGM 
02762                 PERFORM 9000-000-MOVE-MSG-TO-SCREEN.              G7C2PGM 
02763                                                                   G7C2PGM 
02764  2100-900-EXIT.                                                   G7C2PGM 
02765      EXIT.                                                        G7C2PGM 
02766 /***************************************************************  G7C2PGM 
02767 *                                                              *  G7C2PGM 
02768 * 2110  LINK TO FIELD VALIDATION MODULE (GCVIOPGM)             *  G7C2PGM 
02769 *                                                              *  G7C2PGM 
02770 ****************************************************************  G7C2PGM 
02771  2110-000-LINK-TO-GCVIOPGM      SECTION.                          G7C2PGM 
02772  2110-010.                                                        G7C2PGM 
02773                                                                   G7C2PGM 
02774      MOVE  ZEROES        TO  GCVI-RETURN-CODE.                    G7C2PGM 
02775                                                                   G7C2PGM 
02776      EXEC CICS  LINK  PROGRAM ('GCVIOPGM')                        G7C2PGM 
02777                       COMMAREA(GCVIOPGM-PARM-LIST)                G7C2PGM 
02778                       LENGTH  (WS-02-GCVI-PARM-AREA-LEN)          G7C2PGM 
02779                       END-EXEC.                                   G7C2PGM 
02780                                                                   G7C2PGM 
02781      IF  GCVI-VALUE-NOT-LOADED                                    G7C2PGM 
02782          MOVE GCVI-RETURN-CODE TO WS-02-GCVI-RETURN-CODE.         G7C2PGM 
02783                                                                   G7C2PGM 
02784  2110-900-EXIT.                                                   G7C2PGM 
02785      EXIT.                                                        G7C2PGM 
02786 /***************************************************************  G7C2PGM 
02787 *                                                              *  G7C2PGM 
02788 * 2200  DO SCREEN LOGICAL EDITS                                *  G7C2PGM 
02789 *                                                              *  G7C2PGM 
02790 ****************************************************************  G7C2PGM 
02791  2200-000-LOGICAL-EDITS         SECTION.                          G7C2PGM 
02792  2200-010.                                                        G7C2PGM 
02793                                                                   G7C2PGM 
02794 *----------------------------------------------------------------*G7C2PGM 
02795 *                                                                *G7C2PGM 
02796 *                                                                *G7C2PGM 
02797 *   THIS PROGRAM HAS NO REQUIREMENT FOR LOGICAL EDITS            *G7C2PGM 
02798 *                                                                *G7C2PGM 
02799 *                                                                *G7C2PGM 
02800 *----------------------------------------------------------------*G7C2PGM 
02801                                                                   G7C2PGM 
02802                                                                   G7C2PGM 
02803 *------------- CHECK FOR EMPTY EDIT TABLE -----------------------*G7C2PGM 
02804                                                                   G7C2PGM 
02805      IF  WS-02-SCREEN-HAS-ERRORS                                  G7C2PGM 
02806      THEN                                                         G7C2PGM 
02807          NEXT SENTENCE                                            G7C2PGM 
02808      ELSE                                                         G7C2PGM 
02809          IF  WS-02-GCVI-VALUE-NOT-LOADED                          G7C2PGM 
02810          THEN                                                     G7C2PGM 
02811              IF EIBAID = DFHPF4 OR DFHPF16                        G7C2PGM 
02812              THEN                                                 G7C2PGM 
02813                  NEXT SENTENCE                                    G7C2PGM 
02814              ELSE                                                 G7C2PGM 
02815                  MOVE  -1        TO S2ERRL                        G7C2PGM 
02816                  MOVE '1'        TO WS-02-SCREEN-ERROR-SWITCH     G7C2PGM 
02817                  SET WT-01-INDEX TO +06                           G7C2PGM 
02818                  PERFORM 9000-000-MOVE-MSG-TO-SCREEN              G7C2PGM 
02819          ELSE                                                     G7C2PGM 
02820              NEXT SENTENCE.                                       G7C2PGM 
02821                                                                   G7C2PGM 
02822                                                                   G7C2PGM 
02823  2200-900-EXIT.                                                   G7C2PGM 
02824      EXIT.                                                        G7C2PGM 
02825 /***************************************************************  G7C2PGM 
02826 *                                                              *  G7C2PGM 
02827 * 2300  APPLY ANY CHANGES TO BENEFIT PROVISION RECORD AND      *  G7C2PGM 
02828 *        REWRITE TO WORKFILE.                                  *  G7C2PGM 
02829 *                                                              *  G7C2PGM 
02830 ****************************************************************  G7C2PGM 
02831  2300-000-APPLY-RECORD-CHANGES.                                   G7C2PGM 
02832                                                                   G7C2PGM 
02833 *----- READ WORKFILE BENEFIT PROVISION RECORD -------------------*G7C2PGM 
02834                                                                   G7C2PGM 
02835      PERFORM 2310-000-BUILD-BEN-PROV-KEY.                         G7C2PGM 
02836      MOVE GC-GCBENPRV-VARY-MAX-OCUR                               G7C2PGM 
02837        TO GCP2-COUNT-TAB-PROVN-POINTERS.                          G7C2PGM 
02838      MOVE 'RD '  TO  GCIO2-FILE-ACCESS-CODE.                      G7C2PGM 
02839      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7C2PGM 
02840      IF  NOT GCIO2-GOOD-RETURN                                    G7C2PGM 
02841          MOVE WS-01-ABCODE-C2F2     TO WS-01-ABCODE               G7C2PGM 
02842          MOVE WS-01-ABCODE-C2F2-MSG TO WS-01-ABCODE-MSG           G7C2PGM 
02843          PERFORM  9999-000-ABEND-THE-TASK.                        G7C2PGM 
02844                                                                   G7C2PGM 
02845 *----- DETERMINE IF ANY CHANGES HAVE BEEN MADE TO FIELDS --------*G7C2PGM 
02846                                                                   G7C2PGM 
02847      MOVE GPC2-MULT-UNRL-PROC-1-PCT TO                            G7C2PGM 
02848        WS-GPC2-MULT-UNRL-PROC-1-PCT.                              G7C2PGM 
02849                                                                   G7C2PGM 
02850      MOVE GPC2-MULT-UNRL-PROC-2-PCT TO                            G7C2PGM 
02851        WS-GPC2-MULT-UNRL-PROC-2-PCT.                              G7C2PGM 
02852                                                                   G7C2PGM 
02853      MOVE GPC2-MULT-UNRL-PROC-3-PCT TO                            G7C2PGM 
02854        WS-GPC2-MULT-UNRL-PROC-3-PCT.                              G7C2PGM 
02855                                                                   G7C2PGM 
02856 ************************************************                  G7C2PGM 
02857                                                                   G7C2PGM 
02858      MOVE GPC2-MULT-RL-PROC-1-PCT TO                              G7C2PGM 
02859        WS-GPC2-MULT-RL-PROC-1-PCT.                                G7C2PGM 
02860                                                                   G7C2PGM 
02861      MOVE GPC2-MULT-RL-PROC-2-PCT TO                              G7C2PGM 
02862        WS-GPC2-MULT-RL-PROC-2-PCT.                                G7C2PGM 
02863                                                                   G7C2PGM 
02864      MOVE GPC2-MULT-RL-PROC-3-PCT TO                              G7C2PGM 
02865        WS-GPC2-MULT-RL-PROC-3-PCT.                                G7C2PGM 
02866                                                                   G7C2PGM 
02867 ************************************************                  G7C2PGM 
02868                                                                   G7C2PGM 
02869      MOVE GPC2-MULT-INJ-LVL-1-PCT TO                              G7C2PGM 
02870        WS-GPC2-MULT-INJ-LVL-1-PCT.                                G7C2PGM 
02871                                                                   G7C2PGM 
02872      MOVE GPC2-MULT-INJ-LVL-2-PCT TO                              G7C2PGM 
02873        WS-GPC2-MULT-INJ-LVL-2-PCT.                                G7C2PGM 
02874                                                                   G7C2PGM 
02875      MOVE GPC2-MULT-INJ-LVL-3-PCT TO                              G7C2PGM 
02876        WS-GPC2-MULT-INJ-LVL-3-PCT.                                G7C2PGM 
02877                                                                   G7C2PGM 
02878 ************************************************                  G7C2PGM 
02879                                                                   G7C2PGM 
02880      MOVE GPC2-MULT-POD-PROC-1-PCT TO                             G7C2PGM 
02881        WS-GPC2-MULT-POD-PROC-1-PCT.                               G7C2PGM 
02882                                                                   G7C2PGM 
02883      MOVE GPC2-MULT-POD-PROC-2-PCT TO                             G7C2PGM 
02884        WS-GPC2-MULT-POD-PROC-2-PCT.                               G7C2PGM 
02885                                                                   G7C2PGM 
02886      MOVE GPC2-MULT-POD-PROC-3-PCT TO                             G7C2PGM 
02887        WS-GPC2-MULT-POD-PROC-3-PCT.                               G7C2PGM 
02888                                                                   G7C2PGM 
02889      MOVE GPC2-MULT-POD-PROC-4-PCT TO                             G7C2PGM 
02890        WS-GPC2-MULT-POD-PROC-4-PCT.                               G7C2PGM 
02891                                                                   G7C2PGM 
02892 ************************************************                  G7C2PGM 
02893                                                                   G7C2PGM 
02894      MOVE GPC2-PRIM-SURG-DPD-PAY-PCT TO                           G7C2PGM 
02895        WS-GPC2-PRIM-SURG-DPD-PAY-PCT.                             G7C2PGM 
02896                                                                   G7C2PGM 
02897 * =======>  MULT UNRELATED PROCEDURE LEVEL PERCENT                G7C2PGM 
02898      IF WS-02-MULT-UNR-PROC-PCT-L1 =                              G7C2PGM 
02899                                   WS-GPC2-MULT-UNRL-PROC-1-PCT ANDG7C2PGM 
02900         WS-02-MULT-UNR-PROC-PCT-L2 =                              G7C2PGM 
02901                                   WS-GPC2-MULT-UNRL-PROC-2-PCT ANDG7C2PGM 
02902         WS-02-MULT-UNR-PROC-PCT-L3 =                              G7C2PGM 
02903                                   WS-GPC2-MULT-UNRL-PROC-3-PCT    G7C2PGM 
02904         NEXT SENTENCE                                             G7C2PGM 
02905      ELSE                                                         G7C2PGM 
02906         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02907                                                                   G7C2PGM 
02908 * =======>  MULT UNRELATED NUMBER OF OCCURRANCES                  G7C2PGM 
02909      IF S2UNOC1I =   GPC2-MULT-UNRL-1-NO-OCCUR   AND              G7C2PGM 
02910         S2UNOC2I =   GPC2-MULT-UNRL-2-NO-OCCUR   AND              G7C2PGM 
02911         S2UNOC3I =   GPC2-MULT-UNRL-3-NO-OCCUR                    G7C2PGM 
02912         NEXT SENTENCE                                             G7C2PGM 
02913      ELSE                                                         G7C2PGM 
02914         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02915                                                                   G7C2PGM 
02916 * =======>  MULT RELATED PROCEDURE LEVEL PERCENT                  G7C2PGM 
02917      IF WS-02-MULT-REL-PROC-PCT-L1 =                              G7C2PGM 
02918                                   WS-GPC2-MULT-RL-PROC-1-PCT AND  G7C2PGM 
02919         WS-02-MULT-REL-PROC-PCT-L2 =                              G7C2PGM 
02920                                   WS-GPC2-MULT-RL-PROC-2-PCT AND  G7C2PGM 
02921         WS-02-MULT-REL-PROC-PCT-L3 =                              G7C2PGM 
02922                                   WS-GPC2-MULT-RL-PROC-3-PCT      G7C2PGM 
02923         NEXT SENTENCE                                             G7C2PGM 
02924      ELSE                                                         G7C2PGM 
02925         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02926                                                                   G7C2PGM 
02927 * =======>  MULT RELATED NUMBER OF OCCURRANCES                    G7C2PGM 
02928      IF S2RNOC1I =   GPC2-MULT-RL-1-NO-OCCUR   AND                G7C2PGM 
02929         S2RNOC2I =   GPC2-MULT-RL-2-NO-OCCUR   AND                G7C2PGM 
02930         S2RNOC3I =   GPC2-MULT-RL-3-NO-OCCUR                      G7C2PGM 
02931         NEXT SENTENCE                                             G7C2PGM 
02932      ELSE                                                         G7C2PGM 
02933         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02934                                                                   G7C2PGM 
02935 * =======>  MULT INJURY PROCEDURE LEVEL PERCENT                   G7C2PGM 
02936      IF WS-02-MULT-INJ-LVL-PCT-L1 =                               G7C2PGM 
02937                                  WS-GPC2-MULT-INJ-LVL-1-PCT AND   G7C2PGM 
02938         WS-02-MULT-INJ-LVL-PCT-L2 =                               G7C2PGM 
02939                                  WS-GPC2-MULT-INJ-LVL-2-PCT AND   G7C2PGM 
02940         WS-02-MULT-INJ-LVL-PCT-L3 =                               G7C2PGM 
02941                                  WS-GPC2-MULT-INJ-LVL-3-PCT       G7C2PGM 
02942         NEXT SENTENCE                                             G7C2PGM 
02943      ELSE                                                         G7C2PGM 
02944         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02945                                                                   G7C2PGM 
02946 * =======>  MULT INJURY NUMBER OF OCCURRANCES                     G7C2PGM 
02947      IF S2INOC1I =   GPC2-MULT-INJ-1-NO-OCCUR  AND                G7C2PGM 
02948         S2INOC2I =   GPC2-MULT-INJ-2-NO-OCCUR  AND                G7C2PGM 
02949         S2INOC3I =   GPC2-MULT-INJ-3-NO-OCCUR                     G7C2PGM 
02950         NEXT SENTENCE                                             G7C2PGM 
02951      ELSE                                                         G7C2PGM 
02952         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02953                                                                   G7C2PGM 
02954 * =======>  MULT PODIATRY PROCEDURE LEVEL PERCENT                 G7C2PGM 
02955      IF WS-02-MULT-POD-PROC-PCT-L1 =                              G7C2PGM 
02956                                   WS-GPC2-MULT-POD-PROC-1-PCT AND G7C2PGM 
02957         WS-02-MULT-POD-PROC-PCT-L2 =                              G7C2PGM 
02958                                   WS-GPC2-MULT-POD-PROC-2-PCT AND G7C2PGM 
02959         WS-02-MULT-POD-PROC-PCT-L3 =                              G7C2PGM 
02960                                   WS-GPC2-MULT-POD-PROC-3-PCT AND G7C2PGM 
02961         WS-02-MULT-POD-PROC-PCT-L4 =                              G7C2PGM 
02962                                   WS-GPC2-MULT-POD-PROC-4-PCT     G7C2PGM 
02963         NEXT SENTENCE                                             G7C2PGM 
02964      ELSE                                                         G7C2PGM 
02965         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02966                                                                   G7C2PGM 
02967 * =======>  MULT PODIATRY NUMBER OF OCCURRANCES                   G7C2PGM 
02968      IF S2PNOC1I =   GPC2-MULT-POD-1-NO-OCCUR  AND                G7C2PGM 
02969         S2PNOC2I =   GPC2-MULT-POD-2-NO-OCCUR  AND                G7C2PGM 
02970         S2PNOC3I =   GPC2-MULT-POD-3-NO-OCCUR  AND                G7C2PGM 
02971         S2PNOC4I =   GPC2-MULT-POD-4-NO-OCCUR                     G7C2PGM 
02972         NEXT SENTENCE                                             G7C2PGM 
02973      ELSE                                                         G7C2PGM 
02974         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02975                                                                   G7C2PGM 
02976 * =======>  MULT PODIATRY PRICING INDICATOR                       G7C2PGM 
02977      IF S2MPPPII =   GPC2-MULT-POD-PROC-PRICE-IND                 G7C2PGM 
02978         NEXT SENTENCE                                             G7C2PGM 
02979      ELSE                                                         G7C2PGM 
02980         GO TO 2300-100-WRITE-UPDATED-RECD.                        G7C2PGM 
02981                                                                   G7C2PGM 
02982 * =======>  PRIMARY SURGEON DEPENDENCY PAYMENT PERCENT            G7C2PGM 
02983      IF WS-02-PRI-SUR-DEP-PAY-PCT  =                              G7C2PGM 
02984                                   WS-GPC2-PRIM-SURG-DPD-PAY-PCT   G7C2PGM 
02985         GO TO 2300-900-EXIT.                                      G7C2PGM 
02986                                                                   G7C2PGM 
02987 /**************************************************************** G7C2PGM 
02988 *                                                                 G7C2PGM 
02989  2300-100-WRITE-UPDATED-RECD.                                     G7C2PGM 
02990 *                                                                 G7C2PGM 
02991 ***************************************************************** G7C2PGM 
02992 *----- READ WORKFILE BENEFIT PROVISION RECORD FOR UPDATE --------*G7C2PGM 
02993                                                                   G7C2PGM 
02994      MOVE 'RU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7C2PGM 
02995      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7C2PGM 
02996      IF  NOT GCIO2-GOOD-RETURN                                    G7C2PGM 
02997          MOVE WS-01-ABCODE-C2F3     TO WS-01-ABCODE               G7C2PGM 
02998          MOVE WS-01-ABCODE-C2F3-MSG TO WS-01-ABCODE-MSG           G7C2PGM 
02999          PERFORM  9999-000-ABEND-THE-TASK.                        G7C2PGM 
03000                                                                   G7C2PGM 
03001                                                                   G7C2PGM 
03002 *----- UPDATE BENEFIT PROVISION RECORD CHANGED FIELDS -----------*G7C2PGM 
03003                                                                   G7C2PGM 
03004 * =======>  MULT UNRELATED PROCEDURE LEVEL PERCENT                G7C2PGM 
03005      MOVE   WS-02-MULT-UNR-PROC-PCT-L1      TO                    G7C2PGM 
03006                   GPC2-MULT-UNRL-PROC-1-PCT.                      G7C2PGM 
03007      MOVE   WS-02-MULT-UNR-PROC-PCT-L2      TO                    G7C2PGM 
03008                   GPC2-MULT-UNRL-PROC-2-PCT.                      G7C2PGM 
03009      MOVE   WS-02-MULT-UNR-PROC-PCT-L3      TO                    G7C2PGM 
03010                   GPC2-MULT-UNRL-PROC-3-PCT.                      G7C2PGM 
03011                                                                   G7C2PGM 
03012 * =======>  MULT UNRELATED NUMBER OF OCCURRANCES                  G7C2PGM 
03013      MOVE S2UNOC1I TO   GPC2-MULT-UNRL-1-NO-OCCUR.                G7C2PGM 
03014      MOVE S2UNOC2I TO   GPC2-MULT-UNRL-2-NO-OCCUR.                G7C2PGM 
03015      MOVE S2UNOC3I TO   GPC2-MULT-UNRL-3-NO-OCCUR.                G7C2PGM 
03016                                                                   G7C2PGM 
03017 * =======>  MULT RELATED PROCEDURE LEVEL PERCENT                  G7C2PGM 
03018      MOVE   WS-02-MULT-REL-PROC-PCT-L1      TO                    G7C2PGM 
03019                   GPC2-MULT-RL-PROC-1-PCT.                        G7C2PGM 
03020      MOVE   WS-02-MULT-REL-PROC-PCT-L2      TO                    G7C2PGM 
03021                   GPC2-MULT-RL-PROC-2-PCT.                        G7C2PGM 
03022      MOVE   WS-02-MULT-REL-PROC-PCT-L3      TO                    G7C2PGM 
03023                   GPC2-MULT-RL-PROC-3-PCT.                        G7C2PGM 
03024                                                                   G7C2PGM 
03025 * =======>  MULT RELATED NUMBER OF OCCURRANCES                    G7C2PGM 
03026      MOVE S2RNOC1I TO  GPC2-MULT-RL-1-NO-OCCUR.                   G7C2PGM 
03027      MOVE S2RNOC2I TO  GPC2-MULT-RL-2-NO-OCCUR.                   G7C2PGM 
03028      MOVE S2RNOC3I TO  GPC2-MULT-RL-3-NO-OCCUR.                   G7C2PGM 
03029                                                                   G7C2PGM 
03030 * =======>  MULT INJURY PROCEDURE LEVEL PERCENT                   G7C2PGM 
03031      MOVE   WS-02-MULT-INJ-LVL-PCT-L1      TO                     G7C2PGM 
03032                   GPC2-MULT-INJ-LVL-1-PCT.                        G7C2PGM 
03033      MOVE   WS-02-MULT-INJ-LVL-PCT-L2      TO                     G7C2PGM 
03034                   GPC2-MULT-INJ-LVL-2-PCT.                        G7C2PGM 
03035      MOVE   WS-02-MULT-INJ-LVL-PCT-L3      TO                     G7C2PGM 
03036                   GPC2-MULT-INJ-LVL-3-PCT.                        G7C2PGM 
03037                                                                   G7C2PGM 
03038 * =======>  MULT INJURY NUMBER OF OCCURRANCES                     G7C2PGM 
03039      MOVE S2INOC1I TO  GPC2-MULT-INJ-1-NO-OCCUR.                  G7C2PGM 
03040      MOVE S2INOC2I TO  GPC2-MULT-INJ-2-NO-OCCUR.                  G7C2PGM 
03041      MOVE S2INOC3I TO  GPC2-MULT-INJ-3-NO-OCCUR.                  G7C2PGM 
03042                                                                   G7C2PGM 
03043 * =======>  MULT PODIATRY PROCEDURE LEVEL PERCENT                 G7C2PGM 
03044      MOVE   WS-02-MULT-POD-PROC-PCT-L1      TO                    G7C2PGM 
03045                   GPC2-MULT-POD-PROC-1-PCT.                       G7C2PGM 
03046      MOVE   WS-02-MULT-POD-PROC-PCT-L2      TO                    G7C2PGM 
03047                   GPC2-MULT-POD-PROC-2-PCT.                       G7C2PGM 
03048      MOVE   WS-02-MULT-POD-PROC-PCT-L3      TO                    G7C2PGM 
03049                   GPC2-MULT-POD-PROC-3-PCT.                       G7C2PGM 
03050      MOVE   WS-02-MULT-POD-PROC-PCT-L4      TO                    G7C2PGM 
03051                   GPC2-MULT-POD-PROC-4-PCT.                       G7C2PGM 
03052                                                                   G7C2PGM 
03053 * =======>  MULT PODIATRY NUMBER OF OCCURRANCES                   G7C2PGM 
03054      MOVE S2PNOC1I TO  GPC2-MULT-POD-1-NO-OCCUR.                  G7C2PGM 
03055      MOVE S2PNOC2I TO  GPC2-MULT-POD-2-NO-OCCUR.                  G7C2PGM 
03056      MOVE S2PNOC3I TO  GPC2-MULT-POD-3-NO-OCCUR.                  G7C2PGM 
03057      MOVE S2PNOC4I TO  GPC2-MULT-POD-4-NO-OCCUR.                  G7C2PGM 
03058                                                                   G7C2PGM 
03059 * =======>  MULT PODIATRY PRICING INDICATOR                       G7C2PGM 
03060      MOVE S2MPPPII TO GPC2-MULT-POD-PROC-PRICE-IND.               G7C2PGM 
03061                                                                   G7C2PGM 
03062 * =======>  PRIMARY SURGEON DEPENDENCY PAYMENT PERCENT            G7C2PGM 
03063      MOVE WS-02-PRI-SUR-DEP-PAY-PCT   TO                          G7C2PGM 
03064            GPC2-PRIM-SURG-DPD-PAY-PCT.                            G7C2PGM 
03065                                                                   G7C2PGM 
03066 *----- REWRITE WORKFILE BENEFIT PROVISION RECORD ----------------*G7C2PGM 
03067                                                                   G7C2PGM 
03068 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          G7C2PGM 
03069                                                                   G7C2PGM 
03070      MOVE '1'    TO  GCIO2-OPER-ID-IND.                           G7C2PGM 
03071      MOVE 'WU '  TO  GCIO2-FILE-ACCESS-CODE.                      G7C2PGM 
03072      PERFORM  5000-000-W-F-BEN-PROV-IO.                           G7C2PGM 
03073      IF  NOT GCIO2-GOOD-RETURN                                    G7C2PGM 
03074          MOVE WS-01-ABCODE-C2F4     TO WS-01-ABCODE               G7C2PGM 
03075          MOVE WS-01-ABCODE-C2F4-MSG TO WS-01-ABCODE-MSG           G7C2PGM 
03076          PERFORM  9999-000-ABEND-THE-TASK.                        G7C2PGM 
03077                                                                   G7C2PGM 
03078  2300-900-EXIT.                                                   G7C2PGM 
03079      EXIT.                                                        G7C2PGM 
03080 /***************************************************************  G7C2PGM 
03081 *                                                              *  G7C2PGM 
03082 * 2310  BUILD WORKFILE BENEFIT PROVISION GCIOPARM AREA         *  G7C2PGM 
03083 *                                                              *  G7C2PGM 
03084 ****************************************************************  G7C2PGM 
03085  2310-000-BUILD-BEN-PROV-KEY    SECTION.                          G7C2PGM 
03086  2310-010.                                                        G7C2PGM 
03087                                                                   G7C2PGM 
03088                                                                   G7C2PGM 
03089 *----- ACQUIRE STORAGE FOR W/F BEN PROV RECORD ------------------*G7C2PGM 
03090                                                                   G7C2PGM 
03091      COMPUTE WS-02-W-F-GCBENPRV-MAX-LEN = GC-GCIOPARM-LEN         G7C2PGM 
03092                                         + GC-WORKFILE-KEY-LEN     G7C2PGM 
03093                                         + GC-GCBENPRV-MAX-REC-LEN.G7C2PGM 
03094                                                                   G7C2PGM 
03095      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-BEN-PROV-AREA)    G7C2PGM 
03096                          INITIMG(WS-02-HEX-00)                    G7C2PGM 
03097                          LENGTH (WS-02-W-F-GCBENPRV-MAX-LEN)      G7C2PGM 
03098                          END-EXEC.                                G7C2PGM 
03099                                                                   G7C2PGM 
03100 *    SERVICE RELOAD  IO-PARM-BEN-PROV-AREA.                       G7C2PGM 
03101                                                                   G7C2PGM 
03102 *----- BUILD GCIOPARM AREA FOR WORKFILE BENEFIT PROVISION RECORD *G7C2PGM 
03103                                                                   G7C2PGM 
03104      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           G7C2PGM 
03105      MOVE SPACES                 TO GCIO-CONTRACT-FILE-KEY.       G7C2PGM 
03106                                                                   G7C2PGM 
03107      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7C2PGM 
03108      MOVE WRK-PLAN-CODE          TO GCIO-WRK-PLAN-CODE.           G7C2PGM 
03109      MOVE WRK-GROUP-NO-1-3       TO GCIO-WRK-GROUP-NO-1-3.        G7C2PGM 
03110      MOVE WRK-SEC-NO-1           TO GCIO-WRK-SEC-NO-1.            G7C2PGM 
03111      MOVE WRK-PKG-CODE           TO GCIO-WRK-PKG-CODE.            G7C2PGM 
03112      MOVE WRK-EFFECTIVE-DATE     TO GCIO-WRK-EFFECTIVE-DT.        G7C2PGM 
03113      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7C2PGM 
03114      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7C2PGM 
03115      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7C2PGM 
03116      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7C2PGM 
03117      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7C2PGM 
03118      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7C2PGM 
03119      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7C2PGM 
03120                                                                   G7C2PGM 
03121 *    MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7C2PGM 
03122 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7C2PGM 
03123 *    IF  HGADATE-RETURN = ZEROS                                   G7C2PGM 
03124 *    THEN                                                         G7C2PGM 
03125 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7C2PGM 
03126 *    ELSE                                                         G7C2PGM 
03127 *        SET WT-01-INDEX TO +07                                   G7C2PGM 
03128 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7C2PGM 
03129 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7C2PGM 
03130                                                                   G7C2PGM 
03131      MOVE 'C4'                   TO  GCIO-WRK-RECORD-TYPE.        G7C2PGM 
03132      MOVE S2BPVIDI               TO  GCIO-WRK-PROVISION-ID.       G7C2PGM 
03133      MOVE +9999999               TO  GCIO-WRK-PROVISION-SLOT-NO.  G7C2PGM 
03134      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7C2PGM 
03135      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7C2PGM 
03136      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              G7C2PGM 
03137      MOVE '1'                    TO  GCIO2-IO-AREA-TO-USE.        G7C2PGM 
03138                                                                   G7C2PGM 
03139                                                                   G7C2PGM 
03140  2310-900-EXIT.                                                   G7C2PGM 
03141      EXIT.                                                        G7C2PGM 
03142 /***************************************************************  G7C2PGM 
03143 *                                                              *  G7C2PGM 
03144 * 2400  PASS CONTROL TO NEXT SCREEN PROGRAM                    *  G7C2PGM 
03145 *                                                              *  G7C2PGM 
03146 ****************************************************************  G7C2PGM 
03147  2400-000-XCTL-TO-NEXT-PGM      SECTION.                          G7C2PGM 
03148  2400-010.                                                        G7C2PGM 
03149                                                                   G7C2PGM 
03150 *    PF7 = PREVIOUS SCREEN                                        G7C2PGM 
03151      IF  EIBAID = DFHPF7  OR DFHPF19                              G7C2PGM 
03152      THEN                                                         G7C2PGM 
03153          MOVE 'G7C1PGM' TO WS-02-NEXT-PROGRAM.                    G7C2PGM 
03154                                                                   G7C2PGM 
03155 *    PF8/ PF4/  ENTER = NEXT SCREEN                               G7C2PGM 
03156      IF  EIBAID = DFHENTER OR                                     G7C2PGM 
03157                   DFHPF4   OR DFHPF16 OR                          G7C2PGM 
03158                   DFHPF8   OR DFHPF20                             G7C2PGM 
03159      THEN                                                         G7C2PGM 
03160          MOVE 'GC6APGM' TO WS-02-NEXT-PROGRAM.                    G7C2PGM 
03161                                                                   G7C2PGM 
03162 *    PF6  BENEFIT PROVISION TABULAR MENU                          G7C2PGM 
03163      IF  EIBAID = DFHPF6  OR DFHPF18                              G7C2PGM 
03164      THEN                                                         G7C2PGM 
03165          MOVE 'GC8APGM' TO WS-02-NEXT-PROGRAM.                    G7C2PGM 
03166                                                                   G7C2PGM 
03167                                                                   G7C2PGM 
03168      EXEC CICS  XCTL  PROGRAM (WS-02-NEXT-PROGRAM)                G7C2PGM 
03169                       COMMAREA(WORK-RECORD-2)                     G7C2PGM 
03170                       LENGTH  (GCIO2-RECORD-LENGTH)               G7C2PGM 
03171                       END-EXEC.                                   G7C2PGM 
03172                                                                   G7C2PGM 
03173  2400-900-EXIT.                                                   G7C2PGM 
03174      EXIT.                                                        G7C2PGM 
03175 /***************************************************************  G7C2PGM 
03176 *                                                              *  G7C2PGM 
03177 * 2500   LINK TO GX3APGM FOR CONVERSION                           G7C2PGM 
03178 *                                                              *  G7C2PGM 
03179 ****************************************************************  G7C2PGM 
03180  2500-LINK-TO-GX3APGM.                                            G7C2PGM 
03181                                                                   G7C2PGM 
03182      EXEC CICS  LINK  PROGRAM ('GX3APGM')                         G7C2PGM 
03183                       COMMAREA(WS-DECIMAL-CONVERT-COMMAREA)       G7C2PGM 
03184                       LENGTH  (+51)                               G7C2PGM 
03185                       END-EXEC.                                   G7C2PGM 
03186  2500-EXIT.                                                       G7C2PGM 
03187      EXIT.                                                        G7C2PGM 
03188 /***************************************************************  G7C2PGM 
03189 *                                                              *  G7C2PGM 
03190 * 5000   CALL IO MODULE TO READ OR UPDATE WORKFILE BENEFIT     *  G7C2PGM 
03191 *         PROVISION RECORD (TYPE=C4)                           *  G7C2PGM 
03192 *                                                              *  G7C2PGM 
03193 ****************************************************************  G7C2PGM 
03194  5000-000-W-F-BEN-PROV-IO       SECTION.                          G7C2PGM 
03195  5000-010.                                                        G7C2PGM 
03196                                                                   G7C2PGM 
03197      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7C2PGM 
03198                       COMMAREA(IO-PARM-BEN-PROV-AREA)             G7C2PGM 
03199                       LENGTH  (WS-02-W-F-GCBENPRV-MAX-LEN)        G7C2PGM 
03200                       END-EXEC.                                   G7C2PGM 
03201                                                                   G7C2PGM 
03202                                                                   G7C2PGM 
03203  5000-900-EXIT.                                                   G7C2PGM 
03204      EXIT.                                                        G7C2PGM 
03205 /***************************************************************  G7C2PGM 
03206 *                                                              *  G7C2PGM 
03207 * 5100                                                         *  G7C2PGM 
03208 *    CALL IO MODULE TO READ WORKFILE CONTRACT RECORD (TYPE=C2) *  G7C2PGM 
03209 *                                                              *  G7C2PGM 
03210 ****************************************************************  G7C2PGM 
03211  5100-000-W-F-CONTRACT-IO       SECTION.                          G7C2PGM 
03212  5100-010.                                                        G7C2PGM 
03213                                                                   G7C2PGM 
03214      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         G7C2PGM 
03215                       COMMAREA(IO-PARM-CONTRACT-AREA)             G7C2PGM 
03216                       LENGTH  (WS-02-W-F-GCCONTR-MAX-LEN)         G7C2PGM 
03217                       END-EXEC.                                   G7C2PGM 
03218                                                                   G7C2PGM 
03219                                                                   G7C2PGM 
03220  5100-900-EXIT.                                                   G7C2PGM 
03221      EXIT.                                                        G7C2PGM 
03222 /***************************************************************  G7C2PGM 
03223 *                                                              *  G7C2PGM 
03224 * 9000   MOVE MESSAGE TO SCREEN                                *  G7C2PGM 
03225 *                                                              *  G7C2PGM 
03226 ****************************************************************  G7C2PGM 
03227  9000-000-MOVE-MSG-TO-SCREEN    SECTION.                          G7C2PGM 
03228  9000-010.                                                        G7C2PGM 
03229                                                                   G7C2PGM 
03230      MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO S2ERRO.              G7C2PGM 
03231                                                                   G7C2PGM 
03232  9000-900-EXIT.                                                   G7C2PGM 
03233      EXIT.                                                        G7C2PGM 
03234 /***************************************************************  G7C2PGM 
03235 *                                                              *  G7C2PGM 
03236 * 9100 SEND SCREEN AND RETURN                                  *  G7C2PGM 
03237 *                                                              *  G7C2PGM 
03238 ****************************************************************  G7C2PGM 
03239  9100-000-SEND-THEN-RETURN      SECTION.                          G7C2PGM 
03240  9100-010.                                                        G7C2PGM 
03241                                                                   G7C2PGM 
03242                                                                   G7C2PGM 
03243 *--- SET FAILSAFE CURSOR POSITION TO AVOID POSSIBLE PROG402.      G7C2PGM 
03244      MOVE  -1 TO  S2ERRL.                                         G7C2PGM 
03245                                                                   G7C2PGM 
03246                                                                   G7C2PGM 
03247      IF  WS-02-MY-EIBTRNID                                        G7C2PGM 
03248      THEN                                                         G7C2PGM 
03249          EXEC CICS  SEND MAP   ('G7C2I01')                        G7C2PGM 
03250                          MAPSET('G7C2SET')                        G7C2PGM 
03251                          DATAONLY                                 G7C2PGM 
03252                          CURSOR                                   G7C2PGM 
03253                          END-EXEC                                 G7C2PGM 
03254      ELSE                                                         G7C2PGM 
03255          EXEC CICS  SEND MAP   ('G7C2I01')                        G7C2PGM 
03256                          MAPSET('G7C2SET')                        G7C2PGM 
03257                          ERASE                                    G7C2PGM 
03258                          CURSOR                                   G7C2PGM 
03259                          END-EXEC.                                G7C2PGM 
03260                                                                   G7C2PGM 
03261      EXEC CICS RETURN                                             G7C2PGM 
03262                TRANSID  ('G7C2')                                  G7C2PGM 
03263                COMMAREA (DFHCOMMAREA)                             G7C2PGM 
03264                LENGTH   (LENGTH OF DFHCOMMAREA)                   G7C2PGM 
03265                END-EXEC.                                          G7C2PGM 
03266                                                                   G7C2PGM 
03267 *    EXEC CICS  RETURN                                            G7C2PGM 
03268 *               END-EXEC.                                         G7C2PGM 
03269 *                                                                 G7C2PGM 
03270 *                                                                 G7C2PGM 
03271  9100-900-EXIT.                                                   G7C2PGM 
03272      EXIT.                                                        G7C2PGM 
03273 /*****************************************************************G7C2PGM 
03274 *                                                                *G7C2PGM 
03275 * 9200    XCTL TO GCPSPGM                                        *G7C2PGM 
03276 *                                                                *G7C2PGM 
03277 *                                                                *G7C2PGM 
03278 ******************************************************************G7C2PGM 
03279  9200-000-XCTL-TO-GCPSPGM       SECTION.                          G7C2PGM 
03280  9200-010.                                                        G7C2PGM 
03281                                                                   G7C2PGM 
03282      EXEC CICS  XCTL  PROGRAM('GCPSPGM')                          G7C2PGM 
03283                       END-EXEC.                                   G7C2PGM 
03284                                                                   G7C2PGM 
03285  9200-900-EXIT.                                                   G7C2PGM 
03286      EXIT.                                                        G7C2PGM 
03287 /*****************************************************************G7C2PGM 
03288 *                                                                *G7C2PGM 
03289 * 9210    XCTL TO PREVIOUS MENU (EITHER GC5A OR GPM1)            *G7C2PGM 
03290 *                                                                *G7C2PGM 
03291 *                                                                *G7C2PGM 
03292 ******************************************************************G7C2PGM 
03293  9210-000-XCTL-TO-PREVIOUS-MENU SECTION.                          G7C2PGM 
03294  9210-010.                                                        G7C2PGM 
03295                                                                   G7C2PGM 
03296      IF  S2GRPNOI = '000SPS000'                                   G7C2PGM 
03297          EXEC CICS  XCTL  PROGRAM('GPM1PGM')                      G7C2PGM 
03298                           END-EXEC.                               G7C2PGM 
03299                                                                   G7C2PGM 
03300 *----- ACQUIRE STORAGE FOR W/F CONTRACT RECORD READ -------------*G7C2PGM 
03301                                                                   G7C2PGM 
03302      COMPUTE WS-02-W-F-GCCONTR-MAX-LEN = GC-GCIOPARM-LEN          G7C2PGM 
03303                                        + GC-WORKFILE-KEY-LEN      G7C2PGM 
03304                                        + GC-GCCONTR-MAX-REC-LEN.  G7C2PGM 
03305                                                                   G7C2PGM 
03306      EXEC CICS  GETMAIN  SET(ADDRESS OF IO-PARM-CONTRACT-AREA)    G7C2PGM 
03307                          INITIMG(WS-02-HEX-00)                    G7C2PGM 
03308                          LENGTH (WS-02-W-F-GCCONTR-MAX-LEN)       G7C2PGM 
03309                          END-EXEC.                                G7C2PGM 
03310                                                                   G7C2PGM 
03311 *    COMPUTE  CONTRACT-PNTR-2 =  CONTRACT-PNTR +  4096.           G7C2PGM 
03312 *    SERVICE RELOAD  IO-PARM-CONTRACT-AREA.                       G7C2PGM 
03313                                                                   G7C2PGM 
03314 *----- READ W/F CONTRACT RECORD AND PASS IT TO GC5A -------------*G7C2PGM 
03315                                                                   G7C2PGM 
03316      MOVE GC-GCCONTR-VARY-MAX-OCUR                                G7C2PGM 
03317        TO GCT2-COUNT-BEN-PROVN-POINTERS.                          G7C2PGM 
03318                                                                   G7C2PGM 
03319      MOVE 'RD '                  TO  GCIO3-FILE-ACCESS-CODE.      G7C2PGM 
03320      MOVE GC-GCPSWORK-DDNAME     TO  GCIO3-FILE-DDNAME.           G7C2PGM 
03321                                                                   G7C2PGM 
03322      MOVE SPACES                 TO  GCIO-CONTRACT-FILE-KEY.      G7C2PGM 
03323      MOVE WRK-PLAN-CODE          TO  GCIO-WRK-PLAN-CODE.          G7C2PGM 
03324      MOVE WRK-GROUP-NO-1-3       TO  GCIO-WRK-GROUP-NO-1-3.       G7C2PGM 
03325      MOVE WRK-SEC-NO-1           TO  GCIO-WRK-SEC-NO-1.           G7C2PGM 
03326      MOVE WRK-PKG-CODE           TO  GCIO-WRK-PKG-CODE.           G7C2PGM 
03327      MOVE WRK-EFFECTIVE-DATE     TO  GCIO-WRK-EFFECTIVE-DT.       G7C2PGM 
03328      MOVE 'C'                    TO  GCIO-WRK-STATUS-CODE.        G7C2PGM 
03329      MOVE S2PLNCDI               TO  GCIO-WRK-PLAN-CODE.          G7C2PGM 
03330      MOVE S2GRPNOI               TO  GCIO-WRK-GROUP-NUM.          G7C2PGM 
03331      MOVE S2SECNOI               TO  GCIO-WRK-SECTION-NUM.        G7C2PGM 
03332      MOVE S2PKGCDI               TO  GCIO-WRK-PKG-CODE.           G7C2PGM 
03333      MOVE S2LOBI                 TO  GCIO-WRK-LINE-OF-BUS.        G7C2PGM 
03334      MOVE S2PRVI                 TO  GCIO-WRK-PROVIDER-CONTROL.   G7C2PGM 
03335      MOVE S2FRLI                 TO  GCIO-WRK-FAMILY-RELATION-LVL.G7C2PGM 
03336                                                                   G7C2PGM 
03337 *    MOVE S2EFFDTI               TO  HGADATE-DATE1.               G7C2PGM 
03338 *    PERFORM 9800-000-GREGORIAN-TO-JULIAN.                        G7C2PGM 
03339 *    IF  HGADATE-RETURN = ZEROS                                   G7C2PGM 
03340 *    THEN                                                         G7C2PGM 
03341 *        MOVE HGADATE-JULIAN2    TO  GCIO-WRK-EFFECTIVE-DATE      G7C2PGM 
03342 *    ELSE                                                         G7C2PGM 
03343 *        SET WT-01-INDEX TO +07                                   G7C2PGM 
03344 *        PERFORM 9000-000-MOVE-MSG-TO-SCREEN                      G7C2PGM 
03345 *        PERFORM 9100-000-SEND-THEN-RETURN.                       G7C2PGM 
03346                                                                   G7C2PGM 
03347      MOVE 'C2'                   TO  GCIO-WRK-RECORD-TYPE.        G7C2PGM 
03348      MOVE SPACES                 TO  GCIO-WRK-PROVISION-ID.       G7C2PGM 
03349      MOVE ZEROS                  TO  GCIO-WRK-PROVISION-SLOT-NO.  G7C2PGM 
03350      MOVE SPACES                 TO  GCIO-WRK-TAB-PROVISION-ID.   G7C2PGM 
03351      MOVE ZEROS                  TO  GCIO-WRK-TAB-PROV-SLOT-NO.   G7C2PGM 
03352      MOVE GCIO-WORKFILE-KEY      TO  GCIO3-FILE-KEY.              G7C2PGM 
03353      MOVE '1'                    TO  GCIO3-IO-AREA-TO-USE.        G7C2PGM 
03354                                                                   G7C2PGM 
03355      PERFORM  5100-000-W-F-CONTRACT-IO.                           G7C2PGM 
03356                                                                   G7C2PGM 
03357      IF  NOT GCIO3-GOOD-RETURN                                    G7C2PGM 
03358          MOVE WS-01-ABCODE-C2F1     TO WS-01-ABCODE               G7C2PGM 
03359          MOVE WS-01-ABCODE-C2F1-MSG TO WS-01-ABCODE-MSG           G7C2PGM 
03360          PERFORM  9999-000-ABEND-THE-TASK.                        G7C2PGM 
03361                                                                   G7C2PGM 
03362      EXEC CICS  XCTL  PROGRAM ('GC5APGM')                         G7C2PGM 
03363                       COMMAREA(WORK-RECORD-3)                     G7C2PGM 
03364                       LENGTH  (GCIO3-RECORD-LENGTH)               G7C2PGM 
03365                       END-EXEC.                                   G7C2PGM 
03366                                                                   G7C2PGM 
03367  9210-900-EXIT.                                                   G7C2PGM 
03368      EXIT.                                                        G7C2PGM 
03369 /*****************************************************************G7C2PGM 
03370 *                                                                *G7C2PGM 
03371 * 9220    XCTL TO HARDCOPY PROGRAM FOR SCREEN PRINT              *G7C2PGM 
03372 *                                                                *G7C2PGM 
03373 *                                                                *G7C2PGM 
03374 ******************************************************************G7C2PGM 
03375  9220-000-XCTL-TO-HARDCOPY-PGM  SECTION.                          G7C2PGM 
03376  9220-010.                                                        G7C2PGM 
03377                                                                   G7C2PGM 
03378      EXEC CICS  XCTL  PROGRAM('HGACOPYP')                         G7C2PGM 
03379                       END-EXEC.                                   G7C2PGM 
03380                                                                   G7C2PGM 
03381  9220-900-EXIT.                                                   G7C2PGM 
03382      EXIT.                                                        G7C2PGM 
03383 /*****************************************************************G7C2PGM 
03384 *                                                                *G7C2PGM 
03385 * 9800    G R E G O R I A N   T O   J U L I A N                  *G7C2PGM 
03386 *                                                                *G7C2PGM 
03387 *   CONVERT GREGORIAN DATE (MMDDYY) TO JULIAN (YYDDD) FORMAT.    *G7C2PGM 
03388 *                                                                *G7C2PGM 
03389 ******************************************************************G7C2PGM 
03390  9800-000-GREGORIAN-TO-JULIAN   SECTION.                          G7C2PGM 
03391  9800-010.                                                        G7C2PGM 
03392                                                                   G7C2PGM 
03393      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7C2PGM 
03394      MOVE 'M'   TO  HGADATE-FORM1.                                G7C2PGM 
03395      MOVE 'J'   TO  HGADATE-FORM2.                                G7C2PGM 
03396      MOVE ZEROS TO  HGADATE-RETURN                                G7C2PGM 
03397                     HGADATE-AMOUNT.                               G7C2PGM 
03398      EXEC CICS LINK PROGRAM ('HGADATES')                          G7C2PGM 
03399                     COMMAREA(HGADATES-COMMAREA)                   G7C2PGM 
03400                     LENGTH  (24)                                  G7C2PGM 
03401                     END-EXEC.                                     G7C2PGM 
03402                                                                   G7C2PGM 
03403  9800-900-900-EXIT.                                               G7C2PGM 
03404      EXIT.                                                        G7C2PGM 
03405 /*****************************************************************G7C2PGM 
03406 *                                                                *G7C2PGM 
03407 * 9810    J U L I A N    T O    G R E G O R I A N                *G7C2PGM 
03408 *                                                                *G7C2PGM 
03409 *   CONVERT JULIAN DATE (YYDDD) TO GREGORIAN (MMDDYY) FORMAT.    *G7C2PGM 
03410 *                                                                *G7C2PGM 
03411 ******************************************************************G7C2PGM 
03412  9810-000-JULIAN-TO-GREGORIAN   SECTION.                          G7C2PGM 
03413  9810-010.                                                        G7C2PGM 
03414                                                                   G7C2PGM 
03415      MOVE 'CNV' TO  HGADATE-FUNC.                                 G7C2PGM 
03416      MOVE 'J'   TO  HGADATE-FORM1.                                G7C2PGM 
03417      MOVE 'M'   TO  HGADATE-FORM2.                                G7C2PGM 
03418      MOVE ZEROS TO  HGADATE-RETURN                                G7C2PGM 
03419                     HGADATE-AMOUNT.                               G7C2PGM 
03420      EXEC CICS LINK PROGRAM ('HGADATES')                          G7C2PGM 
03421                     COMMAREA(HGADATES-COMMAREA)                   G7C2PGM 
03422                     LENGTH  (24)                                  G7C2PGM 
03423                     END-EXEC.                                     G7C2PGM 
03424                                                                   G7C2PGM 
03425  9810-900-900-EXIT.                                               G7C2PGM 
03426      EXIT.                                                        G7C2PGM 
03427 /***************************************************************  G7C2PGM 
03428 *                                                              *  G7C2PGM 
03429 * 9999  ABEND THE TASK                                         *  G7C2PGM 
03430 *                                                              *  G7C2PGM 
03431 ****************************************************************  G7C2PGM 
03432  9999-000-ABEND-THE-TASK SECTION.                                 G7C2PGM 
03433  9999-010.                                                        G7C2PGM 
03434                                                                   G7C2PGM 
03435      EXEC CICS  ABEND                                             G7C2PGM 
03436                 ABCODE(WS-01-ABCODE)                              G7C2PGM 
03437                 END-EXEC.                                         G7C2PGM 
03438                                                                   G7C2PGM 
03439  9900-900-EXIT.                                                   G7C2PGM 
03440      EXIT.                                                        G7C2PGM 
