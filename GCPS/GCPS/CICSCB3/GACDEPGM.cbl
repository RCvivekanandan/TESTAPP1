00001  ID DIVISION.                                                     09/03/03
00002  PROGRAM-ID.     GACDEPGM.                                        GACDEPGM
00003 **** THIS IS A COBOL/2 PROGRAM ***                                   LV003
00004  AUTHOR.         J.L.ARKEMA.                                      GACDEPGM
00005  DATE-WRITTEN.   05/06/87.                                        GACDEPGM
00006  DATE-COMPILED.                                                   GACDEPGM
00007 ******************************************************************GACDEPGM
00008 *                                                                *GACDEPGM
00009 *       M A I N T E N A N C E     L O G                          *GACDEPGM
00010 *                                                                *GACDEPGM
00011 *                                                                *GACDEPGM
00012 **-CHG-* *-DATE-* *WHO* *---------DESCRIPTION--------------------*GACDEPGM
00013 *  N126      08/28/87  JLA   ADD LOGIC FOR SUICIDE BIT           *GACDEPGM
00014 *                                                                *GACDEPGM
00015 *  D116      09/16/87  JLA  CAUSE GCIOPGM TO CALL GX5ZPGM TO     *GACDEPGM
00016 *                           UPDATE OPERATOR-ID IN W/F RECORD     *GACDEPGM
00017 *                           WHEN A W/F CONTROL RECORD IS         *GACDEPGM
00018 *                           UPDATED.                             *GACDEPGM
00019 *                                                                *GACDEPGM
00020 *  ????      09/23/87  JLA  CHANGE DEFINITION OF WHEN AN ACCUM   *GACDEPGM
00021 *                            IS CRITICAL.  THE PROD SLOT# AND    *GACDEPGM
00022 *                            THE W/F SLOT# NOLONGER HAVE TO      *GACDEPGM
00023 *                            MATCH.  IT IS ONLY REQUIRED THAT    *GACDEPGM
00024 *                            THE PROD CONTRACT, GROUP SPECIFIC   *GACDEPGM
00025 *                            OR BEN PROV HAVE A MATCHING ACTIVE  *GACDEPGM
00026 *                            ACCUM ATTACHED TO IT.               *GACDEPGM
00027 *                                                                *GACDEPGM
00028 *  D143      10/08/87  JLA   ADD NON-CDE RELEASE-LOGIC.          *GACDEPGM
00029 *                                                                *GACDEPGM
00030 *  D143       1/11/88  DES   ENHANCED NON-CDE LOGIC              *GACDEPGM
00031 *                                                                *GACDEPGM
00032 *  D143       1/28/88  DES   ADDED NEW NON-CDE DESIGN'S LOGIC    *GACDEPGM
00033 *                                                                *GACDEPGM
00034 * ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC WILL FLAG THE   *GACDEPGM
00035 *                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *GACDEPGM
00036 *                            TAB LOGIC TO FLAG ITS ACCUM RECORD  *GACDEPGM
00037 *                            WHEN THE INTERNL REC FLAGED CDE.    *GACDEPGM
00038 *                                                                *GACDEPGM
00039 *  ????    08/22/88  NE   UPDATE THE CONTROL RECORD CDE STATUS   *GACDEPGM
00040 *                         TO REFLECT THE LOWEST STATUS WITHIN    *GACDEPGM
00041 *                         THE GROUP RECORDS 4700-000- SECTION    *GACDEPGM
00042 *                                                                *GACDEPGM
00043 * D????  01/13/89  ENW  ADDED LOGIC FOR BISCENDING INDICATOR.    *GACDEPGM
00044 *                                                                *GACDEPGM
00045 *                                                                *GACDEPGM
00046 * ????    02/20/89 NGE  FIX CDE STATUS RESET FOR INTERNAL TABS   *GACDEPGM
00047 *                        (4900-) SECTION DO NOT RESET IF THE     *GACDEPGM
00048 *                        ACUMM TABULAR IS A REPLACEMENT.         *GACDEPGM
00049 *                                                                *GACDEPGM
00050 *  D200   05/18/89 NGE  ADD TWO NEW COND-BITS INF AND TMJ        *GACDEPGM
00051 *                                                                *GACDEPGM
00052 * 11154   10/15/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GACDEPGM
00053 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GACDEPGM
00054 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GACDEPGM
00055 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GACDEPGM
00056 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GACDEPGM
00057 *                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*GACDEPGM
00058 *                       7. >>> CONVERT TO COBOL/2 <<<.           *GACDEPGM
00059 *         11/27/90      8. AGE-LIMIT FIELDS TO BE CDE ELEMENTS.  *GACDEPGM
00060 *                                                                *GACDEPGM
00061 * 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *GACDEPGM
00062 *                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *GACDEPGM
00063 *                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*GACDEPGM
00064 *                                                                *GACDEPGM
00065 * 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *GACDEPGM
00066 *                                                                *GACDEPGM
00067 * D12009 9/11/91  GDM  INCREASE GRP-SPEC-FAM-REL-LVL             *GACDEPGM
00068 *                               CONTRACT-FAM-REL-LVL             *GACDEPGM
00069 *                               BEN-PROV-FAM-REL-LVL             *GACDEPGM
00070 *                            TO 2 POSITIONS                      *GACDEPGM
00071 *                                                                *GACDEPGM
00072 *  D303  02/03/97 DAU  ADD FEAK INDICATOR                        *GACDEPGM
00073 *                                                                *GACDEPGM
00074 * 14726/ 10/24/97 DAU  ADDED CODE TO SUPPORT THE YEAR 2000 AND   *GACDEPGM
00075 * 15057                THE EXPANSION OF THE GROUP SPECIFIC AND   *GACDEPGM
00076 *                      CONTRACT KEY TO SUPPORT THE TEXAS MERGER. *GACDEPGM
00077 *                                                                *GACDEPGM
00078 * 14726/ 11/30/97  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *GACDEPGM
00079 * 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *GACDEPGM
00080 *                       INCREASE GROUP AND SECTION NUMBERS.      *GACDEPGM
00081 *                                                                *GACDEPGM
00082 *  D341  10/01/98 GDM  MODIFY FOR NEW ACCUM #ACP                 *GACDEPGM
00083 *                                                                *GACDEPGM
00084 *  D303  05/25/99 DAU  REMOVED FEAK INDICATOR FROM BEING A CDE   *GACDEPGM
00085 *                                                                *GACDEPGM
00086 *  D===  11/14/01 AKK  ADD SUPPORT FOR 4 NEW BITS, 2 FOR EMER AND*GACDEPGM
00087 *                      TWO FOR SERIOUS MENTAL ILLNESS            *GACDEPGM
00088 *                                                                *GACDEPGM
00089 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GACDEPGM
00090 *                                                                *GACDEPGM
00091 *            06-30-03   DAF   USE COPYBOOK GCTIPGP2 INSTEAD OF   *GACDEPGM
00092 *                             GCTIPGT2                           *GACDEPGM
00093 *                                                                *GACDEPGM
00091 *  P09400    11-07-06   GF    ADD BISCENDING INDICATOR LOGIC     *GACDEPGM
00092 *                             FOR #ABM, #ACP, #ADL               *GACDEPGM
00093 *                                                                *GACDEPGM
00091 *  P21595    09/19/16 HSB  RECOMPILE FOR GCPS NEW FIELDS BENEFIT *GACDEPGM
00092 *                          TYPE, TIER CODE, TIER LEVEL.          *GACDEPGM
00093 *                                                                *GACDEPGM
SI0724*  P56703    05/08/24  SI  RECOMPILE - PEAQ COPYBOOK EXPANSION   *        
SI0724*                          COPY ABM, ACP, ACL, ADL, AOL,         *        
SI0724*                          GCCDRLEN                              *        
00094 ******************************************************************GACDEPGM
00095                                                                   GACDEPGM
00096 ******************************************************************GACDEPGM
00097 *    GACDEPGM - THIS PROGRAM PERFORMS CDE FUNCTION FOR ALL LEVEL *GACDEPGM
00098 *               TABULAR MAINTENANCE PROGRAMS GA1BPGM, GA1CPGM,   *GACDEPGM
00099 *               GA1DPGM, GA1EPGM, AND GA1PPGM.                   *GACDEPGM
00100 *                                                                *GACDEPGM
00101 *    TRANSID: (GA1B, GA1C, GA1D, GA1E, OR GA1P)                  *GACDEPGM
00102 *                                                                *GACDEPGM
00103 *                          ********************************      *GACDEPGM
00104 *                          *   THIS MAPSET IS SHARED BY   *      *GACDEPGM
00105 *                          *   THE FOLLOWING MODULES:     *      *GACDEPGM
00106 *                          *   1. GA1BPGM                 *      *GACDEPGM
00107 *                          *   2. GA1CPGM                 *      *GACDEPGM
00108 *                          *   3. GA1DPGM                 *      *GACDEPGM
00109 *                          *   4. GA1EPGM                 *      *GACDEPGM
00110 *                          *   5. GA1PPGM                 *      *GACDEPGM
00111 *                          *   6. GASEDIT1                *      *GACDEPGM
00112 *   MAPSET:    GA1XSETC ==>*   7. GACDEPGM                *      *GACDEPGM
00113 *                          *   8. GK1BPGM                 *      *GACDEPGM
00114 *                          *   9. GK1CPGM                 *      *GACDEPGM
00115 *                          *  10. GK1DPGM                 *      *GACDEPGM
00116 *                          *  11. GK1EPGM                 *      *GACDEPGM
00117 *                          *  12. GK1PPGM                 *      *GACDEPGM
00118 *                          *  13. GAS1UPD                 *      *GACDEPGM
00119 *                          *  14. GAS2UPD                 *      *GACDEPGM
00120 *                          *  15. GAS3UPD                 *      *GACDEPGM
00121 *                          *  16. GAS4UPD                 *      *GACDEPGM
00122 *                          ********************************      *GACDEPGM
00123 *                                                                *GACDEPGM
00124 *    VALGEN:  NONE                                               *GACDEPGM
00125 *                                                                *GACDEPGM
00126 ******************************************************************GACDEPGM
00127                                                                   GACDEPGM
00128  ENVIRONMENT DIVISION.                                            GACDEPGM
00129 /        D A T A   D I V I S I O N                                GACDEPGM
00130  DATA DIVISION.                                                   GACDEPGM
00131  WORKING-STORAGE SECTION.                                         GACDEPGM
00132  01  WS-BEGIN                    PIC X(58) VALUE                  GACDEPGM
00133      '*** GACDEPGM  WORKING-STORAGE BEGINS HERE ***'.             GACDEPGM
00134                                                                   GACDEPGM
00135  01  WS-ABEND-AREA.                                               GACDEPGM
00136      05  FILLER                   PIC X(16)  VALUE                GACDEPGM
00137          '** ABEND AREA **'.                                      GACDEPGM
00138                                                                   GACDEPGM
00139      05  WS-ABEND-CODES-AND-MSG.                                  GACDEPGM
00140          10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. GACDEPGM
00141          10  WS-ABCODE-MSG              PIC X(44)  VALUE  SPACES. GACDEPGM
00142                                                                   GACDEPGM
00143          10  WS-ABCODE-CDF1             PIC X(04)  VALUE  'CDF1'. GACDEPGM
00144          10  WS-ABCODE-CDF1-MSG         PIC X(44)  VALUE          GACDEPGM
00145             'READ ERROR ON ALL LVL TABULAR            '.          GACDEPGM
00146                                                                   GACDEPGM
00147          10  WS-ABCODE-CDF2             PIC X(04)  VALUE  'CDF2'. GACDEPGM
00148          10  WS-ABCODE-CDF2-MSG         PIC X(44)  VALUE          GACDEPGM
00149             'READ ERROR ON W/F CONTROL RECORD         '.          GACDEPGM
00150                                                                   GACDEPGM
00151          10  WS-ABCODE-CDF3             PIC X(04)  VALUE  'CDF3'. GACDEPGM
00152          10  WS-ABCODE-CDF3-MSG         PIC X(44)  VALUE          GACDEPGM
00153             'REWRITE ERROR ON W/F CONTROL RECORD      '.          GACDEPGM
00154                                                                   GACDEPGM
00155          10  WS-ABCODE-CDF4             PIC X(04)  VALUE  'CDF4'. GACDEPGM
00156          10  WS-ABCODE-CDF4-MSG         PIC X(44)  VALUE          GACDEPGM
00157             'READ ERROR ON W/F ALL LVL TABULAR        '.          GACDEPGM
00158                                                                   GACDEPGM
00159          10  WS-ABCODE-CDF5             PIC X(04)  VALUE  'CDF5'. GACDEPGM
00160          10  WS-ABCODE-CDF5-MSG         PIC X(44)  VALUE          GACDEPGM
00161             'REWRITE ERROR ON W/F ALL LVL TABULAR     '.          GACDEPGM
00162                                                                   GACDEPGM
00163          10  WS-ABCODE-CDF6             PIC X(04)  VALUE  'CDF6'. GACDEPGM
00164          10  WS-ABCODE-CDF6-MSG         PIC X(44)  VALUE          GACDEPGM
00165             'REWRITE ERROR ON W/F CONTROL RECORD      '.          GACDEPGM
00166                                                                   GACDEPGM
00167          10  WS-ABCODE-CDF7             PIC X(04)  VALUE  'CDF7'. GACDEPGM
00168          10  WS-ABCODE-CDF7-MSG         PIC X(44)  VALUE          GACDEPGM
00169             'REWRITE ERROR ON W/F CONTROL RECORD      '.          GACDEPGM
00170                                                                   GACDEPGM
00171          10  WS-ABCODE-CDF8             PIC X(04)  VALUE  'CDF8'. GACDEPGM
00172          10  WS-ABCODE-CDF8-MSG         PIC X(44)  VALUE          GACDEPGM
00173             'READ ERROR ON W/F CONTROL RECORD         '.          GACDEPGM
00174                                                                   GACDEPGM
00175          10  WS-ABCODE-CDF9             PIC X(04)  VALUE  'CDF9'. GACDEPGM
00176          10  WS-ABCODE-CDF9-MSG         PIC X(44)  VALUE          GACDEPGM
00177             'REWRITE ERROR ON W/F CONTROL RECORD      '.          GACDEPGM
00178                                                                   GACDEPGM
00179          10  WS-ABCODE-CDFA             PIC X(04)  VALUE  'CDFA'. GACDEPGM
00180          10  WS-ABCODE-CDFA-MSG         PIC X(44)  VALUE          GACDEPGM
00181             'READ ERROR ON W/F INTERNAL TAB RECORD    '.          GACDEPGM
00182                                                                   GACDEPGM
00183          10  WS-ABCODE-CDFB             PIC X(04)  VALUE  'CDFB'. GACDEPGM
00184          10  WS-ABCODE-CDFB-MSG         PIC X(44)  VALUE          GACDEPGM
00185             'REWRITE ERROR ON W/F INTERNAL TAB RECORD '.          GACDEPGM
00186                                                                   GACDEPGM
00187          10  WS-ABCODE-CDFC             PIC X(04)  VALUE  'CDFB'. GACDEPGM
00188          10  WS-ABCODE-CDFC-MSG         PIC X(44)  VALUE          GACDEPGM
00189             'READ ERROR ON W/F CONTROL RECORD         '.          GACDEPGM
00190                                                                   GACDEPGM
00191                                                                   GACDEPGM
00192          10  WS-ABCODE-CDP1             PIC X(04)  VALUE  'CDP1'. GACDEPGM
00193          10  WS-ABCODE-CDP1-MSG         PIC X(44)  VALUE          GACDEPGM
00194             'INVALID REQUEST CODE RECEIVED FROM CALLER'.          GACDEPGM
00195                                                                   GACDEPGM
00196          10  WS-ABCODE-CDL1             PIC X(04)  VALUE  'CDL1'. GACDEPGM
00197          10  WS-ABCODE-CDL1-MSG         PIC X(44)  VALUE          GACDEPGM
00198             'ILLOGICAL SITUATION DISCOVERED           '.          GACDEPGM
00199                                                                   GACDEPGM
00200          10  WS-ABCODE-CDL2             PIC X(04)  VALUE  'CDL2'. GACDEPGM
00201          10  WS-ABCODE-CDL2-MSG         PIC X(44)  VALUE          GACDEPGM
00202             'ILLOGICAL SITUATION DISCOVERED           '.          GACDEPGM
00203                                                                   GACDEPGM
00204          10  WS-ABCODE-CDL3             PIC X(04)  VALUE  'CDL3'. GACDEPGM
00205          10  WS-ABCODE-CDL3-MSG         PIC X(44)  VALUE          GACDEPGM
00206             'ILLOGICAL SITUATION DISCOVERED           '.          GACDEPGM
00207                                                                   GACDEPGM
00208          10  WS-ABCODE-CDL4             PIC X(04)  VALUE  'CDL4'. GACDEPGM
00209          10  WS-ABCODE-CDL4-MSG         PIC X(44)  VALUE          GACDEPGM
00210             'ILLOGICAL SITUATION DISCOVERED           '.          GACDEPGM
00211          10  WS-ABCODE-CDL5             PIC X(04)  VALUE  'CDL5'. GACDEPGM
00212          10  WS-ABCODE-CDL5-MSG         PIC X(44)  VALUE          GACDEPGM
00213             'ILLOGICAL SITUATION DISCOVERED           '.          GACDEPGM
00214                                                                   GACDEPGM
00215 /                                                                 GACDEPGM
00216  01  WS-WORK-FIELDS.                                              GACDEPGM
00217                                                                   GACDEPGM
00218      05  WS-SUM-BIT                    PIC 9(2).                  GACDEPGM
00219      05  WS-SUM-BIT1                   PIC 9(2).                  GACDEPGM
00220      05  WS-SUM-BIT2                   PIC 9(2).                  GACDEPGM
00221      05  WS-SUM-BIT3                   PIC 9(2).                  GACDEPGM
00222                                                                   GACDEPGM
00223      05  WS-SUM-1                      PIC 9.                     GACDEPGM
00224      05  WS-SUM-2                      PIC 9.                     GACDEPGM
00225      05  WS-SUM-3                      PIC 9.                     GACDEPGM
00226      05  WS-SUM-4                      PIC 9.                     GACDEPGM
00227      05  WS-SUM-5                      PIC 9.                     GACDEPGM
00228      05  WS-SUM-6                      PIC 9.                     GACDEPGM
00229      05  WS-SUM-7                      PIC 9.                     GACDEPGM
00230      05  WS-SUM-8                      PIC 9.                     GACDEPGM
00231      05  WS-SUM-9                      PIC 9.                     GACDEPGM
00232      05  WS-SUM-10                     PIC 9.                     GACDEPGM
00233      05  WS-SUM-11                     PIC 9.                     GACDEPGM
00234      05  WS-SUM-12                     PIC 9.                     GACDEPGM
00235      05  WS-SUM-13                     PIC 9.                     GACDEPGM
00236      05  WS-SUM-14                     PIC 9.                     GACDEPGM
00237      05  WS-SUM-15                     PIC 9.                     GACDEPGM
00238      05  WS-SUM-16                     PIC 9.                     GACDEPGM
00239      05  WS-SLOT-CNT                   PIC 9  VALUE ZEROS.        GACDEPGM
00240      05  WS-DEL-CNT                    PIC 9  VALUE ZEROS.        GACDEPGM
00241                                                                   GACDEPGM
00242      05  WS-HEX-00                     PIC X  VALUE LOW-VALUES.   GACDEPGM
00243      05  WS-ADD-OCCR-SW                PIC X  VALUE SPACE.        GACDEPGM
00244                                                                   GACDEPGM
00245      05  WS-QUOTIENT                   PIC 999 COMP-3.            GACDEPGM
00246      05  WS-REMAINDER                  PIC 999 COMP-3.            GACDEPGM
00247                                                                   GACDEPGM
00248      05  WS-RETURN-CODES.                                         GACDEPGM
00249          10  WS-CDE-RETURN-CONTINUE    PIC X(2)  VALUE '00'.      GACDEPGM
00250          10  WS-CDE-RETURN-DONT-SEND   PIC X(2)  VALUE '01'.      GACDEPGM
00251          10  WS-CDE-RETURN-WITH-SEND   PIC X(2)  VALUE '02'.      GACDEPGM
00252                                                                   GACDEPGM
00253     05 WS-IO-PARM-WRK-ALL-LVL-LEN      PIC S9(4) COMP  VALUE +0.  GACDEPGM
00254     05 WS-IO-PARM-WRK-BEN-PROV-LEN     PIC S9(4) COMP  VALUE +0.  GACDEPGM
00255     05 WS-IO-PARM-WRK-CONTRACT-LEN     PIC S9(4) COMP  VALUE +0.  GACDEPGM
00256     05 WS-IO-PARM-WRK-CONTROL-LEN      PIC S9(4) COMP  VALUE +0.  GACDEPGM
00257     05 WS-IO-PARM-WRK-GRP-SPEC-LEN     PIC S9(4) COMP  VALUE +0.  GACDEPGM
00258     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN  PIC S9(4) COMP  VALUE +0.  GACDEPGM
00259     05 WS-WRK-BEN-PROV-LEN             PIC S9(4) COMP  VALUE +0.  GACDEPGM
00260     05 WS-WRK-CONTRACT-LEN             PIC S9(4) COMP  VALUE +0.  GACDEPGM
00261     05 WS-WRK-GRP-SPEC-LEN             PIC S9(4) COMP  VALUE +0.  GACDEPGM
00262 /                                                                 GACDEPGM
00263  01  WS-EIBDATE-AREA.                                             GACDEPGM
00264      05  WS-EIBDATE.                                              GACDEPGM
00265          10  WS-EIBDATE-CC.                                       GACDEPGM
00266              15  WS-EIBDATE-1          PIC 9.                     GACDEPGM
00267              15  WS-EIBDATE-2          PIC 9.                     GACDEPGM
00268          10  WS-EIBDATE-DT             PIC 9(5).                  GACDEPGM
00269      05  WS-EIBDATE-CEN REDEFINES WS-EIBDATE                      GACDEPGM
00270                                        PIC 9(7).                  GACDEPGM
00271 /    T I T L E   L I N E S                                        GACDEPGM
00272  01  WS-TITLE-LINES.                                              GACDEPGM
00273  COPY GCMHLINE.                                                   GACDEPGM
00274                                                                   GACDEPGM
00275 /-------------- DATE ROUTINE COMMAREA ---------------------------*GACDEPGM
00276  01  HGADATES-COMMAREA.                                           GACDEPGM
00277  COPY HGCDAT01.                                                   GACDEPGM
00278                                                                   GACDEPGM
00279 /-------------- HEX VALUES COPYMEMBER ---------------------------*GACDEPGM
00280  COPY HEXCOBOL.                                                   GACDEPGM
00281                                                                   GACDEPGM
00282 /       T R A N S R O U T I N G   C O M M A R E A                 GACDEPGM
00283  01  GCTRSRT-COMMAREA.                                            GACDEPGM
00284  COPY GCCDCDE1.                                                   GACDEPGM
00285                                                                   GACDEPGM
00286 /                                                                 GACDEPGM
00287  01  WT-00-GACDEPGM-TABLES.                                       GACDEPGM
00288      05  FILLER                   PIC X(16)  VALUE                GACDEPGM
00289          '*GACDEPGM TABLES'.                                      GACDEPGM
00290                                                                   GACDEPGM
00291  01  WT-01-TABLE.                                                 GACDEPGM
00292      05  FILLER                  PIC X(16) VALUE                  GACDEPGM
00293          '* WT-01-TABLE  *'.                                      GACDEPGM
00294 ******************************************************************GACDEPGM
00295 *    WT-01   MESSAGE TABLE                                       *GACDEPGM
00296 ******************************************************************GACDEPGM
00297  01  FILLER.                                                      GACDEPGM
00298      05  WT-01-MESSAGE-VALUES.                                    GACDEPGM
00299 *----------------------------------------------------------------*GACDEPGM
00300          10  WT-01-ENTRY-001.                                     GACDEPGM
00301              15  FILLER              PIC X(2)  VALUE '¬>'.        GACDEPGM
00302              15  WT-01-MESSAGE-TEXT-001.                          GACDEPGM
00303                  20  FILLER          PIC X(4)  VALUE  'GACD'.     GACDEPGM
00304                  20  FILLER          PIC X(1)  VALUE  '-'.        GACDEPGM
00305                  20  FILLER          PIC X(3)  VALUE  '001'.      GACDEPGM
00306                  20  FILLER          PIC X(1)  VALUE  ' '.        GACDEPGM
00307                  20  FILLER          PIC X(70) VALUE              GACDEPGM
00308                      'CRITICAL ELEMENT CHANGED,  HIT ENTER TO CONTGACDEPGM
00309 -                    'INUE                     '.                 GACDEPGM
00310              15  FILLER              PIC X(2)  VALUE '<¬'.        GACDEPGM
00311 *----------------------------------------------------------------*GACDEPGM
00312          10  WT-01-ENTRY-002.                                     GACDEPGM
00313              15  FILLER              PIC X(2)  VALUE '¬>'.        GACDEPGM
00314              15  WT-01-MESSAGE-TEXT-002.                          GACDEPGM
00315                  20  FILLER          PIC X(4)  VALUE  'GACD'.     GACDEPGM
00316                  20  FILLER          PIC X(1)  VALUE  '-'.        GACDEPGM
00317                  20  FILLER          PIC X(3)  VALUE  '002'.      GACDEPGM
00318                  20  FILLER          PIC X(1)  VALUE  ' '.        GACDEPGM
00319                  20  FILLER          PIC X(70) VALUE              GACDEPGM
00320                      'CRITICAL ELEMENT CHANGED,  HIT PF3 TO CONTINGACDEPGM
00321 -                    'UE                       '.                 GACDEPGM
00322              15  FILLER              PIC X(2)  VALUE '<¬'.        GACDEPGM
00323 *----------------------------------------------------------------*GACDEPGM
00324          10  WT-01-ENTRY-003.                                     GACDEPGM
00325              15  FILLER              PIC X(2)  VALUE '¬>'.        GACDEPGM
00326              15  WT-01-MESSAGE-TEXT-003.                          GACDEPGM
00327                  20  FILLER          PIC X(4)  VALUE  'GACD'.     GACDEPGM
00328                  20  FILLER          PIC X(1)  VALUE  '-'.        GACDEPGM
00329                  20  FILLER          PIC X(3)  VALUE  '003'.      GACDEPGM
00330                  20  FILLER          PIC X(1)  VALUE  ' '.        GACDEPGM
00331                  20  FILLER          PIC X(70) VALUE              GACDEPGM
00332                      'GROUP IN CONVERSION STATUS, CANNOT CHANGE HIGACDEPGM
00333 -                    'GH-LIGHTED ELEMENTS      '.                 GACDEPGM
00334              15  FILLER              PIC X(2)  VALUE '<¬'.        GACDEPGM
00335 *----------------------------------------------------------------*GACDEPGM
00336          10  WT-01-ENTRY-004.                                     GACDEPGM
00337              15  FILLER              PIC X(2)  VALUE '¬>'.        GACDEPGM
00338              15  WT-01-MESSAGE-TEXT-004.                          GACDEPGM
00339                  20  FILLER          PIC X(4)  VALUE  'GACD'.     GACDEPGM
00340                  20  FILLER          PIC X(1)  VALUE  '-'.        GACDEPGM
00341                  20  FILLER          PIC X(3)  VALUE  '004'.      GACDEPGM
00342                  20  FILLER          PIC X(1)  VALUE  ' '.        GACDEPGM
00343                  20  FILLER          PIC X(70) VALUE              GACDEPGM
00344                      '********** F U T U R E   U S E *************GACDEPGM
00345 -                    '*************************'.                 GACDEPGM
00346              15  FILLER              PIC X(2)  VALUE '<¬'.        GACDEPGM
00347 *----------------------------------------------------------------*GACDEPGM
00348      05  WT-01-MESSAGE-TABLE         REDEFINES                    GACDEPGM
00349          WT-01-MESSAGE-VALUES         OCCURS 004 TIMES            GACDEPGM
00350                                      INDEXED BY WT-01-INDEX.      GACDEPGM
00351          10  WT-01-ENTRY.                                         GACDEPGM
00352              15  FILLER              PIC X(02).                   GACDEPGM
00353              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GACDEPGM
00354              15  FILLER              PIC X(02).                   GACDEPGM
00355                                                                   GACDEPGM
00356 /     M A P   F I E L D   A T T R I B U T E S                     GACDEPGM
00357  COPY DFHBMSCA.                                                   GACDEPGM
00358 *                         AUTOSKIP, BRIGHT, FSET                  GACDEPGM
00359      02  DFHBMABF         PIC X  VALUE '9'.                       GACDEPGM
00360                                                                   GACDEPGM
00361 /     A T T E N T I O N   K E Y S                                 GACDEPGM
00362  COPY DFHAID.                                                     GACDEPGM
00363                                                                   GACDEPGM
00364 /    A L T E R N A T I V E   W O R K F I L E   K E Y S            GACDEPGM
00365  01  WS-ALT-WORKFILE-KEYS.                                        GACDEPGM
00366      COPY GCWRKKEY.                                               GACDEPGM
00367                                                                   GACDEPGM
00368 /    G . C .   G L O B A L L Y   D E F I N E D   L E N G T H S    GACDEPGM
00369  01  FILLER.                                                      GACDEPGM
00370      COPY GCCDRLEN.                                               GACDEPGM
00371                                                                   GACDEPGM
00372                                                                   GACDEPGM
00373  01  WS-END                       PIC X(58) VALUE                 GACDEPGM
00374      '*** GACDEPGM  WORKING-STORAGE ENDS HERE ***'.               GACDEPGM
00375 /    L I N K A G E   S E C T I O N                                GACDEPGM
00376  LINKAGE SECTION.                                                 GACDEPGM
00377 /    D F H C O M M A R E A                                        GACDEPGM
00378  01  DFHCOMMAREA.                                                 GACDEPGM
00379                                                                   GACDEPGM
00380 **** COMMON WORKAREAS PASSED TO AND FROM THIS PROGRAM ****        GACDEPGM
00381      COPY G2ALCKEC.                                               GACDEPGM
00382      COPY GACDACWA.                                               GACDEPGM
00383          05  GAS1UPD-PASSED-AREA.                                 GACDEPGM
00384              07  LVL2-B-SW        PIC X.                          GACDEPGM
00385              07  LVL2-F-SW        PIC X.                          GACDEPGM
00386              07  LVL2-G-SW        PIC X.                          GACDEPGM
00387              07  INTR-TAB-PGM-ID  PIC X(8).                       GACDEPGM
00388              07  FILLER           PIC X(9).                       GACDEPGM
00389          05  DELADD-OPTION        PIC X(7).                       GACDEPGM
00390                                                                   GACDEPGM
00391 /*****************************************************************GACDEPGM
00392 * W O R K F I L E   -   A L L   L E V E L   T A B   R E C O R D  *GACDEPGM
00393 ******************************************************************GACDEPGM
00394  01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               GACDEPGM
00395  COPY GCIOPRM1.                                                   GACDEPGM
00396                                                                   GACDEPGM
00397  COPY GCWRKDCC.                                                   GACDEPGM
00398                                                                   GACDEPGM
SI0724*    03  WF-ALL-LVL-TAB-RECORD             PIC X(8157).           GACDEPGM
SI0724     03  WF-ALL-LVL-TAB-RECORD             PIC X(30861).          GACDEPGM
00400                                                                   GACDEPGM
00401      03  WF-GAA-RECORD     REDEFINES WF-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTABMC   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTABMC   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00404                           == DEPENDING ON GAA-ENTRY-COUNT ==      GACDEPGM
00405                        BY ==                              == .    GACDEPGM
00406 /                                                                 GACDEPGM
00407      03  WF-GAB-RECORD     REDEFINES WF-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTACLC   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTACLC   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00410                           == DEPENDING ON GAB-ENTRY-COUNT ==      GACDEPGM
00411                        BY ==                              == .    GACDEPGM
00412 /                                                                 GACDEPGM
00413      03  WF-GAC-RECORD     REDEFINES WF-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTADLC   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTADLC   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00416                           == DEPENDING ON GAC-ENTRY-COUNT ==      GACDEPGM
00417                        BY ==                              == .    GACDEPGM
00418 /                                                                 GACDEPGM
00419      03  WF-GAD-RECORD     REDEFINES WF-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTAOLC   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTAOLC   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00422                           == DEPENDING ON GAD-ENTRY-COUNT ==      GACDEPGM
00423                        BY ==                              == .    GACDEPGM
00424 /                                                                 GACDEPGM
00425      03  WF-GAF-RECORD     REDEFINES WF-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTACPC   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTACPC   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00428                           == DEPENDING ON GAF-ENTRY-COUNT ==      GACDEPGM
00429                        BY ==                              == .    GACDEPGM
00430 /                                                                 GACDEPGM
00431 /*****************************************************************GACDEPGM
00432 * W O R K F I L E   -   I N T E R N A L   T A B   R E C O R D    *GACDEPGM
00433 ******************************************************************GACDEPGM
00434  01  WF-IO-PARM-INTERNAL-TAB-REC.                                 GACDEPGM
00435  COPY GCIOPRM2.                                                   GACDEPGM
00436 /                                                                 GACDEPGM
00437  COPY GCWRKDC2.                                                   GACDEPGM
00438 /                                                                 GACDEPGM
00439  COPY GCTIPGP2  REPLACING == 1 TO 1109 ==                         GACDEPGM
00440                        BY ==      1109 ==                         GACDEPGM
00441                           == DEPENDING ON GXAB-ENTRY-COUNT ==     GACDEPGM
00442                        BY ==                               == .   GACDEPGM
00443 /*****************************************************************GACDEPGM
00444 * W O R K F I L E   -   B E N E F I T   P V S N   R E C O R D    *GACDEPGM
00445 ******************************************************************GACDEPGM
00446  01  WF-IO-PARM-WRK-BEN-PROV-REC.                                 GACDEPGM
00447  COPY GCIOPRM5.                                                   GACDEPGM
00448 /                                                                 GACDEPGM
00449  COPY GCWRKDC5.                                                   GACDEPGM
00450 /                                                                 GACDEPGM
00451  COPY GCBENPVC.                                                   GACDEPGM
00452 /*****************************************************************GACDEPGM
00453 * W O R K F I L E   -   C O N T R O L   R E C O R D              *GACDEPGM
00454 ******************************************************************GACDEPGM
00455  01  WF-IO-PARM-WRK-CONTROL-REC.                                  GACDEPGM
00456  COPY GCIOPRM6.                                                   GACDEPGM
00457 /                                                                 GACDEPGM
00458  COPY GCWRKDC6.                                                   GACDEPGM
00459 /                                                                 GACDEPGM
00460  COPY GCCCRDCC.                                                   GACDEPGM
00461 /*****************************************************************GACDEPGM
00462 * P R O D U C T I O N   -   C O N T R A C T   R E C O R D        *GACDEPGM
00463 ******************************************************************GACDEPGM
00464  01  PR-IO-PARM-WRK-CONTRACT-REC.                                 GACDEPGM
00465  COPY GCIOPRM7.                                                   GACDEPGM
00466 /                                                                 GACDEPGM
00467  COPY GCWRKDC7.                                                   GACDEPGM
00468 /                                                                 GACDEPGM
00469  COPY GCCONTR2.                                                   GACDEPGM
00470 /*****************************************************************GACDEPGM
00471 * P R O D U C T I O N   -   G R O U P   S P E C .   R E C O R D  *GACDEPGM
00472 ******************************************************************GACDEPGM
00473  01  PR-IO-PARM-WRK-GRP-SPEC-REC.                                 GACDEPGM
00474  COPY GCIOPRM8.                                                   GACDEPGM
00475 /                                                                 GACDEPGM
00476  COPY GCWRKDC8.                                                   GACDEPGM
00477 /                                                                 GACDEPGM
00478  COPY GCGROUP2.                                                   GACDEPGM
00479 /*****************************************************************GACDEPGM
00480 * P R O D U C T I O N   -   B E N E F I T   P V S N   R E C O R D*GACDEPGM
00481 ******************************************************************GACDEPGM
00482  01  PR-IO-PARM-WRK-BEN-PROV-REC.                                 GACDEPGM
00483  COPY GCIOPRM9.                                                   GACDEPGM
00484 /                                                                 GACDEPGM
00485  COPY GCWRKDC9.                                                   GACDEPGM
00486 /                                                                 GACDEPGM
00487  COPY GCBENPV2.                                                   GACDEPGM
00488 /*****************************************************************GACDEPGM
00489 * P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C    *GACDEPGM
00490 ******************************************************************GACDEPGM
00491  01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               GACDEPGM
00492  COPY GCIOPRMA.                                                   GACDEPGM
00493 /                                                                 GACDEPGM
00494  COPY GCWRKDCA.                                                   GACDEPGM
00495 /                                                                 GACDEPGM
SI0724*    03  PR-ALL-LVL-TAB-RECORD             PIC X(8157).           GACDEPGM
SI0724     03  PR-ALL-LVL-TAB-RECORD             PIC X(30861).          GACDEPGM
00497                                                                   GACDEPGM
00498      03  PR-GAA-RECORD     REDEFINES PR-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTABM2   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTABM2   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00501                           == DEPENDING ON GAA2-ENTRY-COUNT ==     GACDEPGM
00502                        BY ==                               == .   GACDEPGM
00503 /                                                                 GACDEPGM
00504      03  PR-GAB-RECORD     REDEFINES PR-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTACL2   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTACL2   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00507                           == DEPENDING ON GAB2-ENTRY-COUNT ==     GACDEPGM
00508                        BY ==                               == .   GACDEPGM
00509 /                                                                 GACDEPGM
00510      03  PR-GAC-RECORD     REDEFINES PR-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTADL2   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTADL2   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00513                           == DEPENDING ON GAC2-ENTRY-COUNT ==     GACDEPGM
00514                        BY ==                               == .   GACDEPGM
00515 /                                                                 GACDEPGM
00516      03  PR-GAD-RECORD     REDEFINES PR-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTAOL2   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTAOL2   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00519                           == DEPENDING ON GAD2-ENTRY-COUNT ==     GACDEPGM
00520                        BY ==                               == .   GACDEPGM
00521 /                                                                 GACDEPGM
00522      03  PR-GAF-RECORD     REDEFINES PR-ALL-LVL-TAB-RECORD.       GACDEPGM
SI0724*COPY GCTACP2   REPLACING == 1 TO 44 ==                           GACDEPGM
SI0724*                      BY ==      44 ==                           GACDEPGM
SI0724 COPY GCTACP2   REPLACING == 1 TO 175 ==                          GACDEPGM
SI0724                       BY ==      175 ==                          GACDEPGM
00525                           == DEPENDING ON GAF2-ENTRY-COUNT ==     GACDEPGM
00526                        BY ==                               == .   GACDEPGM
00527                                                                   GACDEPGM
00528 /    M A P S E T   P A S S E D   T O   T H I S   P R O G R A M    GACDEPGM
00529                                                                   GACDEPGM
00530      COPY GA1XSETC.                                               GACDEPGM
00531                                                                   GACDEPGM
00532                                                                   GACDEPGM
00533 /    P R O C E D U R E   D I V I S I O N                          GACDEPGM
00534  PROCEDURE DIVISION.                                              GACDEPGM
00535                                                                   GACDEPGM
00536 ****************************************************************  GACDEPGM
00537 *           P R O C E S S     C O N T R O L                    *  GACDEPGM
00538 ****************************************************************  GACDEPGM
00539  0000-000-PROCESS-CONTROL       SECTION.                          GACDEPGM
00540                                                                   GACDEPGM
00541                                                                   GACDEPGM
00542      SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              GACDEPGM
00543                                                                   GACDEPGM
00544                                                                   GACDEPGM
00545      SET ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD  TO            GACDEPGM
00546                      ACWA-WF-ALL-LEVEL-TAB-PNTR.                  GACDEPGM
00547                                                                   GACDEPGM
00548      SET ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-REC    TO            GACDEPGM
00549                      ACWA-WF-INTERNAL-TAB-PNTR.                   GACDEPGM
00550                                                                   GACDEPGM
00551      SET ADDRESS OF  WF-IO-PARM-WRK-BEN-PROV-REC    TO            GACDEPGM
00552                      ACWA-WF-BEN-PROV-PNTR.                       GACDEPGM
00553                                                                   GACDEPGM
00554      SET ADDRESS OF  WF-IO-PARM-WRK-CONTROL-REC     TO            GACDEPGM
00555                      ACWA-WF-CONTROL-RECORD-PNTR.                 GACDEPGM
00556                                                                   GACDEPGM
00557                                                                   GACDEPGM
00558      MOVE WS-CDE-RETURN-CONTINUE  TO  ACWA-CDE-RETURN-CODE.       GACDEPGM
00559      MOVE ACWA-ALT-WORKFILE-KEYS  TO  WS-ALT-WORKFILE-KEYS.       GACDEPGM
00560      SET  GAA-INDEX    TO  ACWA-INDEX-1.                          GACDEPGM
00561      SET  GAB-INDEX    TO  ACWA-INDEX-1.                          GACDEPGM
00562      SET  GAC-INDEX    TO  ACWA-INDEX-1.                          GACDEPGM
00563      SET  GAD-INDEX    TO  ACWA-INDEX-1.                          GACDEPGM
00564      SET  GAF-INDEX    TO  ACWA-INDEX-1.                          GACDEPGM
00565                                                                   GACDEPGM
00566      COMPUTE WS-IO-PARM-WRK-GRP-SPEC-LEN =                        GACDEPGM
00567              GC-GCIOPARM-LEN             +                        GACDEPGM
00568              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00569              GC-GCGRPSPC-FIXED-LEN       +                        GACDEPGM
00570             (GC-GCGRPSPC-VARY-LEN        *                        GACDEPGM
00571              GC-GCGRPSPC-VARY-MAX-OCUR).                          GACDEPGM
00572                                                                   GACDEPGM
00573      COMPUTE WS-WRK-GRP-SPEC-LEN         =                        GACDEPGM
00574              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00575              GC-GCGRPSPC-FIXED-LEN       +                        GACDEPGM
00576             (GC-GCGRPSPC-VARY-LEN        *                        GACDEPGM
00577              GC-GCGRPSPC-VARY-MAX-OCUR).                          GACDEPGM
00578                                                                   GACDEPGM
00579      COMPUTE WS-IO-PARM-WRK-CONTRACT-LEN =                        GACDEPGM
00580              GC-GCIOPARM-LEN             +                        GACDEPGM
00581              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00582              GC-GCCONTR-FIXED-LEN        +                        GACDEPGM
00583             (GC-GCCONTR-VARY-LEN         *                        GACDEPGM
00584              GC-GCCONTR-VARY-MAX-OCUR).                           GACDEPGM
00585                                                                   GACDEPGM
00586      COMPUTE WS-WRK-CONTRACT-LEN         =                        GACDEPGM
00587              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00588              GC-GCCONTR-FIXED-LEN        +                        GACDEPGM
00589             (GC-GCCONTR-VARY-LEN         *                        GACDEPGM
00590              GC-GCCONTR-VARY-MAX-OCUR).                           GACDEPGM
00591                                                                   GACDEPGM
00592      COMPUTE WS-IO-PARM-WRK-BEN-PROV-LEN =                        GACDEPGM
00593              GC-GCIOPARM-LEN             +                        GACDEPGM
00594              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00595              GC-GCBENPRV-FIXED-LEN       +                        GACDEPGM
00596             (GC-GCBENPRV-VARY-LEN        *                        GACDEPGM
00597              GC-GCBENPRV-VARY-MAX-OCUR).                          GACDEPGM
00598                                                                   GACDEPGM
00599      COMPUTE WS-WRK-BEN-PROV-LEN         =                        GACDEPGM
00600              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00601              GC-GCBENPRV-FIXED-LEN       +                        GACDEPGM
00602             (GC-GCBENPRV-VARY-LEN        *                        GACDEPGM
00603              GC-GCBENPRV-VARY-MAX-OCUR).                          GACDEPGM
00604                                                                   GACDEPGM
00605      COMPUTE WS-IO-PARM-WRK-CONTROL-LEN  =                        GACDEPGM
00606              GC-GCIOPARM-LEN             +                        GACDEPGM
00607              GC-WORKFILE-KEY-LEN         +                        GACDEPGM
00608              GC-WORKFILE-CONTROL-REC-LEN.                         GACDEPGM
00609                                                                   GACDEPGM
00610      IF  ACWA-REQUEST-4500-CDE-PROTECT                            GACDEPGM
00611          PERFORM 4500-000-PROTECT-CRIT-DATA-ELE                   GACDEPGM
00612      ELSE                                                         GACDEPGM
00613      IF  ACWA-REQUEST-4600-CDE-STATUS                             GACDEPGM
00614          PERFORM 4600-000-UPDATE-CDE-STATUS                       GACDEPGM
00615      ELSE                                                         GACDEPGM
00616      IF  ACWA-REQUEST-4700-CNTL-RECORD                            GACDEPGM
00617          PERFORM 4700-000-UPDATE-CONTROL-RECORD                   GACDEPGM
00618      ELSE                                                         GACDEPGM
00619      IF ACWA-REQUEST-4900-CNTL-RECORD                             GACDEPGM
00620         PERFORM 4900-000-UPDT-INTRNL-AND-CNTL                     GACDEPGM
00621      ELSE                                                         GACDEPGM
00622          MOVE WS-ABCODE-CDP1        TO  WS-ABCODE                 GACDEPGM
00623          MOVE WS-ABCODE-CDP1-MSG    TO  WS-ABCODE-MSG             GACDEPGM
00624          PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   GACDEPGM
00625                                                                   GACDEPGM
00626      EXEC CICS  RETURN  END-EXEC.                                 GACDEPGM
00627                                                                   GACDEPGM
00628      GOBACK.                                                      GACDEPGM
00629                                                                   GACDEPGM
00630  0000-900-EXIT.      EXIT.                                        GACDEPGM
00631 /*****************************************************************GACDEPGM
00632 *  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *GACDEPGM
00633 *                                                                *GACDEPGM
00634 *        FUNCTIONS:                                              *GACDEPGM
00635 *          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *GACDEPGM
00636 *          2. IF GROUP IS CRITICAL:                              *GACDEPGM
00637 *              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *GACDEPGM
00638 *                BENEFIT PROVISION.                              *GACDEPGM
00639 *                - IF ON DATA BASE:                              *GACDEPGM
00640 *                  - SCAN FOR TABULAR ID(#ABM,#ACL,#ADL,#AOL OR  *GACDEPGM
00641 *                                        #ACP)                   *GACDEPGM
00642 *                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *GACDEPGM
00643 *                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *GACDEPGM
00644 *                      ON SCREEN AND ISSUE MESSAGE.              *GACDEPGM
00645 ******************************************************************GACDEPGM
00646  4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          GACDEPGM
00647                                                                   GACDEPGM
00648 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*GACDEPGM
00649 *                IF EFFECTIVE DATE GREATER THAN TODAYS DATE      *GACDEPGM
00650 *                CRITICAL DATA ELEMENTS ARE NON-CRITICAL.        *GACDEPGM
00651 *                                                                *GACDEPGM
00652      IF  FRMNUIDI  =  'GS3A'                                      GACDEPGM
00653          MOVE  IDLINEI          TO GROUP-SPECIFIC-ID-LINE.        GACDEPGM
00654 *        MOVE GRP-SPEC-EFF-DATE TO HGADATE-DATE1.                 GACDEPGM
00655      IF  FRMNUIDI  =  'GC4A'  OR 'GTM1'                           GACDEPGM
00656          MOVE IDLINEI           TO  CONTRACT-ID-LINE.             GACDEPGM
00657 *        MOVE CONTRACT-EFF-DATE TO  HGADATE-DATE1.                GACDEPGM
00658      IF  FRMNUIDI  =  'GC8A'                                      GACDEPGM
00659          MOVE IDLINEI           TO  BENEFIT-PROVISION-ID-LINE.    GACDEPGM
00660 *        MOVE BEN-PROV-EFF-DATE TO  HGADATE-DATE1.                GACDEPGM
00661                                                                   GACDEPGM
00662      MOVE SPACES  TO  IDPRODO.                                    GACDEPGM
00663                                                                   GACDEPGM
00664 *    PERFORM  9200-000-GREGORIAN-TO-JULIAN.                       GACDEPGM
00665      MOVE GCA-EFFDT-CEN TO WS-EIBDATE-CEN.                        GACDEPGM
00666      IF WS-EIBDATE-DT  <  70000                                   GACDEPGM
00667          MOVE 0  TO  WS-EIBDATE-1                                 GACDEPGM
00668          MOVE 1  TO  WS-EIBDATE-2                                 GACDEPGM
00669      ELSE                                                         GACDEPGM
00670          MOVE 0  TO  WS-EIBDATE-1                                 GACDEPGM
00671          MOVE 0  TO  WS-EIBDATE-2.                                GACDEPGM
00672      IF WS-EIBDATE-CEN  >  EIBDATE                                GACDEPGM
00673         GO TO 4500-900-EXIT.                                      GACDEPGM
00674 *                                                                *GACDEPGM
00675 *                                                                *GACDEPGM
00676 *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*GACDEPGM
00677                                                                   GACDEPGM
00678                                                                   GACDEPGM
00679 **************************************                            GACDEPGM
00680 *    ** BUILD GCTRSRT COMMAREA **    *                            GACDEPGM
00681 **************************************                            GACDEPGM
00682                                                                   GACDEPGM
00683 *    MOVE ZEROS                 TO CDE1-GROUP-NO-ZERO-PAD.        GACDEPGM
00684                                                                   GACDEPGM
00685 *    IF  FRMNUIDI  =  'GS3A'                                      GACDEPGM
00686 *        MOVE GRP-SPEC-GROUP-NO TO CDE1-GROUP-NO-6.               GACDEPGM
00687 *    IF  FRMNUIDI  =  'GC4A'  OR 'GTM1'                           GACDEPGM
00688 *        MOVE CONTRACT-GROUP-NO TO CDE1-GROUP-NO-6.               GACDEPGM
00689 *    IF  FRMNUIDI  =  'GC8A'                                      GACDEPGM
00690 *        MOVE BEN-PROV-GROUP-NO TO CDE1-GROUP-NO-6.               GACDEPGM
00691                                                                   GACDEPGM
00692      MOVE GCA-GROUP-NO-1-3      TO CDE1-GROUP-NO-ZERO-PAD.        GACDEPGM
00693      MOVE GCA-GRP-NO            TO CDE1-GROUP-NO-6.               GACDEPGM
00694                                                                   GACDEPGM
00695      MOVE SPACES                TO CDE1-MESSAGE.                  GACDEPGM
00696      MOVE ZERO                  TO CDE1-RETURN-CODE.              GACDEPGM
00697      MOVE SPACES                TO CDE1-FILLER.                   GACDEPGM
00698                                                                   GACDEPGM
00699 **************************************                            GACDEPGM
00700 *    ** CALL TRANS ROUTING MODULE ** *                            GACDEPGM
00701 **************************************                            GACDEPGM
00702                                                                   GACDEPGM
00703      EXEC CICS  LINK  PROGRAM('GCTRSRT')                          GACDEPGM
00704                 COMMAREA (GCTRSRT-COMMAREA)                       GACDEPGM
00705                 LENGTH   (LENGTH OF GCTRSRT-COMMAREA)             GACDEPGM
00706                 END-EXEC.                                         GACDEPGM
00707                                                                   GACDEPGM
00708      IF  CDE1-TECHNICAL-PROBLEM                                   GACDEPGM
00709      THEN                                                         GACDEPGM
00710          IF  ERRMSGO > SPACES                                     GACDEPGM
00711          THEN                                                     GACDEPGM
00712              GO TO 4500-900-EXIT                                  GACDEPGM
00713          ELSE                                                     GACDEPGM
00714              MOVE CDE1-MESSAGE TO ERRMSGO                         GACDEPGM
00715              GO TO 4500-900-EXIT                                  GACDEPGM
00716      ELSE                                                         GACDEPGM
00717          NEXT SENTENCE.                                           GACDEPGM
00718                                                                   GACDEPGM
00719      IF  CDE1-GROUP-IS-NOT-CRITICAL                               GACDEPGM
00720          GO TO 4500-900-EXIT.                                     GACDEPGM
00721                                                                   GACDEPGM
00722      IF  FRMNUIDI  =  'GS3A'                                      GACDEPGM
00723          GO TO 4500-100-READ-GRPSPC.                              GACDEPGM
00724                                                                   GACDEPGM
00725      IF  FRMNUIDI  =  'GC4A'  OR 'GTM1' OR   'GC8A'               GACDEPGM
00726          GO TO 4500-200-READ-CONTRACT.                            GACDEPGM
00727                                                                   GACDEPGM
00728                                                                   GACDEPGM
00729 **************************************                            GACDEPGM
00730 *  1. READ GROUP SPECIFIC            *                            GACDEPGM
00731 *  2. SCAN GROUP SPECIFIC FOR TAB ID *                            GACDEPGM
00732 *     (#ABM, #ACL, #ADL, #AOL, OR    *                            GACDEPGM
00733 *      #ACP)                         *                            GACDEPGM
00734 *     THAT MATCHES THE TABULAR ID    *                            GACDEPGM
00735 *     FROM THE W/F                   *                            GACDEPGM
00736 *     ALL LEVEL TABULAR RECORD.      *                            GACDEPGM
00737 *  3. HIDE PROD SLOT# ON SCREEN      *                            GACDEPGM
00738 **************************************                            GACDEPGM
00739                                                                   GACDEPGM
00740  4500-100-READ-GRPSPC.                                            GACDEPGM
00741                                                                   GACDEPGM
00742      MOVE GCA-PLAN-CODE         TO  GCIO-GRP-PLAN-CODE.           GACDEPGM
00743      MOVE GCA-GROUP-NUM         TO  GCIO-GRP-GROUP-NUM.           GACDEPGM
00744      MOVE GCA-SECTION-NUM       TO  GCIO-GRP-SECTION-NUM.         GACDEPGM
00745      MOVE GCA-PKG-CODE          TO  GCIO-GRP-PKG-CODE.            GACDEPGM
00746      MOVE GCA-FAM-REL-LVL       TO  GCIO-GRP-FAMILY-RELATION-LVL. GACDEPGM
00747      MOVE GCA-EFFDT-CEN         TO  GCIO-GRP-EFFDT-CEN.           GACDEPGM
00748                                                                   GACDEPGM
00749      EXEC CICS GETMAIN                                            GACDEPGM
00750                SET(ADDRESS OF PR-IO-PARM-WRK-GRP-SPEC-REC)        GACDEPGM
00751                INITIMG(WS-HEX-00)                                 GACDEPGM
00752                LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)                GACDEPGM
00753                END-EXEC.                                          GACDEPGM
00754                                                                   GACDEPGM
00755      MOVE GC-GCGRPSPC-DDNAME     TO GCIO8-FILE-DDNAME.            GACDEPGM
00756      MOVE GCIO-GROUP-SPECIFIC    TO GCIO8-FILE-KEY.               GACDEPGM
00757      MOVE GC-GCIO-ACCESS-CODE-RD TO GCIO8-FILE-ACCESS-CODE.       GACDEPGM
00758      MOVE GC-GCIO-AREA-2         TO GCIO8-IO-AREA-TO-USE.         GACDEPGM
00759      MOVE GC-GCGRPSPC-VARY-MAX-OCUR  TO                           GACDEPGM
00760           GCG2-COUNT-TAB-PROVN-POINTERS.                          GACDEPGM
00761                                                                   GACDEPGM
00762      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GACDEPGM
00763                 COMMAREA(PR-IO-PARM-WRK-GRP-SPEC-REC)             GACDEPGM
00764                 LENGTH(WS-IO-PARM-WRK-GRP-SPEC-LEN)  END-EXEC.    GACDEPGM
00765                                                                   GACDEPGM
00766      IF NOT GCIO8-GOOD-RETURN                                     GACDEPGM
00767         GO TO 4500-900-EXIT.                                      GACDEPGM
00768                                                                   GACDEPGM
00769      MOVE GCG2-COUNT-TAB-PROVN-POINTERS                           GACDEPGM
00770        TO GCG2-COUNT-TAB-PROVN-POINTERS.                          GACDEPGM
00771                                                                   GACDEPGM
00772      SET GCG2-INDEX TO +1.                                        GACDEPGM
00773      SEARCH  GCG2-GRP-SPEC-TAB-ID                                 GACDEPGM
00774            AT END                                                 GACDEPGM
00775                  MOVE ZEROS                        TO PRDSLTNO    GACDEPGM
00776                  GO TO 4500-900-EXIT                              GACDEPGM
00777            WHEN  GCG2-TAB-ID(GCG2-INDEX) = TABIDI                 GACDEPGM
00778                  MOVE GCG2-TAB-SLOT-NO(GCG2-INDEX) TO PRDSLTNO    GACDEPGM
00779                  PERFORM 5000-000-READ-PROD-ALL-LVL-TAB           GACDEPGM
00780                  GO TO 4500-700-CDE-ARE-CRITICAL.                 GACDEPGM
00781                                                                   GACDEPGM
00782                                                                   GACDEPGM
00783 **************************************                            GACDEPGM
00784 *  1. READ CONTRACT                  *                            GACDEPGM
00785 *  2. SCAN CONTRACT FOR TABULAR ID   *                            GACDEPGM
00786 *     (#ABM, #ACL, #ADL, #AOL OR     *                            GACDEPGM
00787 *      #ACP)                         *                            GACDEPGM
00788 *     THAT MATCHES THE TABULAR ID    *                            GACDEPGM
00789 *     FROM THE W/F                   *                            GACDEPGM
00790 *     ALL LEVEL TABULAR RECORD.      *                            GACDEPGM
00791 *  3. HIDE PROD SLOT# ON SCREEN      *                            GACDEPGM
00792 **************************************                            GACDEPGM
00793                                                                   GACDEPGM
00794  4500-200-READ-CONTRACT.                                          GACDEPGM
00795                                                                   GACDEPGM
00796      IF  FRMNUIDI  =  'GC4A'  OR 'GTM1'                           GACDEPGM
00797         MOVE GCA-PLAN-CODE         TO  GCIO-CON-PLAN-CODE         GACDEPGM
00798         MOVE GCA-GROUP-NUM         TO  GCIO-CON-GROUP-NUM         GACDEPGM
00799         MOVE GCA-SECTION-NUM       TO  GCIO-CON-SECTION-NUM       GACDEPGM
00800         MOVE GCA-PKG-CODE          TO  GCIO-CON-PKG-CODE          GACDEPGM
00801         MOVE GCA-L-O-B             TO  GCIO-CON-LINE-OF-BUS       GACDEPGM
00802         MOVE GCA-PROV-CTL          TO  GCIO-CON-PROVIDER-CONTROL  GACDEPGM
00803         MOVE GCA-FAM-REL-LVL      TO  GCIO-CON-FAMILY-RELATION-LVLGACDEPGM
00804         MOVE GCA-EFFDT-CEN         TO  GCIO-CON-EFFDT-CEN         GACDEPGM
00805      ELSE                                                         GACDEPGM
00806         MOVE GCA-PLAN-CODE         TO  GCIO-CON-PLAN-CODE         GACDEPGM
00807         MOVE GCA-GROUP-NUM         TO  GCIO-CON-GROUP-NUM         GACDEPGM
00808         MOVE GCA-SECTION-NUM       TO  GCIO-CON-SECTION-NUM       GACDEPGM
00809         MOVE GCA-PKG-CODE          TO  GCIO-CON-PKG-CODE          GACDEPGM
00810         MOVE GCA-L-O-B             TO  GCIO-CON-LINE-OF-BUS       GACDEPGM
00811         MOVE GCA-PROV-CTL          TO  GCIO-CON-PROVIDER-CONTROL  GACDEPGM
00812         MOVE GCA-FAM-REL-LVL      TO  GCIO-CON-FAMILY-RELATION-LVLGACDEPGM
00813         MOVE GCA-EFFDT-CEN         TO  GCIO-CON-EFFDT-CEN.        GACDEPGM
00814                                                                   GACDEPGM
00815      EXEC CICS GETMAIN                                            GACDEPGM
00816                SET(ADDRESS OF PR-IO-PARM-WRK-CONTRACT-REC)        GACDEPGM
00817                INITIMG(WS-HEX-00)                                 GACDEPGM
00818                LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)                GACDEPGM
00819                END-EXEC.                                          GACDEPGM
00820                                                                   GACDEPGM
00821      MOVE GC-GCCONTR-DDNAME       TO  GCIO7-FILE-DDNAME.          GACDEPGM
00822      MOVE GCIO-CONTRACT-FILE-KEY  TO  GCIO7-FILE-KEY.             GACDEPGM
00823      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIO7-FILE-ACCESS-CODE.     GACDEPGM
00824      MOVE GC-GCIO-AREA-2          TO  GCIO7-IO-AREA-TO-USE.       GACDEPGM
00825      MOVE GC-GCCONTR-VARY-MAX-OCUR  TO                            GACDEPGM
00826           GCT2-COUNT-BEN-PROVN-POINTERS.                          GACDEPGM
00827                                                                   GACDEPGM
00828      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GACDEPGM
00829                 COMMAREA(PR-IO-PARM-WRK-CONTRACT-REC)             GACDEPGM
00830                 LENGTH(WS-IO-PARM-WRK-CONTRACT-LEN)  END-EXEC.    GACDEPGM
00831                                                                   GACDEPGM
00832      IF NOT GCIO7-GOOD-RETURN                                     GACDEPGM
00833         GO TO 4500-900-EXIT.                                      GACDEPGM
00834                                                                   GACDEPGM
00835      IF FRMNUIDI  =  'GC8A'                                       GACDEPGM
00836         GO TO 4500-300-READ-BENPROV.                              GACDEPGM
00837                                                                   GACDEPGM
00838      SET GCT2-TAB-INDEX TO +1                                     GACDEPGM
00839      SEARCH  GCT2-CON-TAB-ID-SLOT                                 GACDEPGM
00840            AT END                                                 GACDEPGM
00841                  MOVE ZEROS                        TO PRDSLTNO    GACDEPGM
00842                  GO TO 4500-900-EXIT                              GACDEPGM
00843            WHEN  GCT2-CON-TAB-ID(GCT2-TAB-INDEX) = TABIDI         GACDEPGM
00844                  AND   GCT2-CON-TAB-SLOT(GCT2-TAB-INDEX) > ZERO   GACDEPGM
00845                  MOVE  GCT2-CON-TAB-SLOT(GCT2-TAB-INDEX)          GACDEPGM
00846                                                    TO PRDSLTNO    GACDEPGM
00847                  PERFORM 5000-000-READ-PROD-ALL-LVL-TAB           GACDEPGM
00848                  GO TO 4500-700-CDE-ARE-CRITICAL.                 GACDEPGM
00849                                                                   GACDEPGM
00850                                                                   GACDEPGM
00851 ********************************************                      GACDEPGM
00852 *  1. READ BENEFIT PROVISION FROM WORKFILE *                      GACDEPGM
00853 *  2. READ BENEFIT PROVISION FROM POOL     *                      GACDEPGM
00854 *  3. SCAN BEN PROV FOR TABULAR ID         *                      GACDEPGM
00855 *     (#ABM, #ACL, #ADL, *AOL OR           *                      GACDEPGM
00856 *      #ACP)                               *                      GACDEPGM
00857 *     THAT MATCHES ON THE TABULAR ID FROM  *                      GACDEPGM
00858 *     THE W/F ALL LEVEL TABULAR RECORD.    *                      GACDEPGM
00859 *  4. HIDE PROD SLOT# ON SCREEN            *                      GACDEPGM
00860 ********************************************                      GACDEPGM
00861                                                                   GACDEPGM
00862  4500-300-READ-BENPROV.                                           GACDEPGM
00863                                                                   GACDEPGM
00864      EXEC CICS GETMAIN                                            GACDEPGM
00865                SET(ADDRESS OF PR-IO-PARM-WRK-BEN-PROV-REC)        GACDEPGM
00866                INITIMG(WS-HEX-00)                                 GACDEPGM
00867                LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)                GACDEPGM
00868                END-EXEC.                                          GACDEPGM
00869                                                                   GACDEPGM
00870      MOVE GCT2-COUNT-BEN-PROVN-POINTERS  TO                       GACDEPGM
00871           GCT2-COUNT-BEN-PROVN-POINTERS.                          GACDEPGM
00872                                                                   GACDEPGM
00873      SET GCT2-INDEX   TO   +1.                                    GACDEPGM
00874      SEARCH  GCT2-BEN-PROVN                                       GACDEPGM
00875         AT END                                                    GACDEPGM
00876              MOVE ZEROS  TO  PRDSLTNO                             GACDEPGM
00877              GO TO 4500-900-EXIT                                  GACDEPGM
00878         WHEN GCT2-BEN-PROVN-ID(GCT2-INDEX)  =  BEN-PROV-ID-NO     GACDEPGM
00879              MOVE GCT2-BEN-PROVN-SLOT-NO(GCT2-INDEX)  TO          GACDEPGM
00880                                     GCIO-BEN-PROVISION-SLOT-NO.   GACDEPGM
00881      MOVE GCA-BEN-PROV-ID       TO  GCIO-BEN-PROVISION-ID.        GACDEPGM
00882                                                                   GACDEPGM
00883      MOVE GC-GCBENPRV-DDNAME      TO  GCIO9-FILE-DDNAME.          GACDEPGM
00884      MOVE GCIO-BEN-PROV-POOL      TO  GCIO9-FILE-KEY.             GACDEPGM
00885      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIO9-FILE-ACCESS-CODE.     GACDEPGM
00886      MOVE GC-GCIO-AREA-2          TO  GCIO9-IO-AREA-TO-USE.       GACDEPGM
00887      MOVE GC-GCBENPRV-VARY-MAX-OCUR  TO                           GACDEPGM
00888           GCP2-COUNT-TAB-PROVN-POINTERS.                          GACDEPGM
00889                                                                   GACDEPGM
00890 *---- READ BENEFIT PROVISION FROM POOL                            GACDEPGM
00891                                                                   GACDEPGM
00892      EXEC CICS  LINK  PROGRAM ('GCIOPGM')                         GACDEPGM
00893                 COMMAREA(PR-IO-PARM-WRK-BEN-PROV-REC)             GACDEPGM
00894                 LENGTH(WS-IO-PARM-WRK-BEN-PROV-LEN)  END-EXEC.    GACDEPGM
00895                                                                   GACDEPGM
00896      IF NOT GCIO9-GOOD-RETURN                                     GACDEPGM
00897         MOVE ZEROS  TO  PRDSLTNO                                  GACDEPGM
00898         GO TO 4500-900-EXIT.                                      GACDEPGM
00899                                                                   GACDEPGM
00900                                                                   GACDEPGM
00901 *---- SCAN BEN PROV FOR ACCUM TABULAR ID                          GACDEPGM
00902                                                                   GACDEPGM
00903      MOVE GCP2-COUNT-TAB-PROVN-POINTERS  TO                       GACDEPGM
00904           GCP2-COUNT-TAB-PROVN-POINTERS.                          GACDEPGM
00905      SET GCP2-INDEX  TO  +1.                                      GACDEPGM
00906      SEARCH  GCP2-BEN-TAB-PROVN-ID                                GACDEPGM
00907         AT END                                                    GACDEPGM
00908            MOVE ZEROS  TO  PRDSLTNO                               GACDEPGM
00909            GO TO 4500-900-EXIT                                    GACDEPGM
00910         WHEN  GCP2-BP-ID(GCP2-INDEX)  =  TABIDI                   GACDEPGM
00911            MOVE  GCP2-BP-SLOT-NO(GCP2-INDEX)  TO  PRDSLTNO        GACDEPGM
00912            PERFORM 5000-000-READ-PROD-ALL-LVL-TAB                 GACDEPGM
00913            GO TO 4500-700-CDE-ARE-CRITICAL.                       GACDEPGM
00914                                                                   GACDEPGM
00915                                                                   GACDEPGM
00916  4500-700-CDE-ARE-CRITICAL.                                       GACDEPGM
00917 *--------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS                GACDEPGM
00918      MOVE '+CDE+'   TO  CDEINDO.                                  GACDEPGM
00919      MOVE OENTCTRI  TO  ACWA-DISPLAY-LEN-7-X.                     GACDEPGM
00920                                                                   GACDEPGM
00921      MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA           GACDEPGM
00922                       PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA       GACDEPGM
00923                       COPAYITA  INTDESTA CONDTG1A  CONDTG2A       GACDEPGM
00924           BISNDITA  AGEQLTA  AGELIMA.                             GACDEPGM
00925                                                                   GACDEPGM
00926      IF FUNCTONI  =  'GA1B'                                       GACDEPGM
00927         SEARCH GAA2-ENTRY                                         GACDEPGM
00928            VARYING GAA2-INDEX                                     GACDEPGM
00929            WHEN                                                   GACDEPGM
00930               GAA2-INDEX  NOT <  GAA2-ENTRY-COUNT OR              GACDEPGM
00931               GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX)  =            GACDEPGM
00932                                               ACWA-DISPLAY-LEN-7  GACDEPGM
00933               NEXT SENTENCE.                                      GACDEPGM
00934                                                                   GACDEPGM
00935      IF FUNCTONI  =  'GA1B'                                       GACDEPGM
00936         IF GAA2-INDEX  <  GAA2-ENTRY-COUNT AND                    GACDEPGM
00937            GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX)  =               GACDEPGM
00938                                            ACWA-DISPLAY-LEN-7     GACDEPGM
00939            MOVE GAA2-BAMA-INTERNAL-DESCRIPTOR(GAA2-INDEX)  TO     GACDEPGM
00940                                                           IDPRODO.GACDEPGM
00941                                                                   GACDEPGM
00942                                                                   GACDEPGM
00943      IF FUNCTONI  =  'GA1C'                                       GACDEPGM
00944         MOVE DFHBMABF  TO  MANAPLTA  PERLITTA                     GACDEPGM
00945         SEARCH GAB2-ENTRY                                         GACDEPGM
00946            VARYING GAB2-INDEX                                     GACDEPGM
00947            WHEN                                                   GACDEPGM
00948               GAB2-INDEX  NOT <  GAB2-ENTRY-COUNT OR              GACDEPGM
00949               GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX)  =            GACDEPGM
00950                                               ACWA-DISPLAY-LEN-7  GACDEPGM
00951               NEXT SENTENCE.                                      GACDEPGM
00952                                                                   GACDEPGM
00953      IF FUNCTONI  =  'GA1C'                                       GACDEPGM
00954         IF GAB2-INDEX  <  GAB2-ENTRY-COUNT AND                    GACDEPGM
00955            GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX)  =               GACDEPGM
00956                                               ACWA-DISPLAY-LEN-7  GACDEPGM
00957            MOVE GAB2-COINS-INTERNAL-DESCRIPTOR(GAB2-INDEX)  TO    GACDEPGM
00958                                                           IDPRODO.GACDEPGM
00959                                                                   GACDEPGM
00960                                                                   GACDEPGM
00961      IF FUNCTONI  =  'GA1D'                                       GACDEPGM
00962         MOVE DFHBMABF  TO  MANAPLTA                               GACDEPGM
00963         SEARCH GAC2-ENTRY                                         GACDEPGM
00964            VARYING GAC2-INDEX                                     GACDEPGM
00965            WHEN                                                   GACDEPGM
00966               GAC2-INDEX  NOT <  GAC2-ENTRY-COUNT OR              GACDEPGM
00967               GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX)  =            GACDEPGM
00968                                               ACWA-DISPLAY-LEN-7  GACDEPGM
00969               NEXT SENTENCE.                                      GACDEPGM
00970                                                                   GACDEPGM
00971      IF FUNCTONI  =  'GA1D'                                       GACDEPGM
00972         IF GAC2-INDEX  <  GAC2-ENTRY-COUNT AND                    GACDEPGM
00973            GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX)  =               GACDEPGM
00974                                            ACWA-DISPLAY-LEN-7     GACDEPGM
00975            MOVE GAC2-DEDL-INTERNAL-DESCRIPTOR(GAC2-INDEX)  TO     GACDEPGM
00976                                                           IDPRODO.GACDEPGM
00977                                                                   GACDEPGM
00978                                                                   GACDEPGM
00979      IF FUNCTONI  =  'GA1E'                                       GACDEPGM
00980         MOVE DFHBMABF  TO  PERLITTA                               GACDEPGM
00981         SEARCH GAD2-ENTRY                                         GACDEPGM
00982            VARYING GAD2-INDEX                                     GACDEPGM
00983            WHEN                                                   GACDEPGM
00984               GAD2-INDEX  NOT <  GAD2-ENTRY-COUNT OR              GACDEPGM
00985               GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)  =            GACDEPGM
00986                                               ACWA-DISPLAY-LEN-7  GACDEPGM
00987               NEXT SENTENCE.                                      GACDEPGM
00988                                                                   GACDEPGM
00989      IF FUNCTONI  =  'GA1E'                                       GACDEPGM
00990         IF GAD2-INDEX  <  GAD2-ENTRY-COUNT AND                    GACDEPGM
00991            GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)  =               GACDEPGM
00992                                               ACWA-DISPLAY-LEN-7  GACDEPGM
00993            MOVE GAD2-O-P-X-INTERNAL-DESCRIPTOR(GAD2-INDEX)  TO    GACDEPGM
00994                                                           IDPRODO.GACDEPGM
00995                                                                   GACDEPGM
00996                                                                   GACDEPGM
00997      IF FUNCTONI  =  'GA1P'                                       GACDEPGM
00998         MOVE DFHBMABF  TO  MANAPLTA                               GACDEPGM
00999         SEARCH GAF2-ENTRY                                         GACDEPGM
01000            VARYING GAF2-INDEX                                     GACDEPGM
01001            WHEN                                                   GACDEPGM
01002               GAF2-INDEX  NOT <  GAF2-ENTRY-COUNT OR              GACDEPGM
01003               GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX)  =            GACDEPGM
01004                                               ACWA-DISPLAY-LEN-7  GACDEPGM
01005               NEXT SENTENCE.                                      GACDEPGM
01006                                                                   GACDEPGM
01007      IF FUNCTONI  =  'GA1P'                                       GACDEPGM
01008         IF GAF2-INDEX  <  GAF2-ENTRY-COUNT AND                    GACDEPGM
01009            GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX)  =               GACDEPGM
01010                                            ACWA-DISPLAY-LEN-7     GACDEPGM
01011            MOVE GAF2-COPAY-INTERNAL-DESCRIPTOR(GAF2-INDEX) TO     GACDEPGM
01012                                                           IDPRODO.GACDEPGM
01013                                                                   GACDEPGM
01014                                                                   GACDEPGM
01015      IF INTDESKI  NOT =  IDPRODO                                  GACDEPGM
01016         MOVE DFHBMABF  TO  IBGRIDA  IBGRSLTA  IDGDIDA IDGDSLTA    GACDEPGM
01017         IPGNIDA  IPGNSLTA IPGTIDA  IPGTSLTA  IPGPIDA  IPGPSLTA.   GACDEPGM
01018                                                                   GACDEPGM
01019 *------------ READ W/F CONTROL RECORD                             GACDEPGM
01020      PERFORM 5300-READ-CONTROL-REC.                               GACDEPGM
01021                                                                   GACDEPGM
01022      IF NOT GCIO6-GOOD-RETURN                                     GACDEPGM
01023         MOVE WS-ABCODE-CDFC        TO  WS-ABCODE                  GACDEPGM
01024         MOVE WS-ABCODE-CDFC-MSG    TO  WS-ABCODE-MSG              GACDEPGM
01025         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
01026                                                                   GACDEPGM
01027      IF  WRK6-CDE-SP  =  '2 ' OR  '1U'                            GACDEPGM
01028          GO TO 4500-900-EXIT.                                     GACDEPGM
01029                                                                   GACDEPGM
01030      MOVE '+CDE-'  TO  CDEINDO.                                   GACDEPGM
01031                                                                   GACDEPGM
01032      IF  ERRMSGO  >  SPACES                                       GACDEPGM
01033      THEN                                                         GACDEPGM
01034          NEXT SENTENCE                                            GACDEPGM
01035      ELSE                                                         GACDEPGM
01036          SET  WT-01-INDEX                     TO +03              GACDEPGM
01037          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO.         GACDEPGM
01038                                                                   GACDEPGM
01039 *--------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS               GACDEPGM
01040      IF INTDESKI  NOT =  IDPRODO                                  GACDEPGM
01041         MOVE DFHBMASF  TO  IBGROPTA,  IPGNOPTA,  IPGTOPTA,        GACDEPGM
01042                            IDGDOPTA,  IPGPOPTA.                   GACDEPGM
01043                                                                   GACDEPGM
01044      MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA           GACDEPGM
01045            AGEQLLA    PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA       GACDEPGM
01046            AGELIMLA   COPAYINA  INTDESKA CONDALLA  CONDEXCA       GACDEPGM
01047            AGELIMHA   CONDICDA  CONDTABA CONDMENA  CONDDRGA       GACDEPGM
01048            AGEQLHA    CONDALCA  CONDOBCA CONDOBNA  CONDMALA       GACDEPGM
01049             CONDTMJA  CONDCARA  CONDOBSA CONDKDYA  CONDACCA       GACDEPGM
01050             CONDLIFA                                              GACDEPGM
01051             CONDINFA  CONDPECA  CONDNEMA CONDSUIA  BISNDINA       GACDEPGM
01052             CONDEMCA  CONDEACA  CONDSMIA CONDNSMA.                GACDEPGM
01053                                                                   GACDEPGM
01054      IF  FUNCTONI = 'GA1B'                                        GACDEPGM
01055          NEXT SENTENCE                                            GACDEPGM
01056      ELSE                                                         GACDEPGM
01057          IF  FUNCTONI = 'GA1C'                                    GACDEPGM
01058              MOVE DFHBMASF TO MANAPLIA  PERLIMTA                  GACDEPGM
01059          ELSE                                                     GACDEPGM
01060              IF  FUNCTONI = 'GA1D'                                GACDEPGM
01061                  MOVE DFHBMASF TO MANAPLIA                        GACDEPGM
01062              ELSE                                                 GACDEPGM
01063                  IF  FUNCTONI = 'GA1E'                            GACDEPGM
01064                      MOVE DFHBMASF TO PERLIMTA                    GACDEPGM
01065                  ELSE                                             GACDEPGM
01066                      IF  FUNCTONI = 'GA1P'                        GACDEPGM
01067                      MOVE DFHBMASF TO MANAPLIA.                   GACDEPGM
01068                                                                   GACDEPGM
01069                                                                   GACDEPGM
01070  4500-900-EXIT.      EXIT.                                        GACDEPGM
01071                                                                   GACDEPGM
01072 /*****************************************************************GACDEPGM
01073 *  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *GACDEPGM
01074 *                                                                *GACDEPGM
01075 *        FUNCTIONS:                                              *GACDEPGM
01076 *           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *GACDEPGM
01077 *           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *GACDEPGM
01078 *               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *GACDEPGM
01079 *           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *GACDEPGM
01080 *               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *GACDEPGM
01081 *               +CDE+ INDICATOR (POSITION=8).                    *GACDEPGM
01082 *              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *GACDEPGM
01083 *               ENTER, CONTINUE PROCESSING.                      *GACDEPGM
01084 *           4. COMPARE OCCURS FROM W/F ALL LEVEL TAB WITH THES   *GACDEPGM
01085 *              OCCRS FROM PRODUCTION IF NOT FOUND IT IS AN ADD   *GACDEPGM
01086 *              FLAG THE W/F RECORD WITH 1U.                      *GACDEPGM
01087 ******************************************************************GACDEPGM
01088  4600-000-UPDATE-CDE-STATUS     SECTION.                          GACDEPGM
01089                                                                   GACDEPGM
01090      MOVE 'N'    TO ACWA-CDE-FIELD-CHANGE-IND.                    GACDEPGM
01091      MOVE '  '   TO ACWA-CDE-STATUS-CHANGE-IND.                   GACDEPGM
01092                                                                   GACDEPGM
01093 *------------- READ ALL LEVEL TABULAR FROM PROVISION POOL         GACDEPGM
01094      PERFORM 5000-000-READ-PROD-ALL-LVL-TAB.                      GACDEPGM
01095                                                                   GACDEPGM
01096 *   *-------------------------------------------------------*     GACDEPGM
01097 *   *  COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE     *     GACDEPGM
01098 *   *  ON THE PROVISION POOL ALL LVL TABULAR RECORD.        *     GACDEPGM
01099 *   *-------------------------------------------------------*     GACDEPGM
01100                                                                   GACDEPGM
01101      IF FUNCTONI  =  'GA1B'                                       GACDEPGM
01102         MOVE OENTCTRO  TO  ACWA-DISPLAY-LEN-7                     GACDEPGM
01103         PERFORM 4610-000-COMP-GAA-CDE-ELEMENTS                    GACDEPGM
01104            VARYING GAA2-INDEX  FROM  1  BY  1                     GACDEPGM
01105            UNTIL GAA2-INDEX  NOT <  GAA2-ENTRY-COUNT.             GACDEPGM
01106                                                                   GACDEPGM
01107 ******** THE SEARCH STATEMENTS FOLLOWS IS TO CHECK IF THIS OCCRS  GACDEPGM
01108 *        HAS BEEN JUST INSERTED, IF IT IS WE MAKE SURE THAT THE   GACDEPGM
01109 *        THE ACCUM RECORD IS FLAGED 1U.   SEARCH PRODUCTION OCCRS GACDEPGM
01110 *        FOR EACH W/F ALL LVL TAB OCCUR   NE  03/08/88  ********* GACDEPGM
01111                                                                   GACDEPGM
01112      IF FUNCTONI  =  'GA1B'                                       GACDEPGM
01113         PERFORM 4650-000-FIND-IF-OCCR-ADDED                       GACDEPGM
01114            VARYING GAA-INDEX  FROM  1  BY  1                      GACDEPGM
01115            UNTIL GAA-INDEX  NOT <  GAA-ENTRY-COUNT.               GACDEPGM
01116 ***************                                                   GACDEPGM
01117                                                                   GACDEPGM
01118      IF FUNCTONI  =  'GA1C'                                       GACDEPGM
01119         MOVE OENTCTRO  TO  ACWA-DISPLAY-LEN-7                     GACDEPGM
01120         PERFORM 4620-000-COMP-GAB-CDE-ELEMENTS                    GACDEPGM
01121            VARYING GAB2-INDEX  FROM  1  BY  1                     GACDEPGM
01122            UNTIL GAB2-INDEX  NOT <  GAB2-ENTRY-COUNT.             GACDEPGM
01123                                                                   GACDEPGM
01124      IF FUNCTONI  =  'GA1C'                                       GACDEPGM
01125         PERFORM 4660-000-FIND-IF-OCCR-ADDED                       GACDEPGM
01126            VARYING GAB-INDEX  FROM  1  BY  1                      GACDEPGM
01127            UNTIL GAB-INDEX  NOT <  GAB-ENTRY-COUNT.               GACDEPGM
01128 ***************                                                   GACDEPGM
01129                                                                   GACDEPGM
01130      IF FUNCTONI  =  'GA1D'                                       GACDEPGM
01131         MOVE OENTCTRO  TO  ACWA-DISPLAY-LEN-7                     GACDEPGM
01132         PERFORM 4630-000-COMP-GAC-CDE-ELEMENTS                    GACDEPGM
01133            VARYING GAC2-INDEX  FROM  1  BY  1                     GACDEPGM
01134            UNTIL GAC2-INDEX  NOT <  GAC2-ENTRY-COUNT.             GACDEPGM
01135                                                                   GACDEPGM
01136      IF FUNCTONI  =  'GA1D'                                       GACDEPGM
01137         PERFORM 4670-000-FIND-IF-OCCR-ADDED                       GACDEPGM
01138            VARYING GAC-INDEX  FROM  1  BY  1                      GACDEPGM
01139            UNTIL GAC-INDEX  NOT <  GAC-ENTRY-COUNT.               GACDEPGM
01140 ***************                                                   GACDEPGM
01141                                                                   GACDEPGM
01142      IF FUNCTONI  =  'GA1E'                                       GACDEPGM
01143         MOVE OENTCTRO  TO  ACWA-DISPLAY-LEN-7                     GACDEPGM
01144         PERFORM 4640-000-COMP-GAD-CDE-ELEMENTS                    GACDEPGM
01145            VARYING GAD2-INDEX  FROM  1  BY  1                     GACDEPGM
01146            UNTIL GAD2-INDEX  NOT <  GAD2-ENTRY-COUNT.             GACDEPGM
01147                                                                   GACDEPGM
01148      IF FUNCTONI  =  'GA1E'                                       GACDEPGM
01149         PERFORM 4680-000-FIND-IF-OCCR-ADDED                       GACDEPGM
01150            VARYING GAD-INDEX  FROM  1  BY  1                      GACDEPGM
01151            UNTIL GAD-INDEX  NOT <  GAD-ENTRY-COUNT.               GACDEPGM
01152 ***************                                                   GACDEPGM
01153                                                                   GACDEPGM
01154      IF FUNCTONI  =  'GA1P'                                       GACDEPGM
01155         MOVE OENTCTRO  TO  ACWA-DISPLAY-LEN-7                     GACDEPGM
01156         PERFORM 4645-000-COMP-GAF-CDE-ELEMENTS                    GACDEPGM
01157            VARYING GAF2-INDEX  FROM  1  BY  1                     GACDEPGM
01158            UNTIL GAF2-INDEX  NOT <  GAF2-ENTRY-COUNT.             GACDEPGM
01159                                                                   GACDEPGM
01160      IF FUNCTONI  =  'GA1P'                                       GACDEPGM
01161         PERFORM 4690-000-FIND-IF-OCCR-ADDED                       GACDEPGM
01162            VARYING GAF-INDEX  FROM  1  BY  1                      GACDEPGM
01163            UNTIL GAF-INDEX  NOT <  GAF-ENTRY-COUNT.               GACDEPGM
01164 ***************                                                   GACDEPGM
01165                                                                   GACDEPGM
01166 *   *-------------------------------------------------------*     GACDEPGM
01167 *   *    IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN       *     GACDEPGM
01168 *   *       CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON   *     GACDEPGM
01169 *   *       +CDE+ INDICATOR (POSITION=8).                   *     GACDEPGM
01170 *   *                                                       *     GACDEPGM
01171 *   *    IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT    *     GACDEPGM
01172 *   *       ENTER, CONTINUE PROCESSING.                     *     GACDEPGM
01173 *   *-------------------------------------------------------*     GACDEPGM
01174                                                                   GACDEPGM
01175      IF ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED OR        GACDEPGM
01176         (WRK-SIGNAL-FROM-ONLINE  = 'W' AND                        GACDEPGM
01177         ACWA-FIELD-CHG-CNT  >  ZERO OR                            GACDEPGM
01178         (ACWA-FIELD-CHG-CNT  =  ZERO AND                          GACDEPGM
01179         DELADDI  =  'CHG/ADD'))                                   GACDEPGM
01180      THEN                                                         GACDEPGM
01181          IF  EIBCPOSN = 8                                         GACDEPGM
01182          THEN                                                     GACDEPGM
01183              NEXT SENTENCE                                        GACDEPGM
01184          ELSE                                                     GACDEPGM
01185              IF  EIBAID  =  DFHENTER                              GACDEPGM
01186              THEN                                                 GACDEPGM
01187                  MOVE WS-CDE-RETURN-DONT-SEND  TO                 GACDEPGM
01188                                               ACWA-CDE-RETURN-CODEGACDEPGM
01189                  SET  WT-01-INDEX                     TO +01      GACDEPGM
01190                  MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO  GACDEPGM
01191                  EXEC CICS  SEND  MAP('GA1XI01')  DATAONLY        GACDEPGM
01192                             MAPSET('GA1XSET')  CURSOR(8)  END-EXECGACDEPGM
01193                  EXEC CICS  RETURN  END-EXEC                      GACDEPGM
01194              ELSE                                                 GACDEPGM
01195                  SET  WT-01-INDEX                      TO  +02    GACDEPGM
01196                  MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGOGACDEPGM
01197                  EXEC CICS  SEND  MAP('GA1XI01')  DATAONLY        GACDEPGM
01198                             MAPSET('GA1XSET') CURSOR(8)  END-EXEC GACDEPGM
01199                  EXEC CICS  RETURN  END-EXEC                      GACDEPGM
01200      ELSE                                                         GACDEPGM
01201          NEXT SENTENCE.                                           GACDEPGM
01202                                                                   GACDEPGM
01203      IF  ACWA-CDE-FIELD-CHANGED OR  ACWA-CDE-REC-CHANGED          GACDEPGM
01204      THEN                                                         GACDEPGM
01205          IF  WRK-CDE-SP  =  '2 '                                  GACDEPGM
01206          THEN                                                     GACDEPGM
01207              ADD  1      TO    ACWA-CDE-1U-COUNT                  GACDEPGM
01208              SUBTRACT 1  FROM  ACWA-CDE-2B-COUNT                  GACDEPGM
01209              MOVE '1U'   TO    WRK-CDE-SP                         GACDEPGM
01210                                ACWA-CDE-STATUS-CHANGE-IND         GACDEPGM
01211          ELSE                                                     GACDEPGM
01212              MOVE '  '   TO    ACWA-CDE-STATUS-CHANGE-IND         GACDEPGM
01213      ELSE                                                         GACDEPGM
01214          IF  WRK-CDE-SP  =  '2 '                                  GACDEPGM
01215          THEN                                                     GACDEPGM
01216              MOVE '  '   TO    ACWA-CDE-STATUS-CHANGE-IND         GACDEPGM
01217          ELSE                                                     GACDEPGM
01218              IF  WRK-CDE-SP  =  '1U'                              GACDEPGM
01219                  ADD  1      TO    ACWA-CDE-2B-COUNT              GACDEPGM
01220                  SUBTRACT 1  FROM  ACWA-CDE-1U-COUNT              GACDEPGM
01221                  MOVE '2 '   TO    WRK-CDE-SP                     GACDEPGM
01222                                  ACWA-CDE-STATUS-CHANGE-IND.      GACDEPGM
01223                                                                   GACDEPGM
01224                                                                   GACDEPGM
01225  4600-900-EXIT.       EXIT.                                       GACDEPGM
01226                                                                   GACDEPGM
01227 /*****************************************************************GACDEPGM
01228 *  4610  -  COMPARE GAA CDE ELEMENTS (#ABM)                      *GACDEPGM
01229 *                                                                *GACDEPGM
01230 *        FUNCTIONS:                                              *GACDEPGM
01231 *                  1. FIND OCCURANCE WITHIN W/F ALL LVL TAB      *GACDEPGM
01232 *                     THAT MATCHS ALL LVL TAB OCCURANCE FROM     *GACDEPGM
01233 *                     PROVISION POOL.  IF THE OCCURANCE WAS      *GACDEPGM
01234 *                     DELETED INDICATED THAT THE RECORD HAS      *GACDEPGM
01235 *                     CHANGED.                                   *GACDEPGM
01236 *                  2. COMPARE CRITICAL DATA ELEMENTS IN THE      *GACDEPGM
01237 *                     TWO OCCURANCES FOR POSSIBLE CHANGES.       *GACDEPGM
01238 *                                                                *GACDEPGM
01239 *                  *. GAA  = W/F ALL LVL TAB                     *GACDEPGM
01240 *                     GAA2 = PRODUCTION ALL LVL TAB              *GACDEPGM
01241 ******************************************************************GACDEPGM
01242  4610-000-COMP-GAA-CDE-ELEMENTS SECTION.                          GACDEPGM
01243                                                                   GACDEPGM
01244      SET    GAA-INDEX TO 1.                                       GACDEPGM
01245                                                                   GACDEPGM
01246  4610-100-SEARCH.                                                 GACDEPGM
01247                                                                   GACDEPGM
01248      SEARCH GAA-ENTRY                                             GACDEPGM
01249         VARYING GAA-INDEX                                         GACDEPGM
01250         WHEN                                                      GACDEPGM
01251            GAA-INDEX  NOT <  GAA-ENTRY-COUNT OR                   GACDEPGM
01252            GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX) =                GACDEPGM
01253                               GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) GACDEPGM
01254            NEXT SENTENCE.                                         GACDEPGM
01255                                                                   GACDEPGM
01256      IF GAA-INDEX  <  GAA-ENTRY-COUNT AND                         GACDEPGM
01257         GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX) =                   GACDEPGM
01258                               GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) GACDEPGM
01259         PERFORM 4610-200-COMP-GAA-CDE-ELEMENTS                    GACDEPGM
01260      ELSE                                                         GACDEPGM
01261         PERFORM 4610-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01262                                                                   GACDEPGM
01263      GO TO 4610-900-EXIT.                                         GACDEPGM
01264                                                                   GACDEPGM
01265  4610-200-COMP-GAA-CDE-ELEMENTS.                                  GACDEPGM
01266                                                                   GACDEPGM
01267      IF   GAA-BAMA-CONDITION           (GAA-INDEX) =              GACDEPGM
01268          GAA2-BAMA-CONDITION           (GAA2-INDEX)      AND      GACDEPGM
01269           GAA-BAMA-BENEFIT-PERIOD      (GAA-INDEX) =              GACDEPGM
01270          GAA2-BAMA-BENEFIT-PERIOD      (GAA2-INDEX)      AND      GACDEPGM
01271           GAA-BAMA-CO-PAY-IND          (GAA-INDEX) =              GACDEPGM
01272          GAA2-BAMA-CO-PAY-IND          (GAA2-INDEX)      AND      GACDEPGM
01273           GAA-BAMA-COST-CONTAIN-IND    (GAA-INDEX) =              GACDEPGM
01274          GAA2-BAMA-COST-CONTAIN-IND    (GAA2-INDEX)      AND      GACDEPGM
01275           GAA-BAMA-FAM-OR-INDIV        (GAA-INDEX) =              GACDEPGM
01276          GAA2-BAMA-FAM-OR-INDIV        (GAA2-INDEX)      AND      GACDEPGM
01277           GAA-BAMA-INTERNAL-DESCRIPTOR (GAA-INDEX) =              GACDEPGM
01278          GAA2-BAMA-INTERNAL-DESCRIPTOR (GAA2-INDEX)      AND      GACDEPGM
01279           GAA-BAMA-L-O-B               (GAA-INDEX) =              GACDEPGM
01280          GAA2-BAMA-L-O-B               (GAA2-INDEX)      AND      GACDEPGM
01281           GAA-BAMA-PLACE-OF-TREATMENT  (GAA-INDEX) =              GACDEPGM
01282          GAA2-BAMA-PLACE-OF-TREATMENT  (GAA2-INDEX)      AND      GACDEPGM
01283           GAA-BAMA-SERVICE-GROUP       (GAA-INDEX) =              GACDEPGM
01284          GAA2-BAMA-SERVICE-GROUP       (GAA2-INDEX)      AND      GACDEPGM
01285           GAA-BAMA-AGE-QUAL-IND-FROM   (GAA-INDEX) =              GACDEPGM
01286          GAA2-BAMA-AGE-QUAL-IND-FROM   (GAA2-INDEX)      AND      GACDEPGM
01287           GAA-BAMA-AGE-QUAL-IND-TO     (GAA-INDEX) =              GACDEPGM
01288          GAA2-BAMA-AGE-QUAL-IND-TO     (GAA2-INDEX)      AND      GACDEPGM
01289           GAA-BAMA-AGE-LIMIT-FROM      (GAA-INDEX) =              GACDEPGM
01290          GAA2-BAMA-AGE-LIMIT-FROM      (GAA2-INDEX)      AND      GACDEPGM
01291           GAA-BAMA-AGE-LIMIT-TO        (GAA-INDEX) =              GACDEPGM
01292          GAA2-BAMA-AGE-LIMIT-TO        (GAA2-INDEX)      AND      GACDEPGM
                GAA-BAMA-BISCENDING-IND-RSV  (GAA-INDEX) =                      
               GAA2-BAMA-BISCENDING-IND-RES  (GAA2-INDEX)      AND              
01293           GAA-BAMA-VALUE-QUALIFIER     (GAA-INDEX) =              GACDEPGM
01294          GAA2-BAMA-VALUE-QUALIFIER     (GAA2-INDEX)               GACDEPGM
01295      THEN                                                         GACDEPGM
01296          NEXT SENTENCE                                            GACDEPGM
01297      ELSE                                                         GACDEPGM
01298         PERFORM 4610-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01299                                                                   GACDEPGM
01300      IF WRK-SIGNAL-FROM-ONLINE  = 'W'                             GACDEPGM
01301         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01302                                                                   GACDEPGM
01303  4610-300-DETERMINE-CDE-SETTIN.                                   GACDEPGM
01304      IF GAA-INDEX  <  GAA-ENTRY-COUNT AND                         GACDEPGM
01305         GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01306         MOVE 'Y'  TO  ACWA-CDE-FIELD-CHANGE-IND                   GACDEPGM
01307      ELSE                                                         GACDEPGM
01308         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01309                                                                   GACDEPGM
01310                                                                   GACDEPGM
01311  4610-900-EXIT.                                                   GACDEPGM
01312                      EXIT.                                        GACDEPGM
01313 /*****************************************************************GACDEPGM
01314 *  4620  -  COMPARE GAB CDE ELEMENTS (#ACL)                      *GACDEPGM
01315 *                                                                *GACDEPGM
01316 *        FUNCTIONS:                                              *GACDEPGM
01317 *                  1. FIND OCCURANCE WITHIN W/F ALL LVL TAB      *GACDEPGM
01318 *                     THAT MATCHS ALL LVL TAB OCCURANCE FROM     *GACDEPGM
01319 *                     PROVISION POOL.  IF THE OCCURANCE WAS      *GACDEPGM
01320 *                     DELETED INDICATED THAT THE RECORD HAS      *GACDEPGM
01321 *                     CHANGED.                                   *GACDEPGM
01322 *                  2. COMPARE CRITICAL DATA ELEMENTS IN THE      *GACDEPGM
01323 *                     TWO OCCURANCES FOR POSSIBLE CHANGES.       *GACDEPGM
01324 *                                                                *GACDEPGM
01325 *                  *. GAB  = W/F ALL LVL TAB                     *GACDEPGM
01326 *                     GAB2 = PRODUCTION ALL LVL TAB              *GACDEPGM
01327 ******************************************************************GACDEPGM
01328  4620-000-COMP-GAB-CDE-ELEMENTS SECTION.                          GACDEPGM
01329                                                                   GACDEPGM
01330      SET  GAB-INDEX  TO  1.                                       GACDEPGM
01331                                                                   GACDEPGM
01332  4620-100-SEARCH.                                                 GACDEPGM
01333                                                                   GACDEPGM
01334      SEARCH GAB-ENTRY                                             GACDEPGM
01335         VARYING GAB-INDEX                                         GACDEPGM
01336         WHEN                                                      GACDEPGM
01337            GAB-INDEX  NOT <  GAB-ENTRY-COUNT OR                   GACDEPGM
01338            GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX) =                GACDEPGM
01339                               GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) GACDEPGM
01340            NEXT SENTENCE.                                         GACDEPGM
01341                                                                   GACDEPGM
01342      IF GAB-INDEX  <  GAB-ENTRY-COUNT AND                         GACDEPGM
01343         GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX) =                   GACDEPGM
01344                               GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) GACDEPGM
01345         PERFORM 4620-200-COMP-GAB-CDE-ELEMENTS                    GACDEPGM
01346      ELSE                                                         GACDEPGM
01347         PERFORM 4620-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01348                                                                   GACDEPGM
01349      GO TO 4620-900-EXIT.                                         GACDEPGM
01350                                                                   GACDEPGM
01351  4620-200-COMP-GAB-CDE-ELEMENTS.                                  GACDEPGM
01352                                                                   GACDEPGM
01353      IF   GAB-COINS-CONDITION           (GAB-INDEX) =             GACDEPGM
01354          GAB2-COINS-CONDITION           (GAB2-INDEX)      AND     GACDEPGM
01355           GAB-COINS-BENEFIT-PERIOD      (GAB-INDEX) =             GACDEPGM
01356          GAB2-COINS-BENEFIT-PERIOD      (GAB2-INDEX)      AND     GACDEPGM
01357           GAB-COINS-CO-PAY-IND          (GAB-INDEX) =             GACDEPGM
01358          GAB2-COINS-CO-PAY-IND          (GAB2-INDEX)      AND     GACDEPGM
01359           GAB-COINS-BISCENDING-IND      (GAB-INDEX) =             GACDEPGM
01360          GAB2-COINS-BISCENDING-IND      (GAB2-INDEX)      AND     GACDEPGM
01361           GAB-COINS-COST-CONTAIN-IND    (GAB-INDEX) =             GACDEPGM
01362          GAB2-COINS-COST-CONTAIN-IND    (GAB2-INDEX)      AND     GACDEPGM
01363           GAB-COINS-FAM-OR-INDIV        (GAB-INDEX) =             GACDEPGM
01364          GAB2-COINS-FAM-OR-INDIV        (GAB2-INDEX)      AND     GACDEPGM
01365           GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX) =             GACDEPGM
01366          GAB2-COINS-INTERNAL-DESCRIPTOR (GAB2-INDEX)      AND     GACDEPGM
01367           GAB-COINS-L-O-B               (GAB-INDEX) =             GACDEPGM
01368          GAB2-COINS-L-O-B               (GAB2-INDEX)      AND     GACDEPGM
01369           GAB-COINS-LMT-MANDATORY-IND   (GAB-INDEX) =             GACDEPGM
01370          GAB2-COINS-LMT-MANDATORY-IND   (GAB2-INDEX)      AND     GACDEPGM
01371           GAB-COINS-PERCENT-LEVEL       (GAB-INDEX) =             GACDEPGM
01372          GAB2-COINS-PERCENT-LEVEL       (GAB2-INDEX)      AND     GACDEPGM
01373           GAB-COINS-PLACE-OF-TREATMENT  (GAB-INDEX) =             GACDEPGM
01374          GAB2-COINS-PLACE-OF-TREATMENT  (GAB2-INDEX)      AND     GACDEPGM
01375           GAB-COINS-SERVICE-GROUP       (GAB-INDEX) =             GACDEPGM
01376          GAB2-COINS-SERVICE-GROUP       (GAB2-INDEX)      AND     GACDEPGM
01377           GAB-COINS-AGE-QUAL-IND-FROM   (GAB-INDEX) =             GACDEPGM
01378          GAB2-COINS-AGE-QUAL-IND-FROM   (GAB2-INDEX)      AND     GACDEPGM
01379           GAB-COINS-AGE-QUAL-IND-TO     (GAB-INDEX) =             GACDEPGM
01380          GAB2-COINS-AGE-QUAL-IND-TO     (GAB2-INDEX)      AND     GACDEPGM
01381           GAB-COINS-AGE-LIMIT-FROM      (GAB-INDEX) =             GACDEPGM
01382          GAB2-COINS-AGE-LIMIT-FROM      (GAB2-INDEX)      AND     GACDEPGM
01383           GAB-COINS-AGE-LIMIT-TO        (GAB-INDEX) =             GACDEPGM
01384          GAB2-COINS-AGE-LIMIT-TO        (GAB2-INDEX)      AND     GACDEPGM
01385           GAB-COINS-VALUE-QUALIFIER     (GAB-INDEX) =             GACDEPGM
01386          GAB2-COINS-VALUE-QUALIFIER     (GAB2-INDEX)              GACDEPGM
01387      THEN                                                         GACDEPGM
01388          NEXT SENTENCE                                            GACDEPGM
01389      ELSE                                                         GACDEPGM
01390         PERFORM 4620-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01391                                                                   GACDEPGM
01392      IF WRK-SIGNAL-FROM-ONLINE  = 'W'                             GACDEPGM
01393         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01394                                                                   GACDEPGM
01395  4620-300-DETERMINE-CDE-SETTIN.                                   GACDEPGM
01396      IF GAB-INDEX  <  GAB-ENTRY-COUNT AND                         GACDEPGM
01397         GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01398         MOVE 'Y'  TO  ACWA-CDE-FIELD-CHANGE-IND                   GACDEPGM
01399      ELSE                                                         GACDEPGM
01400         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01401                                                                   GACDEPGM
01402  4620-900-EXIT.       EXIT.                                       GACDEPGM
01403                                                                   GACDEPGM
01404 /*****************************************************************GACDEPGM
01405 *  4630  -  COMPARE GAC CDE ELEMENTS (#ADL)                      *GACDEPGM
01406 *                                                                *GACDEPGM
01407 *        FUNCTIONS:                                              *GACDEPGM
01408 *                  1. FIND OCCURANCE WITHIN W/F ALL LVL TAB      *GACDEPGM
01409 *                     THAT MATCHS ALL LVL TAB OCCURANCE FROM     *GACDEPGM
01410 *                     PROVISION POOL.  IF THE OCCURANCE WAS      *GACDEPGM
01411 *                     DELETED INDICATED THAT THE RECORD HAS      *GACDEPGM
01412 *                     CHANGED.                                   *GACDEPGM
01413 *                  2. COMPARE CRITICAL DATA ELEMENTS IN THE      *GACDEPGM
01414 *                     TWO OCCURANCES FOR POSSIBLE CHANGES.       *GACDEPGM
01415 *                                                                *GACDEPGM
01416 *                  *. GAC  = W/F ALL LVL TAB                     *GACDEPGM
01417 *                     GAC2 = PRODUCTION ALL LVL TAB              *GACDEPGM
01418 ******************************************************************GACDEPGM
01419  4630-000-COMP-GAC-CDE-ELEMENTS SECTION.                          GACDEPGM
01420                                                                   GACDEPGM
01421      SET    GAC-INDEX TO 1.                                       GACDEPGM
01422                                                                   GACDEPGM
01423  4630-100-SEARCH.                                                 GACDEPGM
01424                                                                   GACDEPGM
01425      SEARCH GAC-ENTRY                                             GACDEPGM
01426         VARYING GAC-INDEX                                         GACDEPGM
01427         WHEN                                                      GACDEPGM
01428            GAC-INDEX  NOT <  GAC-ENTRY-COUNT OR                   GACDEPGM
01429            GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX) =                GACDEPGM
01430                               GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX) GACDEPGM
01431            NEXT SENTENCE.                                         GACDEPGM
01432                                                                   GACDEPGM
01433      IF GAC-INDEX  <  GAC-ENTRY-COUNT AND                         GACDEPGM
01434         GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX) =                   GACDEPGM
01435                               GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX) GACDEPGM
01436         PERFORM 4630-200-COMP-GAC-CDE-ELEMENTS                    GACDEPGM
01437      ELSE                                                         GACDEPGM
01438         PERFORM 4630-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01439                                                                   GACDEPGM
01440      GO TO 4630-900-EXIT.                                         GACDEPGM
01441                                                                   GACDEPGM
01442  4630-200-COMP-GAC-CDE-ELEMENTS.                                  GACDEPGM
01443                                                                   GACDEPGM
01444      IF   GAC-DEDL-CONDITION            (GAC-INDEX) =             GACDEPGM
01445          GAC2-DEDL-CONDITION            (GAC2-INDEX)      AND     GACDEPGM
01446           GAC-DEDL-BENEFIT-PERIOD       (GAC-INDEX) =             GACDEPGM
01447          GAC2-DEDL-BENEFIT-PERIOD       (GAC2-INDEX)      AND     GACDEPGM
01448           GAC-DEDL-CO-PAY-IND           (GAC-INDEX) =             GACDEPGM
01449          GAC2-DEDL-CO-PAY-IND           (GAC2-INDEX)      AND     GACDEPGM
01450           GAC-DEDL-COST-CONTAIN-IND     (GAC-INDEX) =             GACDEPGM
01451          GAC2-DEDL-COST-CONTAIN-IND     (GAC2-INDEX)      AND     GACDEPGM
01452           GAC-DEDL-FAM-OR-INDIV         (GAC-INDEX) =             GACDEPGM
01453          GAC2-DEDL-FAM-OR-INDIV         (GAC2-INDEX)      AND     GACDEPGM
01454           GAC-DEDL-INTERNAL-DESCRIPTOR  (GAC-INDEX) =             GACDEPGM
01455          GAC2-DEDL-INTERNAL-DESCRIPTOR (GAC2-INDEX)       AND     GACDEPGM
01456           GAC-DEDL-L-O-B                (GAC-INDEX) =             GACDEPGM
01457          GAC2-DEDL-L-O-B                (GAC2-INDEX)      AND     GACDEPGM
01458           GAC-DEDL-MANDATORY-IND        (GAC-INDEX) =             GACDEPGM
01459          GAC2-DEDL-MANDATORY-IND        (GAC2-INDEX)      AND     GACDEPGM
01460           GAC-DEDL-PLACE-OF-TREATMENT   (GAC-INDEX) =             GACDEPGM
01461          GAC2-DEDL-PLACE-OF-TREATMENT   (GAC2-INDEX)      AND     GACDEPGM
01462           GAC-DEDL-SERVICE-GROUP        (GAC-INDEX) =             GACDEPGM
01463          GAC2-DEDL-SERVICE-GROUP        (GAC2-INDEX)      AND     GACDEPGM
01464           GAC-DEDL-AGE-QUAL-IND-FROM    (GAC-INDEX) =             GACDEPGM
01465          GAC2-DEDL-AGE-QUAL-IND-FROM    (GAC2-INDEX)      AND     GACDEPGM
01466           GAC-DEDL-AGE-QUAL-IND-TO      (GAC-INDEX) =             GACDEPGM
01467          GAC2-DEDL-AGE-QUAL-IND-TO      (GAC2-INDEX)      AND     GACDEPGM
01468           GAC-DEDL-AGE-LIMIT-FROM       (GAC-INDEX) =             GACDEPGM
01469          GAC2-DEDL-AGE-LIMIT-FROM       (GAC2-INDEX)      AND     GACDEPGM
01470           GAC-DEDL-AGE-LIMIT-TO         (GAC-INDEX) =             GACDEPGM
01471          GAC2-DEDL-AGE-LIMIT-TO         (GAC2-INDEX)      AND     GACDEPGM
                GAC-DEDL-BISCENDING-IND-RSV   (GAC-INDEX) =                     
               GAC2-DEDL-BISCENDING-IND-RSV   (GAC2-INDEX)      AND             
01472           GAC-DEDL-VALUE-QUALIFIER      (GAC-INDEX) =             GACDEPGM
01473          GAC2-DEDL-VALUE-QUALIFIER      (GAC2-INDEX)              GACDEPGM
01474      THEN                                                         GACDEPGM
01475          NEXT SENTENCE                                            GACDEPGM
01476      ELSE                                                         GACDEPGM
01477         PERFORM 4630-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01478                                                                   GACDEPGM
01479      IF WRK-SIGNAL-FROM-ONLINE  = 'W'                             GACDEPGM
01480         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01481                                                                   GACDEPGM
01482  4630-300-DETERMINE-CDE-SETTIN.                                   GACDEPGM
01483      IF GAC-INDEX  <  GAC-ENTRY-COUNT AND                         GACDEPGM
01484         GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01485         MOVE 'Y'  TO  ACWA-CDE-FIELD-CHANGE-IND                   GACDEPGM
01486      ELSE                                                         GACDEPGM
01487         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01488                                                                   GACDEPGM
01489  4630-900-EXIT.      EXIT.                                        GACDEPGM
01490                                                                   GACDEPGM
01491 /*****************************************************************GACDEPGM
01492 *  4640  -  COMPARE GAD CDE ELEMENTS (#AOL)                      *GACDEPGM
01493 *                                                                *GACDEPGM
01494 *        FUNCTIONS:                                              *GACDEPGM
01495 *                  1. FIND OCCURANCE WITHIN W/F ALL LVL TAB      *GACDEPGM
01496 *                     THAT MATCHS ALL LVL TAB OCCURANCE FROM     *GACDEPGM
01497 *                     PROVISION POOL.  IF THE OCCURANCE WAS      *GACDEPGM
01498 *                     DELETED INDICATED THAT THE RECORD HAS      *GACDEPGM
01499 *                     CHANGED.                                   *GACDEPGM
01500 *                  2. COMPARE CRITICAL DATA ELEMENTS IN THE      *GACDEPGM
01501 *                     TWO OCCURANCES FOR POSSIBLE CHANGES.       *GACDEPGM
01502 *                                                                *GACDEPGM
01503 *                  *. GAD  = W/F ALL LVL TAB                     *GACDEPGM
01504 *                     GAD2 = PRODUCTION ALL LVL TAB              *GACDEPGM
01505 ******************************************************************GACDEPGM
01506  4640-000-COMP-GAD-CDE-ELEMENTS SECTION.                          GACDEPGM
01507                                                                   GACDEPGM
01508      SET  GAD-INDEX  TO  1.                                       GACDEPGM
01509                                                                   GACDEPGM
01510  4640-100-SEARCH.                                                 GACDEPGM
01511                                                                   GACDEPGM
01512      SEARCH GAD-ENTRY                                             GACDEPGM
01513         VARYING GAD-INDEX                                         GACDEPGM
01514         WHEN                                                      GACDEPGM
01515            GAD-INDEX  NOT <  GAD-ENTRY-COUNT OR                   GACDEPGM
01516            GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)  =               GACDEPGM
01517                               GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX) GACDEPGM
01518            NEXT SENTENCE.                                         GACDEPGM
01519                                                                   GACDEPGM
01520      IF GAD-INDEX  <  GAD-ENTRY-COUNT AND                         GACDEPGM
01521         GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)  =                  GACDEPGM
01522                               GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX) GACDEPGM
01523         PERFORM 4640-200-COMP-GAD-CDE-ELEMENTS                    GACDEPGM
01524      ELSE                                                         GACDEPGM
01525         PERFORM 4640-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01526                                                                   GACDEPGM
01527      GO TO 4640-900-EXIT.                                         GACDEPGM
01528                                                                   GACDEPGM
01529  4640-200-COMP-GAD-CDE-ELEMENTS.                                  GACDEPGM
01530                                                                   GACDEPGM
01531      IF   GAD-O-P-X-CONDITION           (GAD-INDEX) =             GACDEPGM
01532          GAD2-O-P-X-CONDITION           (GAD2-INDEX)      AND     GACDEPGM
01533           GAD-O-P-X-BENEFIT-PERIOD      (GAD-INDEX) =             GACDEPGM
01534          GAD2-O-P-X-BENEFIT-PERIOD      (GAD2-INDEX)      AND     GACDEPGM
01535           GAD-O-P-X-CO-PAY-IND          (GAD-INDEX) =             GACDEPGM
01536          GAD2-O-P-X-CO-PAY-IND          (GAD2-INDEX)      AND     GACDEPGM
01537           GAD-O-P-X-BISCENDING-IND      (GAD-INDEX) =             GACDEPGM
01538          GAD2-O-P-X-BISCENDING-IND      (GAD2-INDEX)      AND     GACDEPGM
01539           GAD-O-P-X-COST-CONTAIN-IND    (GAD-INDEX) =             GACDEPGM
01540          GAD2-O-P-X-COST-CONTAIN-IND    (GAD2-INDEX)      AND     GACDEPGM
01541           GAD-O-P-X-FAM-OR-INDIV        (GAD-INDEX) =             GACDEPGM
01542          GAD2-O-P-X-FAM-OR-INDIV        (GAD2-INDEX)      AND     GACDEPGM
01543           GAD-O-P-X-INTERNAL-DESCRIPTOR (GAD-INDEX) =             GACDEPGM
01544          GAD2-O-P-X-INTERNAL-DESCRIPTOR (GAD2-INDEX)      AND     GACDEPGM
01545           GAD-O-P-X-L-O-B               (GAD-INDEX) =             GACDEPGM
01546          GAD2-O-P-X-L-O-B               (GAD2-INDEX)      AND     GACDEPGM
01547           GAD-O-P-X-PERCENT-LEVEL       (GAD-INDEX) =             GACDEPGM
01548          GAD2-O-P-X-PERCENT-LEVEL       (GAD2-INDEX)      AND     GACDEPGM
01549           GAD-O-P-X-PLACE-OF-TREATMENT  (GAD-INDEX) =             GACDEPGM
01550          GAD2-O-P-X-PLACE-OF-TREATMENT  (GAD2-INDEX)      AND     GACDEPGM
01551           GAD-O-P-X-SERVICE-GROUP       (GAD-INDEX) =             GACDEPGM
01552          GAD2-O-P-X-SERVICE-GROUP       (GAD2-INDEX)      AND     GACDEPGM
01553           GAD-O-P-X-AGE-QUAL-IND-FROM   (GAD-INDEX) =             GACDEPGM
01554          GAD2-O-P-X-AGE-QUAL-IND-FROM   (GAD2-INDEX)      AND     GACDEPGM
01555           GAD-O-P-X-AGE-QUAL-IND-TO     (GAD-INDEX) =             GACDEPGM
01556          GAD2-O-P-X-AGE-QUAL-IND-TO     (GAD2-INDEX)      AND     GACDEPGM
01557           GAD-O-P-X-AGE-LIMIT-FROM      (GAD-INDEX) =             GACDEPGM
01558          GAD2-O-P-X-AGE-LIMIT-FROM      (GAD2-INDEX)      AND     GACDEPGM
01559           GAD-O-P-X-AGE-LIMIT-TO        (GAD-INDEX) =             GACDEPGM
01560          GAD2-O-P-X-AGE-LIMIT-TO        (GAD2-INDEX)      AND     GACDEPGM
01561           GAD-O-P-X-VALUE-QUALIFIER     (GAD-INDEX) =             GACDEPGM
01562          GAD2-O-P-X-VALUE-QUALIFIER     (GAD2-INDEX)              GACDEPGM
01563      THEN                                                         GACDEPGM
01564          NEXT SENTENCE                                            GACDEPGM
01565      ELSE                                                         GACDEPGM
01566         PERFORM 4640-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01567                                                                   GACDEPGM
01568      IF WRK-SIGNAL-FROM-ONLINE  = 'W'                             GACDEPGM
01569         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01570                                                                   GACDEPGM
01571                                                                   GACDEPGM
01572  4640-300-DETERMINE-CDE-SETTIN.                                   GACDEPGM
01573      IF GAD-INDEX  <  GAD-ENTRY-COUNT AND                         GACDEPGM
01574         GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01575         MOVE 'Y'  TO  ACWA-CDE-FIELD-CHANGE-IND                   GACDEPGM
01576      ELSE                                                         GACDEPGM
01577         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01578                                                                   GACDEPGM
01579  4640-900-EXIT.      EXIT.                                        GACDEPGM
01580                                                                   GACDEPGM
01581 /*****************************************************************GACDEPGM
01582 *  4645  -  COMPARE GAF CDE ELEMENTS (#ACP)                      *GACDEPGM
01583 *                                                                *GACDEPGM
01584 *        FUNCTIONS:                                              *GACDEPGM
01585 *                  1. FIND OCCURANCE WITHIN W/F ALL LVL TAB      *GACDEPGM
01586 *                     THAT MATCHS ALL LVL TAB OCCURANCE FROM     *GACDEPGM
01587 *                     PROVISION POOL.  IF THE OCCURANCE WAS      *GACDEPGM
01588 *                     DELETED INDICATED THAT THE RECORD HAS      *GACDEPGM
01589 *                     CHANGED.                                   *GACDEPGM
01590 *                  2. COMPARE CRITICAL DATA ELEMENTS IN THE      *GACDEPGM
01591 *                     TWO OCCURANCES FOR POSSIBLE CHANGES.       *GACDEPGM
01592 *                                                                *GACDEPGM
01593 *                  *. GAF  = W/F ALL LVL TAB                     *GACDEPGM
01594 *                     GAF2 = PRODUCTION ALL LVL TAB              *GACDEPGM
01595 ******************************************************************GACDEPGM
01596  4645-000-COMP-GAF-CDE-ELEMENTS SECTION.                          GACDEPGM
01597                                                                   GACDEPGM
01598      SET    GAF-INDEX TO 1.                                       GACDEPGM
01599                                                                   GACDEPGM
01600  4645-100-SEARCH.                                                 GACDEPGM
01601                                                                   GACDEPGM
01602      SEARCH GAF-ENTRY                                             GACDEPGM
01603         VARYING GAF-INDEX                                         GACDEPGM
01604         WHEN                                                      GACDEPGM
01605            GAF-INDEX  NOT <  GAF-ENTRY-COUNT OR                   GACDEPGM
01606            GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX) =                GACDEPGM
01607                               GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX) GACDEPGM
01608            NEXT SENTENCE.                                         GACDEPGM
01609                                                                   GACDEPGM
01610      IF GAF-INDEX  <  GAF-ENTRY-COUNT AND                         GACDEPGM
01611         GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX) =                   GACDEPGM
01612                               GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX) GACDEPGM
01613         PERFORM 4645-200-COMP-GAF-CDE-ELEMENTS                    GACDEPGM
01614      ELSE                                                         GACDEPGM
01615         PERFORM 4645-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01616                                                                   GACDEPGM
01617      GO TO 4645-900-EXIT.                                         GACDEPGM
01618                                                                   GACDEPGM
01619  4645-200-COMP-GAF-CDE-ELEMENTS.                                  GACDEPGM
01620                                                                   GACDEPGM
01621      IF   GAF-COPAY-CONDITION           (GAF-INDEX) =             GACDEPGM
01622          GAF2-COPAY-CONDITION           (GAF2-INDEX)      AND     GACDEPGM
01623           GAF-COPAY-BENEFIT-PERIOD      (GAF-INDEX) =             GACDEPGM
01624          GAF2-COPAY-BENEFIT-PERIOD      (GAF2-INDEX)      AND     GACDEPGM
01625           GAF-COPAY-CO-PAY-IND          (GAF-INDEX) =             GACDEPGM
01626          GAF2-COPAY-CO-PAY-IND          (GAF2-INDEX)      AND     GACDEPGM
01627           GAF-COPAY-COST-CONTAIN-IND    (GAF-INDEX) =             GACDEPGM
01628          GAF2-COPAY-COST-CONTAIN-IND    (GAF2-INDEX)      AND     GACDEPGM
01629           GAF-COPAY-FAM-OR-INDIV        (GAF-INDEX) =             GACDEPGM
01630          GAF2-COPAY-FAM-OR-INDIV        (GAF2-INDEX)      AND     GACDEPGM
01631           GAF-COPAY-INTERNAL-DESCRIPTOR (GAF-INDEX) =             GACDEPGM
01632          GAF2-COPAY-INTERNAL-DESCRIPTOR (GAF2-INDEX)      AND     GACDEPGM
01633           GAF-COPAY-L-O-B               (GAF-INDEX) =             GACDEPGM
01634          GAF2-COPAY-L-O-B               (GAF2-INDEX)      AND     GACDEPGM
01635           GAF-COPAY-MANDATORY-IND       (GAF-INDEX) =             GACDEPGM
01636          GAF2-COPAY-MANDATORY-IND       (GAF2-INDEX)      AND     GACDEPGM
01637           GAF-COPAY-PLACE-OF-TREATMENT  (GAF-INDEX) =             GACDEPGM
01638          GAF2-COPAY-PLACE-OF-TREATMENT  (GAF2-INDEX)      AND     GACDEPGM
01639           GAF-COPAY-SERVICE-GROUP       (GAF-INDEX) =             GACDEPGM
01640          GAF2-COPAY-SERVICE-GROUP       (GAF2-INDEX)      AND     GACDEPGM
01641           GAF-COPAY-AGE-QUAL-IND-FROM   (GAF-INDEX) =             GACDEPGM
01642          GAF2-COPAY-AGE-QUAL-IND-FROM   (GAF2-INDEX)      AND     GACDEPGM
01643           GAF-COPAY-AGE-QUAL-IND-TO     (GAF-INDEX) =             GACDEPGM
01644          GAF2-COPAY-AGE-QUAL-IND-TO     (GAF2-INDEX)      AND     GACDEPGM
01645           GAF-COPAY-AGE-LIMIT-FROM      (GAF-INDEX) =             GACDEPGM
01646          GAF2-COPAY-AGE-LIMIT-FROM      (GAF2-INDEX)      AND     GACDEPGM
01647           GAF-COPAY-AGE-LIMIT-TO        (GAF-INDEX) =             GACDEPGM
01648          GAF2-COPAY-AGE-LIMIT-TO        (GAF2-INDEX)      AND     GACDEPGM
                GAF-COPAY-BISCENDING-IND      (GAF-INDEX) =                     
               GAF2-COPAY-BISCENDING-IND      (GAF2-INDEX)      AND             
01649           GAF-COPAY-VALUE-QUALIFIER     (GAF-INDEX) =             GACDEPGM
01650          GAF2-COPAY-VALUE-QUALIFIER     (GAF2-INDEX)              GACDEPGM
01651      THEN                                                         GACDEPGM
01652          NEXT SENTENCE                                            GACDEPGM
01653      ELSE                                                         GACDEPGM
01654         PERFORM 4645-300-DETERMINE-CDE-SETTIN.                    GACDEPGM
01655                                                                   GACDEPGM
01656      IF WRK-SIGNAL-FROM-ONLINE  = 'W'                             GACDEPGM
01657         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01658                                                                   GACDEPGM
01659  4645-300-DETERMINE-CDE-SETTIN.                                   GACDEPGM
01660      IF GAF-INDEX  <  GAF-ENTRY-COUNT AND                         GACDEPGM
01661         GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01662         MOVE 'Y'  TO  ACWA-CDE-FIELD-CHANGE-IND                   GACDEPGM
01663      ELSE                                                         GACDEPGM
01664         MOVE 'Y'  TO  ACWA-CDE-REC-CHANGE-IND.                    GACDEPGM
01665                                                                   GACDEPGM
01666  4645-900-EXIT.      EXIT.                                        GACDEPGM
01667                                                                   GACDEPGM
01668 /*****************************************************************GACDEPGM
01669 *  4650  -  FIND IF THIS OCCR IS A NEW ADD - #ABM                *GACDEPGM
01670 ******************************************************************GACDEPGM
01671  4650-000-FIND-IF-OCCR-ADDED    SECTION.                          GACDEPGM
01672                                                                   GACDEPGM
01673      SET  GAA2-INDEX  TO 1.                                       GACDEPGM
01674         SEARCH GAA2-ENTRY                                         GACDEPGM
01675           VARYING GAA2-INDEX                                      GACDEPGM
01676           WHEN                                                    GACDEPGM
01677                GAA2-INDEX NOT <  GAA2-ENTRY-COUNT  OR             GACDEPGM
01678                GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) =              GACDEPGM
01679                          GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX)    GACDEPGM
01680                NEXT SENTENCE.                                     GACDEPGM
01681                                                                   GACDEPGM
01682           IF  GAA2-INDEX <  GAA2-ENTRY-COUNT   AND                GACDEPGM
01683                GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX) =              GACDEPGM
01684                          GAA2-OCCURS-ENTRY-COUNTER(GAA2-INDEX)    GACDEPGM
01685                NEXT  SENTENCE                                     GACDEPGM
01686           ELSE                                                    GACDEPGM
01687                MOVE 'Y'  TO ACWA-CDE-REC-CHANGE-IND.              GACDEPGM
01688                                                                   GACDEPGM
01689  4650-900-EXIT.                                                   GACDEPGM
01690        EXIT.                                                      GACDEPGM
01691 /*****************************************************************GACDEPGM
01692 *  4660  -  FIND IF THIS OCCR IS A NEW ADD - #ACL                *GACDEPGM
01693 ******************************************************************GACDEPGM
01694  4660-000-FIND-IF-OCCR-ADDED    SECTION.                          GACDEPGM
01695                                                                   GACDEPGM
01696      SET  GAB2-INDEX  TO 1.                                       GACDEPGM
01697         SEARCH GAB2-ENTRY                                         GACDEPGM
01698           VARYING GAB2-INDEX                                      GACDEPGM
01699           WHEN                                                    GACDEPGM
01700                GAB2-INDEX NOT <  GAB2-ENTRY-COUNT  OR             GACDEPGM
01701                GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) =              GACDEPGM
01702                          GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX)    GACDEPGM
01703                NEXT SENTENCE.                                     GACDEPGM
01704                                                                   GACDEPGM
01705           IF  GAB2-INDEX <  GAB2-ENTRY-COUNT   AND                GACDEPGM
01706                GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX) =              GACDEPGM
01707                          GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX)    GACDEPGM
01708                NEXT SENTENCE                                      GACDEPGM
01709           ELSE                                                    GACDEPGM
01710                MOVE 'Y'  TO ACWA-CDE-REC-CHANGE-IND.              GACDEPGM
01711                                                                   GACDEPGM
01712  4660-900-EXIT.                                                   GACDEPGM
01713        EXIT.                                                      GACDEPGM
01714 /*****************************************************************GACDEPGM
01715 *  4670  -  FIND IF THIS OCCR IS A NEW ADD - #ADL                *GACDEPGM
01716 ******************************************************************GACDEPGM
01717  4670-000-FIND-IF-OCCR-ADDED    SECTION.                          GACDEPGM
01718                                                                   GACDEPGM
01719      SET  GAC2-INDEX  TO 1.                                       GACDEPGM
01720         SEARCH GAC2-ENTRY                                         GACDEPGM
01721           VARYING GAC2-INDEX                                      GACDEPGM
01722           WHEN                                                    GACDEPGM
01723                GAC2-INDEX NOT <  GAC2-ENTRY-COUNT  OR             GACDEPGM
01724                GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX) =              GACDEPGM
01725                          GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX)    GACDEPGM
01726                NEXT SENTENCE.                                     GACDEPGM
01727                                                                   GACDEPGM
01728           IF  GAC2-INDEX <  GAC2-ENTRY-COUNT   AND                GACDEPGM
01729                GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX) =              GACDEPGM
01730                          GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX)    GACDEPGM
01731                NEXT SENTENCE                                      GACDEPGM
01732           ELSE                                                    GACDEPGM
01733                MOVE 'Y'  TO ACWA-CDE-REC-CHANGE-IND.              GACDEPGM
01734                                                                   GACDEPGM
01735  4670-900-EXIT.                                                   GACDEPGM
01736        EXIT.                                                      GACDEPGM
01737 /*****************************************************************GACDEPGM
01738 *  4680  -  FIND IF THIS OCCR IS A NEW ADD - #AOL                *GACDEPGM
01739 ******************************************************************GACDEPGM
01740  4680-000-FIND-IF-OCCR-ADDED    SECTION.                          GACDEPGM
01741                                                                   GACDEPGM
01742      SET  GAD2-INDEX  TO 1.                                       GACDEPGM
01743         SEARCH GAD2-ENTRY                                         GACDEPGM
01744           VARYING GAD2-INDEX                                      GACDEPGM
01745           WHEN                                                    GACDEPGM
01746                GAD2-INDEX NOT <  GAD2-ENTRY-COUNT  OR             GACDEPGM
01747                GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX) =              GACDEPGM
01748                          GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)    GACDEPGM
01749                NEXT SENTENCE.                                     GACDEPGM
01750                                                                   GACDEPGM
01751           IF  GAD2-INDEX <  GAD2-ENTRY-COUNT   AND                GACDEPGM
01752                GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX) =              GACDEPGM
01753                          GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)    GACDEPGM
01754                NEXT SENTENCE                                      GACDEPGM
01755           ELSE                                                    GACDEPGM
01756                MOVE 'Y'  TO ACWA-CDE-REC-CHANGE-IND.              GACDEPGM
01757                                                                   GACDEPGM
01758  4680-900-EXIT.                                                   GACDEPGM
01759        EXIT.                                                      GACDEPGM
01760 /*****************************************************************GACDEPGM
01761 *  4690  -  FIND IF THIS OCCR IS A NEW ADD - #ACP                *GACDEPGM
01762 ******************************************************************GACDEPGM
01763  4690-000-FIND-IF-OCCR-ADDED    SECTION.                          GACDEPGM
01764                                                                   GACDEPGM
01765      SET  GAF2-INDEX  TO 1.                                       GACDEPGM
01766         SEARCH GAF2-ENTRY                                         GACDEPGM
01767           VARYING GAF2-INDEX                                      GACDEPGM
01768           WHEN                                                    GACDEPGM
01769                GAF2-INDEX NOT <  GAF2-ENTRY-COUNT  OR             GACDEPGM
01770                GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX) =              GACDEPGM
01771                          GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX)    GACDEPGM
01772                NEXT SENTENCE.                                     GACDEPGM
01773                                                                   GACDEPGM
01774           IF  GAF2-INDEX <  GAF2-ENTRY-COUNT   AND                GACDEPGM
01775                GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX) =              GACDEPGM
01776                          GAF2-OCCURS-ENTRY-COUNTER(GAF2-INDEX)    GACDEPGM
01777                NEXT SENTENCE                                      GACDEPGM
01778           ELSE                                                    GACDEPGM
01779                MOVE 'Y'  TO ACWA-CDE-REC-CHANGE-IND.              GACDEPGM
01780                                                                   GACDEPGM
01781  4690-900-EXIT.                                                   GACDEPGM
01782        EXIT.                                                      GACDEPGM
01783 /*****************************************************************GACDEPGM
01784 *  4700  -  UPDATE W/F CONTROL RECORD                            *GACDEPGM
01785 *                                                                *GACDEPGM
01786 *        FUNCTIONS:                                              *GACDEPGM
01787 *          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *GACDEPGM
01788 *             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *GACDEPGM
01789 *             RECORD IF ITS CDE STATUS CHANGED.                  *GACDEPGM
01790 *          2. REWRITE W/F CONTROL RECORD                         *GACDEPGM
01791 ******************************************************************GACDEPGM
01792  4700-000-UPDATE-CONTROL-RECORD SECTION.                          GACDEPGM
01793                                                                   GACDEPGM
01794      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GACDEPGM
01795         ACWA-CDE-2B-COUNT  =  ZERO                                GACDEPGM
01796         GO TO 4700-900-EXIT.                                      GACDEPGM
01797                                                                   GACDEPGM
01798 *------------ READ W/F CONTROL RECORD                             GACDEPGM
01799      PERFORM 5100-READ-CONTROL-UPDT.                              GACDEPGM
01800                                                                   GACDEPGM
01801      IF NOT GCIO6-GOOD-RETURN                                     GACDEPGM
01802         MOVE WS-ABCODE-CDF2        TO  WS-ABCODE                  GACDEPGM
01803         MOVE WS-ABCODE-CDF2-MSG    TO  WS-ABCODE-MSG              GACDEPGM
01804         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
01805                                                                   GACDEPGM
01806      COMPUTE CCR-CDE-1U-COUNT  =  CCR-CDE-1U-COUNT  +             GACDEPGM
01807                                  ACWA-CDE-1U-COUNT.               GACDEPGM
01808      COMPUTE CCR-CDE-2B-COUNT  =  CCR-CDE-2B-COUNT  +             GACDEPGM
01809                                  ACWA-CDE-2B-COUNT.               GACDEPGM
01810      COMPUTE CCR-CDE-TOTAL-COUNT  =  CCR-CDE-TOTAL-COUNT  +       GACDEPGM
01811           ACWA-CDE-1U-COUNT  +   ACWA-CDE-2B-COUNT.               GACDEPGM
01812                                                                   GACDEPGM
01813      MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      GACDEPGM
01814                                                                   GACDEPGM
01815      MOVE '2 '  TO  WRK6-CDE-SP.                                  GACDEPGM
01816                                                                   GACDEPGM
01817      IF CCR-CDE-1R-COUNT  >  ZERO                                 GACDEPGM
01818         MOVE '1R'  TO  WRK6-CDE-SP.                               GACDEPGM
01819      IF CCR-CDE-1P-COUNT  >  ZERO                                 GACDEPGM
01820         MOVE '1P'  TO  WRK6-CDE-SP.                               GACDEPGM
01821      IF CCR-CDE-1H-COUNT  >  ZERO                                 GACDEPGM
01822         MOVE '1H'  TO  WRK6-CDE-SP.                               GACDEPGM
01823      IF CCR-CDE-1U-COUNT  >  ZERO                                 GACDEPGM
01824         MOVE '1U'  TO  WRK6-CDE-SP.                               GACDEPGM
01825                                                                   GACDEPGM
01826      IF CCR-CDE-CEN  >  ZERO                                      GACDEPGM
01827         NEXT SENTENCE                                             GACDEPGM
01828      ELSE                                                         GACDEPGM
01829         MOVE EIBDATE              TO WS-EIBDATE-CEN               GACDEPGM
01830         IF WS-EIBDATE-2 = 0                                       GACDEPGM
01831             MOVE HEX-19           TO CCR-CDE-CC                   GACDEPGM
01832             MOVE WS-EIBDATE-DT    TO CCR-CDE-DATE                 GACDEPGM
01833         ELSE                                                      GACDEPGM
01834         IF WS-EIBDATE-2 = 1                                       GACDEPGM
01835             MOVE HEX-20           TO CCR-CDE-CC                   GACDEPGM
01836             MOVE WS-EIBDATE-DT    TO CCR-CDE-DATE.                GACDEPGM
01837                                                                   GACDEPGM
01838 **   IF CCR-CDE-1R-COUNT  >  0  OR                                GACDEPGM
01839 **      CCR-CDE-1P-COUNT  >  0  OR                                GACDEPGM
01840 ***     CCR-CDE-1H-COUNT  >  0                                    GACDEPGM
01841 **      MOVE WS-ABCODE-CDL5        TO  WS-ABCODE                  GACDEPGM
01842 **      MOVE WS-ABCODE-CDL5-MSG    TO  WS-ABCODE-MSG              GACDEPGM
01843 **      PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
01844                                                                   GACDEPGM
01845 *------------ REWRITE W/F CONTROL RECORD                          GACDEPGM
01846      PERFORM 5200-REWRITE-CONTROL-RECORD.                         GACDEPGM
01847                                                                   GACDEPGM
01848  4700-900-EXIT.       EXIT.                                       GACDEPGM
01849                                                                   GACDEPGM
01850 /*****************************************************************GACDEPGM
01851 *  4900  -  UPDATE INTERNALS AND CONTROL BASED ON ACCUM           GACDEPGM
01852 *                                                                 GACDEPGM
01853 *        FUNCTIONS:                                               GACDEPGM
01854 *          1. ACCESS W/F ACCUMULATOR RECORD SINCE THE INTERNAL    GACDEPGM
01855 *             DESCRIPTOR HAS CHANGED, LOOK FOR THE INTERNAL TABS  GACDEPGM
01856 *             WHICH ARE TO BE RESET, IF ACCUM IS NOT REPLACEMENT. GACDEPGM
01857 *          2. READ THE INTERNAL TABULAR RECORD.  TAKE APPROPRIATE GACDEPGM
01858 *             ACTION BASED ON THE DESCRIPTOR AND THE REC'S STATUS GACDEPGM
01859 *             UPDATE A VERSION OF THE CNTL REC'S CNTS IN COMMAREA.GACDEPGM
01860 ******************************************************************GACDEPGM
01861  4900-000-UPDT-INTRNL-AND-CNTL  SECTION.                          GACDEPGM
01862                                                                   GACDEPGM
01863      MOVE OENTCTRI  TO  ACWA-DISPLAY-LEN-7-X.                     GACDEPGM
01864      IF FRMNUIDI  =  'GS3A'                                       GACDEPGM
01865         MOVE  IDLINEI  TO  GROUP-SPECIFIC-ID-LINE                 GACDEPGM
01866         MOVE  SPACES               TO  GCIO-WORKFILE-KEY          GACDEPGM
01867         MOVE  'G'                  TO  GCIO-WRK-STATUS-CODE       GACDEPGM
01868         MOVE  GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE         GACDEPGM
01869         MOVE  GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM         GACDEPGM
01870         MOVE  GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM       GACDEPGM
01871         MOVE  GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE          GACDEPGM
01872         MOVE  SPACES               TO  GCIO-WRK-LINE-OF-BUS       GACDEPGM
01873                                        GCIO-WRK-PROVIDER-CONTROL  GACDEPGM
01874         MOVE  GCA-FAM-REL-LVL       TO                            GACDEPGM
01875                            GCIO-WRK-FAMILY-RELATION-LVL           GACDEPGM
01876         MOVE  'G4'     TO  GCIO-WRK-RECORD-TYPE                   GACDEPGM
01877         MOVE GCA-EFFDT-CEN      TO  GCIO-WRK-EFFDT-CEN.           GACDEPGM
01878                                                                   GACDEPGM
01879      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GACDEPGM
01880         MOVE IDLINEI  TO  CONTRACT-ID-LINE                        GACDEPGM
01881         MOVE  SPACES               TO  GCIO-WORKFILE-KEY          GACDEPGM
01882         MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE       GACDEPGM
01883         MOVE  GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE         GACDEPGM
01884         MOVE  GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM         GACDEPGM
01885         MOVE  GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM       GACDEPGM
01886         MOVE  GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE          GACDEPGM
01887         MOVE  GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS       GACDEPGM
01888         MOVE  GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL  GACDEPGM
01889         MOVE  GCA-FAM-REL-LVL       TO                            GACDEPGM
01890                            GCIO-WRK-FAMILY-RELATION-LVL           GACDEPGM
01891         MOVE  'C3'     TO  GCIO-WRK-RECORD-TYPE                   GACDEPGM
01892         MOVE  GCA-EFFDT-CEN    TO  GCIO-WRK-EFFDT-CEN.            GACDEPGM
01893                                                                   GACDEPGM
01894      IF FRMNUIDI  =  'GC8A'                                       GACDEPGM
01895         MOVE IDLINEI  TO  BENEFIT-PROVISION-ID-LINE               GACDEPGM
01896         MOVE  SPACES               TO  GCIO-WORKFILE-KEY          GACDEPGM
01897         MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE       GACDEPGM
01898         MOVE  GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE         GACDEPGM
01899         MOVE  GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM         GACDEPGM
01900         MOVE  GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM       GACDEPGM
01901         MOVE  GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE          GACDEPGM
01902         MOVE  GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS       GACDEPGM
01903         MOVE  GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL  GACDEPGM
01904         MOVE  GCA-FAM-REL-LVL    TO GCIO-WRK-FAMILY-RELATION-LVL  GACDEPGM
01905         MOVE  'C6'     TO  GCIO-WRK-RECORD-TYPE                   GACDEPGM
01906         MOVE GCA-EFFDT-CEN     TO  GCIO-WRK-EFFDT-CEN.            GACDEPGM
01907                                                                   GACDEPGM
01908      IF FUNCTONI  =  'GA1B'                                       GACDEPGM
01909         PERFORM 4900-100-GAA-OCCURS-CHANGED.                      GACDEPGM
01910                                                                   GACDEPGM
01911      IF FUNCTONI  =  'GA1C'                                       GACDEPGM
01912         PERFORM 4900-200-GAB-OCCURS-CHANGED.                      GACDEPGM
01913                                                                   GACDEPGM
01914      IF FUNCTONI  =  'GA1D'                                       GACDEPGM
01915         PERFORM 4900-300-GAC-OCCURS-CHANGED.                      GACDEPGM
01916                                                                   GACDEPGM
01917      IF FUNCTONI  =  'GA1E'                                       GACDEPGM
01918         PERFORM 4900-400-GAD-OCCURS-CHANGED.                      GACDEPGM
01919                                                                   GACDEPGM
01920      IF FUNCTONI  =  'GA1P'                                       GACDEPGM
01921         PERFORM 4900-450-GAF-OCCURS-CHANGED.                      GACDEPGM
01922                                                                   GACDEPGM
01923      GO TO 4900-900-EXIT.                                         GACDEPGM
01924                                                                   GACDEPGM
01925  4900-100-GAA-OCCURS-CHANGED.                                     GACDEPGM
01926                                                                   GACDEPGM
01927      SET GAA-INDEX  TO  1.                                        GACDEPGM
01928      SEARCH GAA-ENTRY                                             GACDEPGM
01929         VARYING GAA-INDEX                                         GACDEPGM
01930         WHEN                                                      GACDEPGM
01931            GAA-INDEX  NOT <  GAA-ENTRY-COUNT OR                   GACDEPGM
01932            GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX)  =                 GACDEPGM
01933                                                ACWA-DISPLAY-LEN-7 GACDEPGM
01934            NEXT SENTENCE.                                         GACDEPGM
01935                                                                   GACDEPGM
01936      IF GAA-INDEX  <  GAA-ENTRY-COUNT AND                         GACDEPGM
01937         GAA-OCCURS-ENTRY-COUNTER(GAA-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01938         NEXT SENTENCE                                             GACDEPGM
01939      ELSE                                                         GACDEPGM
01940         MOVE WS-ABCODE-CDL1        TO  WS-ABCODE                  GACDEPGM
01941         MOVE WS-ABCODE-CDL1-MSG    TO  WS-ABCODE-MSG              GACDEPGM
01942         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
01943                                                                   GACDEPGM
01944      PERFORM 4900-110-CHEC-INTRNL-SLOT                            GACDEPGM
01945        VARYING GAA-INT-INDEX  FROM  1  BY  1                      GACDEPGM
01946        UNTIL GAA-INT-INDEX  NOT <                                 GACDEPGM
01947                           GAA-INTERNAL-TABULAR-COUNT(GAA-INDEX) ORGACDEPGM
01948            GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  =  HIGH-VALUES.   GACDEPGM
01949                                                                   GACDEPGM
01950  4900-110-CHEC-INTRNL-SLOT.                                       GACDEPGM
01951                                                                   GACDEPGM
01952      IF GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)   >  +8999999       GACDEPGM
01953         MOVE GAA-INT-ID(GAA-INDEX GAA-INT-INDEX)  TO              GACDEPGM
01954                                         GCIO-WRK-TAB-PROVISION-ID GACDEPGM
01955         MOVE GAA-INT-SLOT(GAA-INDEX GAA-INT-INDEX)  TO            GACDEPGM
01956                                         GCIO-WRK-TAB-PROV-SLOT-NO GACDEPGM
01957         PERFORM 5400-READ-UPD-INTR-TAB-REC                        GACDEPGM
01958         PERFORM 4900-500-CHEC-INTRNL-STATUS.                      GACDEPGM
01959                                                                   GACDEPGM
01960                                                                   GACDEPGM
01961  4900-200-GAB-OCCURS-CHANGED.                                     GACDEPGM
01962                                                                   GACDEPGM
01963      SET GAB-INDEX  TO  1.                                        GACDEPGM
01964      SEARCH GAB-ENTRY                                             GACDEPGM
01965         VARYING GAB-INDEX                                         GACDEPGM
01966         WHEN                                                      GACDEPGM
01967            GAB-INDEX  NOT <  GAB-ENTRY-COUNT OR                   GACDEPGM
01968            GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)  =                 GACDEPGM
01969                                                ACWA-DISPLAY-LEN-7 GACDEPGM
01970            NEXT SENTENCE.                                         GACDEPGM
01971                                                                   GACDEPGM
01972      IF GAB-INDEX  <  GAB-ENTRY-COUNT AND                         GACDEPGM
01973         GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
01974         NEXT SENTENCE                                             GACDEPGM
01975      ELSE                                                         GACDEPGM
01976         MOVE WS-ABCODE-CDL2        TO  WS-ABCODE                  GACDEPGM
01977         MOVE WS-ABCODE-CDL2-MSG    TO  WS-ABCODE-MSG              GACDEPGM
01978         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
01979                                                                   GACDEPGM
01980      PERFORM 4900-210-CHEC-INTRNL-SLOT                            GACDEPGM
01981        VARYING GAB-INT-INDEX  FROM  1  BY  1                      GACDEPGM
01982        UNTIL GAB-INT-INDEX  NOT <                                 GACDEPGM
01983                           GAB-INTERNAL-TABULAR-COUNT(GAB-INDEX) ORGACDEPGM
01984            GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  =  HIGH-VALUES.   GACDEPGM
01985                                                                   GACDEPGM
01986  4900-210-CHEC-INTRNL-SLOT.                                       GACDEPGM
01987                                                                   GACDEPGM
01988      IF GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)   >  +8999999       GACDEPGM
01989         MOVE GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  TO              GACDEPGM
01990                                         GCIO-WRK-TAB-PROVISION-ID GACDEPGM
01991         MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)  TO            GACDEPGM
01992                                         GCIO-WRK-TAB-PROV-SLOT-NO GACDEPGM
01993         PERFORM 5400-READ-UPD-INTR-TAB-REC                        GACDEPGM
01994         PERFORM 4900-500-CHEC-INTRNL-STATUS.                      GACDEPGM
01995                                                                   GACDEPGM
01996  4900-300-GAC-OCCURS-CHANGED.                                     GACDEPGM
01997                                                                   GACDEPGM
01998      SET GAC-INDEX  TO  1.                                        GACDEPGM
01999      SEARCH GAC-ENTRY                                             GACDEPGM
02000         VARYING GAC-INDEX                                         GACDEPGM
02001         WHEN                                                      GACDEPGM
02002            GAC-INDEX  NOT <  GAC-ENTRY-COUNT OR                   GACDEPGM
02003            GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  =                 GACDEPGM
02004                                                ACWA-DISPLAY-LEN-7 GACDEPGM
02005            NEXT SENTENCE.                                         GACDEPGM
02006                                                                   GACDEPGM
02007      IF GAC-INDEX  <  GAC-ENTRY-COUNT AND                         GACDEPGM
02008         GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
02009         NEXT SENTENCE                                             GACDEPGM
02010      ELSE                                                         GACDEPGM
02011         MOVE WS-ABCODE-CDL3        TO  WS-ABCODE                  GACDEPGM
02012         MOVE WS-ABCODE-CDL3-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02013         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02014                                                                   GACDEPGM
02015      PERFORM 4900-310-CHEC-INTRNL-SLOT                            GACDEPGM
02016        VARYING GAC-INT-INDEX  FROM  1  BY  1                      GACDEPGM
02017        UNTIL GAC-INT-INDEX  NOT <                                 GACDEPGM
02018                           GAC-INTERNAL-TABULAR-COUNT(GAC-INDEX) ORGACDEPGM
02019            GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  =  HIGH-VALUES.   GACDEPGM
02020                                                                   GACDEPGM
02021  4900-310-CHEC-INTRNL-SLOT.                                       GACDEPGM
02022                                                                   GACDEPGM
02023      IF GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)   >  +8999999       GACDEPGM
02024         MOVE GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  TO              GACDEPGM
02025                                         GCIO-WRK-TAB-PROVISION-ID GACDEPGM
02026         MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)  TO            GACDEPGM
02027                                         GCIO-WRK-TAB-PROV-SLOT-NO GACDEPGM
02028         PERFORM 5400-READ-UPD-INTR-TAB-REC                        GACDEPGM
02029         PERFORM 4900-500-CHEC-INTRNL-STATUS.                      GACDEPGM
02030                                                                   GACDEPGM
02031                                                                   GACDEPGM
02032  4900-400-GAD-OCCURS-CHANGED.                                     GACDEPGM
02033                                                                   GACDEPGM
02034      SET GAD-INDEX  TO  1.                                        GACDEPGM
02035      SEARCH GAD-ENTRY                                             GACDEPGM
02036         VARYING GAD-INDEX                                         GACDEPGM
02037         WHEN                                                      GACDEPGM
02038            GAD-INDEX  NOT <  GAD-ENTRY-COUNT OR                   GACDEPGM
02039            GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  =                 GACDEPGM
02040                                                ACWA-DISPLAY-LEN-7 GACDEPGM
02041            NEXT SENTENCE.                                         GACDEPGM
02042                                                                   GACDEPGM
02043      IF GAD-INDEX  <  GAD-ENTRY-COUNT AND                         GACDEPGM
02044         GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
02045         NEXT SENTENCE                                             GACDEPGM
02046      ELSE                                                         GACDEPGM
02047         MOVE WS-ABCODE-CDL4        TO  WS-ABCODE                  GACDEPGM
02048         MOVE WS-ABCODE-CDL4-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02049         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02050                                                                   GACDEPGM
02051      PERFORM 4900-410-CHEC-INTRNL-SLOT                            GACDEPGM
02052        VARYING GAD-INT-INDEX  FROM  1  BY  1                      GACDEPGM
02053        UNTIL GAD-INT-INDEX  NOT <                                 GACDEPGM
02054                           GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX) ORGACDEPGM
02055            GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  =  HIGH-VALUES.   GACDEPGM
02056                                                                   GACDEPGM
02057  4900-410-CHEC-INTRNL-SLOT.                                       GACDEPGM
02058                                                                   GACDEPGM
02059      IF GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)   >  +8999999       GACDEPGM
02060         MOVE GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  TO              GACDEPGM
02061                                         GCIO-WRK-TAB-PROVISION-ID GACDEPGM
02062         MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)  TO            GACDEPGM
02063                                         GCIO-WRK-TAB-PROV-SLOT-NO GACDEPGM
02064         PERFORM 5400-READ-UPD-INTR-TAB-REC                        GACDEPGM
02065         PERFORM 4900-500-CHEC-INTRNL-STATUS.                      GACDEPGM
02066                                                                   GACDEPGM
02067  4900-450-GAF-OCCURS-CHANGED.                                     GACDEPGM
02068                                                                   GACDEPGM
02069      SET GAF-INDEX  TO  1.                                        GACDEPGM
02070      SEARCH GAF-ENTRY                                             GACDEPGM
02071         VARYING GAF-INDEX                                         GACDEPGM
02072         WHEN                                                      GACDEPGM
02073            GAF-INDEX  NOT <  GAF-ENTRY-COUNT OR                   GACDEPGM
02074            GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  =                 GACDEPGM
02075                                                ACWA-DISPLAY-LEN-7 GACDEPGM
02076            NEXT SENTENCE.                                         GACDEPGM
02077                                                                   GACDEPGM
02078      IF GAF-INDEX  <  GAF-ENTRY-COUNT AND                         GACDEPGM
02079         GAF-OCCURS-ENTRY-COUNTER(GAF-INDEX)  =  ACWA-DISPLAY-LEN-7GACDEPGM
02080         NEXT SENTENCE                                             GACDEPGM
02081      ELSE                                                         GACDEPGM
02082         MOVE WS-ABCODE-CDL3        TO  WS-ABCODE                  GACDEPGM
02083         MOVE WS-ABCODE-CDL3-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02084         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02085                                                                   GACDEPGM
02086      PERFORM 4950-310-CHEC-INTRNL-SLOT                            GACDEPGM
02087        VARYING GAF-INT-INDEX  FROM  1  BY  1                      GACDEPGM
02088        UNTIL GAF-INT-INDEX  NOT <                                 GACDEPGM
02089                           GAF-INTERNAL-TABULAR-COUNT(GAF-INDEX) ORGACDEPGM
02090            GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  =  HIGH-VALUES.   GACDEPGM
02091                                                                   GACDEPGM
02092  4950-310-CHEC-INTRNL-SLOT.                                       GACDEPGM
02093                                                                   GACDEPGM
02094      IF GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)   >  +8999999       GACDEPGM
02095         MOVE GAF-INT-ID(GAF-INDEX GAF-INT-INDEX)  TO              GACDEPGM
02096                                         GCIO-WRK-TAB-PROVISION-ID GACDEPGM
02097         MOVE GAF-INT-SLOT(GAF-INDEX GAF-INT-INDEX)  TO            GACDEPGM
02098                                         GCIO-WRK-TAB-PROV-SLOT-NO GACDEPGM
02099         PERFORM 5400-READ-UPD-INTR-TAB-REC                        GACDEPGM
02100         PERFORM 4900-500-CHEC-INTRNL-STATUS.                      GACDEPGM
02101                                                                   GACDEPGM
02102  4900-500-CHEC-INTRNL-STATUS.                                     GACDEPGM
02103                                                                   GACDEPGM
02104      IF INTDESKI  =  IDPRODI                                      GACDEPGM
02105         IF WRK2-CDE-SP  =  '1U'          AND                      GACDEPGM
02106            WRK2-SIGNAL-FROM-ONLINE  NOT = 'W'                     GACDEPGM
02107            MOVE '2 '  TO  WRK2-CDE-SP                             GACDEPGM
02108            COMPUTE ACWA-CDE-1U-COUNT  =  ACWA-CDE-1U-COUNT  -  1  GACDEPGM
02109            COMPUTE ACWA-CDE-2B-COUNT  =  ACWA-CDE-2B-COUNT  +  1  GACDEPGM
02110            PERFORM 5600-REWRITE-INTRNL-TAB-REC                    GACDEPGM
02111         ELSE                                                      GACDEPGM
02112            PERFORM 5500-RLSE-RU-INTRNL-TAB-REC                    GACDEPGM
02113      ELSE                                                         GACDEPGM
02114         IF WRK2-CDE-SP  =  '2 '                                   GACDEPGM
02115            MOVE '1U'  TO  WRK2-CDE-SP                             GACDEPGM
02116            COMPUTE ACWA-CDE-1U-COUNT  =  ACWA-CDE-1U-COUNT  +  1  GACDEPGM
02117            COMPUTE ACWA-CDE-2B-COUNT  =  ACWA-CDE-2B-COUNT  -  1  GACDEPGM
02118            PERFORM 5600-REWRITE-INTRNL-TAB-REC                    GACDEPGM
02119         ELSE                                                      GACDEPGM
02120            PERFORM 5500-RLSE-RU-INTRNL-TAB-REC.                   GACDEPGM
02121                                                                   GACDEPGM
02122                                                                   GACDEPGM
02123  4900-900-EXIT.       EXIT.                                       GACDEPGM
02124                                                                   GACDEPGM
02125 /*****************************************************************GACDEPGM
02126 *     READ ALL LEVEL TABULAR FROM PROVISION POOL                  GACDEPGM
02127 *                                                                 GACDEPGM
02128 ******************************************************************GACDEPGM
02129  5000-000-READ-PROD-ALL-LVL-TAB  SECTION.                         GACDEPGM
02130                                                                   GACDEPGM
02131      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   GACDEPGM
02132               GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ABM-FIXED-LEN  +GACDEPGM
02133              (GC-GCTABULR-ABM-VARY-LEN  *                         GACDEPGM
02134                                    GC-GCTABULR-ABM-VARY-MAX-OCUR).GACDEPGM
02135                                                                   GACDEPGM
02136      IF  ACWA-PR-ALL-LEVEL-TAB-COMP  >  ZEROS                     GACDEPGM
02137          SET ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD  TO         GACDEPGM
02138              ACWA-PR-ALL-LEVEL-TAB-PNTR                           GACDEPGM
02139      ELSE                                                         GACDEPGM
02140         EXEC CICS GETMAIN                                         GACDEPGM
02141                SET(ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD)      GACDEPGM
02142                INITIMG(WS-HEX-00)                                 GACDEPGM
02143                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GACDEPGM
02144                END-EXEC                                           GACDEPGM
02145         SET ACWA-PR-ALL-LEVEL-TAB-PNTR                            GACDEPGM
02146             TO ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD.          GACDEPGM
02147                                                                   GACDEPGM
02148      MOVE TABIDI                  TO  GCIO-TAB-TABULAR-ID.        GACDEPGM
02149      MOVE PRDSLTNI                TO  ACWA-DISPLAY-LEN-7.         GACDEPGM
02150      MOVE ACWA-DISPLAY-LEN-7      TO  GCIO-TAB-SLOT-NO.           GACDEPGM
02151      MOVE GCIO-WORKFILE-KEY       TO  GCIOA-FILE-KEY.             GACDEPGM
02152      MOVE GC-GCIO-AREA-2          TO  GCIOA-IO-AREA-TO-USE.       GACDEPGM
02153      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIOA-FILE-ACCESS-CODE.     GACDEPGM
02154      MOVE GC-GCTABULR-DDNAME      TO  GCIOA-FILE-DDNAME.          GACDEPGM
02155      IF GCIO-TAB-TABULAR-ID  =  GAB2-PROVISION-ID AND             GACDEPGM
02156         GAB2-PROVISION-SLOT-NO  NUMERIC AND                       GACDEPGM
02157         GCIO-TAB-SLOT-NO     =  GAB2-PROVISION-SLOT-NO            GACDEPGM
02158         GO TO 5000-900-EXIT.                                      GACDEPGM
02159                                                                   GACDEPGM
02160      EXEC CICS  LINK   PROGRAM  ('GCIOPGM')                       GACDEPGM
02161                 COMMAREA (PR-IO-PARM-ALL-LVL-TAB-RECORD)          GACDEPGM
02162                 LENGTH (WS-IO-PARM-WRK-ALL-LVL-LEN)    END-EXEC.  GACDEPGM
02163                                                                   GACDEPGM
02164      IF NOT GCIOA-GOOD-RETURN                                     GACDEPGM
02165         MOVE WS-ABCODE-CDF1        TO  WS-ABCODE                  GACDEPGM
02166         MOVE WS-ABCODE-CDF1-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02167         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02168                                                                   GACDEPGM
02169  5000-900-EXIT.     EXIT.                                         GACDEPGM
02170                                                                   GACDEPGM
02171 /*****************************************************************GACDEPGM
02172 *     READ FOR UPDATE THE CONTROL RECORD FROM THE WORKFILE        GACDEPGM
02173 *                                                                 GACDEPGM
02174 ******************************************************************GACDEPGM
02175  5100-READ-CONTROL-UPDT SECTION.                                  GACDEPGM
02176                                                                   GACDEPGM
02177         EXEC CICS GETMAIN                                         GACDEPGM
02178                SET(ADDRESS OF WF-IO-PARM-WRK-CONTROL-REC)         GACDEPGM
02179                INITIMG(WS-HEX-00)                                 GACDEPGM
02180                LENGTH(WS-IO-PARM-WRK-CONTROL-LEN)                 GACDEPGM
02181                END-EXEC                                           GACDEPGM
02182                                                                   GACDEPGM
02183         SET ACWA-WF-CONTROL-RECORD-PNTR                           GACDEPGM
02184             TO ADDRESS OF WF-IO-PARM-WRK-CONTROL-REC.             GACDEPGM
02185                                                                   GACDEPGM
02186      MOVE GCIO-FILE-KEY   TO  GCIO-WORKFILE-KEY.                  GACDEPGM
02187      MOVE SPACES          TO  GCIO-WRK-FILLER-1.                  GACDEPGM
02188                                                                   GACDEPGM
02189      IF FRMNUIDI  =  'GS3A'                                       GACDEPGM
02190         MOVE 'G1'         TO  GCIO-WRK-RECORD-TYPE.               GACDEPGM
02191      IF  FRMNUIDI  =  'GC4A'  OR 'GTM1'                           GACDEPGM
02192         MOVE 'C1'         TO  GCIO-WRK-RECORD-TYPE.               GACDEPGM
02193      IF FRMNUIDI  =  'GC8A'                                       GACDEPGM
02194         MOVE 'C1'         TO  GCIO-WRK-RECORD-TYPE.               GACDEPGM
02195                                                                   GACDEPGM
02196      MOVE SPACES   TO  GCIO-WRK-PROVISION-ID.                     GACDEPGM
02197      MOVE ZEROS    TO  GCIO-WRK-PROVISION-SLOT-NO.                GACDEPGM
02198      MOVE SPACES   TO  GCIO-WRK-TAB-PROVISION-ID.                 GACDEPGM
02199      MOVE ZEROS    TO  GCIO-WRK-TAB-PROV-SLOT-NO.                 GACDEPGM
02200      MOVE GCIO-WORKFILE-KEY       TO  GCIO6-FILE-KEY.             GACDEPGM
02201      MOVE GC-GCIO-AREA-1          TO  GCIO6-IO-AREA-TO-USE.       GACDEPGM
02202      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO6-FILE-ACCESS-CODE.     GACDEPGM
02203      MOVE GC-GCPSWORK-DDNAME      TO  GCIO6-FILE-DDNAME.          GACDEPGM
02204                                                                   GACDEPGM
02205      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GACDEPGM
02206                 COMMAREA (WF-IO-PARM-WRK-CONTROL-REC)             GACDEPGM
02207                 LENGTH(WS-IO-PARM-WRK-CONTROL-LEN)   END-EXEC.    GACDEPGM
02208                                                                   GACDEPGM
02209  5199-EXIT.     EXIT.                                             GACDEPGM
02210                                                                   GACDEPGM
02211 ******************************************************************GACDEPGM
02212 * 5200  REWRITE THE CONTROL RECORD TO THE WORKFILE               *GACDEPGM
02213 ******************************************************************GACDEPGM
02214  5200-REWRITE-CONTROL-RECORD SECTION.                             GACDEPGM
02215                                                                   GACDEPGM
02216 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GACDEPGM
02217      MOVE '1'                     TO  GCIO6-OPER-ID-IND.          GACDEPGM
02218      MOVE GC-GCIO-ACCESS-CODE-WU  TO  GCIO6-FILE-ACCESS-CODE.     GACDEPGM
02219      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GACDEPGM
02220                 COMMAREA(WF-IO-PARM-WRK-CONTROL-REC)              GACDEPGM
02221                 LENGTH(WS-IO-PARM-WRK-CONTROL-LEN)   END-EXEC.    GACDEPGM
02222                                                                   GACDEPGM
02223      IF NOT GCIO6-GOOD-RETURN                                     GACDEPGM
02224         MOVE WS-ABCODE-CDF3      TO  WS-ABCODE                    GACDEPGM
02225         MOVE WS-ABCODE-CDF3-MSG  TO  WS-ABCODE-MSG                GACDEPGM
02226         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02227  5299-EXIT.     EXIT.                                             GACDEPGM
02228                                                                   GACDEPGM
02229 /*****************************************************************GACDEPGM
02230 *          READ THE CONTROL RECORD FROM THE WORKFILE              GACDEPGM
02231 *                                                                 GACDEPGM
02232 ******************************************************************GACDEPGM
02233  5300-READ-CONTROL-REC SECTION.                                   GACDEPGM
02234                                                                   GACDEPGM
02235      EXEC CICS GETMAIN                                            GACDEPGM
02236                SET(ADDRESS OF WF-IO-PARM-WRK-CONTROL-REC)         GACDEPGM
02237                INITIMG(WS-HEX-00)                                 GACDEPGM
02238                LENGTH(WS-IO-PARM-WRK-CONTROL-LEN)                 GACDEPGM
02239                END-EXEC                                           GACDEPGM
02240                                                                   GACDEPGM
02241         SET ACWA-WF-CONTROL-RECORD-PNTR                           GACDEPGM
02242             TO ADDRESS OF WF-IO-PARM-WRK-CONTROL-REC.             GACDEPGM
02243                                                                   GACDEPGM
02244      MOVE GCIO-FILE-KEY   TO  GCIO-WORKFILE-KEY.                  GACDEPGM
02245      MOVE SPACES          TO  GCIO-WRK-FILLER-1.                  GACDEPGM
02246                                                                   GACDEPGM
02247      IF FRMNUIDI  =  'GS3A'                                       GACDEPGM
02248         MOVE 'G1'         TO  GCIO-WRK-RECORD-TYPE.               GACDEPGM
02249      IF  FRMNUIDI  =  'GC4A'  OR 'GTM1'                           GACDEPGM
02250         MOVE 'C1'         TO  GCIO-WRK-RECORD-TYPE.               GACDEPGM
02251      IF FRMNUIDI  =  'GC8A'                                       GACDEPGM
02252         MOVE 'C1'         TO  GCIO-WRK-RECORD-TYPE.               GACDEPGM
02253                                                                   GACDEPGM
02254      MOVE SPACES   TO  GCIO-WRK-PROVISION-ID.                     GACDEPGM
02255      MOVE ZEROS    TO  GCIO-WRK-PROVISION-SLOT-NO.                GACDEPGM
02256      MOVE SPACES   TO  GCIO-WRK-TAB-PROVISION-ID.                 GACDEPGM
02257      MOVE ZEROS    TO  GCIO-WRK-TAB-PROV-SLOT-NO.                 GACDEPGM
02258      MOVE GCIO-WORKFILE-KEY       TO  GCIO6-FILE-KEY.             GACDEPGM
02259      MOVE GC-GCIO-AREA-1          TO  GCIO6-IO-AREA-TO-USE.       GACDEPGM
02260      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIO6-FILE-ACCESS-CODE.     GACDEPGM
02261      MOVE GC-GCPSWORK-DDNAME      TO  GCIO6-FILE-DDNAME.          GACDEPGM
02262                                                                   GACDEPGM
02263      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GACDEPGM
02264                 COMMAREA (WF-IO-PARM-WRK-CONTROL-REC)             GACDEPGM
02265                 LENGTH(WS-IO-PARM-WRK-CONTROL-LEN)   END-EXEC.    GACDEPGM
02266                                                                   GACDEPGM
02267  5399-EXIT.     EXIT.                                             GACDEPGM
02268 /*****************************************************************GACDEPGM
02269 *     READ FOR UPDATE THE INTERNAL TABULAR FROM THE WORKFILE      GACDEPGM
02270 *                                                                 GACDEPGM
02271 ******************************************************************GACDEPGM
02272  5400-READ-UPD-INTR-TAB-REC  SECTION.                             GACDEPGM
02273                                                                   GACDEPGM
02274      COMPUTE WS-IO-PARM-WRK-INTERNL-TAB-LEN  =  GC-GCIOPARM-LEN  +GACDEPGM
02275              GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-IPGP-FIXED-LEN  +GACDEPGM
02276              (GC-GCTABULR-IPGP-VARY-LEN  *                        GACDEPGM
02277                                   GC-GCTABULR-IPGP-VARY-MAX-OCUR).GACDEPGM
02278                                                                   GACDEPGM
02279      EXEC CICS GETMAIN                                            GACDEPGM
02280                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-REC)        GACDEPGM
02281                INITIMG(WS-HEX-00)                                 GACDEPGM
02282                LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)             GACDEPGM
02283                END-EXEC.                                          GACDEPGM
02284                                                                   GACDEPGM
02285         SET ACWA-WF-INTERNAL-TAB-PNTR                             GACDEPGM
02286             TO ADDRESS OF WF-IO-PARM-INTERNAL-TAB-REC.            GACDEPGM
02287                                                                   GACDEPGM
02288                                                                   GACDEPGM
02289      MOVE TABIDI                  TO  GCIO-WRK-PROVISION-ID.      GACDEPGM
02290      MOVE TABSLTNI                TO  ACWA-DISPLAY-LEN-7.         GACDEPGM
02291      MOVE ACWA-DISPLAY-LEN-7      TO  GCIO-WRK-PROVISION-SLOT-NO. GACDEPGM
02292                                                                   GACDEPGM
02293      MOVE GCIO-WORKFILE-KEY       TO  GCIO2-FILE-KEY.             GACDEPGM
02294      MOVE GC-GCIO-AREA-1          TO  GCIO2-IO-AREA-TO-USE.       GACDEPGM
02295      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO2-FILE-ACCESS-CODE.     GACDEPGM
02296      MOVE GC-GCPSWORK-DDNAME      TO  GCIO2-FILE-DDNAME.          GACDEPGM
02297                                                                   GACDEPGM
02298      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GACDEPGM
02299                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-REC)             GACDEPGM
02300                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.GACDEPGM
02301                                                                   GACDEPGM
02302      IF NOT GCIO2-GOOD-RETURN                                     GACDEPGM
02303         MOVE WS-ABCODE-CDFA        TO  WS-ABCODE                  GACDEPGM
02304         MOVE WS-ABCODE-CDFA-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02305         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02306                                                                   GACDEPGM
02307      MOVE GXAB-ENTRY-COUNT  TO  GXAB-ENTRY-COUNT.                 GACDEPGM
02308                                                                   GACDEPGM
02309  5499-EXIT.     EXIT.                                             GACDEPGM
02310                                                                   GACDEPGM
02311 ******************************************************************GACDEPGM
02312 * 5500  UNLOCK THE INTERNAL TABULAR RECORD EARLIER READ FOR UPDATEGACDEPGM
02313 ******************************************************************GACDEPGM
02314  5500-RLSE-RU-INTRNL-TAB-REC SECTION.                             GACDEPGM
02315                                                                   GACDEPGM
02316      MOVE GC-GCIO-ACCESS-CODE-UNL  TO  GCIO2-FILE-ACCESS-CODE.    GACDEPGM
02317      MOVE GC-GCPSWORK-DDNAME       TO  GCIO2-FILE-DDNAME.         GACDEPGM
02318                                                                   GACDEPGM
02319      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GACDEPGM
02320                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-REC)             GACDEPGM
02321                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.GACDEPGM
02322                                                                   GACDEPGM
02323      IF NOT GCIO2-GOOD-RETURN                                     GACDEPGM
02324         MOVE WS-ABCODE-CDFA        TO  WS-ABCODE                  GACDEPGM
02325         MOVE WS-ABCODE-CDFA-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02326         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02327  5599-EXIT.     EXIT.                                             GACDEPGM
02328                                                                   GACDEPGM
02329 ******************************************************************GACDEPGM
02330 * 5600  REWRITE THE INTERNAL TABULAR RECORD TO THE WORKFILE      *GACDEPGM
02331 ******************************************************************GACDEPGM
02332  5600-REWRITE-INTRNL-TAB-REC SECTION.                             GACDEPGM
02333                                                                   GACDEPGM
02334 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GACDEPGM
02335      MOVE '1'                     TO  GCIO2-OPER-ID-IND.          GACDEPGM
02336      COMPUTE  GCIO2-RECORD-LENGTH  =                              GACDEPGM
02337               GC-WORKFILE-KEY-LEN  +                              GACDEPGM
02338               GCIO2-RECORD-LENGTH.                                GACDEPGM
02339                                                                   GACDEPGM
02340      MOVE GC-GCIO-ACCESS-CODE-WU  TO  GCIO2-FILE-ACCESS-CODE.     GACDEPGM
02341      MOVE GC-GCPSWORK-DDNAME      TO  GCIO2-FILE-DDNAME.          GACDEPGM
02342                                                                   GACDEPGM
02343      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GACDEPGM
02344                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-REC)             GACDEPGM
02345                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)   END-EXEC.GACDEPGM
02346                                                                   GACDEPGM
02347      IF NOT GCIO2-GOOD-RETURN                                     GACDEPGM
02348         MOVE WS-ABCODE-CDFB        TO  WS-ABCODE                  GACDEPGM
02349         MOVE WS-ABCODE-CDFB-MSG    TO  WS-ABCODE-MSG              GACDEPGM
02350         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GACDEPGM
02351  5699-EXIT.     EXIT.                                             GACDEPGM
02352                                                                   GACDEPGM
02353                                                                   GACDEPGM
02354 /*****************************************************************GACDEPGM
02355 * 9200  GREGORIAN TO JULIAN                                      *GACDEPGM
02356 *                                                                *GACDEPGM
02357 *         MMDDYY---->YYDDD                                       *GACDEPGM
02358 ******************************************************************GACDEPGM
02359  9200-000-GREGORIAN-TO-JULIAN   SECTION.                          GACDEPGM
02360                                                                   GACDEPGM
02361                                                                   GACDEPGM
02362      MOVE 'CNV' TO  HGADATE-FUNC.                                 GACDEPGM
02363      MOVE 'M'   TO  HGADATE-FORM1.                                GACDEPGM
02364      MOVE 'J'   TO  HGADATE-FORM2.                                GACDEPGM
02365      MOVE ZEROS TO  HGADATE-RETURN                                GACDEPGM
02366                     HGADATE-AMOUNT.                               GACDEPGM
02367      EXEC CICS LINK PROGRAM ('HGADATES')                          GACDEPGM
02368                     COMMAREA(HGADATES-COMMAREA)                   GACDEPGM
02369                     LENGTH  (LENGTH OF HGADATES-COMMAREA)         GACDEPGM
02370                     END-EXEC.                                     GACDEPGM
02371                                                                   GACDEPGM
02372  9200-900-EXIT.       EXIT.                                       GACDEPGM
02373                                                                   GACDEPGM
02374 /*****************************************************************GACDEPGM
02375 * 9800  ERROR MSG THEN ABEND                                     *GACDEPGM
02376 *                                                                *GACDEPGM
02377 *    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *GACDEPGM
02378 *  AND THEN ABENDS USING THE ABEND CODE EARLIER DEFINED.         *GACDEPGM
02379 ******************************************************************GACDEPGM
02380  9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          GACDEPGM
02381                                                                   GACDEPGM
02382      MOVE -1               TO MFRMSLTL.                           GACDEPGM
02383      MOVE WS-ABCODE-MSG    TO ERRMSGO.                            GACDEPGM
02384                                                                   GACDEPGM
02385      EXEC CICS  SEND   MAP('GA1XI01')  ERASE  CURSOR   WAIT       GACDEPGM
02386                 MAPSET('GA1XSET')  END-EXEC.                      GACDEPGM
02387                                                                   GACDEPGM
02388      EXEC CICS  ABEND   ABCODE(WS-ABCODE)  END-EXEC.              GACDEPGM
02389                                                                   GACDEPGM
02390  9800-900-EXIT. EXIT.                                             GACDEPGM
