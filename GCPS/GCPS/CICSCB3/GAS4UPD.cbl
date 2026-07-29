00001  IDENTIFICATION DIVISION.                                         02/18/05
00002  PROGRAM-ID. GAS4UPD.                                             GAS4UPD 
00003 **** THIS IS A COBOL/2 PROGRAM ***                                   LV005
00004  AUTHOR. N ELBAZ.                                                 GAS4UPD 
00005  DATE-WRITTEN.   07/15/87.                                        GAS4UPD 
00006  DATE-COMPILED.                                                   GAS4UPD 
00007      SKIP3                                                        GAS4UPD 
00008 ******************************************************************GAS4UPD 
00009 *   GAS4UPD         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *GAS4UPD 
00010 *                   OUT-OF-POCKET LIMITS TABULAR         - #AOL  *GAS4UPD 
00011 *                                                                *GAS4UPD 
00012 *     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *GAS4UPD 
00013 *   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *GAS4UPD 
00014 *   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *GAS4UPD 
00015 *   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *GAS4UPD 
00016 *   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *GAS4UPD 
00017 *   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*GAS4UPD 
00018 *                                                                *GAS4UPD 
00019 *     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *GAS4UPD 
00020 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *GAS4UPD 
00021 *   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*GAS4UPD 
00022 *   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *GAS4UPD 
00023 *   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *GAS4UPD 
00024 *   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *GAS4UPD 
00025 *                                                                *GAS4UPD 
00026 *    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*GAS4UPD 
00027 *   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*GAS4UPD 
00028 *   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *GAS4UPD 
00029 *   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *GAS4UPD 
00030 *   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *GAS4UPD 
00031 *   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *GAS4UPD 
00032 *                                                                *GAS4UPD 
00033 *   FUNC CODE: GAS4                                              *GAS4UPD 
00034 *                          ********************************      *GAS4UPD 
00035 *                          *   THIS MAPSET IS SHARED BY   *      *GAS4UPD 
00036 *                          *   THE FOLLOWING MODULES:     *      *GAS4UPD 
00037 *                          *   1. GA1BPGM                 *      *GAS4UPD 
00038 *                          *   2. GA1CPGM                 *      *GAS4UPD 
00039 *                          *   3. GA1DPGM                 *      *GAS4UPD 
00040 *   MAPSET:    GA1XSETC ==>*   4. GA1EPGM                 *      *GAS4UPD 
00041 *                          *   5. GASEDIT1                *      *GAS4UPD 
00042 *                          *   6. GACDEPGM                *      *GAS4UPD 
00043 *                          *   7. GK1BPGM                 *      *GAS4UPD 
00044 *                          *   8. GK1CPGM                 *      *GAS4UPD 
00045 *                          *   9. GK1DPGM                 *      *GAS4UPD 
00046 *                          *  10. GK1EPGM                 *      *GAS4UPD 
00047 *                          *  11. GAS1UPD                 *      *GAS4UPD 
00048 *                          *  12. GAS2UPD                 *      *GAS4UPD 
00049 *                          *  11. GAS3UPD                 *      *GAS4UPD 
00050 *                          *  11. GAS4UPD                 *      *GAS4UPD 
00051 *                          ********************************      *GAS4UPD 
00052 *                                                                *GAS4UPD 
00053 *   FILES:     GCPSWORK           GCGRPSPC                       *GAS4UPD 
00054 *              GCTABULR           GCSTABLR                       *GAS4UPD 
00055 *              GCCONTR            GCSPROVN                       *GAS4UPD 
00056 *                                                                *GAS4UPD 
00057 ******************************************************************GAS4UPD 
00058                                                                   GAS4UPD 
00059 /    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS4UPD 
00060 *    *-*         U P D A T E   H I S T O R Y         *-*          GAS4UPD 
00061 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS4UPD 
00062                                                                   GAS4UPD 
00063 *NUM-* *-DATE-* *WHO* *-----------DESCRIPTION--------------------*GAS4UPD 
00064 *                                                                *GAS4UPD 
00065 * D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *GAS4UPD 
00066 * D200 06/07/87  NGE  SEPERATE CHANGE/DELETE FUNCTION TO         *GAS4UPD 
00067 *                     ANOTHER MODULE.                            *GAS4UPD 
00068 * N121 08/10/87  NGE  ADD NEW FIELD -DEFINITION-                 *GAS4UPD 
00069 * N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT                  *GAS4UPD 
00070 *                                                                *GAS4UPD 
00071 *  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *GAS4UPD 
00072 *                             4600- SECTION THAT CAUSED THE CDE  *GAS4UPD 
00073 *                             MODIFIED STATUS TO BE SET.         *GAS4UPD 
00074 *                                                                *GAS4UPD 
00075 * D143 02/01/88  DES  CDE/NON-CDE CHANGES                        *GAS4UPD 
00076 *                                                                *GAS4UPD 
00077 *  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *GAS4UPD 
00078 *                              TO 'A'.                           *GAS4UPD 
00079 *                                                                *GAS4UPD 
00080 *  R1218     06/02/88  NGE  1. CHANGE ACCUM LOGIC FOR MAPING     *GAS4UPD 
00081 *                                                                *GAS4UPD 
00082 * ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC TO FLAG  THE    *GAS4UPD 
00083 *                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *GAS4UPD 
00084 *                            TAB LOGIC TO FLAG ITS ACCUM WITH CDE*GAS4UPD 
00085 *                            WHEN THE INTERNL FLAGED CDE.        *GAS4UPD 
00086 *                                                                *GAS4UPD 
00087 * ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *GAS4UPD 
00088 *                        UPDATING CDE COUNTERS DEPENDING ON THE  *GAS4UPD 
00089 *                        INTRNL TAB RECORD NOT THE CDE STATUS    *GAS4UPD 
00090 *                        IN THE ACCUM RECORD ATTACHED.           *GAS4UPD 
00091 *                                                                *GAS4UPD 
00092 *D????  01/13/89 ENW   DARKENED THE BISCENDING IND FIELD.         GAS4UPD 
00093 *                                                                *GAS4UPD 
00094 * D200 05/18/89  NGE  ADD TWO NEW COND-BITS TMJ AND INF,         *GAS4UPD 
00095 *                     TEMPROMAND-JOINT AND INFERTILITY-COND.     *GAS4UPD 
00096 *                                                                *GAS4UPD 
00097 * 11154   10/15/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GAS4UPD 
00098 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GAS4UPD 
00099 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GAS4UPD 
00100 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GAS4UPD 
00101 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GAS4UPD 
00102 *                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*GAS4UPD 
00103 *                       7. >>> CONVERT TO COBOL/2 <<<.           *GAS4UPD 
00104 *                                                                *GAS4UPD 
00105 * 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *GAS4UPD 
00106 *                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *GAS4UPD 
00107 *                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*GAS4UPD 
00108 *                                                                *GAS4UPD 
00109 * 11154 02/19/91  NGE  REDUCE OCCUR MAX NUM FROM 46 TO 44.       *GAS4UPD 
00110 *                                                                *GAS4UPD 
00111 *D12009 09/16/91  BSO  CHANGES FOR FRL EXPANSION                 *GAS4UPD 
00112 *                                                                *GAS4UPD 
00113 * P-034  09/17/91  ENW  FIXED PROGRAM ERROR LEFT OVER FROM THE   *GAS4UPD 
00114 *                       11154 ACCUM EXPANSION. MOVED HIGH VALUES *GAS4UPD 
00115 *                       TO THE LAST OCCURS AFTER A DELETE OF AN  *GAS4UPD 
00116 *                       INTERNAL TABULAR.                        *GAS4UPD 
00117 *                                                                *GAS4UPD 
00118 * 12262  02/28/92 TPM ADD NEW COND-BIT LIF  (LIFE-THREATING)     *GAS4UPD 
00119 *                     COND-LIFE-THREAT-BIT                       *GAS4UPD 
00120 *                                                                *GAS4UPD 
00121 *  D303  02/03/97 DAU ADD FEAK INDICATOR                         *GAS4UPD 
00122 *                                                                *GAS4UPD 
00123 * 14726/ 11/10/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *GAS4UPD 
00124 * 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *GAS4UPD 
00125 *                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *GAS4UPD 
00126 *                                                                *GAS4UPD 
00127 * 14726/ 11/30/97  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *GAS4UPD 
00128 * 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *GAS4UPD 
00129 *                       INCREASE GROUP AND SECTION NUMBERS.      *GAS4UPD 
00130 *                                                                *GAS4UPD 
00131 *  D341  10/07/98  GDM  HIDE TIME/DOLLAR FIELD FROM SCREEN       *GAS4UPD 
00132 *                                                                *GAS4UPD 
00133 *        07/07/00  GSP  ADD LOGIC FOR NEW #IPGS INTERNAL TABULAR.*GAS4UPD 
00134 *                                                                *GAS4UPD 
00135 *D303  02/03/97  DAU  ADD ACCUM IDENTIFIER                       *GAS4UPD 
00136 *                                                                *GAS4UPD 
00137 *        11/29/00  GSP  ADD LOGIC TO DISPLAY MESSAGE IF A 6TH    *GAS4UPD 
00138 *                       INTERNAL TABULAR IS ATTEMPTED TO BE      *GAS4UPD 
00139 *                       ADDED.                                   *GAS4UPD 
00140 *                                                                *GAS4UPD 
00141 *        01/12/01 GSP ADD LOGIC TO PREVENT INTERNAL TABULAR      *GAS4UPD 
00142 *                     COUNT FROM BEING INCREASED TO GREATER      *GAS4UPD 
00143 *                     THAN 5.                                    *GAS4UPD 
00144 *                                                                *GAS4UPD 
00145 *        11/15/01 AKK ADD SUPPORT FOR 4 NEW BITS TWO FOR SERIOUS *GAS4UPD 
00146 *                     MENTAL ILLNESS AND 2 FOR EMER SERVICES.    *GAS4UPD 
00147 *                                                                *GAS4UPD 
00148 *D365B 06/03/02   JP  ADD COMBINATION APPLIED INDICATOR (CAPI)   *GAS4UPD 
00149 *                                                                *GAS4UPD 
00150 *  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *GAS4UPD 
00151 *                         DETERMINATION (SABD)                   *GAS4UPD 
00152 *                                                                *GAS4UPD 
00153 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GAS4UPD 
00154 *                                                                *GAS4UPD 
00155 *            06-13-03   DAF   CORRECTED PROGRAM SO THAT THE      *GAS4UPD 
00156 *                             TABULAR ID IS ON THE 'C6' RECORD   *GAS4UPD 
00157 *                             INSTEAD OF THE BENEFIT PROVISION ID*GAS4UPD 
00158 *                             AND THE TABULAR SLOT NUMBER IS ON  *GAS4UPD 
00159 *                             THE 'C6' RECORD INSTEAD OF THE     *GAS4UPD 
00160 *                             BENEFIT PROVISION SLOT NUMBER      *GAS4UPD 
00161 *                             USE COPYBOOK GCTIPGPC INSTEAD OF   *GAS4UPD 
00162 *                             GCTIPGTC                           *GAS4UPD 
00163 *                                                                *GAS4UPD 
00153 *            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *GAS4UPD 
00154 *                                                                *GAS4UPD 
00156 *            04-14-25   CJB   FIXED ISSUE WITH OCCURS ENTRIES    *GAS1UPD 
00164 ******************************************************************GAS4UPD 
00165                                                                   GAS4UPD 
00166 **************************************************************    GAS4UPD 
00167 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS4UPD 
00168 * - DEAD CODE ELIMINATION.                                        GAS4UPD 
      *                                                               *         
      * P21595  09/19/16  HSB  CHANGES FOR NEW FIELDS - BEN TYPE CODE,*         
      *                        BENEFIT TIER CODE,BENEFIT TIER LEVEL.  *         
SI0724* P56703  05/08/24  SI   RECOMPILE - PEAQ COPYBOOK EXPANSION    *         
SI0724*                        COPY ABM, ACP, ACL, ADL, AOL,          *         
SI0724*                        GCCDRLEN                               *         
00169 ***************************************************************** GAS4UPD 
00170                                                                   GAS4UPD 
00171      SKIP3                                                        GAS4UPD 
00172  ENVIRONMENT DIVISION.                                            GAS4UPD 
00173 /    D A T A   D I V I S I O N                                    GAS4UPD 
00174  DATA DIVISION.                                                   GAS4UPD 
00175  WORKING-STORAGE SECTION.                                         GAS4UPD 
00176  01  WS-BEGIN                    PIC X(24)  VALUE                 GAS4UPD 
00177      '***GAS4UPD WS BEGINS***'.                                   GAS4UPD 
00178                                                                   GAS4UPD 
00179 *    T I T L E   L I N E S                                        GAS4UPD 
00180  01  WS-TITLE-LINES.                                              GAS4UPD 
00181  COPY GCMHLINE.                                                   GAS4UPD 
00182 *****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                GAS4UPD 
00183 *      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        GAS4UPD 
00184 *    05  GROUP-SPECIFIC-ID-LINE.                                  GAS4UPD 
00185 *      10  FILLER                        PIC X(20)                GAS4UPD 
00186 *        VALUE 'GROUP SPECIFIC ID= '.                             GAS4UPD 
00187 *      10  FILLER                        PIC X(5) VALUE 'GRP= '.  GAS4UPD 
00188 *      10  GRP-SPEC-GROUP-NO             PIC X(6).                GAS4UPD 
00189 *      10  FILLER                        PIC X(6) VALUE ' SEC= '. GAS4UPD 
00190 *      10  GRP-SPEC-SECTION-NO           PIC X(4).                GAS4UPD 
00191 *      10  FILLER                        PIC X(5) VALUE ' FR= '.  GAS4UPD 
00192 *      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  GAS4UPD 
00193 *      10  FILLER                        PIC X(7) VALUE ' EFDT= '.GAS4UPD 
00194 *      10  GRP-SPEC-EFF-DATE             PIC X(6).                GAS4UPD 
00195 *    05  CONTRACT-TITLE-LINE             PIC X(42)                GAS4UPD 
00196 *      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         GAS4UPD 
00197 *    05  CONTRACT-ID-LINE.                                        GAS4UPD 
00198 *      10  FILLER                      PIC X(14)                  GAS4UPD 
00199 *        VALUE 'CONTRACT ID= '.                                   GAS4UPD 
00200 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS4UPD 
00201 *      10  CONTRACT-GROUP-NO           PIC X(6).                  GAS4UPD 
00202 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS4UPD 
00203 *      10  CONTRACT-SECTION-NO         PIC X(4).                  GAS4UPD 
00204 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS4UPD 
00205 *      10  CONTRACT-LOB                PIC X.                     GAS4UPD 
00206 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS4UPD 
00207 *      10  CONTRACT-PROV-CTL           PIC XX.                    GAS4UPD 
00208 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS4UPD 
00209 *      10  CONTRACT-FAM-REL-LVL        PIC XX.                    GAS4UPD 
00210 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS4UPD 
00211 *      10  CONTRACT-EFF-DATE               PIC X(6).              GAS4UPD 
00212 *    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                GAS4UPD 
00213 *      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        GAS4UPD 
00214 *    05  BENEFIT-PROVISION-ID-LINE.                               GAS4UPD 
00215 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS4UPD 
00216 *      10  BEN-PROV-GROUP-NO           PIC X(6).                  GAS4UPD 
00217 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS4UPD 
00218 *      10  BEN-PROV-SECTION-NO         PIC X(4).                  GAS4UPD 
00219 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS4UPD 
00220 *      10  BEN-PROV-LOB                PIC X.                     GAS4UPD 
00221 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS4UPD 
00222 *      10  BEN-PROV-PROV-CTL           PIC XX.                    GAS4UPD 
00223 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS4UPD 
00224 *      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    GAS4UPD 
00225 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS4UPD 
00226 *      10  BEN-PROV-EFF-DATE           PIC X(6).                  GAS4UPD 
00227 *      10  FILLER                      PIC X(8) VALUE ' BPVID= '. GAS4UPD 
00228 *      10  BEN-PROV-ID-NO              PIC X(6).                  GAS4UPD 
00229 *    05  AOL-TITLE-LINE                PIC X(26)                  GAS4UPD 
00230 ******** VALUE '  OUT-OF-POCKET LIMITS    '.                      GAS4UPD 
00231 /    A L T E R N A T I V E   W O R K F I L E   K E Y S            GAS4UPD 
00232  01  FILLER                      PIC X(32)  VALUE                 GAS4UPD 
00233      '*** ALTERNATIVE WORKFILE KEY ***'.                          GAS4UPD 
00234  01  SAVE-WS-ALT-WORKFILE-KEYS.                                   GAS4UPD 
00235      05 FILLER                   PIC X(63) VALUE SPACES.          GAS4UPD 
00236                                                                   GAS4UPD 
00237  01  SAVE-RESTORE-KEY.                                            GAS4UPD 
00238      05 WS-SV-RESTO-KY           PIC X(63) VALUE SPACES.          GAS4UPD 
00239                                                                   GAS4UPD 
00240  01  WS-ALT-WORKFILE-KEYS.                                        GAS4UPD 
00241  COPY GCWRKKEY.                                                   GAS4UPD 
00242                                                                   GAS4UPD 
00243                                                                   GAS4UPD 
00244 *   D A T E   F O R M A T T I N G   C O M M A R E A               GAS4UPD 
00245                                                                   GAS4UPD 
00246  01  HGADATES-COMMAREA.                                           GAS4UPD 
00247  COPY HGCDAT01.                                                   GAS4UPD 
00248                                                                   GAS4UPD 
00249 *    W O R K F I E L D S ,   A N D   S W I T C H E S              GAS4UPD 
00250  01  WS-WORK-FIELDS.                                              GAS4UPD 
00251                                                                   GAS4UPD 
00252      05  WS-SPACES-ZEROS.                                         GAS4UPD 
00253        10  WS-SPACES-ZEROS-SPACES       PIC X(6)  VALUE SPACES.   GAS4UPD 
00254        10  WS-SPACES-ZEROS-ZEROS        PIC S9(7) COMP-3          GAS4UPD 
00255                                                   VALUE ZEROS.    GAS4UPD 
00256                                                                   GAS4UPD 
00257      05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    GAS4UPD 
00258                                                                   GAS4UPD 
00259      05  WS-TAB-PROV-COPY-SLOT          PIC S9(7) COMP-3.         GAS4UPD 
00260      05  WS-INTL-TAB-ID.                                          GAS4UPD 
00261        10  WS-INTL-TAB-TAB-ID           PIC X(6).                 GAS4UPD 
00262        10  WS-INTL-TAB-TAB-SLOT         PIC S9(7) COMP-3.         GAS4UPD 
00263      05  WS-SAVE-INTL-TAB.                                        GAS4UPD 
00264        10  WS-SAVE-INTL-TAB-ID          PIC X(6).                 GAS4UPD 
00265        10  WS-SAVE-INTL-TAB-SLOT        PIC S9(7) COMP-3.         GAS4UPD 
00266                                                                   GAS4UPD 
00267      05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  GAS4UPD 
00268      05  WS-QUOTIENT                   PIC 999  COMP-3.           GAS4UPD 
00269      05  WS-REMAINDER                  PIC 999  COMP-3.           GAS4UPD 
00270                                                                   GAS4UPD 
00271      05  WS-CDE-REQUEST-CODES.                                    GAS4UPD 
00272          10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.GAS4UPD 
00273          10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.GAS4UPD 
00274          10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.GAS4UPD 
00275          10  WS-REQUEST-4800-CNTL-UPDATE    PIC X(4) VALUE '4800'.GAS4UPD 
00276          10  WS-REQUEST-4900-CNTL-UPDATE    PIC X(4) VALUE '4900'.GAS4UPD 
00277                                                                   GAS4UPD 
00278      05  WS-INTRNL-TABS-TO-CHG-CNT          PIC S9   COMP-3.      GAS4UPD 
00279      05  WS-INT-TAB-CHANGE-INDICATOR        PIC XX   VALUE SPACE. GAS4UPD 
00280        88  WS-INT-DESCRP-CHG-TO-NON-PROD        VALUE 'PN'.       GAS4UPD 
00281        88  WS-INT-DESCRP-CHG-BACK-TO-PROD       VALUE 'NP'.       GAS4UPD 
00282        88  WS-INT-DESCRP-NOCHG-AT-PROD          VALUE '  '.       GAS4UPD 
00283        88  WS-INT-DESCRP-NOCHG-AT-NONPROD       VALUE 'NN'.       GAS4UPD 
00284                                                                   GAS4UPD 
00285 *    I N T E R N A L   T A B U L A R   P R O G R A M   N A M E    GAS4UPD 
00286  01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  GAS4UPD 
00287                                                                   GAS4UPD 
00288                                                                   GAS4UPD 
00289 ** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          GAS4UPD 
00290  01  WS-ENTRY                          PIC X(176).                GAS4UPD 
00291      SKIP3                                                        GAS4UPD 
00292 /   A T T R I B U T E S                                           GAS4UPD 
00293  COPY DFHBMSCA.                                                   GAS4UPD 
00294      02  DFHBMABF                PIC X VALUE '9'.                 GAS4UPD 
00295 /   A T T E N T I O N   I D E N T I F I E R S                     GAS4UPD 
00296  COPY DFHAID.                                                     GAS4UPD 
00297 /   R E C O R D   L E N G T H S                                   GAS4UPD 
00298                                                                   GAS4UPD 
00299  01  WS-RECORD-LENGTHS.                                           GAS4UPD 
00300 *   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   GAS4UPD 
00301     05 WS-COMMON-WORKAREA-LEN         PIC S9(4) COMP  VALUE +550. GAS4UPD 
00302     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. GAS4UPD 
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +8096.GAS4UPD 
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.GAS4UPD 
00304     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00305     05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00306     05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00307     05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00308     05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00309     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00310     05 WS-WF-INTR-TAB-LEN             PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00311     05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00312     05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00313     05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   GAS4UPD 
00314     05 WS-NEW-OCCR-ON-WF              PIC X   VALUE SPACES.       GAS4UPD 
00315                                                                   GAS4UPD 
00316 ******************************************************************GAS4UPD 
00317 ** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **GAS4UPD 
00318 ******************************************************************GAS4UPD 
00319  01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  GAS4UPD 
00320  01  CURNT-OCURS-PKD             PIC 9(4).                        GAS4UPD 
00321  01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            GAS4UPD 
00322      05  FILLER                  PIC XX.                          GAS4UPD 
00323      05  CURNT-OCCURS-OUT        PIC XX.                          GAS4UPD 
00324                                                                   GAS4UPD 
00325  01  TOTAL-OCURS-UNK             PIC 9(5).                        GAS4UPD 
00326  01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            GAS4UPD 
00327      05  FILLER                  PIC XXX.                         GAS4UPD 
00328      05  TOTAL-OCCURS-OUT        PIC XX.                          GAS4UPD 
00329 /    G . C .   R E C O R D S   L E N G T H S                      GAS4UPD 
00330  01  WS-GC-RECORD-LENGTHS.                                        GAS4UPD 
00331      COPY GCCDRLEN.                                               GAS4UPD 
00332                                                                   GAS4UPD 
00333 /    A B E N D   A R E A                                          GAS4UPD 
00334  01  WS-01-ABEND-AREA.                                            GAS4UPD 
00335      05  FILLER                   PIC X(16)  VALUE                GAS4UPD 
00336          '** ABEND AREA **'.                                      GAS4UPD 
00337                                                                   GAS4UPD 
00338      05  WS-ABCODE-CODES-AND-MSG.                                 GAS4UPD 
00339          10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. GAS4UPD 
00340          10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. GAS4UPD 
00341          10  WS-ABCODE-1EC1             PIC X(04)  VALUE  '1EC1'. GAS4UPD 
00342          10  WS-ABCODE-1EC1-MSG         PIC X(79)  VALUE          GAS4UPD 
00343              '*** INVALID PARAMETER LENGTH FOUND ***              GAS4UPD 
00344 -            '                           '.                       GAS4UPD 
00345          10  WS-ABCODE-1EF1             PIC X(04)  VALUE  '1EF1'. GAS4UPD 
00346          10  WS-ABCODE-1EF1-MSG         PIC X(79)  VALUE          GAS4UPD 
00347              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS4UPD 
00348 -            ' CONTACT SYSTEMS ***       '.                       GAS4UPD 
00349          10  WS-ABCODE-1EF2             PIC X(04)  VALUE  '1EF2'. GAS4UPD 
00350          10  WS-ABCODE-1EF2-MSG         PIC X(79)  VALUE          GAS4UPD 
00351              '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTAGAS4UPD 
00352 -            'CT SYSTEMS ***             '.                       GAS4UPD 
00353          10  WS-ABCODE-1EF3             PIC X(04)  VALUE  '1EF3'. GAS4UPD 
00354          10  WS-ABCODE-1EF3-MSG         PIC X(79)  VALUE          GAS4UPD 
00355              '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTAGAS4UPD 
00356 -            'CT SYSTEMS ***             '.                       GAS4UPD 
00357          10  WS-ABCODE-1EF4             PIC X(04)  VALUE  '1EF4'. GAS4UPD 
00358          10  WS-ABCODE-1EF4-MSG         PIC X(79)  VALUE          GAS4UPD 
00359              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS4UPD 
00360 -            ' SYSTEMS ***               '.                       GAS4UPD 
00361          10  WS-ABCODE-1EF5             PIC X(04)  VALUE  '1EF5'. GAS4UPD 
00362          10  WS-ABCODE-1EF5-MSG         PIC X(79)  VALUE          GAS4UPD 
00363              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILEGAS4UPD 
00364 -            '.  PLEASE CONTACT SYSTEMS  '.                       GAS4UPD 
00365          10  WS-ABCODE-1EF6             PIC X(04)  VALUE  '1EF6'. GAS4UPD 
00366          10  WS-ABCODE-1EF6-MSG         PIC X(79)  VALUE          GAS4UPD 
00367              '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE GAS4UPD 
00368 -            'CONTACT SYSTEMS ***        '.                       GAS4UPD 
00369          10  WS-ABCODE-1EF7             PIC X(04)  VALUE  '1EF7'. GAS4UPD 
00370          10  WS-ABCODE-1EF7-MSG         PIC X(79)  VALUE          GAS4UPD 
00371              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS4UPD 
00372 -            ' SYSTEMS ***               '.                       GAS4UPD 
00373          10  WS-ABCODE-1EF8             PIC X(04)  VALUE  '1EF8'. GAS4UPD 
00374          10  WS-ABCODE-1EF8-MSG         PIC X(79)  VALUE          GAS4UPD 
00375              '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTACGAS4UPD 
00376 -            'T SYSTEMS ***              '.                       GAS4UPD 
00377          10  WS-ABCODE-1EF9             PIC X(04)  VALUE  '1EF9'. GAS4UPD 
00378          10  WS-ABCODE-1EF9-MSG         PIC X(79)  VALUE          GAS4UPD 
00379              '*** ERROR WHEN READING  INTERNAL TAB.  PLEASE CONTACGAS4UPD 
00380 -            'T SYSTEMS ***              '.                       GAS4UPD 
00381          10  WS-ABCODE-1EL1             PIC X(04)  VALUE  '1EL1'. GAS4UPD 
00382          10  WS-ABCODE-1EL1-MSG         PIC X(79)  VALUE          GAS4UPD 
00383              '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***GAS4UPD 
00384 -            '                           '.                       GAS4UPD 
00385          10  WS-ABCODE-1EL2             PIC X(04)  VALUE  '1EL2'. GAS4UPD 
00386          10  WS-ABCODE-1EL2-MSG         PIC X(79)  VALUE          GAS4UPD 
00387              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS4UPD 
00388 -            'MS ***                     '.                       GAS4UPD 
00389          10  WS-ABCODE-1EL3             PIC X(04)  VALUE  '1EL3'. GAS4UPD 
00390          10  WS-ABCODE-1EL3-MSG         PIC X(79)  VALUE          GAS4UPD 
00391              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS4UPD 
00392 -            'EMS ***                    '.                       GAS4UPD 
00393          10  WS-ABCODE-1ELX             PIC X(04)  VALUE  '1ELX'. GAS4UPD 
00394          10  WS-ABCODE-1ELX-MSG         PIC X(79)  VALUE          GAS4UPD 
00395              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS4UPD 
00396 -            'MS ***                     '.                       GAS4UPD 
00397          10  WS-ABCODE-1EP1             PIC X(04)  VALUE  '1EP1'. GAS4UPD 
00398          10  WS-ABCODE-1EP1-MSG         PIC X(79)  VALUE          GAS4UPD 
00399              '????????????????????????????????????????????????????GAS4UPD 
00400 -            '???????????????????????????'.                       GAS4UPD 
00401                                                                   GAS4UPD 
00402 /*****************************************************************GAS4UPD 
00403 *    WT-01   M E S S A G E   T A B L E                            GAS4UPD 
00404 ******************************************************************GAS4UPD 
00405  01  WT-01-TABLE.                                                 GAS4UPD 
00406      05  FILLER                  PIC X(16) VALUE                  GAS4UPD 
00407          '* WT-01-TABLE  *'.                                      GAS4UPD 
00408  01  FILLER.                                                      GAS4UPD 
00409      05  WT-01-MESSAGE-VALUES.                                    GAS4UPD 
00410                                                                   GAS4UPD 
00411 *----------------------------------------------------------------*GAS4UPD 
00412          10  WT-01-ENTRY-001.                                     GAS4UPD 
00413              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00414              15  WT-01-MESSAGE-TEXT-001.                          GAS4UPD 
00415                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00416                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00417                  20  FILLER          PIC X(3)  VALUE  '001'.      GAS4UPD 
00418                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00419                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00420                      '#IBGR HAS BEEN SUCCESSFULLY MAPPED          GAS4UPD 
00421 -                    '                         '.                 GAS4UPD 
00422              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00423 *----------------------------------------------------------------*GAS4UPD 
00424          10  WT-01-ENTRY-002.                                     GAS4UPD 
00425              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00426              15  WT-01-MESSAGE-TEXT-002.                          GAS4UPD 
00427                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00428                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00429                  20  FILLER          PIC X(3)  VALUE  '002'.      GAS4UPD 
00430                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00431                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00432                      '#IPGN HAS BEEN SUCCESSFULLY MAPPED          GAS4UPD 
00433 -                    '                         '.                 GAS4UPD 
00434              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00435 *----------------------------------------------------------------*GAS4UPD 
00436          10  WT-01-ENTRY-003.                                     GAS4UPD 
00437              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00438              15  WT-01-MESSAGE-TEXT-003.                          GAS4UPD 
00439                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00440                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00441                  20  FILLER          PIC X(3)  VALUE  '003'.      GAS4UPD 
00442                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00443                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00444                      '#IPGT HAS BEEN SUCCESSFULLY MAPPED          GAS4UPD 
00445 -                    '                         '.                 GAS4UPD 
00446              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00447 *----------------------------------------------------------------*GAS4UPD 
00448          10  WT-01-ENTRY-004.                                     GAS4UPD 
00449              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00450              15  WT-01-MESSAGE-TEXT-004.                          GAS4UPD 
00451                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00452                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00453                  20  FILLER          PIC X(3)  VALUE  '004'.      GAS4UPD 
00454                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00455                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00456                      '******************* F U T U R E   U S E ****GAS4UPD 
00457 -                    '*************************'.                 GAS4UPD 
00458              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00459 *----------------------------------------------------------------*GAS4UPD 
00460          10  WT-01-ENTRY-005.                                     GAS4UPD 
00461              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00462              15  WT-01-MESSAGE-TEXT-005.                          GAS4UPD 
00463                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00464                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00465                  20  FILLER          PIC X(3)  VALUE  '005'.      GAS4UPD 
00466                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00467                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00468                      '******************* F U T U R E   U S E ****GAS4UPD 
00469 -                    '*************************'.                 GAS4UPD 
00470              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00471 *----------------------------------------------------------------*GAS4UPD 
00472          10  WT-01-ENTRY-006.                                     GAS4UPD 
00473              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00474              15  WT-01-MESSAGE-TEXT-006.                          GAS4UPD 
00475                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00476                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00477                  20  FILLER          PIC X(3)  VALUE  '006'.      GAS4UPD 
00478                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00479                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00480                      'DELETE OPTION MUST BE \
00481 -                    'VALID                    '.                 GAS4UPD 
00482              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00483 *----------------------------------------------------------------*GAS4UPD 
00484          10  WT-01-ENTRY-007.                                     GAS4UPD 
00485              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00486              15  WT-01-MESSAGE-TEXT-007.                          GAS4UPD 
00487                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00488                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00489                  20  FILLER          PIC X(3)  VALUE  '007'.      GAS4UPD 
00490                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00491                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00492                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSGAS4UPD 
00493 -                    ' PF4/PF16 TO CONTINUE    '.                 GAS4UPD 
00494              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00495 *----------------------------------------------------------------*GAS4UPD 
00496          10  WT-01-ENTRY-008.                                     GAS4UPD 
00497              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00498              15  WT-01-MESSAGE-TEXT-008.                          GAS4UPD 
00499                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00500                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00501                  20  FILLER          PIC X(3)  VALUE  '008'.      GAS4UPD 
00502                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00503                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00504                      'GROUP IN CONVERSION STATUS, CANNOT CHANGE HIGAS4UPD 
00505 -                    'GH-LIGHTED ELEMENTS      '.                 GAS4UPD 
00506              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00507 *----------------------------------------------------------------*GAS4UPD 
00508          10  WT-01-ENTRY-009.                                     GAS4UPD 
00509              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00510              15  WT-01-MESSAGE-TEXT-009.                          GAS4UPD 
00511                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00512                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00513                  20  FILLER          PIC X(3)  VALUE  '009'.      GAS4UPD 
00514                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00515                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00516                      'INVALID PFKEY SELECTION                     GAS4UPD 
00517 -                    '                         '.                 GAS4UPD 
00518              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00519 *----------------------------------------------------------------*GAS4UPD 
00520          10  WT-01-ENTRY-010.                                     GAS4UPD 
00521              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00522              15  WT-01-MESSAGE-TEXT-010.                          GAS4UPD 
00523                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00524                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00525                  20  FILLER          PIC X(3)  VALUE  '010'.      GAS4UPD 
00526                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00527                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00528                      'INVALID REQUEST.  THAT PF KEY HAS NO MEANINGGAS4UPD 
00529 -                    ' TO THIS PROGRAM         '.                 GAS4UPD 
00530              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00531 *----------------------------------------------------------------*GAS4UPD 
00532          10  WT-01-ENTRY-011.                                     GAS4UPD 
00533              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00534              15  WT-01-MESSAGE-TEXT-011.                          GAS4UPD 
00535                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00536                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00537                  20  FILLER          PIC X(3)  VALUE  '011'.      GAS4UPD 
00538                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00539                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00540                      'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT GAS4UPD 
00541 -                    '                         '.                 GAS4UPD 
00542              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00543 *----------------------------------------------------------------*GAS4UPD 
00544          10  WT-01-ENTRY-012.                                     GAS4UPD 
00545              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00546              15  WT-01-MESSAGE-TEXT-012.                          GAS4UPD 
00547                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00548                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00549                  20  FILLER          PIC X(3)  VALUE  '012'.      GAS4UPD 
00550                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00551                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00552                      'NO ENTRIES TO DISPLAY                       GAS4UPD 
00553 -                    '                         '.                 GAS4UPD 
00554              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00555 *----------------------------------------------------------------*GAS4UPD 
00556          10  WT-01-ENTRY-013.                                     GAS4UPD 
00557              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00558              15  WT-01-MESSAGE-TEXT-013.                          GAS4UPD 
00559                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00560                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00561                  20  FILLER          PIC X(3)  VALUE  '013'.      GAS4UPD 
00562                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00563                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00564                      'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HIGAS4UPD 
00565 -                    'T ENTER FOR ERR MSG      '.                 GAS4UPD 
00566              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00567 *----------------------------------------------------------------*GAS4UPD 
00568          10  WT-01-ENTRY-014.                                     GAS4UPD 
00569              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00570              15  WT-01-MESSAGE-TEXT-014.                          GAS4UPD 
00571                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00572                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00573                  20  FILLER          PIC X(3)  VALUE  '014'.      GAS4UPD 
00574                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00575                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00576                      'PROCESSING FROM THE TOP OF THE LIST         GAS4UPD 
00577 -                    '                         '.                 GAS4UPD 
00578              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00579 *----------------------------------------------------------------*GAS4UPD 
00580          10  WT-01-ENTRY-015.                                     GAS4UPD 
00581              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00582              15  WT-01-MESSAGE-TEXT-015.                          GAS4UPD 
00583                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00584                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00585                  20  FILLER          PIC X(3)  VALUE  '015'.      GAS4UPD 
00586                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00587                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00588                      'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDEGAS4UPD 
00589 -                    'D                        '.                 GAS4UPD 
00590              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00591 *----------------------------------------------------------------*GAS4UPD 
00592          10  WT-01-ENTRY-016.                                     GAS4UPD 
00593              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00594              15  WT-01-MESSAGE-TEXT-016.                          GAS4UPD 
00595                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00596                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00597                  20  FILLER          PIC X(3)  VALUE  '016'.      GAS4UPD 
00598                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00599                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00600                      'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUMGAS4UPD 
00601 -                    'BER OF OCCURANCES        '.                 GAS4UPD 
00602              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00603 *----------------------------------------------------------------*GAS4UPD 
00604          10  WT-01-ENTRY-017.                                     GAS4UPD 
00605              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00606              15  WT-01-MESSAGE-TEXT-017.                          GAS4UPD 
00607                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00608                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00609                  20  FILLER          PIC X(3)  VALUE  '017'.      GAS4UPD 
00610                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00611                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00612                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS4UPD 
00613 -                    'T BE CHANGED             '.                 GAS4UPD 
00614              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00615 *----------------------------------------------------------------*GAS4UPD 
00616          10  WT-01-ENTRY-018.                                     GAS4UPD 
00617              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00618              15  WT-01-MESSAGE-TEXT-018.                          GAS4UPD 
00619                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00620                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00621                  20  FILLER          PIC X(3)  VALUE  '018'.      GAS4UPD 
00622                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00623                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00624                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS4UPD 
00625 -                    'T BE MAPPED              '.                 GAS4UPD 
00626              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00627 *----------------------------------------------------------------*GAS4UPD 
00628          10  WT-01-ENTRY-019.                                     GAS4UPD 
00629              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00630              15  WT-01-MESSAGE-TEXT-019.                          GAS4UPD 
00631                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00632                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00633                  20  FILLER          PIC X(3)  VALUE  '019'.      GAS4UPD 
00634                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00635                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00636                      'THERE ARE NO MORE ENTRIES TO DISPLAY        GAS4UPD 
00637 -                    '                         '.                 GAS4UPD 
00638              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00639 *----------------------------------------------------------------*GAS4UPD 
00640          10  WT-01-ENTRY-020.                                     GAS4UPD 
00641              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00642              15  WT-01-MESSAGE-TEXT-020.                          GAS4UPD 
00643                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00644                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00645                  20  FILLER          PIC X(3)  VALUE  '020'.      GAS4UPD 
00646                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00647                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00648                      'THIS IS THE FIRST ON THE TABLE              GAS4UPD 
00649 -                    '                         '.                 GAS4UPD 
00650              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00651 *----------------------------------------------------------------*GAS4UPD 
00652          10  WT-01-ENTRY-021.                                     GAS4UPD 
00653              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00654              15  WT-01-MESSAGE-TEXT-021.                          GAS4UPD 
00655                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00656                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00657                  20  FILLER          PIC X(3)  VALUE  '021'.      GAS4UPD 
00658                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00659                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00660                      'THIS IS THE LAST ON THE TABLE               GAS4UPD 
00661 -                    '                         '.                 GAS4UPD 
00662              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00663 *----------------------------------------------------------------*GAS4UPD 
00664          10  WT-01-ENTRY-022.                                     GAS4UPD 
00665              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00666              15  WT-01-MESSAGE-TEXT-022.                          GAS4UPD 
00667                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00668                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00669                  20  FILLER          PIC X(3)  VALUE  '022'.      GAS4UPD 
00670                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00671                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00672                      'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  GAS4UPD 
00673 -                    '                         '.                 GAS4UPD 
00674              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00675 *----------------------------------------------------------------*GAS4UPD 
00676          10  WT-01-ENTRY-023.                                     GAS4UPD 
00677              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00678              15  WT-01-MESSAGE-TEXT-023.                          GAS4UPD 
00679                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00680                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00681                  20  FILLER          PIC X(3)  VALUE  '023'.      GAS4UPD 
00682                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00683                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00684                      '#IDGD HAS BEEN SUCCESSFULLY MAPPED          GAS4UPD 
00685 -                    '                         '.                 GAS4UPD 
00686              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00687 *----------------------------------------------------------------*GAS4UPD 
00688          10  WT-01-ENTRY-024.                                     GAS4UPD 
00689              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00690              15  WT-01-MESSAGE-TEXT-024.                          GAS4UPD 
00691                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00692                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00693                  20  FILLER          PIC X(3)  VALUE  '024'.      GAS4UPD 
00694                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00695                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00696                      '#IPGP HAS BEEN SUCCESSFULLY MAPPED          GAS4UPD 
00697 -                    '                         '.                 GAS4UPD 
00698              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00699 *----------------------------------------------------------------*GAS4UPD 
00700          10  WT-01-ENTRY-025.                                     GAS4UPD 
00701              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00702              15  WT-01-MESSAGE-TEXT-003.                          GAS4UPD 
00703                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00704                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00705                  20  FILLER          PIC X(3)  VALUE  '025'.      GAS4UPD 
00706                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00707                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00708                      '#IPGS HAS BEEN SUCCESSFULLY MAPPED          GAS4UPD 
00709 -                    '                         '.                 GAS4UPD 
00710              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00711 *----------------------------------------------------------------*GAS4UPD 
00712          10  WT-01-ENTRY-026.                                     GAS4UPD 
00713              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00714              15  WT-01-MESSAGE-TEXT-025.                          GAS4UPD 
00715                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00716                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00717                  20  FILLER          PIC X(3)  VALUE  '026'.      GAS4UPD 
00718                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00719                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00720                      'MAXIMUM OF 5 INTERNAL TABULARS HAS ALREADY BGAS4UPD 
00721 -                    'EEN REACHED              '.                 GAS4UPD 
00722              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00723 *----------------------------------------------------------------*GAS4UPD 
00724          10  WT-01-ENTRY-027.                                     GAS4UPD 
00725              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS4UPD 
00726              15  WT-01-MESSAGE-TEXT-027.                          GAS4UPD 
00727                  20  FILLER          PIC X(4)  VALUE  'GAS4'.     GAS4UPD 
00728                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS4UPD 
00729                  20  FILLER          PIC X(3)  VALUE  '027'.      GAS4UPD 
00730                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS4UPD 
00731                  20  FILLER          PIC X(70) VALUE              GAS4UPD 
00732                      '********** F U T U R E   U S E *************GAS4UPD 
00733 -                    '*************************'.                 GAS4UPD 
00734              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS4UPD 
00735 *----------------------------------------------------------------*GAS4UPD 
00736                                                                   GAS4UPD 
00737      05  WT-01-MESSAGE-TABLE         REDEFINES                    GAS4UPD 
00738          WT-01-MESSAGE-VALUES         OCCURS 027 TIMES            GAS4UPD 
00739                                      INDEXED BY WT-01-INDEX.      GAS4UPD 
00740          10  WT-01-ENTRY.                                         GAS4UPD 
00741              15  FILLER              PIC X(02).                   GAS4UPD 
00742              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GAS4UPD 
00743              15  FILLER              PIC X(02).                   GAS4UPD 
00744                                                                   GAS4UPD 
00745  01  WS-END                      PIC X(16)  VALUE                 GAS4UPD 
00746      '*** W/S ENDS ***'.                                          GAS4UPD 
00747 /    L I N K A G E   S E C T I O N                                GAS4UPD 
00748  LINKAGE SECTION.                                                 GAS4UPD 
00749  01  DFHCOMMAREA.                                                 GAS4UPD 
00750  COPY  G2ALCKEC.                                                  GAS4UPD 
00751  COPY  GACDACWA.                                                  GAS4UPD 
00752      05  GAS4UPD-PASSED-AREA.                                     GAS4UPD 
00753          07  LVL2-B-SW                PIC X.                      GAS4UPD 
00754          07  LVL2-F-SW                PIC X.                      GAS4UPD 
00755          07  LVL2-G-SW                PIC X.                      GAS4UPD 
00756          07  INTR-TAB-PGM-ID          PIC X(8).                   GAS4UPD 
00757          07  FILLER                   PIC X(09).                  GAS4UPD 
00758      05  DELADD-OPTION                PIC X(7).                   GAS4UPD 
00759                                                                   GAS4UPD 
00760 /*****************************************************************GAS4UPD 
00761 * W O R K F I L E   -   A L L   L E V E L   T A B U L A R   R E C GAS4UPD 
00762 ******************************************************************GAS4UPD 
00763  01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS4UPD 
00764  COPY GCIOPRM1.                                                   GAS4UPD 
00765 /                                                                 GAS4UPD 
00766  COPY GCWRKDCC.                                                   GAS4UPD 
00767 /                                                                 GAS4UPD 
00768  COPY GCTAOLC.                                                    GAS4UPD 
00769 /*****************************************************************GAS4UPD 
00770 *    C O M M U N I C A T I O N   K E Y   A R E A                  GAS4UPD 
00771 ******************************************************************GAS4UPD 
00772 *01  COMMUNICATION-KEY-AREA.                                      GAS4UPD 
00773 *COPY G2ALCKEC.                                                   GAS4UPD 
00774                                                                   GAS4UPD 
00775 /*****************************************************************GAS4UPD 
00776 *    C O P Y   T A B U L A R   T A B L E   A R E A                GAS4UPD 
00777 ******************************************************************GAS4UPD 
00778  01  COPY-TABULAR-TABLE-AREA.                                     GAS4UPD 
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          GAS4UPD 
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          GAS4UPD 
00780          COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               GAS4UPD 
00781        10  COPY-SORTABLE-FLDS              PIC X(169).            GAS4UPD 
00782        10  COPY-SORT-FYI                   PIC X(003).            GAS4UPD 
00783        10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      GAS4UPD 
00784                                                                   GAS4UPD 
00785 /*****************************************************************GAS4UPD 
00786 * W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   GAS4UPD 
00787 ******************************************************************GAS4UPD 
00788  01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              GAS4UPD 
00789  COPY GCIOPRM2.                                                   GAS4UPD 
00790 /                                                                 GAS4UPD 
00791  COPY GCWRKDC2.                                                   GAS4UPD 
00792 /                                                                 GAS4UPD 
00793  COPY GCTIPGPC.                                                   GAS4UPD 
00794 /*****************************************************************GAS4UPD 
00795 * P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     GAS4UPD 
00796 ******************************************************************GAS4UPD 
00797  01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS4UPD 
00798  COPY GCIOPRMA   SUPPRESS.                                        GAS4UPD 
00799                                                                   GAS4UPD 
00800  COPY GCWRKDCA   SUPPRESS.                                        GAS4UPD 
00801                                                                   GAS4UPD 
00802  COPY GCTAOL2    SUPPRESS.                                        GAS4UPD 
00803 /*****************************************************************GAS4UPD 
00804 *    M A P S E T   A R E A                                        GAS4UPD 
00805 ******************************************************************GAS4UPD 
00806      COPY GA1XSETC.                                               GAS4UPD 
00807 /    P R O C E D U R E   D I V I S I O N                          GAS4UPD 
00808  PROCEDURE DIVISION.                                              GAS4UPD 
00809                                                                   GAS4UPD 
00810 ******************************************************************GAS4UPD 
00811 * 0000  HOUSEKEEPING                                             *GAS4UPD 
00812 ******************************************************************GAS4UPD 
00813  0000-000-HOUSEKEEPING          SECTION.                          GAS4UPD 
00814  0000-010.                                                        GAS4UPD 
00815                                                                   GAS4UPD 
00816      SET ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD TO            GAS4UPD 
00817                      ACWA-WF-INTERNAL-TAB-PNTR.                   GAS4UPD 
00818                                                                   GAS4UPD 
00819      SET ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS4UPD 
00820                      ACWA-WF-ALL-LEVEL-TAB-PNTR.                  GAS4UPD 
00821                                                                   GAS4UPD 
00822      SET ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS4UPD 
00823                      ACWA-PR-ALL-LEVEL-TAB-PNTR.                  GAS4UPD 
00824                                                                   GAS4UPD 
00825      SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              GAS4UPD 
00826                                                                   GAS4UPD 
00827      MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      GAS4UPD 
00828                                                                   GAS4UPD 
00829                                                                   GAS4UPD 
00830 ***  MOVE GCA-FROM-MENU-ID  TO FRMNUIDO.                          GAS4UPD 
00831                                                                   GAS4UPD 
00832      IF FRMNUIDI  =  'GS3A'                                       GAS4UPD 
00833         MOVE IDLINEI  TO  GROUP-SPECIFIC-ID-LINE.                 GAS4UPD 
00834      IF FRMNUIDI  =  'GC4A' OR 'GTM1'                             GAS4UPD 
00835         MOVE IDLINEI  TO  CONTRACT-ID-LINE.                       GAS4UPD 
00836      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
00837         MOVE IDLINEI  TO  BENEFIT-PROVISION-ID-LINE.              GAS4UPD 
00838                                                                   GAS4UPD 
00839      MOVE AOL-TITLE-LINE   TO  TITLEO.                            GAS4UPD 
00840                                                                   GAS4UPD 
00841      PERFORM 1000-000-MAIN-PROCESS.                               GAS4UPD 
00842                                                                   GAS4UPD 
00843      EXEC CICS  RETURN    END-EXEC.                               GAS4UPD 
00844      GOBACK.                                                      GAS4UPD 
00845                                                                   GAS4UPD 
00846  0000-900-EXIT.                                                   GAS4UPD 
00847      EXIT.                                                        GAS4UPD 
00848 /*****************************************************************GAS4UPD 
00849 * 1000  MAIN PROCESS                                             *GAS4UPD 
00850 ******************************************************************GAS4UPD 
00851  1000-000-MAIN-PROCESS          SECTION.                          GAS4UPD 
00852  1000-010.                                                        GAS4UPD 
00853                                                                   GAS4UPD 
00854      EXEC CICS  HANDLE CONDITION                                  GAS4UPD 
00855                 MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)   END-EXEC.   GAS4UPD 
00856                                                                   GAS4UPD 
00857      MOVE  INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.      GAS4UPD 
00858                                                                   GAS4UPD 
00859      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS4UPD 
00860         DELADDI   =     'CHG/ADD'                     AND         GAS4UPD 
00861         OENTCTRI  NOT = '0000000'                                 GAS4UPD 
00862         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS4UPD 
00863                                                                   GAS4UPD 
00864      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS4UPD 
00865         DELADDI   =    'CHG/DEL'                      AND         GAS4UPD 
00866         DELOPTNI  =    'D'                                        GAS4UPD 
00867         PERFORM  2400-000-DELETE-THIS-OCCURANCE.                  GAS4UPD 
00868                                                                   GAS4UPD 
00869      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS4UPD 
00870         DELADDI   =     'CHG/DEL'                      AND        GAS4UPD 
00871         DELOPTNI  NOT = 'D'                                       GAS4UPD 
00872         MOVE SPACES  TO  ERRMSGO                                  GAS4UPD 
00873         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS4UPD 
00874                                                                   GAS4UPD 
00875      IF (EIBAID   =    DFHPF4 OR DFHPF7 OR DFHPF8 OR              GAS4UPD 
00876                        DFHPF19 OR DFHPF20 OR DFHPF16) AND         GAS4UPD 
00877         DELADDI   =    'CHG/DEL'                      AND         GAS4UPD 
00878         DELOPTNI  NOT = 'D'                                       GAS4UPD 
00879         MOVE SPACES  TO  ERRMSGO                                  GAS4UPD 
00880         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS4UPD 
00881                                                                   GAS4UPD 
00882      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS4UPD 
00883         DELADDI   =    'CHG/DEL'                      AND         GAS4UPD 
00884         DELOPTNI  NOT = 'D'                                       GAS4UPD 
00885         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS4UPD 
00886                                                                   GAS4UPD 
00887  1000-900-EXIT.   EXIT.                                           GAS4UPD 
00888                                                                   GAS4UPD 
00889 /*****************************************************************GAS4UPD 
00890 * 2200  UPDATE THIS OCCURANCE                                    *GAS4UPD 
00891 *                                                                *GAS4UPD 
00892 *    THIS ROUTINE WILL CHANGE ANY FIELD THAT THE OPERATOR HAS    *GAS4UPD 
00893 *  CHANGED, AND HAS CODE FOR THE MAINTENANCE OF THE INTERNAL     *GAS4UPD 
00894 *  TABULAR ENTRIES.                                              *GAS4UPD 
00895 ******************************************************************GAS4UPD 
00896  2200-000-UPDATE-THIS-OCCURANCE SECTION.                          GAS4UPD 
00897  2200-010.                                                        GAS4UPD 
00898                                                                   GAS4UPD 
00899      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS4UPD 
00900                                                                   GAS4UPD 
00901      IF NOT GCIO-GOOD-RETURN                                      GAS4UPD 
00902         MOVE WS-ABCODE-1EF4        TO WS-ABCODE                   GAS4UPD 
00903         MOVE WS-ABCODE-1EF4-MSG    TO WS-ABCODE-MSG               GAS4UPD 
00904         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
00905                                                                   GAS4UPD 
00906      MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   GAS4UPD 
00907      SET GAD-INDEX         TO  1.                                 GAS4UPD 
00908      MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                GAS4UPD 
00909                                                                   GAS4UPD 
00910  2200-210-FIND-RIGHT-OCCURS.                                      GAS4UPD 
00911                                                                   GAS4UPD 
00912      IF GAD-O-P-X-BENEFIT-PERIOD(GAD-INDEX)  NOT = HIGH-VALUES ANDGAS4UPD 
00913         GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  NOT =                GAS4UPD 
00914                                                 ACWA-DISPLAY-LEN-7GAS4UPD 
00915      THEN                                                         GAS4UPD 
00916          IF  GAD-INDEX  <  GAD-ENTRY-COUNT                        GAS4UPD 
00917          THEN                                                     GAS4UPD 
00918              SET GAD-INDEX  UP BY  1                              GAS4UPD 
00919              GO TO 2200-210-FIND-RIGHT-OCCURS                     GAS4UPD 
00920          ELSE                                                     GAS4UPD 
00921              MOVE WS-ABCODE-1EL1        TO WS-ABCODE              GAS4UPD 
00922              MOVE WS-ABCODE-1EL1-MSG    TO WS-ABCODE-MSG          GAS4UPD 
00923              MOVE -1                    TO  MFRMSLTL              GAS4UPD 
00924              PERFORM 9800-000-ERROR-MSG-THEN-ABEND                GAS4UPD 
00925      ELSE                                                         GAS4UPD 
00926          NEXT SENTENCE.                                           GAS4UPD 
00927                                                                   GAS4UPD 
00928      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS4UPD 
00929          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
00930          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
00931          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
00932          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
00933          (IPGSOPTI  =  'MT' OR 'A')                               GAS4UPD 
00934      THEN                                                         GAS4UPD 
00935          ADD 1 TO ACWA-FIELD-CHG-CNT.                             GAS4UPD 
00936                                                                   GAS4UPD 
00937                                                                   GAS4UPD 
00938      IF DAYFACII  NOT =    GAD-O-P-X-DAY-FACTOR-IND (GAD-INDEX)   GAS4UPD 
00939         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00940         MOVE DAYFACII  TO  GAD-O-P-X-DAY-FACTOR-IND (GAD-INDEX).  GAS4UPD 
00941                                                                   GAS4UPD 
00942      IF COPAYINI  NOT =    GAD-O-P-X-CO-PAY-IND (GAD-INDEX)       GAS4UPD 
00943         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00944         MOVE COPAYINI  TO  GAD-O-P-X-CO-PAY-IND (GAD-INDEX).      GAS4UPD 
00945                                                                   GAS4UPD 
00946      IF BISNDINI  NOT =    GAD-O-P-X-BISCENDING-IND (GAD-INDEX)   GAS4UPD 
00947         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00948         MOVE BISNDINI  TO  GAD-O-P-X-BISCENDING-IND (GAD-INDEX).  GAS4UPD 
00949                                                                   GAS4UPD 
00950      IF CARYOVRI  NOT =    GAD-CARRY-OVER-CREDIT-IND (GAD-INDEX)  GAS4UPD 
00951         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00952         MOVE CARYOVRI  TO  GAD-CARRY-OVER-CREDIT-IND (GAD-INDEX). GAS4UPD 
00953                                                                   GAS4UPD 
00954      IF ASCDSCDI  NOT =    GAD-O-P-X-ASCEND-DESCEND-IND(GAD-INDEX)GAS4UPD 
00955         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00956         MOVE ASCDSCDI TO  GAD-O-P-X-ASCEND-DESCEND-IND(GAD-INDEX).GAS4UPD 
00957                                                                   GAS4UPD 
      **P21595 CHANGES STARTS                                                   
00954      IF BENTYPI   NOT =    GAD-O-P-X-BEN-TYPE         (GAD-INDEX) GAS4UPD 
00955         ADD 1         TO  ACWA-FIELD-CHG-CNT                      GAS4UPD 
00956         MOVE BENTYPI  TO   GAD-O-P-X-BEN-TYPE         (GAD-INDEX).GAS4UPD 
00957                                                                   GAS4UPD 
00954      IF TIERCDI   NOT =    GAD-O-P-X-TIER-CODE        (GAD-INDEX) GAS4UPD 
00955         ADD 1         TO  ACWA-FIELD-CHG-CNT                      GAS4UPD 
00956         MOVE TIERCDI  TO   GAD-O-P-X-TIER-CODE        (GAD-INDEX).GAS4UPD 
                                                                                
00954      IF TIERLVI   NOT =    GAD-O-P-X-TIER-LVL         (GAD-INDEX) GAS4UPD 
00955         ADD 1         TO  ACWA-FIELD-CHG-CNT                      GAS4UPD 
00956         MOVE TIERLVI  TO   GAD-O-P-X-TIER-LVL         (GAD-INDEX).GAS4UPD 
      **P21595 CHANGES ENDS                                                     
                                                                                
00958      IF DEFINTNI  NOT =    GAD-O-P-X-DEFINITION (GAD-INDEX)       GAS4UPD 
00959         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00960         MOVE DEFINTNI  TO  GAD-O-P-X-DEFINITION (GAD-INDEX).      GAS4UPD 
00961                                                                   GAS4UPD 
00962      IF CSTCONTI  NOT =    GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX) GAS4UPD 
00963         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00964         MOVE CSTCONTI  TO  GAD-O-P-X-COST-CONTAIN-IND (GAD-INDEX).GAS4UPD 
00965                                                                   GAS4UPD 
00966      IF PERIODI  NOT =     GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX)   GAS4UPD 
00967         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00968         MOVE PERIODI   TO  GAD-O-P-X-BENEFIT-PERIOD (GAD-INDEX).  GAS4UPD 
00969                                                                   GAS4UPD 
00970      IF PERTQALI  NOT =    GAD-O-P-X-BEN-PER-TIME-QUAL(GAD-INDEX) GAS4UPD 
00971         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00972         MOVE PERTQALI  TO  GAD-O-P-X-BEN-PER-TIME-QUAL(GAD-INDEX).GAS4UPD 
00973                                                                   GAS4UPD 
00974      IF FAMINDII  NOT =    GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX)     GAS4UPD 
00975         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00976         MOVE FAMINDII  TO  GAD-O-P-X-FAM-OR-INDIV (GAD-INDEX).    GAS4UPD 
00977                                                                   GAS4UPD 
00978      IF PLCTRMTI  NOT =    GAD-O-P-X-PLACE-OF-TREATMENT(GAD-INDEX)GAS4UPD 
00979         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00980         MOVE PLCTRMTI TO  GAD-O-P-X-PLACE-OF-TREATMENT(GAD-INDEX).GAS4UPD 
00981                                                                   GAS4UPD 
00982      IF SRVGRUPI  NOT =    GAD-O-P-X-SERVICE-GROUP (GAD-INDEX)    GAS4UPD 
00983         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
00984         MOVE SRVGRUPI  TO  GAD-O-P-X-SERVICE-GROUP (GAD-INDEX).   GAS4UPD 
00985                                                                   GAS4UPD 
00986      MOVE AGELIMLI   TO ACWA-DISPLAY-LEN-3-X.                     GAS4UPD 
00987      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS4UPD 
00988                           GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX)    GAS4UPD 
00989         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS4UPD 
00990        MOVE ACWA-DISPLAY-LEN-3                                    GAS4UPD 
00991                        TO GAD-O-P-X-AGE-LIMIT-FROM (GAD-INDEX).   GAS4UPD 
00992                                                                   GAS4UPD 
00993      MOVE AGELIMHI   TO ACWA-DISPLAY-LEN-3-X.                     GAS4UPD 
00994      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS4UPD 
00995                           GAD-O-P-X-AGE-LIMIT-TO   (GAD-INDEX)    GAS4UPD 
00996         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS4UPD 
00997        MOVE ACWA-DISPLAY-LEN-3                                    GAS4UPD 
00998                        TO GAD-O-P-X-AGE-LIMIT-TO   (GAD-INDEX).   GAS4UPD 
00999                                                                   GAS4UPD 
01000      IF FEAKINDI  NOT =  GAD-O-P-X-FEAK-IND         (GAD-INDEX)   GAS4UPD 
01001         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01002         MOVE FEAKINDI TO GAD-O-P-X-FEAK-IND         (GAD-INDEX).  GAS4UPD 
01003                                                                   GAS4UPD 
01004      IF ACCUMIDI  NOT =  GAD-O-P-X-ACCUMID          (GAD-INDEX)   GAS4UPD 
01005         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01006         MOVE ACCUMIDI TO GAD-O-P-X-ACCUMID          (GAD-INDEX).  GAS4UPD 
01007                                                                   GAS4UPD 
01008      IF CAPINDI   NOT =  GAD-O-P-X-COMB-APPLIED-IND (GAD-INDEX)   GAS4UPD 
01009         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01010         MOVE CAPINDI  TO GAD-O-P-X-COMB-APPLIED-IND (GAD-INDEX).  GAS4UPD 
01011                                                                   GAS4UPD 
01012      IF SABDINDI  NOT =  GAD-O-P-X-SEL-ADDL-BEN-DET (GAD-INDEX)   GAS4UPD 
01013         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01014         MOVE SABDINDI TO GAD-O-P-X-SEL-ADDL-BEN-DET (GAD-INDEX).  GAS4UPD 
01015                                                                   GAS4UPD 
01016      IF AGEQLLI   NOT =  GAD-O-P-X-AGE-QUAL-IND-FROM(GAD-INDEX)   GAS4UPD 
01017         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01018         MOVE AGEQLLI  TO                                          GAS4UPD 
01019                         GAD-O-P-X-AGE-QUAL-IND-FROM(GAD-INDEX).   GAS4UPD 
01020                                                                   GAS4UPD 
01021      IF AGEQLHI   NOT =  GAD-O-P-X-AGE-QUAL-IND-TO  (GAD-INDEX)   GAS4UPD 
01022         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01023         MOVE AGEQLHI  TO                                          GAS4UPD 
01024                         GAD-O-P-X-AGE-QUAL-IND-TO  (GAD-INDEX).   GAS4UPD 
01025                                                                   GAS4UPD 
01026      IF RELPINDI  NOT =  GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX)   GAS4UPD 
01027         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS4UPD 
01028         MOVE RELPINDI TO                                          GAS4UPD 
01029                         GAD-O-P-X-RELATIONSHIP-IND (GAD-INDEX).   GAS4UPD 
01030                                                                   GAS4UPD 
01031                                                                   GAS4UPD 
01032        MOVE PRTIMEFI   TO  ACWA-DISPLAY-LEN-3-X.                  GAS4UPD 
01033        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS4UPD 
01034                            GAD-O-P-X-BEN-PER-TIME-FCTR (GAD-INDEX)GAS4UPD 
01035         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01036        MOVE ACWA-DISPLAY-LEN-3                                    GAS4UPD 
01037                        TO  GAD-O-P-X-BEN-PER-TIME-FCTR(GAD-INDEX).GAS4UPD 
01038                                                                   GAS4UPD 
01039      IF CLMLVLII NOT =    GAD-O-P-X-CLAIM-LVL-ACCUM-IND(GAD-INDEX)GAS4UPD 
01040         ADD 1         TO  ACWA-FIELD-CHG-CNT                      GAS4UPD 
01041         MOVE CLMLVLII TO GAD-O-P-X-CLAIM-LVL-ACCUM-IND(GAD-INDEX).GAS4UPD 
01042                                                                   GAS4UPD 
01043      MOVE INTRVALI  TO     ACWA-DISPLAY-LEN-3-X.                  GAS4UPD 
01044      IF ACWA-DISPLAY-LEN-3  NOT =                                 GAS4UPD 
01045                            GAD-O-P-X-INTERVAL-TIME-FCTR(GAD-INDEX)GAS4UPD 
01046         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01047         MOVE ACWA-DISPLAY-LEN-3  TO                               GAS4UPD 
01048                           GAD-O-P-X-INTERVAL-TIME-FCTR(GAD-INDEX).GAS4UPD 
01049                                                                   GAS4UPD 
01050      IF INTTYPEI  NOT =    GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX)    GAS4UPD 
01051         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01052         MOVE INTTYPEI  TO  GAD-O-P-X-INTERVAL-TYPE (GAD-INDEX).   GAS4UPD 
01053                                                                   GAS4UPD 
01054      IF LOBI   NOT =       GAD-O-P-X-L-O-B  (GAD-INDEX)           GAS4UPD 
01055         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01056         MOVE LOBI      TO  GAD-O-P-X-L-O-B (GAD-INDEX).           GAS4UPD 
01057                                                                   GAS4UPD 
01058      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS4UPD 
01059      THEN                                                         GAS4UPD 
01060          MOVE ACWA-VAL-LIM-SCREEN  TO  ACWA-VALUE-LIMIT-9-9       GAS4UPD 
01061      ELSE                                                         GAS4UPD 
01062          IF  ACWA-VAL-LIM-SCREEN-NEG1-3  =  'NEG' OR              GAS4UPD 
01063              ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG'                 GAS4UPD 
01064          THEN                                                     GAS4UPD 
01065              MOVE -1                  TO  ACWA-VALUE-LIMIT-9-9    GAS4UPD 
01066          ELSE                                                     GAS4UPD 
01062          IF  ACWA-VAL-LIM-SCREEN-NEG1-3  =  'UNL' OR              GAS4UPD 
01063              ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL'                 GAS4UPD 
01064          THEN                                                     GAS4UPD 
01065              MOVE -2                  TO  ACWA-VALUE-LIMIT-9-9    GAS4UPD 
01066          ELSE                                                     GAS4UPD 
01067              MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7   GAS4UPD 
01068              MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2.  GAS4UPD 
01069                                                                   GAS4UPD 
01070      IF ACWA-VALUE-LIMIT-9  NOT = GAD-O-P-X-VALUE-LIMIT(GAD-INDEX)GAS4UPD 
01071         PERFORM 2600-000-PROCESS-VAL-LIMIT.                       GAS4UPD 
01072                                                                   GAS4UPD 
01073      IF ACWA-VALUE-LIMIT-9  NOT = GAD-O-P-X-VALUE-LIMIT(GAD-INDEX)GAS4UPD 
01074         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01075         MOVE ACWA-VALUE-LIMIT-9  TO                               GAS4UPD 
01076                            GAD-O-P-X-VALUE-LIMIT(GAD-INDEX).      GAS4UPD 
01077                                                                   GAS4UPD 
01078      IF BENVLQLI  NOT =    GAD-O-P-X-VALUE-QUALIFIER(GAD-INDEX)   GAS4UPD 
01079         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01080         MOVE BENVLQLI  TO  GAD-O-P-X-VALUE-QUALIFIER(GAD-INDEX).  GAS4UPD 
01081                                                                   GAS4UPD 
01082      MOVE PERLIMTI     TO  ACWA-DISPLAY-LEN-3-X.                  GAS4UPD 
01083      IF ACWA-DISPLAY-LEN-3  NOT =                                 GAS4UPD 
01084                            GAD-O-P-X-PERCENT-LEVEL(GAD-INDEX)     GAS4UPD 
01085         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01086         MOVE ACWA-DISPLAY-LEN-3  TO                               GAS4UPD 
01087                            GAD-O-P-X-PERCENT-LEVEL (GAD-INDEX).   GAS4UPD 
01088                                                                   GAS4UPD 
01089      MOVE NEWVALUI     TO  ACWA-DISPLAY-LEN-5-X.                  GAS4UPD 
01090      IF  ACWA-DISPLAY-LEN-5  NOT =                                GAS4UPD 
01091                           GAD-O-P-X-INTERVAL-OVRD-VALUE(GAD-INDEX)GAS4UPD 
01092      THEN                                                         GAS4UPD 
01093          ADD 1     TO ACWA-FIELD-CHG-CNT                          GAS4UPD 
01094          MOVE ACWA-DISPLAY-LEN-5                                  GAS4UPD 
01095                    TO GAD-O-P-X-INTERVAL-OVRD-VALUE (GAD-INDEX).  GAS4UPD 
01096                                                                   GAS4UPD 
01097      IF OVRDINDI  NOT =    GAD-O-P-X-INTERVAL-OVRD-IND(GAD-INDEX) GAS4UPD 
01098         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01099         MOVE OVRDINDI  TO  GAD-O-P-X-INTERVAL-OVRD-IND(GAD-INDEX).GAS4UPD 
01100                                                                   GAS4UPD 
01101      IF FYIVALI   NOT =    GAD-O-P-X-FYI-VALUE (GAD-INDEX)        GAS4UPD 
01102         ADD 1         TO   ACWA-FIELD-CHG-CNT                     GAS4UPD 
01103         MOVE FYIVALI  TO   GAD-O-P-X-FYI-VALUE (GAD-INDEX).       GAS4UPD 
01104                                                                   GAS4UPD 
01105      IF INTDESKI    =     GAD-O-P-X-INTERNAL-DESCRIPTOR(GAD-INDEX)GAS4UPD 
01106         MOVE SPACE   TO   WS-INT-TAB-CHANGE-INDICATOR             GAS4UPD 
01107      ELSE                                                         GAS4UPD 
01108         ADD 1     TO      ACWA-FIELD-CHG-CNT                      GAS4UPD 
01109         IF INTDESKI  =  IDPRODI                                   GAS4UPD 
01110            MOVE 'NP'   TO  WS-INT-TAB-CHANGE-INDICATOR            GAS4UPD 
01111            MOVE INTDESKI  TO                                      GAS4UPD 
01112                           GAD-O-P-X-INTERNAL-DESCRIPTOR(GAD-INDEX)GAS4UPD 
01113         ELSE                                                      GAS4UPD 
01114            IF IDPRODI  =  GAD-O-P-X-INTERNAL-DESCRIPTOR(GAD-INDEX)GAS4UPD 
01115               MOVE 'PN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS4UPD 
01116               MOVE INTDESKI  TO                                   GAS4UPD 
01117                           GAD-O-P-X-INTERNAL-DESCRIPTOR(GAD-INDEX)GAS4UPD 
01118            ELSE                                                   GAS4UPD 
01119               MOVE 'NN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS4UPD 
01120               MOVE INTDESKI  TO                                   GAS4UPD 
01121                          GAD-O-P-X-INTERNAL-DESCRIPTOR(GAD-INDEX).GAS4UPD 
01122                                                                   GAS4UPD 
01123      IF CONDALLI  NOT =    GAD-COND-ALL-BIT (GAD-INDEX)           GAS4UPD 
01124         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01125         MOVE CONDALLI  TO  GAD-COND-ALL-BIT (GAD-INDEX).          GAS4UPD 
01126                                                                   GAS4UPD 
01127      IF CONDEXCI  NOT =    GAD-COND-EXCLUSION-BIT (GAD-INDEX)     GAS4UPD 
01128         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01129         MOVE CONDEXCI  TO  GAD-COND-EXCLUSION-BIT (GAD-INDEX).    GAS4UPD 
01130                                                                   GAS4UPD 
01131      IF CONDICDI  NOT =    GAD-COND-ICD-BIT (GAD-INDEX)           GAS4UPD 
01132         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01133         MOVE CONDICDI  TO  GAD-COND-ICD-BIT (GAD-INDEX).          GAS4UPD 
01134                                                                   GAS4UPD 
01135      IF CONDTABI  NOT =    GAD-COND-TB-BIT (GAD-INDEX)            GAS4UPD 
01136         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01137         MOVE CONDTABI  TO  GAD-COND-TB-BIT (GAD-INDEX).           GAS4UPD 
01138                                                                   GAS4UPD 
01139      IF CONDMENI  NOT =    GAD-COND-MENTAL-BIT (GAD-INDEX)        GAS4UPD 
01140         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01141         MOVE CONDMENI  TO  GAD-COND-MENTAL-BIT (GAD-INDEX).       GAS4UPD 
01142                                                                   GAS4UPD 
01143      IF CONDDRGI  NOT =    GAD-COND-DRUG-BIT (GAD-INDEX)          GAS4UPD 
01144         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01145         MOVE CONDDRGI  TO  GAD-COND-DRUG-BIT (GAD-INDEX).         GAS4UPD 
01146                                                                   GAS4UPD 
01147      IF CONDALCI  NOT =    GAD-COND-ALCOHOL-BIT (GAD-INDEX)       GAS4UPD 
01148         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01149         MOVE CONDALCI  TO  GAD-COND-ALCOHOL-BIT (GAD-INDEX).      GAS4UPD 
01150                                                                   GAS4UPD 
01151      IF CONDOBCI  NOT =    GAD-COND-OB-COMP-BIT (GAD-INDEX)       GAS4UPD 
01152         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01153         MOVE CONDOBCI  TO  GAD-COND-OB-COMP-BIT (GAD-INDEX).      GAS4UPD 
01154                                                                   GAS4UPD 
01155      IF CONDOBNI  NOT =    GAD-COND-OB-NORM-BIT (GAD-INDEX)       GAS4UPD 
01156         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01157         MOVE CONDOBNI  TO  GAD-COND-OB-NORM-BIT (GAD-INDEX).      GAS4UPD 
01158                                                                   GAS4UPD 
01159      IF CONDMALI  NOT =    GAD-COND-MALIGNANCY-BIT (GAD-INDEX)    GAS4UPD 
01160         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01161         MOVE CONDMALI  TO  GAD-COND-MALIGNANCY-BIT (GAD-INDEX).   GAS4UPD 
01162                                                                   GAS4UPD 
01163      IF CONDCARI  NOT =    GAD-COND-CARDIAC-DISEASE-BIT(GAD-INDEX)GAS4UPD 
01164         ADD 1         TO   ACWA-FIELD-CHG-CNT                     GAS4UPD 
01165        MOVE CONDCARI  TO  GAD-COND-CARDIAC-DISEASE-BIT(GAD-INDEX).GAS4UPD 
01166                                                                   GAS4UPD 
01167      IF CONDOBSI  NOT =    GAD-COND-OBESITY-BIT (GAD-INDEX)       GAS4UPD 
01168         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01169         MOVE CONDOBSI  TO  GAD-COND-OBESITY-BIT (GAD-INDEX).      GAS4UPD 
01170                                                                   GAS4UPD 
01171      IF CONDKDYI  NOT =    GAD-COND-KIDNEY-DISEASE-BIT(GAD-INDEX) GAS4UPD 
01172         ADD 1          TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01173         MOVE CONDKDYI  TO  GAD-COND-KIDNEY-DISEASE-BIT(GAD-INDEX).GAS4UPD 
01174                                                                   GAS4UPD 
01175      IF CONDACCI  NOT  =   GAD-COND-ACCIDENT-BIT (GAD-INDEX)      GAS4UPD 
01176         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01177         MOVE CONDACCI  TO  GAD-COND-ACCIDENT-BIT (GAD-INDEX).     GAS4UPD 
01178                                                                   GAS4UPD 
01179      IF CONDPECI  NOT  =   GAD-COND-PRE-EXIST-BIT (GAD-INDEX)     GAS4UPD 
01180         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01181         MOVE CONDPECI  TO  GAD-COND-PRE-EXIST-BIT (GAD-INDEX).    GAS4UPD 
01182                                                                   GAS4UPD 
01183      IF CONDNEMI  NOT  =   GAD-COND-NON-EMER-BIT (GAD-INDEX)      GAS4UPD 
01184         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01185         MOVE CONDNEMI  TO  GAD-COND-NON-EMER-BIT (GAD-INDEX).     GAS4UPD 
01186                                                                   GAS4UPD 
01187      IF CONDSUII  NOT  =   GAD-COND-SUICIDE-BIT  (GAD-INDEX)      GAS4UPD 
01188         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01189         MOVE CONDSUII  TO  GAD-COND-SUICIDE-BIT  (GAD-INDEX).     GAS4UPD 
01190                                                                   GAS4UPD 
01191      IF CONDTMJI  NOT  =   GAD-COND-TMJ-BIT      (GAD-INDEX)      GAS4UPD 
01192         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01193         MOVE CONDTMJI  TO  GAD-COND-TMJ-BIT      (GAD-INDEX).     GAS4UPD 
01194                                                                   GAS4UPD 
01195      IF CONDINFI  NOT  =   GAD-COND-INF-BIT      (GAD-INDEX)      GAS4UPD 
01196         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01197         MOVE CONDINFI  TO  GAD-COND-INF-BIT      (GAD-INDEX).     GAS4UPD 
01198                                                                   GAS4UPD 
01199      IF CONDLIFI  NOT  =   GAD-COND-LIFE-THREAT-BIT (GAD-INDEX)   GAS4UPD 
01200         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01201         MOVE CONDLIFI  TO  GAD-COND-LIFE-THREAT-BIT (GAD-INDEX).  GAS4UPD 
01202                                                                   GAS4UPD 
01203      IF CONDEMCI  NOT  =  GAD-COND-EMER-MED-BIT     (GAD-INDEX)   GAS4UPD 
01204         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01205         MOVE CONDEMCI  TO GAD-COND-EMER-MED-BIT     (GAD-INDEX).  GAS4UPD 
01206                                                                   GAS4UPD 
01207      IF CONDEACI  NOT  =  GAD-COND-EMER-ACC-BIT     (GAD-INDEX)   GAS4UPD 
01208         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01209         MOVE CONDEACI  TO GAD-COND-EMER-ACC-BIT     (GAD-INDEX).  GAS4UPD 
01210                                                                   GAS4UPD 
01211      IF CONDSMII  NOT  = GAD-COND-SER-MEN-ILL-BIT   (GAD-INDEX)   GAS4UPD 
01212         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01213         MOVE CONDSMII  TO GAD-COND-SER-MEN-ILL-BIT  (GAD-INDEX).  GAS4UPD 
01214                                                                   GAS4UPD 
01215      IF CONDNSMI  NOT  = GAD-COND-NON-SER-MEN-ILL-BIT (GAD-INDEX) GAS4UPD 
01216         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS4UPD 
01217         MOVE CONDNSMI  TO GAD-COND-NON-SER-MEN-ILL-BIT (GAD-INDEX)GAS4UPD 
01218                                                                   GAS4UPD 
01219      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
01220         MOVE GCIO-WRK-TABULAR-PROVISION  TO                       GAS4UPD 
01221                                     GCIO-WRK-BENEFIT-PROVISION.   GAS4UPD 
01222                                                                   GAS4UPD 
01223 ******* IF THE OCCUR IS A NEW ADDED ONE THEN IT IS FLAGED 1U      GAS4UPD 
01224 *** IN GA1BPGM ALL ATTACHED INTERNAL TABS TO THIS ADDED OCCUR     GAS4UPD 
01225 *** ALSO WILL BE FLAGED 1U IN THIS ROUTINE    NE 08/03/88         GAS4UPD 
01226                                                                   GAS4UPD 
01227      PERFORM  5000-000-READ-PROD-ALL-LVL-TAB.                     GAS4UPD 
01228         SEARCH GAD2-ENTRY                                         GAS4UPD 
01229            VARYING GAD2-INDEX                                     GAS4UPD 
01230            WHEN                                                   GAS4UPD 
01231               GAD2-INDEX NOT <  GAD2-ENTRY-COUNT  OR              GAS4UPD 
01232               GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)  =            GAS4UPD 
01233                              GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  GAS4UPD 
01234               NEXT SENTENCE.                                      GAS4UPD 
01235                                                                   GAS4UPD 
01236         IF GAD2-INDEX <  GAD2-ENTRY-COUNT  AND                    GAS4UPD 
01237            GAD2-OCCURS-ENTRY-COUNTER(GAD2-INDEX)  =               GAS4UPD 
01238                              GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  GAS4UPD 
01239            MOVE 'N'      TO  WS-NEW-OCCR-ON-WF                    GAS4UPD 
01240         ELSE                                                      GAS4UPD 
01241            MOVE 'Y'      TO WS-NEW-OCCR-ON-WF.                    GAS4UPD 
01242 ******************************************************** 8/3/88   GAS4UPD 
01243      IF ACWA-INTERNAL-TAB-CHANGE-ONLY                             GAS4UPD 
01244         GO TO 2200-260-CHANGE-INTERNAL-TAB.                       GAS4UPD 
01245                                                                   GAS4UPD 
01246 *** CHECK LVL2-B-SWITCH                                           GAS4UPD 
01247      IF EIBAID    =       DFHENTER  AND                           GAS4UPD 
01248         DELADDI   =      'CHG/ADD'  AND                           GAS4UPD 
01249         OENTCTRI  NOT =  '0000000'  AND                           GAS4UPD 
01250         ACWA-NO-CHANGE-FOUND                                      GAS4UPD 
01251         MOVE 'Y'   TO  LVL2-B-SW                                  GAS4UPD 
01252         PERFORM 3100-RLSE-RU-GAD-REC                              GAS4UPD 
01253         GO  TO  2200-900-EXIT.                                    GAS4UPD 
01254                                                                   GAS4UPD 
01255      IF  EIBAID  =  DFHENTER       AND                            GAS4UPD 
01256          ACWA-SCREEN-HAS-NO-ERRORS AND                            GAS4UPD 
01257          GCVI-TABLE-SW = 'N'                                      GAS4UPD 
01258      THEN                                                         GAS4UPD 
01259          IF  ACWA-NO-CHANGE-FOUND                                 GAS4UPD 
01260          THEN                                                     GAS4UPD 
01261              SET  WT-01-INDEX                     TO +11          GAS4UPD 
01262              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS4UPD 
01263              PERFORM 7900-000-RESET-ATTRIBUTES                    GAS4UPD 
01264              MOVE -1 TO PERIODL                                   GAS4UPD 
01265              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS4UPD 
01266          ELSE                                                     GAS4UPD 
01267              SET  WT-01-INDEX                     TO +07          GAS4UPD 
01268              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS4UPD 
01269              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS4UPD 
01270      ELSE                                                         GAS4UPD 
01271          NEXT SENTENCE.                                           GAS4UPD 
01272                                                                   GAS4UPD 
01273      IF (EIBAID  =  DFHPF4 OR  DFHPF16) AND                       GAS4UPD 
01274          ACWA-SCREEN-HAS-NO-ERRORS      AND                       GAS4UPD 
01275          GCVI-TABLE-SW = 'N'            AND                       GAS4UPD 
01276          ACWA-NO-CHANGE-FOUND                                     GAS4UPD 
01277      THEN                                                         GAS4UPD 
01278          SET  WT-01-INDEX                     TO +09              GAS4UPD 
01279          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS4UPD 
01280          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS4UPD 
01281          MOVE -1 TO PERIODL                                       GAS4UPD 
01282          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS4UPD 
01283                                                                   GAS4UPD 
01284      IF  EIBAID   =   DFHENTER AND                                GAS4UPD 
01285          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS4UPD 
01286          ACWA-NO-CHANGE-FOUND                                     GAS4UPD 
01287      THEN                                                         GAS4UPD 
01288          SET  WT-01-INDEX                     TO +11              GAS4UPD 
01289          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS4UPD 
01290          MOVE -1 TO PERIODL                                       GAS4UPD 
01291          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS4UPD 
01292                                                                   GAS4UPD 
01293      PERFORM 7900-000-RESET-ATTRIBUTES.                           GAS4UPD 
01294                                                                   GAS4UPD 
01295 *** LVL2-F-SWITCH                                                 GAS4UPD 
01296      IF  (EIBAID  =   DFHPF7 OR DFHPF19) AND                      GAS4UPD 
01297          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS4UPD 
01298          ACWA-NO-CHANGE-FOUND                                     GAS4UPD 
01299      THEN                                                         GAS4UPD 
01300          MOVE  'Y'   TO  LVL2-F-SW                                GAS4UPD 
01301         PERFORM 3100-RLSE-RU-GAD-REC                              GAS4UPD 
01302          GO TO  2200-900-EXIT.                                    GAS4UPD 
01303                                                                   GAS4UPD 
01304 *** LVL2-G-SWITCH                                                 GAS4UPD 
01305      IF  (EIBAID  =  DFHPF8 OR DFHPF20) AND                       GAS4UPD 
01306          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS4UPD 
01307          ACWA-NO-CHANGE-FOUND                                     GAS4UPD 
01308      THEN                                                         GAS4UPD 
01309          MOVE  'Y'   TO  LVL2-G-SW                                GAS4UPD 
01310         PERFORM 3100-RLSE-RU-GAD-REC                              GAS4UPD 
01311          GO TO  2200-900-EXIT.                                    GAS4UPD 
01312                                                                   GAS4UPD 
01313      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS4UPD 
01314         DELADDI  =  'CHG/DEL'                                     GAS4UPD 
01315         PERFORM 4600-000-UPDATE-CDE-STATUS.                       GAS4UPD 
01316                                                                   GAS4UPD 
01317      IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE OR             GAS4UPD 
01318          (IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999')) AND      GAS4UPD 
01319          (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE OR             GAS4UPD 
01320          (IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999')) AND      GAS4UPD 
01321          (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE OR             GAS4UPD 
01322          (IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999')) AND      GAS4UPD 
01323          (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE OR             GAS4UPD 
01324          (IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999')) AND      GAS4UPD 
01325          (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE OR             GAS4UPD 
01326          (IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999')) AND      GAS4UPD 
01327          (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE OR             GAS4UPD 
01328          (IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'))          GAS4UPD 
01329      THEN                                                         GAS4UPD 
01330          GO TO 2200-250-UPDATE-ALL-LVL-TAB.                       GAS4UPD 
01331                                                                   GAS4UPD 
01332      IF  IBGROPTI  =  'C' OR                                      GAS4UPD 
01333          IDGDOPTI  =  'C' OR                                      GAS4UPD 
01334          IPGNOPTI  =  'C' OR                                      GAS4UPD 
01335          IPGPOPTI  =  'C' OR                                      GAS4UPD 
01336          IPGTOPTI  =  'C' OR                                      GAS4UPD 
01337          IPGSOPTI  =  'C'                                         GAS4UPD 
01338      THEN                                                         GAS4UPD 
01339          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS4UPD 
01340                                                                   GAS4UPD 
01341      IF  (IBGROPTI  =  'MT' OR 'A') AND IBGRSLTI  =  '0000000' OR GAS4UPD 
01342          (IDGDOPTI  =  'MT' OR 'A') AND IDGDSLTI  =  '0000000' OR GAS4UPD 
01343          (IPGNOPTI  =  'MT' OR 'A') AND IPGNSLTI  =  '0000000' OR GAS4UPD 
01344          (IPGPOPTI  =  'MT' OR 'A') AND IPGPSLTI  =  '0000000' OR GAS4UPD 
01345          (IPGTOPTI  =  'MT' OR 'A') AND IPGTSLTI  =  '0000000' OR GAS4UPD 
01346          (IPGSOPTI  =  'MT' OR 'A') AND IPGSSLTI  =  '0000000'    GAS4UPD 
01347      THEN                                                         GAS4UPD 
01348          GO TO 2200-220-ADD-INTERNAL-OCCURS.                      GAS4UPD 
01349                                                                   GAS4UPD 
01350      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS4UPD 
01351          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
01352          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
01353          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
01354          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS4UPD 
01355          (IPGSOPTI  =  'MT' OR 'A')                               GAS4UPD 
01356      THEN                                                         GAS4UPD 
01357          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS4UPD 
01358                                                                   GAS4UPD 
01359      IF  IBGROPTI  =  'D'                                         GAS4UPD 
01360      THEN                                                         GAS4UPD 
01361          MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID             GAS4UPD 
01362          MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO             GAS4UPD 
01363      ELSE                                                         GAS4UPD 
01364          IF  IPGNOPTI  =  'D'                                     GAS4UPD 
01365          THEN                                                     GAS4UPD 
01366              MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID         GAS4UPD 
01367              MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO         GAS4UPD 
01368          ELSE                                                     GAS4UPD 
01369              IF  IPGTOPTI  =  'D'                                 GAS4UPD 
01370              THEN                                                 GAS4UPD 
01371                  MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS4UPD 
01372                  MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS4UPD 
01373              ELSE                                                 GAS4UPD 
01374              IF  IPGSOPTI  =  'D'                                 GAS4UPD 
01375              THEN                                                 GAS4UPD 
01376                  MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS4UPD 
01377                  MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS4UPD 
01378              ELSE                                                 GAS4UPD 
01379              IF  IDGDOPTI  =  'D'                                 GAS4UPD 
01380              THEN                                                 GAS4UPD 
01381                  MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS4UPD 
01382                  MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS4UPD 
01383              ELSE                                                 GAS4UPD 
01384              IF  IPGPOPTI  =  'D'                                 GAS4UPD 
01385              THEN                                                 GAS4UPD 
01386                  MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS4UPD 
01387                  MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS4UPD 
01388              ELSE                                                 GAS4UPD 
01389                  NEXT SENTENCE.                                   GAS4UPD 
01390                                                                   GAS4UPD 
01391                                                                   GAS4UPD 
01392      IF    GAD-O-P-X-INTL-TAB-1 (GAD-INDEX)                       GAS4UPD 
01393          = GCIO-WRK-TABULAR-PROVISION                             GAS4UPD 
01394      THEN                                                         GAS4UPD 
01395          MOVE GAD-O-P-X-INTL-TAB-2 (GAD-INDEX)                    GAS4UPD 
01396            TO GAD-O-P-X-INTL-TAB-1 (GAD-INDEX)                    GAS4UPD 
01397          MOVE GAD-O-P-X-INTL-TAB-3 (GAD-INDEX)                    GAS4UPD 
01398            TO GAD-O-P-X-INTL-TAB-2 (GAD-INDEX)                    GAS4UPD 
01399          MOVE GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)                    GAS4UPD 
01400            TO GAD-O-P-X-INTL-TAB-3 (GAD-INDEX)                    GAS4UPD 
01401          MOVE GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)                    GAS4UPD 
01402            TO GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)                    GAS4UPD 
01403 ******** MOVE HIGH-VALUES                                         GAS4UPD 
01404 ********   TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)                    GAS4UPD 
01405          IF GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)                      GAS4UPD 
01406                       = HIGH-VALUES OR WS-SPACES-ZEROS            GAS4UPD 
01407             MOVE WS-SPACES-ZEROS                                  GAS4UPD 
01408               TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)                 GAS4UPD 
01409          ELSE                                                     GAS4UPD 
01410             MOVE HIGH-VALUES                                      GAS4UPD 
01411               TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)                 GAS4UPD 
01412          END-IF                                                   GAS4UPD 
01413      ELSE                                                         GAS4UPD 
01414          IF   GAD-O-P-X-INTL-TAB-2 (GAD-INDEX)                    GAS4UPD 
01415             = GCIO-WRK-TABULAR-PROVISION                          GAS4UPD 
01416         THEN                                                      GAS4UPD 
01417             MOVE GAD-O-P-X-INTL-TAB-3 (GAD-INDEX)                 GAS4UPD 
01418               TO GAD-O-P-X-INTL-TAB-2 (GAD-INDEX)                 GAS4UPD 
01419             MOVE GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)                 GAS4UPD 
01420               TO GAD-O-P-X-INTL-TAB-3 (GAD-INDEX)                 GAS4UPD 
01421             MOVE GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)                 GAS4UPD 
01422               TO GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)                 GAS4UPD 
01423 *********** MOVE HIGH-VALUES                                      GAS4UPD 
01424 ***********   TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)                 GAS4UPD 
01425             IF GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)                   GAS4UPD 
01426                          = HIGH-VALUES OR WS-SPACES-ZEROS         GAS4UPD 
01427                MOVE WS-SPACES-ZEROS                               GAS4UPD 
01428                  TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)              GAS4UPD 
01429             ELSE                                                  GAS4UPD 
01430                MOVE HIGH-VALUES                                   GAS4UPD 
01431                  TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)              GAS4UPD 
01432             END-IF                                                GAS4UPD 
01433         ELSE                                                      GAS4UPD 
01434             IF    GAD-O-P-X-INTL-TAB-3 (GAD-INDEX)                GAS4UPD 
01435                 = GCIO-WRK-TABULAR-PROVISION                      GAS4UPD 
01436             THEN                                                  GAS4UPD 
01437                 MOVE GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)             GAS4UPD 
01438                   TO GAD-O-P-X-INTL-TAB-3 (GAD-INDEX)             GAS4UPD 
01439                 MOVE GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)             GAS4UPD 
01440                   TO GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)             GAS4UPD 
01441 *************** MOVE HIGH-VALUES                                  GAS4UPD 
01442 ***************   TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)             GAS4UPD 
01443                 IF GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)               GAS4UPD 
01444                              = HIGH-VALUES OR WS-SPACES-ZEROS     GAS4UPD 
01445                    MOVE WS-SPACES-ZEROS                           GAS4UPD 
01446                      TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)          GAS4UPD 
01447                 ELSE                                              GAS4UPD 
01448                    MOVE HIGH-VALUES                               GAS4UPD 
01449                      TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)          GAS4UPD 
01450                 END-IF                                            GAS4UPD 
01451             ELSE                                                  GAS4UPD 
01452                 IF    GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)            GAS4UPD 
01453                     = GCIO-WRK-TABULAR-PROVISION                  GAS4UPD 
01454                 THEN                                              GAS4UPD 
01455                     MOVE GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)         GAS4UPD 
01456                       TO GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)         GAS4UPD 
01457 ******************* MOVE HIGH-VALUES                              GAS4UPD 
01458 *******************   TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)         GAS4UPD 
01459                     IF GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)           GAS4UPD 
01460                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS4UPD 
01461                        MOVE WS-SPACES-ZEROS                       GAS4UPD 
01462                          TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)      GAS4UPD 
01463                     ELSE                                          GAS4UPD 
01464                        MOVE HIGH-VALUES                           GAS4UPD 
01465                          TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)      GAS4UPD 
01466                     END-IF                                        GAS4UPD 
01467             ELSE                                                  GAS4UPD 
01468                 IF    GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)            GAS4UPD 
01469                     = GCIO-WRK-TABULAR-PROVISION                  GAS4UPD 
01470                 THEN                                              GAS4UPD 
01471 ******************* MOVE HIGH-VALUES                              GAS4UPD 
01472 *******************   TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)         GAS4UPD 
01473                     IF GAD-O-P-X-INTL-TAB-4 (GAD-INDEX)           GAS4UPD 
01474                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS4UPD 
01475                        MOVE WS-SPACES-ZEROS                       GAS4UPD 
01476                          TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)      GAS4UPD 
01477                     ELSE                                          GAS4UPD 
01478                        MOVE HIGH-VALUES                           GAS4UPD 
01479                          TO GAD-O-P-X-INTL-TAB-5 (GAD-INDEX)      GAS4UPD 
01480                     END-IF                                        GAS4UPD 
01481                 ELSE                                              GAS4UPD 
01482                     MOVE WS-ABCODE-1EL2        TO WS-ABCODE       GAS4UPD 
01483                     MOVE WS-ABCODE-1EL2-MSG    TO WS-ABCODE-MSG   GAS4UPD 
01484                     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.        GAS4UPD 
01485                                                                   GAS4UPD 
01486 **** SUBTRACT  1  FROM  GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX).   GAS4UPD 
01487      IF GAD-O-P-X-INTL-TAB-5 (GAD-INDEX) = HIGH-VALUES            GAS4UPD 
01488         NEXT SENTENCE                                             GAS4UPD 
01489      ELSE                                                         GAS4UPD 
01490         SUBTRACT  1  FROM  GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX).GAS4UPD 
01491                                                                   GAS4UPD 
01492      GO TO 2200-250-UPDATE-ALL-LVL-TAB.                           GAS4UPD 
01493                                                                   GAS4UPD 
01494                                                                   GAS4UPD 
01495  2200-220-ADD-INTERNAL-OCCURS.                                    GAS4UPD 
01496                                                                   GAS4UPD 
01497 **** IF GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX) > 5                GAS4UPD 
01498      IF GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX) = 5 AND            GAS4UPD 
01499**       GAD-INT-TS (GAD-INDEX 5) NOT = HIGH-VALUES                GAS4UPD 
01499         GAD-INT-ID (GAD-INDEX 5) NOT = HIGH-VALUES                GAS4UPD 
01500         SET  WT-01-INDEX                     TO +26               GAS4UPD 
01501         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           GAS4UPD 
01502         MOVE SPACES TO  IBGROPTO, IPGPOPTO, IDGDOPTO,             GAS4UPD 
01503                         IPGTOPTO, IPGNOPTO, IPGSOPTO              GAS4UPD 
01504         MOVE SPACES TO  MFRMSLTO                                  GAS4UPD 
01505         MOVE -1 TO PERIODL                                        GAS4UPD 
01506         PERFORM 9010-000-SEND-DATAONLY-RETURN.                    GAS4UPD 
01507                                                                   GAS4UPD 
01508      MOVE GXA-PROVISION-ID          TO     WS-SAVE-INTL-TAB-ID.   GAS4UPD 
01509      MOVE GXA-PROVISION-SLOT-NO     TO     WS-TAB-PROV-COPY-SLOT. GAS4UPD 
01510      MOVE GAD-OCCURS-ENTRY-COUNTER(GAD-INDEX)  TO                 GAS4UPD 
01511                                            WS-SAVE-INTL-TAB-SLOT. GAS4UPD 
01512                                                                   GAS4UPD 
01513      SET GAD-INT-INDEX  TO  1.                                    GAS4UPD 
01514      SEARCH GAD-INT-TS                                            GAS4UPD 
01515         VARYING GAD-INT-INDEX                                     GAS4UPD 
01516         AT END                                                    GAS4UPD 
01517            MOVE WS-ABCODE-1EL3      TO  WS-ABCODE                 GAS4UPD 
01518            MOVE WS-ABCODE-1EL3-MSG  TO  WS-ABCODE-MSG             GAS4UPD 
01519            PERFORM 9800-000-ERROR-MSG-THEN-ABEND                  GAS4UPD 
01520         WHEN                                                      GAS4UPD 
01521            GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  NOT <             GAS4UPD 
01522                                                 WS-SAVE-INTL-TAB  GAS4UPD 
01523            NEXT SENTENCE.                                         GAS4UPD 
01524                                                                   GAS4UPD 
01525      IF GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  NOT =                GAS4UPD 
01526                                                  WS-SAVE-INTL-TAB GAS4UPD 
01527         PERFORM 2200-225-SHIFT-OCCURS-UP                          GAS4UPD 
01528            VARYING GAD-INT-INDEX  FROM  GAD-INT-INDEX  BY  1      GAS4UPD 
01529            UNTIL GAD-INT-INDEX  >  5                              GAS4UPD 
01530 ******* ADD  1  TO  GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX)        GAS4UPD 
01531         IF GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX) < 5             GAS4UPD 
01532            ADD  1  TO  GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX)     GAS4UPD 
01533         END-IF                                                    GAS4UPD 
01534      ELSE                                                         GAS4UPD 
01535         MOVE WS-SAVE-INTL-TAB  TO                                 GAS4UPD 
01536                               GAD-INT-TS(GAD-INDEX GAD-INT-INDEX).GAS4UPD 
01537                                                                   GAS4UPD 
01538      GO TO 2200-240-SETUP-GCIO-PARMS.                             GAS4UPD 
01539                                                                   GAS4UPD 
01540                                                                   GAS4UPD 
01541  2200-225-SHIFT-OCCURS-UP.                                        GAS4UPD 
01542      MOVE GAD-INT-TS(GAD-INDEX GAD-INT-INDEX)  TO  WS-INTL-TAB-ID.GAS4UPD 
01543      MOVE WS-SAVE-INTL-TAB  TO                                    GAS4UPD 
01544                             GAD-INT-TS(GAD-INDEX GAD-INT-INDEX).  GAS4UPD 
01545                                                                   GAS4UPD 
01546      MOVE WS-INTL-TAB-ID  TO  WS-SAVE-INTL-TAB.                   GAS4UPD 
01547                                                                   GAS4UPD 
01548                                                                   GAS4UPD 
01549  2200-230-CHANGE-PROD-SLOT-NO.                                    GAS4UPD 
01550                                                                   GAS4UPD 
01551      MOVE GXA-PROVISION-SLOT-NO  TO  WS-TAB-PROV-COPY-SLOT.       GAS4UPD 
01552      SET  GAD-INT-INDEX TO      1.                                GAS4UPD 
01553      SET  GAD-INT-INDEX DOWN BY 1.                                GAS4UPD 
01554                                                                   GAS4UPD 
01555  2200-240-CHANGE-LOOP.                                            GAS4UPD 
01556                                                                   GAS4UPD 
01557      SET GAD-INT-INDEX UP BY 1.                                   GAS4UPD 
01558      IF  GAD-INT-INDEX > 5                                        GAS4UPD 
01559          GO TO 2200-240-SETUP-GCIO-PARMS.                         GAS4UPD 
01560                                                                   GAS4UPD 
01561      IF  GAD-INT-ID (GAD-INDEX GAD-INT-INDEX) = GXA-PROVISION-ID  GAS4UPD 
01562      THEN                                                         GAS4UPD 
01563          MOVE GAD-OCCURS-ENTRY-COUNTER (GAD-INDEX)                GAS4UPD 
01564            TO GXA-PROVISION-SLOT-NO                               GAS4UPD 
01565               GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)              GAS4UPD 
01566          GO TO 2200-240-SETUP-GCIO-PARMS                          GAS4UPD 
01567      ELSE                                                         GAS4UPD 
01568          GO TO 2200-240-CHANGE-LOOP.                              GAS4UPD 
01569                                                                   GAS4UPD 
01570                                                                   GAS4UPD 
01571  2200-240-SETUP-GCIO-PARMS.                                       GAS4UPD 
01572                                                                   GAS4UPD 
01573      IF FRMNUIDI  =  'GS3A'                                       GAS4UPD 
01574         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS4UPD 
01575      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS4UPD 
01576         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS4UPD 
01577      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
01578         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS4UPD 
01579         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS4UPD 
01580         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS4UPD 
01581                                                                   GAS4UPD 
01582      MOVE GC-GCPSWORK-DDNAME   TO  GCIO2-FILE-DDNAME.             GAS4UPD 
01583      MOVE GC-GCIO-AREA-1       TO  GCIO2-IO-AREA-TO-USE.          GAS4UPD 
01584      MOVE GXA-PROVISION-ID     TO  GCIO-WRK-TAB-PROVISION-ID.     GAS4UPD 
01585      MOVE GAD-OCCURS-ENTRY-COUNTER (GAD-INDEX)                    GAS4UPD 
01586                                  TO  GCIO-WRK-TAB-PROV-SLOT-NO    GAS4UPD 
01587                                      GXA-PROVISION-SLOT-NO.       GAS4UPD 
01588      MOVE GCIO-WORKFILE-KEY    TO  GCIO2-FILE-KEY                 GAS4UPD 
01589                                    WORK-RECORD-2.                 GAS4UPD 
01590      MOVE WS-TAB-PROV-COPY-SLOT  TO  WRK2-PROV-POOL-COPY-SLOT.    GAS4UPD 
01591                                                                   GAS4UPD 
01592      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
01593         MOVE GCA-BEN-PROV-ID  TO  WRK2-ALL-LEV-BEN-PROV.          GAS4UPD 
01594                                                                   GAS4UPD 
01595      MOVE GC-GCIO-ACCESS-CODE-WR  TO  GCIO2-FILE-ACCESS-CODE.     GAS4UPD 
01596                                                                   GAS4UPD 
01597      COMPUTE WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                    GAS4UPD 
01598               GC-GCIOPARM-LEN  +   GCIO2-RECORD-LENGTH.           GAS4UPD 
01599                                                                   GAS4UPD 
01600                                                                   GAS4UPD 
01601  2200-250-UPDATE-ALL-LVL-TAB.                                     GAS4UPD 
01602                                                                   GAS4UPD 
01603 *******                                                           GAS4UPD 
01604 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS4UPD 
01605 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLE UPDATES THEGAS4UPD 
01606 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS4UPD 
01607 *******                                                           GAS4UPD 
01608                                                                   GAS4UPD 
01609      IF  IBGROPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS4UPD 
01610      THEN                                                         GAS4UPD 
01611          MOVE MFRMSLTI     TO  IBGRSLTI,   ACWA-DISPLAY-LEN-7     GAS4UPD 
01612          SET  WT-01-INDEX  TO  +01                                GAS4UPD 
01613          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
01614          MOVE -1           TO  IBGROPTL                           GAS4UPD 
01615          MOVE DFHBMABF     TO  IBGRSLTA,   IBGRIDA                GAS4UPD 
01616          MOVE SPACES       TO  IBGROPTI                           GAS4UPD 
01617          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS4UPD 
01618          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS4UPD 
01619          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS4UPD 
01620      IF  IDGDOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS4UPD 
01621      THEN                                                         GAS4UPD 
01622          MOVE MFRMSLTI     TO  IDGDSLTI,   ACWA-DISPLAY-LEN-7     GAS4UPD 
01623          SET  WT-01-INDEX  TO  +23                                GAS4UPD 
01624          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
01625          MOVE -1           TO  IDGDOPTL                           GAS4UPD 
01626          MOVE DFHBMABF     TO  IDGDSLTA,   IDGDIDA                GAS4UPD 
01627          MOVE SPACES       TO  IDGDOPTI                           GAS4UPD 
01628          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS4UPD 
01629          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS4UPD 
01630          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS4UPD 
01631      IF  IPGNOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS4UPD 
01632      THEN                                                         GAS4UPD 
01633          MOVE MFRMSLTI     TO  IPGNSLTI,   ACWA-DISPLAY-LEN-7     GAS4UPD 
01634          SET  WT-01-INDEX  TO  +02                                GAS4UPD 
01635          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
01636          MOVE -1           TO  IPGNOPTL                           GAS4UPD 
01637          MOVE DFHBMABF     TO  IPGNSLTA,   IPGNIDA                GAS4UPD 
01638          MOVE SPACES       TO  IPGNOPTI                           GAS4UPD 
01639          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS4UPD 
01640          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS4UPD 
01641          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS4UPD 
01642      IF  IPGPOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS4UPD 
01643      THEN                                                         GAS4UPD 
01644          MOVE MFRMSLTI     TO  IPGPSLTI,   ACWA-DISPLAY-LEN-7     GAS4UPD 
01645          SET  WT-01-INDEX  TO  +24                                GAS4UPD 
01646          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
01647          MOVE -1           TO  IPGPOPTL                           GAS4UPD 
01648          MOVE DFHBMABF     TO  IPGPSLTA,   IPGPIDA                GAS4UPD 
01649          MOVE SPACES       TO  IPGPOPTI                           GAS4UPD 
01650          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS4UPD 
01651          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS4UPD 
01652          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS4UPD 
01653      IF  IPGSOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS4UPD 
01654      THEN                                                         GAS4UPD 
01655          MOVE MFRMSLTI     TO  IPGSSLTI,   ACWA-DISPLAY-LEN-7     GAS4UPD 
01656          SET  WT-01-INDEX  TO  +03                                GAS4UPD 
01657          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
01658          MOVE -1           TO  IPGSOPTL                           GAS4UPD 
01659          MOVE DFHBMABF     TO IPGSSLTA,    IPGSIDA                GAS4UPD 
01660          MOVE SPACES       TO IPGSOPTI                            GAS4UPD 
01661          MOVE DFHBMUNP     TO MFRMSLTA                            GAS4UPD 
01662          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS4UPD 
01663          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS4UPD 
01664      IF  IPGTOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS4UPD 
01665      THEN                                                         GAS4UPD 
01666          MOVE MFRMSLTI     TO  IPGTSLTI,   ACWA-DISPLAY-LEN-7     GAS4UPD 
01667          SET  WT-01-INDEX  TO  +03                                GAS4UPD 
01668          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
01669          MOVE -1           TO  IPGTOPTL                           GAS4UPD 
01670          MOVE DFHBMABF     TO IPGTSLTA,    IPGTIDA                GAS4UPD 
01671          MOVE SPACES       TO IPGTOPTI                            GAS4UPD 
01672          MOVE DFHBMUNP     TO MFRMSLTA                            GAS4UPD 
01673          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS4UPD 
01674          GO TO 2200-252-REPLACE-INT-TAB-SLOT                      GAS4UPD 
01675      ELSE                                                         GAS4UPD 
01676          GO TO 2200-253-BYPASS-INT-TAB-SLOT.                      GAS4UPD 
01677                                                                   GAS4UPD 
01678                                                                   GAS4UPD 
01679  2200-252-REPLACE-INT-TAB-SLOT.                                   GAS4UPD 
01680      SET  GAD-INT-INDEX  TO       1.                              GAS4UPD 
01681      SET  GAD-INT-INDEX  DOWN BY  1.                              GAS4UPD 
01682                                                                   GAS4UPD 
01683  2200-252-REPLACE-LOOP.                                           GAS4UPD 
01684                                                                   GAS4UPD 
01685      SET GAD-INT-INDEX  UP BY  1.                                 GAS4UPD 
01686                                                                   GAS4UPD 
01687      IF  GAD-INT-INDEX  >  5                                      GAS4UPD 
01688          MOVE WS-ABCODE-1ELX       TO  WS-ABCODE                  GAS4UPD 
01689          MOVE WS-ABCODE-1ELX-MSG   TO  WS-ABCODE-MSG              GAS4UPD 
01690          PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   GAS4UPD 
01691                                                                   GAS4UPD 
01692      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  =  GXA-PROVISION-ID GAS4UPD 
01693      THEN                                                         GAS4UPD 
01694          MOVE ACWA-DISPLAY-LEN-7  TO                              GAS4UPD 
01695                             GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX) GAS4UPD 
01696          GO TO 2200-252-REPLACE-LOOP-END                          GAS4UPD 
01697      ELSE                                                         GAS4UPD 
01698          GO TO 2200-252-REPLACE-LOOP.                             GAS4UPD 
01699                                                                   GAS4UPD 
01700  2200-252-REPLACE-LOOP-END.                                       GAS4UPD 
01701                                                                   GAS4UPD 
01702  2200-253-BYPASS-INT-TAB-SLOT.                                    GAS4UPD 
01703 *******                                                          |GAS4UPD 
01704 * STS *----------------------------------------------------------*GAS4UPD 
01705 *******                                                           GAS4UPD 
01706                                                                   GAS4UPD 
01707                                                                   GAS4UPD 
01708      PERFORM 3000-000-UPDATE-GAD-RECORD.                          GAS4UPD 
01709                                                                   GAS4UPD 
01710 *******                                                           GAS4UPD 
01711 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS4UPD 
01712 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS4UPD 
01713 *******     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.| GAS4UPD 
01714                                                                   GAS4UPD 
01715      IF  DELADDI   =  'CHG/ADD'  AND  FRMNUIDI  =  'GTM1'  AND    GAS4UPD 
01716          OENTCTRI  NOT =  '0000000' AND                           GAS4UPD 
01717         (IBGROPTI  =  'MT'  OR   IPGNOPTI  =  'MT' OR             GAS4UPD 
01718          IDGDOPTI  =  'MT'  OR   IPGPOPTI  =  'MT' OR             GAS4UPD 
01719          IPGTOPTI  =  'MT'  OR   IPGSOPTI  =  'MT')               GAS4UPD 
01720      THEN                                                         GAS4UPD 
01721          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS4UPD 
01722          MOVE 'CHG/ADD'   TO  DELADDO                             GAS4UPD 
01723          MOVE SPACES      TO  COCURANO                            GAS4UPD 
01724          MOVE DFHBMASD    TO  DLOPTLTA,   DELOPTNA                GAS4UPD 
01725          PERFORM 4100-000-DISPLAY-SKELETON                        GAS4UPD 
01726      ELSE                                                         GAS4UPD 
01727          NEXT SENTENCE.                                           GAS4UPD 
01728 *******                                                         | GAS4UPD 
01729 * STS *---------------------------------------------------------* GAS4UPD 
01730 *******                                                           GAS4UPD 
01731                                                                   GAS4UPD 
01732      IF (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND            GAS4UPD 
01733         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND            GAS4UPD 
01734         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND            GAS4UPD 
01735         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND            GAS4UPD 
01736         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND            GAS4UPD 
01737         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)                GAS4UPD 
01738         GO TO 2200-800.                                           GAS4UPD 
01739                                                                   GAS4UPD 
01740 *******                                                           GAS4UPD 
01741 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS4UPD 
01742 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS4UPD 
01743 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS4UPD 
01744 *******                                                           GAS4UPD 
01745                                                                   GAS4UPD 
01746      IF  DELADDI   =  'CHG/DEL'  AND  FRMNUIDI  =  'GTM1'  AND    GAS4UPD 
01747         (IBGROPTI  =  'MT'  OR  IPGNOPTI  =  'MT'  OR             GAS4UPD 
01748          IDGDOPTI  =  'MT'  OR  IPGPOPTI  =  'MT'  OR             GAS4UPD 
01749          IPGTOPTI  =  'MT'  OR  IPGSOPTI  =  'MT')                GAS4UPD 
01750      THEN                                                         GAS4UPD 
01751          GO TO 2200-800                                           GAS4UPD 
01752      ELSE                                                         GAS4UPD 
01753          NEXT SENTENCE.                                           GAS4UPD 
01754                                                                   GAS4UPD 
01755 *******                                                           GAS4UPD 
01756 * STS *==> DELETES DURING SINGLE TABULAR SUPPORT, THERE IS NEVER  GAS4UPD 
01757 *     *     A WORKFILE INTERNAL TABULAR TO DELETE                 GAS4UPD 
01758 *******                                                           GAS4UPD 
01759                                                                   GAS4UPD 
01760      IF  IBGROPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS4UPD 
01761          MOVE SPACE      TO  IBGROPTO                             GAS4UPD 
01762          MOVE '0000000'  TO  IBGRSLTO                             GAS4UPD 
01763          GO TO 2200-800.                                          GAS4UPD 
01764      IF  IDGDOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS4UPD 
01765          MOVE SPACE      TO  IDGDOPTO                             GAS4UPD 
01766          MOVE '0000000'  TO  IDGDSLTO                             GAS4UPD 
01767          GO TO 2200-800.                                          GAS4UPD 
01768      IF  IPGNOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS4UPD 
01769          MOVE SPACE      TO  IPGNOPTO                             GAS4UPD 
01770          MOVE '0000000'  TO  IPGNSLTO                             GAS4UPD 
01771          GO TO 2200-800.                                          GAS4UPD 
01772      IF  IPGPOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS4UPD 
01773          MOVE SPACE      TO  IPGPOPTO                             GAS4UPD 
01774          MOVE '0000000'  TO  IPGPSLTO                             GAS4UPD 
01775          GO TO 2200-800.                                          GAS4UPD 
01776      IF  IPGTOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS4UPD 
01777          MOVE SPACE      TO  IPGTOPTO                             GAS4UPD 
01778          MOVE '0000000'  TO  IPGTSLTO                             GAS4UPD 
01779          GO TO 2200-800.                                          GAS4UPD 
01780      IF  IPGSOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS4UPD 
01781          MOVE SPACE      TO  IPGSOPTO                             GAS4UPD 
01782          MOVE '0000000'  TO  IPGSSLTO                             GAS4UPD 
01783          GO TO 2200-800.                                          GAS4UPD 
01784                                                                   GAS4UPD 
01785 *******                                                          |GAS4UPD 
01786 * STS *----------------------------------------------------------*GAS4UPD 
01787 *******                                                           GAS4UPD 
01788                                                                   GAS4UPD 
01789      IF IBGROPTI  =  'D' AND  IBGRSLTI  <  '9000000'              GAS4UPD 
01790         MOVE SPACE      TO  IBGROPTO                              GAS4UPD 
01791         MOVE '0000000'  TO  IBGRSLTO                              GAS4UPD 
01792         GO TO 2200-800.                                           GAS4UPD 
01793      IF IDGDOPTI  =  'D' AND  IDGDSLTI  <  '9000000'              GAS4UPD 
01794         MOVE SPACE      TO  IDGDOPTO                              GAS4UPD 
01795         MOVE '0000000'  TO  IDGDSLTO                              GAS4UPD 
01796         GO TO 2200-800.                                           GAS4UPD 
01797      IF IPGNOPTI  =  'D' AND  IPGNSLTI  <  '9000000'              GAS4UPD 
01798         MOVE SPACE      TO  IPGNOPTO                              GAS4UPD 
01799         MOVE '0000000'  TO  IPGNSLTO                              GAS4UPD 
01800         GO TO 2200-800.                                           GAS4UPD 
01801      IF IPGPOPTI  =  'D' AND  IPGPSLTI  <  '9000000'              GAS4UPD 
01802         MOVE SPACE      TO  IPGPOPTO                              GAS4UPD 
01803         MOVE '0000000'  TO  IPGPSLTO                              GAS4UPD 
01804         GO TO 2200-800.                                           GAS4UPD 
01805      IF IPGTOPTI  =  'D' AND  IPGTSLTI  <  '9000000'              GAS4UPD 
01806         MOVE SPACE      TO  IPGTOPTO                              GAS4UPD 
01807         MOVE '0000000'  TO  IPGTSLTO                              GAS4UPD 
01808         GO TO 2200-800.                                           GAS4UPD 
01809      IF IPGSOPTI  =  'D' AND  IPGSSLTI  <  '9000000'              GAS4UPD 
01810         MOVE SPACE      TO  IPGSOPTO                              GAS4UPD 
01811         MOVE '0000000'  TO  IPGSSLTO                              GAS4UPD 
01812         GO TO 2200-800.                                           GAS4UPD 
01813                                                                   GAS4UPD 
01814      IF IBGROPTI  =  'D'                                          GAS4UPD 
01815         MOVE '0000000'  TO  IBGRSLTO                              GAS4UPD 
01816         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS4UPD 
01817      IF IDGDOPTI  =  'D'                                          GAS4UPD 
01818         MOVE '0000000'  TO  IDGDSLTO                              GAS4UPD 
01819         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS4UPD 
01820      IF IPGNOPTI  =  'D'                                          GAS4UPD 
01821         MOVE '0000000'  TO  IPGNSLTO                              GAS4UPD 
01822         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS4UPD 
01823      IF IPGPOPTI  =  'D'                                          GAS4UPD 
01824         MOVE '0000000'  TO  IPGPSLTO                              GAS4UPD 
01825         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS4UPD 
01826      IF IPGTOPTI  =  'D'                                          GAS4UPD 
01827         MOVE '0000000'  TO  IPGTSLTO                              GAS4UPD 
01828         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS4UPD 
01829      IF IPGSOPTI  =  'D'                                          GAS4UPD 
01830         MOVE '0000000'  TO  IPGSSLTO                              GAS4UPD 
01831         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS4UPD 
01832                                                                   GAS4UPD 
01833                                                                   GAS4UPD 
01834  2200-260-CHANGE-INTERNAL-TAB.                                    GAS4UPD 
01835                                                                   GAS4UPD 
01836 *    EXEC CICS GETMAIN                                            GAS4UPD 
01837 *              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             GAS4UPD 
01838 *              INITIMG(WS-HEX-00)                                 GAS4UPD 
01839 *              LENGTH(WS-COMMUNICATION-KEY-LEN)                   GAS4UPD 
01840 *              END-EXEC.                                          GAS4UPD 
01841                                                                   GAS4UPD 
01842 *    SET ACWA-COMM-KEY-PNTR  TO                                   GAS4UPD 
01843 *                      ADDRESS OF COMMUNICATION-KEY-AREA.         GAS4UPD 
01844                                                                   GAS4UPD 
01845      SET GCA-RECORD-POINTER TO                                    GAS4UPD 
01846                     ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    GAS4UPD 
01847                                                                   GAS4UPD 
01848      MOVE GCIO-WRK-PLAN-CODE         TO  GCA-PLAN-CODE.           GAS4UPD 
01849      MOVE GCIO-WRK-GROUP-NUM         TO  GCA-GROUP-NUM.           GAS4UPD 
01850      MOVE GCIO-WRK-SECTION-NUM       TO  GCA-SECTION-NUM.         GAS4UPD 
01851      MOVE GCIO-WRK-PKG-CODE          TO  GCA-PKG-CODE.            GAS4UPD 
01852      MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B.               GAS4UPD 
01853      MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL.            GAS4UPD 
01854      MOVE GCIO-WRK-FAMILY-RELATION-LVL  TO                        GAS4UPD 
01855                                          GCA-FAM-REL-LVL.         GAS4UPD 
01856      MOVE GCIO-WRK-EFFDT-CEN         TO  GCA-EFFDT-CEN.           GAS4UPD 
01857 *    IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
01858 *       MOVE BEN-PROV-ID-NO          TO  GCA-BEN-PROV-ID.         GAS4UPD 
01859      MOVE TABIDI                     TO  GCA-ALL-LEVEL-TAB-ID.    GAS4UPD 
01860      MOVE TABSLTNI                   TO  ACWA-DISPLAY-LEN-7.      GAS4UPD 
01861      MOVE ACWA-DISPLAY-LEN-7         TO  GCA-ALL-LEVEL-TAB-SLOT.  GAS4UPD 
01862      MOVE FUNCTONI          TO   GCA-ALL-LEVEL-TAB-FUNC-CODE.     GAS4UPD 
01863      MOVE GXA-PROVISION-ID           TO  GCA-INTERNAL-TAB-ID.     GAS4UPD 
01864      MOVE OENTCTRI                   TO  GCA-INTERNAL-TAB-SLOT    GAS4UPD 
01865                                          GCA-OCCURS-ENTRY-COUNTER.GAS4UPD 
01866                                                                   GAS4UPD 
01867      IF  DELADDI  =  'CHG/ADD'                                    GAS4UPD 
01868          MOVE 'A'  TO  GCA-ADD-DEL-IND                            GAS4UPD 
01869      ELSE                                                         GAS4UPD 
01870          MOVE 'D'  TO  GCA-ADD-DEL-IND.                           GAS4UPD 
01871                                                                   GAS4UPD 
01872      MOVE GXA-INCLUDE-EXCLUDE-IND  TO  GCA-I-E-INDC.              GAS4UPD 
01873      MOVE FRMNUIDI                 TO  GCA-FROM-MENU-ID.          GAS4UPD 
01874                                                                   GAS4UPD 
01875      IF IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999' OR           GAS4UPD 
01876         IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999' OR           GAS4UPD 
01877         IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999' OR           GAS4UPD 
01878         IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999' OR           GAS4UPD 
01879         IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999' OR           GAS4UPD 
01880         IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'              GAS4UPD 
01881         GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                       GAS4UPD 
01882                                                                   GAS4UPD 
01883      IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            GAS4UPD 
01884         MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                GAS4UPD 
01885         MOVE '1U'      TO  WRK2-CDE-SP                            GAS4UPD 
01886         ADD   1        TO  ACWA-CDE-1U-COUNT                      GAS4UPD 
01887      ELSE                                                         GAS4UPD 
01888         IF CDEINDO   =  ('+CDE+' OR  '+CDE-') AND                 GAS4UPD 
01889            DELADDI   =  'CHG/DEL'                                 GAS4UPD 
01890            IF INTDESKI  =  IDPRODI  AND                           GAS4UPD 
01891               WS-NEW-OCCR-ON-WF  = 'N'         THEN               GAS4UPD 
01892               MOVE '2 '   TO  WRK2-CDE-SP                         GAS4UPD 
01893               ADD   1     TO  ACWA-CDE-2B-COUNT                   GAS4UPD 
01894            ELSE                                                   GAS4UPD 
01895               MOVE '1U'   TO  WRK2-CDE-SP                         GAS4UPD 
01896               ADD   1     TO  ACWA-CDE-1U-COUNT                   GAS4UPD 
01897         ELSE                                                      GAS4UPD 
01898            IF  WS-NEW-OCCR-ON-WF = 'Y'     AND                    GAS4UPD 
01899                CDEINDO  = ('+CDE+'  OR '+CDE-')                   GAS4UPD 
01900                MOVE '1U'     TO WRK2-CDE-SP                       GAS4UPD 
01901                ADD   1       TO ACWA-CDE-1U-COUNT                 GAS4UPD 
01902            ELSE                                                   GAS4UPD 
01903                MOVE '2 '   TO  WRK2-CDE-SP                        GAS4UPD 
01904                ADD   1     TO  ACWA-CDE-2B-COUNT.                 GAS4UPD 
01905                                                                   GAS4UPD 
01906 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS4UPD 
01907      MOVE '1'          TO  GCIO2-OPER-ID-IND.                     GAS4UPD 
01908                                                                   GAS4UPD 
01909      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
01910                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS4UPD 
01911                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS4UPD 
01912                                                                   GAS4UPD 
01913      IF NOT GCIO2-GOOD-RETURN                                     GAS4UPD 
01914         MOVE WS-ABCODE-1EF5       TO  WS-ABCODE                   GAS4UPD 
01915         MOVE WS-ABCODE-1EF5-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
01916         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
01917                                                                   GAS4UPD 
01918      GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                          GAS4UPD 
01919                                                                   GAS4UPD 
01920                                                                   GAS4UPD 
01921  2200-270-DELETE-INTERNAL-TAB.                                    GAS4UPD 
01922                                                                   GAS4UPD 
01923      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS4UPD 
01924         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS4UPD 
01925           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS4UPD 
01926           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS4UPD 
01927                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS4UPD 
01928         EXEC CICS GETMAIN                                         GAS4UPD 
01929                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS4UPD 
01930                INITIMG(WS-HEX-00)                                 GAS4UPD 
01931                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS4UPD 
01932                END-EXEC                                           GAS4UPD 
01933         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS4UPD 
01934                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS4UPD 
01935                                                                   GAS4UPD 
01936      IF FRMNUIDI  =  'GS3A'                                       GAS4UPD 
01937         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS4UPD 
01938      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS4UPD 
01939         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS4UPD 
01940      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
01941         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS4UPD 
01942         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS4UPD 
01943         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS4UPD 
01944                                                                   GAS4UPD 
01945      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS4UPD 
01946      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS4UPD 
01947      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS4UPD 
01948      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS4UPD 
01949      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS4UPD 
01950           TO  GXA-ENTRY-COUNT.                                    GAS4UPD 
01951                                                                   GAS4UPD 
01952      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
01953                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS4UPD 
01954                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS4UPD 
01955                                                                   GAS4UPD 
01956      IF NOT GCIO2-GOOD-RETURN                                     GAS4UPD 
01957         MOVE WS-ABCODE-1EF9       TO  WS-ABCODE                   GAS4UPD 
01958         MOVE WS-ABCODE-1EF9-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
01959         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
01960                                                                   GAS4UPD 
01961      MOVE GC-GCIO-ACCESS-CODE-DL TO  GCIO2-FILE-ACCESS-CODE.      GAS4UPD 
01962      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
01963                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS4UPD 
01964                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS4UPD 
01965                                                                   GAS4UPD 
01966      IF NOT GCIO2-GOOD-RETURN                                     GAS4UPD 
01967         MOVE WS-ABCODE-1EF8       TO  WS-ABCODE                   GAS4UPD 
01968         MOVE WS-ABCODE-1EF8-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
01969         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
01970                                                                   GAS4UPD 
01971      IF WRK2-CDE-SP =  '1U'                                       GAS4UPD 
01972         SUBTRACT  1  FROM  ACWA-CDE-1U-COUNT.                     GAS4UPD 
01973                                                                   GAS4UPD 
01974      IF WRK2-CDE-SP  =  '2 '                                      GAS4UPD 
01975         SUBTRACT  1  FROM  ACWA-CDE-2B-COUNT.                     GAS4UPD 
01976                                                                   GAS4UPD 
01977      MOVE SPACES  TO  IBGROPTO,  IPGNOPTO,  IPGTOPTO              GAS4UPD 
01978                       IDGDOPTO,  IPGPOPTO,  IPGSOPTO.             GAS4UPD 
01979      GO TO 2200-800.                                              GAS4UPD 
01980                                                                   GAS4UPD 
01981                                                                   GAS4UPD 
01982  2200-280-XCTL-TO-INT-TAB-PGM.                                    GAS4UPD 
01983                                                                   GAS4UPD 
01984      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS4UPD 
01985         DELADDI  =  'CHG/DEL' AND                                 GAS4UPD 
01986         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS4UPD 
01987         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS4UPD 
01988         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS4UPD 
01989                                                                   GAS4UPD 
01990      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS4UPD 
01991         ACWA-CDE-2B-COUNT  =  ZERO                                GAS4UPD 
01992         NEXT SENTENCE                                             GAS4UPD 
01993      ELSE                                                         GAS4UPD 
01994         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS4UPD 
01995                                                                   GAS4UPD 
01996      MOVE INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.       GAS4UPD 
01997      MOVE 'GAS4UPD' TO DELADD-OPTION.                             GAS4UPD 
01998                                                                   GAS4UPD 
01999      EXEC CICS  XCTL  PROGRAM (WS-INTERNAL-TABULAR-PGM-ID)        GAS4UPD 
02000                 COMMAREA(DFHCOMMAREA)                             GAS4UPD 
02001                 LENGTH  (LENGTH OF DFHCOMMAREA)                   GAS4UPD 
02002                 END-EXEC.                                         GAS4UPD 
02003                                                                   GAS4UPD 
02004  2200-800.                                                        GAS4UPD 
02005                                                                   GAS4UPD 
02006      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS4UPD 
02007         DELADDI  =  'CHG/DEL' AND                                 GAS4UPD 
02008         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS4UPD 
02009         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS4UPD 
02010         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS4UPD 
02011                                                                   GAS4UPD 
02012      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS4UPD 
02013         ACWA-CDE-2B-COUNT  =  ZERO                                GAS4UPD 
02014         NEXT SENTENCE                                             GAS4UPD 
02015      ELSE                                                         GAS4UPD 
02016         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS4UPD 
02017                                                                   GAS4UPD 
02018      MOVE -1  TO  PERIODL.                                        GAS4UPD 
02019      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS4UPD 
02020                                                                   GAS4UPD 
02021  2200-900-EXIT. EXIT.                                             GAS4UPD 
02022 /*****************************************************************GAS4UPD 
02023 *           D E L E T E   T H I S   O C C U R A N C E            *GAS4UPD 
02024 *    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *GAS4UPD 
02025 *  SCREEN'S DISPLAY AND REMOVE THAT ENTRY FROM THE ALL LEVEL     *GAS4UPD 
02026 *  TABULAR RECORD, INCLUDED WITH THAT IS CODE TO DELETE ANY      *GAS4UPD 
02027 *  INTERNAL TABULAR ENTRIES THAT MIGHT BE SPECIFIED BY THAT ENTRY*GAS4UPD 
02028 ******************************************************************GAS4UPD 
02029  2400-000-DELETE-THIS-OCCURANCE SECTION.                          GAS4UPD 
02030  2400-010.                                                        GAS4UPD 
02031                                                                   GAS4UPD 
02032      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS4UPD 
02033                                                                   GAS4UPD 
02034      IF NOT GCIO-GOOD-RETURN                                      GAS4UPD 
02035         MOVE WS-ABCODE-1EF7       TO  WS-ABCODE                   GAS4UPD 
02036         MOVE WS-ABCODE-1EF7-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
02037         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
02038                                                                   GAS4UPD 
02039      IF FRMNUIDI  =  'GS3A'                                       GAS4UPD 
02040         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS4UPD 
02041      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS4UPD 
02042         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS4UPD 
02043      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
02044         MOVE 'C6'  TO  GCIO-WRK-RECORD-TYPE                       GAS4UPD 
02045         MOVE GCIO-WRK-TABULAR-PROVISION                           GAS4UPD 
02046                    TO  GCIO-WRK-BENEFIT-PROVISION.                GAS4UPD 
02047                                                                   GAS4UPD 
02048      MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   GAS4UPD 
02049      MOVE OENTCTRI         TO  ACWA-DISPLAY-LEN-7.                GAS4UPD 
02050                                                                   GAS4UPD 
02051         EXEC CICS GETMAIN                                         GAS4UPD 
02052                SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            GAS4UPD 
02053                INITIMG(WS-HEX-00)                                 GAS4UPD 
02054                LENGTH(WS-COPY-TABLE-LEN)                          GAS4UPD 
02055                END-EXEC.                                          GAS4UPD 
02056                                                                   GAS4UPD 
02057         SET ACWA-COPY-TAB-PNTR        TO                          GAS4UPD 
02058                  ADDRESS OF COPY-TABULAR-TABLE-AREA.              GAS4UPD 
02059                                                                   GAS4UPD 
02060      SET GAD-INDEX,  COPY-IDX  TO  1.                             GAS4UPD 
02061                                                                   GAS4UPD 
02062                                                                   GAS4UPD 
02063  2400-100-COPY-SAVED-AND-DELETE.                                  GAS4UPD 
02064                                                                   GAS4UPD 
02065      IF  GAD-INDEX  <  GAD-ENTRY-COUNT                            GAS4UPD 
02066      THEN                                                         GAS4UPD 
02067          IF  GAD-OCCURS-ENTRY-COUNTER (GAD-INDEX)  NOT =          GAS4UPD 
02068              ACWA-DISPLAY-LEN-7                                   GAS4UPD 
02069          THEN                                                     GAS4UPD 
02070              MOVE GAD-ENTRY          (GAD-INDEX)                  GAS4UPD 
02071                TO COPY-TABULAR-TABLE (COPY-IDX)                   GAS4UPD 
02072              SET  GAD-INDEX,  COPY-IDX  UP BY  1                  GAS4UPD 
02073              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS4UPD 
02074          ELSE                                                     GAS4UPD 
02075 *---- WE ARE NOT GOING TO COPY THE ENTRY THAT IS BEING DELETED.   GAS4UPD 
02076 *---- BUT WE SAVE IT SINCE WE HAVE TO DELETE ANY INTERNAL TABULARSGAS4UPD 
02077              MOVE GAD-ENTRY (GAD-INDEX)  TO  WS-ENTRY             GAS4UPD 
02078              SET  COPY-IDX3  TO  GAD-INDEX                        GAS4UPD 
02079              SET  GAD-INDEX  UP BY  1                             GAS4UPD 
02080              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS4UPD 
02081      ELSE                                                         GAS4UPD 
02082          MOVE GAD-ENTRY          (GAD-INDEX)                      GAS4UPD 
02083            TO COPY-TABULAR-TABLE (COPY-IDX).                      GAS4UPD 
02084                                                                   GAS4UPD 
02085      SET  GAD-ENTRY-COUNT  TO  COPY-IDX.                          GAS4UPD 
02086      MOVE GAD-ENTRY-COUNT  TO  GAD-ENTRY-COUNT.                   GAS4UPD 
02087      SET COPY-IDX,  GAD-INDEX  TO  1.                             GAS4UPD 
02088                                                                   GAS4UPD 
02089                                                                   GAS4UPD 
02090  2400-200-MOVE-UPDATED-TABLE.                                     GAS4UPD 
02091                                                                   GAS4UPD 
02092      IF GAD-INDEX  NOT >  GAD-ENTRY-COUNT                         GAS4UPD 
02093         MOVE COPY-TABULAR-TABLE (COPY-IDX)                        GAS4UPD 
02094           TO GAD-ENTRY          (GAD-INDEX)                       GAS4UPD 
02095         SET GAD-INDEX,  COPY-IDX  UP BY  1                        GAS4UPD 
02096         GO TO 2400-200-MOVE-UPDATED-TABLE.                        GAS4UPD 
02097                                                                   GAS4UPD 
02098 *=========== D1218 06/03/88 NE  =============================     GAS4UPD 
02099       IF CDEINDO  =  '+CDE+'  OR '+CDE-'                          GAS4UPD 
02100          PERFORM 4600-000-UPDATE-CDE-STATUS.                      GAS4UPD 
02101                                                                   GAS4UPD 
02102 *===========================================================      GAS4UPD 
02103      PERFORM 3000-000-UPDATE-GAD-RECORD.                          GAS4UPD 
02104                                                                   GAS4UPD 
02105      SET GAD-INDEX  TO  GAD-ENTRY-COUNT.                          GAS4UPD 
02106      MOVE WS-ENTRY  TO  GAD-ENTRY (GAD-INDEX).                    GAS4UPD 
02107                                                                   GAS4UPD 
02108      IF GAD-INTERNAL-TABULAR-COUNT (GAD-INDEX)  =  1              GAS4UPD 
02109         GO TO 2400-340-DELETE-LOOP-END.                           GAS4UPD 
02110                                                                   GAS4UPD 
02111                                                                   GAS4UPD 
02112 *---- DELETE INTERNAL TABULARS FROM W/F THAT ARE ATTACHED TO      GAS4UPD 
02113 *       ALL LVL TAB OCCURANCE BEING DELETED                       GAS4UPD 
02114  2400-300-DELETE-INTERNAL-TABS.                                   GAS4UPD 
02115      SET GAD-INT-INDEX  TO       1.                               GAS4UPD 
02116      SET GAD-INT-INDEX  DOWN BY  1.                               GAS4UPD 
02117                                                                   GAS4UPD 
02118  2400-320-DELETE-LOOP.                                            GAS4UPD 
02119                                                                   GAS4UPD 
02120      SET GAD-INT-INDEX UP BY 1.                                   GAS4UPD 
02121      IF  GAD-INT-INDEX >  5                                       GAS4UPD 
02122          GO TO 2400-340-DELETE-LOOP-END.                          GAS4UPD 
02123                                                                   GAS4UPD 
02124      IF GAD-INT-ID (GAD-INDEX GAD-INT-INDEX)  =  HIGH-VALUES      GAS4UPD 
02125         GO TO 2400-340-DELETE-LOOP-END.                           GAS4UPD 
02126                                                                   GAS4UPD 
02127 ************   09/14/88  NE                                       GAS4UPD 
02128      IF  GAD-INT-SLOT (GAD-INDEX GAD-INT-INDEX)  >  8999999       GAS4UPD 
02129          NEXT SENTENCE                                            GAS4UPD 
02130      ELSE                                                         GAS4UPD 
02131          GO TO  2400-320-DELETE-LOOP.                             GAS4UPD 
02132                                                                   GAS4UPD 
02133      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS4UPD 
02134         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS4UPD 
02135           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS4UPD 
02136           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS4UPD 
02137                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS4UPD 
02138         EXEC CICS GETMAIN                                         GAS4UPD 
02139                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS4UPD 
02140                INITIMG(WS-HEX-00)                                 GAS4UPD 
02141                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS4UPD 
02142                END-EXEC                                           GAS4UPD 
02143         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS4UPD 
02144                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS4UPD 
02145                                                                   GAS4UPD 
02146                                                                   GAS4UPD 
02147      MOVE GAD-INT-TS (GAD-INDEX GAD-INT-INDEX)                    GAS4UPD 
02148                                  TO GCIO-WRK-TABULAR-PROVISION.   GAS4UPD 
02149      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS4UPD 
02150      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS4UPD 
02151      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS4UPD 
02152      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS4UPD 
02153                                                                   GAS4UPD 
02154      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS4UPD 
02155           TO  GXA-ENTRY-COUNT.                                    GAS4UPD 
02156                                                                   GAS4UPD 
02157      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
02158                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS4UPD 
02159                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS4UPD 
02160      IF NOT GCIO2-GOOD-RETURN                                     GAS4UPD 
02161         MOVE WS-ABCODE-1EF9       TO  WS-ABCODE                   GAS4UPD 
02162         MOVE WS-ABCODE-1EF9-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
02163         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
02164                                                                   GAS4UPD 
02165                                                                   GAS4UPD 
02166      MOVE GC-GCIO-ACCESS-CODE-DL TO GCIO2-FILE-ACCESS-CODE.       GAS4UPD 
02167      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
02168                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS4UPD 
02169                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS4UPD 
02170                                                                   GAS4UPD 
02171      IF NOT GCIO2-GOOD-RETURN                                     GAS4UPD 
02172         MOVE WS-ABCODE-1EF8       TO  WS-ABCODE                   GAS4UPD 
02173         MOVE WS-ABCODE-1EF8-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
02174         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
02175                                                                   GAS4UPD 
02176      IF WRK2-CDE-SP  =  '1U'                                      GAS4UPD 
02177         SUBTRACT 1  FROM  ACWA-CDE-1U-COUNT.                      GAS4UPD 
02178      IF WRK2-CDE-SP  =  '2 '                                      GAS4UPD 
02179         SUBTRACT 1  FROM  ACWA-CDE-2B-COUNT.                      GAS4UPD 
02180                                                                   GAS4UPD 
02181      GO TO 2400-320-DELETE-LOOP.                                  GAS4UPD 
02182                                                                   GAS4UPD 
02183                                                                   GAS4UPD 
02184  2400-340-DELETE-LOOP-END.                                        GAS4UPD 
02185      PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      GAS4UPD 
02186                                                                   GAS4UPD 
02187  2400-400-DISPLAY-SCREEN.                                         GAS4UPD 
02188                                                                   GAS4UPD 
02189      IF COPY-IDX3  =  GAD-ENTRY-COUNT AND  =  1                   GAS4UPD 
02190         MOVE 'CHG/ADD'     TO  DELADDO                            GAS4UPD 
02191         MOVE SPACES        TO  DELOPTNO,   DELOLITO               GAS4UPD 
02192         MOVE DFHBMASD      TO  DLOPTLTA,   DELOPTNA               GAS4UPD 
02193         SET  WT-01-INDEX   TO  +19                                GAS4UPD 
02194         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO         GAS4UPD 
02195         PERFORM 4100-000-DISPLAY-SKELETON.                        GAS4UPD 
02196                                                                   GAS4UPD 
02197      IF  COPY-IDX3  <  GAD-ENTRY-COUNT                            GAS4UPD 
02198      THEN                                                         GAS4UPD 
02199          SET GAD-INDEX  TO  COPY-IDX3                             GAS4UPD 
02200          MOVE SPACES    TO  ERRMSGO                               GAS4UPD 
02201          PERFORM 4400-000-BUILD-DISPLAY                           GAS4UPD 
02202      ELSE                                                         GAS4UPD 
02203          SET  GAD-INDEX     TO  1                                 GAS4UPD 
02204          SET  WT-01-INDEX   TO  +14                               GAS4UPD 
02205          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS4UPD 
02206          PERFORM 4400-000-BUILD-DISPLAY.                          GAS4UPD 
02207                                                                   GAS4UPD 
02208      MOVE SPACES  TO  DELOPTNO.                                   GAS4UPD 
02209      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS4UPD 
02210                                                                   GAS4UPD 
02211  2400-900-EXIT. EXIT.                                             GAS4UPD 
02212                                                                   GAS4UPD 
02213 /*****************************************************************GAS4UPD 
02214 *     P R O C E S S   V A L   L I M I T                           GAS4UPD 
02215 ******************************************************************GAS4UPD 
02216  2600-000-PROCESS-VAL-LIMIT     SECTION.                          GAS4UPD 
02217  2600-010.                                                        GAS4UPD 
02218                                                                   GAS4UPD 
02219      IF (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'NEG' OR                   GAS4UPD 
02220          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 GAS4UPD 
02219         (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'UNL' OR                   GAS4UPD 
02220          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    GAS4UPD 
02221          GO TO 2600-900-EXIT.                                     GAS4UPD 
02222                                                                   GAS4UPD 
02223      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS4UPD 
02224      THEN                                                         GAS4UPD 
02225          IF  BENVLQLI  =  '5'                                     GAS4UPD 
02226          THEN                                                     GAS4UPD 
02227              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         GAS4UPD 
02228              MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         GAS4UPD 
02229              GO TO 2600-900-EXIT                                  GAS4UPD 
02230          ELSE                                                     GAS4UPD 
02231              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       GAS4UPD 
02232              GO TO 2600-900-EXIT                                  GAS4UPD 
02233      ELSE                                                         GAS4UPD 
02234          NEXT SENTENCE.                                           GAS4UPD 
02235                                                                   GAS4UPD 
02236      IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            GAS4UPD 
02237          MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       GAS4UPD 
02238          MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       GAS4UPD 
02239          GO TO 2600-900-EXIT.                                     GAS4UPD 
02240                                                                   GAS4UPD 
02241  2600-900-EXIT. EXIT.                                             GAS4UPD 
02242                                                                   GAS4UPD 
02243 /*****************************************************************GAS4UPD 
02244 * 3000 UPDATE GAD RECORD                                         *GAS4UPD 
02245 *                                                                *GAS4UPD 
02246 *    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *GAS4UPD 
02247 ******************************************************************GAS4UPD 
02248  3000-000-UPDATE-GAD-RECORD     SECTION.                          GAS4UPD 
02249  3000-010.                                                        GAS4UPD 
02250                                                                   GAS4UPD 
02251      COMPUTE  GCIO-RECORD-LENGTH   =                              GAS4UPD 
02252          GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-AOL-FIXED-LEN  +     GAS4UPD 
02253          (GC-GCTABULR-AOL-VARY-LEN * GAD-ENTRY-COUNT).            GAS4UPD 
02254                                                                   GAS4UPD 
02255 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS4UPD 
02256      MOVE '1'                      TO  GCIO-OPER-ID-IND.          GAS4UPD 
02257      MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     GAS4UPD 
02258                                                                   GAS4UPD 
02259      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
02260                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS4UPD 
02261                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS4UPD 
02262                                                                   GAS4UPD 
02263      IF NOT GCIO-GOOD-RETURN                                      GAS4UPD 
02264         MOVE WS-ABCODE-1EF2       TO  WS-ABCODE                   GAS4UPD 
02265         MOVE WS-ABCODE-1EF2-MSG   TO  WS-ABCODE-MSG               GAS4UPD 
02266         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
02267                                                                   GAS4UPD 
02268  3000-900-EXIT. EXIT.                                             GAS4UPD 
02269                                                                   GAS4UPD 
02270 ******************************************************************GAS4UPD 
02271 * 3100  UNLOCK THE GAD ACCUM TAB RECORD READ EARLIER FOR UPDATE   GAS4UPD 
02272 ******************************************************************GAS4UPD 
02273  3100-RLSE-RU-GAD-REC SECTION.                                    GAS4UPD 
02274                                                                   GAS4UPD 
02275      MOVE GC-GCIO-ACCESS-CODE-UNL  TO  GCIO-FILE-ACCESS-CODE.     GAS4UPD 
02276                                                                   GAS4UPD 
02277      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GAS4UPD 
02278                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS4UPD 
02279                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS4UPD 
02280                                                                   GAS4UPD 
02281      IF NOT GCIO-GOOD-RETURN                                      GAS4UPD 
02282         MOVE WS-ABCODE-1EF3        TO  WS-ABCODE                  GAS4UPD 
02283         MOVE WS-ABCODE-1EF3-MSG    TO  WS-ABCODE-MSG              GAS4UPD 
02284         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
02285  3199-EXIT.     EXIT.                                             GAS4UPD 
02286                                                                   GAS4UPD 
02287 /*****************************************************************GAS4UPD 
02288 * 3200  READ REC FOR UPDATE                                      *GAS4UPD 
02289 *                                                                *GAS4UPD 
02290 *    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *GAS4UPD 
02291 *  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *GAS4UPD 
02292 ******************************************************************GAS4UPD 
02293  3200-000-READ-REC-FOR-UPDATE   SECTION.                          GAS4UPD 
02294  3300-010.                                                        GAS4UPD 
02295                                                                   GAS4UPD 
02296      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN   +  GAS4UPD 
02297            GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-AOL-FIXED-LEN   +  GAS4UPD 
02298            (GC-GCTABULR-AOL-VARY-LEN   *                          GAS4UPD 
02299                                  GC-GCTABULR-AOL-VARY-MAX-OCUR).  GAS4UPD 
02300                                                                   GAS4UPD 
02301      IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS4UPD 
02302         NEXT SENTENCE                                             GAS4UPD 
02303      ELSE                                                         GAS4UPD 
02304         EXEC CICS GETMAIN                                         GAS4UPD 
02305                SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      GAS4UPD 
02306                INITIMG(WS-HEX-00)                                 GAS4UPD 
02307                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS4UPD 
02308                END-EXEC                                           GAS4UPD 
02309         SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     GAS4UPD 
02310                  ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        GAS4UPD 
02311                                                                   GAS4UPD 
02312                                                                   GAS4UPD 
02313      IF FRMNUIDI  =  'GS3A'                                       GAS4UPD 
02314         PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    GAS4UPD 
02315      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS4UPD 
02316         PERFORM 6100-000-BUILD-CONTRACT-KEY.                      GAS4UPD 
02317      IF FRMNUIDI  =  'GC8A'                                       GAS4UPD 
02318         PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      GAS4UPD 
02319                                                                   GAS4UPD 
02320      MOVE GC-GCPSWORK-DDNAME      TO  GCIO-FILE-DDNAME.           GAS4UPD 
02321      MOVE GC-GCIO-AREA-1          TO  GCIO-IO-AREA-TO-USE.        GAS4UPD 
02322      MOVE GCIO-WORKFILE-KEY       TO  GCIO-FILE-KEY.              GAS4UPD 
02323      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO-FILE-ACCESS-CODE.      GAS4UPD 
02324      MOVE GC-GCTABULR-AOL-VARY-MAX-OCUR  TO  GAD-ENTRY-COUNT.     GAS4UPD 
02325                                                                   GAS4UPD 
02326      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS4UPD 
02327                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS4UPD 
02328                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS4UPD 
02329                                                                   GAS4UPD 
02330  3200-900-EXIT. EXIT.                                             GAS4UPD 
02331                                                                   GAS4UPD 
02332 /*****************************************************************GAS4UPD 
02333 * 4100  DISPLAY SKELETON                                         *GAS4UPD 
02334 *                                                                *GAS4UPD 
02335 *    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *GAS4UPD 
02336 *  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *GAS4UPD 
02337 *  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *GAS4UPD 
02338 ******************************************************************GAS4UPD 
02339  4100-000-DISPLAY-SKELETON      SECTION.                          GAS4UPD 
02340  4100-010.                                                        GAS4UPD 
02341                                                                   GAS4UPD 
02342      MOVE SPACES    TO  ERRMSGO.                                  GAS4UPD 
02343                                                                   GAS4UPD 
02344      MOVE DFHBMFSE  TO  PERIODA.                                  GAS4UPD 
02345                                                                   GAS4UPD 
02346      MOVE DFHBMUNP  TO  BENVLQLA FAMINDIA  INTDESKA  LOBA         GAS4UPD 
02347                         IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA     GAS4UPD 
02348                         IDGDOPTA IPGPOPTA  IPGSOPTA.              GAS4UPD 
02349                                                                   GAS4UPD 
02350      MOVE ALL '_'  TO  PERIODO   BENVLQLO  LOBO                   GAS4UPD 
02351                        FAMINDIO  PLCTRMTO.                        GAS4UPD 
02352                                                                   GAS4UPD 
02353      MOVE LOW-VALUES  TO  INTDESKO  IBGROPTO  IPGNOPTO IDGDOPTO   GAS4UPD 
02354                           IPGTOPTO  MFRMSLTO  IPGPOPTO IPGSOPTO.  GAS4UPD 
02355                                                                   GAS4UPD 
02356      MOVE ZEROS  TO  COPAYINO  CSTCONTO  PERTQALO  ASCDSCDO       GAS4UPD 
02357            DAYFACIO  SRVGRUPO  PRTIMEFO  CARYOVRO  PERLIMTO       GAS4UPD 
02358            CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO  BISNDINO       GAS4UPD 
02359            FYIVALO   OVRDINDO  NEWVALUO  DEFINTNO  CONDLIFO       GAS4UPD 
02360            CONDALLO  CONDEXCO  CONDICDO  CONDTABO  CONDMENO       GAS4UPD 
02361         CONDDRGO  CONDALCO  CONDOBNO  CONDOBCO  CONDMALO CONDTMJO GAS4UPD 
02362         CONDCARO  CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO CONDINFO GAS4UPD 
02363         PRTIMEFO  INTRVALO  CONDPECO  CONDNEMO  NEWVALUO AGEQLLO  GAS4UPD 
02364         OENTCTRO  IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO AGEQLHO  GAS4UPD 
02365         IPGSSLTO                                                  GAS4UPD 
02366            RELPINDO  AGELIMLO  AGELIMHO IDGDSLTO  IPGPSLTO        GAS4UPD 
02367            FEAKINDO  ACCUMIDO  CAPINDO  SABDINDO                  GAS4UPD 
02367            BENTYPO   TIERCDO   TIERLVO.                           GAS4UPD 
02368                                                                   GAS4UPD 
02369      MOVE '01'    TO  COCURANO.                                   GAS4UPD 
02370      MOVE -1      TO  PERIODL.                                    GAS4UPD 
02371                                                                   GAS4UPD 
02372      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS4UPD 
02373                                                                   GAS4UPD 
02374  4100-900-EXIT. EXIT.                                             GAS4UPD 
02375 /*****************************************************************GAS4UPD 
02376 * 4400 BUILD DISPLAY                                             *GAS4UPD 
02377 *                                                                *GAS4UPD 
02378 *    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *GAS4UPD 
02379 *  SPECIFIED BY INDEX GAD-INDEX TO THE SCREEN.                   *GAS4UPD 
02380 ******************************************************************GAS4UPD 
02381  4400-000-BUILD-DISPLAY         SECTION.                          GAS4UPD 
02382  4400-010.                                                        GAS4UPD 
02383                                                                   GAS4UPD 
02384      IF  DELADDI  =  'CHG/DEL'                                    GAS4UPD 
02385      THEN                                                         GAS4UPD 
02386          MOVE 'D'  TO  DELOLITO                                   GAS4UPD 
02387      ELSE                                                         GAS4UPD 
02388          MOVE SPACE  TO  DELOLITO.                                GAS4UPD 
02389                                                                   GAS4UPD 
02390      MOVE SPACE                                       TO DELOPTNO.GAS4UPD 
02391      MOVE GAD-OCCURS-ENTRY-COUNTER     (GAD-INDEX)  TO            GAS4UPD 
02392                                                ACWA-DISPLAY-LEN-7.GAS4UPD 
02393      MOVE ACWA-DISPLAY-LEN-7                        TO  OENTCTRO. GAS4UPD 
02394      MOVE GAD-O-P-X-DAY-FACTOR-IND     (GAD-INDEX)  TO  DAYFACIO. GAS4UPD 
02395      MOVE GAD-O-P-X-CO-PAY-IND         (GAD-INDEX)  TO  COPAYINO. GAS4UPD 
02396      MOVE GAD-O-P-X-BISCENDING-IND     (GAD-INDEX)  TO  BISNDINO. GAS4UPD 
02397      MOVE GAD-CARRY-OVER-CREDIT-IND    (GAD-INDEX)  TO  CARYOVRO. GAS4UPD 
02398      MOVE GAD-O-P-X-ASCEND-DESCEND-IND (GAD-INDEX)  TO  ASCDSCDO. GAS4UPD 
      **P21595 CHANGES STARTS                                                   
02398      MOVE GAD-O-P-X-BEN-TYPE           (GAD-INDEX)  TO  BENTYPO.  GAS4UPD 
02398      MOVE GAD-O-P-X-TIER-CODE          (GAD-INDEX)  TO  TIERCDO.  GAS4UPD 
02398      MOVE GAD-O-P-X-TIER-LVL           (GAD-INDEX)  TO  TIERLVO.  GAS4UPD 
      **P21595 CHANGES ENDS                                                     
02399      MOVE GAD-O-P-X-DEFINITION         (GAD-INDEX)  TO  DEFINTNO. GAS4UPD 
02400      MOVE GAD-O-P-X-COST-CONTAIN-IND   (GAD-INDEX)  TO  CSTCONTO. GAS4UPD 
02401      MOVE GAD-O-P-X-BENEFIT-PERIOD     (GAD-INDEX)  TO  PERIODO.  GAS4UPD 
02402      MOVE GAD-O-P-X-BEN-PER-TIME-QUAL  (GAD-INDEX)  TO  PERTQALO. GAS4UPD 
02403      MOVE GAD-O-P-X-FAM-OR-INDIV       (GAD-INDEX)  TO  FAMINDIO. GAS4UPD 
02404      MOVE GAD-O-P-X-PLACE-OF-TREATMENT (GAD-INDEX)  TO  PLCTRMTO. GAS4UPD 
02405      MOVE GAD-O-P-X-SERVICE-GROUP      (GAD-INDEX)  TO  SRVGRUPO. GAS4UPD 
02406      MOVE GAD-O-P-X-RELATIONSHIP-IND   (GAD-INDEX)  TO  RELPINDO. GAS4UPD 
02407      MOVE GAD-O-P-X-AGE-QUAL-IND-FROM  (GAD-INDEX)  TO  AGEQLLO.  GAS4UPD 
02408      MOVE GAD-O-P-X-AGE-QUAL-IND-TO    (GAD-INDEX)  TO  AGEQLHO.  GAS4UPD 
02409      MOVE GAD-O-P-X-AGE-LIMIT-FROM     (GAD-INDEX)  TO            GAS4UPD 
02410                                                ACWA-DISPLAY-LEN-3.GAS4UPD 
02411      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMLO. GAS4UPD 
02412      MOVE GAD-O-P-X-AGE-LIMIT-TO       (GAD-INDEX)  TO            GAS4UPD 
02413                                                ACWA-DISPLAY-LEN-3.GAS4UPD 
02414      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMHO. GAS4UPD 
02415                                                                   GAS4UPD 
02416      MOVE GAD-O-P-X-ACCUMID            (GAD-INDEX)  TO  ACCUMIDO. GAS4UPD 
02417      MOVE GAD-O-P-X-COMB-APPLIED-IND   (GAD-INDEX)  TO  CAPINDO.  GAS4UPD 
02418      MOVE GAD-O-P-X-SEL-ADDL-BEN-DET   (GAD-INDEX)  TO  SABDINDO. GAS4UPD 
02419      MOVE GAD-O-P-X-BEN-PER-TIME-FCTR  (GAD-INDEX)  TO            GAS4UPD 
02420                                                ACWA-DISPLAY-LEN-3.GAS4UPD 
02421      MOVE ACWA-DISPLAY-LEN-3                        TO  PRTIMEFO. GAS4UPD 
02422      MOVE GAD-O-P-X-CLAIM-LVL-ACCUM-IND (GAD-INDEX) TO  CLMLVLIO. GAS4UPD 
02423      MOVE GAD-O-P-X-INTERVAL-TIME-FCTR (GAD-INDEX)  TO            GAS4UPD 
02424                                                ACWA-DISPLAY-LEN-3.GAS4UPD 
02425      MOVE ACWA-DISPLAY-LEN-3                        TO  INTRVALO. GAS4UPD 
02426      MOVE GAD-O-P-X-INTERVAL-TYPE      (GAD-INDEX)  TO  INTTYPEO. GAS4UPD 
02427      MOVE GAD-O-P-X-L-O-B              (GAD-INDEX)  TO  LOBO.     GAS4UPD 
02428      MOVE GAD-O-P-X-VALUE-LIMIT        (GAD-INDEX)  TO            GAS4UPD 
02429                                                ACWA-VALUE-LIMIT-9.GAS4UPD 
02430      IF ACWA-VALUE-LIMIT-9-9  =  -1                               GAS4UPD 
02431      THEN                                                         GAS4UPD 
02432          MOVE 'NEG'  TO  BNMXVALO                                 GAS4UPD 
02433      ELSE                                                         GAS4UPD 
02430      IF ACWA-VALUE-LIMIT-9-9  =  -2                               GAS4UPD 
02431      THEN                                                         GAS4UPD 
02432          MOVE 'UNL'  TO  BNMXVALO                                 GAS4UPD 
02433      ELSE                                                         GAS4UPD 
02434          IF  GAD-O-P-X-VALUE-QUALIFIER (GAD-INDEX)  =  '5'        GAS4UPD 
02435          THEN                                                     GAS4UPD 
02436              MOVE ACWA-VALUE-LIMIT-9     TO  ACWA-EDIT-VALUE-LIMITGAS4UPD 
02437              MOVE ACWA-EDIT-VALUE-LIMIT  TO  BNMXVALO             GAS4UPD 
02438          ELSE                                                     GAS4UPD 
02439              MOVE GAD-O-P-X-VALUE-LIMIT(GAD-INDEX)  TO            GAS4UPD 
02440                                               ACWA-DISPLAY-LEN-9-2GAS4UPD 
02441              MOVE ACWA-DISPLAY-LEN-9-X   TO  ACWA-DISPLAY-9       GAS4UPD 
02442              MOVE SPACES                 TO  ACWA-DISPLAY-1       GAS4UPD 
02443              MOVE ACWA-DISPLAY-VALUE-LIMIT  TO  BNMXVALO.         GAS4UPD 
02444                                                                   GAS4UPD 
02445      MOVE GAD-O-P-X-PERCENT-LEVEL     (GAD-INDEX)  TO             GAS4UPD 
02446                                                ACWA-DISPLAY-LEN-3.GAS4UPD 
02447      MOVE ACWA-DISPLAY-LEN-3                       TO  PERLIMTO.  GAS4UPD 
02448      MOVE GAD-O-P-X-VALUE-QUALIFIER   (GAD-INDEX)  TO  BENVLQLO.  GAS4UPD 
02449      MOVE GAD-O-P-X-INTERVAL-OVRD-VALUE(GAD-INDEX)  TO            GAS4UPD 
02450                                                ACWA-DISPLAY-LEN-5.GAS4UPD 
02451      MOVE ACWA-DISPLAY-LEN-5                       TO  NEWVALUO.  GAS4UPD 
02452      MOVE GAD-O-P-X-INTERVAL-OVRD-IND (GAD-INDEX)  TO  OVRDINDO.  GAS4UPD 
02453      MOVE GAD-O-P-X-INTERNAL-DESCRIPTOR(GAD-INDEX) TO  INTDESKO.  GAS4UPD 
02454      MOVE GAD-COND-ALL-BIT            (GAD-INDEX)  TO  CONDALLO.  GAS4UPD 
02455      MOVE GAD-COND-EXCLUSION-BIT      (GAD-INDEX)  TO  CONDEXCO.  GAS4UPD 
02456      MOVE GAD-COND-ICD-BIT            (GAD-INDEX)  TO  CONDICDO.  GAS4UPD 
02457      MOVE GAD-COND-TB-BIT             (GAD-INDEX)  TO  CONDTABO.  GAS4UPD 
02458      MOVE GAD-COND-MENTAL-BIT         (GAD-INDEX)  TO  CONDMENO.  GAS4UPD 
02459      MOVE GAD-COND-DRUG-BIT           (GAD-INDEX)  TO  CONDDRGO.  GAS4UPD 
02460      MOVE GAD-COND-ALCOHOL-BIT        (GAD-INDEX)  TO  CONDALCO.  GAS4UPD 
02461      MOVE GAD-COND-OB-COMP-BIT        (GAD-INDEX)  TO  CONDOBCO.  GAS4UPD 
02462      MOVE GAD-COND-OB-NORM-BIT        (GAD-INDEX)  TO  CONDOBNO.  GAS4UPD 
02463      MOVE GAD-COND-MALIGNANCY-BIT     (GAD-INDEX)  TO  CONDMALO.  GAS4UPD 
02464      MOVE GAD-COND-CARDIAC-DISEASE-BIT(GAD-INDEX)  TO  CONDCARO.  GAS4UPD 
02465      MOVE GAD-COND-OBESITY-BIT        (GAD-INDEX)  TO  CONDOBSO.  GAS4UPD 
02466      MOVE GAD-COND-KIDNEY-DISEASE-BIT (GAD-INDEX)  TO  CONDKDYO.  GAS4UPD 
02467      MOVE GAD-COND-ACCIDENT-BIT       (GAD-INDEX)  TO  CONDACCO.  GAS4UPD 
02468      MOVE GAD-COND-PRE-EXIST-BIT      (GAD-INDEX)  TO  CONDPECO.  GAS4UPD 
02469      MOVE GAD-COND-NON-EMER-BIT       (GAD-INDEX)  TO  CONDNEMO.  GAS4UPD 
02470      MOVE GAD-COND-SUICIDE-BIT        (GAD-INDEX)  TO  CONDSUIO.  GAS4UPD 
02471      MOVE GAD-COND-TMJ-BIT            (GAD-INDEX)  TO  CONDTMJO.  GAS4UPD 
02472      MOVE GAD-COND-INF-BIT            (GAD-INDEX)  TO  CONDINFO.  GAS4UPD 
02473      MOVE GAD-COND-LIFE-THREAT-BIT    (GAD-INDEX)  TO  CONDLIFO.  GAS4UPD 
02474                                                                   GAS4UPD 
02475      MOVE -1  TO  PERIODL.                                        GAS4UPD 
02476                                                                   GAS4UPD 
02477      MOVE GAD-O-P-X-FYI-VALUE(GAD-INDEX)   TO  FYIVALO.           GAS4UPD 
02478      SET  CURNT-OCURS-BIN        TO  GAD-INDEX.                   GAS4UPD 
02479      MOVE CURNT-OCURS-BIN        TO  CURNT-OCURS-PKD.             GAS4UPD 
02480      MOVE CURNT-OCCURS-OUT       TO  COCURANO.                    GAS4UPD 
02481                                                                   GAS4UPD 
02482      IF GAD-ENTRY-COUNT  >  1                                     GAS4UPD 
02483      THEN                                                         GAS4UPD 
02484          COMPUTE  TOTAL-OCURS-UNK  =  GAD-ENTRY-COUNT  - 1        GAS4UPD 
02485          MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                     GAS4UPD 
02486      ELSE                                                         GAS4UPD 
02487          MOVE  '01'              TO  TOCURANO.                    GAS4UPD 
02488                                                                   GAS4UPD 
02489                                                                   GAS4UPD 
02490      MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              GAS4UPD 
02491                       IDGDSLTO,  IPGPSLTO,  IPGSSLTO.             GAS4UPD 
02492                                                                   GAS4UPD 
02493      SET GAD-INT-INDEX  TO       1.                               GAS4UPD 
02494      SET GAD-INT-INDEX  DOWN BY  1.                               GAS4UPD 
02495                                                                   GAS4UPD 
02496  4400-300-DISPLAY-LOOP.                                           GAS4UPD 
02497                                                                   GAS4UPD 
02498      SET GAD-INT-INDEX  UP BY  1.                                 GAS4UPD 
02499      IF  GAD-INT-INDEX  >  5                                      GAS4UPD 
02500          GO TO 4400-800-SEND.                                     GAS4UPD 
02501                                                                   GAS4UPD 
02502      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  =  HIGH-VALUES      GAS4UPD 
02503          GO TO 4400-800-SEND.                                     GAS4UPD 
02504                                                                   GAS4UPD 
02505      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)   =   '#IBGR '       GAS4UPD 
02506          MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)               GAS4UPD 
02507                                   TO  ACWA-DISPLAY-LEN-7          GAS4UPD 
02508          MOVE ACWA-DISPLAY-LEN-7  TO  IBGRSLTO                    GAS4UPD 
02509          GO TO 4400-300-DISPLAY-LOOP.                             GAS4UPD 
02510                                                                   GAS4UPD 
02511      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)   =   '#IDGD '       GAS4UPD 
02512          MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)               GAS4UPD 
02513                                   TO  ACWA-DISPLAY-LEN-7          GAS4UPD 
02514          MOVE ACWA-DISPLAY-LEN-7  TO  IDGDSLTO                    GAS4UPD 
02515          GO TO 4400-300-DISPLAY-LOOP.                             GAS4UPD 
02516                                                                   GAS4UPD 
02517      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)   =   '#IPGN '       GAS4UPD 
02518          MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)               GAS4UPD 
02519                                   TO  ACWA-DISPLAY-LEN-7          GAS4UPD 
02520          MOVE ACWA-DISPLAY-LEN-7  TO  IPGNSLTO                    GAS4UPD 
02521          GO TO 4400-300-DISPLAY-LOOP.                             GAS4UPD 
02522                                                                   GAS4UPD 
02523      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)   =   '#IPGP '       GAS4UPD 
02524          MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)               GAS4UPD 
02525                                   TO  ACWA-DISPLAY-LEN-7          GAS4UPD 
02526          MOVE ACWA-DISPLAY-LEN-7  TO  IPGPSLTO                    GAS4UPD 
02527          GO TO 4400-300-DISPLAY-LOOP.                             GAS4UPD 
02528                                                                   GAS4UPD 
02529      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)   =   '#IPGT '       GAS4UPD 
02530          MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)               GAS4UPD 
02531                                   TO  ACWA-DISPLAY-LEN-7          GAS4UPD 
02532          MOVE ACWA-DISPLAY-LEN-7  TO  IPGTSLTO                    GAS4UPD 
02533          GO TO 4400-300-DISPLAY-LOOP.                             GAS4UPD 
02534                                                                   GAS4UPD 
02535      IF  GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)   =   '#IPGS '       GAS4UPD 
02536          MOVE GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)               GAS4UPD 
02537                                   TO  ACWA-DISPLAY-LEN-7          GAS4UPD 
02538          MOVE ACWA-DISPLAY-LEN-7  TO  IPGSSLTO                    GAS4UPD 
02539          GO TO 4400-300-DISPLAY-LOOP.                             GAS4UPD 
02540                                                                   GAS4UPD 
02541      MOVE WS-ABCODE-1EF1       TO  WS-ABCODE                      GAS4UPD 
02542      MOVE WS-ABCODE-1EF1-MSG   TO  WS-ABCODE-MSG                  GAS4UPD 
02543      PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       GAS4UPD 
02544                                                                   GAS4UPD 
02545                                                                   GAS4UPD 
02546  4400-800-SEND.                                                   GAS4UPD 
02547                                                                   GAS4UPD 
02548      PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      GAS4UPD 
02549                                                                   GAS4UPD 
02550      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS4UPD 
02551                                                                   GAS4UPD 
02552  4400-900-EXIT. EXIT.                                             GAS4UPD 
02553 /*****************************************************************GAS4UPD 
02554 *  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *GAS4UPD 
02555 *                                                                *GAS4UPD 
02556 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS4UPD 
02557 *          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *GAS4UPD 
02558 *          2. IF GROUP IS CRITICAL:                              *GAS4UPD 
02559 *              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *GAS4UPD 
02560 *                BENEFIT PROVISION.                              *GAS4UPD 
02561 *                - IF ON DATA BASE:                              *GAS4UPD 
02562 *                  - SCAN FOR #AOL TABULAR                       *GAS4UPD 
02563 *                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *GAS4UPD 
02564 *                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *GAS4UPD 
02565 *                      ON SCREEN AND ISSUE MESSAGE.              *GAS4UPD 
02566 ******************************************************************GAS4UPD 
02567  4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          GAS4UPD 
02568  4500-010.                                                        GAS4UPD 
02569                                                                   GAS4UPD 
02570      IF DELADDI  =  'CHG/DEL'    OR                               GAS4UPD 
02571         DELOLITI =  SPACES                                        GAS4UPD 
02572      THEN                                                         GAS4UPD 
02573         NEXT SENTENCE                                             GAS4UPD 
02574      ELSE                                                         GAS4UPD 
02575         GO TO 4500-900-EXIT.                                      GAS4UPD 
02576                                                                   GAS4UPD 
02577      MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. GAS4UPD 
02578                                                                   GAS4UPD 
02579      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS4UPD 
02580                 COMMAREA (DFHCOMMAREA)                            GAS4UPD 
02581                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS4UPD 
02582                                                                   GAS4UPD 
02583                                                                   GAS4UPD 
02584  4500-900-EXIT.   EXIT.                                           GAS4UPD 
02585                                                                   GAS4UPD 
02586 /*****************************************************************GAS4UPD 
02587 *  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *GAS4UPD 
02588 *                                                                *GAS4UPD 
02589 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS4UPD 
02590 *           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *GAS4UPD 
02591 *           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *GAS4UPD 
02592 *               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *GAS4UPD 
02593 *           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *GAS4UPD 
02594 *               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *GAS4UPD 
02595 *               +CDE+ INDICATOR (POSITION=8).                    *GAS4UPD 
02596 *              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *GAS4UPD 
02597 *               ENTER, CONTINUE PROCESSING.                      *GAS4UPD 
02598 ******************************************************************GAS4UPD 
02599  4600-000-UPDATE-CDE-STATUS     SECTION.                          GAS4UPD 
02600  4600-010.                                                        GAS4UPD 
02601                                                                   GAS4UPD 
02602                                                                   GAS4UPD 
02603      SET  ACWA-INDEX-1    TO  GAD-INDEX.                          GAS4UPD 
02604      MOVE WS-REQUEST-4600-CDE-STATUS  TO  ACWA-CDE-REQUEST-CODE.  GAS4UPD 
02605                                                                   GAS4UPD 
02606      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS4UPD 
02607                 COMMAREA (DFHCOMMAREA)                            GAS4UPD 
02608                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS4UPD 
02609                                                                   GAS4UPD 
02610      IF  ACWA-CDE-RETURN-DONT-SEND                                GAS4UPD 
02611          EXEC CICS  RETURN   END-EXEC.                            GAS4UPD 
02612                                                                   GAS4UPD 
02613  4600-900-EXIT.   EXIT.                                           GAS4UPD 
02614                                                                   GAS4UPD 
02615 /*****************************************************************GAS4UPD 
02616 *  4700  -  UPDATE W/F CONTROL RECORD                            *GAS4UPD 
02617 *                                                                *GAS4UPD 
02618 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS4UPD 
02619 *          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *GAS4UPD 
02620 *             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *GAS4UPD 
02621 *             RECORD AND THE INTERNAL TABULAR RECORDS; IF THE    *GAS4UPD 
02622 *             CDE STATUS HAS CHANGED.                            *GAS4UPD 
02623 *          2. REWRITE W/F CONTROL RECORD                         *GAS4UPD 
02624 ******************************************************************GAS4UPD 
02625  4700-000-UPDATE-CONTROL-RECORD SECTION.                          GAS4UPD 
02626  4700-010.                                                        GAS4UPD 
02627                                                                   GAS4UPD 
02628      SET  ACWA-INDEX-1     TO  GAD-INDEX.                         GAS4UPD 
02629      MOVE WS-REQUEST-4700-CNTL-UPDATE  TO  ACWA-CDE-REQUEST-CODE. GAS4UPD 
02630                                                                   GAS4UPD 
02631      EXEC CICS  LINK   PROGRAM ('GACDEPGM')                       GAS4UPD 
02632                 COMMAREA (DFHCOMMAREA)                            GAS4UPD 
02633                 LENGTH(LENGTH OF DFHCOMMAREA)      END-EXEC.      GAS4UPD 
02634                                                                   GAS4UPD 
02635  4700-900-EXIT.  EXIT.                                            GAS4UPD 
02636                                                                   GAS4UPD 
02637 ******************************************************************GAS4UPD 
02638 *  4900  -  R E S E T   O T H E R   I N T E R N A L   T A B S     GAS4UPD 
02639 *                                                                 GAS4UPD 
02640 *    FUNCTION  (VIA CDE MODULE GACDEPGM)                          GAS4UPD 
02641 *          READ W/F ACCUM'S INTERNAL TABULAR RECORDS, THOSE ON    GAS4UPD 
02642 *          W/F ONLY.  RESET THE CDE STATUS INDICATOR ON THIS      GAS4UPD 
02643 *          INTERNAL TO EITHER 1U OR 2 BASED ON THE INTERNAL       GAS4UPD 
02644 *          DESCRIPTOR, THEN REWRITE THIS RECORD.                  GAS4UPD 
02645 ******************************************************************GAS4UPD 
02646  4900-RESET-OTHER-INT-TABS      SECTION.                          GAS4UPD 
02647                                                                   GAS4UPD 
02648      MOVE ZERO  TO  WS-INTRNL-TABS-TO-CHG-CNT.                    GAS4UPD 
02649      PERFORM 4900-COUNT-INT-TAB                                   GAS4UPD 
02650         VARYING GAD-INT-INDEX  FROM  1  BY  1                     GAS4UPD 
02651         UNTIL GAD-INT-INDEX  NOT <                                GAS4UPD 
02652                          GAD-INTERNAL-TABULAR-COUNT(GAD-INDEX)  ORGAS4UPD 
02653               GAD-INT-ID(GAD-INDEX GAD-INT-INDEX)  =  HIGH-VALUES.GAS4UPD 
02654                                                                   GAS4UPD 
02655      IF WS-INTRNL-TABS-TO-CHG-CNT  >  ZERO                        GAS4UPD 
02656         MOVE WS-REQUEST-4900-CNTL-UPDATE  TO                      GAS4UPD 
02657                                            ACWA-CDE-REQUEST-CODE  GAS4UPD 
02658         EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                   GAS4UPD 
02659                    COMMAREA (DFHCOMMAREA)                         GAS4UPD 
02660                    LENGTH(LENGTH OF DFHCOMMAREA)        END-EXEC. GAS4UPD 
02661                                                                   GAS4UPD 
02662      GO TO 4999-EXIT.                                             GAS4UPD 
02663                                                                   GAS4UPD 
02664  4900-COUNT-INT-TAB.                                              GAS4UPD 
02665      IF GAD-INT-SLOT(GAD-INDEX GAD-INT-INDEX)  >  +8999999        GAS4UPD 
02666         ADD 1  TO  WS-INTRNL-TABS-TO-CHG-CNT.                     GAS4UPD 
02667                                                                   GAS4UPD 
02668  4999-EXIT.       EXIT.                                           GAS4UPD 
02669 /*****************************************************************GAS4UPD 
02670 *     READ ALL LEVEL TABULAR FROM PROVISION POOL                  GAS4UPD 
02671 *                                                                 GAS4UPD 
02672 ******************************************************************GAS4UPD 
02673  5000-000-READ-PROD-ALL-LVL-TAB  SECTION.                         GAS4UPD 
02674                                                                   GAS4UPD 
02675      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   GAS4UPD 
02676               GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-AOL-FIXED-LEN  +GAS4UPD 
02677              (GC-GCTABULR-AOL-VARY-LEN  *                         GAS4UPD 
02678                                    GC-GCTABULR-AOL-VARY-MAX-OCUR).GAS4UPD 
02679                                                                   GAS4UPD 
02680      IF ACWA-PR-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS4UPD 
02681         NEXT SENTENCE                                             GAS4UPD 
02682      ELSE                                                         GAS4UPD 
02683         EXEC CICS GETMAIN                                         GAS4UPD 
02684                SET(ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD)      GAS4UPD 
02685                INITIMG(WS-HEX-00)                                 GAS4UPD 
02686                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS4UPD 
02687                END-EXEC                                           GAS4UPD 
02688         SET ACWA-PR-ALL-LEVEL-TAB-PNTR     TO                     GAS4UPD 
02689                  ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD.        GAS4UPD 
02690                                                                   GAS4UPD 
02691                                                                   GAS4UPD 
02692      MOVE GCIO-WORKFILE-KEY       TO WS-SV-RESTO-KY.              GAS4UPD 
02693      MOVE TABIDI                  TO  GCIO-TAB-TABULAR-ID.        GAS4UPD 
02694      MOVE WRK-TAB-PROV-COPY-SLOT  TO  GCIO-TAB-SLOT-NO.           GAS4UPD 
02695      MOVE GCIO-WORKFILE-KEY       TO  GCIOA-FILE-KEY.             GAS4UPD 
02696      MOVE GC-GCIO-AREA-2          TO  GCIOA-IO-AREA-TO-USE.       GAS4UPD 
02697      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIOA-FILE-ACCESS-CODE.     GAS4UPD 
02698      MOVE GC-GCTABULR-DDNAME      TO  GCIOA-FILE-DDNAME.          GAS4UPD 
02699      MOVE GC-GCTABULR-AOL-VARY-MAX-OCUR                           GAS4UPD 
02700           TO  GAD2-ENTRY-COUNT.                                   GAS4UPD 
02701                                                                   GAS4UPD 
02702      MOVE WS-SV-RESTO-KY    TO  GCIO-WORKFILE-KEY.                GAS4UPD 
02703                                                                   GAS4UPD 
02704      IF GCIO-TAB-TABULAR-ID  =  GAD2-PROVISION-ID AND             GAS4UPD 
02705         GAD2-PROVISION-SLOT-NO  NUMERIC AND                       GAS4UPD 
02706         GCIO-TAB-SLOT-NO     =  GAD2-PROVISION-SLOT-NO            GAS4UPD 
02707         GO TO 5000-900-EXIT.                                      GAS4UPD 
02708                                                                   GAS4UPD 
02709      EXEC CICS  LINK   PROGRAM  ('GCIOPGM')                       GAS4UPD 
02710                 COMMAREA (PR-IO-PARM-ALL-LVL-TAB-RECORD)          GAS4UPD 
02711                 LENGTH (WS-IO-PARM-WRK-ALL-LVL-LEN)    END-EXEC.  GAS4UPD 
02712                                                                   GAS4UPD 
02713      IF NOT GCIOA-GOOD-RETURN                                     GAS4UPD 
02714         MOVE WS-ABCODE-1EF7        TO  WS-ABCODE                  GAS4UPD 
02715         MOVE WS-ABCODE-1EF7-MSG    TO  WS-ABCODE-MSG              GAS4UPD 
02716         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS4UPD 
02717                                                                   GAS4UPD 
02718  5000-900-EXIT.     EXIT.                                         GAS4UPD 
02719                                                                   GAS4UPD 
02720                                                                   GAS4UPD 
02721 /*****************************************************************GAS4UPD 
02722 * 6000  BUILD GROUP SPEC KEY                                     *GAS4UPD 
02723 *                                                                *GAS4UPD 
02724 *    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *GAS4UPD 
02725 ******************************************************************GAS4UPD 
02726  6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          GAS4UPD 
02727  6000-010.                                                        GAS4UPD 
02728                                                                   GAS4UPD 
02729      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS4UPD 
02730      MOVE  'G'                 TO  GCIO-WRK-STATUS-CODE.          GAS4UPD 
02731      MOVE  'G3'                TO  GCIO-WRK-RECORD-TYPE.          GAS4UPD 
02732      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS4UPD 
02733      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS4UPD 
02734      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS4UPD 
02735      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS4UPD 
02736      MOVE SPACES               TO  GCIO-WRK-LINE-OF-BUS,          GAS4UPD 
02737                                    GCIO-WRK-PROVIDER-CONTROL.     GAS4UPD 
02738      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS4UPD 
02739      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS4UPD 
02740      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS4UPD 
02741      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS4UPD 
02742      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS4UPD 
02743      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS4UPD 
02744      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS4UPD 
02745                                                                   GAS4UPD 
02746  6000-900-EXIT. EXIT.                                             GAS4UPD 
02747                                                                   GAS4UPD 
02748 ******************************************************************GAS4UPD 
02749 * 6100  BUILD CONTRACT KEY                                       *GAS4UPD 
02750 *                                                                *GAS4UPD 
02751 *    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *GAS4UPD 
02752 ******************************************************************GAS4UPD 
02753  6100-000-BUILD-CONTRACT-KEY    SECTION.                          GAS4UPD 
02754  6100-010.                                                        GAS4UPD 
02755                                                                   GAS4UPD 
02756      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS4UPD 
02757      MOVE  'C'                 TO  GCIO-WRK-STATUS-CODE.          GAS4UPD 
02758      MOVE  'C3'                TO  GCIO-WRK-RECORD-TYPE.          GAS4UPD 
02759      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS4UPD 
02760      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS4UPD 
02761      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS4UPD 
02762      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS4UPD 
02763      MOVE GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS.          GAS4UPD 
02764      MOVE GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL.     GAS4UPD 
02765      MOVE GCA-FAM-REL-LVL      TO  GCIO-WRK-FAMILY-RELATION-LVL.  GAS4UPD 
02766      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS4UPD 
02767      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS4UPD 
02768      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS4UPD 
02769      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS4UPD 
02770      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS4UPD 
02771      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS4UPD 
02772  6100-900-EXIT. EXIT.                                             GAS4UPD 
02773                                                                   GAS4UPD 
02774 /*****************************************************************GAS4UPD 
02775 * 6200  BUILD BEN PROV KEY                                       *GAS4UPD 
02776 *                                                                *GAS4UPD 
02777 *    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *GAS4UPD 
02778 ******************************************************************GAS4UPD 
02779  6200-000-BUILD-BEN-PROV-KEY    SECTION.                          GAS4UPD 
02780  6200-010.                                                        GAS4UPD 
02781                                                                   GAS4UPD 
02782      MOVE SPACES                TO  GCIO-WORKFILE-KEY.            GAS4UPD 
02783      MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE.         GAS4UPD 
02784      MOVE  'C5'                 TO  GCIO-WRK-RECORD-TYPE.         GAS4UPD 
02785      MOVE GCA-PLAN-CODE         TO  GCIO-WRK-PLAN-CODE.           GAS4UPD 
02786      MOVE GCA-GROUP-NUM         TO  GCIO-WRK-GROUP-NUM.           GAS4UPD 
02787      MOVE GCA-SECTION-NUM       TO  GCIO-WRK-SECTION-NUM.         GAS4UPD 
02788      MOVE GCA-PKG-CODE          TO  GCIO-WRK-PKG-CODE.            GAS4UPD 
02789      MOVE GCA-L-O-B             TO  GCIO-WRK-LINE-OF-BUS.         GAS4UPD 
02790      MOVE GCA-PROV-CTL          TO  GCIO-WRK-PROVIDER-CONTROL.    GAS4UPD 
02791      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS4UPD 
02792      MOVE GCA-EFFDT-CEN         TO  GCIO-WRK-EFFDT-CEN.           GAS4UPD 
02793      MOVE GCA-BEN-PROV-ID       TO  GCIO-WRK-PROVISION-ID.        GAS4UPD 
02794      MOVE +9999999              TO  GCIO-WRK-PROVISION-SLOT-NO.   GAS4UPD 
02795      MOVE TABIDI                TO  GCIO-WRK-TAB-PROVISION-ID.    GAS4UPD 
02796      MOVE TABSLTNI              TO  ACWA-DISPLAY-LEN-7.           GAS4UPD 
02797      MOVE ACWA-DISPLAY-LEN-7    TO  GCIO-WRK-TAB-PROV-SLOT-NO.    GAS4UPD 
02798                                                                   GAS4UPD 
02799  6200-900-EXIT. EXIT.                                             GAS4UPD 
02800                                                                   GAS4UPD 
02801 /*****************************************************************GAS4UPD 
02802 *  XCTL TO MAIN MENU                                             *GAS4UPD 
02803 *                                                                *GAS4UPD 
02804 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *GAS4UPD 
02805 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*GAS4UPD 
02806 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUSGAS4UPD 
02807 *  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GETGAS4UPD 
02808 *  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *GAS4UPD 
02809 ******************************************************************GAS4UPD 
02810  6400-000-XCTL-TO-MAIN-MENU     SECTION.                          GAS4UPD 
02811  6400-010.                                                        GAS4UPD 
02812                                                                   GAS4UPD 
02813      MOVE WS-ABCODE-1EP1       TO  WS-ABCODE.                     GAS4UPD 
02814      MOVE WS-ABCODE-1EP1-MSG   TO  WS-ABCODE-MSG.                 GAS4UPD 
02815                                                                   GAS4UPD 
02816      EXEC CICS  XCTL   PROGRAM('GCPSPGM')   END-EXEC.             GAS4UPD 
02817                                                                   GAS4UPD 
02818  6400-900-EXIT. EXIT.                                             GAS4UPD 
02819                                                                   GAS4UPD 
02820 **************************************************************    GAS4UPD 
02821 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS4UPD 
02822 * - DEAD CODE ELIMINATION.                                        GAS4UPD 
02823 * - REMOVED: 7000-000-PRINT-HARDCOPY        SECTION.              GAS4UPD 
02824 **************************************************************    GAS4UPD 
02825                                                                   GAS4UPD 
02826 /*****************************************************************GAS4UPD 
02827 * 7900  R E S E T   A T T R I B U T E S                          *GAS4UPD 
02828 ******************************************************************GAS4UPD 
02829  7900-000-RESET-ATTRIBUTES      SECTION.                          GAS4UPD 
02830  7900-010.                                                        GAS4UPD 
02831                                                                   GAS4UPD 
02832      MOVE DFHBMUNF  TO  BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA    GAS4UPD 
02833               LOBA      PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA    GAS4UPD 
02834               CARYOVRA  PERLIMTA  INTRVALA  INTTYPEA  CLMLVLIA    GAS4UPD 
02835               BNMXVALA  DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA    GAS4UPD 
02836               FYIVALA   CONDALLA  CONDEXCA  CONDICDA  CONDTABA    GAS4UPD 
02837               CONDMENA  CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA    GAS4UPD 
02838               CONDMALA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA    GAS4UPD 
02839               CONDPECA  CONDNEMA  CONDSUIA  DEFINTNA  ASCDSCDA    GAS4UPD 
02840               IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA    GAS4UPD 
02841               IDGDOPTA  IPGPOPTA  RELPINDA  AGELIMLA AGELIMHA     GAS4UPD 
02842               CONDLIFA  IPGSOPTA                                  GAS4UPD 
02843               BISNDINA  CONDTMJA CONDINFA  AGEQLLA AGEQLHA        GAS4UPD 
02844               FEAKINDA  ACCUMIDA CAPINDA   SABDINDA               GAS4UPD 
02844               BENTYPA   TIERCDA  TIERLVA.                         GAS4UPD 
02845                                                                   GAS4UPD 
02846      IF DELADDO   =  'CHG/DEL'                                    GAS4UPD 
02847         NEXT SENTENCE                                             GAS4UPD 
02848      ELSE                                                         GAS4UPD 
02849         GO TO 7900-900-EXIT.                                      GAS4UPD 
02850                                                                   GAS4UPD 
02851                                                                   GAS4UPD 
02852      IF  CDEINDO  =  '+CDE+'                                      GAS4UPD 
02853      THEN                                                         GAS4UPD 
02854 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS4UPD 
02855          MOVE DFHBMABF TO DLOPTLTA  PERIOTA   BENVLQTA  LOTA      GAS4UPD 
02856                AGELIMA    PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  GAS4UPD 
02857                AGEQLTA    COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  GAS4UPD 
02858                           PERLITTA                                GAS4UPD 
02859      ELSE                                                         GAS4UPD 
02860          NEXT SENTENCE.                                           GAS4UPD 
02861                                                                   GAS4UPD 
02862                                                                   GAS4UPD 
02863      IF  CDEINDO  =  '+CDE-'                                      GAS4UPD 
02864      THEN                                                         GAS4UPD 
02865 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS4UPD 
02866          MOVE DFHBMABF TO DLOPTLTA  PERIOTA   BENVLQTA  LOTA      GAS4UPD 
02867                AGELIMA    PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  GAS4UPD 
02868                AGEQLTA    COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  GAS4UPD 
02869                           PERLITTA  BISNDITA                      GAS4UPD 
02870 *---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        GAS4UPD 
02871          MOVE DFHBMASF TO DELOPTNA  PERIODA   BENVLQLA  LOBA      GAS4UPD 
02872        AGELIMLA AGELIMHA  PLCTRMTA  FAMINDIA  SRVGRUPA  CSTCONTA  GAS4UPD 
02873         AGEQLLA  AGEQLHA  COPAYINA  INTDESKA  CONDALLA  CONDEXCA  GAS4UPD 
02874                           CONDICDA  CONDTABA  CONDMENA  CONDDRGA  GAS4UPD 
02875                           CONDALCA  CONDOBCA  CONDOBNA  CONDMALA  GAS4UPD 
02876                 CONDTMJA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  GAS4UPD 
02877                 CONDINFA  CONDPECA  CONDNEMA  CONDSUIA  BISNDINA  GAS4UPD 
02878                 PERLIMTA  CONDLIFA                                GAS4UPD 
                                                                                
02879          IF ERRMSGO   >  SPACES                                   GAS4UPD 
02880             NEXT SENTENCE                                         GAS4UPD 
02881          ELSE                                                     GAS4UPD 
02882             SET  WT-01-INDEX  TO  +08                             GAS4UPD 
02883             MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO     GAS4UPD 
02884      ELSE                                                         GAS4UPD 
02885         NEXT SENTENCE.                                            GAS4UPD 
02886                                                                   GAS4UPD 
02887  7900-900-EXIT. EXIT.                                             GAS4UPD 
02888                                                                   GAS4UPD 
02889 /*****************************************************************GAS4UPD 
02890 * 9000  S E N D   E R A S E   T H E N   R E T U R N               GAS4UPD 
02891 ******************************************************************GAS4UPD 
02892  9000-000-SEND-ERASE-RETURN     SECTION.                          GAS4UPD 
02893  9000-010.                                                        GAS4UPD 
02894                                                                   GAS4UPD 
02895      MOVE DFHBMASD  TO  MAXOVRTA  REININTA  MANAPLTA  FDLRCLTA    GAS4UPD 
02896                         MAXOVRDA  REININDA  MANAPLIA  FDLRCLIA    GAS4UPD 
02897                         TIMEDLRA  TIMEDOLA.                       GAS4UPD 
02898                                                                   GAS4UPD 
02899      MOVE -1  TO  ERRMSGL.                                        GAS4UPD 
02900                                                                   GAS4UPD 
02901      EXEC CICS  SEND   MAP ('GA1XI01')    ERASE  CURSOR           GAS4UPD 
02902                 MAPSET('GA1XSET')    END-EXEC.                    GAS4UPD 
02903                                                                   GAS4UPD 
02904      EXEC CICS  RETURN   END-EXEC.                                GAS4UPD 
02905                                                                   GAS4UPD 
02906  9000-900-EXIT.    EXIT.                                          GAS4UPD 
02907                                                                   GAS4UPD 
02908 /*****************************************************************GAS4UPD 
02909 * 9010  S E N D   D A T A O N L Y   A N D   T H E N   R E T U R N GAS4UPD 
02910 ******************************************************************GAS4UPD 
02911  9010-000-SEND-DATAONLY-RETURN  SECTION.                          GAS4UPD 
02912  9010-010.                                                        GAS4UPD 
02913                                                                   GAS4UPD 
02914      MOVE -1  TO  ERRMSGL.                                        GAS4UPD 
02915                                                                   GAS4UPD 
02916      EXEC CICS  SEND   MAP('GA1XI01')  DATAONLY  CURSOR           GAS4UPD 
02917                 MAPSET('GA1XSET')       END-EXEC.                 GAS4UPD 
02918                                                                   GAS4UPD 
02919      EXEC CICS  RETURN   END-EXEC.                                GAS4UPD 
02920                                                                   GAS4UPD 
02921  9010-900-EXIT.     EXIT.                                         GAS4UPD 
02922                                                                   GAS4UPD 
02923 **************************************************************    GAS4UPD 
02924 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS4UPD 
02925 * - DEAD CODE ELIMINATION.                                        GAS4UPD 
02926 * - REMOVED: 9200-000-GREGORIAN-TO-JULIAN   SECTION.              GAS4UPD 
02927 *            9300-000-JULIAN-TO-GREGORIAN   SECTION.              GAS4UPD 
02928 **************************************************************    GAS4UPD 
02929                                                                   GAS4UPD 
02930 ******************************************************************GAS4UPD 
02931 * 9800  E R R O R   M S G   T H E N   A B E N D                  *GAS4UPD 
02932 *                                                                *GAS4UPD 
02933 *    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *GAS4UPD 
02934 *  AND THEN ABENDS USING THE ABEND CODE EARLIER MEFINED.         *GAS4UPD 
02935 ******************************************************************GAS4UPD 
02936  9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          GAS4UPD 
02937  9800-010.                                                        GAS4UPD 
02938                                                                   GAS4UPD 
02939      MOVE -1              TO  MFRMSLTL.                           GAS4UPD 
02940      MOVE WS-ABCODE-MSG   TO  ERRMSGO.                            GAS4UPD 
02941                                                                   GAS4UPD 
02942      EXEC CICS  SEND   MAP ('GA1XI01') ERASE  CURSOR   WAIT       GAS4UPD 
02943                 MAPSET('GA1XSET')      END-EXEC.                  GAS4UPD 
02944                                                                   GAS4UPD 
02945      EXEC CICS  ABEND   ABCODE(WS-ABCODE)   END-EXEC.             GAS4UPD 
02946                                                                   GAS4UPD 
02947  9800-900-EXIT. EXIT.                                             GAS4UPD 
