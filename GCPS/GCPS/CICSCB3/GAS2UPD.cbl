00001  IDENTIFICATION DIVISION.                                         02/18/05
00002  PROGRAM-ID. GAS2UPD.                                             GAS2UPD 
00003 **** THIS IS A COBOL/2 PROGRAM ***                                   LV005
00004  AUTHOR. N ELBAZ.                                                 GAS2UPD 
00005  DATE-WRITTEN.   07/15/87.                                        GAS2UPD 
00006  DATE-COMPILED.                                                   GAS2UPD 
00007      SKIP3                                                        GAS2UPD 
00008 ******************************************************************GAS2UPD 
00009 *   GAS2UPD         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *GAS2UPD 
00010 *       CONINSURANCE LIMITS ACCULATOR DESCRIPTOR TABULAR - #ACL  *GAS2UPD 
00011 *                                                                *GAS2UPD 
00012 *     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *GAS2UPD 
00013 *   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *GAS2UPD 
00014 *   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *GAS2UPD 
00015 *   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *GAS2UPD 
00016 *   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *GAS2UPD 
00017 *   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*GAS2UPD 
00018 *                                                                *GAS2UPD 
00019 *     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *GAS2UPD 
00020 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *GAS2UPD 
00021 *   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*GAS2UPD 
00022 *   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *GAS2UPD 
00023 *   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *GAS2UPD 
00024 *   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *GAS2UPD 
00025 *                                                                *GAS2UPD 
00026 *    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*GAS2UPD 
00027 *   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*GAS2UPD 
00028 *   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *GAS2UPD 
00029 *   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *GAS2UPD 
00030 *   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *GAS2UPD 
00031 *   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *GAS2UPD 
00032 *                                                                *GAS2UPD 
00033 *   FUNC CODE: GAS2                                              *GAS2UPD 
00034 *                          ********************************      *GAS2UPD 
00035 *                          *   THIS MAPSET IS SHARED BY   *      *GAS2UPD 
00036 *                          *   THE FOLLOWING MODULES:     *      *GAS2UPD 
00037 *                          *   1. GA1BPGM                 *      *GAS2UPD 
00038 *                          *   2. GA1CPGM                 *      *GAS2UPD 
00039 *                          *   3. GA1DPGM                 *      *GAS2UPD 
00040 *   MAPSET:    GA1XSETC ==>*   4. GA1EPGM                 *      *GAS2UPD 
00041 *                          *   5. GASEDIT1                *      *GAS2UPD 
00042 *                          *   6. GACDEPGM                *      *GAS2UPD 
00043 *                          *   7. GK1BPGM                 *      *GAS2UPD 
00044 *                          *   8. GK1CPGM                 *      *GAS2UPD 
00045 *                          *   9. GK1DPGM                 *      *GAS2UPD 
00046 *                          *  10. GK1EPGM                 *      *GAS2UPD 
00047 *                          *  11. GAS1UPD                 *      *GAS2UPD 
00048 *                          *  12. GAS2UPD                 *      *GAS2UPD 
00049 *                          *  11. GAS3UPD                 *      *GAS2UPD 
00050 *                          *  11. GAS4UPD                 *      *GAS2UPD 
00051 *                          ********************************      *GAS2UPD 
00052 *                                                                *GAS2UPD 
00053 *   FILES:     GCPSWORK           GCGRPSPC                       *GAS2UPD 
00054 *              GCTABULR           GCSTABLR                       *GAS2UPD 
00055 *              GCCONTR            GCSPROVN                       *GAS2UPD 
00056 *                                                                *GAS2UPD 
00057 ******************************************************************GAS2UPD 
00058                                                                   GAS2UPD 
00059 /    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS2UPD 
00060 *    *-*         U P D A T E   H I S T O R Y         *-*          GAS2UPD 
00061 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS2UPD 
00062                                                                   GAS2UPD 
00063 *NUM-* *-DATE-* *WHO* *-----------DESCRIPTION--------------------*GAS2UPD 
00064 *                                                                *GAS2UPD 
00065 *                                                                *GAS2UPD 
00066 * D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *GAS2UPD 
00067 * D200 06/07/87  NGE  SEPERATE CHANGE/DELETE FUNCTION TO         *GAS2UPD 
00068 *                     ANOTHER MODULE.                            *GAS2UPD 
00069 * N121 08/10/87  NGE  ADD NEW FIELD -DEFINITION-                 *GAS2UPD 
00070 * N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT                  *GAS2UPD 
00071 *                                                                *GAS2UPD 
00072 *  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *GAS2UPD 
00073 *                             4600- SECTION THAT CAUSED THE CDE  *GAS2UPD 
00074 *                             MODIFIED STATUS TO BE SET.         *GAS2UPD 
00075 *                                                                *GAS2UPD 
00076 * D143 02/01/88  DES  CDE/NON-CDE CHANGES                        *GAS2UPD 
00077 *                                                                *GAS2UPD 
00078 *  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *GAS2UPD 
00079 *                              TO 'A'.                           *GAS2UPD 
00080 *                                                                *GAS2UPD 
00081 *  D1218     06/03/88  NGE  FIX NON-CDE OCCURS DELETE            *GAS2UPD 
00082 *                                                                *GAS2UPD 
00083 * ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC WILL FLAG THE   *GAS2UPD 
00084 *                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *GAS2UPD 
00085 *                            TAB LOGIC TO FLAG ITS ACCUM AS CDE  *GAS2UPD 
00086 *                            WHEN THE INTERNL REC FLAGED AS CDE. *GAS2UPD 
00087 *                                                                *GAS2UPD 
00088 * ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *GAS2UPD 
00089 *                        UPDATING CDE COUNTERS DEPENDING ON THE  *GAS2UPD 
00090 *                        INTRNL TAB RECORD, NOT THE CDE STATUS   *GAS2UPD 
00091 *                        IN THE ACCUM RECORD ATTACHED.           *GAS2UPD 
00092 *                                                                *GAS2UPD 
00093 *D???? 01/13/89  ENW   ADDED LOGIC FOR BISCENDING INDICATOR.     *GAS2UPD 
00094 *                                                                *GAS2UPD 
00095 * D200 05/18/89  NGE  ADD TWO NEW COND-BITS TMJ AND INF,         *GAS2UPD 
00096 *                     TEMPROMAND-JOINT AND INFERTILITY-COND.     *GAS2UPD 
00097 *                                                                *GAS2UPD 
00098 * 11154   10/15/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GAS2UPD 
00099 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GAS2UPD 
00100 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GAS2UPD 
00101 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GAS2UPD 
00102 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GAS2UPD 
00103 *                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*GAS2UPD 
00104 *                       7. >>> CONVERT TO COBOL/2 <<<.           *GAS2UPD 
00105 *                                                                *GAS2UPD 
00106 * 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *GAS2UPD 
00107 *                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *GAS2UPD 
00108 *                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*GAS2UPD 
00109 *                                                                *GAS2UPD 
00110 * 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *GAS2UPD 
00111 *                                                                *GAS2UPD 
00112 *D12009 09/16/91  BSO  CHANGES FOR FRL EXPANSION                 *GAS2UPD 
00113 *                                                                *GAS2UPD 
00114 * P-034  09/17/91  ENW  FIXED PROGRAM ERROR LEFT OVER FROM THE   *GAS2UPD 
00115 *                       11154 ACCUM EXPANSION. MOVED HIGH VALUES *GAS2UPD 
00116 *                       TO THE LAST OCCURS AFTER A DELETE OF AN  *GAS2UPD 
00117 *                       INTERNAL TABULAR.                        *GAS2UPD 
00118 *                                                                *GAS2UPD 
00119 * 12262  02/28/92 TPM ADD NEW COND-BIT LIF   (LIFE-THREATING)    *GAS2UPD 
00120 *                     COND-LIFE-THREAT-BIT                       *GAS2UPD 
00121 *                                                                *GAS2UPD 
00122 *  D303  02/03/97 DAU ADD FEAK INDICATOR                         *GAS2UPD 
00123 *                                                                *GAS2UPD 
00124 * 14726/ 11/10/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *GAS2UPD 
00125 * 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *GAS2UPD 
00126 *                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *GAS2UPD 
00127 *                                                                *GAS2UPD 
00128 * 14726/ 11/30/97  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *GAS2UPD 
00129 * 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *GAS2UPD 
00130 *                       INCREASE GROUP AND SECTION NUMBERS.      *GAS2UPD 
00131 *                                                                *GAS2UPD 
00132 *  D341  10/07/98  GDM  1. HIDE TIME/DOLLAR FIELD FROM SCREEN    *GAS2UPD 
00133 *                       2. ADD GAB-CARRY-OVER-CREDIT-IND         *GAS2UPD 
00134 *                                                                *GAS2UPD 
00135 *        07/07/00  GSP  ADDED LOGIC FOR NEW INTERNAL TABULAR     *GAS2UPD 
00136 *                       #IPGS.                                   *GAS2UPD 
00137 *                                                                *GAS2UPD 
00138 *D352  09/21/00  GDM  ADD ACCUM IDENTIFIER                       *GAS2UPD 
00139 *                                                                *GAS2UPD 
00140 *        11/29/00  GSP  ADD LOGIC TO DISPLAY MESSAGE IF A 6TH    *GAS2UPD 
00141 *                       INTERNAL TABULAR IS ATTEMPTED TO BE      *GAS2UPD 
00142 *                       ADDED.                                   *GAS2UPD 
00143 *                                                                *GAS2UPD 
00144 *        01/12/01 GSP ADD LOGIC TO PREVENT INTERNAL TABULAR      *GAS2UPD 
00145 *                     COUNT FROM BEING INCREASED TO GREATER      *GAS2UPD 
00146 *                     THAN 5.                                    *GAS2UPD 
00147 *                                                                *GAS2UPD 
00148 *        11/15/01 AKK ADD SUPPORT FOR 4 NEW BITS TWO FOR SERIOUS *GAS2UPD 
00149 *                     MENTAL ILLNESS AND 2 FOR EMER SERVICES.    *GAS2UPD 
00150 *                                                                *GAS2UPD 
00151 *D365B 06/03/02   JP  ADD COMBINATION APPLIED INDICATOR (CAPI)   *GAS2UPD 
00152 *                                                                *GAS2UPD 
00153 *  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *GAS2UPD 
00154 *                         DETERMINATION (SABD)                   *GAS2UPD 
00155 *                                                                *GAS2UPD 
00156 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GAS2UPD 
00157 *                                                                *GAS2UPD 
00158 *            06-13-03   DAF   CORRECTED PROGRAM SO THAT THE      *GAS2UPD 
00159 *                             TABULAR ID IS ON THE 'C6' RECORD   *GAS2UPD 
00160 *                             INSTEAD OF THE BENEFIT PROVISION ID*GAS2UPD 
00161 *                             AND THE TABULAR SLOT NUMBER IS ON  *GAS2UPD 
00162 *                             THE 'C6' RECORD INSTEAD OF THE     *GAS2UPD 
00163 *                             BENEFIT PROVISION SLOT NUMBER      *GAS2UPD 
00164 *                             USE COPYBOOK GCTIPGPC INSTEAD OF   *GAS2UPD 
00165 *                             GCTIPGTC                           *GAS2UPD 
00166 *                                                                *GAS2UPD 
00158 *            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *GAS2UPD 
00156 *            04-14-25   CJB   FIXED ISSUE WITH OCCURS ENTRIES    *GAS1UPD 
00170 ******************************************************************GAS2UPD 
00168 * CCSP  NDF - FLAGSHIP RENOVATION - OCT 2004                     *GAS2UPD 
00169 * - REMOVED DEAD CODE LINES                                      *GAS2UPD 
      *                                                                *        
      * P21595  09/19/16  HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *        
00167 *                       TYPE CODE,TIER CODE,TIER LEVEL.          *GAS2UPD 
      *                                                                *        
SI0724* P56703  05/08/24  SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *        
SI0724*                       COPY ABM, ACP, ACL, ADL, AOL,            *        
SI0724*                       GCCDRLEN                                 *        
00170 ******************************************************************GAS2UPD 
00171 ******************************************************************GAS2UPD 
00172      SKIP3                                                        GAS2UPD 
00173  ENVIRONMENT DIVISION.                                            GAS2UPD 
00174 /    D A T A   D I V I S I O N                                    GAS2UPD 
00175  DATA DIVISION.                                                   GAS2UPD 
00176  WORKING-STORAGE SECTION.                                         GAS2UPD 
00177  01  WS-BEGIN                    PIC X(24)  VALUE                 GAS2UPD 
00178      '***GAS2UPD WS BEGINS***'.                                   GAS2UPD 
00179                                                                   GAS2UPD 
00180 *    T I T L E   L I N E S                                        GAS2UPD 
00181  01  WS-TITLE-LINES.                                              GAS2UPD 
00182  COPY GCMHLINE.                                                   GAS2UPD 
00183 *****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                GAS2UPD 
00184 *      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        GAS2UPD 
00185 *    05  GROUP-SPECIFIC-ID-LINE.                                  GAS2UPD 
00186 *      10  FILLER                        PIC X(20)                GAS2UPD 
00187 *        VALUE 'GROUP SPECIFIC ID= '.                             GAS2UPD 
00188 *      10  FILLER                        PIC X(5) VALUE 'GRP= '.  GAS2UPD 
00189 *      10  GRP-SPEC-GROUP-NO             PIC X(6).                GAS2UPD 
00190 *      10  FILLER                        PIC X(6) VALUE ' SEC= '. GAS2UPD 
00191 *      10  GRP-SPEC-SECTION-NO           PIC X(4).                GAS2UPD 
00192 *      10  FILLER                        PIC X(5) VALUE ' FR= '.  GAS2UPD 
00193 *      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  GAS2UPD 
00194 *      10  FILLER                        PIC X(7) VALUE ' EFDT= '.GAS2UPD 
00195 *      10  GRP-SPEC-EFF-DATE             PIC X(6).                GAS2UPD 
00196 *    05  CONTRACT-TITLE-LINE             PIC X(42)                GAS2UPD 
00197 *      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         GAS2UPD 
00198 *    05  CONTRACT-ID-LINE.                                        GAS2UPD 
00199 *      10  FILLER                      PIC X(14)                  GAS2UPD 
00200 *        VALUE 'CONTRACT ID= '.                                   GAS2UPD 
00201 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS2UPD 
00202 *      10  CONTRACT-GROUP-NO           PIC X(6).                  GAS2UPD 
00203 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS2UPD 
00204 *      10  CONTRACT-SECTION-NO         PIC X(4).                  GAS2UPD 
00205 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS2UPD 
00206 *      10  CONTRACT-LOB                PIC X.                     GAS2UPD 
00207 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS2UPD 
00208 *      10  CONTRACT-PROV-CTL           PIC XX.                    GAS2UPD 
00209 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS2UPD 
00210 *      10  CONTRACT-FAM-REL-LVL        PIC XX.                    GAS2UPD 
00211 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS2UPD 
00212 *      10  CONTRACT-EFF-DATE               PIC X(6).              GAS2UPD 
00213 *    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                GAS2UPD 
00214 *      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        GAS2UPD 
00215 *    05  BENEFIT-PROVISION-ID-LINE.                               GAS2UPD 
00216 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS2UPD 
00217 *      10  BEN-PROV-GROUP-NO           PIC X(6).                  GAS2UPD 
00218 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS2UPD 
00219 *      10  BEN-PROV-SECTION-NO         PIC X(4).                  GAS2UPD 
00220 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS2UPD 
00221 *      10  BEN-PROV-LOB                PIC X.                     GAS2UPD 
00222 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS2UPD 
00223 *      10  BEN-PROV-PROV-CTL           PIC XX.                    GAS2UPD 
00224 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS2UPD 
00225 *      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    GAS2UPD 
00226 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS2UPD 
00227 *      10  BEN-PROV-EFF-DATE           PIC X(6).                  GAS2UPD 
00228 *      10  FILLER                      PIC X(8) VALUE ' BPVID= '. GAS2UPD 
00229 *      10  BEN-PROV-ID-NO              PIC X(6).                  GAS2UPD 
00230 *    05  ACL-TITLE-LINE                PIC X(26)                  GAS2UPD 
00231 *********VALUE '    COINSURANCE LIMITS    '.                      GAS2UPD 
00232 /    A L T E R N A T I V E   W O R K F I L E   K E Y S            GAS2UPD 
00233  01  FILLER                      PIC X(32)  VALUE                 GAS2UPD 
00234      '*** ALTERNATIVE WORKFILE KEY ***'.                          GAS2UPD 
00235  01  SAVE-WS-ALT-WORKFILE-KEYS.                                   GAS2UPD 
00236      05 FILLER                   PIC X(63) VALUE SPACES.          GAS2UPD 
00237                                                                   GAS2UPD 
00238  01  SAVE-RESTORE-KEY.                                            GAS2UPD 
00239      05 WS-SV-RESTO-KY           PIC X(63) VALUE SPACES.          GAS2UPD 
00240                                                                   GAS2UPD 
00241  01  WS-ALT-WORKFILE-KEYS.                                        GAS2UPD 
00242  COPY GCWRKKEY.                                                   GAS2UPD 
00243                                                                   GAS2UPD 
00244                                                                   GAS2UPD 
00245 *   D A T E   F O R M A T T I N G   C O M M A R E A               GAS2UPD 
00246  01  HGADATES-COMMAREA.                                           GAS2UPD 
00247  COPY HGCDAT01.                                                   GAS2UPD 
00248                                                                   GAS2UPD 
00249 *    W O R K F I E L D S ,   A N D   S W I T C H E S              GAS2UPD 
00250  01  WS-WORK-FIELDS.                                              GAS2UPD 
00251                                                                   GAS2UPD 
00252      05  WS-SPACES-ZEROS.                                         GAS2UPD 
00253        10  WS-SPACES-ZEROS-SPACES       PIC X(6)  VALUE SPACES.   GAS2UPD 
00254        10  WS-SPACES-ZEROS-ZEROS        PIC S9(7) COMP-3          GAS2UPD 
00255                                                   VALUE ZEROS.    GAS2UPD 
00256                                                                   GAS2UPD 
00257      05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    GAS2UPD 
00258                                                                   GAS2UPD 
00259      05  WS-TAB-PROV-COPY-SLOT          PIC S9(7) COMP-3.         GAS2UPD 
00260      05  WS-INTL-TAB-ID.                                          GAS2UPD 
00261        10  WS-INTL-TAB-TAB-ID           PIC X(6).                 GAS2UPD 
00262        10  WS-INTL-TAB-TAB-SLOT         PIC S9(7) COMP-3.         GAS2UPD 
00263      05  WS-SAVE-INTL-TAB.                                        GAS2UPD 
00264        10  WS-SAVE-INTL-TAB-ID          PIC X(6).                 GAS2UPD 
00265        10  WS-SAVE-INTL-TAB-SLOT        PIC S9(7) COMP-3.         GAS2UPD 
00266                                                                   GAS2UPD 
00267      05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  GAS2UPD 
00268      05  WS-QUOTIENT                   PIC 999  COMP-3.           GAS2UPD 
00269      05  WS-REMAINDER                  PIC 999  COMP-3.           GAS2UPD 
00270                                                                   GAS2UPD 
00271      05  WS-CDE-REQUEST-CODES.                                    GAS2UPD 
00272          10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.GAS2UPD 
00273          10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.GAS2UPD 
00274          10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.GAS2UPD 
00275          10  WS-REQUEST-4900-CNTL-UPDATE    PIC X(4) VALUE '4900'.GAS2UPD 
00276                                                                   GAS2UPD 
00277      05  WS-INTRNL-TABS-TO-CHG-CNT          PIC S9   COMP-3.      GAS2UPD 
00278      05  WS-INT-TAB-CHANGE-INDICATOR        PIC XX   VALUE SPACE. GAS2UPD 
00279        88  WS-INT-DESCRP-CHG-TO-NON-PROD        VALUE 'PN'.       GAS2UPD 
00280        88  WS-INT-DESCRP-CHG-BACK-TO-PROD       VALUE 'NP'.       GAS2UPD 
00281        88  WS-INT-DESCRP-NOCHG-AT-PROD          VALUE '  '.       GAS2UPD 
00282        88  WS-INT-DESCRP-NOCHG-AT-NONPROD       VALUE 'NN'.       GAS2UPD 
00283                                                                   GAS2UPD 
00284 *    I N T E R N A L   T A B U L A R   P R O G R A M   N A M E    GAS2UPD 
00285  01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  GAS2UPD 
00286                                                                   GAS2UPD 
00287                                                                   GAS2UPD 
00288 ** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          GAS2UPD 
00289  01  WS-ENTRY                          PIC X(176).                GAS2UPD 
00290      SKIP3                                                        GAS2UPD 
00291 /   A T T R I B U T E S                                           GAS2UPD 
00292  COPY DFHBMSCA.                                                   GAS2UPD 
00293      02  DFHBMABF                PIC X VALUE '9'.                 GAS2UPD 
00294 /   A T T E N T I O N   I D E N T I F I E R S                     GAS2UPD 
00295  COPY DFHAID.                                                     GAS2UPD 
00296 /   R E C O R D   L E N G T H S                                   GAS2UPD 
00297                                                                   GAS2UPD 
00298  01  WS-RECORD-LENGTHS.                                           GAS2UPD 
00299 *   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   GAS2UPD 
00300     05 WS-COMMON-WORKAREA-LEN         PIC S9(4) COMP  VALUE +550. GAS2UPD 
00301     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. GAS2UPD 
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.GAS2UPD 
SI0724    05 WS-COPY-TABLE-LEN             PIC S9(4) COMP  VALUE +30800.GAS2UPD 
00303     05 WS-GETMAIN-ADDRESS             PIC S9(8) COMP  VALUE +0.   GAS2UPD 
00304     05 WS-GETMAIN-LEN                 PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00305     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00306     05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00307     05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00308     05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00309     05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00310     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00311     05 WS-WF-INTR-TAB-LEN             PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00312     05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00313     05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00314     05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   GAS2UPD 
00315     05 WS-NEW-OCCR-ON-WF              PIC X   VALUE SPACES.       GAS2UPD 
00316                                                                   GAS2UPD 
00317 ******************************************************************GAS2UPD 
00318 ** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **GAS2UPD 
00319 ******************************************************************GAS2UPD 
00320  01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  GAS2UPD 
00321  01  CURNT-OCURS-PKD             PIC 9(4).                        GAS2UPD 
00322  01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            GAS2UPD 
00323      05  FILLER                  PIC XX.                          GAS2UPD 
00324      05  CURNT-OCCURS-OUT        PIC XX.                          GAS2UPD 
00325                                                                   GAS2UPD 
00326  01  TOTAL-OCURS-UNK             PIC 9(5).                        GAS2UPD 
00327  01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            GAS2UPD 
00328      05  FILLER                  PIC XXX.                         GAS2UPD 
00329      05  TOTAL-OCCURS-OUT        PIC XX.                          GAS2UPD 
00330 /    G . C .   R E C O R D S   L E N G T H S                      GAS2UPD 
00331  01  WS-GC-RECORD-LENGTHS.                                        GAS2UPD 
00332      COPY GCCDRLEN.                                               GAS2UPD 
00333                                                                   GAS2UPD 
00334 /    A B E N D   A R E A                                          GAS2UPD 
00335  01  WS-01-ABEND-AREA.                                            GAS2UPD 
00336      05  FILLER                   PIC X(16)  VALUE                GAS2UPD 
00337          '** ABEND AREA **'.                                      GAS2UPD 
00338                                                                   GAS2UPD 
00339      05  WS-ABCODE-CODES-AND-MSG.                                 GAS2UPD 
00340          10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. GAS2UPD 
00341          10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. GAS2UPD 
00342          10  WS-ABCODE-1CC1             PIC X(04)  VALUE  '1CC1'. GAS2UPD 
00343          10  WS-ABCODE-1CC1-MSG         PIC X(79)  VALUE          GAS2UPD 
00344              '*** INVALID PARAMETER LENGTH FOUND ***              GAS2UPD 
00345 -            '                           '.                       GAS2UPD 
00346          10  WS-ABCODE-1CC2             PIC X(04)  VALUE  '1CC2'. GAS2UPD 
00347          10  WS-ABCODE-1CC2-MSG         PIC X(79)  VALUE          GAS2UPD 
00348              '*** WRONG RECORD STATUS PASSED TO THIS PGM ***      GAS2UPD 
00349 -            '                           '.                       GAS2UPD 
00350          10  WS-ABCODE-1CC3             PIC X(04)  VALUE  '1CC3'. GAS2UPD 
00351          10  WS-ABCODE-1CC3-MSG         PIC X(79)  VALUE          GAS2UPD 
00352              '*** WRONG RECORD TYPE PASSED TO THIS PGM ***        GAS2UPD 
00353 -            '                           '.                       GAS2UPD 
00354          10  WS-ABCODE-1CF1             PIC X(04)  VALUE  '1CF1'. GAS2UPD 
00355          10  WS-ABCODE-1CF1-MSG         PIC X(79)  VALUE          GAS2UPD 
00356              '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABUGAS2UPD 
00357 -            'LAR.  CONTACT SYSTEMS ***  '.                       GAS2UPD 
00358          10  WS-ABCODE-1CF2             PIC X(04)  VALUE  '1CF2'. GAS2UPD 
00359          10  WS-ABCODE-1CF2-MSG         PIC X(79)  VALUE          GAS2UPD 
00360              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS2UPD 
00361 -            ' CONTACT SYSTEMS ***       '.                       GAS2UPD 
00362          10  WS-ABCODE-1CF3             PIC X(04)  VALUE  '1CF3'. GAS2UPD 
00363          10  WS-ABCODE-1CF3-MSG         PIC X(79)  VALUE          GAS2UPD 
00364              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS2UPD 
00365 -            ' CONTACT SYSTEMS ***       '.                       GAS2UPD 
00366          10  WS-ABCODE-1CF4             PIC X(04)  VALUE  '1CF4'. GAS2UPD 
00367          10  WS-ABCODE-1CF4-MSG         PIC X(79)  VALUE          GAS2UPD 
00368              '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTAGAS2UPD 
00369 -            'CT SYSTEMS ***             '.                       GAS2UPD 
00370          10  WS-ABCODE-1CF5             PIC X(04)  VALUE  '1CF5'. GAS2UPD 
00371          10  WS-ABCODE-1CF5-MSG         PIC X(79)  VALUE          GAS2UPD 
00372              'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFIGAS2UPD 
00373 -            'LE.  PLEASE CONTACT SYSTEMS'.                       GAS2UPD 
00374          10  WS-ABCODE-1CF6             PIC X(04)  VALUE  '1CF6'. GAS2UPD 
00375          10  WS-ABCODE-1CF6-MSG         PIC X(79)  VALUE          GAS2UPD 
00376              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFILGAS2UPD 
00377 -            'E.  PLEASE CONTACT SYSTEMS '.                       GAS2UPD 
00378          10  WS-ABCODE-1CF7             PIC X(04)  VALUE  '1CF7'. GAS2UPD 
00379          10  WS-ABCODE-1CF7-MSG         PIC X(79)  VALUE          GAS2UPD 
00380              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS2UPD 
00381 -            ' SYSTEMS ***               '.                       GAS2UPD 
00382          10  WS-ABCODE-1CF9             PIC X(04)  VALUE  '1CF9'. GAS2UPD 
00383          10  WS-ABCODE-1CF9-MSG         PIC X(79)  VALUE          GAS2UPD 
00384              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILEGAS2UPD 
00385 -            '.  PLEASE CONTACT SYSTEMS  '.                       GAS2UPD 
00386          10  WS-ABCODE-1CFA             PIC X(04)  VALUE  '1CFA'. GAS2UPD 
00387          10  WS-ABCODE-1CFA-MSG         PIC X(79)  VALUE          GAS2UPD 
00388              '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE GAS2UPD 
00389 -            'CONTACT SYSTEMS ***        '.                       GAS2UPD 
00390          10  WS-ABCODE-1CFB             PIC X(04)  VALUE  '1CFB'. GAS2UPD 
00391          10  WS-ABCODE-1CFB-MSG         PIC X(79)  VALUE          GAS2UPD 
00392              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS2UPD 
00393 -            ' SYSTEMS ***               '.                       GAS2UPD 
00394          10  WS-ABCODE-1CFC             PIC X(04)  VALUE  '1CFC'. GAS2UPD 
00395          10  WS-ABCODE-1CFC-MSG         PIC X(79)  VALUE          GAS2UPD 
00396              '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTACGAS2UPD 
00397 -            'T SYSTEMS ***              '.                       GAS2UPD 
00398          10  WS-ABCODE-1CFJ             PIC X(04)  VALUE  '1CFJ'. GAS2UPD 
00399          10  WS-ABCODE-1CFJ-MSG         PIC X(79)  VALUE          GAS2UPD 
00400              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS2UPD 
00401 -            ' SYSTEMS ***               '.                       GAS2UPD 
00402          10  WS-ABCODE-1CFK             PIC X(04)  VALUE  '1CFK'. GAS2UPD 
00403          10  WS-ABCODE-1CFK-MSG         PIC X(79)  VALUE          GAS2UPD 
00404              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS2UPD 
00405 -            ' SYSTEMS ***               '.                       GAS2UPD 
00406          10  WS-ABCODE-1CFL             PIC X(04)  VALUE  '1CFL'. GAS2UPD 
00407          10  WS-ABCODE-1CFL-MSG         PIC X(79)  VALUE          GAS2UPD 
00408              '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TOGAS2UPD 
00409 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS2UPD 
00410          10  WS-ABCODE-1CFM             PIC X(04)  VALUE  '1CFM'. GAS2UPD 
00411          10  WS-ABCODE-1CFM-MSG         PIC X(79)  VALUE          GAS2UPD 
00412              '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  GAS2UPD 
00413 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS2UPD 
00414          10  WS-ABCODE-1CFN             PIC X(04)  VALUE  '1CFN'. GAS2UPD 
00415          10  WS-ABCODE-1CFN-MSG         PIC X(79)  VALUE          GAS2UPD 
00416              '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO MGAS2UPD 
00417 -            'ENU.  CONTACT SYSTEMS ***  '.                       GAS2UPD 
00418          10  WS-ABCODE-1CFO             PIC X(04)  VALUE  '1CFO'. GAS2UPD 
00419          10  WS-ABCODE-1CFO-MSG         PIC X(79)  VALUE          GAS2UPD 
00420              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS2UPD 
00421 -            ' SYSTEMS ***               '.                       GAS2UPD 
00422          10  WS-ABCODE-1CFP             PIC X(04)  VALUE  '1CFP'. GAS2UPD 
00423          10  WS-ABCODE-1CFP-MSG         PIC X(79)  VALUE          GAS2UPD 
00424              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS2UPD 
00425 -            ' SYSTEMS ***               '.                       GAS2UPD 
00426          10  WS-ABCODE-1CFQ             PIC X(04)  VALUE  '1CFQ'. GAS2UPD 
00427          10  WS-ABCODE-1CFQ-MSG         PIC X(79)  VALUE          GAS2UPD 
00428              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS2UPD 
00429 -            'CT SYSTEMS ***             '.                       GAS2UPD 
00430          10  WS-ABCODE-1CFR             PIC X(04)  VALUE  '1CFR'. GAS2UPD 
00431          10  WS-ABCODE-1CFR-MSG         PIC X(79)  VALUE          GAS2UPD 
00432              '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACTGAS2UPD 
00433 -            ' SYSTEMS ***               '.                       GAS2UPD 
00434          10  WS-ABCODE-1CFS             PIC X(04)  VALUE  '1CFS'. GAS2UPD 
00435          10  WS-ABCODE-1CFS-MSG         PIC X(79)  VALUE          GAS2UPD 
00436              '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACTGAS2UPD 
00437 -            ' SYSTEMS ***               '.                       GAS2UPD 
00438          10  WS-ABCODE-1CFT             PIC X(04)  VALUE  '1CFT'. GAS2UPD 
00439          10  WS-ABCODE-1CFT-MSG         PIC X(79)  VALUE          GAS2UPD 
00440              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS2UPD 
00441 -            ' SYSTEMS ***               '.                       GAS2UPD 
00442          10  WS-ABCODE-1CFU             PIC X(04)  VALUE  '1CFU'. GAS2UPD 
00443          10  WS-ABCODE-1CFU-MSG         PIC X(79)  VALUE          GAS2UPD 
00444              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS2UPD 
00445 -            'CT SYSTEMS ***             '.                       GAS2UPD 
00446          10  WS-ABCODE-1CL1             PIC X(04)  VALUE  '1CL1'. GAS2UPD 
00447          10  WS-ABCODE-1CL1-MSG         PIC X(79)  VALUE          GAS2UPD 
00448              '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***GAS2UPD 
00449 -            '                           '.                       GAS2UPD 
00450          10  WS-ABCODE-1CL2             PIC X(04)  VALUE  '1CL2'. GAS2UPD 
00451          10  WS-ABCODE-1CL2-MSG         PIC X(79)  VALUE          GAS2UPD 
00452              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS2UPD 
00453 -            'MS ***                     '.                       GAS2UPD 
00454          10  WS-ABCODE-1CL3             PIC X(04)  VALUE  '1CL3'. GAS2UPD 
00455          10  WS-ABCODE-1CL3-MSG         PIC X(79)  VALUE          GAS2UPD 
00456              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS2UPD 
00457 -            'EMS ***                    '.                       GAS2UPD 
00458          10  WS-ABCODE-1CL4             PIC X(04)  VALUE  '1CL4'. GAS2UPD 
00459          10  WS-ABCODE-1CL4-MSG         PIC X(79)  VALUE          GAS2UPD 
00460              '*** THE OCCURS WE ARE TO DISPLAY HAS BEEN DELETED   GAS2UPD 
00461 -            '                           '.                       GAS2UPD 
00462          10  WS-ABCODE-1CLX             PIC X(04)  VALUE  '1CLX'. GAS2UPD 
00463          10  WS-ABCODE-1CLX-MSG         PIC X(79)  VALUE          GAS2UPD 
00464              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS2UPD 
00465 -            'MS ***                     '.                       GAS2UPD 
00466          10  WS-ABCODE-1CP1             PIC X(04)  VALUE  '1CP1'. GAS2UPD 
00467          10  WS-ABCODE-1CP1-MSG         PIC X(79)  VALUE          GAS2UPD 
00468              '????????????????????????????????????????????????????GAS2UPD 
00469 -            '???????????????????????????'.                       GAS2UPD 
00470                                                                   GAS2UPD 
00471 /*****************************************************************GAS2UPD 
00472 *    WT-01   M E S S A G E   T A B L E                            GAS2UPD 
00473 ******************************************************************GAS2UPD 
00474  01  WT-01-TABLE.                                                 GAS2UPD 
00475      05  FILLER                  PIC X(16) VALUE                  GAS2UPD 
00476          '* WT-01-TABLE  *'.                                      GAS2UPD 
00477  01  FILLER.                                                      GAS2UPD 
00478      05  WT-01-MESSAGE-VALUES.                                    GAS2UPD 
00479                                                                   GAS2UPD 
00480 *----------------------------------------------------------------*GAS2UPD 
00481          10  WT-01-ENTRY-001.                                     GAS2UPD 
00482              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00483              15  WT-01-MESSAGE-TEXT-001.                          GAS2UPD 
00484                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00485                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00486                  20  FILLER          PIC X(3)  VALUE  '001'.      GAS2UPD 
00487                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00488                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00489                      '#IBGR HAS BEEN SUCCESSFULLY MAPPED          GAS2UPD 
00490 -                    '                         '.                 GAS2UPD 
00491              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00492 *----------------------------------------------------------------*GAS2UPD 
00493          10  WT-01-ENTRY-002.                                     GAS2UPD 
00494              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00495              15  WT-01-MESSAGE-TEXT-002.                          GAS2UPD 
00496                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00497                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00498                  20  FILLER          PIC X(3)  VALUE  '002'.      GAS2UPD 
00499                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00500                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00501                      '#IPGN HAS BEEN SUCCESSFULLY MAPPED          GAS2UPD 
00502 -                    '                         '.                 GAS2UPD 
00503              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00504 *----------------------------------------------------------------*GAS2UPD 
00505          10  WT-01-ENTRY-003.                                     GAS2UPD 
00506              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00507              15  WT-01-MESSAGE-TEXT-003.                          GAS2UPD 
00508                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00509                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00510                  20  FILLER          PIC X(3)  VALUE  '003'.      GAS2UPD 
00511                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00512                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00513                      '#IPGT HAS BEEN SUCCESSFULLY MAPPED          GAS2UPD 
00514 -                    '                         '.                 GAS2UPD 
00515              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00516 *----------------------------------------------------------------*GAS2UPD 
00517          10  WT-01-ENTRY-004.                                     GAS2UPD 
00518              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00519              15  WT-01-MESSAGE-TEXT-004.                          GAS2UPD 
00520                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00521                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00522                  20  FILLER          PIC X(3)  VALUE  '004'.      GAS2UPD 
00523                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00524                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00525                      '******************* F U T U R E   U S E ****GAS2UPD 
00526 -                    '*************************'.                 GAS2UPD 
00527              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00528 *----------------------------------------------------------------*GAS2UPD 
00529          10  WT-01-ENTRY-005.                                     GAS2UPD 
00530              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00531              15  WT-01-MESSAGE-TEXT-005.                          GAS2UPD 
00532                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00533                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00534                  20  FILLER          PIC X(3)  VALUE  '005'.      GAS2UPD 
00535                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00536                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00537                      '******************* F U T U R E   U S E ****GAS2UPD 
00538 -                    '*************************'.                 GAS2UPD 
00539              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00540 *----------------------------------------------------------------*GAS2UPD 
00541          10  WT-01-ENTRY-006.                                     GAS2UPD 
00542              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00543              15  WT-01-MESSAGE-TEXT-006.                          GAS2UPD 
00544                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00545                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00546                  20  FILLER          PIC X(3)  VALUE  '006'.      GAS2UPD 
00547                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00548                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00549                      'DELETE OPTION MUST BE \
00550 -                    'VALID                    '.                 GAS2UPD 
00551              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00552 *----------------------------------------------------------------*GAS2UPD 
00553          10  WT-01-ENTRY-007.                                     GAS2UPD 
00554              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00555              15  WT-01-MESSAGE-TEXT-007.                          GAS2UPD 
00556                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00557                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00558                  20  FILLER          PIC X(3)  VALUE  '007'.      GAS2UPD 
00559                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00560                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00561                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSGAS2UPD 
00562 -                    ' PF4/PF16 TO CONTINUE    '.                 GAS2UPD 
00563              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00564 *----------------------------------------------------------------*GAS2UPD 
00565          10  WT-01-ENTRY-008.                                     GAS2UPD 
00566              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00567              15  WT-01-MESSAGE-TEXT-008.                          GAS2UPD 
00568                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00569                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00570                  20  FILLER          PIC X(3)  VALUE  '008'.      GAS2UPD 
00571                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00572                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00573                      'GROUP IN CONVERSION STATUS, CANNOT CHANGE HIGAS2UPD 
00574 -                    'GH-LIGHTED ELEMENTS      '.                 GAS2UPD 
00575              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00576 *----------------------------------------------------------------*GAS2UPD 
00577          10  WT-01-ENTRY-009.                                     GAS2UPD 
00578              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00579              15  WT-01-MESSAGE-TEXT-009.                          GAS2UPD 
00580                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00581                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00582                  20  FILLER          PIC X(3)  VALUE  '009'.      GAS2UPD 
00583                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00584                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00585                      'INVALID PFKEY SELECTION                     GAS2UPD 
00586 -                    '                         '.                 GAS2UPD 
00587              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00588 *----------------------------------------------------------------*GAS2UPD 
00589          10  WT-01-ENTRY-010.                                     GAS2UPD 
00590              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00591              15  WT-01-MESSAGE-TEXT-010.                          GAS2UPD 
00592                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00593                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00594                  20  FILLER          PIC X(3)  VALUE  '010'.      GAS2UPD 
00595                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00596                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00597                      'INVALID REQUEST.  THAT PF KEY HAS NO MEANINGGAS2UPD 
00598 -                    ' TO THIS PROGRAM         '.                 GAS2UPD 
00599              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00600 *----------------------------------------------------------------*GAS2UPD 
00601          10  WT-01-ENTRY-011.                                     GAS2UPD 
00602              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00603              15  WT-01-MESSAGE-TEXT-011.                          GAS2UPD 
00604                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00605                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00606                  20  FILLER          PIC X(3)  VALUE  '011'.      GAS2UPD 
00607                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00608                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00609                      'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT GAS2UPD 
00610 -                    '                         '.                 GAS2UPD 
00611              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00612 *----------------------------------------------------------------*GAS2UPD 
00613          10  WT-01-ENTRY-012.                                     GAS2UPD 
00614              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00615              15  WT-01-MESSAGE-TEXT-012.                          GAS2UPD 
00616                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00617                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00618                  20  FILLER          PIC X(3)  VALUE  '012'.      GAS2UPD 
00619                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00620                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00621                      'NO ENTRIES TO DISPLAY                       GAS2UPD 
00622 -                    '                         '.                 GAS2UPD 
00623              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00624 *----------------------------------------------------------------*GAS2UPD 
00625          10  WT-01-ENTRY-013.                                     GAS2UPD 
00626              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00627              15  WT-01-MESSAGE-TEXT-013.                          GAS2UPD 
00628                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00629                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00630                  20  FILLER          PIC X(3)  VALUE  '013'.      GAS2UPD 
00631                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00632                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00633                      'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HIGAS2UPD 
00634 -                    'T ENTER FOR ERR MSG      '.                 GAS2UPD 
00635              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00636 *----------------------------------------------------------------*GAS2UPD 
00637          10  WT-01-ENTRY-014.                                     GAS2UPD 
00638              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00639              15  WT-01-MESSAGE-TEXT-014.                          GAS2UPD 
00640                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00641                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00642                  20  FILLER          PIC X(3)  VALUE  '014'.      GAS2UPD 
00643                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00644                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00645                      'PROCESSING FROM THE TOP OF THE LIST         GAS2UPD 
00646 -                    '                         '.                 GAS2UPD 
00647              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00648 *----------------------------------------------------------------*GAS2UPD 
00649          10  WT-01-ENTRY-015.                                     GAS2UPD 
00650              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00651              15  WT-01-MESSAGE-TEXT-015.                          GAS2UPD 
00652                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00653                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00654                  20  FILLER          PIC X(3)  VALUE  '015'.      GAS2UPD 
00655                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00656                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00657                      'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDEGAS2UPD 
00658 -                    'D                        '.                 GAS2UPD 
00659              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00660 *----------------------------------------------------------------*GAS2UPD 
00661          10  WT-01-ENTRY-016.                                     GAS2UPD 
00662              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00663              15  WT-01-MESSAGE-TEXT-016.                          GAS2UPD 
00664                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00665                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00666                  20  FILLER          PIC X(3)  VALUE  '016'.      GAS2UPD 
00667                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00668                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00669                      'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUMGAS2UPD 
00670 -                    'BER OF OCCURANCES        '.                 GAS2UPD 
00671              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00672 *----------------------------------------------------------------*GAS2UPD 
00673          10  WT-01-ENTRY-017.                                     GAS2UPD 
00674              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00675              15  WT-01-MESSAGE-TEXT-017.                          GAS2UPD 
00676                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00677                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00678                  20  FILLER          PIC X(3)  VALUE  '017'.      GAS2UPD 
00679                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00680                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00681                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS2UPD 
00682 -                    'T BE CHANGED             '.                 GAS2UPD 
00683              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00684 *----------------------------------------------------------------*GAS2UPD 
00685          10  WT-01-ENTRY-018.                                     GAS2UPD 
00686              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00687              15  WT-01-MESSAGE-TEXT-018.                          GAS2UPD 
00688                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00689                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00690                  20  FILLER          PIC X(3)  VALUE  '018'.      GAS2UPD 
00691                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00692                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00693                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS2UPD 
00694 -                    'T BE MAPPED              '.                 GAS2UPD 
00695              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00696 *----------------------------------------------------------------*GAS2UPD 
00697          10  WT-01-ENTRY-019.                                     GAS2UPD 
00698              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00699              15  WT-01-MESSAGE-TEXT-019.                          GAS2UPD 
00700                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00701                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00702                  20  FILLER          PIC X(3)  VALUE  '019'.      GAS2UPD 
00703                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00704                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00705                      'THERE ARE NO MORE ENTRIES TO DISPLAY        GAS2UPD 
00706 -                    '                         '.                 GAS2UPD 
00707              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00708 *----------------------------------------------------------------*GAS2UPD 
00709          10  WT-01-ENTRY-020.                                     GAS2UPD 
00710              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00711              15  WT-01-MESSAGE-TEXT-020.                          GAS2UPD 
00712                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00713                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00714                  20  FILLER          PIC X(3)  VALUE  '020'.      GAS2UPD 
00715                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00716                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00717                      'THIS IS THE FIRST ON THE TABLE              GAS2UPD 
00718 -                    '                         '.                 GAS2UPD 
00719              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00720 *----------------------------------------------------------------*GAS2UPD 
00721          10  WT-01-ENTRY-021.                                     GAS2UPD 
00722              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00723              15  WT-01-MESSAGE-TEXT-021.                          GAS2UPD 
00724                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00725                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00726                  20  FILLER          PIC X(3)  VALUE  '021'.      GAS2UPD 
00727                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00728                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00729                      'THIS IS THE LAST ON THE TABLE               GAS2UPD 
00730 -                    '                         '.                 GAS2UPD 
00731              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00732 *----------------------------------------------------------------*GAS2UPD 
00733          10  WT-01-ENTRY-022.                                     GAS2UPD 
00734              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00735              15  WT-01-MESSAGE-TEXT-022.                          GAS2UPD 
00736                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00737                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00738                  20  FILLER          PIC X(3)  VALUE  '022'.      GAS2UPD 
00739                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00740                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00741                      'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  GAS2UPD 
00742 -                    '                         '.                 GAS2UPD 
00743              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00744 *----------------------------------------------------------------*GAS2UPD 
00745          10  WT-01-ENTRY-023.                                     GAS2UPD 
00746              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00747              15  WT-01-MESSAGE-TEXT-023.                          GAS2UPD 
00748                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00749                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00750                  20  FILLER          PIC X(3)  VALUE  '023'.      GAS2UPD 
00751                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00752                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00753                      '#IDGD HAS BEEN SUCCESSFULLY MAPPED          GAS2UPD 
00754 -                    '                         '.                 GAS2UPD 
00755              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00756 *----------------------------------------------------------------*GAS2UPD 
00757          10  WT-01-ENTRY-024.                                     GAS2UPD 
00758              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00759              15  WT-01-MESSAGE-TEXT-024.                          GAS2UPD 
00760                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00761                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00762                  20  FILLER          PIC X(3)  VALUE  '024'.      GAS2UPD 
00763                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00764                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00765                      '#IPGP HAS BEEN SUCCESSFULLY MAPPED          GAS2UPD 
00766 -                    '                         '.                 GAS2UPD 
00767              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00768 *----------------------------------------------------------------*GAS2UPD 
00769          10  WT-01-ENTRY-025.                                     GAS2UPD 
00770              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00771              15  WT-01-MESSAGE-TEXT-003.                          GAS2UPD 
00772                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00773                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00774                  20  FILLER          PIC X(3)  VALUE  '025'.      GAS2UPD 
00775                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00776                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00777                      '#IPGS HAS BEEN SUCCESSFULLY MAPPED          GAS2UPD 
00778 -                    '                         '.                 GAS2UPD 
00779              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00780 *----------------------------------------------------------------*GAS2UPD 
00781          10  WT-01-ENTRY-026.                                     GAS2UPD 
00782              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00783              15  WT-01-MESSAGE-TEXT-026.                          GAS2UPD 
00784                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00785                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00786                  20  FILLER          PIC X(3)  VALUE  '026'.      GAS2UPD 
00787                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00788                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00789                      'MAXIMUM OF 5 INTERNAL TABULARS HAS ALREADY BGAS2UPD 
00790 -                    'EEN REACHED              '.                 GAS2UPD 
00791              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00792 *----------------------------------------------------------------*GAS2UPD 
00793          10  WT-01-ENTRY-027.                                     GAS2UPD 
00794              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS2UPD 
00795              15  WT-01-MESSAGE-TEXT-027.                          GAS2UPD 
00796                  20  FILLER          PIC X(4)  VALUE  'GAS2'.     GAS2UPD 
00797                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS2UPD 
00798                  20  FILLER          PIC X(3)  VALUE  '027'.      GAS2UPD 
00799                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS2UPD 
00800                  20  FILLER          PIC X(70) VALUE              GAS2UPD 
00801                      '********** F U T U R E   U S E *************GAS2UPD 
00802 -                    '*************************'.                 GAS2UPD 
00803              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS2UPD 
00804 *----------------------------------------------------------------*GAS2UPD 
00805                                                                   GAS2UPD 
00806      05  WT-01-MESSAGE-TABLE         REDEFINES                    GAS2UPD 
00807          WT-01-MESSAGE-VALUES         OCCURS 027 TIMES            GAS2UPD 
00808                                      INDEXED BY WT-01-INDEX.      GAS2UPD 
00809          10  WT-01-ENTRY.                                         GAS2UPD 
00810              15  FILLER              PIC X(02).                   GAS2UPD 
00811              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GAS2UPD 
00812              15  FILLER              PIC X(02).                   GAS2UPD 
00813                                                                   GAS2UPD 
00814  01  WS-END                      PIC X(16)  VALUE                 GAS2UPD 
00815      '*** W/S ENDS ***'.                                          GAS2UPD 
00816 /    L I N K A G E   S E C T I O N                                GAS2UPD 
00817  LINKAGE SECTION.                                                 GAS2UPD 
00818  01  DFHCOMMAREA.                                                 GAS2UPD 
00819  COPY  G2ALCKEC.                                                  GAS2UPD 
00820  COPY  GACDACWA.                                                  GAS2UPD 
00821      05  GAS2UPD-PASSED-AREA.                                     GAS2UPD 
00822          07  LVL2-B-SW                PIC X.                      GAS2UPD 
00823          07  LVL2-F-SW                PIC X.                      GAS2UPD 
00824          07  LVL2-G-SW                PIC X.                      GAS2UPD 
00825          07  INTR-TAB-PGM-ID          PIC X(8).                   GAS2UPD 
00826          07  FILLER                   PIC X(09).                  GAS2UPD 
00827      05  DELADD-OPTION                PIC X(7).                   GAS2UPD 
00828                                                                   GAS2UPD 
00829 /*****************************************************************GAS2UPD 
00830 * W O R K F I L E   -   A L L   L E V E L   T A B U L A R   R E C GAS2UPD 
00831 ******************************************************************GAS2UPD 
00832  01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS2UPD 
00833  COPY GCIOPRM1.                                                   GAS2UPD 
00834 /                                                                 GAS2UPD 
00835  COPY GCWRKDCC.                                                   GAS2UPD 
00836 /                                                                 GAS2UPD 
00837  COPY GCTACLC.                                                    GAS2UPD 
00838 /*****************************************************************GAS2UPD 
00839 *    C O M M U N I C A T I O N   K E Y   A R E A                  GAS2UPD 
00840 ******************************************************************GAS2UPD 
00841 *01  COMMUNICATION-KEY-AREA.                                      GAS2UPD 
00842 *COPY G2ALCKEC.                                                   GAS2UPD 
00843                                                                   GAS2UPD 
00844 /*****************************************************************GAS2UPD 
00845 *    C O P Y   T A B U L A R   T A B L E   A R E A                GAS2UPD 
00846 ******************************************************************GAS2UPD 
00847  01  COPY-TABULAR-TABLE-AREA.                                     GAS2UPD 
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          GAS2UPD 
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          GAS2UPD 
00849          COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               GAS2UPD 
00850        10  COPY-SORTABLE-FLDS              PIC X(169).            GAS2UPD 
00851        10  COPY-SORT-FYI                   PIC X(003).            GAS2UPD 
00852        10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      GAS2UPD 
00853                                                                   GAS2UPD 
00854 /*****************************************************************GAS2UPD 
00855 * W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   GAS2UPD 
00856 ******************************************************************GAS2UPD 
00857  01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              GAS2UPD 
00858  COPY GCIOPRM2.                                                   GAS2UPD 
00859 /                                                                 GAS2UPD 
00860  COPY GCWRKDC2.                                                   GAS2UPD 
00861 /                                                                 GAS2UPD 
00862  COPY GCTIPGPC.                                                   GAS2UPD 
00863 /*****************************************************************GAS2UPD 
00864 * P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     GAS2UPD 
00865 ******************************************************************GAS2UPD 
00866  01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS2UPD 
00867  COPY GCIOPRMA.                                                   GAS2UPD 
00868                                                                   GAS2UPD 
00869  COPY GCWRKDCA.                                                   GAS2UPD 
00870                                                                   GAS2UPD 
00871  COPY GCTACL2.                                                    GAS2UPD 
00872 /*****************************************************************GAS2UPD 
00873 *    M A P S E T   A R E A                                        GAS2UPD 
00874 ******************************************************************GAS2UPD 
00875      COPY GA1XSETC.                                               GAS2UPD 
00876 /    P R O C E D U R E   D I V I S I O N                          GAS2UPD 
00877  PROCEDURE DIVISION.                                              GAS2UPD 
00878                                                                   GAS2UPD 
00879 ******************************************************************GAS2UPD 
00880 * 0000  HOUSEKEEPING                                             *GAS2UPD 
00881 ******************************************************************GAS2UPD 
00882  0000-000-HOUSEKEEPING          SECTION.                          GAS2UPD 
00883  0000-010.                                                        GAS2UPD 
00884                                                                   GAS2UPD 
00885      SET ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD TO            GAS2UPD 
00886                      ACWA-WF-INTERNAL-TAB-PNTR.                   GAS2UPD 
00887                                                                   GAS2UPD 
00888      SET ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS2UPD 
00889                      ACWA-WF-ALL-LEVEL-TAB-PNTR.                  GAS2UPD 
00890                                                                   GAS2UPD 
00891      SET ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS2UPD 
00892                      ACWA-PR-ALL-LEVEL-TAB-PNTR.                  GAS2UPD 
00893                                                                   GAS2UPD 
00894      SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              GAS2UPD 
00895                                                                   GAS2UPD 
00896      MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      GAS2UPD 
00897                                                                   GAS2UPD 
00898 ***  MOVE GCA-FROM-MENU-ID  TO FRMNUIDO.                          GAS2UPD 
00899                                                                   GAS2UPD 
00900      IF FRMNUIDI  =  'GS3A'                                       GAS2UPD 
00901         MOVE IDLINEI  TO  GROUP-SPECIFIC-ID-LINE.                 GAS2UPD 
00902      IF FRMNUIDI  =  'GC4A' OR 'GTM1'                             GAS2UPD 
00903         MOVE IDLINEI  TO  CONTRACT-ID-LINE.                       GAS2UPD 
00904      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
00905         MOVE IDLINEI  TO  BENEFIT-PROVISION-ID-LINE.              GAS2UPD 
00906                                                                   GAS2UPD 
00907      MOVE ACL-TITLE-LINE   TO  TITLEO.                            GAS2UPD 
00908                                                                   GAS2UPD 
00909      PERFORM 1000-000-MAIN-PROCESS.                               GAS2UPD 
00910                                                                   GAS2UPD 
00911      EXEC CICS  RETURN    END-EXEC.                               GAS2UPD 
00912      GOBACK.                                                      GAS2UPD 
00913                                                                   GAS2UPD 
00914  0000-900-EXIT.                                                   GAS2UPD 
00915      EXIT.                                                        GAS2UPD 
00916 /*****************************************************************GAS2UPD 
00917 * 1000  MAIN PROCESS                                             *GAS2UPD 
00918 ******************************************************************GAS2UPD 
00919  1000-000-MAIN-PROCESS          SECTION.                          GAS2UPD 
00920  1000-010.                                                        GAS2UPD 
00921                                                                   GAS2UPD 
00922      EXEC CICS  HANDLE CONDITION                                  GAS2UPD 
00923                 MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)   END-EXEC.   GAS2UPD 
00924                                                                   GAS2UPD 
00925      MOVE  INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.      GAS2UPD 
00926                                                                   GAS2UPD 
00927      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS2UPD 
00928         DELADDI   =     'CHG/ADD'                     AND         GAS2UPD 
00929         OENTCTRI  NOT = '0000000'                                 GAS2UPD 
00930         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS2UPD 
00931                                                                   GAS2UPD 
00932      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS2UPD 
00933         DELADDI   =    'CHG/DEL'                      AND         GAS2UPD 
00934         DELOPTNI  =    'D'                                        GAS2UPD 
00935         PERFORM  2400-000-DELETE-THIS-OCCURANCE.                  GAS2UPD 
00936                                                                   GAS2UPD 
00937      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS2UPD 
00938         DELADDI   =     'CHG/DEL'                      AND        GAS2UPD 
00939         DELOPTNI  NOT = 'D'                                       GAS2UPD 
00940         MOVE SPACES  TO  ERRMSGO                                  GAS2UPD 
00941         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS2UPD 
00942                                                                   GAS2UPD 
00943      IF (EIBAID   =    DFHPF4 OR DFHPF7 OR DFHPF8 OR              GAS2UPD 
00944                        DFHPF19 OR DFHPF20 OR DFHPF16) AND         GAS2UPD 
00945         DELADDI   =    'CHG/DEL'                      AND         GAS2UPD 
00946         DELOPTNI  NOT = 'D'                                       GAS2UPD 
00947         MOVE SPACES  TO  ERRMSGO                                  GAS2UPD 
00948         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS2UPD 
00949                                                                   GAS2UPD 
00950      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS2UPD 
00951         DELADDI   =    'CHG/DEL'                      AND         GAS2UPD 
00952         DELOPTNI  NOT = 'D'                                       GAS2UPD 
00953         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS2UPD 
00954                                                                   GAS2UPD 
00955  1000-900-EXIT.   EXIT.                                           GAS2UPD 
00956                                                                   GAS2UPD 
00957 /*****************************************************************GAS2UPD 
00958 * 2200  UPDATE THIS OCCURANCE                                    *GAS2UPD 
00959 *                                                                *GAS2UPD 
00960 *    THIS ROUTINE WILL CHANGE ANY FIELD THAT THE OPERATOR HAS    *GAS2UPD 
00961 *  CHANGED, AND HAS CODE FOR THE MAINTENANCE OF THE INTERNAL     *GAS2UPD 
00962 *  TABULAR ENTRIES.                                              *GAS2UPD 
00963 ******************************************************************GAS2UPD 
00964  2200-000-UPDATE-THIS-OCCURANCE SECTION.                          GAS2UPD 
00965  2200-010.                                                        GAS2UPD 
00966                                                                   GAS2UPD 
00967      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS2UPD 
00968                                                                   GAS2UPD 
00969      IF NOT GCIO-GOOD-RETURN                                      GAS2UPD 
00970         MOVE WS-ABCODE-1CF7        TO WS-ABCODE                   GAS2UPD 
00971         MOVE WS-ABCODE-1CF7-MSG    TO WS-ABCODE-MSG               GAS2UPD 
00972         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
00973                                                                   GAS2UPD 
00974      MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   GAS2UPD 
00975      SET GAB-INDEX         TO  1.                                 GAS2UPD 
00976      MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                GAS2UPD 
00977                                                                   GAS2UPD 
00978  2200-210-FIND-RIGHT-OCCURS.                                      GAS2UPD 
00979                                                                   GAS2UPD 
00980      IF GAB-COINS-BENEFIT-PERIOD(GAB-INDEX)  NOT = HIGH-VALUES ANDGAS2UPD 
00981         GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)  NOT =                GAS2UPD 
00982                                                 ACWA-DISPLAY-LEN-7GAS2UPD 
00983      THEN                                                         GAS2UPD 
00984          IF  GAB-INDEX  <  GAB-ENTRY-COUNT                        GAS2UPD 
00985          THEN                                                     GAS2UPD 
00986              SET GAB-INDEX  UP BY  1                              GAS2UPD 
00987              GO TO 2200-210-FIND-RIGHT-OCCURS                     GAS2UPD 
00988          ELSE                                                     GAS2UPD 
00989              MOVE WS-ABCODE-1CL1        TO WS-ABCODE              GAS2UPD 
00990              MOVE WS-ABCODE-1CL1-MSG    TO WS-ABCODE-MSG          GAS2UPD 
00991              MOVE -1                    TO  MFRMSLTL              GAS2UPD 
00992              PERFORM 9800-000-ERROR-MSG-THEN-ABEND                GAS2UPD 
00993      ELSE                                                         GAS2UPD 
00994          NEXT SENTENCE.                                           GAS2UPD 
00995                                                                   GAS2UPD 
00996      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS2UPD 
00997          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
00998          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
00999          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01000          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01001          (IPGSOPTI  =  'MT' OR 'A')                               GAS2UPD 
01002      THEN                                                         GAS2UPD 
01003          ADD 1 TO ACWA-FIELD-CHG-CNT.                             GAS2UPD 
01004                                                                   GAS2UPD 
01005                                                                   GAS2UPD 
01006      IF DAYFACII NOT =   GAB-COINS-DAY-FACTOR-IND (GAB-INDEX)     GAS2UPD 
01007         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01008         MOVE DAYFACII TO GAB-COINS-DAY-FACTOR-IND (GAB-INDEX).    GAS2UPD 
01009                                                                   GAS2UPD 
01010      IF COPAYINI  NOT =  GAB-COINS-CO-PAY-IND (GAB-INDEX)         GAS2UPD 
01011         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01012         MOVE COPAYINI TO GAB-COINS-CO-PAY-IND (GAB-INDEX).        GAS2UPD 
01013                                                                   GAS2UPD 
01014      IF BISNDINI  NOT =  GAB-COINS-BISCENDING-IND (GAB-INDEX)     GAS2UPD 
01015         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01016         MOVE BISNDINI TO GAB-COINS-BISCENDING-IND (GAB-INDEX).    GAS2UPD 
01017                                                                   GAS2UPD 
01018      IF ASCDSCDI  NOT =  GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX) GAS2UPD 
01019         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01020         MOVE ASCDSCDI TO GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX).GAS2UPD 
01021                                                                   GAS2UPD 
      **P21595 CHANGES STARTS                                                   
01018      IF BENTYPI   NOT =  GAB-COINS-BEN-TYPE           (GAB-INDEX) GAS2UPD 
01019         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01020         MOVE BENTYPI  TO GAB-COINS-BEN-TYPE           (GAB-INDEX).GAS2UPD 
01021                                                                   GAS2UPD 
01018      IF TIERCDI   NOT =  GAB-COINS-TIER-CODE          (GAB-INDEX) GAS2UPD 
01019         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01020         MOVE TIERCDI  TO GAB-COINS-TIER-CODE          (GAB-INDEX).GAS2UPD 
                                                                                
01018      IF TIERLVI   NOT =  GAB-COINS-TIER-LVL           (GAB-INDEX) GAS2UPD 
01019         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01020         MOVE TIERLVI  TO GAB-COINS-TIER-LVL           (GAB-INDEX).GAS2UPD 
      **P21595 CHANGES ENDS                                                     
                                                                                
01022      IF CSTCONTI  NOT =   GAB-COINS-COST-CONTAIN-IND (GAB-INDEX)  GAS2UPD 
01023         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01024         MOVE CSTCONTI  TO GAB-COINS-COST-CONTAIN-IND (GAB-INDEX). GAS2UPD 
01025                                                                   GAS2UPD 
01026      IF PERIODI  NOT =    GAB-COINS-BENEFIT-PERIOD (GAB-INDEX)    GAS2UPD 
01027         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01028         MOVE PERIODI   TO GAB-COINS-BENEFIT-PERIOD (GAB-INDEX).   GAS2UPD 
01029                                                                   GAS2UPD 
01030      IF DEFINTNI NOT =    GAB-COINS-DEFINITION (GAB-INDEX)        GAS2UPD 
01031         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01032         MOVE DEFINTNI  TO GAB-COINS-DEFINITION (GAB-INDEX).       GAS2UPD 
01033                                                                   GAS2UPD 
01034      IF FYIVALI   NOT =  GAB-COINS-FYI-VALUE (GAB-INDEX)          GAS2UPD 
01035         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01036         MOVE FYIVALI  TO GAB-COINS-FYI-VALUE (GAB-INDEX).         GAS2UPD 
01037                                                                   GAS2UPD 
01038      IF PERTQALI  NOT =   GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX) GAS2UPD 
01039         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01040         MOVE PERTQALI  TO GAB-COINS-BEN-PER-TIME-QUAL (GAB-INDEX).GAS2UPD 
01041                                                                   GAS2UPD 
01042      IF FAMINDII  NOT =   GAB-COINS-FAM-OR-INDIV (GAB-INDEX)      GAS2UPD 
01043         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01044         MOVE FAMINDII  TO GAB-COINS-FAM-OR-INDIV (GAB-INDEX).     GAS2UPD 
01045                                                                   GAS2UPD 
01046      IF PLCTRMTI  NOT =   GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)GAS2UPD 
01047         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01048         MOVE PLCTRMTI  TO GAB-COINS-PLACE-OF-TREATMENT(GAB-INDEX).GAS2UPD 
01049                                                                   GAS2UPD 
01050      IF SRVGRUPI  NOT =   GAB-COINS-SERVICE-GROUP (GAB-INDEX)     GAS2UPD 
01051         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01052         MOVE SRVGRUPI  TO GAB-COINS-SERVICE-GROUP (GAB-INDEX).    GAS2UPD 
01053                                                                   GAS2UPD 
01054        MOVE AGELIMLI   TO ACWA-DISPLAY-LEN-3-X.                   GAS2UPD 
01055        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS2UPD 
01056                           GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX)    GAS2UPD 
01057         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01058        MOVE ACWA-DISPLAY-LEN-3                                    GAS2UPD 
01059                        TO GAB-COINS-AGE-LIMIT-FROM (GAB-INDEX).   GAS2UPD 
01060                                                                   GAS2UPD 
01061        MOVE AGELIMHI   TO ACWA-DISPLAY-LEN-3-X.                   GAS2UPD 
01062        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS2UPD 
01063                           GAB-COINS-AGE-LIMIT-TO   (GAB-INDEX)    GAS2UPD 
01064         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01065        MOVE ACWA-DISPLAY-LEN-3                                    GAS2UPD 
01066                        TO GAB-COINS-AGE-LIMIT-TO   (GAB-INDEX).   GAS2UPD 
01067                                                                   GAS2UPD 
01068      IF FEAKINDI  NOT =  GAB-COINS-FEAK-IND        (GAB-INDEX)    GAS2UPD 
01069         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01070         MOVE FEAKINDI TO GAB-COINS-FEAK-IND        (GAB-INDEX).   GAS2UPD 
01071                                                                   GAS2UPD 
01072      IF ACCUMIDI  NOT =  GAB-COINS-ACCUMID         (GAB-INDEX)    GAS2UPD 
01073         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01074         MOVE ACCUMIDI TO GAB-COINS-ACCUMID         (GAB-INDEX).   GAS2UPD 
01075                                                                   GAS2UPD 
01076      IF CAPINDI   NOT =  GAB-COINS-COMB-APPLIED-IND (GAB-INDEX)   GAS2UPD 
01077         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01078         MOVE CAPINDI  TO GAB-COINS-COMB-APPLIED-IND (GAB-INDEX).  GAS2UPD 
01079                                                                   GAS2UPD 
01080      IF SABDINDI  NOT =  GAB-COINS-SEL-ADDL-BEN-DET (GAB-INDEX)   GAS2UPD 
01081         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01082         MOVE SABDINDI TO GAB-COINS-SEL-ADDL-BEN-DET (GAB-INDEX).  GAS2UPD 
01083                                                                   GAS2UPD 
01084      IF AGEQLLI   NOT =  GAB-COINS-AGE-QUAL-IND-FROM(GAB-INDEX)   GAS2UPD 
01085         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01086         MOVE AGEQLLI  TO                                          GAS2UPD 
01087                         GAB-COINS-AGE-QUAL-IND-FROM(GAB-INDEX).   GAS2UPD 
01088                                                                   GAS2UPD 
01089      IF AGEQLHI   NOT =  GAB-COINS-AGE-QUAL-IND-TO  (GAB-INDEX)   GAS2UPD 
01090         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01091         MOVE AGEQLHI  TO                                          GAS2UPD 
01092                         GAB-COINS-AGE-QUAL-IND-TO  (GAB-INDEX).   GAS2UPD 
01093                                                                   GAS2UPD 
01094      IF RELPINDI  NOT =  GAB-COINS-RELATIONSHIP-IND (GAB-INDEX)   GAS2UPD 
01095         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01096         MOVE RELPINDI TO                                          GAS2UPD 
01097                         GAB-COINS-RELATIONSHIP-IND (GAB-INDEX).   GAS2UPD 
01098                                                                   GAS2UPD 
01099      IF CARYOVRI  NOT =  GAB-CARRY-OVER-CREDIT-IND  (GAB-INDEX)   GAS2UPD 
01100         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01101         MOVE CARYOVRI TO                                          GAS2UPD 
01102                         GAB-CARRY-OVER-CREDIT-IND  (GAB-INDEX).   GAS2UPD 
01103                                                                   GAS2UPD 
01104        MOVE PRTIMEFI   TO ACWA-DISPLAY-LEN-3-X.                   GAS2UPD 
01105        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS2UPD 
01106                           GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX) GAS2UPD 
01107         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01108        MOVE ACWA-DISPLAY-LEN-3                                    GAS2UPD 
01109                        TO GAB-COINS-BEN-PER-TIME-FCTR (GAB-INDEX).GAS2UPD 
01110                                                                   GAS2UPD 
01111      IF REININDI  NOT =   GAB-COINS-REINSTATEMENT-IND (GAB-INDEX) GAS2UPD 
01112         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01113         MOVE REININDI  TO GAB-COINS-REINSTATEMENT-IND (GAB-INDEX).GAS2UPD 
01114                                                                   GAS2UPD 
01115      IF MANAPLII  NOT =  GAB-COINS-LMT-MANDATORY-IND(GAB-INDEX)   GAS2UPD 
01116         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01117         MOVE MANAPLII TO GAB-COINS-LMT-MANDATORY-IND(GAB-INDEX).  GAS2UPD 
01118                                                                   GAS2UPD 
01119      IF FDLRCLII NOT = GAB-COINS-1ST-DOLR-COVRGE-LMT(GAB-INDEX)   GAS2UPD 
01120         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01121        MOVE FDLRCLII TO GAB-COINS-1ST-DOLR-COVRGE-LMT(GAB-INDEX). GAS2UPD 
01122                                                                   GAS2UPD 
01123      IF CLMLVLII NOT =   GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX)GAS2UPD 
01124         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01125         MOVE CLMLVLII TO GAB-COINS-CLAIM-LVL-ACCUM-IND(GAB-INDEX).GAS2UPD 
01126                                                                   GAS2UPD 
01127         MOVE INTRVALI TO ACWA-DISPLAY-LEN-3-X.                    GAS2UPD 
01128      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS2UPD 
01129                          GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX) GAS2UPD 
01130         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01131         MOVE ACWA-DISPLAY-LEN-3 TO                                GAS2UPD 
01132                         GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX). GAS2UPD 
01133                                                                   GAS2UPD 
01134      IF INTTYPEI  NOT =  GAB-COINS-INTERVAL-TYPE (GAB-INDEX)      GAS2UPD 
01135         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01136         MOVE INTTYPEI TO GAB-COINS-INTERVAL-TYPE (GAB-INDEX).     GAS2UPD 
01137                                                                   GAS2UPD 
01138      IF LOBI NOT =       GAB-COINS-L-O-B  (GAB-INDEX)             GAS2UPD 
01139         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01140         MOVE LOBI     TO GAB-COINS-L-O-B (GAB-INDEX).             GAS2UPD 
01141                                                                   GAS2UPD 
01142      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS2UPD 
01143      THEN                                                         GAS2UPD 
01144          MOVE ACWA-VAL-LIM-SCREEN TO ACWA-VALUE-LIMIT-9-9         GAS2UPD 
01145      ELSE                                                         GAS2UPD 
01146          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                GAS2UPD 
01147              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG'                   GAS2UPD 
01148          THEN                                                     GAS2UPD 
01149              MOVE -1                  TO ACWA-VALUE-LIMIT-9-9     GAS2UPD 
01145      ELSE                                                         GAS2UPD 
01146          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                GAS2UPD 
01147              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL'                   GAS2UPD 
01148          THEN                                                     GAS2UPD 
01149              MOVE -2                  TO ACWA-VALUE-LIMIT-9-9     GAS2UPD 
01150          ELSE                                                     GAS2UPD 
01151              MOVE ACWA-VAL-LIM-SCREEN-7 TO ACWA-VALUE-LIMIT-7     GAS2UPD 
01152              MOVE ACWA-VAL-LIM-SCREEN-2 TO ACWA-VALUE-LIMIT-2.    GAS2UPD 
01153                                                                   GAS2UPD 
01154      IF ACWA-VALUE-LIMIT-9 NOT = GAB-COINS-VALUE-LIMIT (GAB-INDEX)GAS2UPD 
01155         PERFORM 2600-000-PROCESS-VAL-LIMIT.                       GAS2UPD 
01156                                                                   GAS2UPD 
01157      IF ACWA-VALUE-LIMIT-9 NOT = GAB-COINS-VALUE-LIMIT (GAB-INDEX)GAS2UPD 
01158         ADD 1                  TO ACWA-FIELD-CHG-CNT              GAS2UPD 
01159        MOVE ACWA-VALUE-LIMIT-9 TO                                 GAS2UPD 
01160                                 GAB-COINS-VALUE-LIMIT(GAB-INDEX). GAS2UPD 
01161                                                                   GAS2UPD 
01162      IF BENVLQLI  NOT =   GAB-COINS-VALUE-QUALIFIER (GAB-INDEX)   GAS2UPD 
01163         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01164         MOVE BENVLQLI  TO GAB-COINS-VALUE-QUALIFIER (GAB-INDEX).  GAS2UPD 
01165                                                                   GAS2UPD 
01166      MOVE PERLIMTI     TO ACWA-DISPLAY-LEN-3-X.                   GAS2UPD 
01167      IF ACWA-DISPLAY-LEN-3  NOT =                                 GAS2UPD 
01168                       GAB-COINS-PERCENT-LEVEL (GAB-INDEX)         GAS2UPD 
01169         ADD 1 TO      ACWA-FIELD-CHG-CNT                          GAS2UPD 
01170         MOVE ACWA-DISPLAY-LEN-3 TO                                GAS2UPD 
01171                       GAB-COINS-PERCENT-LEVEL (GAB-INDEX).        GAS2UPD 
01172                                                                   GAS2UPD 
01173      MOVE NEWVALUI     TO ACWA-DISPLAY-LEN-5-X.                   GAS2UPD 
01174      IF  ACWA-DISPLAY-LEN-5 NOT =                                 GAS2UPD 
01175                       GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX)   GAS2UPD 
01176      THEN                                                         GAS2UPD 
01177          ADD 1     TO ACWA-FIELD-CHG-CNT                          GAS2UPD 
01178          MOVE ACWA-DISPLAY-LEN-5                                  GAS2UPD 
01179                    TO GAB-COINS-INTERVAL-OVRD-VALUE (GAB-INDEX).  GAS2UPD 
01180                                                                   GAS2UPD 
01181      IF OVRDINDI  NOT =   GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX) GAS2UPD 
01182         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01183         MOVE OVRDINDI  TO GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX).GAS2UPD 
01184                                                                   GAS2UPD 
01185      IF INTDESKI  =  GAB-COINS-INTERNAL-DESCRIPTOR (GAB-INDEX)    GAS2UPD 
01186         MOVE SPACE  TO  WS-INT-TAB-CHANGE-INDICATOR               GAS2UPD 
01187      ELSE                                                         GAS2UPD 
01188         ADD 1   TO  ACWA-FIELD-CHG-CNT                            GAS2UPD 
01189         IF INTDESKI  =  IDPRODI                                   GAS2UPD 
01190            MOVE 'NP'   TO  WS-INT-TAB-CHANGE-INDICATOR            GAS2UPD 
01191            MOVE INTDESKI  TO                                      GAS2UPD 
01192                           GAB-COINS-INTERNAL-DESCRIPTOR(GAB-INDEX)GAS2UPD 
01193         ELSE                                                      GAS2UPD 
01194            IF IDPRODI  =  GAB-COINS-INTERNAL-DESCRIPTOR(GAB-INDEX)GAS2UPD 
01195               MOVE 'PN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS2UPD 
01196               MOVE INTDESKI  TO                                   GAS2UPD 
01197                           GAB-COINS-INTERNAL-DESCRIPTOR(GAB-INDEX)GAS2UPD 
01198            ELSE                                                   GAS2UPD 
01199               MOVE 'NN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS2UPD 
01200               MOVE INTDESKI  TO                                   GAS2UPD 
01201                          GAB-COINS-INTERNAL-DESCRIPTOR(GAB-INDEX).GAS2UPD 
01202                                                                   GAS2UPD 
01203                                                                   GAS2UPD 
01204      IF CONDALLI  NOT =   GAB-COND-ALL-BIT (GAB-INDEX)            GAS2UPD 
01205         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01206         MOVE CONDALLI  TO GAB-COND-ALL-BIT (GAB-INDEX).           GAS2UPD 
01207                                                                   GAS2UPD 
01208      IF CONDEXCI  NOT =   GAB-COND-EXCLUSION-BIT (GAB-INDEX)      GAS2UPD 
01209         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01210         MOVE CONDEXCI  TO GAB-COND-EXCLUSION-BIT (GAB-INDEX).     GAS2UPD 
01211                                                                   GAS2UPD 
01212      IF CONDICDI  NOT =   GAB-COND-ICD-BIT (GAB-INDEX)            GAS2UPD 
01213         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01214         MOVE CONDICDI  TO GAB-COND-ICD-BIT (GAB-INDEX).           GAS2UPD 
01215                                                                   GAS2UPD 
01216      IF CONDTABI  NOT =   GAB-COND-TB-BIT (GAB-INDEX)             GAS2UPD 
01217         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01218         MOVE CONDTABI  TO GAB-COND-TB-BIT (GAB-INDEX).            GAS2UPD 
01219                                                                   GAS2UPD 
01220      IF CONDMENI  NOT =   GAB-COND-MENTAL-BIT (GAB-INDEX)         GAS2UPD 
01221         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01222         MOVE CONDMENI  TO GAB-COND-MENTAL-BIT (GAB-INDEX).        GAS2UPD 
01223                                                                   GAS2UPD 
01224      IF CONDDRGI  NOT =   GAB-COND-DRUG-BIT (GAB-INDEX)           GAS2UPD 
01225         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01226         MOVE CONDDRGI  TO GAB-COND-DRUG-BIT (GAB-INDEX).          GAS2UPD 
01227                                                                   GAS2UPD 
01228      IF CONDALCI  NOT =   GAB-COND-ALCOHOL-BIT (GAB-INDEX)        GAS2UPD 
01229         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01230         MOVE CONDALCI  TO GAB-COND-ALCOHOL-BIT (GAB-INDEX).       GAS2UPD 
01231                                                                   GAS2UPD 
01232      IF CONDOBCI  NOT =   GAB-COND-OB-COMP-BIT (GAB-INDEX)        GAS2UPD 
01233         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01234         MOVE CONDOBCI  TO GAB-COND-OB-COMP-BIT (GAB-INDEX).       GAS2UPD 
01235                                                                   GAS2UPD 
01236      IF CONDOBNI  NOT =   GAB-COND-OB-NORM-BIT (GAB-INDEX)        GAS2UPD 
01237         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01238         MOVE CONDOBNI  TO GAB-COND-OB-NORM-BIT (GAB-INDEX).       GAS2UPD 
01239                                                                   GAS2UPD 
01240      IF CONDMALI  NOT =   GAB-COND-MALIGNANCY-BIT (GAB-INDEX)     GAS2UPD 
01241         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS2UPD 
01242         MOVE CONDMALI  TO GAB-COND-MALIGNANCY-BIT (GAB-INDEX).    GAS2UPD 
01243                                                                   GAS2UPD 
01244      IF CONDCARI  NOT =  GAB-COND-CARDIAC-DISEASE-BIT (GAB-INDEX) GAS2UPD 
01245         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01246        MOVE CONDCARI  TO GAB-COND-CARDIAC-DISEASE-BIT (GAB-INDEX).GAS2UPD 
01247                                                                   GAS2UPD 
01248      IF CONDOBSI  NOT =  GAB-COND-OBESITY-BIT (GAB-INDEX)         GAS2UPD 
01249         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01250         MOVE CONDOBSI TO GAB-COND-OBESITY-BIT (GAB-INDEX).        GAS2UPD 
01251                                                                   GAS2UPD 
01252      IF CONDKDYI  NOT =  GAB-COND-KIDNEY-DISEASE-BIT (GAB-INDEX)  GAS2UPD 
01253         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS2UPD 
01254         MOVE CONDKDYI TO GAB-COND-KIDNEY-DISEASE-BIT (GAB-INDEX). GAS2UPD 
01255                                                                   GAS2UPD 
01256      IF CONDACCI  NOT  =   GAB-COND-ACCIDENT-BIT (GAB-INDEX)      GAS2UPD 
01257         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01258         MOVE CONDACCI  TO  GAB-COND-ACCIDENT-BIT (GAB-INDEX).     GAS2UPD 
01259                                                                   GAS2UPD 
01260      IF CONDPECI  NOT  =  GAB-COND-PRE-EXIST-BIT (GAB-INDEX)      GAS2UPD 
01261         ADD  1        TO  ACWA-FIELD-CHG-CNT                      GAS2UPD 
01262         MOVE CONDPECI TO  GAB-COND-PRE-EXIST-BIT (GAB-INDEX).     GAS2UPD 
01263                                                                   GAS2UPD 
01264      IF CONDNEMI  NOT  =   GAB-COND-NON-EMER-BIT (GAB-INDEX)      GAS2UPD 
01265         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01266         MOVE CONDNEMI  TO  GAB-COND-NON-EMER-BIT (GAB-INDEX).     GAS2UPD 
01267                                                                   GAS2UPD 
01268      IF CONDSUII  NOT  =   GAB-COND-SUICIDE-BIT  (GAB-INDEX)      GAS2UPD 
01269         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01270         MOVE CONDSUII  TO  GAB-COND-SUICIDE-BIT  (GAB-INDEX).     GAS2UPD 
01271                                                                   GAS2UPD 
01272      IF CONDTMJI  NOT  =   GAB-COND-TMJ-BIT      (GAB-INDEX)      GAS2UPD 
01273         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01274         MOVE CONDTMJI  TO  GAB-COND-TMJ-BIT      (GAB-INDEX).     GAS2UPD 
01275                                                                   GAS2UPD 
01276      IF CONDINFI  NOT  =   GAB-COND-INF-BIT      (GAB-INDEX)      GAS2UPD 
01277         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01278         MOVE CONDINFI  TO  GAB-COND-INF-BIT      (GAB-INDEX).     GAS2UPD 
01279                                                                   GAS2UPD 
01280      IF CONDLIFI  NOT  =   GAB-COND-LIFE-THREAT-BIT  (GAB-INDEX)  GAS2UPD 
01281         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01282         MOVE CONDLIFI  TO  GAB-COND-LIFE-THREAT-BIT  (GAB-INDEX). GAS2UPD 
01283                                                                   GAS2UPD 
01284      IF CONDEMCI  NOT  =   GAB-COND-EMER-MED-BIT     (GAB-INDEX)  GAS2UPD 
01285         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01286         MOVE CONDEMCI  TO  GAB-COND-EMER-MED-BIT     (GAB-INDEX). GAS2UPD 
01287                                                                   GAS2UPD 
01288      IF CONDEACI  NOT  =   GAB-COND-EMER-ACC-BIT     (GAB-INDEX)  GAS2UPD 
01289         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01290         MOVE CONDEACI  TO  GAB-COND-EMER-ACC-BIT     (GAB-INDEX). GAS2UPD 
01291                                                                   GAS2UPD 
01292      IF CONDSMII  NOT  =   GAB-COND-SER-MEN-ILL-BIT  (GAB-INDEX)  GAS2UPD 
01293         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01294         MOVE CONDSMII  TO  GAB-COND-SER-MEN-ILL-BIT  (GAB-INDEX). GAS2UPD 
01295                                                                   GAS2UPD 
01296      IF CONDNSMI  NOT  = GAB-COND-NON-SER-MEN-ILL-BIT (GAB-INDEX) GAS2UPD 
01297         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS2UPD 
01298         MOVE CONDNSMI  TO GAB-COND-NON-SER-MEN-ILL-BIT (GAB-INDEX)GAS2UPD 
01299                                                                   GAS2UPD 
01300      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
01301         MOVE GCIO-WRK-TABULAR-PROVISION  TO                       GAS2UPD 
01302                                     GCIO-WRK-BENEFIT-PROVISION.   GAS2UPD 
01303                                                                   GAS2UPD 
01304 ******* IF THE OCCUR IS A NEW ADDED ONE THEN IT IS FLAGED 1U      GAS2UPD 
01305 *** IN GA1BPGM ALL ATTACHED INTERNAL TABS TO THIS ADDED OCCUR     GAS2UPD 
01306 *** ALSO WILL BE FLAGED 1U IN THIS ROUTINE    NE 08/03/88         GAS2UPD 
01307                                                                   GAS2UPD 
01308      PERFORM  5000-000-READ-PROD-ALL-LVL-TAB.                     GAS2UPD 
01309         SEARCH GAB2-ENTRY                                         GAS2UPD 
01310            VARYING GAB2-INDEX                                     GAS2UPD 
01311            WHEN                                                   GAS2UPD 
01312               GAB2-INDEX NOT <  GAB2-ENTRY-COUNT  OR              GAS2UPD 
01313               GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX)  =            GAS2UPD 
01314                             GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)   GAS2UPD 
01315               NEXT SENTENCE.                                      GAS2UPD 
01316                                                                   GAS2UPD 
01317         IF GAB2-INDEX <  GAB2-ENTRY-COUNT  AND                    GAS2UPD 
01318            GAB2-OCCURS-ENTRY-COUNTER(GAB2-INDEX)  =               GAS2UPD 
01319                             GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)   GAS2UPD 
01320            MOVE 'N'      TO  WS-NEW-OCCR-ON-WF                    GAS2UPD 
01321         ELSE                                                      GAS2UPD 
01322            MOVE 'Y'      TO WS-NEW-OCCR-ON-WF.                    GAS2UPD 
01323 ******************************************************** 8/3/88   GAS2UPD 
01324      IF ACWA-INTERNAL-TAB-CHANGE-ONLY                             GAS2UPD 
01325         GO TO 2200-260-CHANGE-INTERNAL-TAB.                       GAS2UPD 
01326                                                                   GAS2UPD 
01327 *** CHECK LVL2-B-SWITCH                                           GAS2UPD 
01328      IF EIBAID    =       DFHENTER  AND                           GAS2UPD 
01329         DELADDI   =      'CHG/ADD'  AND                           GAS2UPD 
01330         OENTCTRI  NOT =  '0000000'  AND                           GAS2UPD 
01331         ACWA-NO-CHANGE-FOUND                                      GAS2UPD 
01332         MOVE 'Y'   TO  LVL2-B-SW                                  GAS2UPD 
01333         PERFORM 3100-RLSE-RU-GAB-REC                              GAS2UPD 
01334         GO  TO  2200-900-EXIT.                                    GAS2UPD 
01335                                                                   GAS2UPD 
01336      IF  EIBAID  =  DFHENTER       AND                            GAS2UPD 
01337          ACWA-SCREEN-HAS-NO-ERRORS AND                            GAS2UPD 
01338          GCVI-TABLE-SW = 'N'                                      GAS2UPD 
01339      THEN                                                         GAS2UPD 
01340          IF  ACWA-NO-CHANGE-FOUND                                 GAS2UPD 
01341          THEN                                                     GAS2UPD 
01342              SET  WT-01-INDEX                     TO +11          GAS2UPD 
01343              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS2UPD 
01344              PERFORM 7900-000-RESET-ATTRIBUTES                    GAS2UPD 
01345              MOVE -1 TO PERIODL                                   GAS2UPD 
01346              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS2UPD 
01347          ELSE                                                     GAS2UPD 
01348              SET  WT-01-INDEX                     TO +07          GAS2UPD 
01349              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS2UPD 
01350              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS2UPD 
01351      ELSE                                                         GAS2UPD 
01352          NEXT SENTENCE.                                           GAS2UPD 
01353                                                                   GAS2UPD 
01354      IF (EIBAID  =  DFHPF4 OR  DFHPF16) AND                       GAS2UPD 
01355          ACWA-SCREEN-HAS-NO-ERRORS      AND                       GAS2UPD 
01356          GCVI-TABLE-SW = 'N'            AND                       GAS2UPD 
01357          ACWA-NO-CHANGE-FOUND                                     GAS2UPD 
01358      THEN                                                         GAS2UPD 
01359          SET  WT-01-INDEX                     TO +09              GAS2UPD 
01360          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS2UPD 
01361          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS2UPD 
01362          MOVE -1 TO PERIODL                                       GAS2UPD 
01363          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS2UPD 
01364                                                                   GAS2UPD 
01365      IF  EIBAID   =   DFHENTER AND                                GAS2UPD 
01366          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS2UPD 
01367          ACWA-NO-CHANGE-FOUND                                     GAS2UPD 
01368      THEN                                                         GAS2UPD 
01369          SET  WT-01-INDEX                     TO +11              GAS2UPD 
01370          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS2UPD 
01371          MOVE -1 TO PERIODL                                       GAS2UPD 
01372          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS2UPD 
01373                                                                   GAS2UPD 
01374      PERFORM 7900-000-RESET-ATTRIBUTES.                           GAS2UPD 
01375                                                                   GAS2UPD 
01376 *** LVL2-F-SWITCH                                                 GAS2UPD 
01377      IF  (EIBAID  =   DFHPF7 OR DFHPF19) AND                      GAS2UPD 
01378          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS2UPD 
01379          ACWA-NO-CHANGE-FOUND                                     GAS2UPD 
01380      THEN                                                         GAS2UPD 
01381          MOVE  'Y'   TO  LVL2-F-SW                                GAS2UPD 
01382         PERFORM 3100-RLSE-RU-GAB-REC                              GAS2UPD 
01383          GO TO  2200-900-EXIT.                                    GAS2UPD 
01384                                                                   GAS2UPD 
01385 *** LVL2-G-SWITCH                                                 GAS2UPD 
01386      IF  (EIBAID  =  DFHPF8 OR DFHPF20) AND                       GAS2UPD 
01387          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS2UPD 
01388          ACWA-NO-CHANGE-FOUND                                     GAS2UPD 
01389      THEN                                                         GAS2UPD 
01390          MOVE  'Y'   TO  LVL2-G-SW                                GAS2UPD 
01391         PERFORM 3100-RLSE-RU-GAB-REC                              GAS2UPD 
01392          GO TO  2200-900-EXIT.                                    GAS2UPD 
01393                                                                   GAS2UPD 
01394      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS2UPD 
01395         DELADDI  =  'CHG/DEL'                                     GAS2UPD 
01396         PERFORM 4600-000-UPDATE-CDE-STATUS.                       GAS2UPD 
01397                                                                   GAS2UPD 
01398      IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE OR             GAS2UPD 
01399          (IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999')) AND      GAS2UPD 
01400          (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE OR             GAS2UPD 
01401          (IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999')) AND      GAS2UPD 
01402          (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE OR             GAS2UPD 
01403          (IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999')) AND      GAS2UPD 
01404          (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE OR             GAS2UPD 
01405          (IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999')) AND      GAS2UPD 
01406          (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE OR             GAS2UPD 
01407          (IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999')) AND      GAS2UPD 
01408          (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE OR             GAS2UPD 
01409          (IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'))          GAS2UPD 
01410      THEN                                                         GAS2UPD 
01411          GO TO 2200-250-UPDATE-ALL-LVL-TAB.                       GAS2UPD 
01412                                                                   GAS2UPD 
01413      IF  IBGROPTI  =  'C' OR                                      GAS2UPD 
01414          IDGDOPTI  =  'C' OR                                      GAS2UPD 
01415          IPGNOPTI  =  'C' OR                                      GAS2UPD 
01416          IPGPOPTI  =  'C' OR                                      GAS2UPD 
01417          IPGTOPTI  =  'C' OR                                      GAS2UPD 
01418          IPGSOPTI  =  'C'                                         GAS2UPD 
01419      THEN                                                         GAS2UPD 
01420          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS2UPD 
01421                                                                   GAS2UPD 
01422      IF  (IBGROPTI  =  'MT' OR 'A') AND IBGRSLTI  =  '0000000' OR GAS2UPD 
01423          (IDGDOPTI  =  'MT' OR 'A') AND IDGDSLTI  =  '0000000' OR GAS2UPD 
01424          (IPGNOPTI  =  'MT' OR 'A') AND IPGNSLTI  =  '0000000' OR GAS2UPD 
01425          (IPGPOPTI  =  'MT' OR 'A') AND IPGPSLTI  =  '0000000' OR GAS2UPD 
01426          (IPGTOPTI  =  'MT' OR 'A') AND IPGTSLTI  =  '0000000' OR GAS2UPD 
01427          (IPGSOPTI  =  'MT' OR 'A') AND IPGSSLTI  =  '0000000'    GAS2UPD 
01428      THEN                                                         GAS2UPD 
01429          GO TO 2200-220-ADD-INTERNAL-OCCURS.                      GAS2UPD 
01430                                                                   GAS2UPD 
01431      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01432          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01433          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01434          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01435          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS2UPD 
01436          (IPGSOPTI  =  'MT' OR 'A')                               GAS2UPD 
01437      THEN                                                         GAS2UPD 
01438          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS2UPD 
01439                                                                   GAS2UPD 
01440      IF  IBGROPTI  =  'D'                                         GAS2UPD 
01441      THEN                                                         GAS2UPD 
01442          MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID             GAS2UPD 
01443          MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO             GAS2UPD 
01444      ELSE                                                         GAS2UPD 
01445          IF  IPGNOPTI  =  'D'                                     GAS2UPD 
01446          THEN                                                     GAS2UPD 
01447              MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID         GAS2UPD 
01448              MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO         GAS2UPD 
01449          ELSE                                                     GAS2UPD 
01450              IF  IPGTOPTI  =  'D'                                 GAS2UPD 
01451              THEN                                                 GAS2UPD 
01452                  MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS2UPD 
01453                  MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS2UPD 
01454              ELSE                                                 GAS2UPD 
01455              IF  IPGSOPTI  =  'D'                                 GAS2UPD 
01456              THEN                                                 GAS2UPD 
01457                  MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS2UPD 
01458                  MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS2UPD 
01459              ELSE                                                 GAS2UPD 
01460              IF  IDGDOPTI  =  'D'                                 GAS2UPD 
01461              THEN                                                 GAS2UPD 
01462                  MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS2UPD 
01463                  MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS2UPD 
01464              ELSE                                                 GAS2UPD 
01465              IF  IPGPOPTI  =  'D'                                 GAS2UPD 
01466              THEN                                                 GAS2UPD 
01467                  MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS2UPD 
01468                  MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS2UPD 
01469              ELSE                                                 GAS2UPD 
01470                  NEXT SENTENCE.                                   GAS2UPD 
01471                                                                   GAS2UPD 
01472                                                                   GAS2UPD 
01473      IF    GAB-COINS-INTL-TAB-1 (GAB-INDEX)                       GAS2UPD 
01474          = GCIO-WRK-TABULAR-PROVISION                             GAS2UPD 
01475      THEN                                                         GAS2UPD 
01476          MOVE GAB-COINS-INTL-TAB-2 (GAB-INDEX)                    GAS2UPD 
01477            TO GAB-COINS-INTL-TAB-1 (GAB-INDEX)                    GAS2UPD 
01478          MOVE GAB-COINS-INTL-TAB-3 (GAB-INDEX)                    GAS2UPD 
01479            TO GAB-COINS-INTL-TAB-2 (GAB-INDEX)                    GAS2UPD 
01480          MOVE GAB-COINS-INTL-TAB-4 (GAB-INDEX)                    GAS2UPD 
01481            TO GAB-COINS-INTL-TAB-3 (GAB-INDEX)                    GAS2UPD 
01482          MOVE GAB-COINS-INTL-TAB-5 (GAB-INDEX)                    GAS2UPD 
01483            TO GAB-COINS-INTL-TAB-4 (GAB-INDEX)                    GAS2UPD 
01484 ******** MOVE HIGH-VALUES                                         GAS2UPD 
01485 ********   TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)                    GAS2UPD 
01486          IF GAB-COINS-INTL-TAB-4 (GAB-INDEX)                      GAS2UPD 
01487                       = HIGH-VALUES OR WS-SPACES-ZEROS            GAS2UPD 
01488             MOVE WS-SPACES-ZEROS                                  GAS2UPD 
01489               TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)                 GAS2UPD 
01490          ELSE                                                     GAS2UPD 
01491             MOVE HIGH-VALUES                                      GAS2UPD 
01492               TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)                 GAS2UPD 
01493          END-IF                                                   GAS2UPD 
01494      ELSE                                                         GAS2UPD 
01495          IF   GAB-COINS-INTL-TAB-2 (GAB-INDEX)                    GAS2UPD 
01496             = GCIO-WRK-TABULAR-PROVISION                          GAS2UPD 
01497         THEN                                                      GAS2UPD 
01498             MOVE GAB-COINS-INTL-TAB-3 (GAB-INDEX)                 GAS2UPD 
01499               TO GAB-COINS-INTL-TAB-2 (GAB-INDEX)                 GAS2UPD 
01500             MOVE GAB-COINS-INTL-TAB-4 (GAB-INDEX)                 GAS2UPD 
01501               TO GAB-COINS-INTL-TAB-3 (GAB-INDEX)                 GAS2UPD 
01502             MOVE GAB-COINS-INTL-TAB-5 (GAB-INDEX)                 GAS2UPD 
01503               TO GAB-COINS-INTL-TAB-4 (GAB-INDEX)                 GAS2UPD 
01504 *********** MOVE HIGH-VALUES                                      GAS2UPD 
01505 ***********   TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)                 GAS2UPD 
01506             IF GAB-COINS-INTL-TAB-4 (GAB-INDEX)                   GAS2UPD 
01507                          = HIGH-VALUES OR WS-SPACES-ZEROS         GAS2UPD 
01508                MOVE WS-SPACES-ZEROS                               GAS2UPD 
01509                  TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)              GAS2UPD 
01510             ELSE                                                  GAS2UPD 
01511                MOVE HIGH-VALUES                                   GAS2UPD 
01512                  TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)              GAS2UPD 
01513             END-IF                                                GAS2UPD 
01514         ELSE                                                      GAS2UPD 
01515             IF    GAB-COINS-INTL-TAB-3 (GAB-INDEX)                GAS2UPD 
01516                 = GCIO-WRK-TABULAR-PROVISION                      GAS2UPD 
01517             THEN                                                  GAS2UPD 
01518                 MOVE GAB-COINS-INTL-TAB-4 (GAB-INDEX)             GAS2UPD 
01519                   TO GAB-COINS-INTL-TAB-3 (GAB-INDEX)             GAS2UPD 
01520                 MOVE GAB-COINS-INTL-TAB-5 (GAB-INDEX)             GAS2UPD 
01521                   TO GAB-COINS-INTL-TAB-4 (GAB-INDEX)             GAS2UPD 
01522 *************** MOVE HIGH-VALUES                                  GAS2UPD 
01523 ***************   TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)             GAS2UPD 
01524                 IF GAB-COINS-INTL-TAB-4 (GAB-INDEX)               GAS2UPD 
01525                              = HIGH-VALUES OR WS-SPACES-ZEROS     GAS2UPD 
01526                    MOVE WS-SPACES-ZEROS                           GAS2UPD 
01527                      TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)          GAS2UPD 
01528                 ELSE                                              GAS2UPD 
01529                    MOVE HIGH-VALUES                               GAS2UPD 
01530                      TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)          GAS2UPD 
01531                 END-IF                                            GAS2UPD 
01532             ELSE                                                  GAS2UPD 
01533                 IF    GAB-COINS-INTL-TAB-4 (GAB-INDEX)            GAS2UPD 
01534                     = GCIO-WRK-TABULAR-PROVISION                  GAS2UPD 
01535                 THEN                                              GAS2UPD 
01536                     MOVE GAB-COINS-INTL-TAB-5 (GAB-INDEX)         GAS2UPD 
01537                       TO GAB-COINS-INTL-TAB-4 (GAB-INDEX)         GAS2UPD 
01538 ******************* MOVE HIGH-VALUES                              GAS2UPD 
01539 *******************   TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)         GAS2UPD 
01540                     IF GAB-COINS-INTL-TAB-4 (GAB-INDEX)           GAS2UPD 
01541                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS2UPD 
01542                        MOVE WS-SPACES-ZEROS                       GAS2UPD 
01543                          TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)      GAS2UPD 
01544                     ELSE                                          GAS2UPD 
01545                        MOVE HIGH-VALUES                           GAS2UPD 
01546                          TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)      GAS2UPD 
01547                     END-IF                                        GAS2UPD 
01548             ELSE                                                  GAS2UPD 
01549                 IF    GAB-COINS-INTL-TAB-5 (GAB-INDEX)            GAS2UPD 
01550                     = GCIO-WRK-TABULAR-PROVISION                  GAS2UPD 
01551                 THEN                                              GAS2UPD 
01552 ******************* MOVE HIGH-VALUES                              GAS2UPD 
01553 *******************   TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)         GAS2UPD 
01554                     IF GAB-COINS-INTL-TAB-4 (GAB-INDEX)           GAS2UPD 
01555                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS2UPD 
01556                        MOVE WS-SPACES-ZEROS                       GAS2UPD 
01557                          TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)      GAS2UPD 
01558                     ELSE                                          GAS2UPD 
01559                        MOVE HIGH-VALUES                           GAS2UPD 
01560                          TO GAB-COINS-INTL-TAB-5 (GAB-INDEX)      GAS2UPD 
01561                     END-IF                                        GAS2UPD 
01562                 ELSE                                              GAS2UPD 
01563                     MOVE WS-ABCODE-1CL2        TO WS-ABCODE       GAS2UPD 
01564                     MOVE WS-ABCODE-1CL2-MSG    TO WS-ABCODE-MSG   GAS2UPD 
01565                     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.        GAS2UPD 
01566                                                                   GAS2UPD 
01567 **** SUBTRACT  1  FROM  GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX).   GAS2UPD 
01568      IF GAB-COINS-INTL-TAB-5 (GAB-INDEX) = HIGH-VALUES            GAS2UPD 
01569         NEXT SENTENCE                                             GAS2UPD 
01570      ELSE                                                         GAS2UPD 
01571         SUBTRACT  1  FROM  GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX).GAS2UPD 
01572                                                                   GAS2UPD 
01573      GO TO 2200-250-UPDATE-ALL-LVL-TAB.                           GAS2UPD 
01574                                                                   GAS2UPD 
01575                                                                   GAS2UPD 
01576  2200-220-ADD-INTERNAL-OCCURS.                                    GAS2UPD 
01577                                                                   GAS2UPD 
01578 **** IF GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX) > 5                GAS2UPD 
01579      IF GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX) = 5 AND            GAS2UPD 
01580**       GAB-INT-TS (GAB-INDEX 5) NOT = HIGH-VALUES                GAS2UPD 
01580         GAB-INT-ID (GAB-INDEX 5) NOT = HIGH-VALUES                GAS2UPD 
01581         SET  WT-01-INDEX                     TO +26               GAS2UPD 
01582         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           GAS2UPD 
01583         MOVE SPACES TO  IBGROPTO, IPGPOPTO, IDGDOPTO,             GAS2UPD 
01584                         IPGTOPTO, IPGNOPTO, IPGSOPTO              GAS2UPD 
01585         MOVE SPACES TO  MFRMSLTO                                  GAS2UPD 
01586         MOVE -1 TO PERIODL                                        GAS2UPD 
01587         PERFORM 9010-000-SEND-DATAONLY-RETURN.                    GAS2UPD 
01588                                                                   GAS2UPD 
01589      MOVE GXA-PROVISION-ID          TO     WS-SAVE-INTL-TAB-ID.   GAS2UPD 
01590      MOVE GXA-PROVISION-SLOT-NO     TO     WS-TAB-PROV-COPY-SLOT. GAS2UPD 
01591      MOVE GAB-OCCURS-ENTRY-COUNTER(GAB-INDEX)  TO                 GAS2UPD 
01592                                            WS-SAVE-INTL-TAB-SLOT. GAS2UPD 
01593                                                                   GAS2UPD 
01594      SET GAB-INT-INDEX  TO  1.                                    GAS2UPD 
01595      SEARCH GAB-INT-TS                                            GAS2UPD 
01596         VARYING GAB-INT-INDEX                                     GAS2UPD 
01597         AT END                                                    GAS2UPD 
01598            MOVE WS-ABCODE-1CL3      TO  WS-ABCODE                 GAS2UPD 
01599            MOVE WS-ABCODE-1CL3-MSG  TO  WS-ABCODE-MSG             GAS2UPD 
01600            PERFORM 9800-000-ERROR-MSG-THEN-ABEND                  GAS2UPD 
01601         WHEN                                                      GAS2UPD 
01602            GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  NOT <             GAS2UPD 
01603                                                 WS-SAVE-INTL-TAB  GAS2UPD 
01604            NEXT SENTENCE.                                         GAS2UPD 
01605                                                                   GAS2UPD 
01606      IF GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  NOT =                GAS2UPD 
01607                                                  WS-SAVE-INTL-TAB GAS2UPD 
01608         PERFORM 2200-225-SHIFT-OCCURS-UP                          GAS2UPD 
01609            VARYING GAB-INT-INDEX  FROM  GAB-INT-INDEX  BY  1      GAS2UPD 
01610            UNTIL GAB-INT-INDEX  >  5                              GAS2UPD 
01611 ******* ADD  1  TO  GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)        GAS2UPD 
01612         IF GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX) < 5             GAS2UPD 
01613            ADD  1  TO  GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)     GAS2UPD 
01614         END-IF                                                    GAS2UPD 
01615      ELSE                                                         GAS2UPD 
01616         MOVE WS-SAVE-INTL-TAB  TO                                 GAS2UPD 
01617                               GAB-INT-TS(GAB-INDEX GAB-INT-INDEX).GAS2UPD 
01618                                                                   GAS2UPD 
01619      GO TO 2200-240-SETUP-GCIO-PARMS.                             GAS2UPD 
01620                                                                   GAS2UPD 
01621                                                                   GAS2UPD 
01622  2200-225-SHIFT-OCCURS-UP.                                        GAS2UPD 
01623      MOVE GAB-INT-TS(GAB-INDEX GAB-INT-INDEX)  TO  WS-INTL-TAB-ID.GAS2UPD 
01624      MOVE WS-SAVE-INTL-TAB  TO                                    GAS2UPD 
01625                             GAB-INT-TS(GAB-INDEX GAB-INT-INDEX).  GAS2UPD 
01626                                                                   GAS2UPD 
01627      MOVE WS-INTL-TAB-ID  TO  WS-SAVE-INTL-TAB.                   GAS2UPD 
01628                                                                   GAS2UPD 
01629                                                                   GAS2UPD 
01630  2200-230-CHANGE-PROD-SLOT-NO.                                    GAS2UPD 
01631                                                                   GAS2UPD 
01632      MOVE GXA-PROVISION-SLOT-NO  TO  WS-TAB-PROV-COPY-SLOT.       GAS2UPD 
01633      SET  GAB-INT-INDEX TO      1.                                GAS2UPD 
01634      SET  GAB-INT-INDEX DOWN BY 1.                                GAS2UPD 
01635                                                                   GAS2UPD 
01636  2200-240-CHANGE-LOOP.                                            GAS2UPD 
01637                                                                   GAS2UPD 
01638      SET GAB-INT-INDEX UP BY 1.                                   GAS2UPD 
01639      IF  GAB-INT-INDEX > 5                                        GAS2UPD 
01640          GO TO 2200-240-SETUP-GCIO-PARMS.                         GAS2UPD 
01641                                                                   GAS2UPD 
01642      IF  GAB-INT-ID (GAB-INDEX GAB-INT-INDEX) = GXA-PROVISION-ID  GAS2UPD 
01643      THEN                                                         GAS2UPD 
01644          MOVE GAB-OCCURS-ENTRY-COUNTER (GAB-INDEX)                GAS2UPD 
01645            TO GXA-PROVISION-SLOT-NO                               GAS2UPD 
01646               GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)              GAS2UPD 
01647          GO TO 2200-240-SETUP-GCIO-PARMS                          GAS2UPD 
01648      ELSE                                                         GAS2UPD 
01649          GO TO 2200-240-CHANGE-LOOP.                              GAS2UPD 
01650                                                                   GAS2UPD 
01651                                                                   GAS2UPD 
01652  2200-240-SETUP-GCIO-PARMS.                                       GAS2UPD 
01653                                                                   GAS2UPD 
01654      IF FRMNUIDI  =  'GS3A'                                       GAS2UPD 
01655         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS2UPD 
01656      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS2UPD 
01657         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS2UPD 
01658      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
01659         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS2UPD 
01660         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS2UPD 
01661         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS2UPD 
01662                                                                   GAS2UPD 
01663      MOVE GC-GCPSWORK-DDNAME   TO  GCIO2-FILE-DDNAME.             GAS2UPD 
01664      MOVE GC-GCIO-AREA-1       TO  GCIO2-IO-AREA-TO-USE.          GAS2UPD 
01665      MOVE GXA-PROVISION-ID     TO  GCIO-WRK-TAB-PROVISION-ID.     GAS2UPD 
01666      MOVE GAB-OCCURS-ENTRY-COUNTER (GAB-INDEX)                    GAS2UPD 
01667                                  TO  GCIO-WRK-TAB-PROV-SLOT-NO    GAS2UPD 
01668                                      GXA-PROVISION-SLOT-NO.       GAS2UPD 
01669      MOVE GCIO-WORKFILE-KEY    TO  GCIO2-FILE-KEY                 GAS2UPD 
01670                                    WORK-RECORD-2.                 GAS2UPD 
01671      MOVE WS-TAB-PROV-COPY-SLOT  TO  WRK2-PROV-POOL-COPY-SLOT.    GAS2UPD 
01672                                                                   GAS2UPD 
01673      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
01674         MOVE GCA-BEN-PROV-ID  TO  WRK2-ALL-LEV-BEN-PROV.          GAS2UPD 
01675                                                                   GAS2UPD 
01676      MOVE GC-GCIO-ACCESS-CODE-WR  TO  GCIO2-FILE-ACCESS-CODE.     GAS2UPD 
01677                                                                   GAS2UPD 
01678      COMPUTE WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                    GAS2UPD 
01679               GC-GCIOPARM-LEN  +   GCIO2-RECORD-LENGTH.           GAS2UPD 
01680                                                                   GAS2UPD 
01681                                                                   GAS2UPD 
01682  2200-250-UPDATE-ALL-LVL-TAB.                                     GAS2UPD 
01683                                                                   GAS2UPD 
01684 *******                                                           GAS2UPD 
01685 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS2UPD 
01686 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLE UPDATES THEGAS2UPD 
01687 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS2UPD 
01688 *******                                                           GAS2UPD 
01689                                                                   GAS2UPD 
01690      IF  IBGROPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS2UPD 
01691      THEN                                                         GAS2UPD 
01692          MOVE MFRMSLTI     TO  IBGRSLTI,   ACWA-DISPLAY-LEN-7     GAS2UPD 
01693          SET  WT-01-INDEX  TO  +01                                GAS2UPD 
01694          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
01695          MOVE -1           TO  IBGROPTL                           GAS2UPD 
01696          MOVE DFHBMABF     TO  IBGRSLTA,   IBGRIDA                GAS2UPD 
01697          MOVE SPACES       TO  IBGROPTI                           GAS2UPD 
01698          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS2UPD 
01699          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS2UPD 
01700          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS2UPD 
01701      IF  IDGDOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS2UPD 
01702      THEN                                                         GAS2UPD 
01703          MOVE MFRMSLTI     TO  IDGDSLTI,   ACWA-DISPLAY-LEN-7     GAS2UPD 
01704          SET  WT-01-INDEX  TO  +23                                GAS2UPD 
01705          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
01706          MOVE -1           TO  IDGDOPTL                           GAS2UPD 
01707          MOVE DFHBMABF     TO  IDGDSLTA,   IDGDIDA                GAS2UPD 
01708          MOVE SPACES       TO  IDGDOPTI                           GAS2UPD 
01709          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS2UPD 
01710          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS2UPD 
01711          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS2UPD 
01712      IF  IPGNOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS2UPD 
01713      THEN                                                         GAS2UPD 
01714          MOVE MFRMSLTI     TO  IPGNSLTI,   ACWA-DISPLAY-LEN-7     GAS2UPD 
01715          SET  WT-01-INDEX  TO  +02                                GAS2UPD 
01716          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
01717          MOVE -1           TO  IPGNOPTL                           GAS2UPD 
01718          MOVE DFHBMABF     TO  IPGNSLTA,   IPGNIDA                GAS2UPD 
01719          MOVE SPACES       TO  IPGNOPTI                           GAS2UPD 
01720          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS2UPD 
01721          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS2UPD 
01722          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS2UPD 
01723      IF  IPGPOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS2UPD 
01724      THEN                                                         GAS2UPD 
01725          MOVE MFRMSLTI     TO  IPGPSLTI,   ACWA-DISPLAY-LEN-7     GAS2UPD 
01726          SET  WT-01-INDEX  TO  +24                                GAS2UPD 
01727          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
01728          MOVE -1           TO  IPGPOPTL                           GAS2UPD 
01729          MOVE DFHBMABF     TO  IPGPSLTA,   IPGPIDA                GAS2UPD 
01730          MOVE SPACES       TO  IPGPOPTI                           GAS2UPD 
01731          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS2UPD 
01732          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS2UPD 
01733          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS2UPD 
01734      IF  IPGSOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS2UPD 
01735      THEN                                                         GAS2UPD 
01736          MOVE MFRMSLTI     TO  IPGSSLTI,   ACWA-DISPLAY-LEN-7     GAS2UPD 
01737          SET  WT-01-INDEX  TO  +03                                GAS2UPD 
01738          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
01739          MOVE -1           TO  IPGSOPTL                           GAS2UPD 
01740          MOVE DFHBMABF     TO IPGSSLTA,    IPGSIDA                GAS2UPD 
01741          MOVE SPACES       TO IPGSOPTI                            GAS2UPD 
01742          MOVE DFHBMUNP     TO MFRMSLTA                            GAS2UPD 
01743          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS2UPD 
01744          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS2UPD 
01745      IF  IPGTOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS2UPD 
01746      THEN                                                         GAS2UPD 
01747          MOVE MFRMSLTI     TO  IPGTSLTI,   ACWA-DISPLAY-LEN-7     GAS2UPD 
01748          SET  WT-01-INDEX  TO  +03                                GAS2UPD 
01749          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
01750          MOVE -1           TO  IPGTOPTL                           GAS2UPD 
01751          MOVE DFHBMABF     TO IPGTSLTA,    IPGTIDA                GAS2UPD 
01752          MOVE SPACES       TO IPGTOPTI                            GAS2UPD 
01753          MOVE DFHBMUNP     TO MFRMSLTA                            GAS2UPD 
01754          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS2UPD 
01755          GO TO 2200-252-REPLACE-INT-TAB-SLOT                      GAS2UPD 
01756      ELSE                                                         GAS2UPD 
01757          GO TO 2200-253-BYPASS-INT-TAB-SLOT.                      GAS2UPD 
01758                                                                   GAS2UPD 
01759                                                                   GAS2UPD 
01760  2200-252-REPLACE-INT-TAB-SLOT.                                   GAS2UPD 
01761      SET  GAB-INT-INDEX  TO       1.                              GAS2UPD 
01762      SET  GAB-INT-INDEX  DOWN BY  1.                              GAS2UPD 
01763                                                                   GAS2UPD 
01764  2200-252-REPLACE-LOOP.                                           GAS2UPD 
01765                                                                   GAS2UPD 
01766      SET GAB-INT-INDEX  UP BY  1.                                 GAS2UPD 
01767                                                                   GAS2UPD 
01768      IF  GAB-INT-INDEX  >  5                                      GAS2UPD 
01769          MOVE WS-ABCODE-1CLX       TO  WS-ABCODE                  GAS2UPD 
01770          MOVE WS-ABCODE-1CLX-MSG   TO  WS-ABCODE-MSG              GAS2UPD 
01771          PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   GAS2UPD 
01772                                                                   GAS2UPD 
01773      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  =  GXA-PROVISION-ID GAS2UPD 
01774      THEN                                                         GAS2UPD 
01775          MOVE ACWA-DISPLAY-LEN-7  TO                              GAS2UPD 
01776                             GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX) GAS2UPD 
01777          GO TO 2200-252-REPLACE-LOOP-END                          GAS2UPD 
01778      ELSE                                                         GAS2UPD 
01779          GO TO 2200-252-REPLACE-LOOP.                             GAS2UPD 
01780                                                                   GAS2UPD 
01781  2200-252-REPLACE-LOOP-END.                                       GAS2UPD 
01782                                                                   GAS2UPD 
01783  2200-253-BYPASS-INT-TAB-SLOT.                                    GAS2UPD 
01784 *******                                                          |GAS2UPD 
01785 * STS *----------------------------------------------------------*GAS2UPD 
01786 *******                                                           GAS2UPD 
01787                                                                   GAS2UPD 
01788                                                                   GAS2UPD 
01789      PERFORM 3000-000-UPDATE-GAB-RECORD.                          GAS2UPD 
01790                                                                   GAS2UPD 
01791 *******                                                           GAS2UPD 
01792 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS2UPD 
01793 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS2UPD 
01794 *******     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.| GAS2UPD 
01795                                                                   GAS2UPD 
01796      IF  DELADDI   =  'CHG/ADD'  AND  FRMNUIDI  =  'GTM1'  AND    GAS2UPD 
01797          OENTCTRI  NOT =  '0000000' AND                           GAS2UPD 
01798         (IBGROPTI  =  'MT'  OR   IPGNOPTI  =  'MT' OR             GAS2UPD 
01799          IDGDOPTI  =  'MT'  OR   IPGPOPTI  =  'MT' OR             GAS2UPD 
01800          IPGTOPTI  =  'MT'  OR   IPGSOPTI  =  'MT')               GAS2UPD 
01801      THEN                                                         GAS2UPD 
01802          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS2UPD 
01803          MOVE 'CHG/ADD'   TO  DELADDO                             GAS2UPD 
01804          MOVE SPACES      TO  COCURANO                            GAS2UPD 
01805          MOVE DFHBMASD    TO  DLOPTLTA,   DELOPTNA                GAS2UPD 
01806          PERFORM 4100-000-DISPLAY-SKELETON                        GAS2UPD 
01807      ELSE                                                         GAS2UPD 
01808          NEXT SENTENCE.                                           GAS2UPD 
01809 *******                                                         | GAS2UPD 
01810 * STS *---------------------------------------------------------* GAS2UPD 
01811 *******                                                           GAS2UPD 
01812                                                                   GAS2UPD 
01813      IF (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND            GAS2UPD 
01814         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND            GAS2UPD 
01815         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND            GAS2UPD 
01816         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND            GAS2UPD 
01817         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND            GAS2UPD 
01818         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)                GAS2UPD 
01819         GO TO 2200-800.                                           GAS2UPD 
01820                                                                   GAS2UPD 
01821 *******                                                           GAS2UPD 
01822 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS2UPD 
01823 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS2UPD 
01824 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS2UPD 
01825 *******                                                           GAS2UPD 
01826                                                                   GAS2UPD 
01827      IF  DELADDI   =  'CHG/DEL'  AND  FRMNUIDI  =  'GTM1'  AND    GAS2UPD 
01828         (IBGROPTI  =  'MT'  OR  IPGNOPTI  =  'MT'  OR             GAS2UPD 
01829          IDGDOPTI  =  'MT'  OR  IPGPOPTI  =  'MT'  OR             GAS2UPD 
01830          IPGTOPTI  =  'MT'  OR  IPGSOPTI  =  'MT')                GAS2UPD 
01831      THEN                                                         GAS2UPD 
01832          GO TO 2200-800                                           GAS2UPD 
01833      ELSE                                                         GAS2UPD 
01834          NEXT SENTENCE.                                           GAS2UPD 
01835                                                                   GAS2UPD 
01836 *******                                                           GAS2UPD 
01837 * STS *==> DELETES DURING SINGLE TABULAR SUPPORT, THERE IS NEVER  GAS2UPD 
01838 *     *     A WORKFILE INTERNAL TABULAR TO DELETE                 GAS2UPD 
01839 *******                                                           GAS2UPD 
01840                                                                   GAS2UPD 
01841      IF  IBGROPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS2UPD 
01842          MOVE SPACE      TO  IBGROPTO                             GAS2UPD 
01843          MOVE '0000000'  TO  IBGRSLTO                             GAS2UPD 
01844          GO TO 2200-800.                                          GAS2UPD 
01845      IF  IDGDOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS2UPD 
01846          MOVE SPACE      TO  IDGDOPTO                             GAS2UPD 
01847          MOVE '0000000'  TO  IDGDSLTO                             GAS2UPD 
01848          GO TO 2200-800.                                          GAS2UPD 
01849      IF  IPGNOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS2UPD 
01850          MOVE SPACE      TO  IPGNOPTO                             GAS2UPD 
01851          MOVE '0000000'  TO  IPGNSLTO                             GAS2UPD 
01852          GO TO 2200-800.                                          GAS2UPD 
01853      IF  IPGPOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS2UPD 
01854          MOVE SPACE      TO  IPGPOPTO                             GAS2UPD 
01855          MOVE '0000000'  TO  IPGPSLTO                             GAS2UPD 
01856          GO TO 2200-800.                                          GAS2UPD 
01857      IF  IPGTOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS2UPD 
01858          MOVE SPACE      TO  IPGTOPTO                             GAS2UPD 
01859          MOVE '0000000'  TO  IPGTSLTO                             GAS2UPD 
01860          GO TO 2200-800.                                          GAS2UPD 
01861      IF  IPGSOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS2UPD 
01862          MOVE SPACE      TO  IPGSOPTO                             GAS2UPD 
01863          MOVE '0000000'  TO  IPGSSLTO                             GAS2UPD 
01864          GO TO 2200-800.                                          GAS2UPD 
01865                                                                   GAS2UPD 
01866 *******                                                          |GAS2UPD 
01867 * STS *----------------------------------------------------------*GAS2UPD 
01868 *******                                                           GAS2UPD 
01869                                                                   GAS2UPD 
01870      IF IBGROPTI  =  'D' AND  IBGRSLTI  <  '9000000'              GAS2UPD 
01871         MOVE SPACE      TO  IBGROPTO                              GAS2UPD 
01872         MOVE '0000000'  TO  IBGRSLTO                              GAS2UPD 
01873         GO TO 2200-800.                                           GAS2UPD 
01874      IF IDGDOPTI  =  'D' AND  IDGDSLTI  <  '9000000'              GAS2UPD 
01875         MOVE SPACE      TO  IDGDOPTO                              GAS2UPD 
01876         MOVE '0000000'  TO  IDGDSLTO                              GAS2UPD 
01877         GO TO 2200-800.                                           GAS2UPD 
01878      IF IPGNOPTI  =  'D' AND  IPGNSLTI  <  '9000000'              GAS2UPD 
01879         MOVE SPACE      TO  IPGNOPTO                              GAS2UPD 
01880         MOVE '0000000'  TO  IPGNSLTO                              GAS2UPD 
01881         GO TO 2200-800.                                           GAS2UPD 
01882      IF IPGPOPTI  =  'D' AND  IPGPSLTI  <  '9000000'              GAS2UPD 
01883         MOVE SPACE      TO  IPGPOPTO                              GAS2UPD 
01884         MOVE '0000000'  TO  IPGPSLTO                              GAS2UPD 
01885         GO TO 2200-800.                                           GAS2UPD 
01886      IF IPGTOPTI  =  'D' AND  IPGTSLTI  <  '9000000'              GAS2UPD 
01887         MOVE SPACE      TO  IPGTOPTO                              GAS2UPD 
01888         MOVE '0000000'  TO  IPGTSLTO                              GAS2UPD 
01889         GO TO 2200-800.                                           GAS2UPD 
01890      IF IPGSOPTI  =  'D' AND  IPGSSLTI  <  '9000000'              GAS2UPD 
01891         MOVE SPACE      TO  IPGSOPTO                              GAS2UPD 
01892         MOVE '0000000'  TO  IPGSSLTO                              GAS2UPD 
01893         GO TO 2200-800.                                           GAS2UPD 
01894                                                                   GAS2UPD 
01895      IF IBGROPTI  =  'D'                                          GAS2UPD 
01896         MOVE '0000000'  TO  IBGRSLTO                              GAS2UPD 
01897         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS2UPD 
01898      IF IDGDOPTI  =  'D'                                          GAS2UPD 
01899         MOVE '0000000'  TO  IDGDSLTO                              GAS2UPD 
01900         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS2UPD 
01901      IF IPGNOPTI  =  'D'                                          GAS2UPD 
01902         MOVE '0000000'  TO  IPGNSLTO                              GAS2UPD 
01903         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS2UPD 
01904      IF IPGPOPTI  =  'D'                                          GAS2UPD 
01905         MOVE '0000000'  TO  IPGPSLTO                              GAS2UPD 
01906         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS2UPD 
01907      IF IPGTOPTI  =  'D'                                          GAS2UPD 
01908         MOVE '0000000'  TO  IPGTSLTO                              GAS2UPD 
01909         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS2UPD 
01910      IF IPGSOPTI  =  'D'                                          GAS2UPD 
01911         MOVE '0000000'  TO  IPGSSLTO                              GAS2UPD 
01912         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS2UPD 
01913                                                                   GAS2UPD 
01914                                                                   GAS2UPD 
01915  2200-260-CHANGE-INTERNAL-TAB.                                    GAS2UPD 
01916                                                                   GAS2UPD 
01917 *    EXEC CICS GETMAIN                                            GAS2UPD 
01918 *              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             GAS2UPD 
01919 *              INITIMG(WS-HEX-00)                                 GAS2UPD 
01920 *              LENGTH(WS-COMMUNICATION-KEY-LEN)                   GAS2UPD 
01921 *              END-EXEC.                                          GAS2UPD 
01922                                                                   GAS2UPD 
01923 *    SET ACWA-COMM-KEY-PNTR  TO                                   GAS2UPD 
01924 *                      ADDRESS OF COMMUNICATION-KEY-AREA.         GAS2UPD 
01925                                                                   GAS2UPD 
01926      SET GCA-RECORD-POINTER TO                                    GAS2UPD 
01927                     ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    GAS2UPD 
01928                                                                   GAS2UPD 
01929      MOVE GCIO-WRK-PLAN-CODE         TO  GCA-PLAN-CODE.           GAS2UPD 
01930      MOVE GCIO-WRK-GROUP-NUM         TO  GCA-GROUP-NUM.           GAS2UPD 
01931      MOVE GCIO-WRK-SECTION-NUM       TO  GCA-SECTION-NUM.         GAS2UPD 
01932      MOVE GCIO-WRK-PKG-CODE          TO  GCA-PKG-CODE.            GAS2UPD 
01933      MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B.               GAS2UPD 
01934      MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL.            GAS2UPD 
01935      MOVE GCIO-WRK-FAMILY-RELATION-LVL  TO                        GAS2UPD 
01936                                          GCA-FAM-REL-LVL.         GAS2UPD 
01937      MOVE GCIO-WRK-EFFDT-CEN         TO  GCA-EFFDT-CEN.           GAS2UPD 
01938 *    IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
01939 *       MOVE BEN-PROV-ID-NO          TO  GCA-BEN-PROV-ID.         GAS2UPD 
01940      MOVE TABIDI                     TO  GCA-ALL-LEVEL-TAB-ID.    GAS2UPD 
01941      MOVE TABSLTNI                   TO  ACWA-DISPLAY-LEN-7.      GAS2UPD 
01942      MOVE ACWA-DISPLAY-LEN-7         TO  GCA-ALL-LEVEL-TAB-SLOT.  GAS2UPD 
01943      MOVE FUNCTONI          TO   GCA-ALL-LEVEL-TAB-FUNC-CODE.     GAS2UPD 
01944      MOVE GXA-PROVISION-ID           TO  GCA-INTERNAL-TAB-ID.     GAS2UPD 
01945      MOVE OENTCTRI                   TO  GCA-INTERNAL-TAB-SLOT    GAS2UPD 
01946                                          GCA-OCCURS-ENTRY-COUNTER.GAS2UPD 
01947                                                                   GAS2UPD 
01948      IF  DELADDI  =  'CHG/ADD'                                    GAS2UPD 
01949          MOVE 'A'  TO  GCA-ADD-DEL-IND                            GAS2UPD 
01950      ELSE                                                         GAS2UPD 
01951          MOVE 'D'  TO  GCA-ADD-DEL-IND.                           GAS2UPD 
01952                                                                   GAS2UPD 
01953      MOVE GXA-INCLUDE-EXCLUDE-IND  TO  GCA-I-E-INDC.              GAS2UPD 
01954      MOVE FRMNUIDI                 TO  GCA-FROM-MENU-ID.          GAS2UPD 
01955                                                                   GAS2UPD 
01956      IF IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999' OR           GAS2UPD 
01957         IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999' OR           GAS2UPD 
01958         IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999' OR           GAS2UPD 
01959         IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999' OR           GAS2UPD 
01960         IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999' OR           GAS2UPD 
01961         IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'              GAS2UPD 
01962         GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                       GAS2UPD 
01963                                                                   GAS2UPD 
01964      IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            GAS2UPD 
01965         MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                GAS2UPD 
01966         MOVE '1U'      TO  WRK2-CDE-SP                            GAS2UPD 
01967         ADD   1        TO  ACWA-CDE-1U-COUNT                      GAS2UPD 
01968      ELSE                                                         GAS2UPD 
01969         IF CDEINDO   =  ('+CDE+' OR  '+CDE-') AND                 GAS2UPD 
01970            DELADDI   =  'CHG/DEL'                                 GAS2UPD 
01971            IF INTDESKI  =  IDPRODI  AND                           GAS2UPD 
01972               WS-NEW-OCCR-ON-WF  = 'N'         THEN               GAS2UPD 
01973               MOVE '2 '   TO  WRK2-CDE-SP                         GAS2UPD 
01974               ADD   1     TO  ACWA-CDE-2B-COUNT                   GAS2UPD 
01975            ELSE                                                   GAS2UPD 
01976               MOVE '1U'   TO  WRK2-CDE-SP                         GAS2UPD 
01977               ADD   1     TO  ACWA-CDE-1U-COUNT                   GAS2UPD 
01978         ELSE                                                      GAS2UPD 
01979            IF  WS-NEW-OCCR-ON-WF = 'Y'    AND                     GAS2UPD 
01980                CDEINDO  = ('+CDE+'  OR '+CDE-')                   GAS2UPD 
01981                MOVE '1U'     TO WRK2-CDE-SP                       GAS2UPD 
01982                ADD   1       TO ACWA-CDE-1U-COUNT                 GAS2UPD 
01983            ELSE                                                   GAS2UPD 
01984                MOVE '2 '   TO  WRK2-CDE-SP                        GAS2UPD 
01985                ADD   1     TO  ACWA-CDE-2B-COUNT.                 GAS2UPD 
01986                                                                   GAS2UPD 
01987 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS2UPD 
01988      MOVE '1'          TO  GCIO2-OPER-ID-IND.                     GAS2UPD 
01989                                                                   GAS2UPD 
01990      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
01991                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS2UPD 
01992                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS2UPD 
01993                                                                   GAS2UPD 
01994      IF NOT GCIO2-GOOD-RETURN                                     GAS2UPD 
01995         MOVE WS-ABCODE-1CF9       TO  WS-ABCODE                   GAS2UPD 
01996         MOVE WS-ABCODE-1CF9-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
01997         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
01998                                                                   GAS2UPD 
01999      GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                          GAS2UPD 
02000                                                                   GAS2UPD 
02001                                                                   GAS2UPD 
02002  2200-270-DELETE-INTERNAL-TAB.                                    GAS2UPD 
02003                                                                   GAS2UPD 
02004      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS2UPD 
02005         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS2UPD 
02006           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS2UPD 
02007           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS2UPD 
02008                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS2UPD 
02009         EXEC CICS GETMAIN                                         GAS2UPD 
02010                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS2UPD 
02011                INITIMG(WS-HEX-00)                                 GAS2UPD 
02012                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS2UPD 
02013                END-EXEC                                           GAS2UPD 
02014         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS2UPD 
02015                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS2UPD 
02016                                                                   GAS2UPD 
02017      IF FRMNUIDI  =  'GS3A'                                       GAS2UPD 
02018         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS2UPD 
02019      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS2UPD 
02020         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS2UPD 
02021      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
02022         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS2UPD 
02023         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS2UPD 
02024         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS2UPD 
02025                                                                   GAS2UPD 
02026      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS2UPD 
02027      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS2UPD 
02028      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS2UPD 
02029      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS2UPD 
02030      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS2UPD 
02031           TO  GXA-ENTRY-COUNT.                                    GAS2UPD 
02032                                                                   GAS2UPD 
02033      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
02034                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS2UPD 
02035                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS2UPD 
02036                                                                   GAS2UPD 
02037      IF NOT GCIO2-GOOD-RETURN                                     GAS2UPD 
02038         MOVE WS-ABCODE-1CF5       TO  WS-ABCODE                   GAS2UPD 
02039         MOVE WS-ABCODE-1CF5-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
02040         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02041                                                                   GAS2UPD 
02042      MOVE GC-GCIO-ACCESS-CODE-DL TO  GCIO2-FILE-ACCESS-CODE.      GAS2UPD 
02043      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
02044                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS2UPD 
02045                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS2UPD 
02046                                                                   GAS2UPD 
02047      IF NOT GCIO2-GOOD-RETURN                                     GAS2UPD 
02048         MOVE WS-ABCODE-1CFC       TO  WS-ABCODE                   GAS2UPD 
02049         MOVE WS-ABCODE-1CFC-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
02050         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02051                                                                   GAS2UPD 
02052      IF WRK2-CDE-SP =  '1U'                                       GAS2UPD 
02053         SUBTRACT  1  FROM  ACWA-CDE-1U-COUNT.                     GAS2UPD 
02054                                                                   GAS2UPD 
02055      IF WRK2-CDE-SP  =  '2 '                                      GAS2UPD 
02056         SUBTRACT  1  FROM  ACWA-CDE-2B-COUNT.                     GAS2UPD 
02057                                                                   GAS2UPD 
02058      MOVE SPACES  TO  IBGROPTO,  IPGNOPTO,  IPGTOPTO              GAS2UPD 
02059                       IDGDOPTO,  IPGPOPTO   IPGSOPTO.             GAS2UPD 
02060      GO TO 2200-800.                                              GAS2UPD 
02061                                                                   GAS2UPD 
02062                                                                   GAS2UPD 
02063  2200-280-XCTL-TO-INT-TAB-PGM.                                    GAS2UPD 
02064                                                                   GAS2UPD 
02065      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS2UPD 
02066         DELADDI  =  'CHG/DEL' AND                                 GAS2UPD 
02067         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS2UPD 
02068         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS2UPD 
02069         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS2UPD 
02070                                                                   GAS2UPD 
02071      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS2UPD 
02072         ACWA-CDE-2B-COUNT  =  ZERO                                GAS2UPD 
02073         NEXT SENTENCE                                             GAS2UPD 
02074      ELSE                                                         GAS2UPD 
02075         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS2UPD 
02076                                                                   GAS2UPD 
02077      MOVE INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.       GAS2UPD 
02078      MOVE 'GAS2UPD' TO DELADD-OPTION.                             GAS2UPD 
02079                                                                   GAS2UPD 
02080      EXEC CICS  XCTL  PROGRAM (WS-INTERNAL-TABULAR-PGM-ID)        GAS2UPD 
02081                 COMMAREA(DFHCOMMAREA)                             GAS2UPD 
02082                 LENGTH(LENGTH OF DFHCOMMAREA)                     GAS2UPD 
02083                 END-EXEC.                                         GAS2UPD 
02084                                                                   GAS2UPD 
02085  2200-800.                                                        GAS2UPD 
02086                                                                   GAS2UPD 
02087      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS2UPD 
02088         DELADDI  =  'CHG/DEL' AND                                 GAS2UPD 
02089         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS2UPD 
02090         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS2UPD 
02091         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS2UPD 
02092                                                                   GAS2UPD 
02093      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS2UPD 
02094         ACWA-CDE-2B-COUNT  =  ZERO                                GAS2UPD 
02095         NEXT SENTENCE                                             GAS2UPD 
02096      ELSE                                                         GAS2UPD 
02097         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS2UPD 
02098                                                                   GAS2UPD 
02099      MOVE -1  TO  PERIODL.                                        GAS2UPD 
02100      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS2UPD 
02101                                                                   GAS2UPD 
02102  2200-900-EXIT. EXIT.                                             GAS2UPD 
02103                                                                   GAS2UPD 
02104 /*****************************************************************GAS2UPD 
02105 *           D E L E T E   T H I S   O C C U R A N C E            *GAS2UPD 
02106 *    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *GAS2UPD 
02107 *  SCREEN'S DISPLAY AND REMOVE THAT ENTRY FROM THE ALL LEVEL     *GAS2UPD 
02108 *  TABULAR RECORD, INCLUDED WITH THAT IS CODE TO DELETE ANY      *GAS2UPD 
02109 *  INTERNAL TABULAR ENTRIES THAT MIGHT BE SPECIFIED BY THAT ENTRY*GAS2UPD 
02110 ******************************************************************GAS2UPD 
02111  2400-000-DELETE-THIS-OCCURANCE SECTION.                          GAS2UPD 
02112  2400-010.                                                        GAS2UPD 
02113                                                                   GAS2UPD 
02114      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS2UPD 
02115                                                                   GAS2UPD 
02116      IF NOT GCIO-GOOD-RETURN                                      GAS2UPD 
02117         MOVE WS-ABCODE-1CFB       TO  WS-ABCODE                   GAS2UPD 
02118         MOVE WS-ABCODE-1CFB-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
02119         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02120                                                                   GAS2UPD 
02121      IF FRMNUIDI  =  'GS3A'                                       GAS2UPD 
02122         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS2UPD 
02123      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS2UPD 
02124         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS2UPD 
02125      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
02126         MOVE 'C6'  TO  GCIO-WRK-RECORD-TYPE                       GAS2UPD 
02127         MOVE GCIO-WRK-TABULAR-PROVISION                           GAS2UPD 
02128                    TO  GCIO-WRK-BENEFIT-PROVISION.                GAS2UPD 
02129                                                                   GAS2UPD 
02130      MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   GAS2UPD 
02131      MOVE OENTCTRI         TO  ACWA-DISPLAY-LEN-7.                GAS2UPD 
02132                                                                   GAS2UPD 
02133         EXEC CICS GETMAIN                                         GAS2UPD 
02134                SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            GAS2UPD 
02135                INITIMG(WS-HEX-00)                                 GAS2UPD 
02136                LENGTH(WS-COPY-TABLE-LEN)                          GAS2UPD 
02137                END-EXEC.                                          GAS2UPD 
02138                                                                   GAS2UPD 
02139         SET ACWA-COPY-TAB-PNTR        TO                          GAS2UPD 
02140                  ADDRESS OF COPY-TABULAR-TABLE-AREA.              GAS2UPD 
02141                                                                   GAS2UPD 
02142      SET GAB-INDEX,  COPY-IDX  TO  1.                             GAS2UPD 
02143                                                                   GAS2UPD 
02144                                                                   GAS2UPD 
02145  2400-100-COPY-SAVED-AND-DELETE.                                  GAS2UPD 
02146                                                                   GAS2UPD 
02147      IF  GAB-INDEX  <  GAB-ENTRY-COUNT                            GAS2UPD 
02148      THEN                                                         GAS2UPD 
02149          IF  GAB-OCCURS-ENTRY-COUNTER (GAB-INDEX)  NOT =          GAS2UPD 
02150              ACWA-DISPLAY-LEN-7                                   GAS2UPD 
02151          THEN                                                     GAS2UPD 
02152              MOVE GAB-ENTRY          (GAB-INDEX)                  GAS2UPD 
02153                TO COPY-TABULAR-TABLE (COPY-IDX)                   GAS2UPD 
02154              SET  GAB-INDEX,  COPY-IDX  UP BY  1                  GAS2UPD 
02155              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS2UPD 
02156          ELSE                                                     GAS2UPD 
02157 *---- WE ARE NOT GOING TO COPY THE ENTRY THAT IS BEING DELETED.   GAS2UPD 
02158 *---- BUT WE SAVE IT SINCE WE HAVE TO DELETE ANY INTERNAL TABULARSGAS2UPD 
02159              MOVE GAB-ENTRY (GAB-INDEX)  TO  WS-ENTRY             GAS2UPD 
02160              SET  COPY-IDX3  TO  GAB-INDEX                        GAS2UPD 
02161              SET  GAB-INDEX  UP BY  1                             GAS2UPD 
02162              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS2UPD 
02163      ELSE                                                         GAS2UPD 
02164          MOVE GAB-ENTRY          (GAB-INDEX)                      GAS2UPD 
02165            TO COPY-TABULAR-TABLE (COPY-IDX).                      GAS2UPD 
02166                                                                   GAS2UPD 
02167      SET  GAB-ENTRY-COUNT  TO  COPY-IDX.                          GAS2UPD 
02168      MOVE GAB-ENTRY-COUNT  TO  GAB-ENTRY-COUNT.                   GAS2UPD 
02169      SET COPY-IDX,  GAB-INDEX  TO  1.                             GAS2UPD 
02170                                                                   GAS2UPD 
02171                                                                   GAS2UPD 
02172  2400-200-MOVE-UPDATED-TABLE.                                     GAS2UPD 
02173                                                                   GAS2UPD 
02174      IF GAB-INDEX  NOT >  GAB-ENTRY-COUNT                         GAS2UPD 
02175         MOVE COPY-TABULAR-TABLE (COPY-IDX)                        GAS2UPD 
02176           TO GAB-ENTRY          (GAB-INDEX)                       GAS2UPD 
02177         SET GAB-INDEX,  COPY-IDX  UP BY  1                        GAS2UPD 
02178         GO TO 2400-200-MOVE-UPDATED-TABLE.                        GAS2UPD 
02179 *====== D1618 06/03/88  NE ================================       GAS2UPD 
02180                                                                   GAS2UPD 
02181       IF CDEINDO  =  '+CDE+'  OR '+CDE-'                          GAS2UPD 
02182          PERFORM 4600-000-UPDATE-CDE-STATUS.                      GAS2UPD 
02183                                                                   GAS2UPD 
02184 *==========================================================       GAS2UPD 
02185      PERFORM 3000-000-UPDATE-GAB-RECORD.                          GAS2UPD 
02186                                                                   GAS2UPD 
02187      SET GAB-INDEX  TO  GAB-ENTRY-COUNT.                          GAS2UPD 
02188      MOVE WS-ENTRY  TO  GAB-ENTRY (GAB-INDEX).                    GAS2UPD 
02189                                                                   GAS2UPD 
02190      IF GAB-INTERNAL-TABULAR-COUNT (GAB-INDEX)  =  1              GAS2UPD 
02191         GO TO 2400-340-DELETE-LOOP-END.                           GAS2UPD 
02192                                                                   GAS2UPD 
02193                                                                   GAS2UPD 
02194 *---- DELETE INTERNAL TABULARS FROM W/F THAT ARE ATTACHED TO      GAS2UPD 
02195 *       ALL LVL TAB OCCURANCE BEING DELETED                       GAS2UPD 
02196  2400-300-DELETE-INTERNAL-TABS.                                   GAS2UPD 
02197      SET GAB-INT-INDEX  TO       1.                               GAS2UPD 
02198      SET GAB-INT-INDEX  DOWN BY  1.                               GAS2UPD 
02199                                                                   GAS2UPD 
02200  2400-320-DELETE-LOOP.                                            GAS2UPD 
02201                                                                   GAS2UPD 
02202      SET GAB-INT-INDEX UP BY 1.                                   GAS2UPD 
02203      IF  GAB-INT-INDEX >  5                                       GAS2UPD 
02204          GO TO 2400-340-DELETE-LOOP-END.                          GAS2UPD 
02205                                                                   GAS2UPD 
02206      IF GAB-INT-ID (GAB-INDEX GAB-INT-INDEX)  =  HIGH-VALUES      GAS2UPD 
02207         GO TO 2400-340-DELETE-LOOP-END.                           GAS2UPD 
02208                                                                   GAS2UPD 
02209 ************   09/14/88  NE                                       GAS2UPD 
02210      IF  GAB-INT-SLOT (GAB-INDEX GAB-INT-INDEX)  >  8999999       GAS2UPD 
02211          NEXT SENTENCE                                            GAS2UPD 
02212      ELSE                                                         GAS2UPD 
02213          GO TO  2400-320-DELETE-LOOP.                             GAS2UPD 
02214                                                                   GAS2UPD 
02215      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS2UPD 
02216         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS2UPD 
02217           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS2UPD 
02218           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS2UPD 
02219                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS2UPD 
02220         EXEC CICS GETMAIN                                         GAS2UPD 
02221                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS2UPD 
02222                INITIMG(WS-HEX-00)                                 GAS2UPD 
02223                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS2UPD 
02224                END-EXEC                                           GAS2UPD 
02225         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS2UPD 
02226                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS2UPD 
02227                                                                   GAS2UPD 
02228      MOVE GAB-INT-TS (GAB-INDEX GAB-INT-INDEX)                    GAS2UPD 
02229                                  TO GCIO-WRK-TABULAR-PROVISION.   GAS2UPD 
02230      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS2UPD 
02231      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS2UPD 
02232      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS2UPD 
02233      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS2UPD 
02234                                                                   GAS2UPD 
02235      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS2UPD 
02236           TO  GXA-ENTRY-COUNT.                                    GAS2UPD 
02237                                                                   GAS2UPD 
02238      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
02239                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS2UPD 
02240                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS2UPD 
02241      IF NOT GCIO2-GOOD-RETURN                                     GAS2UPD 
02242         MOVE WS-ABCODE-1CF5       TO  WS-ABCODE                   GAS2UPD 
02243         MOVE WS-ABCODE-1CF5-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
02244         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02245                                                                   GAS2UPD 
02246                                                                   GAS2UPD 
02247      MOVE GC-GCIO-ACCESS-CODE-DL TO GCIO2-FILE-ACCESS-CODE.       GAS2UPD 
02248      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
02249                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS2UPD 
02250                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS2UPD 
02251                                                                   GAS2UPD 
02252      IF NOT GCIO2-GOOD-RETURN                                     GAS2UPD 
02253         MOVE WS-ABCODE-1CFC       TO  WS-ABCODE                   GAS2UPD 
02254         MOVE WS-ABCODE-1CFC-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
02255         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02256                                                                   GAS2UPD 
02257      IF WRK2-CDE-SP  =  '1U'                                      GAS2UPD 
02258         SUBTRACT 1  FROM  ACWA-CDE-1U-COUNT.                      GAS2UPD 
02259      IF WRK2-CDE-SP  =  '2 '                                      GAS2UPD 
02260         SUBTRACT 1  FROM  ACWA-CDE-2B-COUNT.                      GAS2UPD 
02261                                                                   GAS2UPD 
02262      GO TO 2400-320-DELETE-LOOP.                                  GAS2UPD 
02263                                                                   GAS2UPD 
02264                                                                   GAS2UPD 
02265  2400-340-DELETE-LOOP-END.                                        GAS2UPD 
02266      PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      GAS2UPD 
02267                                                                   GAS2UPD 
02268  2400-400-DISPLAY-SCREEN.                                         GAS2UPD 
02269                                                                   GAS2UPD 
02270      IF COPY-IDX3  =  GAB-ENTRY-COUNT AND  =  1                   GAS2UPD 
02271         MOVE 'CHG/ADD'     TO  DELADDO                            GAS2UPD 
02272         MOVE SPACES        TO  DELOPTNO,   DELOLITO               GAS2UPD 
02273         MOVE DFHBMASD      TO  DLOPTLTA,   DELOPTNA               GAS2UPD 
02274         SET  WT-01-INDEX   TO  +19                                GAS2UPD 
02275         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO         GAS2UPD 
02276         PERFORM 4100-000-DISPLAY-SKELETON.                        GAS2UPD 
02277                                                                   GAS2UPD 
02278      IF  COPY-IDX3  <  GAB-ENTRY-COUNT                            GAS2UPD 
02279      THEN                                                         GAS2UPD 
02280          SET GAB-INDEX  TO  COPY-IDX3                             GAS2UPD 
02281          MOVE SPACES    TO  ERRMSGO                               GAS2UPD 
02282          PERFORM 4400-000-BUILD-DISPLAY                           GAS2UPD 
02283      ELSE                                                         GAS2UPD 
02284          SET  GAB-INDEX     TO  1                                 GAS2UPD 
02285          SET  WT-01-INDEX   TO  +14                               GAS2UPD 
02286          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS2UPD 
02287          PERFORM 4400-000-BUILD-DISPLAY.                          GAS2UPD 
02288                                                                   GAS2UPD 
02289 ***************************************************************** GAS2UPD 
02290 * CCSP  NDF - FLAGSHIP RENOVATION - OCT 2004                      GAS2UPD 
02291 * - REMOVED DEAD CODE LINES BELOW -                               GAS2UPD 
02292 *    MOVE SPACES  TO  DELOPTNO.                                   GAS2UPD 
02293 *    PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS2UPD 
02294 ***************************************************************** GAS2UPD 
02295                                                                   GAS2UPD 
02296  2400-900-EXIT. EXIT.                                             GAS2UPD 
02297                                                                   GAS2UPD 
02298 /*****************************************************************GAS2UPD 
02299 *     P R O C E S S   V A L   L I M I T                           GAS2UPD 
02300 ******************************************************************GAS2UPD 
02301  2600-000-PROCESS-VAL-LIMIT     SECTION.                          GAS2UPD 
02302  2600-010.                                                        GAS2UPD 
02303                                                                   GAS2UPD 
02304      IF (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'NEG' OR                   GAS2UPD 
02305          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 GAS2UPD 
02304         (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'UNL' OR                   GAS2UPD 
02305          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    GAS2UPD 
02306          GO TO 2600-900-EXIT.                                     GAS2UPD 
02307                                                                   GAS2UPD 
02308      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS2UPD 
02309      THEN                                                         GAS2UPD 
02310          IF  BENVLQLI  =  '5'                                     GAS2UPD 
02311          THEN                                                     GAS2UPD 
02312              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         GAS2UPD 
02313              MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         GAS2UPD 
02314              GO TO 2600-900-EXIT                                  GAS2UPD 
02315          ELSE                                                     GAS2UPD 
02316              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       GAS2UPD 
02317              GO TO 2600-900-EXIT                                  GAS2UPD 
02318      ELSE                                                         GAS2UPD 
02319          NEXT SENTENCE.                                           GAS2UPD 
02320                                                                   GAS2UPD 
02321      IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            GAS2UPD 
02322          MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       GAS2UPD 
02323          MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       GAS2UPD 
02324          GO TO 2600-900-EXIT.                                     GAS2UPD 
02325                                                                   GAS2UPD 
02326  2600-900-EXIT. EXIT.                                             GAS2UPD 
02327                                                                   GAS2UPD 
02328 /*****************************************************************GAS2UPD 
02329 * 3000 UPDATE GAB RECORD                                         *GAS2UPD 
02330 *                                                                *GAS2UPD 
02331 *    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *GAS2UPD 
02332 ******************************************************************GAS2UPD 
02333  3000-000-UPDATE-GAB-RECORD     SECTION.                          GAS2UPD 
02334  3000-010.                                                        GAS2UPD 
02335                                                                   GAS2UPD 
02336      COMPUTE  GCIO-RECORD-LENGTH   =                              GAS2UPD 
02337          GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACL-FIXED-LEN  +     GAS2UPD 
02338          (GC-GCTABULR-ACL-VARY-LEN * GAB-ENTRY-COUNT).            GAS2UPD 
02339                                                                   GAS2UPD 
02340 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS2UPD 
02341      MOVE '1'                      TO  GCIO-OPER-ID-IND.          GAS2UPD 
02342      MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     GAS2UPD 
02343                                                                   GAS2UPD 
02344      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
02345                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS2UPD 
02346                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS2UPD 
02347                                                                   GAS2UPD 
02348      IF NOT GCIO-GOOD-RETURN                                      GAS2UPD 
02349         MOVE WS-ABCODE-1CF4       TO  WS-ABCODE                   GAS2UPD 
02350         MOVE WS-ABCODE-1CF4-MSG   TO  WS-ABCODE-MSG               GAS2UPD 
02351         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02352                                                                   GAS2UPD 
02353  3000-900-EXIT. EXIT.                                             GAS2UPD 
02354                                                                   GAS2UPD 
02355 ******************************************************************GAS2UPD 
02356 * 3100  UNLOCK THE GAB ACCUM TAB RECORD READ EARLIER FOR UPDATE   GAS2UPD 
02357 ******************************************************************GAS2UPD 
02358  3100-RLSE-RU-GAB-REC SECTION.                                    GAS2UPD 
02359                                                                   GAS2UPD 
02360      MOVE GC-GCIO-ACCESS-CODE-UNL  TO  GCIO-FILE-ACCESS-CODE.     GAS2UPD 
02361                                                                   GAS2UPD 
02362      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GAS2UPD 
02363                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS2UPD 
02364                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS2UPD 
02365                                                                   GAS2UPD 
02366      IF NOT GCIO-GOOD-RETURN                                      GAS2UPD 
02367         MOVE WS-ABCODE-1CF4        TO  WS-ABCODE                  GAS2UPD 
02368         MOVE WS-ABCODE-1CF4-MSG    TO  WS-ABCODE-MSG              GAS2UPD 
02369         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02370  3199-EXIT.     EXIT.                                             GAS2UPD 
02371                                                                   GAS2UPD 
02372 /*****************************************************************GAS2UPD 
02373 * 3200  READ REC FOR UPDATE                                      *GAS2UPD 
02374 *                                                                *GAS2UPD 
02375 *    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *GAS2UPD 
02376 *  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *GAS2UPD 
02377 ******************************************************************GAS2UPD 
02378  3200-000-READ-REC-FOR-UPDATE   SECTION.                          GAS2UPD 
02379  3300-010.                                                        GAS2UPD 
02380                                                                   GAS2UPD 
02381      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN   +  GAS2UPD 
02382            GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACL-FIXED-LEN   +  GAS2UPD 
02383            (GC-GCTABULR-ACL-VARY-LEN   *                          GAS2UPD 
02384                                  GC-GCTABULR-ACL-VARY-MAX-OCUR).  GAS2UPD 
02385                                                                   GAS2UPD 
02386      IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS2UPD 
02387         NEXT SENTENCE                                             GAS2UPD 
02388      ELSE                                                         GAS2UPD 
02389         EXEC CICS GETMAIN                                         GAS2UPD 
02390                SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      GAS2UPD 
02391                INITIMG(WS-HEX-00)                                 GAS2UPD 
02392                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS2UPD 
02393                END-EXEC                                           GAS2UPD 
02394         SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     GAS2UPD 
02395                  ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        GAS2UPD 
02396                                                                   GAS2UPD 
02397                                                                   GAS2UPD 
02398      IF FRMNUIDI  =  'GS3A'                                       GAS2UPD 
02399         PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    GAS2UPD 
02400      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS2UPD 
02401         PERFORM 6100-000-BUILD-CONTRACT-KEY.                      GAS2UPD 
02402      IF FRMNUIDI  =  'GC8A'                                       GAS2UPD 
02403         PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      GAS2UPD 
02404                                                                   GAS2UPD 
02405      MOVE GC-GCPSWORK-DDNAME      TO  GCIO-FILE-DDNAME.           GAS2UPD 
02406      MOVE GC-GCIO-AREA-1          TO  GCIO-IO-AREA-TO-USE.        GAS2UPD 
02407      MOVE GCIO-WORKFILE-KEY       TO  GCIO-FILE-KEY.              GAS2UPD 
02408      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO-FILE-ACCESS-CODE.      GAS2UPD 
02409      MOVE GC-GCTABULR-ACL-VARY-MAX-OCUR  TO  GAB-ENTRY-COUNT.     GAS2UPD 
02410                                                                   GAS2UPD 
02411      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS2UPD 
02412                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS2UPD 
02413                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS2UPD 
02414                                                                   GAS2UPD 
02415  3200-900-EXIT. EXIT.                                             GAS2UPD 
02416                                                                   GAS2UPD 
02417 /*****************************************************************GAS2UPD 
02418 * 4100  DISPLAY SKELETON                                         *GAS2UPD 
02419 *                                                                *GAS2UPD 
02420 *    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *GAS2UPD 
02421 *  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *GAS2UPD 
02422 *  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *GAS2UPD 
02423 ******************************************************************GAS2UPD 
02424  4100-000-DISPLAY-SKELETON      SECTION.                          GAS2UPD 
02425  4100-010.                                                        GAS2UPD 
02426                                                                   GAS2UPD 
02427      MOVE SPACES    TO  ERRMSGO.                                  GAS2UPD 
02428                                                                   GAS2UPD 
02429      MOVE DFHBMFSE  TO  PERIODA.                                  GAS2UPD 
02430                                                                   GAS2UPD 
02431      MOVE DFHBMUNP  TO  BENVLQLA FAMINDIA  INTDESKA  LOBA         GAS2UPD 
02432                         IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA     GAS2UPD 
02433                         IDGDOPTA IPGPOPTA  IPGSOPTA.              GAS2UPD 
02434                                                                   GAS2UPD 
02435      MOVE ALL '_'  TO  PERIODO   BENVLQLO  LOBO                   GAS2UPD 
02436                        FAMINDIO  PLCTRMTO.                        GAS2UPD 
02437                                                                   GAS2UPD 
02438      MOVE LOW-VALUES  TO  INTDESKO  IBGROPTO  IPGNOPTO IDGDOPTO   GAS2UPD 
02439                           IPGTOPTO  MFRMSLTO IPGPOPTO IPGSOPTO.   GAS2UPD 
02440                                                                   GAS2UPD 
02441      MOVE ZEROS  TO  COPAYINO  CSTCONTO  PERTQALO  BISNDINO       GAS2UPD 
02442            DAYFACIO  SRVGRUPO  PRTIMEFO  MAXOVRDO                 GAS2UPD 
02443            REININDO  CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO       GAS2UPD 
02444            FYIVALO   OVRDINDO  NEWVALUO  DEFINTNO                 GAS2UPD 
02445            CONDALLO  CONDEXCO  CONDICDO  CONDTABO  CONDMENO       GAS2UPD 
02446         CONDDRGO  CONDALCO  CONDOBNO  CONDOBCO  CONDMALO CONDTMJO GAS2UPD 
02447         CONDCARO  CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO CONDINFO GAS2UPD 
02448         CONDEMCO  CONDEACO  CONDSMIO CONDNSMO                     GAS2UPD 
02449         PRTIMEFO  INTRVALO  CONDPECO  CONDNEMO  NEWVALUO          GAS2UPD 
02450         OENTCTRO  IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO          GAS2UPD 
02451         AGEQLLO   AGEQLHO   CONDLIFO  IPGSSLTO                    GAS2UPD 
02452            RELPINDO  AGELIMLO  AGELIMHO IDGDSLTO  IPGPSLTO        GAS2UPD 
02453            FEAKINDO  ACCUMIDO  CAPINDO  SABDINDO                  GAS2UPD 
                 BENTYPO   TIERCDO   TIERLVO.                                   
02454                                                                   GAS2UPD 
02455      MOVE '01'    TO  COCURANO.                                   GAS2UPD 
02456      MOVE -1      TO  PERIODL.                                    GAS2UPD 
02457                                                                   GAS2UPD 
02458      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS2UPD 
02459                                                                   GAS2UPD 
02460  4100-900-EXIT. EXIT.                                             GAS2UPD 
02461                                                                   GAS2UPD 
02462 /*****************************************************************GAS2UPD 
02463 * 4400 BUILD DISPLAY                                             *GAS2UPD 
02464 *                                                                *GAS2UPD 
02465 *    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *GAS2UPD 
02466 *  SPECIFIED BY INDEX GAB-INDEX TO THE SCREEN.                   *GAS2UPD 
02467 ******************************************************************GAS2UPD 
02468  4400-000-BUILD-DISPLAY         SECTION.                          GAS2UPD 
02469  4400-010.                                                        GAS2UPD 
02470                                                                   GAS2UPD 
02471      IF  DELADDI  =  'CHG/DEL'                                    GAS2UPD 
02472      THEN                                                         GAS2UPD 
02473          MOVE 'D'  TO  DELOLITO                                   GAS2UPD 
02474      ELSE                                                         GAS2UPD 
02475          MOVE SPACE  TO  DELOLITO.                                GAS2UPD 
02476                                                                   GAS2UPD 
02477      MOVE SPACE                                       TO DELOPTNO.GAS2UPD 
02478      MOVE GAB-OCCURS-ENTRY-COUNTER     (GAB-INDEX)  TO            GAS2UPD 
02479                                                ACWA-DISPLAY-LEN-7.GAS2UPD 
02480      MOVE ACWA-DISPLAY-LEN-7                        TO  OENTCTRO. GAS2UPD 
02481      MOVE GAB-COINS-DAY-FACTOR-IND     (GAB-INDEX)  TO  DAYFACIO. GAS2UPD 
02482      MOVE GAB-COINS-CO-PAY-IND         (GAB-INDEX)  TO  COPAYINO. GAS2UPD 
02483      MOVE GAB-COINS-BISCENDING-IND     (GAB-INDEX)  TO  BISNDINO. GAS2UPD 
02484      MOVE GAB-COINS-ASCEND-DESCEND-IND (GAB-INDEX)  TO  ASCDSCDO. GAS2UPD 
      **P21595 CHANGES STARTS                                                   
02484      MOVE GAB-COINS-BEN-TYPE           (GAB-INDEX)  TO  BENTYPO.  GAS2UPD 
02484      MOVE GAB-COINS-TIER-CODE          (GAB-INDEX)  TO  TIERCDO.  GAS2UPD 
02484      MOVE GAB-COINS-TIER-LVL           (GAB-INDEX)  TO  TIERLVO.  GAS2UPD 
      **P21595 CHANGES ENDS                                                     
02485      MOVE GAB-COINS-DEFINITION         (GAB-INDEX)  TO  DEFINTNO. GAS2UPD 
02486      MOVE GAB-COINS-COST-CONTAIN-IND   (GAB-INDEX)  TO  CSTCONTO. GAS2UPD 
02487      MOVE GAB-COINS-BENEFIT-PERIOD     (GAB-INDEX)  TO  PERIODO.  GAS2UPD 
02488      MOVE GAB-COINS-BEN-PER-TIME-QUAL  (GAB-INDEX)  TO  PERTQALO. GAS2UPD 
02489      MOVE GAB-COINS-FAM-OR-INDIV       (GAB-INDEX)  TO  FAMINDIO. GAS2UPD 
02490      MOVE GAB-COINS-PLACE-OF-TREATMENT (GAB-INDEX)  TO  PLCTRMTO. GAS2UPD 
02491      MOVE GAB-COINS-SERVICE-GROUP      (GAB-INDEX)  TO  SRVGRUPO. GAS2UPD 
02492      MOVE GAB-COINS-RELATIONSHIP-IND   (GAB-INDEX)  TO  RELPINDO. GAS2UPD 
02493      MOVE GAB-CARRY-OVER-CREDIT-IND    (GAB-INDEX)  TO  CARYOVRO. GAS2UPD 
02494      MOVE GAB-COINS-AGE-QUAL-IND-FROM  (GAB-INDEX)  TO  AGEQLLO.  GAS2UPD 
02495      MOVE GAB-COINS-AGE-QUAL-IND-TO    (GAB-INDEX)  TO  AGEQLHO.  GAS2UPD 
02496      MOVE GAB-COINS-AGE-LIMIT-FROM     (GAB-INDEX)  TO            GAS2UPD 
02497                                               ACWA-DISPLAY-LEN-3. GAS2UPD 
02498      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMLO. GAS2UPD 
02499      MOVE GAB-COINS-AGE-LIMIT-TO       (GAB-INDEX)  TO            GAS2UPD 
02500                                               ACWA-DISPLAY-LEN-3. GAS2UPD 
02501      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMHO. GAS2UPD 
02502      MOVE GAB-COINS-FEAK-IND           (GAB-INDEX)  TO  FEAKINDO. GAS2UPD 
02503      MOVE GAB-COINS-ACCUMID            (GAB-INDEX)  TO  ACCUMIDO. GAS2UPD 
02504      MOVE GAB-COINS-COMB-APPLIED-IND   (GAB-INDEX)  TO  CAPINDO.  GAS2UPD 
02505      MOVE GAB-COINS-SEL-ADDL-BEN-DET   (GAB-INDEX)  TO  SABDINDO. GAS2UPD 
02506      MOVE GAB-COINS-BEN-PER-TIME-FCTR  (GAB-INDEX)  TO            GAS2UPD 
02507                                               ACWA-DISPLAY-LEN-3. GAS2UPD 
02508      MOVE ACWA-DISPLAY-LEN-3                        TO  PRTIMEFO. GAS2UPD 
02509      MOVE GAB-COINS-REINSTATEMENT-IND  (GAB-INDEX)  TO  REININDO. GAS2UPD 
02510      MOVE GAB-COINS-LMT-MANDATORY-IND  (GAB-INDEX)  TO  MANAPLIO. GAS2UPD 
02511      MOVE GAB-COINS-1ST-DOLR-COVRGE-LMT(GAB-INDEX)  TO  FDLRCLIO. GAS2UPD 
02512      MOVE GAB-COINS-CLAIM-LVL-ACCUM-IND (GAB-INDEX) TO  CLMLVLIO. GAS2UPD 
02513      MOVE GAB-COINS-INTERVAL-TIME-FCTR (GAB-INDEX)  TO            GAS2UPD 
02514                                                ACWA-DISPLAY-LEN-3.GAS2UPD 
02515      MOVE ACWA-DISPLAY-LEN-3                        TO  INTRVALO. GAS2UPD 
02516      MOVE GAB-COINS-INTERVAL-TYPE      (GAB-INDEX)  TO  INTTYPEO. GAS2UPD 
02517      MOVE GAB-COINS-L-O-B              (GAB-INDEX)  TO  LOBO.     GAS2UPD 
02518      MOVE GAB-COINS-VALUE-LIMIT        (GAB-INDEX)  TO            GAS2UPD 
02519                                                ACWA-VALUE-LIMIT-9.GAS2UPD 
02520      IF  ACWA-VALUE-LIMIT-9-9 = -1                                GAS2UPD 
02521      THEN                                                         GAS2UPD 
02522          MOVE 'NEG' TO BNMXVALO                                   GAS2UPD 
02523      ELSE                                                         GAS2UPD 
02520      IF  ACWA-VALUE-LIMIT-9-9 = -2                                GAS2UPD 
02521      THEN                                                         GAS2UPD 
02522          MOVE 'UNL' TO BNMXVALO                                   GAS2UPD 
02523      ELSE                                                         GAS2UPD 
02524          IF  GAB-COINS-VALUE-QUALIFIER (GAB-INDEX) = '5'          GAS2UPD 
02525          THEN                                                     GAS2UPD 
02526              MOVE ACWA-VALUE-LIMIT-9     TO  ACWA-EDIT-VALUE-LIMITGAS2UPD 
02527              MOVE ACWA-EDIT-VALUE-LIMIT  TO  BNMXVALO             GAS2UPD 
02528          ELSE                                                     GAS2UPD 
02529              MOVE GAB-COINS-VALUE-LIMIT(GAB-INDEX) TO             GAS2UPD 
02530                                               ACWA-DISPLAY-LEN-9-2GAS2UPD 
02531              MOVE ACWA-DISPLAY-LEN-9-X   TO  ACWA-DISPLAY-9       GAS2UPD 
02532              MOVE SPACES                 TO  ACWA-DISPLAY-1       GAS2UPD 
02533              MOVE ACWA-DISPLAY-VALUE-LIMIT  TO  BNMXVALO.         GAS2UPD 
02534                                                                   GAS2UPD 
02535      MOVE GAB-COINS-PERCENT-LEVEL     (GAB-INDEX)  TO             GAS2UPD 
02536                                                ACWA-DISPLAY-LEN-3.GAS2UPD 
02537      MOVE ACWA-DISPLAY-LEN-3                       TO  PERLIMTO.  GAS2UPD 
02538      MOVE GAB-COINS-VALUE-QUALIFIER   (GAB-INDEX)  TO  BENVLQLO.  GAS2UPD 
02539      MOVE GAB-COINS-INTERVAL-OVRD-VALUE(GAB-INDEX) TO             GAS2UPD 
02540                                                ACWA-DISPLAY-LEN-5.GAS2UPD 
02541      MOVE ACWA-DISPLAY-LEN-5                       TO  NEWVALUO.  GAS2UPD 
02542      MOVE GAB-COINS-INTERVAL-OVRD-IND (GAB-INDEX)  TO  OVRDINDO.  GAS2UPD 
02543      MOVE GAB-COINS-INTERNAL-DESCRIPTOR(GAB-INDEX) TO  INTDESKO.  GAS2UPD 
02544      MOVE GAB-COND-ALL-BIT            (GAB-INDEX)  TO  CONDALLO.  GAS2UPD 
02545      MOVE GAB-COND-EXCLUSION-BIT      (GAB-INDEX)  TO  CONDEXCO.  GAS2UPD 
02546      MOVE GAB-COND-ICD-BIT            (GAB-INDEX)  TO  CONDICDO.  GAS2UPD 
02547      MOVE GAB-COND-TB-BIT             (GAB-INDEX)  TO  CONDTABO.  GAS2UPD 
02548      MOVE GAB-COND-MENTAL-BIT         (GAB-INDEX)  TO  CONDMENO.  GAS2UPD 
02549      MOVE GAB-COND-DRUG-BIT           (GAB-INDEX)  TO  CONDDRGO.  GAS2UPD 
02550      MOVE GAB-COND-ALCOHOL-BIT        (GAB-INDEX)  TO  CONDALCO.  GAS2UPD 
02551      MOVE GAB-COND-OB-COMP-BIT        (GAB-INDEX)  TO  CONDOBCO.  GAS2UPD 
02552      MOVE GAB-COND-OB-NORM-BIT        (GAB-INDEX)  TO  CONDOBNO.  GAS2UPD 
02553      MOVE GAB-COND-MALIGNANCY-BIT     (GAB-INDEX)  TO  CONDMALO.  GAS2UPD 
02554      MOVE GAB-COND-CARDIAC-DISEASE-BIT(GAB-INDEX)  TO  CONDCARO.  GAS2UPD 
02555      MOVE GAB-COND-OBESITY-BIT        (GAB-INDEX)  TO  CONDOBSO.  GAS2UPD 
02556      MOVE GAB-COND-KIDNEY-DISEASE-BIT (GAB-INDEX)  TO  CONDKDYO.  GAS2UPD 
02557      MOVE GAB-COND-ACCIDENT-BIT       (GAB-INDEX)  TO  CONDACCO.  GAS2UPD 
02558      MOVE GAB-COND-PRE-EXIST-BIT      (GAB-INDEX)  TO  CONDPECO.  GAS2UPD 
02559      MOVE GAB-COND-NON-EMER-BIT       (GAB-INDEX)  TO  CONDNEMO.  GAS2UPD 
02560      MOVE GAB-COND-SUICIDE-BIT        (GAB-INDEX)  TO  CONDSUIO.  GAS2UPD 
02561      MOVE GAB-COND-TMJ-BIT            (GAB-INDEX)  TO  CONDTMJO.  GAS2UPD 
02562      MOVE GAB-COND-INF-BIT            (GAB-INDEX)  TO  CONDINFO.  GAS2UPD 
02563      MOVE GAB-COND-LIFE-THREAT-BIT    (GAB-INDEX)  TO  CONDLIFO.  GAS2UPD 
02564      MOVE GAB-COND-EMER-MED-BIT       (GAB-INDEX)  TO  CONDEMCO.  GAS2UPD 
02565      MOVE GAB-COND-EMER-ACC-BIT       (GAB-INDEX)  TO  CONDEACO.  GAS2UPD 
02566      MOVE GAB-COND-SER-MEN-ILL-BIT    (GAB-INDEX)  TO  CONDSMIO.  GAS2UPD 
02567      MOVE GAB-COND-NON-SER-MEN-ILL-BIT (GAB-INDEX)  TO  CONDNSMO. GAS2UPD 
02568                                                                   GAS2UPD 
02569      MOVE -1  TO  PERIODL.                                        GAS2UPD 
02570                                                                   GAS2UPD 
02571      MOVE GAB-COINS-FYI-VALUE(GAB-INDEX) TO  FYIVALO.             GAS2UPD 
02572      SET  CURNT-OCURS-BIN        TO  GAB-INDEX.                   GAS2UPD 
02573      MOVE CURNT-OCURS-BIN        TO  CURNT-OCURS-PKD.             GAS2UPD 
02574      MOVE CURNT-OCCURS-OUT       TO  COCURANO.                    GAS2UPD 
02575                                                                   GAS2UPD 
02576      IF GAB-ENTRY-COUNT  >  1                                     GAS2UPD 
02577      THEN                                                         GAS2UPD 
02578          COMPUTE  TOTAL-OCURS-UNK  =  GAB-ENTRY-COUNT  - 1        GAS2UPD 
02579          MOVE  TOTAL-OCCURS-OUT  TO  TOCURANO                     GAS2UPD 
02580      ELSE                                                         GAS2UPD 
02581          MOVE  '01'    TO  TOCURANO.                              GAS2UPD 
02582                                                                   GAS2UPD 
02583                                                                   GAS2UPD 
02584      MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              GAS2UPD 
02585                       IDGDSLTO,  IPGPSLTO   IPGSSLTO.             GAS2UPD 
02586                                                                   GAS2UPD 
02587      SET GAB-INT-INDEX  TO       1.                               GAS2UPD 
02588      SET GAB-INT-INDEX  DOWN BY  1.                               GAS2UPD 
02589                                                                   GAS2UPD 
02590  4400-300-DISPLAY-LOOP.                                           GAS2UPD 
02591                                                                   GAS2UPD 
02592      SET GAB-INT-INDEX  UP BY  1.                                 GAS2UPD 
02593      IF  GAB-INT-INDEX  >  5                                      GAS2UPD 
02594          GO TO 4400-800-SEND.                                     GAS2UPD 
02595                                                                   GAS2UPD 
02596      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  =  HIGH-VALUES      GAS2UPD 
02597          GO TO 4400-800-SEND.                                     GAS2UPD 
02598                                                                   GAS2UPD 
02599      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)   =   '#IBGR '       GAS2UPD 
02600          MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)               GAS2UPD 
02601                                   TO  ACWA-DISPLAY-LEN-7          GAS2UPD 
02602          MOVE ACWA-DISPLAY-LEN-7  TO  IBGRSLTO                    GAS2UPD 
02603          GO TO 4400-300-DISPLAY-LOOP.                             GAS2UPD 
02604                                                                   GAS2UPD 
02605      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)   =   '#IDGD '       GAS2UPD 
02606          MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)               GAS2UPD 
02607                                   TO  ACWA-DISPLAY-LEN-7          GAS2UPD 
02608          MOVE ACWA-DISPLAY-LEN-7  TO  IDGDSLTO                    GAS2UPD 
02609          GO TO 4400-300-DISPLAY-LOOP.                             GAS2UPD 
02610                                                                   GAS2UPD 
02611      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)   =   '#IPGN '       GAS2UPD 
02612          MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)               GAS2UPD 
02613                                   TO  ACWA-DISPLAY-LEN-7          GAS2UPD 
02614          MOVE ACWA-DISPLAY-LEN-7  TO  IPGNSLTO                    GAS2UPD 
02615          GO TO 4400-300-DISPLAY-LOOP.                             GAS2UPD 
02616                                                                   GAS2UPD 
02617      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)   =   '#IPGP '       GAS2UPD 
02618          MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)               GAS2UPD 
02619                                   TO  ACWA-DISPLAY-LEN-7          GAS2UPD 
02620          MOVE ACWA-DISPLAY-LEN-7  TO  IPGPSLTO                    GAS2UPD 
02621          GO TO 4400-300-DISPLAY-LOOP.                             GAS2UPD 
02622                                                                   GAS2UPD 
02623      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)   =   '#IPGT '       GAS2UPD 
02624          MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)               GAS2UPD 
02625                                   TO  ACWA-DISPLAY-LEN-7          GAS2UPD 
02626          MOVE ACWA-DISPLAY-LEN-7  TO  IPGTSLTO                    GAS2UPD 
02627          GO TO 4400-300-DISPLAY-LOOP.                             GAS2UPD 
02628                                                                   GAS2UPD 
02629      IF  GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)   =   '#IPGS '       GAS2UPD 
02630          MOVE GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)               GAS2UPD 
02631                                   TO  ACWA-DISPLAY-LEN-7          GAS2UPD 
02632          MOVE ACWA-DISPLAY-LEN-7  TO  IPGSSLTO                    GAS2UPD 
02633          GO TO 4400-300-DISPLAY-LOOP.                             GAS2UPD 
02634                                                                   GAS2UPD 
02635      MOVE WS-ABCODE-1CF3       TO  WS-ABCODE                      GAS2UPD 
02636      MOVE WS-ABCODE-1CF3-MSG   TO  WS-ABCODE-MSG                  GAS2UPD 
02637      PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       GAS2UPD 
02638                                                                   GAS2UPD 
02639                                                                   GAS2UPD 
02640  4400-800-SEND.                                                   GAS2UPD 
02641                                                                   GAS2UPD 
02642      PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      GAS2UPD 
02643                                                                   GAS2UPD 
02644      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS2UPD 
02645                                                                   GAS2UPD 
02646  4400-900-EXIT. EXIT.                                             GAS2UPD 
02647                                                                   GAS2UPD 
02648 /*****************************************************************GAS2UPD 
02649 *  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *GAS2UPD 
02650 *                                                                *GAS2UPD 
02651 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS2UPD 
02652 *          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *GAS2UPD 
02653 *          2. IF GROUP IS CRITICAL:                              *GAS2UPD 
02654 *              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *GAS2UPD 
02655 *                BENEFIT PROVISION.                              *GAS2UPD 
02656 *                - IF ON DATA BASE:                              *GAS2UPD 
02657 *                  - SCAN FOR #ACL TABULAR                       *GAS2UPD 
02658 *                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *GAS2UPD 
02659 *                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *GAS2UPD 
02660 *                      ON SCREEN AND ISSUE MESSAGE.              *GAS2UPD 
02661 ******************************************************************GAS2UPD 
02662  4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          GAS2UPD 
02663  4500-010.                                                        GAS2UPD 
02664                                                                   GAS2UPD 
02665      IF DELADDI  =  'CHG/DEL'     OR                              GAS2UPD 
02666         DELOLITI =  SPACES                                        GAS2UPD 
02667      THEN                                                         GAS2UPD 
02668         NEXT SENTENCE                                             GAS2UPD 
02669      ELSE                                                         GAS2UPD 
02670         GO TO 4500-900-EXIT.                                      GAS2UPD 
02671                                                                   GAS2UPD 
02672                                                                   GAS2UPD 
02673      MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. GAS2UPD 
02674                                                                   GAS2UPD 
02675      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS2UPD 
02676                 COMMAREA (DFHCOMMAREA)                            GAS2UPD 
02677                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS2UPD 
02678                                                                   GAS2UPD 
02679      GO TO 4500-900-EXIT.                                         GAS2UPD 
02680                                                                   GAS2UPD 
02681                                                                   GAS2UPD 
02682  4500-900-EXIT.   EXIT.                                           GAS2UPD 
02683                                                                   GAS2UPD 
02684 /*****************************************************************GAS2UPD 
02685 *  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *GAS2UPD 
02686 *                                                                *GAS2UPD 
02687 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS2UPD 
02688 *           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *GAS2UPD 
02689 *           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *GAS2UPD 
02690 *               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *GAS2UPD 
02691 *           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *GAS2UPD 
02692 *               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *GAS2UPD 
02693 *               +CDE+ INDICATOR (POSITION=8).                    *GAS2UPD 
02694 *              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *GAS2UPD 
02695 *               ENTER, CONTINUE PROCESSING.                      *GAS2UPD 
02696 ******************************************************************GAS2UPD 
02697  4600-000-UPDATE-CDE-STATUS     SECTION.                          GAS2UPD 
02698  4600-010.                                                        GAS2UPD 
02699                                                                   GAS2UPD 
02700                                                                   GAS2UPD 
02701      SET  ACWA-INDEX-1    TO  GAB-INDEX.                          GAS2UPD 
02702      MOVE WS-REQUEST-4600-CDE-STATUS  TO  ACWA-CDE-REQUEST-CODE.  GAS2UPD 
02703                                                                   GAS2UPD 
02704      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS2UPD 
02705                 COMMAREA (DFHCOMMAREA)                            GAS2UPD 
02706                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS2UPD 
02707                                                                   GAS2UPD 
02708      IF  ACWA-CDE-RETURN-DONT-SEND                                GAS2UPD 
02709          EXEC CICS  RETURN   END-EXEC.                            GAS2UPD 
02710                                                                   GAS2UPD 
02711  4600-900-EXIT.   EXIT.                                           GAS2UPD 
02712                                                                   GAS2UPD 
02713 /*****************************************************************GAS2UPD 
02714 *  4700  -  UPDATE W/F CONTROL RECORD                            *GAS2UPD 
02715 *                                                                *GAS2UPD 
02716 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS2UPD 
02717 *          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *GAS2UPD 
02718 *             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *GAS2UPD 
02719 *             RECORD AND THE INTERNAL TABULAR RECORDS; IF THE    *GAS2UPD 
02720 *             CDE STATUS HAS CHANGED.                            *GAS2UPD 
02721 *          2. REWRITE W/F CONTROL RECORD                         *GAS2UPD 
02722 ******************************************************************GAS2UPD 
02723  4700-000-UPDATE-CONTROL-RECORD SECTION.                          GAS2UPD 
02724  4700-010.                                                        GAS2UPD 
02725                                                                   GAS2UPD 
02726      SET  ACWA-INDEX-1     TO  GAB-INDEX.                         GAS2UPD 
02727      MOVE WS-REQUEST-4700-CNTL-UPDATE  TO  ACWA-CDE-REQUEST-CODE. GAS2UPD 
02728                                                                   GAS2UPD 
02729      EXEC CICS  LINK   PROGRAM ('GACDEPGM')                       GAS2UPD 
02730                 COMMAREA (DFHCOMMAREA)                            GAS2UPD 
02731                 LENGTH(LENGTH OF DFHCOMMAREA)      END-EXEC.      GAS2UPD 
02732                                                                   GAS2UPD 
02733  4700-900-EXIT.  EXIT.                                            GAS2UPD 
02734                                                                   GAS2UPD 
02735 ******************************************************************GAS2UPD 
02736 *  4900  -  R E S E T   O T H E R   I N T E R N A L   T A B S     GAS2UPD 
02737 *                                                                 GAS2UPD 
02738 *    FUNCTION  (VIA CDE MODULE GACDEPGM)                          GAS2UPD 
02739 *          READ W/F ACCUM'S INTERNAL TABULAR RECORDS, THOSE ON    GAS2UPD 
02740 *          W/F ONLY.  RESET THE CDE STATUS INDICATOR ON THIS      GAS2UPD 
02741 *          INTERNAL TO EITHER 1U OR 2 BASED ON THE INTERNAL       GAS2UPD 
02742 *          DESCRIPTOR, THEN REWRITE THIS RECORD.                  GAS2UPD 
02743 ******************************************************************GAS2UPD 
02744  4900-RESET-OTHER-INT-TABS      SECTION.                          GAS2UPD 
02745                                                                   GAS2UPD 
02746      MOVE ZERO  TO  WS-INTRNL-TABS-TO-CHG-CNT.                    GAS2UPD 
02747      PERFORM 4900-COUNT-INT-TAB                                   GAS2UPD 
02748         VARYING GAB-INT-INDEX  FROM  1  BY  1                     GAS2UPD 
02749         UNTIL GAB-INT-INDEX  NOT <                                GAS2UPD 
02750                          GAB-INTERNAL-TABULAR-COUNT(GAB-INDEX)  ORGAS2UPD 
02751               GAB-INT-ID(GAB-INDEX GAB-INT-INDEX)  =  HIGH-VALUES.GAS2UPD 
02752                                                                   GAS2UPD 
02753      IF WS-INTRNL-TABS-TO-CHG-CNT  >  ZERO                        GAS2UPD 
02754         MOVE WS-REQUEST-4900-CNTL-UPDATE  TO                      GAS2UPD 
02755                                            ACWA-CDE-REQUEST-CODE  GAS2UPD 
02756         EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                   GAS2UPD 
02757                    COMMAREA (DFHCOMMAREA)                         GAS2UPD 
02758                    LENGTH(LENGTH OF DFHCOMMAREA)        END-EXEC. GAS2UPD 
02759                                                                   GAS2UPD 
02760      GO TO 4999-EXIT.                                             GAS2UPD 
02761                                                                   GAS2UPD 
02762  4900-COUNT-INT-TAB.                                              GAS2UPD 
02763      IF GAB-INT-SLOT(GAB-INDEX GAB-INT-INDEX)  >  +8999999        GAS2UPD 
02764         ADD 1  TO  WS-INTRNL-TABS-TO-CHG-CNT.                     GAS2UPD 
02765                                                                   GAS2UPD 
02766  4999-EXIT.       EXIT.                                           GAS2UPD 
02767                                                                   GAS2UPD 
02768 /*****************************************************************GAS2UPD 
02769 *     READ ALL LEVEL TABULAR FROM PROVISION POOL                  GAS2UPD 
02770 *                                                                 GAS2UPD 
02771 ******************************************************************GAS2UPD 
02772  5000-000-READ-PROD-ALL-LVL-TAB  SECTION.                         GAS2UPD 
02773                                                                   GAS2UPD 
02774      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   GAS2UPD 
02775               GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ACL-FIXED-LEN  +GAS2UPD 
02776              (GC-GCTABULR-ACL-VARY-LEN  *                         GAS2UPD 
02777                                    GC-GCTABULR-ACL-VARY-MAX-OCUR).GAS2UPD 
02778                                                                   GAS2UPD 
02779      IF ACWA-PR-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS2UPD 
02780         NEXT SENTENCE                                             GAS2UPD 
02781      ELSE                                                         GAS2UPD 
02782         EXEC CICS GETMAIN                                         GAS2UPD 
02783                SET(ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD)      GAS2UPD 
02784                INITIMG(WS-HEX-00)                                 GAS2UPD 
02785                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS2UPD 
02786                END-EXEC                                           GAS2UPD 
02787         SET ACWA-PR-ALL-LEVEL-TAB-PNTR     TO                     GAS2UPD 
02788                  ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD.        GAS2UPD 
02789                                                                   GAS2UPD 
02790      MOVE GCIO-WORKFILE-KEY       TO WS-SV-RESTO-KY.              GAS2UPD 
02791      MOVE TABIDI                  TO  GCIO-TAB-TABULAR-ID.        GAS2UPD 
02792      MOVE WRK-TAB-PROV-COPY-SLOT  TO  GCIO-TAB-SLOT-NO.           GAS2UPD 
02793      MOVE GCIO-WORKFILE-KEY       TO  GCIOA-FILE-KEY.             GAS2UPD 
02794      MOVE GC-GCIO-AREA-2          TO  GCIOA-IO-AREA-TO-USE.       GAS2UPD 
02795      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIOA-FILE-ACCESS-CODE.     GAS2UPD 
02796      MOVE GC-GCTABULR-DDNAME      TO  GCIOA-FILE-DDNAME.          GAS2UPD 
02797      MOVE GC-GCTABULR-ACL-VARY-MAX-OCUR                           GAS2UPD 
02798           TO  GAB2-ENTRY-COUNT.                                   GAS2UPD 
02799                                                                   GAS2UPD 
02800        MOVE WS-SV-RESTO-KY   TO  GCIO-WORKFILE-KEY.               GAS2UPD 
02801                                                                   GAS2UPD 
02802      IF GCIO-TAB-TABULAR-ID  =  GAB2-PROVISION-ID AND             GAS2UPD 
02803         GAB2-PROVISION-SLOT-NO  NUMERIC AND                       GAS2UPD 
02804         GCIO-TAB-SLOT-NO     =  GAB2-PROVISION-SLOT-NO            GAS2UPD 
02805         GO TO 5000-900-EXIT.                                      GAS2UPD 
02806                                                                   GAS2UPD 
02807      EXEC CICS  LINK   PROGRAM  ('GCIOPGM')                       GAS2UPD 
02808                 COMMAREA (PR-IO-PARM-ALL-LVL-TAB-RECORD)          GAS2UPD 
02809                 LENGTH (WS-IO-PARM-WRK-ALL-LVL-LEN)    END-EXEC.  GAS2UPD 
02810                                                                   GAS2UPD 
02811      IF NOT GCIOA-GOOD-RETURN                                     GAS2UPD 
02812         MOVE WS-ABCODE-1CF7        TO  WS-ABCODE                  GAS2UPD 
02813         MOVE WS-ABCODE-1CF7-MSG    TO  WS-ABCODE-MSG              GAS2UPD 
02814         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS2UPD 
02815                                                                   GAS2UPD 
02816  5000-900-EXIT.     EXIT.                                         GAS2UPD 
02817                                                                   GAS2UPD 
02818 /*****************************************************************GAS2UPD 
02819 * 6000  BUILD GROUP SPEC KEY                                     *GAS2UPD 
02820 *                                                                *GAS2UPD 
02821 *    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *GAS2UPD 
02822 ******************************************************************GAS2UPD 
02823  6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          GAS2UPD 
02824  6000-010.                                                        GAS2UPD 
02825                                                                   GAS2UPD 
02826      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS2UPD 
02827      MOVE  'G'                 TO  GCIO-WRK-STATUS-CODE.          GAS2UPD 
02828      MOVE  'G3'                TO  GCIO-WRK-RECORD-TYPE.          GAS2UPD 
02829      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS2UPD 
02830      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS2UPD 
02831      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS2UPD 
02832      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS2UPD 
02833      MOVE SPACES               TO  GCIO-WRK-LINE-OF-BUS,          GAS2UPD 
02834                                    GCIO-WRK-PROVIDER-CONTROL.     GAS2UPD 
02835      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS2UPD 
02836      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS2UPD 
02837      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS2UPD 
02838      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS2UPD 
02839      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS2UPD 
02840      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS2UPD 
02841      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS2UPD 
02842                                                                   GAS2UPD 
02843  6000-900-EXIT. EXIT.                                             GAS2UPD 
02844                                                                   GAS2UPD 
02845 ******************************************************************GAS2UPD 
02846 * 6100  BUILD CONTRACT KEY                                       *GAS2UPD 
02847 *                                                                *GAS2UPD 
02848 *    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *GAS2UPD 
02849 ******************************************************************GAS2UPD 
02850  6100-000-BUILD-CONTRACT-KEY    SECTION.                          GAS2UPD 
02851  6100-010.                                                        GAS2UPD 
02852                                                                   GAS2UPD 
02853      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS2UPD 
02854      MOVE  'C'                 TO  GCIO-WRK-STATUS-CODE.          GAS2UPD 
02855      MOVE  'C3'                TO  GCIO-WRK-RECORD-TYPE.          GAS2UPD 
02856      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS2UPD 
02857      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS2UPD 
02858      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS2UPD 
02859      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS2UPD 
02860      MOVE GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS.          GAS2UPD 
02861      MOVE GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL.     GAS2UPD 
02862      MOVE GCA-FAM-REL-LVL      TO  GCIO-WRK-FAMILY-RELATION-LVL.  GAS2UPD 
02863      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS2UPD 
02864      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID          GAS2UPD 
02865      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS2UPD 
02866      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS2UPD 
02867      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS2UPD 
02868      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS2UPD 
02869  6100-900-EXIT. EXIT.                                             GAS2UPD 
02870                                                                   GAS2UPD 
02871 /*****************************************************************GAS2UPD 
02872 * 6200  BUILD BEN PROV KEY                                       *GAS2UPD 
02873 *                                                                *GAS2UPD 
02874 *    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *GAS2UPD 
02875 ******************************************************************GAS2UPD 
02876  6200-000-BUILD-BEN-PROV-KEY    SECTION.                          GAS2UPD 
02877  6200-010.                                                        GAS2UPD 
02878                                                                   GAS2UPD 
02879      MOVE SPACES                TO  GCIO-WORKFILE-KEY.            GAS2UPD 
02880      MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE.         GAS2UPD 
02881      MOVE  'C5'                 TO  GCIO-WRK-RECORD-TYPE.         GAS2UPD 
02882      MOVE GCA-PLAN-CODE         TO  GCIO-WRK-PLAN-CODE.           GAS2UPD 
02883      MOVE GCA-GROUP-NUM         TO  GCIO-WRK-GROUP-NUM.           GAS2UPD 
02884      MOVE GCA-SECTION-NUM       TO  GCIO-WRK-SECTION-NUM.         GAS2UPD 
02885      MOVE GCA-PKG-CODE          TO  GCIO-WRK-PKG-CODE.            GAS2UPD 
02886      MOVE GCA-L-O-B             TO  GCIO-WRK-LINE-OF-BUS.         GAS2UPD 
02887      MOVE GCA-PROV-CTL          TO  GCIO-WRK-PROVIDER-CONTROL.    GAS2UPD 
02888      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS2UPD 
02889      MOVE GCA-EFFDT-CEN         TO  GCIO-WRK-EFFDT-CEN.           GAS2UPD 
02890      MOVE GCA-BEN-PROV-ID       TO  GCIO-WRK-PROVISION-ID.        GAS2UPD 
02891      MOVE +9999999              TO  GCIO-WRK-PROVISION-SLOT-NO.   GAS2UPD 
02892      MOVE TABIDI                TO  GCIO-WRK-TAB-PROVISION-ID.    GAS2UPD 
02893      MOVE TABSLTNI              TO  ACWA-DISPLAY-LEN-7.           GAS2UPD 
02894      MOVE ACWA-DISPLAY-LEN-7    TO  GCIO-WRK-TAB-PROV-SLOT-NO.    GAS2UPD 
02895                                                                   GAS2UPD 
02896  6200-900-EXIT. EXIT.                                             GAS2UPD 
02897                                                                   GAS2UPD 
02898  6400-000-XCTL-TO-MAIN-MENU SECTION.                              GAS2UPD 
02899  6400-900-EXIT.  EXIT.                                            GAS2UPD 
02900 ***************************************************************** GAS2UPD 
02901 * CCSP  NDF - FLAGSHIP RENOVATION - OCT 2004                      GAS2UPD 
02902 * - REMOVED 6400-010 ROUTINE - DEAD CODE                          GAS2UPD 
02903 * - REMOVED 7000-000-PRINT-HARDCOPY SECTION - DEAD CODE           GAS2UPD 
02904 * - REMOVED 7000-010 ROUTINE - DEAD CODE                          GAS2UPD 
02905 ***************************************************************** GAS2UPD 
02906                                                                   GAS2UPD 
02907 /*****************************************************************GAS2UPD 
02908 * 7900  RESET ATTRIBUTES                                         *GAS2UPD 
02909 ******************************************************************GAS2UPD 
02910  7900-000-RESET-ATTRIBUTES      SECTION.                          GAS2UPD 
02911  7900-010.                                                        GAS2UPD 
02912                                                                   GAS2UPD 
02913      MOVE DFHBMUNF  TO  BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA    GAS2UPD 
02914               LOBA      PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA    GAS2UPD 
02915               REININDA  INTRVALA  INTTYPEA  CLMLVLIA  DEFINTNA    GAS2UPD 
02916               BNMXVALA  DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA    GAS2UPD 
02917               FYIVALA   MANAPLIA  FDLRCLIA  ASCDSCDA  PERLIMTA    GAS2UPD 
02918               CONDALLA  CONDEXCA  CONDICDA  CONDTABA  BISNDINA    GAS2UPD 
02919               CONDMENA  CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA    GAS2UPD 
02920               CONDMALA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA    GAS2UPD 
02921               CONDPECA  CONDNEMA  CONDSUIA  CONDTMJA  CONDINFA    GAS2UPD 
02922               IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA    GAS2UPD 
02923               AGEQLLA   AGEQLHA   CONDLIFA  IPGSOPTA              GAS2UPD 
02924               CONDEMCA  CONDEACA  CONDSMIA CONDNSMA               GAS2UPD 
02925               IDGDOPTA  IPGPOPTA  RELPINDA  AGELIMLA AGELIMHA     GAS2UPD 
02926               FEAKINDA  ACCUMIDA  CAPINDA   SABDINDA              GAS2UPD 
02926               BENTYPA   TIERCDA   TIERLVA.                        GAS2UPD 
02927                                                                   GAS2UPD 
02928                                                                   GAS2UPD 
02929      IF  DELADDO  =  'CHG/DEL'                                    GAS2UPD 
02930      THEN                                                         GAS2UPD 
02931          NEXT SENTENCE                                            GAS2UPD 
02932      ELSE                                                         GAS2UPD 
02933          GO TO 7900-900-EXIT.                                     GAS2UPD 
02934                                                                   GAS2UPD 
02935                                                                   GAS2UPD 
02936      IF  CDEINDO  =  '+CDE+'                                      GAS2UPD 
02937      THEN                                                         GAS2UPD 
02938 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS2UPD 
02939          MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       GAS2UPD 
02940                 AGELIMA   PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   GAS2UPD 
02941                 AGEQLTA   COPAYITA  INTDESTA CONDTG1A  CONDTG2A   GAS2UPD 
02942                           MANAPLTA  PERLITTA BISNDITA             GAS2UPD 
02943      ELSE                                                         GAS2UPD 
02944          NEXT SENTENCE.                                           GAS2UPD 
02945                                                                   GAS2UPD 
02946                                                                   GAS2UPD 
02947      IF  CDEINDO  =  '+CDE-'                                      GAS2UPD 
02948      THEN                                                         GAS2UPD 
02949 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS2UPD 
02950          MOVE DFHBMABF TO DLOPTLTA  PERIOTA  BENVLQTA  LOTA       GAS2UPD 
02951                 AGELIMA   PLCTRTTA  FAMINDTA SRVGRUTA  CSTCOTTA   GAS2UPD 
02952                 RELPINTA  COPAYITA  INTDESTA CONDTG1A  CONDTG2A   GAS2UPD 
02953                           MANAPLTA  PERLITTA BISNDITA             GAS2UPD 
02954 *---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        GAS2UPD 
02955          MOVE DFHBMASF TO DELOPTNA  PERIODA  BENVLQLA  LOBA       GAS2UPD 
02956        AGELIMLA AGELIMHA  PLCTRMTA  FAMINDIA SRVGRUPA  CSTCONTA   GAS2UPD 
02957        AGEQLLA  AGEQLHA   COPAYINA  INTDESKA CONDALLA  CONDEXCA   GAS2UPD 
02958                 CONDLIFA  CONDICDA  CONDTABA CONDMENA  CONDDRGA   GAS2UPD 
02959                 CONDEMCA  CONDEACA  CONDSMIA CONDNSMA             GAS2UPD 
02960                           CONDALCA  CONDOBCA CONDOBNA  CONDMALA   GAS2UPD 
02961                           CONDCARA  CONDOBSA CONDKDYA  CONDACCA   GAS2UPD 
02962                           CONDPECA  CONDNEMA CONDSUIA  CONDTMJA   GAS2UPD 
02963                           MANAPLIA  PERLIMTA BISNDINA  CONDINFA   GAS2UPD 
02964          IF  ERRMSGO  >  SPACES                                   GAS2UPD 
02965          THEN                                                     GAS2UPD 
02966              NEXT SENTENCE                                        GAS2UPD 
02967          ELSE                                                     GAS2UPD 
02968              SET  WT-01-INDEX  TO  +08                            GAS2UPD 
02969              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO    GAS2UPD 
02970      ELSE                                                         GAS2UPD 
02971          NEXT SENTENCE.                                           GAS2UPD 
02972                                                                   GAS2UPD 
02973  7900-900-EXIT. EXIT.                                             GAS2UPD 
02974                                                                   GAS2UPD 
02975 /*****************************************************************GAS2UPD 
02976 * 9000  SEND ERASE THEN RETURN                                   *GAS2UPD 
02977 ******************************************************************GAS2UPD 
02978  9000-000-SEND-ERASE-RETURN     SECTION.                          GAS2UPD 
02979  9000-010.                                                        GAS2UPD 
02980                                                                   GAS2UPD 
02981      MOVE DFHBMASD  TO   CARYOVTA  MAXOVRTA   CARYOVRA  MAXOVRDA  GAS2UPD 
02982                          TIMEDLRA  TIMEDOLA.                      GAS2UPD 
02983                                                                   GAS2UPD 
02984      MOVE -1  TO  ERRMSGL.                                        GAS2UPD 
02985                                                                   GAS2UPD 
02986      EXEC CICS  SEND   MAP ('GA1XI01')    ERASE  CURSOR           GAS2UPD 
02987                 MAPSET('GA1XSET')    END-EXEC.                    GAS2UPD 
02988                                                                   GAS2UPD 
02989      EXEC CICS  RETURN   END-EXEC.                                GAS2UPD 
02990                                                                   GAS2UPD 
02991  9000-900-EXIT.    EXIT.                                          GAS2UPD 
02992                                                                   GAS2UPD 
02993 /*****************************************************************GAS2UPD 
02994 * 9010  SEND DATAONLY AND RETURN                                 *GAS2UPD 
02995 ******************************************************************GAS2UPD 
02996  9010-000-SEND-DATAONLY-RETURN  SECTION.                          GAS2UPD 
02997  9010-010.                                                        GAS2UPD 
02998                                                                   GAS2UPD 
02999      MOVE -1  TO  ERRMSGL.                                        GAS2UPD 
03000                                                                   GAS2UPD 
03001      EXEC CICS  SEND   MAP('GA1XI01')  DATAONLY  CURSOR           GAS2UPD 
03002                 MAPSET('GA1XSET')       END-EXEC.                 GAS2UPD 
03003                                                                   GAS2UPD 
03004      EXEC CICS  RETURN   END-EXEC.                                GAS2UPD 
03005                                                                   GAS2UPD 
03006  9010-900-EXIT.     EXIT.                                         GAS2UPD 
03007                                                                   GAS2UPD 
03008 ***************************************************************** GAS2UPD 
03009 * CCSP  NDF - FLAGSHIP RENOVATION - OCT 2004                      GAS2UPD 
03010 * - REMOVED 9100-000-GETMAIN SECTION - DEAD CODE                  GAS2UPD 
03011 * - REMOVED 9100-010 ROUTINE - DEAD CODE                          GAS2UPD 
03012 * - REMOVED 9200-000-GREGORIAN-TO-JULIAN SECTION - DEAD CODE      GAS2UPD 
03013 * - REMOVED 9200-010 ROUTINE - DEAD CODE                          GAS2UPD 
03014 * - REMOVED 9300-000-JULIAN-TO-GREGORIAN SECTION - DEAD CODE      GAS2UPD 
03015 * - REMOVED 9300-010 ROUTINE - DEAD CODE                          GAS2UPD 
03016 ***************************************************************** GAS2UPD 
03017 /*****************************************************************GAS2UPD 
03018 * 9800  E R R O R   M S G   T H E N   A B E N D                  *GAS2UPD 
03019 *                                                                *GAS2UPD 
03020 *    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *GAS2UPD 
03021 *  AND THEN ABENDS USING THE ABEND CODE EARLIER MEFINED.         *GAS2UPD 
03022 ******************************************************************GAS2UPD 
03023  9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          GAS2UPD 
03024  9800-010.                                                        GAS2UPD 
03025                                                                   GAS2UPD 
03026      MOVE -1              TO  MFRMSLTL.                           GAS2UPD 
03027      MOVE WS-ABCODE-MSG   TO  ERRMSGO.                            GAS2UPD 
03028                                                                   GAS2UPD 
03029      EXEC CICS  SEND   MAP ('GA1XI01') ERASE  CURSOR   WAIT       GAS2UPD 
03030                 MAPSET('GA1XSET')      END-EXEC.                  GAS2UPD 
03031                                                                   GAS2UPD 
03032      EXEC CICS  ABEND   ABCODE(WS-ABCODE)   END-EXEC.             GAS2UPD 
03033                                                                   GAS2UPD 
03034  9800-900-EXIT. EXIT.                                             GAS2UPD 
