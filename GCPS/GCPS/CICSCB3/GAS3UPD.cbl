00001  IDENTIFICATION DIVISION.                                         02/18/05
00002  PROGRAM-ID. GAS3UPD.                                             GAS3UPD 
00003 **** THIS IS A COBOL/2 PROGRAM ***                                   LV005
00004  AUTHOR. N ELBAZ.                                                 GAS3UPD 
00005  DATE-WRITTEN.   07/14/87.                                        GAS3UPD 
00006  DATE-COMPILED.                                                   GAS3UPD 
00007      SKIP3                                                        GAS3UPD 
00008 ******************************************************************GAS3UPD 
00009 *   GAS3UPD         ALL LEVEL TABULAR MAINTENANCE PROGRAM        *GAS3UPD 
00010 *        DECUCTIBLE LIMITS ACCUMULATOR DESCRIPTION TABLAR - #ADL *GAS3UPD 
00011 *                                                                *GAS3UPD 
00012 *     THIS PROGRAM WILL PERFORM ADD/CHANGE/DELETE MAINTENANCE TO *GAS3UPD 
00013 *   ENTRIES ON THE ALL LEVEL TABULAR RECORD.  THE TABULAR RECORD *GAS3UPD 
00014 *   CAN CONTAIN UP TO 29 ENTRIES IN A TABLE, EACH ENTRY HAS A    *GAS3UPD 
00015 *   NUMBER OF FIELDS AND ANOTHER SMALL TABLE, THIS 2NDARY TABLE  *GAS3UPD 
00016 *   IS A POINTER TO AN INTERNAL TABULAR RECORD.  THE PROGRAM     *GAS3UPD 
00017 *   OPERATES IN TWO MODES AN ADD/CHANGE AND A CHANGE/DELETE MODE.*GAS3UPD 
00018 *                                                                *GAS3UPD 
00019 *     THE CHG/DEL SCREEN WILL DISPLAY AN ENTRY CURRENTLY ON THE  *GAS3UPD 
00020 *   ALL LEVEL TABULAR RECORD.  THE OPERATOR WILL THEN CHANGE ANY *GAS3UPD 
00021 *   FIELD OR ADD, CHANGE, OR DELETE AN INTERNAL TABULAR; THERE IS*GAS3UPD 
00022 *   ALSO THE OPTION OF DELETING THE WHOLE ENTRY IN THE TABULAR,  *GAS3UPD 
00023 *   INTERNAL TABULARS INCLUDED, THIS OPTION CAN BE SELECTED BY   *GAS3UPD 
00024 *   PLACING A 'D' IN THE DELETE OPTION FIELD.                    *GAS3UPD 
00025 *                                                                *GAS3UPD 
00026 *    THE CHG/ADD SCREEN WILL BE SHOWN THE OPERATOR WHEN THEY WANT*GAS3UPD 
00027 *   TO ADD A NEW ENTRY INTO THE TABLE. FROM HERE THE OPERATOR CAN*GAS3UPD 
00028 *   FILL THE ENTRY, THEN REVIEW AND CHANGE THE NEW ENTRY.  AFTER *GAS3UPD 
00029 *   THE OPERATOR KEYS ENTER ON THE REVIEW SCREEN, THE PROGRAM    *GAS3UPD 
00030 *   ASSUMES THAT THEY WANT TO ADD ANOTHER ENTRY AND SO DISPLAYS  *GAS3UPD 
00031 *   THE SKELETON FOR THE OPERATOR TO OVERLAY.                    *GAS3UPD 
00032 *                                                                *GAS3UPD 
00033 *   FUNC CODE: GAS3                                              *GAS3UPD 
00034 *                          ********************************      *GAS3UPD 
00035 *                          *   THIS MAPSET IS SHARED BY   *      *GAS3UPD 
00036 *                          *   THE FOLLOWING MODULES:     *      *GAS3UPD 
00037 *                          *   1. GA1BPGM                 *      *GAS3UPD 
00038 *                          *   2. GA1CPGM                 *      *GAS3UPD 
00039 *                          *   3. GA1DPGM                 *      *GAS3UPD 
00040 *   MAPSET:    GA1XSETC ==>*   4. GA1EPGM                 *      *GAS3UPD 
00041 *                          *   5. GASEDIT1                *      *GAS3UPD 
00042 *                          *   6. GACDEPGM                *      *GAS3UPD 
00043 *                          *   7. GK1BPGM                 *      *GAS3UPD 
00044 *                          *   8. GK1CPGM                 *      *GAS3UPD 
00045 *                          *   9. GK1DPGM                 *      *GAS3UPD 
00046 *                          *  10. GK1EPGM                 *      *GAS3UPD 
00047 *                          *  11. GAS1UPD                 *      *GAS3UPD 
00048 *                          *  12. GAS2UPD                 *      *GAS3UPD 
00049 *                          *  11. GAS3UPD                 *      *GAS3UPD 
00050 *                          *  11. GAS4UPD                 *      *GAS3UPD 
00051 *                          ********************************      *GAS3UPD 
00052 *                                                                *GAS3UPD 
00053 *   FILES:     GCPSWORK           GCGRPSPC                       *GAS3UPD 
00054 *              GCTABULR           GCSTABLR                       *GAS3UPD 
00055 *              GCCONTR            GCSPROVN                       *GAS3UPD 
00056 *                                                                *GAS3UPD 
00057 ******************************************************************GAS3UPD 
00058                                                                   GAS3UPD 
00059 /    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS3UPD 
00060 *    *-*         U P D A T E   H I S T O R Y         *-*          GAS3UPD 
00061 *    *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*          GAS3UPD 
00062                                                                   GAS3UPD 
00063 *NUM-* *-DATE-* *WHO* *-----------DESCRIPTION--------------------*GAS3UPD 
00064 *                                                                *GAS3UPD 
00065 * D200 04/24/87  JLA  BREAK INTO MULTIPLE MODULES.               *GAS3UPD 
00066 * D200 06/07/87  NGE  SEPERATE CHANGE/DELETE FUNCTION TO         *GAS3UPD 
00067 *                     ANOTHER MODULE.                            *GAS3UPD 
00068 * N121 08/10/87  NGE  ADD NEW FIELD -DEFINITION-                 *GAS3UPD 
00069 * N126 08/28/87  JLA  ADD LOGIC FOR SUICIDE BIT                  *GAS3UPD 
00070 *                                                                *GAS3UPD 
00071 *  ????      09/29/87  JLA  FIX EXISTING CDE PROBLEM IN THE      *GAS3UPD 
00072 *                             4600- SECTION THAT CAUSED THE CDE  *GAS3UPD 
00073 *                             MODIFIED STATUS TO BE SET.         *GAS3UPD 
00074 *                                                                *GAS3UPD 
00075 * D143 02/01/88  DES  CDE/NON-CDE CHANGES                        *GAS3UPD 
00076 *                                                                *GAS3UPD 
00077 *  D126      02/24/88  JLA  1. CHANGE OPTION FILE SELECTION 'S'  *GAS3UPD 
00078 *                              TO 'A'.                           *GAS3UPD 
00079 *                                                                *GAS3UPD 
00080 *  R1218     06/02/88  NGE  1. CHANGE ACCUM LOGIC FOR MAPING     *GAS3UPD 
00081 *                                                                *GAS3UPD 
00082 * ????    08/03/88  NGE  FIX ADDING ACCURS LOGIC TO FLAG  THE    *GAS3UPD 
00083 *                            ACCUMS AS A CDE & GAS1PGM INTERNAL  *GAS3UPD 
00084 *                            TAB LOGIC TO FLAG ITS ACCUM WITH CDE*GAS3UPD 
00085 *                            WHEN THE INTERNL FLAGED CDE.        *GAS3UPD 
00086 *                                                                *GAS3UPD 
00087 * ????    09/14/88  NGE  FIX INTERNAL TABS DELETE LOGIC FOR      *GAS3UPD 
00088 *                        UPDATING CDE COUNTERS DEPENDING ON THE  *GAS3UPD 
00089 *                        INTRNL TAB RECORD NOT THE CDE STATUS    *GAS3UPD 
00090 *                        IN THE ACCUM RECORD ATTACHED.           *GAS3UPD 
00091 *                                                                *GAS3UPD 
00092 *D????  01/13/89 ENW   DARKENED THE BISCENDING IND FIELD.         GAS3UPD 
00093 *                                                                *GAS3UPD 
00094 * D200 05/18/89  NGE  ADD TWO NEW COND-BITS TMJ AND INF,         *GAS3UPD 
00095 *                     TEMPROMAND-JOINT AND INFERTILITY-COND.     *GAS3UPD 
00096 *                                                                *GAS3UPD 
00097 * 11154   10/15/90  NGE 1. EXPAND OCCUR LENGTH FROM 132 TO 176.  *GAS3UPD 
00098 * D184, D185, D238,     2. INCREASE MAX OCCURS FROM 29 TO 46.    *GAS3UPD 
00099 * D139, D263, N125,     3. EXPAND DEFINITION FIELD TO 2 BYTES.   *GAS3UPD 
00100 * D270                  4. ADD AGE-LIMIT-FROM AND AGE-LIMIT-TO.  *GAS3UPD 
00101 *                       5. ADD RELATIONSHIP-IND (CDE ELEMENT).   *GAS3UPD 
00102 *                       6. ADD NEW INTERNAL TABS #IDGD AND #IPGP.*GAS3UPD 
00103 *                       7. >>> CONVERT TO COBOL/2 <<<.           *GAS3UPD 
00104 *                                                                *GAS3UPD 
00105 * 11154 01/17/91  NGE 1. ADD AGE-QUAL-IND-FROM AND AGE-QUAL-TO   *GAS3UPD 
00106 *                        TO ALL ACCUM TABULARS, AS CDE FIELDS.   *GAS3UPD 
00107 *                       2. REMOVE RELATIONSHIP-IND FROM CDE LOGIC*GAS3UPD 
00108 *                                                                *GAS3UPD 
00109 * 11154 02/19/91  NGE  REDUCE OCCURS MAX NUM FROM 46 TO 44.      *GAS3UPD 
00110 *                                                                *GAS3UPD 
00111 *D12009 09/16/91  BSO  CHANGES FOR FRL EXPANSION                 *GAS3UPD 
00112 *                                                                *GAS3UPD 
00113 * P-034  09/17/91  ENW  FIXED PROGRAM ERROR LEFT OVER FROM THE   *GAS3UPD 
00114 *                       11154 ACCUM EXPANSION. MOVED HIGH VALUES *GAS3UPD 
00115 *                       TO THE LAST OCCURS AFTER A DELETE OF AN  *GAS3UPD 
00116 *                       INTERNAL TABULAR.                        *GAS3UPD 
00117 *                                                                *GAS3UPD 
00118 * 12262  02/28/92 TPM ADD NEW COND-BIT LIF (LIFE-THREATING)      *GAS3UPD 
00119 *                     COND-LIFE-THREAT-BIT                       *GAS3UPD 
00120 *                                                                *GAS3UPD 
00121 *  D303  02/03/97 DAU ADD FEAK INDICATOR                         *GAS3UPD 
00122 *                                                                *GAS3UPD 
00123 * 14726/ 11/10/97 DAU ADDED CODE TO SUPPORT THE YEAR 2000 AND    *GAS3UPD 
00124 * 15057               THE EXPANSION OF THE GROUP SPECIFIC AND    *GAS3UPD 
00125 *                     CONTRACT KEY TO SUPPORT THE TEXAS MERGER.  *GAS3UPD 
00126 *                                                                *GAS3UPD 
00127 * 14726/ 11/30/97  AB   MODIFIED TO BECOME MILLENNIUM COMPLIANT  *GAS3UPD 
00128 * 15057                 AND TO ADD PACKAGE CODE, PLAN CODE, AND  *GAS3UPD 
00129 *                       INCREASE GROUP AND SECTION NUMBERS.      *GAS3UPD 
00130 *                                                                *GAS3UPD 
00131 *  D341  10/07/98  GDM  HIDE TIME/DOLLAR FIELD FROM SCREEN       *GAS3UPD 
00132 *                                                                *GAS3UPD 
00133 *        07/07/00  GSP  ADD LOGIC FOR NEW #IPGS INTERNAL TABULAR.*GAS3UPD 
00134 *                                                                *GAS3UPD 
00135 *D352  09/21/00  GDM  ADD ACCUM IDENTIFIER                       *GAS3UPD 
00136 *                                                                *GAS3UPD 
00137 *        11/29/00  GSP  ADD LOGIC TO DISPLAY MESSAGE IF A 6TH    *GAS3UPD 
00138 *                       INTERNAL TABULAR IS ATTEMPTED TO BE      *GAS3UPD 
00139 *                       ADDED.                                   *GAS3UPD 
00140 *                                                                *GAS3UPD 
00141 *        01/12/01 GSP ADD LOGIC TO PREVENT INTERNAL TABULAR      *GAS3UPD 
00142 *                     COUNT FROM BEING INCREASED TO GREATER      *GAS3UPD 
00143 *                     THAN 5.                                    *GAS3UPD 
00144 *                                                                *GAS3UPD 
00145 *        11/15/01 AKK ADD SUPPORT FOR 4 NEW BITS, 2 FOR EMER     *GAS3UPD 
00146 *                     AND TWO FOR SERIOUS MENTAL ILLNESS         *GAS3UPD 
00147 *                                                                *GAS3UPD 
00148 *D365B 06/03/02   JP  ADD COMBINATION APPLIED INDICATOR (CAPI)   *GAS3UPD 
00149 *                                                                *GAS3UPD 
00150 *  D368  06/04/02  JP ADD SELECTIVE ADDITIONAL BENEFIT           *GAS3UPD 
00151 *                         DETERMINATION (SABD)                   *GAS3UPD 
00152 *                                                                *GAS3UPD 
00153 *            08-14-02   GTF   RECOMPILE FOR OPID EXPANSION       *GAS3UPD 
00154 *                                                                *GAS3UPD 
00155 *            06-13-03   DAF   CORRECTED PROGRAM SO THAT THE      *GAS3UPD 
00156 *                             TABULAR ID IS ON THE 'C6' RECORD   *GAS3UPD 
00157 *                             INSTEAD OF THE BENEFIT PROVISION ID*GAS3UPD 
00158 *                             AND THE TABULAR SLOT NUMBER IS ON  *GAS3UPD 
00159 *                             THE 'C6' RECORD INSTEAD OF THE     *GAS3UPD 
00160 *                             BENEFIT PROVISION SLOT NUMBER      *GAS3UPD 
00161 *                             USE COPYBOOK GCTIPGPC INSTEAD OF   *GAS3UPD 
00162 *                             GCTIPGTC                           *GAS3UPD 
00163 *                                                                *GAS3UPD 
00153 * P09400     11-07-06   GF    ADD ASCEND/DESCEND AND BISCENDING  *GAS3UPD 
00154 *                             INDICATORS.                        *GAS3UPD 
00163 *                                                                *GAS3UPD 
00153 *            10-15-10   MJL   ALLOW 'UNL' VALUE.                 *GAS3UPD 
00156 *            04-14-25   CJB   FIXED ISSUE WITH OCCURS ENTRIES    *GAS1UPD 
00164 ******************************************************************GAS3UPD 
00165                                                                   GAS3UPD 
00166 ******************************************************************GAS3UPD 
00167 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                     *GAS3UPD 
00168 * - DEAD CODE ELIMINATION.                                       *GAS3UPD 
      *                                                                *        
      * P21595  09/19/16  HSB CHANGES FOR GCPS NEW FIELDS BENEFIT      *        
      *                       TYPE CODE,TIER CODE,TIER LEVEL.          *        
      *                                                                *        
SI0724* P56703  05/08/24  SI  RECOMPILE - PEAQ COPYBOOK EXPANSION      *        
SI0724*                       COPY ABM, ACP, ACL, ADL, AOL &           *        
SI0724*                       GCCDRLEN                                 *        
00169 ******************************************************************GAS3UPD 
00170                                                                   GAS3UPD 
00171      SKIP3                                                        GAS3UPD 
00172  ENVIRONMENT DIVISION.                                            GAS3UPD 
00173 /    D A T A   D I V I S I O N                                    GAS3UPD 
00174  DATA DIVISION.                                                   GAS3UPD 
00175  WORKING-STORAGE SECTION.                                         GAS3UPD 
00176  01  WS-BEGIN                    PIC X(24)  VALUE                 GAS3UPD 
00177      '***GAS3UPD WS BEGINS***'.                                   GAS3UPD 
00178                                                                   GAS3UPD 
00179 *    T I T L E   L I N E S                                        GAS3UPD 
00180  01  WS-TITLE-LINES.                                              GAS3UPD 
00181  COPY GCMHLINE.                                                   GAS3UPD 
00182 *****05  GROUP-SPECIFIC-TITLE-LINE       PIC X(42)                GAS3UPD 
00183 *      VALUE ' GROUP SPEC. ALL-LEVEL TABULAR MAINTENANCE'.        GAS3UPD 
00184 *    05  GROUP-SPECIFIC-ID-LINE.                                  GAS3UPD 
00185 *      10  FILLER                        PIC X(20)                GAS3UPD 
00186 *        VALUE 'GROUP SPECIFIC ID= '.                             GAS3UPD 
00187 *      10  FILLER                        PIC X(5) VALUE 'GRP= '.  GAS3UPD 
00188 *      10  GRP-SPEC-GROUP-NO             PIC X(6).                GAS3UPD 
00189 *      10  FILLER                        PIC X(6) VALUE ' SEC= '. GAS3UPD 
00190 *      10  GRP-SPEC-SECTION-NO           PIC X(4).                GAS3UPD 
00191 *      10  FILLER                        PIC X(5) VALUE ' FR= '.  GAS3UPD 
00192 *      10  GRP-SPEC-FAM-REL-LVL          PIC XX.                  GAS3UPD 
00193 *      10  FILLER                        PIC X(7) VALUE ' EFDT= '.GAS3UPD 
00194 *      10  GRP-SPEC-EFF-DATE             PIC X(6).                GAS3UPD 
00195 *    05  CONTRACT-TITLE-LINE             PIC X(42)                GAS3UPD 
00196 *      VALUE '   CONTRACT ALL-LEVEL TABULAR MAINTENANCE'.         GAS3UPD 
00197 *    05  CONTRACT-ID-LINE.                                        GAS3UPD 
00198 *      10  FILLER                      PIC X(14)                  GAS3UPD 
00199 *        VALUE 'CONTRACT ID= '.                                   GAS3UPD 
00200 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS3UPD 
00201 *      10  CONTRACT-GROUP-NO           PIC X(6).                  GAS3UPD 
00202 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS3UPD 
00203 *      10  CONTRACT-SECTION-NO         PIC X(4).                  GAS3UPD 
00204 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS3UPD 
00205 *      10  CONTRACT-LOB                PIC X.                     GAS3UPD 
00206 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS3UPD 
00207 *      10  CONTRACT-PROV-CTL           PIC XX.                    GAS3UPD 
00208 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS3UPD 
00209 *      10  CONTRACT-FAM-REL-LVL        PIC XX.                    GAS3UPD 
00210 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS3UPD 
00211 *      10  CONTRACT-EFF-DATE               PIC X(6).              GAS3UPD 
00212 *    05  BENEFIT-PROVISION-TITLE-LINE    PIC X(42)                GAS3UPD 
00213 *      VALUE '   BEN. PROV ALL-LEVEL TABULAR MAINTENANCE'.        GAS3UPD 
00214 *    05  BENEFIT-PROVISION-ID-LINE.                               GAS3UPD 
00215 *      10  FILLER                      PIC X(5) VALUE 'GRP= '.    GAS3UPD 
00216 *      10  BEN-PROV-GROUP-NO           PIC X(6).                  GAS3UPD 
00217 *      10  FILLER                      PIC X(6) VALUE ' SEC= '.   GAS3UPD 
00218 *      10  BEN-PROV-SECTION-NO         PIC X(4).                  GAS3UPD 
00219 *      10  FILLER                      PIC X(6) VALUE ' LOB= '.   GAS3UPD 
00220 *      10  BEN-PROV-LOB                PIC X.                     GAS3UPD 
00221 *      10  FILLER                      PIC X(6) VALUE ' PRV= '.   GAS3UPD 
00222 *      10  BEN-PROV-PROV-CTL           PIC XX.                    GAS3UPD 
00223 *      10  FILLER                      PIC X(5) VALUE ' FR= '.    GAS3UPD 
00224 *      10  BEN-PROV-FAM-REL-LVL        PIC XX.                    GAS3UPD 
00225 *      10  FILLER                      PIC X(7) VALUE ' EFDT= '.  GAS3UPD 
00226 *      10  BEN-PROV-EFF-DATE           PIC X(6).                  GAS3UPD 
00227 *      10  FILLER                      PIC X(8) VALUE ' BPVID= '. GAS3UPD 
00228 *      10  BEN-PROV-ID-NO              PIC X(6).                  GAS3UPD 
00229 *    05  ADL-TITLE-LINE                PIC X(26)                  GAS3UPD 
00230 *********VALUE '   DEDUCTIBLE LIMITS      '.                      GAS3UPD 
00231 /    A L T E R N A T I V E   W O R K F I L E   K E Y S            GAS3UPD 
00232  01  FILLER                      PIC X(32)  VALUE                 GAS3UPD 
00233      '*** ALTERNATIVE WORKFILE KEY ***'.                          GAS3UPD 
00234  01  SAVE-WS-ALT-WORKFILE-KEYS.                                   GAS3UPD 
00235      05 FILLER                   PIC X(63) VALUE SPACES.          GAS3UPD 
00236                                                                   GAS3UPD 
00237  01  SAVE-RESTORE-KEY.                                            GAS3UPD 
00238      05 WS-SV-RESTO-KY           PIC X(63) VALUE SPACES.          GAS3UPD 
00239                                                                   GAS3UPD 
00240  01  WS-ALT-WORKFILE-KEYS.                                        GAS3UPD 
00241  COPY GCWRKKEY.                                                   GAS3UPD 
00242                                                                   GAS3UPD 
00243                                                                   GAS3UPD 
00244 *   D A T E   F O R M A T T I N G   C O M M A R E A               GAS3UPD 
00245                                                                   GAS3UPD 
00246  01  HGADATES-COMMAREA.                                           GAS3UPD 
00247  COPY HGCDAT01.                                                   GAS3UPD 
00248                                                                   GAS3UPD 
00249 *    W O R K F I E L D S ,   A N D   S W I T C H E S              GAS3UPD 
00250  01  WS-WORK-FIELDS.                                              GAS3UPD 
00251                                                                   GAS3UPD 
00252      05  WS-SPACES-ZEROS.                                         GAS3UPD 
00253        10  WS-SPACES-ZEROS-SPACES       PIC X(6)  VALUE SPACES.   GAS3UPD 
00254        10  WS-SPACES-ZEROS-ZEROS        PIC S9(7) COMP-3          GAS3UPD 
00255                                                   VALUE ZEROS.    GAS3UPD 
00256                                                                   GAS3UPD 
00257      05  GCTRSRT-COMMAREA-LEN      PIC S9(4)  COMP VALUE +100.    GAS3UPD 
00258                                                                   GAS3UPD 
00259      05  WS-TAB-PROV-COPY-SLOT          PIC S9(7) COMP-3.         GAS3UPD 
00260      05  WS-INTL-TAB-ID.                                          GAS3UPD 
00261        10  WS-INTL-TAB-TAB-ID           PIC X(6).                 GAS3UPD 
00262        10  WS-INTL-TAB-TAB-SLOT         PIC S9(7) COMP-3.         GAS3UPD 
00263      05  WS-SAVE-INTL-TAB.                                        GAS3UPD 
00264        10  WS-SAVE-INTL-TAB-ID          PIC X(6).                 GAS3UPD 
00265        10  WS-SAVE-INTL-TAB-SLOT        PIC S9(7) COMP-3.         GAS3UPD 
00266                                                                   GAS3UPD 
00267      05  WS-HEX-00                     PIC X    VALUE LOW-VALUE.  GAS3UPD 
00268      05  WS-QUOTIENT                   PIC 999  COMP-3.           GAS3UPD 
00269      05  WS-REMAINDER                  PIC 999  COMP-3.           GAS3UPD 
00270                                                                   GAS3UPD 
00271      05  WS-CDE-REQUEST-CODES.                                    GAS3UPD 
00272          10  WS-REQUEST-4500-CDE-PROTECT    PIC X(4) VALUE '4500'.GAS3UPD 
00273          10  WS-REQUEST-4600-CDE-STATUS     PIC X(4) VALUE '4600'.GAS3UPD 
00274          10  WS-REQUEST-4700-CNTL-UPDATE    PIC X(4) VALUE '4700'.GAS3UPD 
00275          10  WS-REQUEST-4900-CNTL-UPDATE    PIC X(4) VALUE '4900'.GAS3UPD 
00276                                                                   GAS3UPD 
00277      05  WS-INTRNL-TABS-TO-CHG-CNT          PIC S9   COMP-3.      GAS3UPD 
00278      05  WS-INT-TAB-CHANGE-INDICATOR        PIC XX   VALUE SPACE. GAS3UPD 
00279        88  WS-INT-DESCRP-CHG-TO-NON-PROD        VALUE 'PN'.       GAS3UPD 
00280        88  WS-INT-DESCRP-CHG-BACK-TO-PROD       VALUE 'NP'.       GAS3UPD 
00281        88  WS-INT-DESCRP-NOCHG-AT-PROD          VALUE '  '.       GAS3UPD 
00282        88  WS-INT-DESCRP-NOCHG-AT-NONPROD       VALUE 'NN'.       GAS3UPD 
00283                                                                   GAS3UPD 
00284 *    I N T E R N A L   T A B U L A R   P R O G R A M   N A M E    GAS3UPD 
00285  01  WS-INTERNAL-TABULAR-PGM-ID        PIC X(8).                  GAS3UPD 
00286                                                                   GAS3UPD 
00287                                                                   GAS3UPD 
00288 ** ***ALL LEVEL TABULAR ENTRY SAVED HERE DURING SORT ***          GAS3UPD 
00289  01  WS-ENTRY                          PIC X(176).                GAS3UPD 
00290      SKIP3                                                        GAS3UPD 
00291 /   A T T R I B U T E S                                           GAS3UPD 
00292  COPY DFHBMSCA.                                                   GAS3UPD 
00293      02  DFHBMABF                PIC X VALUE '9'.                 GAS3UPD 
00294 /   A T T E N T I O N   I D E N T I F I E R S                     GAS3UPD 
00295  COPY DFHAID.                                                     GAS3UPD 
00296 /   R E C O R D   L E N G T H S                                   GAS3UPD 
00297                                                                   GAS3UPD 
00298  01  WS-RECORD-LENGTHS.                                           GAS3UPD 
00299 *   05 WS-COMM-KEY-PNTR-LEN           PIC S9(4) COMP  VALUE +4.   GAS3UPD 
00300     05 WS-COMMON-WORKAREA-LEN         PIC S9(4) COMP  VALUE +550. GAS3UPD 
00301     05 WS-COMMUNICATION-KEY-LEN       PIC S9(4) COMP  VALUE +100. GAS3UPD 
SI0724*   05 WS-COPY-TABLE-LEN              PIC S9(4) COMP  VALUE +7744.GAS3UPD 
SI0724    05 WS-COPY-TABLE-LEN              PIC S9(4) COMP VALUE +30800.GAS3UPD 
00303     05 WS-IO-PARM-WRK-ALL-LVL-LEN     PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00304     05 WS-IO-PARM-WRK-BEN-PROV-LEN    PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00305     05 WS-IO-PARM-WRK-CONTRACT-LEN    PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00306     05 WS-IO-PARM-WRK-CONTROL-LEN     PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00307     05 WS-IO-PARM-WRK-GRP-SPEC-LEN    PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00308     05 WS-IO-PARM-WRK-INTERNL-TAB-LEN PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00309     05 WS-WF-INTR-TAB-LEN             PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00310     05 WS-WRK-BEN-PROV-LEN            PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00311     05 WS-WRK-CONTRACT-LEN            PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00312     05 WS-WRK-GRP-SPEC-LEN            PIC S9(4) COMP  VALUE +0.   GAS3UPD 
00313     05 WS-NEW-OCCR-ON-WF              PIC X   VALUE SPACE.        GAS3UPD 
00314                                                                   GAS3UPD 
00315 ******************************************************************GAS3UPD 
00316 ** REQUIRED FOR N118 - DISPLAY OF TABULAR OCCURS (INDEX)        **GAS3UPD 
00317 ******************************************************************GAS3UPD 
00318  01  CURNT-OCURS-BIN             PIC 9(4)  COMP.                  GAS3UPD 
00319  01  CURNT-OCURS-PKD             PIC 9(4).                        GAS3UPD 
00320  01  CURNT-OCURS-ALH      REDEFINES   CURNT-OCURS-PKD.            GAS3UPD 
00321      05  FILLER                  PIC XX.                          GAS3UPD 
00322      05  CURNT-OCCURS-OUT        PIC XX.                          GAS3UPD 
00323                                                                   GAS3UPD 
00324  01  TOTAL-OCURS-UNK             PIC 9(5).                        GAS3UPD 
00325  01  TOTAL-OCURS-ALH      REDEFINES   TOTAL-OCURS-UNK.            GAS3UPD 
00326      05  FILLER                  PIC XXX.                         GAS3UPD 
00327      05  TOTAL-OCCURS-OUT        PIC XX.                          GAS3UPD 
00328 /    G . C .   R E C O R D S   L E N G T H S                      GAS3UPD 
00329  01  WS-GC-RECORD-LENGTHS.                                        GAS3UPD 
00330      COPY GCCDRLEN.                                               GAS3UPD 
00331                                                                   GAS3UPD 
00332 /    A B E N D   A R E A                                          GAS3UPD 
00333  01  WS-01-ABEND-AREA.                                            GAS3UPD 
00334      05  FILLER                   PIC X(16)  VALUE                GAS3UPD 
00335          '** ABEND AREA **'.                                      GAS3UPD 
00336                                                                   GAS3UPD 
00337      05  WS-ABCODE-CODES-AND-MSG.                                 GAS3UPD 
00338          10  WS-ABCODE                  PIC X(04)  VALUE  SPACES. GAS3UPD 
00339          10  WS-ABCODE-MSG              PIC X(79)  VALUE  SPACES. GAS3UPD 
00340          10  WS-ABCODE-1DF1             PIC X(04)  VALUE  '1DF1'. GAS3UPD 
00341          10  WS-ABCODE-1DF1-MSG         PIC X(79)  VALUE          GAS3UPD 
00342              '*** A SKELETON CAN NOT BE FOUND FOR AN INTERNAL TABUGAS3UPD 
00343 -            'LAR.  CONTACT SYSTEMS ***  '.                       GAS3UPD 
00344          10  WS-ABCODE-1DF2             PIC X(04)  VALUE  '1DF2'. GAS3UPD 
00345          10  WS-ABCODE-1DF2-MSG         PIC X(79)  VALUE          GAS3UPD 
00346              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS3UPD 
00347 -            ' CONTACT SYSTEMS ***       '.                       GAS3UPD 
00348          10  WS-ABCODE-1DF3             PIC X(04)  VALUE  '1DF3'. GAS3UPD 
00349          10  WS-ABCODE-1DF3-MSG         PIC X(79)  VALUE          GAS3UPD 
00350              '*** INVALID INTERNAL TABULAR SITUATION FOUND. PLEASEGAS3UPD 
00351 -            ' CONTACT SYSTEMS ***       '.                       GAS3UPD 
00352          10  WS-ABCODE-1DF4             PIC X(04)  VALUE  '1DF4'. GAS3UPD 
00353          10  WS-ABCODE-1DF4-MSG         PIC X(79)  VALUE          GAS3UPD 
00354              '*** ERROR REWRITING ALL LEVEL TABULAR.  PLEASE CONTAGAS3UPD 
00355 -            'CT SYSTEMS ***             '.                       GAS3UPD 
00356          10  WS-ABCODE-1DF5             PIC X(04)  VALUE  '1DF5'. GAS3UPD 
00357          10  WS-ABCODE-1DF5-MSG         PIC X(79)  VALUE          GAS3UPD 
00358              'THE INTERNAL TABULAR CAN NOT BE READ FROM THE WORKFIGAS3UPD 
00359 -            'LE.  PLEASE CONTACT SYSTEMS'.                       GAS3UPD 
00360          10  WS-ABCODE-1DF6             PIC X(04)  VALUE  '1DF6'. GAS3UPD 
00361          10  WS-ABCODE-1DF6-MSG         PIC X(79)  VALUE          GAS3UPD 
00362              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORKFILGAS3UPD 
00363 -            'E.  PLEASE CONTACT SYSTEMS '.                       GAS3UPD 
00364          10  WS-ABCODE-1DF7             PIC X(04)  VALUE  '1DF7'. GAS3UPD 
00365          10  WS-ABCODE-1DF7-MSG         PIC X(79)  VALUE          GAS3UPD 
00366              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS3UPD 
00367 -            ' SYSTEMS ***               '.                       GAS3UPD 
00368          10  WS-ABCODE-1DF9             PIC X(04)  VALUE  '1DF9'. GAS3UPD 
00369          10  WS-ABCODE-1DF9-MSG         PIC X(79)  VALUE          GAS3UPD 
00370              'THE INTERNAL TABULAR CAN NOT BE ADDED TO THE WORFILEGAS3UPD 
00371 -            '.  PLEASE CONTACT SYSTEMS  '.                       GAS3UPD 
00372          10  WS-ABCODE-1DFA             PIC X(04)  VALUE  '1DFA'. GAS3UPD 
00373          10  WS-ABCODE-1DFA-MSG         PIC X(79)  VALUE          GAS3UPD 
00374              '*** THE INTERNAL TABULAR CAN NOT BE DELETED, PLEASE GAS3UPD 
00375 -            'CONTACT SYSTEMS ***        '.                       GAS3UPD 
00376          10  WS-ABCODE-1DFB             PIC X(04)  VALUE  '1DFB'. GAS3UPD 
00377          10  WS-ABCODE-1DFB-MSG         PIC X(79)  VALUE          GAS3UPD 
00378              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS3UPD 
00379 -            ' SYSTEMS ***               '.                       GAS3UPD 
00380          10  WS-ABCODE-1DFC             PIC X(04)  VALUE  '1DFC'. GAS3UPD 
00381          10  WS-ABCODE-1DFC-MSG         PIC X(79)  VALUE          GAS3UPD 
00382              '*** ERROR WHEN DELETING INTERNAL TAB.  PLEASE CONTACGAS3UPD 
00383 -            'T SYSTEMS ***              '.                       GAS3UPD 
00384          10  WS-ABCODE-1DFJ             PIC X(04)  VALUE  '1DFJ'. GAS3UPD 
00385          10  WS-ABCODE-1DFJ-MSG         PIC X(79)  VALUE          GAS3UPD 
00386              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS3UPD 
00387 -            ' SYSTEMS ***               '.                       GAS3UPD 
00388          10  WS-ABCODE-1DFK             PIC X(04)  VALUE  '1DFK'. GAS3UPD 
00389          10  WS-ABCODE-1DFK-MSG         PIC X(79)  VALUE          GAS3UPD 
00390              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS3UPD 
00391 -            ' SYSTEMS ***               '.                       GAS3UPD 
00392          10  WS-ABCODE-1DFL             PIC X(04)  VALUE  '1DFL'. GAS3UPD 
00393          10  WS-ABCODE-1DFL-MSG         PIC X(79)  VALUE          GAS3UPD 
00394              '*** ERROR READING GROUP SPECIFIC RECORD TO RETURN TOGAS3UPD 
00395 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS3UPD 
00396          10  WS-ABCODE-1DFM             PIC X(04)  VALUE  '1DFM'. GAS3UPD 
00397          10  WS-ABCODE-1DFM-MSG         PIC X(79)  VALUE          GAS3UPD 
00398              '*** ERROR READING CONTRACT MASTER TO RETURN TO THE  GAS3UPD 
00399 -            'MENU.  CONTACT SYSTEMS *** '.                       GAS3UPD 
00400          10  WS-ABCODE-1DFN             PIC X(04)  VALUE  '1DFN'. GAS3UPD 
00401          10  WS-ABCODE-1DFN-MSG         PIC X(79)  VALUE          GAS3UPD 
00402              '*** ERROR READING BENEFIT PROV RECORD TO RETURN TO MGAS3UPD 
00403 -            'ENU.  CONTACT SYSTEMS ***  '.                       GAS3UPD 
00404          10  WS-ABCODE-1DFO             PIC X(04)  VALUE  '1DFO'. GAS3UPD 
00405          10  WS-ABCODE-1DFO-MSG         PIC X(79)  VALUE          GAS3UPD 
00406              '*** ERROR READING ALL LEVEL TABULAR.  PLEASE CONTACTGAS3UPD 
00407 -            ' SYSTEMS ***               '.                       GAS3UPD 
00408          10  WS-ABCODE-1DFP             PIC X(04)  VALUE  '1DFP'. GAS3UPD 
00409          10  WS-ABCODE-1DFP-MSG         PIC X(79)  VALUE          GAS3UPD 
00410              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS3UPD 
00411 -            ' SYSTEMS ***               '.                       GAS3UPD 
00412          10  WS-ABCODE-1DFQ             PIC X(04)  VALUE  '1DFQ'. GAS3UPD 
00413          10  WS-ABCODE-1DFQ-MSG         PIC X(79)  VALUE          GAS3UPD 
00414              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS3UPD 
00415 -            'CT SYSTEMS ***             '.                       GAS3UPD 
00416          10  WS-ABCODE-1DFR             PIC X(04)  VALUE  '1DFR'. GAS3UPD 
00417          10  WS-ABCODE-1DFR-MSG         PIC X(79)  VALUE          GAS3UPD 
00418              '*** ERROR READING W/F ALL LVL TAB.    PLEASE CONTACTGAS3UPD 
00419 -            ' SYSTEMS ***               '.                       GAS3UPD 
00420          10  WS-ABCODE-1DFS             PIC X(04)  VALUE  '1DFS'. GAS3UPD 
00421          10  WS-ABCODE-1DFS-MSG         PIC X(79)  VALUE          GAS3UPD 
00422              '*** ERROR REWRITING W/F ALL LVL TAB.  PLEASE CONTACTGAS3UPD 
00423 -            ' SYSTEMS ***               '.                       GAS3UPD 
00424          10  WS-ABCODE-1DFT             PIC X(04)  VALUE  '1DFT'. GAS3UPD 
00425          10  WS-ABCODE-1DFT-MSG         PIC X(79)  VALUE          GAS3UPD 
00426              '*** ERROR READING W/F CONTROL RECORD. PLEASE CONTACTGAS3UPD 
00427 -            ' SYSTEMS ***               '.                       GAS3UPD 
00428          10  WS-ABCODE-1DFU             PIC X(04)  VALUE  '1DFU'. GAS3UPD 
00429          10  WS-ABCODE-1DFU-MSG         PIC X(79)  VALUE          GAS3UPD 
00430              '*** ERROR REWRITING W/F CONTROL RECORD. PLEASE CONTAGAS3UPD 
00431 -            'CT SYSTEMS ***             '.                       GAS3UPD 
00432          10  WS-ABCODE-1DL1             PIC X(04)  VALUE  '1DL1'. GAS3UPD 
00433          10  WS-ABCODE-1DL1-MSG         PIC X(79)  VALUE          GAS3UPD 
00434              '*** THE OCCURS WE ARE TO UPDATE HAS BEEN DELETED ***GAS3UPD 
00435 -            '                           '.                       GAS3UPD 
00436          10  WS-ABCODE-1DL2             PIC X(04)  VALUE  '1DL2'. GAS3UPD 
00437          10  WS-ABCODE-1DL2-MSG         PIC X(79)  VALUE          GAS3UPD 
00438              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS3UPD 
00439 -            'MS ***                     '.                       GAS3UPD 
00440          10  WS-ABCODE-1DL3             PIC X(04)  VALUE  '1DL3'. GAS3UPD 
00441          10  WS-ABCODE-1DL3-MSG         PIC X(79)  VALUE          GAS3UPD 
00442              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS3UPD 
00443 -            'EMS ***                    '.                       GAS3UPD 
00444          10  WS-ABCODE-1DLX             PIC X(04)  VALUE  '1DLX'. GAS3UPD 
00445          10  WS-ABCODE-1DLX-MSG         PIC X(79)  VALUE          GAS3UPD 
00446              '*** PROGRAM LOGIC ERROR FOUND.  PLEASE CONTACT SYSTEGAS3UPD 
00447 -            'MS ***                     '.                       GAS3UPD 
00448          10  WS-ABCODE-1DP1             PIC X(04)  VALUE  '1DP1'. GAS3UPD 
00449          10  WS-ABCODE-1DP1-MSG         PIC X(79)  VALUE          GAS3UPD 
00450              '????????????????????????????????????????????????????GAS3UPD 
00451 -            '???????????????????????????'.                       GAS3UPD 
00452                                                                   GAS3UPD 
00453 /*****************************************************************GAS3UPD 
00454 *    WT-01   M E S S A G E   T A B L E                            GAS3UPD 
00455 ******************************************************************GAS3UPD 
00456  01  WT-01-TABLE.                                                 GAS3UPD 
00457      05  FILLER                  PIC X(16) VALUE                  GAS3UPD 
00458          '* WT-01-TABLE  *'.                                      GAS3UPD 
00459  01  FILLER.                                                      GAS3UPD 
00460      05  WT-01-MESSAGE-VALUES.                                    GAS3UPD 
00461                                                                   GAS3UPD 
00462 *----------------------------------------------------------------*GAS3UPD 
00463          10  WT-01-ENTRY-001.                                     GAS3UPD 
00464              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00465              15  WT-01-MESSAGE-TEXT-001.                          GAS3UPD 
00466                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00467                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00468                  20  FILLER          PIC X(3)  VALUE  '001'.      GAS3UPD 
00469                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00470                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00471                      '#IBGR HAS BEEN SUCCESSFULLY MAPPED          GAS3UPD 
00472 -                    '                         '.                 GAS3UPD 
00473              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00474 *----------------------------------------------------------------*GAS3UPD 
00475          10  WT-01-ENTRY-002.                                     GAS3UPD 
00476              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00477              15  WT-01-MESSAGE-TEXT-002.                          GAS3UPD 
00478                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00479                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00480                  20  FILLER          PIC X(3)  VALUE  '002'.      GAS3UPD 
00481                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00482                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00483                      '#IPGN HAS BEEN SUCCESSFULLY MAPPED          GAS3UPD 
00484 -                    '                         '.                 GAS3UPD 
00485              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00486 *----------------------------------------------------------------*GAS3UPD 
00487          10  WT-01-ENTRY-003.                                     GAS3UPD 
00488              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00489              15  WT-01-MESSAGE-TEXT-003.                          GAS3UPD 
00490                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00491                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00492                  20  FILLER          PIC X(3)  VALUE  '003'.      GAS3UPD 
00493                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00494                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00495                      '#IPGT HAS BEEN SUCCESSFULLY MAPPED          GAS3UPD 
00496 -                    '                         '.                 GAS3UPD 
00497              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00498 *----------------------------------------------------------------*GAS3UPD 
00499          10  WT-01-ENTRY-004.                                     GAS3UPD 
00500              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00501              15  WT-01-MESSAGE-TEXT-004.                          GAS3UPD 
00502                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00503                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00504                  20  FILLER          PIC X(3)  VALUE  '004'.      GAS3UPD 
00505                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00506                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00507                      '******************* F U T U R E   U S E ****GAS3UPD 
00508 -                    '*************************'.                 GAS3UPD 
00509              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00510 *----------------------------------------------------------------*GAS3UPD 
00511          10  WT-01-ENTRY-005.                                     GAS3UPD 
00512              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00513              15  WT-01-MESSAGE-TEXT-005.                          GAS3UPD 
00514                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00515                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00516                  20  FILLER          PIC X(3)  VALUE  '005'.      GAS3UPD 
00517                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00518                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00519                      '******************* F U T U R E   U S E ****GAS3UPD 
00520 -                    '*************************'.                 GAS3UPD 
00521              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00522 *----------------------------------------------------------------*GAS3UPD 
00523          10  WT-01-ENTRY-006.                                     GAS3UPD 
00524              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00525              15  WT-01-MESSAGE-TEXT-006.                          GAS3UPD 
00526                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00527                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00528                  20  FILLER          PIC X(3)  VALUE  '006'.      GAS3UPD 
00529                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00530                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00531                      'DELETE OPTION MUST BE \
00532 -                    'VALID                    '.                 GAS3UPD 
00533              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00534 *----------------------------------------------------------------*GAS3UPD 
00535          10  WT-01-ENTRY-007.                                     GAS3UPD 
00536              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00537              15  WT-01-MESSAGE-TEXT-007.                          GAS3UPD 
00538                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00539                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00540                  20  FILLER          PIC X(3)  VALUE  '007'.      GAS3UPD 
00541                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00542                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00543                      'EDIT TABLE EMPTY, DATA NOT VALIDATED - PRESSGAS3UPD 
00544 -                    ' PF4/PF16 TO CONTINUE    '.                 GAS3UPD 
00545              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00546 *----------------------------------------------------------------*GAS3UPD 
00547          10  WT-01-ENTRY-008.                                     GAS3UPD 
00548              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00549              15  WT-01-MESSAGE-TEXT-008.                          GAS3UPD 
00550                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00551                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00552                  20  FILLER          PIC X(3)  VALUE  '008'.      GAS3UPD 
00553                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00554                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00555                      'GROUP IN CONVERSION STATUS, CANNOT CHANGE HIGAS3UPD 
00556 -                    'GH-LIGHTED ELEMENTS      '.                 GAS3UPD 
00557              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00558 *----------------------------------------------------------------*GAS3UPD 
00559          10  WT-01-ENTRY-009.                                     GAS3UPD 
00560              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00561              15  WT-01-MESSAGE-TEXT-009.                          GAS3UPD 
00562                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00563                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00564                  20  FILLER          PIC X(3)  VALUE  '009'.      GAS3UPD 
00565                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00566                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00567                      'INVALID PFKEY SELECTION                     GAS3UPD 
00568 -                    '                         '.                 GAS3UPD 
00569              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00570 *----------------------------------------------------------------*GAS3UPD 
00571          10  WT-01-ENTRY-010.                                     GAS3UPD 
00572              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00573              15  WT-01-MESSAGE-TEXT-010.                          GAS3UPD 
00574                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00575                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00576                  20  FILLER          PIC X(3)  VALUE  '010'.      GAS3UPD 
00577                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00578                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00579                      'INVALID REQUEST.  THAT PF KEY HAS NO MEANINGGAS3UPD 
00580 -                    ' TO THIS PROGRAM         '.                 GAS3UPD 
00581              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00582 *----------------------------------------------------------------*GAS3UPD 
00583          10  WT-01-ENTRY-011.                                     GAS3UPD 
00584              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00585              15  WT-01-MESSAGE-TEXT-011.                          GAS3UPD 
00586                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00587                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00588                  20  FILLER          PIC X(3)  VALUE  '011'.      GAS3UPD 
00589                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00590                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00591                      'NO CHANGE FOUND - NO CHANGE MADE, WHAT NEXT GAS3UPD 
00592 -                    '                         '.                 GAS3UPD 
00593              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00594 *----------------------------------------------------------------*GAS3UPD 
00595          10  WT-01-ENTRY-012.                                     GAS3UPD 
00596              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00597              15  WT-01-MESSAGE-TEXT-012.                          GAS3UPD 
00598                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00599                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00600                  20  FILLER          PIC X(3)  VALUE  '012'.      GAS3UPD 
00601                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00602                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00603                      'NO ENTRIES TO DISPLAY                       GAS3UPD 
00604 -                    '                         '.                 GAS3UPD 
00605              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00606 *----------------------------------------------------------------*GAS3UPD 
00607          10  WT-01-ENTRY-013.                                     GAS3UPD 
00608              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00609              15  WT-01-MESSAGE-TEXT-013.                          GAS3UPD 
00610                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00611                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00612                  20  FILLER          PIC X(3)  VALUE  '013'.      GAS3UPD 
00613                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00614                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00615                      'PFKEY INVALID WHILE ERRORS NOT CORRECTED, HIGAS3UPD 
00616 -                    'T ENTER FOR ERR MSG      '.                 GAS3UPD 
00617              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00618 *----------------------------------------------------------------*GAS3UPD 
00619          10  WT-01-ENTRY-014.                                     GAS3UPD 
00620              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00621              15  WT-01-MESSAGE-TEXT-014.                          GAS3UPD 
00622                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00623                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00624                  20  FILLER          PIC X(3)  VALUE  '014'.      GAS3UPD 
00625                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00626                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00627                      'PROCESSING FROM THE TOP OF THE LIST         GAS3UPD 
00628 -                    '                         '.                 GAS3UPD 
00629              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00630 *----------------------------------------------------------------*GAS3UPD 
00631          10  WT-01-ENTRY-015.                                     GAS3UPD 
00632              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00633              15  WT-01-MESSAGE-TEXT-015.                          GAS3UPD 
00634                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00635                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00636                  20  FILLER          PIC X(3)  VALUE  '015'.      GAS3UPD 
00637                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00638                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00639                      'THE MAXIMUM NUMBER OF ENTRIES HAVE BEEN ADDEGAS3UPD 
00640 -                    'D                        '.                 GAS3UPD 
00641              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00642 *----------------------------------------------------------------*GAS3UPD 
00643          10  WT-01-ENTRY-016.                                     GAS3UPD 
00644              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00645              15  WT-01-MESSAGE-TEXT-016.                          GAS3UPD 
00646                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00647                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00648                  20  FILLER          PIC X(3)  VALUE  '016'.      GAS3UPD 
00649                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00650                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00651                      'THE TABULAR ALREADY CONTAINS THE MAXIMUM NUMGAS3UPD 
00652 -                    'BER OF OCCURANCES        '.                 GAS3UPD 
00653              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00654 *----------------------------------------------------------------*GAS3UPD 
00655          10  WT-01-ENTRY-017.                                     GAS3UPD 
00656              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00657              15  WT-01-MESSAGE-TEXT-017.                          GAS3UPD 
00658                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00659                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00660                  20  FILLER          PIC X(3)  VALUE  '017'.      GAS3UPD 
00661                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00662                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00663                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS3UPD 
00664 -                    'T BE CHANGED             '.                 GAS3UPD 
00665              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00666 *----------------------------------------------------------------*GAS3UPD 
00667          10  WT-01-ENTRY-018.                                     GAS3UPD 
00668              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00669              15  WT-01-MESSAGE-TEXT-018.                          GAS3UPD 
00670                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00671                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00672                  20  FILLER          PIC X(3)  VALUE  '018'.      GAS3UPD 
00673                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00674                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00675                      'THE TABULAR RECORD DOES NOT EXIST, AND CANNOGAS3UPD 
00676 -                    'T BE MAPPED              '.                 GAS3UPD 
00677              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00678 *----------------------------------------------------------------*GAS3UPD 
00679          10  WT-01-ENTRY-019.                                     GAS3UPD 
00680              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00681              15  WT-01-MESSAGE-TEXT-019.                          GAS3UPD 
00682                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00683                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00684                  20  FILLER          PIC X(3)  VALUE  '019'.      GAS3UPD 
00685                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00686                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00687                      'THERE ARE NO MORE ENTRIES TO DISPLAY        GAS3UPD 
00688 -                    '                         '.                 GAS3UPD 
00689              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00690 *----------------------------------------------------------------*GAS3UPD 
00691          10  WT-01-ENTRY-020.                                     GAS3UPD 
00692              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00693              15  WT-01-MESSAGE-TEXT-020.                          GAS3UPD 
00694                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00695                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00696                  20  FILLER          PIC X(3)  VALUE  '020'.      GAS3UPD 
00697                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00698                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00699                      'THIS IS THE FIRST ON THE TABLE              GAS3UPD 
00700 -                    '                         '.                 GAS3UPD 
00701              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00702 *----------------------------------------------------------------*GAS3UPD 
00703          10  WT-01-ENTRY-021.                                     GAS3UPD 
00704              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00705              15  WT-01-MESSAGE-TEXT-021.                          GAS3UPD 
00706                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00707                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00708                  20  FILLER          PIC X(3)  VALUE  '021'.      GAS3UPD 
00709                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00710                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00711                      'THIS IS THE LAST ON THE TABLE               GAS3UPD 
00712 -                    '                         '.                 GAS3UPD 
00713              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00714 *----------------------------------------------------------------*GAS3UPD 
00715          10  WT-01-ENTRY-022.                                     GAS3UPD 
00716              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00717              15  WT-01-MESSAGE-TEXT-022.                          GAS3UPD 
00718                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00719                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00720                  20  FILLER          PIC X(3)  VALUE  '022'.      GAS3UPD 
00721                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00722                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00723                      'THIS PFKEY NOT VALID WHILE IN CHG/ADD MODE  GAS3UPD 
00724 -                    '                         '.                 GAS3UPD 
00725              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00726 *----------------------------------------------------------------*GAS3UPD 
00727          10  WT-01-ENTRY-023.                                     GAS3UPD 
00728              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00729              15  WT-01-MESSAGE-TEXT-023.                          GAS3UPD 
00730                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00731                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00732                  20  FILLER          PIC X(3)  VALUE  '023'.      GAS3UPD 
00733                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00734                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00735                      '#IDGD HAS BEEN SUCCESSFULLY MAPPED          GAS3UPD 
00736 -                    '                         '.                 GAS3UPD 
00737              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00738 *----------------------------------------------------------------*GAS3UPD 
00739          10  WT-01-ENTRY-024.                                     GAS3UPD 
00740              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00741              15  WT-01-MESSAGE-TEXT-024.                          GAS3UPD 
00742                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00743                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00744                  20  FILLER          PIC X(3)  VALUE  '024'.      GAS3UPD 
00745                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00746                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00747                      '#IPGP HAS BEEN SUCCESSFULLY MAPPED          GAS3UPD 
00748 -                    '                         '.                 GAS3UPD 
00749              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00750 *----------------------------------------------------------------*GAS3UPD 
00751          10  WT-01-ENTRY-025.                                     GAS3UPD 
00752              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00753              15  WT-01-MESSAGE-TEXT-003.                          GAS3UPD 
00754                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00755                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00756                  20  FILLER          PIC X(3)  VALUE  '025'.      GAS3UPD 
00757                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00758                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00759                      '#IPGS HAS BEEN SUCCESSFULLY MAPPED          GAS3UPD 
00760 -                    '                         '.                 GAS3UPD 
00761              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00762 *----------------------------------------------------------------*GAS3UPD 
00763          10  WT-01-ENTRY-026.                                     GAS3UPD 
00764              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00765              15  WT-01-MESSAGE-TEXT-025.                          GAS3UPD 
00766                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00767                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00768                  20  FILLER          PIC X(3)  VALUE  '026'.      GAS3UPD 
00769                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00770                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00771                      'MAXIMUM OF 5 INTERNAL TABULARS HAS ALREADY BGAS3UPD 
00772 -                    'EEN REACHED              '.                 GAS3UPD 
00773              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00774 *----------------------------------------------------------------*GAS3UPD 
00775          10  WT-01-ENTRY-027.                                     GAS3UPD 
00776              15  FILLER              PIC X(2)  VALUE '¬>'.        GAS3UPD 
00777              15  WT-01-MESSAGE-TEXT-025.                          GAS3UPD 
00778                  20  FILLER          PIC X(4)  VALUE  'GAS3'.     GAS3UPD 
00779                  20  FILLER          PIC X(1)  VALUE  '-'.        GAS3UPD 
00780                  20  FILLER          PIC X(3)  VALUE  '027'.      GAS3UPD 
00781                  20  FILLER          PIC X(1)  VALUE  ' '.        GAS3UPD 
00782                  20  FILLER          PIC X(70) VALUE              GAS3UPD 
00783                      '********** F U T U R E   U S E *************GAS3UPD 
00784 -                    '*************************'.                 GAS3UPD 
00785              15  FILLER              PIC X(2)  VALUE '<¬'.        GAS3UPD 
00786 *----------------------------------------------------------------*GAS3UPD 
00787                                                                   GAS3UPD 
00788      05  WT-01-MESSAGE-TABLE         REDEFINES                    GAS3UPD 
00789          WT-01-MESSAGE-VALUES         OCCURS 027 TIMES            GAS3UPD 
00790                                      INDEXED BY WT-01-INDEX.      GAS3UPD 
00791          10  WT-01-ENTRY.                                         GAS3UPD 
00792              15  FILLER              PIC X(02).                   GAS3UPD 
00793              15  WT-01-MESSAGE-TEXT  PIC X(79).                   GAS3UPD 
00794              15  FILLER              PIC X(02).                   GAS3UPD 
00795                                                                   GAS3UPD 
00796  01  WS-END                      PIC X(16)  VALUE                 GAS3UPD 
00797      '*** W/S ENDS ***'.                                          GAS3UPD 
00798 /    L I N K A G E   S E C T I O N                                GAS3UPD 
00799  LINKAGE SECTION.                                                 GAS3UPD 
00800  01  DFHCOMMAREA.                                                 GAS3UPD 
00801  COPY  G2ALCKEC.                                                  GAS3UPD 
00802  COPY  GACDACWA.                                                  GAS3UPD 
00803      05  GAS3UPD-PASSED-AREA.                                     GAS3UPD 
00804          07  LVL2-B-SW                PIC X.                      GAS3UPD 
00805          07  LVL2-F-SW                PIC X.                      GAS3UPD 
00806          07  LVL2-G-SW                PIC X.                      GAS3UPD 
00807          07  INTR-TAB-PGM-ID          PIC X(8).                   GAS3UPD 
00808          07  FILLER                   PIC X(09).                  GAS3UPD 
00809      05  DELADD-OPTION                PIC X(7).                   GAS3UPD 
00810                                                                   GAS3UPD 
00811 /*****************************************************************GAS3UPD 
00812 * W O R K F I L E   -   A L L   L E V E L   T A B U L A R   R E C GAS3UPD 
00813 ******************************************************************GAS3UPD 
00814  01  WF-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS3UPD 
00815  COPY GCIOPRM1.                                                   GAS3UPD 
00816 /                                                                 GAS3UPD 
00817  COPY GCWRKDCC.                                                   GAS3UPD 
00818 /                                                                 GAS3UPD 
00819  COPY GCTADLC.                                                    GAS3UPD 
00820 /*****************************************************************GAS3UPD 
00821 *    C O M M U N I C A T I O N   K E Y   A R E A                  GAS3UPD 
00822 ******************************************************************GAS3UPD 
00823 *01  COMMUNICATION-KEY-AREA.                                      GAS3UPD 
00824 *COPY G2ALCKEC.                                                   GAS3UPD 
00825                                                                   GAS3UPD 
00826 /*****************************************************************GAS3UPD 
00827 *    C O P Y   T A B U L A R   T A B L E   A R E A                GAS3UPD 
00828 ******************************************************************GAS3UPD 
00829  01  COPY-TABULAR-TABLE-AREA.                                     GAS3UPD 
SI0724*    05  COPY-TABULAR-TABLE  OCCURS  44 TIMES INDEXED BY          GAS3UPD 
SI0724     05  COPY-TABULAR-TABLE  OCCURS 175 TIMES INDEXED BY          GAS3UPD 
00831          COPY-IDX, COPY-IDX2, COPY-IDX3, COPY-IDX4.               GAS3UPD 
00832        10  COPY-SORTABLE-FLDS              PIC X(169).            GAS3UPD 
00833        10  COPY-SORT-FYI                   PIC X(003).            GAS3UPD 
00834        10  COPY-SORT-ENTRY-CNTR            PIC S9(7) COMP-3.      GAS3UPD 
00835                                                                   GAS3UPD 
00836 /*****************************************************************GAS3UPD 
00837 * W O R K F I L E   -   I N T E R N A L   T A B U L A R   R E C   GAS3UPD 
00838 ******************************************************************GAS3UPD 
00839  01  WF-IO-PARM-INTERNAL-TAB-RECORD.                              GAS3UPD 
00840  COPY GCIOPRM2.                                                   GAS3UPD 
00841 /                                                                 GAS3UPD 
00842  COPY GCWRKDC2.                                                   GAS3UPD 
00843 /                                                                 GAS3UPD 
00844  COPY GCTIPGPC.                                                   GAS3UPD 
00845 /*****************************************************************GAS3UPD 
00846 * P R O D U C T I O N   -   A L L   L E V E L   T A B   R E C     GAS3UPD 
00847 ******************************************************************GAS3UPD 
00848  01  PR-IO-PARM-ALL-LVL-TAB-RECORD.                               GAS3UPD 
00849  COPY GCIOPRMA   SUPPRESS.                                        GAS3UPD 
00850                                                                   GAS3UPD 
00851  COPY GCWRKDCA   SUPPRESS.                                        GAS3UPD 
00852                                                                   GAS3UPD 
00853  COPY GCTADL2    SUPPRESS.                                        GAS3UPD 
00854 /*****************************************************************GAS3UPD 
00855 *    M A P S E T   A R E A                                        GAS3UPD 
00856 ******************************************************************GAS3UPD 
00857      COPY GA1XSETC.                                               GAS3UPD 
00858 /    P R O C E D U R E   D I V I S I O N                          GAS3UPD 
00859  PROCEDURE DIVISION.                                              GAS3UPD 
00860                                                                   GAS3UPD 
00861 ******************************************************************GAS3UPD 
00862 * 0000  HOUSEKEEPING                                             *GAS3UPD 
00863 ******************************************************************GAS3UPD 
00864  0000-000-HOUSEKEEPING          SECTION.                          GAS3UPD 
00865  0000-010.                                                        GAS3UPD 
00866                                                                   GAS3UPD 
00867      SET ADDRESS OF  WF-IO-PARM-INTERNAL-TAB-RECORD TO            GAS3UPD 
00868                      ACWA-WF-INTERNAL-TAB-PNTR.                   GAS3UPD 
00869                                                                   GAS3UPD 
00870      SET ADDRESS OF  WF-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS3UPD 
00871                      ACWA-WF-ALL-LEVEL-TAB-PNTR.                  GAS3UPD 
00872                                                                   GAS3UPD 
00873      SET ADDRESS OF  PR-IO-PARM-ALL-LVL-TAB-RECORD  TO            GAS3UPD 
00874                      ACWA-PR-ALL-LEVEL-TAB-PNTR.                  GAS3UPD 
00875                                                                   GAS3UPD 
00876      SET ADDRESS OF  GA1XI01I  TO  ACWA-MAPSET-PNTR.              GAS3UPD 
00877                                                                   GAS3UPD 
00878      MOVE ZEROES  TO  ACWA-CDE-1U-COUNT,  ACWA-CDE-2B-COUNT.      GAS3UPD 
00879                                                                   GAS3UPD 
00880 ***  MOVE GCA-FROM-MENU-ID  TO FRMNUIDO.                          GAS3UPD 
00881                                                                   GAS3UPD 
00882      IF FRMNUIDI  =  'GS3A'                                       GAS3UPD 
00883         MOVE IDLINEI  TO  GROUP-SPECIFIC-ID-LINE.                 GAS3UPD 
00884      IF FRMNUIDI  =  'GC4A' OR 'GTM1'                             GAS3UPD 
00885         MOVE IDLINEI  TO  CONTRACT-ID-LINE.                       GAS3UPD 
00886      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
00887         MOVE IDLINEI  TO  BENEFIT-PROVISION-ID-LINE.              GAS3UPD 
00888                                                                   GAS3UPD 
00889      MOVE ADL-TITLE-LINE   TO  TITLEO.                            GAS3UPD 
00890                                                                   GAS3UPD 
00891      PERFORM 1000-000-MAIN-PROCESS.                               GAS3UPD 
00892                                                                   GAS3UPD 
00893      EXEC CICS  RETURN    END-EXEC.                               GAS3UPD 
00894      GOBACK.                                                      GAS3UPD 
00895                                                                   GAS3UPD 
00896  0000-900-EXIT.                                                   GAS3UPD 
00897         EXIT.                                                     GAS3UPD 
00898 /*****************************************************************GAS3UPD 
00899 * 1000  MAIN PROCESS                                             *GAS3UPD 
00900 ******************************************************************GAS3UPD 
00901  1000-000-MAIN-PROCESS          SECTION.                          GAS3UPD 
00902  1000-010.                                                        GAS3UPD 
00903                                                                   GAS3UPD 
00904      EXEC CICS  HANDLE CONDITION                                  GAS3UPD 
00905                 MAPFAIL(6400-000-XCTL-TO-MAIN-MENU)   END-EXEC.   GAS3UPD 
00906                                                                   GAS3UPD 
00907      MOVE  INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.      GAS3UPD 
00908                                                                   GAS3UPD 
00909      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS3UPD 
00910         DELADDI   =     'CHG/ADD'                     AND         GAS3UPD 
00911         OENTCTRI  NOT = '0000000'                                 GAS3UPD 
00912         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS3UPD 
00913                                                                   GAS3UPD 
00914      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS3UPD 
00915         DELADDI   =    'CHG/DEL'                      AND         GAS3UPD 
00916         DELOPTNI  =    'D'                                        GAS3UPD 
00917         PERFORM  2400-000-DELETE-THIS-OCCURANCE.                  GAS3UPD 
00918                                                                   GAS3UPD 
00919      IF (EIBAID   =     DFHENTER OR DFHPF4 OR DFHPF16) AND        GAS3UPD 
00920         DELADDI   =     'CHG/DEL'                      AND        GAS3UPD 
00921         DELOPTNI  NOT = 'D'                                       GAS3UPD 
00922         MOVE SPACES  TO  ERRMSGO                                  GAS3UPD 
00923         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS3UPD 
00924                                                                   GAS3UPD 
00925      IF (EIBAID   =    DFHPF4 OR DFHPF7 OR DFHPF8 OR              GAS3UPD 
00926                        DFHPF19 OR DFHPF20 OR DFHPF16) AND         GAS3UPD 
00927         DELADDI   =    'CHG/DEL'                      AND         GAS3UPD 
00928         DELOPTNI  NOT = 'D'                                       GAS3UPD 
00929         MOVE SPACES  TO  ERRMSGO                                  GAS3UPD 
00930         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS3UPD 
00931                                                                   GAS3UPD 
00932      IF (EIBAID   =    DFHENTER OR DFHPF4 OR DFHPF16) AND         GAS3UPD 
00933         DELADDI   =    'CHG/DEL'                      AND         GAS3UPD 
00934         DELOPTNI  NOT = 'D'                                       GAS3UPD 
00935         PERFORM  2200-000-UPDATE-THIS-OCCURANCE.                  GAS3UPD 
00936                                                                   GAS3UPD 
00937  1000-900-EXIT.   EXIT.                                           GAS3UPD 
00938                                                                   GAS3UPD 
00939 /*****************************************************************GAS3UPD 
00940 * 2200  UPDATE THIS OCCURANCE                                    *GAS3UPD 
00941 *                                                                *GAS3UPD 
00942 *    THIS ROUTINE WILL CHANGE ANY FIELD THAT THE OPERATOR HAS    *GAS3UPD 
00943 *  CHANGED, AND HAS CODE FOR THE MAINTENANCE OF THE INTERNAL     *GAS3UPD 
00944 *  TABULAR ENTRIES.                                              *GAS3UPD 
00945 ******************************************************************GAS3UPD 
00946  2200-000-UPDATE-THIS-OCCURANCE SECTION.                          GAS3UPD 
00947  2200-010.                                                        GAS3UPD 
00948                                                                   GAS3UPD 
00949      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS3UPD 
00950                                                                   GAS3UPD 
00951      IF NOT GCIO-GOOD-RETURN                                      GAS3UPD 
00952         MOVE WS-ABCODE-1DF7        TO WS-ABCODE                   GAS3UPD 
00953         MOVE WS-ABCODE-1DF7-MSG    TO WS-ABCODE-MSG               GAS3UPD 
00954         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
00955                                                                   GAS3UPD 
00956      MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   GAS3UPD 
00957      SET GAC-INDEX         TO  1.                                 GAS3UPD 
00958      MOVE OENTCTRO         TO  ACWA-DISPLAY-LEN-7.                GAS3UPD 
00959                                                                   GAS3UPD 
00960  2200-210-FIND-RIGHT-OCCURS.                                      GAS3UPD 
00961                                                                   GAS3UPD 
00962      IF GAC-DEDL-BENEFIT-PERIOD(GAC-INDEX)   NOT = HIGH-VALUES ANDGAS3UPD 
00963         GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  NOT =                GAS3UPD 
00964                                                 ACWA-DISPLAY-LEN-7GAS3UPD 
00965      THEN                                                         GAS3UPD 
00966          IF  GAC-INDEX  <  GAC-ENTRY-COUNT                        GAS3UPD 
00967          THEN                                                     GAS3UPD 
00968              SET GAC-INDEX  UP BY  1                              GAS3UPD 
00969              GO TO 2200-210-FIND-RIGHT-OCCURS                     GAS3UPD 
00970          ELSE                                                     GAS3UPD 
00971              MOVE WS-ABCODE-1DL1        TO WS-ABCODE              GAS3UPD 
00972              MOVE WS-ABCODE-1DL1-MSG    TO WS-ABCODE-MSG          GAS3UPD 
00973              MOVE -1                    TO  MFRMSLTL              GAS3UPD 
00974              PERFORM 9800-000-ERROR-MSG-THEN-ABEND                GAS3UPD 
00975      ELSE                                                         GAS3UPD 
00976          NEXT SENTENCE.                                           GAS3UPD 
00977                                                                   GAS3UPD 
00978      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS3UPD 
00979          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
00980          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
00981          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
00982          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
00983          (IPGSOPTI  =  'MT' OR 'A')                               GAS3UPD 
00984      THEN                                                         GAS3UPD 
00985          ADD 1 TO ACWA-FIELD-CHG-CNT.                             GAS3UPD 
00986                                                                   GAS3UPD 
00987                                                                   GAS3UPD 
00988      IF DAYFACII NOT =   GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX)      GAS3UPD 
00989         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
00990         MOVE DAYFACII  TO  GAC-DEDL-DAY-FACTOR-IND (GAC-INDEX).   GAS3UPD 
00991                                                                   GAS3UPD 
00992      IF COPAYINI  NOT =  GAC-DEDL-CO-PAY-IND (GAC-INDEX)          GAS3UPD 
00993         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
00994         MOVE COPAYINI  TO  GAC-DEDL-CO-PAY-IND (GAC-INDEX).       GAS3UPD 
00995                                                                   GAS3UPD 
00996      IF DEFINTNI NOT =    GAC-DEDL-DEFINITION (GAC-INDEX)         GAS3UPD 
00997         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
00998         MOVE DEFINTNI  TO  GAC-DEDL-DEFINITION (GAC-INDEX).       GAS3UPD 
00999                                                                   GAS3UPD 
01000      IF MANAPLII  NOT =  GAC-DEDL-MANDATORY-IND (GAC-INDEX)       GAS3UPD 
01001         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01002         MOVE MANAPLII TO GAC-DEDL-MANDATORY-IND (GAC-INDEX).      GAS3UPD 
01003                                                                   GAS3UPD 
01004      IF CARYOVRI  NOT =  GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX)    GAS3UPD 
01005         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01006         MOVE CARYOVRI TO GAC-CARRY-OVER-CREDIT-IND (GAC-INDEX).   GAS3UPD 
01007                                                                   GAS3UPD 
01008      IF CSTCONTI  NOT =   GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX)   GAS3UPD 
01009         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01010         MOVE CSTCONTI  TO  GAC-DEDL-COST-CONTAIN-IND (GAC-INDEX). GAS3UPD 
01011                                                                   GAS3UPD 
01012      IF PERIODI  NOT =    GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX)     GAS3UPD 
01013         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01014         MOVE PERIODI   TO  GAC-DEDL-BENEFIT-PERIOD (GAC-INDEX).   GAS3UPD 
01015                                                                   GAS3UPD 
01016      IF PERTQALI  NOT =   GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX)  GAS3UPD 
01017         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01018         MOVE PERTQALI  TO  GAC-DEDL-BEN-PER-TIME-QUAL (GAC-INDEX).GAS3UPD 
01019                                                                   GAS3UPD 
01020      IF FAMINDII  NOT =   GAC-DEDL-FAM-OR-INDIV (GAC-INDEX)       GAS3UPD 
01021         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01022         MOVE FAMINDII  TO  GAC-DEDL-FAM-OR-INDIV (GAC-INDEX).     GAS3UPD 
01023                                                                   GAS3UPD 
01024      IF PLCTRMTI  NOT =   GAC-DEDL-PLACE-OF-TREATMENT (GAC-INDEX) GAS3UPD 
01025         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01026         MOVE PLCTRMTI  TO  GAC-DEDL-PLACE-OF-TREATMENT(GAC-INDEX).GAS3UPD 
01027                                                                   GAS3UPD 
01028      IF SRVGRUPI  NOT =   GAC-DEDL-SERVICE-GROUP (GAC-INDEX)      GAS3UPD 
01029         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01030         MOVE SRVGRUPI  TO GAC-DEDL-SERVICE-GROUP (GAC-INDEX).     GAS3UPD 
01031                                                                   GAS3UPD 
01032        MOVE PRTIMEFI   TO ACWA-DISPLAY-LEN-3-X.                   GAS3UPD 
01033        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS3UPD 
01034                           GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX)  GAS3UPD 
01035         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01036        MOVE ACWA-DISPLAY-LEN-3                                    GAS3UPD 
01037                        TO  GAC-DEDL-BEN-PER-TIME-FCTR (GAC-INDEX).GAS3UPD 
01038                                                                   GAS3UPD 
01039        MOVE AGELIMLI   TO ACWA-DISPLAY-LEN-3-X.                   GAS3UPD 
01040        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS3UPD 
01041                           GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX)     GAS3UPD 
01042         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01043        MOVE ACWA-DISPLAY-LEN-3                                    GAS3UPD 
01044                        TO GAC-DEDL-AGE-LIMIT-FROM (GAC-INDEX).    GAS3UPD 
01045                                                                   GAS3UPD 
01046        MOVE AGELIMHI   TO ACWA-DISPLAY-LEN-3-X.                   GAS3UPD 
01047        IF ACWA-DISPLAY-LEN-3 NOT =                                GAS3UPD 
01048                           GAC-DEDL-AGE-LIMIT-TO   (GAC-INDEX)     GAS3UPD 
01049         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01050        MOVE ACWA-DISPLAY-LEN-3                                    GAS3UPD 
01051                        TO GAC-DEDL-AGE-LIMIT-TO   (GAC-INDEX).    GAS3UPD 
01052                                                                   GAS3UPD 
01053      IF FEAKINDI  NOT =  GAC-DEDL-FEAK-IND         (GAC-INDEX)    GAS3UPD 
01054         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01055         MOVE FEAKINDI TO GAC-DEDL-FEAK-IND         (GAC-INDEX).   GAS3UPD 
01056                                                                   GAS3UPD 
           IF BISNDINI  NOT =  GAC-DEDL-BISCENDING-IND-RSV (GAC-INDEX)          
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE BISNDINI TO GAC-DEDL-BISCENDING-IND-RSV (GAC-INDEX).         
                                                                                
           IF ASCDSCDI  NOT =  GAC-DEDL-ASCEND-DESCEND-IND (GAC-INDEX)          
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE ASCDSCDI TO GAC-DEDL-ASCEND-DESCEND-IND (GAC-INDEX).         
                                                                                
      **P21595 CHANGES STARTS                                                   
           IF BENTYPI   NOT =  GAC-DEDL-BEN-TYPE           (GAC-INDEX)          
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE BENTYPI  TO GAC-DEDL-BEN-TYPE           (GAC-INDEX).         
                                                                                
           IF TIERCDI   NOT =  GAC-DEDL-TIER-CODE          (GAC-INDEX)          
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE TIERCDI  TO GAC-DEDL-TIER-CODE          (GAC-INDEX).         
                                                                                
           IF TIERLVI   NOT =  GAC-DEDL-TIER-LVL           (GAC-INDEX)          
              ADD 1         TO ACWA-FIELD-CHG-CNT                               
              MOVE TIERLVI  TO GAC-DEDL-TIER-LVL           (GAC-INDEX).         
      **P21595 CHANGES ENDS                                                     
                                                                                
01057      IF ACCUMIDI  NOT =  GAC-DEDL-ACCUMID          (GAC-INDEX)    GAS3UPD 
01058         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01059         MOVE ACCUMIDI TO GAC-DEDL-ACCUMID          (GAC-INDEX).   GAS3UPD 
01060                                                                   GAS3UPD 
01061      IF CAPINDI   NOT =  GAC-DEDL-COMB-APPLIED-IND (GAC-INDEX)    GAS3UPD 
01062         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01063         MOVE CAPINDI  TO GAC-DEDL-COMB-APPLIED-IND (GAC-INDEX).   GAS3UPD 
01064                                                                   GAS3UPD 
01065      IF SABDINDI  NOT =  GAC-DEDL-SEL-ADDL-BEN-DET (GAC-INDEX)    GAS3UPD 
01066         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01067         MOVE SABDINDI TO GAC-DEDL-SEL-ADDL-BEN-DET (GAC-INDEX).   GAS3UPD 
01068                                                                   GAS3UPD 
01069      IF AGEQLLI   NOT =  GAC-DEDL-AGE-QUAL-IND-FROM(GAC-INDEX)    GAS3UPD 
01070         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01071         MOVE AGEQLLI  TO                                          GAS3UPD 
01072                         GAC-DEDL-AGE-QUAL-IND-FROM(GAC-INDEX).    GAS3UPD 
01073                                                                   GAS3UPD 
01074      IF AGEQLHI   NOT =  GAC-DEDL-AGE-QUAL-IND-TO  (GAC-INDEX)    GAS3UPD 
01075         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01076         MOVE AGEQLHI  TO                                          GAS3UPD 
01077                         GAC-DEDL-AGE-QUAL-IND-TO  (GAC-INDEX).    GAS3UPD 
01078                                                                   GAS3UPD 
01079      IF RELPINDI  NOT =  GAC-DEDL-RELATIONSHIP-IND (GAC-INDEX)    GAS3UPD 
01080         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01081         MOVE RELPINDI TO                                          GAS3UPD 
01082                         GAC-DEDL-RELATIONSHIP-IND (GAC-INDEX).    GAS3UPD 
01083                                                                   GAS3UPD 
01084      IF CLMLVLII NOT =   GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX) GAS3UPD 
01085         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01086         MOVE CLMLVLII TO  GAC-DEDL-CLAIM-LVL-ACCUM-IND(GAC-INDEX).GAS3UPD 
01087                                                                   GAS3UPD 
01088      MOVE INTRVALI    TO ACWA-DISPLAY-LEN-3-X.                    GAS3UPD 
01089      IF ACWA-DISPLAY-LEN-3 NOT =                                  GAS3UPD 
01090                          GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX)  GAS3UPD 
01091         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01092         MOVE ACWA-DISPLAY-LEN-3 TO                                GAS3UPD 
01093                         GAC-DEDL-INTERVAL-TIME-FCTR (GAC-INDEX).  GAS3UPD 
01094                                                                   GAS3UPD 
01095      IF INTTYPEI  NOT =  GAC-DEDL-INTERVAL-TYPE (GAC-INDEX)       GAS3UPD 
01096         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01097         MOVE INTTYPEI  TO  GAC-DEDL-INTERVAL-TYPE (GAC-INDEX).    GAS3UPD 
01098                                                                   GAS3UPD 
01099      IF LOBI NOT =       GAC-DEDL-L-O-B   (GAC-INDEX)             GAS3UPD 
01100         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01101         MOVE LOBI     TO GAC-DEDL-L-O-B (GAC-INDEX).              GAS3UPD 
01102                                                                   GAS3UPD 
01103      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS3UPD 
01104      THEN                                                         GAS3UPD 
01105          MOVE ACWA-VAL-LIM-SCREEN TO ACWA-VALUE-LIMIT-9-9         GAS3UPD 
01106      ELSE                                                         GAS3UPD 
01107          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'NEG' OR                GAS3UPD 
01108              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'NEG'                   GAS3UPD 
01109          THEN                                                     GAS3UPD 
01110              MOVE -1                  TO ACWA-VALUE-LIMIT-9-9     GAS3UPD 
01111          ELSE                                                     GAS3UPD 
01107          IF  ACWA-VAL-LIM-SCREEN-NEG1-3 = 'UNL' OR                GAS3UPD 
01108              ACWA-VAL-LIM-SCREEN-NEG2-3 = 'UNL'                   GAS3UPD 
01109          THEN                                                     GAS3UPD 
01110              MOVE -2                  TO ACWA-VALUE-LIMIT-9-9     GAS3UPD 
01111          ELSE                                                     GAS3UPD 
01112              MOVE ACWA-VAL-LIM-SCREEN-7 TO ACWA-VALUE-LIMIT-7     GAS3UPD 
01113              MOVE ACWA-VAL-LIM-SCREEN-2 TO ACWA-VALUE-LIMIT-2.    GAS3UPD 
01114                                                                   GAS3UPD 
01115      IF ACWA-VALUE-LIMIT-9 NOT = GAC-DEDL-VALUE-LIMIT (GAC-INDEX) GAS3UPD 
01116         PERFORM 2600-000-PROCESS-VAL-LIMIT.                       GAS3UPD 
01117                                                                   GAS3UPD 
01118      IF ACWA-VALUE-LIMIT-9 NOT = GAC-DEDL-VALUE-LIMIT (GAC-INDEX) GAS3UPD 
01119         ADD 1                  TO ACWA-FIELD-CHG-CNT              GAS3UPD 
01120        MOVE ACWA-VALUE-LIMIT-9 TO GAC-DEDL-VALUE-LIMIT(GAC-INDEX).GAS3UPD 
01121                                                                   GAS3UPD 
01122      IF BENVLQLI  NOT =   GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX)    GAS3UPD 
01123         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01124         MOVE BENVLQLI  TO  GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX).  GAS3UPD 
01125                                                                   GAS3UPD 
01126      MOVE NEWVALUI     TO ACWA-DISPLAY-LEN-5-X.                   GAS3UPD 
01127      IF  ACWA-DISPLAY-LEN-5 NOT =                                 GAS3UPD 
01128                       GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX)    GAS3UPD 
01129      THEN                                                         GAS3UPD 
01130          ADD 1     TO ACWA-FIELD-CHG-CNT                          GAS3UPD 
01131          MOVE ACWA-DISPLAY-LEN-5                                  GAS3UPD 
01132                    TO  GAC-DEDL-INTERVAL-OVRD-VALUE (GAC-INDEX).  GAS3UPD 
01133                                                                   GAS3UPD 
01134      IF OVRDINDI  NOT =   GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX)  GAS3UPD 
01135         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01136         MOVE OVRDINDI  TO  GAC-DEDL-INTERVAL-OVRD-IND (GAC-INDEX).GAS3UPD 
01137                                                                   GAS3UPD 
01138      IF FYIVALI   NOT =  GAC-DEDL-FYI-VALUE (GAC-INDEX)           GAS3UPD 
01139         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01140         MOVE FYIVALI  TO  GAC-DEDL-FYI-VALUE (GAC-INDEX).         GAS3UPD 
01141                                                                   GAS3UPD 
01142      IF INTDESKI  =  GAC-DEDL-INTERNAL-DESCRIPTOR (GAC-INDEX)     GAS3UPD 
01143         MOVE SPACE  TO  WS-INT-TAB-CHANGE-INDICATOR               GAS3UPD 
01144      ELSE                                                         GAS3UPD 
01145         ADD 1   TO  ACWA-FIELD-CHG-CNT                            GAS3UPD 
01146         IF INTDESKI  =  IDPRODI                                   GAS3UPD 
01147            MOVE 'NP'   TO  WS-INT-TAB-CHANGE-INDICATOR            GAS3UPD 
01148            MOVE INTDESKI  TO                                      GAS3UPD 
01149                            GAC-DEDL-INTERNAL-DESCRIPTOR(GAC-INDEX)GAS3UPD 
01150         ELSE                                                      GAS3UPD 
01151            IF IDPRODI  =   GAC-DEDL-INTERNAL-DESCRIPTOR(GAC-INDEX)GAS3UPD 
01152               MOVE 'PN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS3UPD 
01153               MOVE INTDESKI  TO                                   GAS3UPD 
01154                            GAC-DEDL-INTERNAL-DESCRIPTOR(GAC-INDEX)GAS3UPD 
01155            ELSE                                                   GAS3UPD 
01156               MOVE 'NN'   TO  WS-INT-TAB-CHANGE-INDICATOR         GAS3UPD 
01157               MOVE INTDESKI  TO                                   GAS3UPD 
01158                           GAC-DEDL-INTERNAL-DESCRIPTOR(GAC-INDEX).GAS3UPD 
01159                                                                   GAS3UPD 
01160                                                                   GAS3UPD 
01161      IF CONDALLI  NOT =   GAC-COND-ALL-BIT (GAC-INDEX)            GAS3UPD 
01162         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01163         MOVE CONDALLI  TO  GAC-COND-ALL-BIT (GAC-INDEX).          GAS3UPD 
01164                                                                   GAS3UPD 
01165      IF CONDEXCI  NOT =   GAC-COND-EXCLUSION-BIT (GAC-INDEX)      GAS3UPD 
01166         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01167         MOVE CONDEXCI  TO  GAC-COND-EXCLUSION-BIT (GAC-INDEX).    GAS3UPD 
01168                                                                   GAS3UPD 
01169      IF CONDICDI  NOT =   GAC-COND-ICD-BIT (GAC-INDEX)            GAS3UPD 
01170         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01171         MOVE CONDICDI  TO  GAC-COND-ICD-BIT (GAC-INDEX).          GAS3UPD 
01172                                                                   GAS3UPD 
01173      IF CONDTABI  NOT =   GAC-COND-TB-BIT (GAC-INDEX)             GAS3UPD 
01174         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01175         MOVE CONDTABI  TO GAC-COND-TB-BIT (GAC-INDEX).            GAS3UPD 
01176                                                                   GAS3UPD 
01177      IF CONDMENI  NOT =   GAC-COND-MENTAL-BIT (GAC-INDEX)         GAS3UPD 
01178         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01179         MOVE CONDMENI  TO GAC-COND-MENTAL-BIT (GAC-INDEX).        GAS3UPD 
01180                                                                   GAS3UPD 
01181      IF CONDDRGI  NOT =   GAC-COND-DRUG-BIT (GAC-INDEX)           GAS3UPD 
01182         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01183         MOVE CONDDRGI  TO GAC-COND-DRUG-BIT (GAC-INDEX).          GAS3UPD 
01184                                                                   GAS3UPD 
01185      IF CONDALCI  NOT =   GAC-COND-ALCOHOL-BIT (GAC-INDEX)        GAS3UPD 
01186         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01187         MOVE CONDALCI  TO GAC-COND-ALCOHOL-BIT (GAC-INDEX).       GAS3UPD 
01188                                                                   GAS3UPD 
01189      IF CONDOBCI  NOT =   GAC-COND-OB-COMP-BIT (GAC-INDEX)        GAS3UPD 
01190         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01191         MOVE CONDOBCI  TO GAC-COND-OB-COMP-BIT (GAC-INDEX).       GAS3UPD 
01192                                                                   GAS3UPD 
01193      IF CONDOBNI  NOT =   GAC-COND-OB-NORM-BIT (GAC-INDEX)        GAS3UPD 
01194         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01195         MOVE CONDOBNI  TO GAC-COND-OB-NORM-BIT (GAC-INDEX).       GAS3UPD 
01196                                                                   GAS3UPD 
01197      IF CONDMALI  NOT =   GAC-COND-MALIGNANCY-BIT (GAC-INDEX)     GAS3UPD 
01198         ADD 1          TO ACWA-FIELD-CHG-CNT                      GAS3UPD 
01199         MOVE CONDMALI  TO GAC-COND-MALIGNANCY-BIT (GAC-INDEX).    GAS3UPD 
01200                                                                   GAS3UPD 
01201      IF CONDCARI  NOT =  GAC-COND-CARDIAC-DISEASE-BIT (GAC-INDEX) GAS3UPD 
01202         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01203        MOVE CONDCARI  TO GAC-COND-CARDIAC-DISEASE-BIT (GAC-INDEX).GAS3UPD 
01204                                                                   GAS3UPD 
01205      IF CONDOBSI  NOT =  GAC-COND-OBESITY-BIT (GAC-INDEX)         GAS3UPD 
01206         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01207         MOVE CONDOBSI TO GAC-COND-OBESITY-BIT (GAC-INDEX).        GAS3UPD 
01208                                                                   GAS3UPD 
01209      IF CONDKDYI  NOT =  GAC-COND-KIDNEY-DISEASE-BIT (GAC-INDEX)  GAS3UPD 
01210         ADD 1         TO ACWA-FIELD-CHG-CNT                       GAS3UPD 
01211         MOVE CONDKDYI TO GAC-COND-KIDNEY-DISEASE-BIT (GAC-INDEX). GAS3UPD 
01212                                                                   GAS3UPD 
01213      IF CONDACCI  NOT  =   GAC-COND-ACCIDENT-BIT (GAC-INDEX)      GAS3UPD 
01214         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01215         MOVE CONDACCI  TO  GAC-COND-ACCIDENT-BIT (GAC-INDEX).     GAS3UPD 
01216                                                                   GAS3UPD 
01217      IF CONDPECI  NOT  =  GAC-COND-PRE-EXIST-BIT (GAC-INDEX)      GAS3UPD 
01218         ADD  1        TO  ACWA-FIELD-CHG-CNT                      GAS3UPD 
01219         MOVE CONDPECI TO  GAC-COND-PRE-EXIST-BIT (GAC-INDEX).     GAS3UPD 
01220                                                                   GAS3UPD 
01221      IF CONDNEMI  NOT  =   GAC-COND-NON-EMER-BIT (GAC-INDEX)      GAS3UPD 
01222         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01223         MOVE CONDNEMI  TO  GAC-COND-NON-EMER-BIT (GAC-INDEX).     GAS3UPD 
01224                                                                   GAS3UPD 
01225      IF CONDSUII  NOT  =   GAC-COND-SUICIDE-BIT  (GAC-INDEX)      GAS3UPD 
01226         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01227         MOVE CONDSUII  TO  GAC-COND-SUICIDE-BIT  (GAC-INDEX).     GAS3UPD 
01228                                                                   GAS3UPD 
01229      IF CONDTMJI  NOT  =   GAC-COND-TMJ-BIT      (GAC-INDEX)      GAS3UPD 
01230         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01231         MOVE CONDTMJI  TO  GAC-COND-TMJ-BIT      (GAC-INDEX).     GAS3UPD 
01232                                                                   GAS3UPD 
01233      IF CONDINFI  NOT  =   GAC-COND-INF-BIT      (GAC-INDEX)      GAS3UPD 
01234         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01235         MOVE CONDINFI  TO  GAC-COND-INF-BIT      (GAC-INDEX).     GAS3UPD 
01236                                                                   GAS3UPD 
01237      IF CONDLIFI  NOT  =   GAC-COND-LIFE-THREAT-BIT (GAC-INDEX)   GAS3UPD 
01238         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01239         MOVE CONDLIFI  TO  GAC-COND-LIFE-THREAT-BIT  (GAC-INDEX). GAS3UPD 
01240                                                                   GAS3UPD 
01241      IF CONDEMCI  NOT  =   GAC-COND-EMER-MED-BIT    (GAC-INDEX)   GAS3UPD 
01242         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01243         MOVE CONDEMCI  TO  GAC-COND-EMER-MED-BIT     (GAC-INDEX). GAS3UPD 
01244                                                                   GAS3UPD 
01245      IF CONDEACI  NOT  =   GAC-COND-EMER-ACC-BIT    (GAC-INDEX)   GAS3UPD 
01246         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01247         MOVE CONDEACI  TO  GAC-COND-EMER-ACC-BIT     (GAC-INDEX). GAS3UPD 
01248                                                                   GAS3UPD 
01249      IF CONDSMII  NOT  =  GAC-COND-SER-MEN-ILL-BIT  (GAC-INDEX)   GAS3UPD 
01250         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01251         MOVE CONDSMII  TO GAC-COND-SER-MEN-ILL-BIT   (GAC-INDEX). GAS3UPD 
01252                                                                   GAS3UPD 
01253      IF CONDNSMI  NOT  = GAC-COND-NON-SER-MEN-ILL-BIT (GAC-INDEX) GAS3UPD 
01254         ADD  1         TO  ACWA-FIELD-CHG-CNT                     GAS3UPD 
01255         MOVE CONDNSMI  TO GAC-COND-NON-SER-MEN-ILL-BIT (GAC-INDEX)GAS3UPD 
01256                                                                   GAS3UPD 
01257      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
01258         MOVE GCIO-WRK-TABULAR-PROVISION  TO                       GAS3UPD 
01259                                     GCIO-WRK-BENEFIT-PROVISION.   GAS3UPD 
01260                                                                   GAS3UPD 
01261 ******* IF THE OCCUR IS A NEW ADDED ONE THEN IT IS FLAGED 1U      GAS3UPD 
01262 *** IN GA1BPGM ALL ATTACHED INTERNAL TABS TO THIS ADDED OCCUR     GAS3UPD 
01263 *** ALSO WILL BE FLAGED 1U IN THIS ROUTINE    NE 08/03/88         GAS3UPD 
01264                                                                   GAS3UPD 
01265      PERFORM  5000-000-READ-PROD-ALL-LVL-TAB.                     GAS3UPD 
01266         SEARCH GAC2-ENTRY                                         GAS3UPD 
01267            VARYING GAC2-INDEX                                     GAS3UPD 
01268            WHEN                                                   GAS3UPD 
01269               GAC2-INDEX NOT <  GAC2-ENTRY-COUNT  OR              GAS3UPD 
01270               GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX)  =            GAS3UPD 
01271                              GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  GAS3UPD 
01272               NEXT SENTENCE.                                      GAS3UPD 
01273                                                                   GAS3UPD 
01274         IF GAC2-INDEX <  GAC2-ENTRY-COUNT  AND                    GAS3UPD 
01275            GAC2-OCCURS-ENTRY-COUNTER(GAC2-INDEX)  =               GAS3UPD 
01276                              GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  GAS3UPD 
01277            MOVE 'N'      TO  WS-NEW-OCCR-ON-WF                    GAS3UPD 
01278         ELSE                                                      GAS3UPD 
01279            MOVE 'Y'      TO WS-NEW-OCCR-ON-WF.                    GAS3UPD 
01280 ******************************************************** 8/3/88   GAS3UPD 
01281      IF ACWA-INTERNAL-TAB-CHANGE-ONLY                             GAS3UPD 
01282         GO TO 2200-260-CHANGE-INTERNAL-TAB.                       GAS3UPD 
01283                                                                   GAS3UPD 
01284 *** CHECK LVL2-B-SWITCH                                           GAS3UPD 
01285      IF EIBAID    =       DFHENTER  AND                           GAS3UPD 
01286         DELADDI   =      'CHG/ADD'  AND                           GAS3UPD 
01287         OENTCTRI  NOT =  '0000000'  AND                           GAS3UPD 
01288         ACWA-NO-CHANGE-FOUND                                      GAS3UPD 
01289         MOVE 'Y'   TO  LVL2-B-SW                                  GAS3UPD 
01290         PERFORM 3100-RLSE-RU-GAC-REC                              GAS3UPD 
01291         GO  TO  2200-900-EXIT.                                    GAS3UPD 
01292                                                                   GAS3UPD 
01293      IF  EIBAID  =  DFHENTER       AND                            GAS3UPD 
01294          ACWA-SCREEN-HAS-NO-ERRORS AND                            GAS3UPD 
01295          GCVI-TABLE-SW = 'N'                                      GAS3UPD 
01296      THEN                                                         GAS3UPD 
01297          IF  ACWA-NO-CHANGE-FOUND                                 GAS3UPD 
01298          THEN                                                     GAS3UPD 
01299              SET  WT-01-INDEX                     TO +11          GAS3UPD 
01300              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS3UPD 
01301              PERFORM 7900-000-RESET-ATTRIBUTES                    GAS3UPD 
01302              MOVE -1 TO PERIODL                                   GAS3UPD 
01303              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS3UPD 
01304          ELSE                                                     GAS3UPD 
01305              SET  WT-01-INDEX                     TO +07          GAS3UPD 
01306              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO      GAS3UPD 
01307              PERFORM 9010-000-SEND-DATAONLY-RETURN                GAS3UPD 
01308      ELSE                                                         GAS3UPD 
01309          NEXT SENTENCE.                                           GAS3UPD 
01310                                                                   GAS3UPD 
01311      IF (EIBAID  =  DFHPF4 OR  DFHPF16) AND                       GAS3UPD 
01312          ACWA-SCREEN-HAS-NO-ERRORS      AND                       GAS3UPD 
01313          GCVI-TABLE-SW = 'N'            AND                       GAS3UPD 
01314          ACWA-NO-CHANGE-FOUND                                     GAS3UPD 
01315      THEN                                                         GAS3UPD 
01316          SET  WT-01-INDEX                     TO +09              GAS3UPD 
01317          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS3UPD 
01318          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS3UPD 
01319          MOVE -1 TO PERIODL                                       GAS3UPD 
01320          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS3UPD 
01321                                                                   GAS3UPD 
01322      IF  EIBAID   =   DFHENTER AND                                GAS3UPD 
01323          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS3UPD 
01324          ACWA-NO-CHANGE-FOUND                                     GAS3UPD 
01325      THEN                                                         GAS3UPD 
01326          SET  WT-01-INDEX                     TO +11              GAS3UPD 
01327          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO          GAS3UPD 
01328          MOVE -1 TO PERIODL                                       GAS3UPD 
01329          PERFORM 9010-000-SEND-DATAONLY-RETURN.                   GAS3UPD 
01330                                                                   GAS3UPD 
01331      PERFORM 7900-000-RESET-ATTRIBUTES.                           GAS3UPD 
01332                                                                   GAS3UPD 
01333 *** LVL2-F-SWITCH                                                 GAS3UPD 
01334      IF  (EIBAID  =   DFHPF7 OR DFHPF19) AND                      GAS3UPD 
01335          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS3UPD 
01336          ACWA-NO-CHANGE-FOUND                                     GAS3UPD 
01337      THEN                                                         GAS3UPD 
01338          MOVE  'Y'   TO  LVL2-F-SW                                GAS3UPD 
01339         PERFORM 3100-RLSE-RU-GAC-REC                              GAS3UPD 
01340          GO TO  2200-900-EXIT.                                    GAS3UPD 
01341                                                                   GAS3UPD 
01342 *** LVL2-G-SWITCH                                                 GAS3UPD 
01343      IF  (EIBAID  =  DFHPF8 OR DFHPF20) AND                       GAS3UPD 
01344          DELADDI  =  'CHG/DEL' AND  DELOPTNI  NOT =  'D' AND      GAS3UPD 
01345          ACWA-NO-CHANGE-FOUND                                     GAS3UPD 
01346      THEN                                                         GAS3UPD 
01347          MOVE  'Y'   TO  LVL2-G-SW                                GAS3UPD 
01348         PERFORM 3100-RLSE-RU-GAC-REC                              GAS3UPD 
01349          GO TO  2200-900-EXIT.                                    GAS3UPD 
01350                                                                   GAS3UPD 
01351      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS3UPD 
01352         DELADDI  =  'CHG/DEL'                                     GAS3UPD 
01353         PERFORM 4600-000-UPDATE-CDE-STATUS.                       GAS3UPD 
01354                                                                   GAS3UPD 
01355      IF  (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE OR             GAS3UPD 
01356          (IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999')) AND      GAS3UPD 
01357          (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE OR             GAS3UPD 
01358          (IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999')) AND      GAS3UPD 
01359          (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE OR             GAS3UPD 
01360          (IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999')) AND      GAS3UPD 
01361          (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE OR             GAS3UPD 
01362          (IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999')) AND      GAS3UPD 
01363          (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE OR             GAS3UPD 
01364          (IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999')) AND      GAS3UPD 
01365          (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE OR             GAS3UPD 
01366          (IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'))          GAS3UPD 
01367      THEN                                                         GAS3UPD 
01368          GO TO 2200-250-UPDATE-ALL-LVL-TAB.                       GAS3UPD 
01369                                                                   GAS3UPD 
01370      IF  IBGROPTI  =  'C' OR                                      GAS3UPD 
01371          IDGDOPTI  =  'C' OR                                      GAS3UPD 
01372          IPGNOPTI  =  'C' OR                                      GAS3UPD 
01373          IPGPOPTI  =  'C' OR                                      GAS3UPD 
01374          IPGTOPTI  =  'C' OR                                      GAS3UPD 
01375          IPGSOPTI  =  'C'                                         GAS3UPD 
01376      THEN                                                         GAS3UPD 
01377          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS3UPD 
01378                                                                   GAS3UPD 
01379      IF  (IBGROPTI  =  'MT' OR 'A') AND IBGRSLTI  =  '0000000' OR GAS3UPD 
01380          (IDGDOPTI  =  'MT' OR 'A') AND IDGDSLTI  =  '0000000' OR GAS3UPD 
01381          (IPGNOPTI  =  'MT' OR 'A') AND IPGNSLTI  =  '0000000' OR GAS3UPD 
01382          (IPGPOPTI  =  'MT' OR 'A') AND IPGPSLTI  =  '0000000' OR GAS3UPD 
01383          (IPGTOPTI  =  'MT' OR 'A') AND IPGTSLTI  =  '0000000' OR GAS3UPD 
01384          (IPGSOPTI  =  'MT' OR 'A') AND IPGSSLTI  =  '0000000'    GAS3UPD 
01385      THEN                                                         GAS3UPD 
01386          GO TO 2200-220-ADD-INTERNAL-OCCURS.                      GAS3UPD 
01387                                                                   GAS3UPD 
01388      IF  (IBGROPTI  =  'MT' OR 'A') OR                            GAS3UPD 
01389          (IDGDOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
01390          (IPGNOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
01391          (IPGPOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
01392          (IPGTOPTI  =  'MT' OR 'A') OR                            GAS3UPD 
01393          (IPGSOPTI  =  'MT' OR 'A')                               GAS3UPD 
01394      THEN                                                         GAS3UPD 
01395          GO TO 2200-230-CHANGE-PROD-SLOT-NO.                      GAS3UPD 
01396                                                                   GAS3UPD 
01397      IF  IBGROPTI  =  'D'                                         GAS3UPD 
01398      THEN                                                         GAS3UPD 
01399          MOVE '#IBGR '  TO  GCIO-WRK-TAB-PROVISION-ID             GAS3UPD 
01400          MOVE IBGRSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO             GAS3UPD 
01401      ELSE                                                         GAS3UPD 
01402          IF  IPGNOPTI  =  'D'                                     GAS3UPD 
01403          THEN                                                     GAS3UPD 
01404              MOVE '#IPGN '  TO  GCIO-WRK-TAB-PROVISION-ID         GAS3UPD 
01405              MOVE IPGNSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO         GAS3UPD 
01406          ELSE                                                     GAS3UPD 
01407              IF  IPGTOPTI  =  'D'                                 GAS3UPD 
01408              THEN                                                 GAS3UPD 
01409                  MOVE '#IPGT '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS3UPD 
01410                  MOVE IPGTSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS3UPD 
01411              ELSE                                                 GAS3UPD 
01412              IF  IPGSOPTI  =  'D'                                 GAS3UPD 
01413              THEN                                                 GAS3UPD 
01414                  MOVE '#IPGS '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS3UPD 
01415                  MOVE IPGSSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS3UPD 
01416              ELSE                                                 GAS3UPD 
01417              IF  IDGDOPTI  =  'D'                                 GAS3UPD 
01418              THEN                                                 GAS3UPD 
01419                  MOVE '#IDGD '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS3UPD 
01420                  MOVE IDGDSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS3UPD 
01421              ELSE                                                 GAS3UPD 
01422              IF  IPGPOPTI  =  'D'                                 GAS3UPD 
01423              THEN                                                 GAS3UPD 
01424                  MOVE '#IPGP '  TO  GCIO-WRK-TAB-PROVISION-ID     GAS3UPD 
01425                  MOVE IPGPSLTI  TO  GCIO-WRK-TAB-PROV-SLOT-NO     GAS3UPD 
01426              ELSE                                                 GAS3UPD 
01427                  NEXT SENTENCE.                                   GAS3UPD 
01428                                                                   GAS3UPD 
01429                                                                   GAS3UPD 
01430      IF    GAC-DEDL-INTL-TAB-1 (GAC-INDEX)                        GAS3UPD 
01431          = GCIO-WRK-TABULAR-PROVISION                             GAS3UPD 
01432      THEN                                                         GAS3UPD 
01433          MOVE GAC-DEDL-INTL-TAB-2 (GAC-INDEX)                     GAS3UPD 
01434            TO GAC-DEDL-INTL-TAB-1 (GAC-INDEX)                     GAS3UPD 
01435          MOVE GAC-DEDL-INTL-TAB-3 (GAC-INDEX)                     GAS3UPD 
01436            TO GAC-DEDL-INTL-TAB-2 (GAC-INDEX)                     GAS3UPD 
01437          MOVE GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                     GAS3UPD 
01438            TO GAC-DEDL-INTL-TAB-3 (GAC-INDEX)                     GAS3UPD 
01439          MOVE GAC-DEDL-INTL-TAB-5 (GAC-INDEX)                     GAS3UPD 
01440            TO GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                     GAS3UPD 
01441 ******** MOVE HIGH-VALUES                                         GAS3UPD 
01442 ********   TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)                     GAS3UPD 
01443          IF GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                       GAS3UPD 
01444                       = HIGH-VALUES OR WS-SPACES-ZEROS            GAS3UPD 
01445             MOVE WS-SPACES-ZEROS                                  GAS3UPD 
01446               TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)                  GAS3UPD 
01447          ELSE                                                     GAS3UPD 
01448             MOVE HIGH-VALUES                                      GAS3UPD 
01449               TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)                  GAS3UPD 
01450          END-IF                                                   GAS3UPD 
01451      ELSE                                                         GAS3UPD 
01452          IF   GAC-DEDL-INTL-TAB-2 (GAC-INDEX)                     GAS3UPD 
01453             = GCIO-WRK-TABULAR-PROVISION                          GAS3UPD 
01454         THEN                                                      GAS3UPD 
01455             MOVE GAC-DEDL-INTL-TAB-3 (GAC-INDEX)                  GAS3UPD 
01456               TO GAC-DEDL-INTL-TAB-2 (GAC-INDEX)                  GAS3UPD 
01457             MOVE GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                  GAS3UPD 
01458               TO GAC-DEDL-INTL-TAB-3 (GAC-INDEX)                  GAS3UPD 
01459             MOVE GAC-DEDL-INTL-TAB-5 (GAC-INDEX)                  GAS3UPD 
01460               TO GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                  GAS3UPD 
01461 *********** MOVE HIGH-VALUES                                      GAS3UPD 
01462 ***********   TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)                  GAS3UPD 
01463             IF GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                    GAS3UPD 
01464                          = HIGH-VALUES OR WS-SPACES-ZEROS         GAS3UPD 
01465                MOVE WS-SPACES-ZEROS                               GAS3UPD 
01466                  TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)               GAS3UPD 
01467             ELSE                                                  GAS3UPD 
01468                MOVE HIGH-VALUES                                   GAS3UPD 
01469                  TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)               GAS3UPD 
01470             END-IF                                                GAS3UPD 
01471         ELSE                                                      GAS3UPD 
01472             IF    GAC-DEDL-INTL-TAB-3 (GAC-INDEX)                 GAS3UPD 
01473                 = GCIO-WRK-TABULAR-PROVISION                      GAS3UPD 
01474             THEN                                                  GAS3UPD 
01475                 MOVE GAC-DEDL-INTL-TAB-4 (GAC-INDEX)              GAS3UPD 
01476                   TO GAC-DEDL-INTL-TAB-3 (GAC-INDEX)              GAS3UPD 
01477                 MOVE GAC-DEDL-INTL-TAB-5 (GAC-INDEX)              GAS3UPD 
01478                   TO GAC-DEDL-INTL-TAB-4 (GAC-INDEX)              GAS3UPD 
01479 *************** MOVE HIGH-VALUES                                  GAS3UPD 
01480 ***************   TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)              GAS3UPD 
01481                 IF GAC-DEDL-INTL-TAB-4 (GAC-INDEX)                GAS3UPD 
01482                              = HIGH-VALUES OR WS-SPACES-ZEROS     GAS3UPD 
01483                    MOVE WS-SPACES-ZEROS                           GAS3UPD 
01484                      TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)           GAS3UPD 
01485                 ELSE                                              GAS3UPD 
01486                    MOVE HIGH-VALUES                               GAS3UPD 
01487                      TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)           GAS3UPD 
01488                 END-IF                                            GAS3UPD 
01489             ELSE                                                  GAS3UPD 
01490                 IF    GAC-DEDL-INTL-TAB-4 (GAC-INDEX)             GAS3UPD 
01491                     = GCIO-WRK-TABULAR-PROVISION                  GAS3UPD 
01492                 THEN                                              GAS3UPD 
01493                     MOVE GAC-DEDL-INTL-TAB-5 (GAC-INDEX)          GAS3UPD 
01494                       TO GAC-DEDL-INTL-TAB-4 (GAC-INDEX)          GAS3UPD 
01495 ******************* MOVE HIGH-VALUES                              GAS3UPD 
01496 *******************   TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)          GAS3UPD 
01497                     IF GAC-DEDL-INTL-TAB-4 (GAC-INDEX)            GAS3UPD 
01498                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS3UPD 
01499                        MOVE WS-SPACES-ZEROS                       GAS3UPD 
01500                          TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)       GAS3UPD 
01501                     ELSE                                          GAS3UPD 
01502                        MOVE HIGH-VALUES                           GAS3UPD 
01503                          TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)       GAS3UPD 
01504                     END-IF                                        GAS3UPD 
01505             ELSE                                                  GAS3UPD 
01506                 IF    GAC-DEDL-INTL-TAB-5 (GAC-INDEX)             GAS3UPD 
01507                     = GCIO-WRK-TABULAR-PROVISION                  GAS3UPD 
01508                 THEN                                              GAS3UPD 
01509 ******************* MOVE HIGH-VALUES                              GAS3UPD 
01510 *******************   TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)          GAS3UPD 
01511                     IF GAC-DEDL-INTL-TAB-4 (GAC-INDEX)            GAS3UPD 
01512                                  = HIGH-VALUES OR WS-SPACES-ZEROS GAS3UPD 
01513                        MOVE WS-SPACES-ZEROS                       GAS3UPD 
01514                          TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)       GAS3UPD 
01515                     ELSE                                          GAS3UPD 
01516                        MOVE HIGH-VALUES                           GAS3UPD 
01517                          TO GAC-DEDL-INTL-TAB-5 (GAC-INDEX)       GAS3UPD 
01518                     END-IF                                        GAS3UPD 
01519                 ELSE                                              GAS3UPD 
01520                     MOVE WS-ABCODE-1DL2        TO WS-ABCODE       GAS3UPD 
01521                     MOVE WS-ABCODE-1DL2-MSG    TO WS-ABCODE-MSG   GAS3UPD 
01522                     PERFORM 9800-000-ERROR-MSG-THEN-ABEND.        GAS3UPD 
01523                                                                   GAS3UPD 
01524 **** SUBTRACT  1  FROM  GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX).   GAS3UPD 
01525      IF GAC-DEDL-INTL-TAB-5 (GAC-INDEX) = HIGH-VALUES             GAS3UPD 
01526         NEXT SENTENCE                                             GAS3UPD 
01527      ELSE                                                         GAS3UPD 
01528         SUBTRACT  1  FROM  GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX).GAS3UPD 
01529                                                                   GAS3UPD 
01530      GO TO 2200-250-UPDATE-ALL-LVL-TAB.                           GAS3UPD 
01531                                                                   GAS3UPD 
01532                                                                   GAS3UPD 
01533  2200-220-ADD-INTERNAL-OCCURS.                                    GAS3UPD 
01534                                                                   GAS3UPD 
01535 **** IF GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX) > 5                GAS3UPD 
01536      IF GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX) = 5 AND            GAS3UPD 
01537**       GAC-INT-TS (GAC-INDEX 5) NOT = HIGH-VALUES                GAS3UPD 
01537         GAC-INT-ID (GAC-INDEX 5) NOT = HIGH-VALUES                GAS3UPD 
01538         SET  WT-01-INDEX                     TO +26               GAS3UPD 
01539         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX) TO ERRMSGO           GAS3UPD 
01540         MOVE SPACES TO  IBGROPTO, IPGPOPTO, IDGDOPTO,             GAS3UPD 
01541                         IPGTOPTO, IPGNOPTO, IPGSOPTO              GAS3UPD 
01542         MOVE SPACES TO  MFRMSLTO                                  GAS3UPD 
01543         MOVE -1 TO PERIODL                                        GAS3UPD 
01544         PERFORM 9010-000-SEND-DATAONLY-RETURN.                    GAS3UPD 
01545                                                                   GAS3UPD 
01546      MOVE GXA-PROVISION-ID          TO     WS-SAVE-INTL-TAB-ID.   GAS3UPD 
01547      MOVE GXA-PROVISION-SLOT-NO     TO     WS-TAB-PROV-COPY-SLOT. GAS3UPD 
01548      MOVE GAC-OCCURS-ENTRY-COUNTER(GAC-INDEX)  TO                 GAS3UPD 
01549                                            WS-SAVE-INTL-TAB-SLOT. GAS3UPD 
01550                                                                   GAS3UPD 
01551      SET GAC-INT-INDEX  TO  1.                                    GAS3UPD 
01552      SEARCH GAC-INT-TS                                            GAS3UPD 
01553         VARYING GAC-INT-INDEX                                     GAS3UPD 
01554         AT END                                                    GAS3UPD 
01555            MOVE WS-ABCODE-1DL3      TO  WS-ABCODE                 GAS3UPD 
01556            MOVE WS-ABCODE-1DL3-MSG  TO  WS-ABCODE-MSG             GAS3UPD 
01557            PERFORM 9800-000-ERROR-MSG-THEN-ABEND                  GAS3UPD 
01558         WHEN                                                      GAS3UPD 
01559            GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  NOT <             GAS3UPD 
01560                                                 WS-SAVE-INTL-TAB  GAS3UPD 
01561            NEXT SENTENCE.                                         GAS3UPD 
01562                                                                   GAS3UPD 
01563      IF GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  NOT =                GAS3UPD 
01564                                                  WS-SAVE-INTL-TAB GAS3UPD 
01565         PERFORM 2200-225-SHIFT-OCCURS-UP                          GAS3UPD 
01566            VARYING GAC-INT-INDEX  FROM  GAC-INT-INDEX  BY  1      GAS3UPD 
01567            UNTIL GAC-INT-INDEX  >  5                              GAS3UPD 
01568 ******* ADD  1  TO  GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)        GAS3UPD 
01569         IF GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX) < 5             GAS3UPD 
01570            ADD  1  TO  GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)     GAS3UPD 
01571         END-IF                                                    GAS3UPD 
01572      ELSE                                                         GAS3UPD 
01573         MOVE WS-SAVE-INTL-TAB  TO                                 GAS3UPD 
01574                               GAC-INT-TS(GAC-INDEX GAC-INT-INDEX).GAS3UPD 
01575                                                                   GAS3UPD 
01576      GO TO 2200-240-SETUP-GCIO-PARMS.                             GAS3UPD 
01577                                                                   GAS3UPD 
01578                                                                   GAS3UPD 
01579  2200-225-SHIFT-OCCURS-UP.                                        GAS3UPD 
01580      MOVE GAC-INT-TS(GAC-INDEX GAC-INT-INDEX)  TO  WS-INTL-TAB-ID.GAS3UPD 
01581      MOVE WS-SAVE-INTL-TAB  TO                                    GAS3UPD 
01582                             GAC-INT-TS(GAC-INDEX GAC-INT-INDEX).  GAS3UPD 
01583                                                                   GAS3UPD 
01584      MOVE WS-INTL-TAB-ID  TO  WS-SAVE-INTL-TAB.                   GAS3UPD 
01585                                                                   GAS3UPD 
01586                                                                   GAS3UPD 
01587  2200-230-CHANGE-PROD-SLOT-NO.                                    GAS3UPD 
01588                                                                   GAS3UPD 
01589      MOVE GXA-PROVISION-SLOT-NO  TO  WS-TAB-PROV-COPY-SLOT.       GAS3UPD 
01590      SET  GAC-INT-INDEX TO      1.                                GAS3UPD 
01591      SET  GAC-INT-INDEX DOWN BY 1.                                GAS3UPD 
01592                                                                   GAS3UPD 
01593  2200-240-CHANGE-LOOP.                                            GAS3UPD 
01594                                                                   GAS3UPD 
01595      SET GAC-INT-INDEX UP BY 1.                                   GAS3UPD 
01596      IF  GAC-INT-INDEX > 5                                        GAS3UPD 
01597          GO TO 2200-240-SETUP-GCIO-PARMS.                         GAS3UPD 
01598                                                                   GAS3UPD 
01599      IF GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  =  GXA-PROVISION-ID  GAS3UPD 
01600      THEN                                                         GAS3UPD 
01601          MOVE GAC-OCCURS-ENTRY-COUNTER (GAC-INDEX)                GAS3UPD 
01602            TO GXA-PROVISION-SLOT-NO                               GAS3UPD 
01603               GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)              GAS3UPD 
01604          GO TO 2200-240-SETUP-GCIO-PARMS                          GAS3UPD 
01605      ELSE                                                         GAS3UPD 
01606          GO TO 2200-240-CHANGE-LOOP.                              GAS3UPD 
01607                                                                   GAS3UPD 
01608                                                                   GAS3UPD 
01609  2200-240-SETUP-GCIO-PARMS.                                       GAS3UPD 
01610                                                                   GAS3UPD 
01611      IF FRMNUIDI  =  'GS3A'                                       GAS3UPD 
01612         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS3UPD 
01613      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS3UPD 
01614         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS3UPD 
01615      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
01616         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS3UPD 
01617         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS3UPD 
01618         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS3UPD 
01619                                                                   GAS3UPD 
01620      MOVE GC-GCPSWORK-DDNAME   TO  GCIO2-FILE-DDNAME.             GAS3UPD 
01621      MOVE GC-GCIO-AREA-1       TO  GCIO2-IO-AREA-TO-USE.          GAS3UPD 
01622      MOVE GXA-PROVISION-ID     TO  GCIO-WRK-TAB-PROVISION-ID.     GAS3UPD 
01623      MOVE GAC-OCCURS-ENTRY-COUNTER (GAC-INDEX)                    GAS3UPD 
01624                                  TO  GCIO-WRK-TAB-PROV-SLOT-NO    GAS3UPD 
01625                                      GXA-PROVISION-SLOT-NO.       GAS3UPD 
01626      MOVE GCIO-WORKFILE-KEY    TO  GCIO2-FILE-KEY                 GAS3UPD 
01627                                    WORK-RECORD-2.                 GAS3UPD 
01628      MOVE WS-TAB-PROV-COPY-SLOT  TO  WRK2-PROV-POOL-COPY-SLOT.    GAS3UPD 
01629                                                                   GAS3UPD 
01630      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
01631         MOVE GCA-BEN-PROV-ID  TO  WRK2-ALL-LEV-BEN-PROV.          GAS3UPD 
01632                                                                   GAS3UPD 
01633      MOVE GC-GCIO-ACCESS-CODE-WR  TO  GCIO2-FILE-ACCESS-CODE.     GAS3UPD 
01634                                                                   GAS3UPD 
01635      COMPUTE WS-IO-PARM-WRK-INTERNL-TAB-LEN  =                    GAS3UPD 
01636               GC-GCIOPARM-LEN  +   GCIO2-RECORD-LENGTH.           GAS3UPD 
01637                                                                   GAS3UPD 
01638                                                                   GAS3UPD 
01639  2200-250-UPDATE-ALL-LVL-TAB.                                     GAS3UPD 
01640                                                                   GAS3UPD 
01641 *******                                                           GAS3UPD 
01642 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS3UPD 
01643 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLE UPDATES THEGAS3UPD 
01644 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS3UPD 
01645 *******                                                           GAS3UPD 
01646                                                                   GAS3UPD 
01647      IF  IBGROPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS3UPD 
01648      THEN                                                         GAS3UPD 
01649          MOVE MFRMSLTI     TO  IBGRSLTI,   ACWA-DISPLAY-LEN-7     GAS3UPD 
01650          SET  WT-01-INDEX  TO  +01                                GAS3UPD 
01651          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
01652          MOVE -1           TO  IBGROPTL                           GAS3UPD 
01653          MOVE DFHBMABF     TO  IBGRSLTA,   IBGRIDA                GAS3UPD 
01654          MOVE SPACES       TO  IBGROPTI                           GAS3UPD 
01655          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS3UPD 
01656          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS3UPD 
01657          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS3UPD 
01658      IF  IDGDOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS3UPD 
01659      THEN                                                         GAS3UPD 
01660          MOVE MFRMSLTI     TO  IDGDSLTI,   ACWA-DISPLAY-LEN-7     GAS3UPD 
01661          SET  WT-01-INDEX  TO  +23                                GAS3UPD 
01662          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
01663          MOVE -1           TO  IDGDOPTL                           GAS3UPD 
01664          MOVE DFHBMABF     TO  IDGDSLTA,   IDGDIDA                GAS3UPD 
01665          MOVE SPACES       TO  IDGDOPTI                           GAS3UPD 
01666          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS3UPD 
01667          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS3UPD 
01668          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS3UPD 
01669      IF  IPGNOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS3UPD 
01670      THEN                                                         GAS3UPD 
01671          MOVE MFRMSLTI     TO  IPGNSLTI,   ACWA-DISPLAY-LEN-7     GAS3UPD 
01672          SET  WT-01-INDEX  TO  +02                                GAS3UPD 
01673          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
01674          MOVE -1           TO  IPGNOPTL                           GAS3UPD 
01675          MOVE DFHBMABF     TO  IPGNSLTA,   IPGNIDA                GAS3UPD 
01676          MOVE SPACES       TO  IPGNOPTI                           GAS3UPD 
01677          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS3UPD 
01678          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS3UPD 
01679          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS3UPD 
01680      IF  IPGPOPTI =  'MT'  AND    FRMNUIDI =  'GTM1'              GAS3UPD 
01681      THEN                                                         GAS3UPD 
01682          MOVE MFRMSLTI     TO  IPGPSLTI,   ACWA-DISPLAY-LEN-7     GAS3UPD 
01683          SET  WT-01-INDEX  TO  +24                                GAS3UPD 
01684          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
01685          MOVE -1           TO  IPGPOPTL                           GAS3UPD 
01686          MOVE DFHBMABF     TO  IPGPSLTA,   IPGPIDA                GAS3UPD 
01687          MOVE SPACES       TO  IPGPOPTI                           GAS3UPD 
01688          MOVE DFHBMUNP     TO  MFRMSLTA                           GAS3UPD 
01689          MOVE ZEROS        TO  MFRMSLTI,   MFRMSLTL               GAS3UPD 
01690          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS3UPD 
01691      IF  IPGSOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS3UPD 
01692      THEN                                                         GAS3UPD 
01693          MOVE MFRMSLTI     TO  IPGSSLTI,   ACWA-DISPLAY-LEN-7     GAS3UPD 
01694          SET  WT-01-INDEX  TO  +03                                GAS3UPD 
01695          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
01696          MOVE -1           TO  IPGSOPTL                           GAS3UPD 
01697          MOVE DFHBMABF     TO IPGSSLTA,    IPGSIDA                GAS3UPD 
01698          MOVE SPACES       TO IPGSOPTI                            GAS3UPD 
01699          MOVE DFHBMUNP     TO MFRMSLTA                            GAS3UPD 
01700          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS3UPD 
01701          GO TO 2200-252-REPLACE-INT-TAB-SLOT.                     GAS3UPD 
01702      IF  IPGTOPTI =  'MT'   AND    FRMNUIDI =  'GTM1'             GAS3UPD 
01703      THEN                                                         GAS3UPD 
01704          MOVE MFRMSLTI     TO  IPGTSLTI,   ACWA-DISPLAY-LEN-7     GAS3UPD 
01705          SET  WT-01-INDEX  TO  +03                                GAS3UPD 
01706          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
01707          MOVE -1           TO  IPGTOPTL                           GAS3UPD 
01708          MOVE DFHBMABF     TO IPGTSLTA,    IPGTIDA                GAS3UPD 
01709          MOVE SPACES       TO IPGTOPTI                            GAS3UPD 
01710          MOVE DFHBMUNP     TO MFRMSLTA                            GAS3UPD 
01711          MOVE ZEROS        TO MFRMSLTI,    MFRMSLTL               GAS3UPD 
01712          GO TO 2200-252-REPLACE-INT-TAB-SLOT                      GAS3UPD 
01713      ELSE                                                         GAS3UPD 
01714          GO TO 2200-253-BYPASS-INT-TAB-SLOT.                      GAS3UPD 
01715                                                                   GAS3UPD 
01716                                                                   GAS3UPD 
01717  2200-252-REPLACE-INT-TAB-SLOT.                                   GAS3UPD 
01718      SET  GAC-INT-INDEX  TO       1.                              GAS3UPD 
01719      SET  GAC-INT-INDEX  DOWN BY  1.                              GAS3UPD 
01720                                                                   GAS3UPD 
01721  2200-252-REPLACE-LOOP.                                           GAS3UPD 
01722                                                                   GAS3UPD 
01723      SET GAC-INT-INDEX  UP BY  1.                                 GAS3UPD 
01724                                                                   GAS3UPD 
01725      IF  GAC-INT-INDEX  >  5                                      GAS3UPD 
01726          MOVE WS-ABCODE-1DLX       TO  WS-ABCODE                  GAS3UPD 
01727          MOVE WS-ABCODE-1DLX-MSG   TO  WS-ABCODE-MSG              GAS3UPD 
01728          PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                   GAS3UPD 
01729                                                                   GAS3UPD 
01730      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  =  GXA-PROVISION-ID GAS3UPD 
01731      THEN                                                         GAS3UPD 
01732          MOVE ACWA-DISPLAY-LEN-7  TO                              GAS3UPD 
01733                             GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX) GAS3UPD 
01734          GO TO 2200-252-REPLACE-LOOP-END                          GAS3UPD 
01735      ELSE                                                         GAS3UPD 
01736          GO TO 2200-252-REPLACE-LOOP.                             GAS3UPD 
01737                                                                   GAS3UPD 
01738  2200-252-REPLACE-LOOP-END.                                       GAS3UPD 
01739                                                                   GAS3UPD 
01740  2200-253-BYPASS-INT-TAB-SLOT.                                    GAS3UPD 
01741 *******                                                          |GAS3UPD 
01742 * STS *----------------------------------------------------------*GAS3UPD 
01743 *******                                                           GAS3UPD 
01744                                                                   GAS3UPD 
01745                                                                   GAS3UPD 
01746      PERFORM 3000-000-UPDATE-GAC-RECORD.                          GAS3UPD 
01747                                                                   GAS3UPD 
01748 *******                                                           GAS3UPD 
01749 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS3UPD 
01750 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS3UPD 
01751 *******     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.| GAS3UPD 
01752                                                                   GAS3UPD 
01753      IF  DELADDI   =  'CHG/ADD'  AND  FRMNUIDI  =  'GTM1'  AND    GAS3UPD 
01754          OENTCTRI  NOT =  '0000000' AND                           GAS3UPD 
01755         (IBGROPTI  =  'MT'  OR   IPGNOPTI  =  'MT' OR             GAS3UPD 
01756          IDGDOPTI  =  'MT'  OR   IPGPOPTI  =  'MT' OR             GAS3UPD 
01757          IPGTOPTI  =  'MT'  OR   IPGSOPTI  =  'MT')               GAS3UPD 
01758      THEN                                                         GAS3UPD 
01759          PERFORM 7900-000-RESET-ATTRIBUTES                        GAS3UPD 
01760          MOVE 'CHG/ADD'   TO  DELADDO                             GAS3UPD 
01761          MOVE SPACES      TO  COCURANO                            GAS3UPD 
01762          MOVE DFHBMASD    TO  DLOPTLTA,   DELOPTNA                GAS3UPD 
01763          PERFORM 4100-000-DISPLAY-SKELETON                        GAS3UPD 
01764      ELSE                                                         GAS3UPD 
01765          NEXT SENTENCE.                                           GAS3UPD 
01766 *******                                                         | GAS3UPD 
01767 * STS *---------------------------------------------------------* GAS3UPD 
01768 *******                                                           GAS3UPD 
01769                                                                   GAS3UPD 
01770      IF (IBGROPTL  =  ZERO OR  IBGROPTI  =  SPACE) AND            GAS3UPD 
01771         (IDGDOPTL  =  ZERO OR  IDGDOPTI  =  SPACE) AND            GAS3UPD 
01772         (IPGNOPTL  =  ZERO OR  IPGNOPTI  =  SPACE) AND            GAS3UPD 
01773         (IPGPOPTL  =  ZERO OR  IPGPOPTI  =  SPACE) AND            GAS3UPD 
01774         (IPGTOPTL  =  ZERO OR  IPGTOPTI  =  SPACE) AND            GAS3UPD 
01775         (IPGSOPTL  =  ZERO OR  IPGSOPTI  =  SPACE)                GAS3UPD 
01776         GO TO 2200-800.                                           GAS3UPD 
01777                                                                   GAS3UPD 
01778 *******                                                           GAS3UPD 
01779 * STS *==> MAP FROM OPTION UNDER SINGLE TABULAR SUPPORT DOES NOT  GAS3UPD 
01780 *     *     CREATE INTERNAL TAB ON WORKFILE, IT SIMPLY UPDATES THEGAS3UPD 
01781 *     *     THE ACCUM RECORD AND SCREEN, THEN REDISPLAYS SCREEN.  GAS3UPD 
01782 *******                                                           GAS3UPD 
01783                                                                   GAS3UPD 
01784      IF  DELADDI   =  'CHG/DEL'  AND  FRMNUIDI  =  'GTM1'  AND    GAS3UPD 
01785         (IBGROPTI  =  'MT'  OR  IPGNOPTI  =  'MT'  OR             GAS3UPD 
01786          IDGDOPTI  =  'MT'  OR  IPGPOPTI  =  'MT'  OR             GAS3UPD 
01787          IPGTOPTI  =  'MT'  OR  IPGSOPTI  =  'MT')                GAS3UPD 
01788      THEN                                                         GAS3UPD 
01789          GO TO 2200-800                                           GAS3UPD 
01790      ELSE                                                         GAS3UPD 
01791          NEXT SENTENCE.                                           GAS3UPD 
01792                                                                   GAS3UPD 
01793 *******                                                           GAS3UPD 
01794 * STS *==> DELETES DURING SINGLE TABULAR SUPPORT, THERE IS NEVER  GAS3UPD 
01795 *     *     A WORKFILE INTERNAL TABULAR TO DELETE                 GAS3UPD 
01796 *******                                                           GAS3UPD 
01797                                                                   GAS3UPD 
01798      IF  IBGROPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS3UPD 
01799          MOVE SPACE      TO  IBGROPTO                             GAS3UPD 
01800          MOVE '0000000'  TO  IBGRSLTO                             GAS3UPD 
01801          GO TO 2200-800.                                          GAS3UPD 
01802      IF  IDGDOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS3UPD 
01803          MOVE SPACE      TO  IDGDOPTO                             GAS3UPD 
01804          MOVE '0000000'  TO  IDGDSLTO                             GAS3UPD 
01805          GO TO 2200-800.                                          GAS3UPD 
01806      IF  IPGNOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS3UPD 
01807          MOVE SPACE      TO  IPGNOPTO                             GAS3UPD 
01808          MOVE '0000000'  TO  IPGNSLTO                             GAS3UPD 
01809          GO TO 2200-800.                                          GAS3UPD 
01810      IF  IPGPOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS3UPD 
01811          MOVE SPACE      TO  IPGPOPTO                             GAS3UPD 
01812          MOVE '0000000'  TO  IPGPSLTO                             GAS3UPD 
01813          GO TO 2200-800.                                          GAS3UPD 
01814      IF  IPGTOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS3UPD 
01815          MOVE SPACE      TO  IPGTOPTO                             GAS3UPD 
01816          MOVE '0000000'  TO  IPGTSLTO                             GAS3UPD 
01817          GO TO 2200-800.                                          GAS3UPD 
01818      IF  IPGSOPTI =  'D'   AND   FRMNUIDI =  'GTM1'               GAS3UPD 
01819          MOVE SPACE      TO  IPGSOPTO                             GAS3UPD 
01820          MOVE '0000000'  TO  IPGSSLTO                             GAS3UPD 
01821          GO TO 2200-800.                                          GAS3UPD 
01822                                                                   GAS3UPD 
01823 *******                                                          |GAS3UPD 
01824 * STS *----------------------------------------------------------*GAS3UPD 
01825 *******                                                           GAS3UPD 
01826                                                                   GAS3UPD 
01827      IF IBGROPTI  =  'D' AND  IBGRSLTI  <  '9000000'              GAS3UPD 
01828         MOVE SPACE      TO  IBGROPTO                              GAS3UPD 
01829         MOVE '0000000'  TO  IBGRSLTO                              GAS3UPD 
01830         GO TO 2200-800.                                           GAS3UPD 
01831      IF IDGDOPTI  =  'D' AND  IDGDSLTI  <  '9000000'              GAS3UPD 
01832         MOVE SPACE      TO  IDGDOPTO                              GAS3UPD 
01833         MOVE '0000000'  TO  IDGDSLTO                              GAS3UPD 
01834         GO TO 2200-800.                                           GAS3UPD 
01835      IF IPGNOPTI  =  'D' AND  IPGNSLTI  <  '9000000'              GAS3UPD 
01836         MOVE SPACE      TO  IPGNOPTO                              GAS3UPD 
01837         MOVE '0000000'  TO  IPGNSLTO                              GAS3UPD 
01838         GO TO 2200-800.                                           GAS3UPD 
01839      IF IPGPOPTI  =  'D' AND  IPGPSLTI  <  '9000000'              GAS3UPD 
01840         MOVE SPACE      TO  IPGPOPTO                              GAS3UPD 
01841         MOVE '0000000'  TO  IPGPSLTO                              GAS3UPD 
01842         GO TO 2200-800.                                           GAS3UPD 
01843      IF IPGTOPTI  =  'D' AND  IPGTSLTI  <  '9000000'              GAS3UPD 
01844         MOVE SPACE      TO  IPGTOPTO                              GAS3UPD 
01845         MOVE '0000000'  TO  IPGTSLTO                              GAS3UPD 
01846         GO TO 2200-800.                                           GAS3UPD 
01847      IF IPGSOPTI  =  'D' AND  IPGSSLTI  <  '9000000'              GAS3UPD 
01848         MOVE SPACE      TO  IPGSOPTO                              GAS3UPD 
01849         MOVE '0000000'  TO  IPGSSLTO                              GAS3UPD 
01850         GO TO 2200-800.                                           GAS3UPD 
01851                                                                   GAS3UPD 
01852      IF IBGROPTI  =  'D'                                          GAS3UPD 
01853         MOVE '0000000'  TO  IBGRSLTO                              GAS3UPD 
01854         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS3UPD 
01855      IF IDGDOPTI  =  'D'                                          GAS3UPD 
01856         MOVE '0000000'  TO  IDGDSLTO                              GAS3UPD 
01857         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS3UPD 
01858      IF IPGNOPTI  =  'D'                                          GAS3UPD 
01859         MOVE '0000000'  TO  IPGNSLTO                              GAS3UPD 
01860         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS3UPD 
01861      IF IPGPOPTI  =  'D'                                          GAS3UPD 
01862         MOVE '0000000'  TO  IPGPSLTO                              GAS3UPD 
01863         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS3UPD 
01864      IF IPGTOPTI  =  'D'                                          GAS3UPD 
01865         MOVE '0000000'  TO  IPGTSLTO                              GAS3UPD 
01866         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS3UPD 
01867      IF IPGSOPTI  =  'D'                                          GAS3UPD 
01868         MOVE '0000000'  TO  IPGSSLTO                              GAS3UPD 
01869         GO TO 2200-270-DELETE-INTERNAL-TAB.                       GAS3UPD 
01870                                                                   GAS3UPD 
01871                                                                   GAS3UPD 
01872  2200-260-CHANGE-INTERNAL-TAB.                                    GAS3UPD 
01873                                                                   GAS3UPD 
01874 *    EXEC CICS GETMAIN                                            GAS3UPD 
01875 *              SET(ADDRESS OF COMMUNICATION-KEY-AREA)             GAS3UPD 
01876 *              INITIMG(WS-HEX-00)                                 GAS3UPD 
01877 *              LENGTH(WS-COMMUNICATION-KEY-LEN)                   GAS3UPD 
01878 *              END-EXEC.                                          GAS3UPD 
01879                                                                   GAS3UPD 
01880 *    SET ACWA-COMM-KEY-PNTR  TO                                   GAS3UPD 
01881 *                      ADDRESS OF COMMUNICATION-KEY-AREA.         GAS3UPD 
01882                                                                   GAS3UPD 
01883      SET GCA-RECORD-POINTER TO                                    GAS3UPD 
01884                     ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.    GAS3UPD 
01885                                                                   GAS3UPD 
01886      MOVE GCIO-WRK-PLAN-CODE         TO  GCA-PLAN-CODE.           GAS3UPD 
01887      MOVE GCIO-WRK-GROUP-NUM         TO  GCA-GROUP-NUM.           GAS3UPD 
01888      MOVE GCIO-WRK-SECTION-NUM       TO  GCA-SECTION-NUM.         GAS3UPD 
01889      MOVE GCIO-WRK-PKG-CODE          TO  GCA-PKG-CODE.            GAS3UPD 
01890      MOVE GCIO-WRK-LINE-OF-BUS       TO  GCA-L-O-B.               GAS3UPD 
01891      MOVE GCIO-WRK-PROVIDER-CONTROL  TO  GCA-PROV-CTL.            GAS3UPD 
01892      MOVE GCIO-WRK-FAMILY-RELATION-LVL  TO                        GAS3UPD 
01893                                          GCA-FAM-REL-LVL.         GAS3UPD 
01894      MOVE GCIO-WRK-EFFDT-CEN         TO  GCA-EFFDT-CEN.           GAS3UPD 
01895 *    IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
01896 *       MOVE BEN-PROV-ID-NO          TO  GCA-BEN-PROV-ID.         GAS3UPD 
01897      MOVE TABIDI                     TO  GCA-ALL-LEVEL-TAB-ID.    GAS3UPD 
01898      MOVE TABSLTNI                   TO  ACWA-DISPLAY-LEN-7.      GAS3UPD 
01899      MOVE ACWA-DISPLAY-LEN-7         TO  GCA-ALL-LEVEL-TAB-SLOT.  GAS3UPD 
01900      MOVE FUNCTONI          TO   GCA-ALL-LEVEL-TAB-FUNC-CODE.     GAS3UPD 
01901      MOVE GXA-PROVISION-ID           TO  GCA-INTERNAL-TAB-ID.     GAS3UPD 
01902      MOVE OENTCTRI                   TO  GCA-INTERNAL-TAB-SLOT    GAS3UPD 
01903                                          GCA-OCCURS-ENTRY-COUNTER.GAS3UPD 
01904                                                                   GAS3UPD 
01905      IF  DELADDI  =  'CHG/ADD'                                    GAS3UPD 
01906          MOVE 'A'  TO  GCA-ADD-DEL-IND                            GAS3UPD 
01907      ELSE                                                         GAS3UPD 
01908          MOVE 'D'  TO  GCA-ADD-DEL-IND.                           GAS3UPD 
01909                                                                   GAS3UPD 
01910      MOVE GXA-INCLUDE-EXCLUDE-IND  TO  GCA-I-E-INDC.              GAS3UPD 
01911      MOVE FRMNUIDI                 TO  GCA-FROM-MENU-ID.          GAS3UPD 
01912                                                                   GAS3UPD 
01913      IF IBGROPTI  =  'C' AND  IBGRSLTI  >  '8999999' OR           GAS3UPD 
01914         IDGDOPTI  =  'C' AND  IDGDSLTI  >  '8999999' OR           GAS3UPD 
01915         IPGNOPTI  =  'C' AND  IPGNSLTI  >  '8999999' OR           GAS3UPD 
01916         IPGPOPTI  =  'C' AND  IPGPSLTI  >  '8999999' OR           GAS3UPD 
01917         IPGTOPTI  =  'C' AND  IPGTSLTI  >  '8999999' OR           GAS3UPD 
01918         IPGSOPTI  =  'C' AND  IPGSSLTI  >  '8999999'              GAS3UPD 
01919         GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                       GAS3UPD 
01920                                                                   GAS3UPD 
01921      IF WRK-SIGNAL-FROM-ONLINE  =  'W'                            GAS3UPD 
01922         MOVE 'W'       TO  WRK2-SIGNAL-FROM-ONLINE                GAS3UPD 
01923         MOVE '1U'      TO  WRK2-CDE-SP                            GAS3UPD 
01924         ADD   1        TO  ACWA-CDE-1U-COUNT                      GAS3UPD 
01925      ELSE                                                         GAS3UPD 
01926         IF CDEINDO   =  ('+CDE+' OR  '+CDE-') AND                 GAS3UPD 
01927            DELADDI   =  'CHG/DEL'                                 GAS3UPD 
01928            IF INTDESKI  =  IDPRODI  AND                           GAS3UPD 
01929               WS-NEW-OCCR-ON-WF  = 'N'         THEN               GAS3UPD 
01930               MOVE '2 '   TO  WRK2-CDE-SP                         GAS3UPD 
01931               ADD   1     TO  ACWA-CDE-2B-COUNT                   GAS3UPD 
01932            ELSE                                                   GAS3UPD 
01933               MOVE '1U'   TO  WRK2-CDE-SP                         GAS3UPD 
01934               ADD   1     TO  ACWA-CDE-1U-COUNT                   GAS3UPD 
01935         ELSE                                                      GAS3UPD 
01936            IF  WS-NEW-OCCR-ON-WF = 'Y'   AND                      GAS3UPD 
01937                CDEINDO  = ('+CDE+'  OR '+CDE-')                   GAS3UPD 
01938                MOVE '1U'     TO WRK2-CDE-SP                       GAS3UPD 
01939                ADD   1       TO ACWA-CDE-1U-COUNT                 GAS3UPD 
01940            ELSE                                                   GAS3UPD 
01941                MOVE '2 '   TO  WRK2-CDE-SP                        GAS3UPD 
01942                ADD   1     TO  ACWA-CDE-2B-COUNT.                 GAS3UPD 
01943                                                                   GAS3UPD 
01944 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS3UPD 
01945      MOVE '1'          TO  GCIO2-OPER-ID-IND.                     GAS3UPD 
01946                                                                   GAS3UPD 
01947      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
01948                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS3UPD 
01949                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS3UPD 
01950                                                                   GAS3UPD 
01951      IF NOT GCIO2-GOOD-RETURN                                     GAS3UPD 
01952         MOVE WS-ABCODE-1DF9       TO  WS-ABCODE                   GAS3UPD 
01953         MOVE WS-ABCODE-1DF9-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
01954         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
01955                                                                   GAS3UPD 
01956      GO TO 2200-280-XCTL-TO-INT-TAB-PGM.                          GAS3UPD 
01957                                                                   GAS3UPD 
01958                                                                   GAS3UPD 
01959  2200-270-DELETE-INTERNAL-TAB.                                    GAS3UPD 
01960                                                                   GAS3UPD 
01961      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS3UPD 
01962         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS3UPD 
01963           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS3UPD 
01964           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS3UPD 
01965                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS3UPD 
01966         EXEC CICS GETMAIN                                         GAS3UPD 
01967                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS3UPD 
01968                INITIMG(WS-HEX-00)                                 GAS3UPD 
01969                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS3UPD 
01970                END-EXEC                                           GAS3UPD 
01971         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS3UPD 
01972                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS3UPD 
01973                                                                   GAS3UPD 
01974      IF FRMNUIDI  =  'GS3A'                                       GAS3UPD 
01975         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS3UPD 
01976      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS3UPD 
01977         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS3UPD 
01978      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
01979         MOVE 'C6'              TO  GCIO-WRK-RECORD-TYPE           GAS3UPD 
01980         MOVE TABIDI            TO  GCIO-WRK-PROVISION-ID          GAS3UPD 
01981         MOVE TABSLTNI          TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS3UPD 
01982                                                                   GAS3UPD 
01983      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS3UPD 
01984      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS3UPD 
01985      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS3UPD 
01986      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS3UPD 
01987      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS3UPD 
01988           TO  GXA-ENTRY-COUNT.                                    GAS3UPD 
01989                                                                   GAS3UPD 
01990      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
01991                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS3UPD 
01992                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS3UPD 
01993                                                                   GAS3UPD 
01994      IF NOT GCIO2-GOOD-RETURN                                     GAS3UPD 
01995         MOVE WS-ABCODE-1DF5       TO  WS-ABCODE                   GAS3UPD 
01996         MOVE WS-ABCODE-1DF5-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
01997         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
01998                                                                   GAS3UPD 
01999      MOVE GC-GCIO-ACCESS-CODE-DL TO  GCIO2-FILE-ACCESS-CODE.      GAS3UPD 
02000      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
02001                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS3UPD 
02002                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS3UPD 
02003                                                                   GAS3UPD 
02004      IF NOT GCIO2-GOOD-RETURN                                     GAS3UPD 
02005         MOVE WS-ABCODE-1DFC       TO  WS-ABCODE                   GAS3UPD 
02006         MOVE WS-ABCODE-1DFC-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
02007         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02008                                                                   GAS3UPD 
02009      IF WRK2-CDE-SP =  '1U'                                       GAS3UPD 
02010         SUBTRACT  1  FROM  ACWA-CDE-1U-COUNT.                     GAS3UPD 
02011                                                                   GAS3UPD 
02012      IF WRK2-CDE-SP  =  '2 '                                      GAS3UPD 
02013         SUBTRACT  1  FROM  ACWA-CDE-2B-COUNT.                     GAS3UPD 
02014                                                                   GAS3UPD 
02015      MOVE SPACES  TO  IBGROPTO,  IPGNOPTO,  IPGTOPTO              GAS3UPD 
02016                       IDGDOPTO,  IPGPOPTO   IPGSOPTO.             GAS3UPD 
02017      GO TO 2200-800.                                              GAS3UPD 
02018                                                                   GAS3UPD 
02019                                                                   GAS3UPD 
02020  2200-280-XCTL-TO-INT-TAB-PGM.                                    GAS3UPD 
02021                                                                   GAS3UPD 
02022      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS3UPD 
02023         DELADDI  =  'CHG/DEL' AND                                 GAS3UPD 
02024         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS3UPD 
02025         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS3UPD 
02026         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS3UPD 
02027                                                                   GAS3UPD 
02028      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS3UPD 
02029         ACWA-CDE-2B-COUNT  =  ZERO                                GAS3UPD 
02030         NEXT SENTENCE                                             GAS3UPD 
02031      ELSE                                                         GAS3UPD 
02032         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS3UPD 
02033                                                                   GAS3UPD 
02034      MOVE INTR-TAB-PGM-ID   TO  WS-INTERNAL-TABULAR-PGM-ID.       GAS3UPD 
02035      MOVE 'GAS3UPD' TO DELADD-OPTION.                             GAS3UPD 
02036                                                                   GAS3UPD 
02037      EXEC CICS  XCTL  PROGRAM (WS-INTERNAL-TABULAR-PGM-ID)        GAS3UPD 
02038                 COMMAREA(DFHCOMMAREA)                             GAS3UPD 
02039                 LENGTH  (LENGTH OF DFHCOMMAREA)  END-EXEC.        GAS3UPD 
02040                                                                   GAS3UPD 
02041  2200-800.                                                        GAS3UPD 
02042                                                                   GAS3UPD 
02043      IF CDEINDO  =  ('+CDE+' OR  '+CDE-') AND                     GAS3UPD 
02044         DELADDI  =  'CHG/DEL' AND                                 GAS3UPD 
02045         (WS-INT-DESCRP-CHG-TO-NON-PROD OR                         GAS3UPD 
02046         WS-INT-DESCRP-CHG-BACK-TO-PROD)                           GAS3UPD 
02047         PERFORM 4900-RESET-OTHER-INT-TABS.                        GAS3UPD 
02048                                                                   GAS3UPD 
02049      IF ACWA-CDE-1U-COUNT  =  ZERO AND                            GAS3UPD 
02050         ACWA-CDE-2B-COUNT  =  ZERO                                GAS3UPD 
02051         NEXT SENTENCE                                             GAS3UPD 
02052      ELSE                                                         GAS3UPD 
02053         PERFORM 4700-000-UPDATE-CONTROL-RECORD.                   GAS3UPD 
02054                                                                   GAS3UPD 
02055      MOVE -1  TO  PERIODL.                                        GAS3UPD 
02056      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS3UPD 
02057                                                                   GAS3UPD 
02058  2200-900-EXIT. EXIT.                                             GAS3UPD 
02059                                                                   GAS3UPD 
02060 /*****************************************************************GAS3UPD 
02061 *           D E L E T E   T H I S   O C C U R A N C E            *GAS3UPD 
02062 *    THIS ROUTINE WILL FIND THE ENTRY CORRESPONDING TO THE       *GAS3UPD 
02063 *  SCREEN'S DISPLAY AND REMOVE THAT ENTRY FROM THE ALL LEVEL     *GAS3UPD 
02064 *  TABULAR RECORD, INCLUDED WITH THAT IS CODE TO DELETE ANY      *GAS3UPD 
02065 *  INTERNAL TABULAR ENTRIES THAT MIGHT BE SPECIFIED BY THAT ENTRY*GAS3UPD 
02066 ******************************************************************GAS3UPD 
02067  2400-000-DELETE-THIS-OCCURANCE SECTION.                          GAS3UPD 
02068  2400-010.                                                        GAS3UPD 
02069                                                                   GAS3UPD 
02070      PERFORM 3200-000-READ-REC-FOR-UPDATE.                        GAS3UPD 
02071                                                                   GAS3UPD 
02072      IF NOT GCIO-GOOD-RETURN                                      GAS3UPD 
02073         MOVE WS-ABCODE-1DFB       TO  WS-ABCODE                   GAS3UPD 
02074         MOVE WS-ABCODE-1DFB-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
02075         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02076                                                                   GAS3UPD 
02077      IF FRMNUIDI  =  'GS3A'                                       GAS3UPD 
02078         MOVE 'G4'  TO  GCIO-WRK-RECORD-TYPE.                      GAS3UPD 
02079      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS3UPD 
02080         MOVE 'C3'  TO  GCIO-WRK-RECORD-TYPE.                      GAS3UPD 
02081      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
02082         MOVE 'C6'  TO  GCIO-WRK-RECORD-TYPE                       GAS3UPD 
02083         MOVE GCIO-WRK-TABULAR-PROVISION                           GAS3UPD 
02084                    TO  GCIO-WRK-BENEFIT-PROVISION.                GAS3UPD 
02085                                                                   GAS3UPD 
02086      MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   GAS3UPD 
02087      MOVE OENTCTRI         TO  ACWA-DISPLAY-LEN-7.                GAS3UPD 
02088                                                                   GAS3UPD 
02089         EXEC CICS GETMAIN                                         GAS3UPD 
02090                SET(ADDRESS OF COPY-TABULAR-TABLE-AREA)            GAS3UPD 
02091                INITIMG(WS-HEX-00)                                 GAS3UPD 
02092                LENGTH(WS-COPY-TABLE-LEN)                          GAS3UPD 
02093                END-EXEC.                                          GAS3UPD 
02094                                                                   GAS3UPD 
02095         SET ACWA-COPY-TAB-PNTR        TO                          GAS3UPD 
02096                  ADDRESS OF COPY-TABULAR-TABLE-AREA.              GAS3UPD 
02097                                                                   GAS3UPD 
02098      SET GAC-INDEX,  COPY-IDX  TO  1.                             GAS3UPD 
02099                                                                   GAS3UPD 
02100                                                                   GAS3UPD 
02101  2400-100-COPY-SAVED-AND-DELETE.                                  GAS3UPD 
02102                                                                   GAS3UPD 
02103      IF  GAC-INDEX  <  GAC-ENTRY-COUNT                            GAS3UPD 
02104      THEN                                                         GAS3UPD 
02105          IF  GAC-OCCURS-ENTRY-COUNTER (GAC-INDEX)  NOT =          GAS3UPD 
02106              ACWA-DISPLAY-LEN-7                                   GAS3UPD 
02107          THEN                                                     GAS3UPD 
02108              MOVE GAC-ENTRY          (GAC-INDEX)                  GAS3UPD 
02109                TO COPY-TABULAR-TABLE (COPY-IDX)                   GAS3UPD 
02110              SET  GAC-INDEX,  COPY-IDX  UP BY  1                  GAS3UPD 
02111              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS3UPD 
02112          ELSE                                                     GAS3UPD 
02113 *---- WE ARE NOT GOING TO COPY THE ENTRY THAT IS BEING DELETED.   GAS3UPD 
02114 *---- BUT WE SAVE IT SINCE WE HAVE TO DELETE ANY INTERNAL TABULARSGAS3UPD 
02115              MOVE GAC-ENTRY (GAC-INDEX)  TO  WS-ENTRY             GAS3UPD 
02116              SET  COPY-IDX3  TO  GAC-INDEX                        GAS3UPD 
02117              SET  GAC-INDEX  UP BY  1                             GAS3UPD 
02118              GO TO 2400-100-COPY-SAVED-AND-DELETE                 GAS3UPD 
02119      ELSE                                                         GAS3UPD 
02120          MOVE GAC-ENTRY          (GAC-INDEX)                      GAS3UPD 
02121            TO COPY-TABULAR-TABLE (COPY-IDX).                      GAS3UPD 
02122                                                                   GAS3UPD 
02123      SET  GAC-ENTRY-COUNT  TO  COPY-IDX.                          GAS3UPD 
02124      MOVE GAC-ENTRY-COUNT  TO  GAC-ENTRY-COUNT.                   GAS3UPD 
02125      SET COPY-IDX,  GAC-INDEX  TO  1.                             GAS3UPD 
02126                                                                   GAS3UPD 
02127                                                                   GAS3UPD 
02128  2400-200-MOVE-UPDATED-TABLE.                                     GAS3UPD 
02129                                                                   GAS3UPD 
02130      IF GAC-INDEX  NOT >  GAC-ENTRY-COUNT                         GAS3UPD 
02131         MOVE COPY-TABULAR-TABLE (COPY-IDX)                        GAS3UPD 
02132           TO GAC-ENTRY          (GAC-INDEX)                       GAS3UPD 
02133         SET GAC-INDEX,  COPY-IDX  UP BY  1                        GAS3UPD 
02134         GO TO 2400-200-MOVE-UPDATED-TABLE.                        GAS3UPD 
02135                                                                   GAS3UPD 
02136 *======== D1218 06/03/88 NG  ==================================   GAS3UPD 
02137       IF CDEINDO  =  '+CDE+'  OR '+CDE-'                          GAS3UPD 
02138          PERFORM 4600-000-UPDATE-CDE-STATUS.                      GAS3UPD 
02139                                                                   GAS3UPD 
02140 *=============================================================    GAS3UPD 
02141      PERFORM 3000-000-UPDATE-GAC-RECORD.                          GAS3UPD 
02142                                                                   GAS3UPD 
02143      SET GAC-INDEX  TO  GAC-ENTRY-COUNT.                          GAS3UPD 
02144      MOVE WS-ENTRY  TO  GAC-ENTRY (GAC-INDEX).                    GAS3UPD 
02145                                                                   GAS3UPD 
02146      IF GAC-INTERNAL-TABULAR-COUNT (GAC-INDEX)  =  1              GAS3UPD 
02147         GO TO 2400-340-DELETE-LOOP-END.                           GAS3UPD 
02148                                                                   GAS3UPD 
02149                                                                   GAS3UPD 
02150 *---- DELETE INTERNAL TABULARS FROM W/F THAT ARE ATTACHED TO      GAS3UPD 
02151 *       ALL LVL TAB OCCURANCE BEING DELETED                       GAS3UPD 
02152  2400-300-DELETE-INTERNAL-TABS.                                   GAS3UPD 
02153      SET GAC-INT-INDEX  TO       1.                               GAS3UPD 
02154      SET GAC-INT-INDEX  DOWN BY  1.                               GAS3UPD 
02155                                                                   GAS3UPD 
02156  2400-320-DELETE-LOOP.                                            GAS3UPD 
02157                                                                   GAS3UPD 
02158      SET GAC-INT-INDEX UP BY 1.                                   GAS3UPD 
02159      IF  GAC-INT-INDEX >  5                                       GAS3UPD 
02160          GO TO 2400-340-DELETE-LOOP-END.                          GAS3UPD 
02161                                                                   GAS3UPD 
02162      IF GAC-INT-ID (GAC-INDEX GAC-INT-INDEX) = HIGH-VALUES        GAS3UPD 
02163         GO TO 2400-340-DELETE-LOOP-END.                           GAS3UPD 
02164                                                                   GAS3UPD 
02165                                                                   GAS3UPD 
02166 ************   09/14/88  NE                                       GAS3UPD 
02167      IF  GAC-INT-SLOT (GAC-INDEX GAC-INT-INDEX)  >  8999999       GAS3UPD 
02168          NEXT SENTENCE                                            GAS3UPD 
02169      ELSE                                                         GAS3UPD 
02170          GO TO  2400-320-DELETE-LOOP.                             GAS3UPD 
02171                                                                   GAS3UPD 
02172      IF ACWA-WF-INTERNAL-TAB-COMP  NOT >  ZERO                    GAS3UPD 
02173         COMPUTE WS-WF-INTR-TAB-LEN  =    GC-GCIOPARM-LEN       +  GAS3UPD 
02174           GC-WORKFILE-KEY-LEN   +  GC-GCTABULR-IPGP-FIXED-LEN  +  GAS3UPD 
02175           GC-GCTABULR-IPGP-VARY-LEN  *                            GAS3UPD 
02176                                 GC-GCTABULR-IPGP-VARY-MAX-OCUR    GAS3UPD 
02177         EXEC CICS GETMAIN                                         GAS3UPD 
02178                SET(ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD)     GAS3UPD 
02179                INITIMG(WS-HEX-00)                                 GAS3UPD 
02180                LENGTH(WS-WF-INTR-TAB-LEN)                         GAS3UPD 
02181                END-EXEC                                           GAS3UPD 
02182         SET ACWA-WF-INTERNAL-TAB-PNTR      TO                     GAS3UPD 
02183                  ADDRESS OF WF-IO-PARM-INTERNAL-TAB-RECORD.       GAS3UPD 
02184                                                                   GAS3UPD 
02185      MOVE GAC-INT-TS (GAC-INDEX GAC-INT-INDEX)                    GAS3UPD 
02186                                  TO GCIO-WRK-TABULAR-PROVISION.   GAS3UPD 
02187      MOVE GC-GCPSWORK-DDNAME     TO  GCIO2-FILE-DDNAME.           GAS3UPD 
02188      MOVE GC-GCIO-AREA-1         TO  GCIO2-IO-AREA-TO-USE.        GAS3UPD 
02189      MOVE GCIO-WORKFILE-KEY      TO  GCIO2-FILE-KEY.              GAS3UPD 
02190      MOVE GC-GCIO-ACCESS-CODE-RD TO  GCIO2-FILE-ACCESS-CODE.      GAS3UPD 
02191      MOVE GC-GCTABULR-IPGP-VARY-MAX-OCUR                          GAS3UPD 
02192           TO  GXA-ENTRY-COUNT.                                    GAS3UPD 
02193                                                                   GAS3UPD 
02194      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
02195                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS3UPD 
02196                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS3UPD 
02197      IF NOT GCIO2-GOOD-RETURN                                     GAS3UPD 
02198         MOVE WS-ABCODE-1DF5       TO  WS-ABCODE                   GAS3UPD 
02199         MOVE WS-ABCODE-1DF5-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
02200         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02201                                                                   GAS3UPD 
02202                                                                   GAS3UPD 
02203      MOVE GC-GCIO-ACCESS-CODE-DL TO GCIO2-FILE-ACCESS-CODE.       GAS3UPD 
02204      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
02205                 COMMAREA(WF-IO-PARM-INTERNAL-TAB-RECORD)          GAS3UPD 
02206                 LENGTH(WS-IO-PARM-WRK-INTERNL-TAB-LEN)  END-EXEC. GAS3UPD 
02207                                                                   GAS3UPD 
02208      IF NOT GCIO2-GOOD-RETURN                                     GAS3UPD 
02209         MOVE WS-ABCODE-1DFC       TO  WS-ABCODE                   GAS3UPD 
02210         MOVE WS-ABCODE-1DFC-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
02211         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02212                                                                   GAS3UPD 
02213      IF WRK2-CDE-SP  =  '1U'                                      GAS3UPD 
02214         SUBTRACT 1  FROM  ACWA-CDE-1U-COUNT.                      GAS3UPD 
02215      IF WRK2-CDE-SP  =  '2 '                                      GAS3UPD 
02216         SUBTRACT 1  FROM  ACWA-CDE-2B-COUNT.                      GAS3UPD 
02217                                                                   GAS3UPD 
02218      GO TO 2400-320-DELETE-LOOP.                                  GAS3UPD 
02219                                                                   GAS3UPD 
02220                                                                   GAS3UPD 
02221  2400-340-DELETE-LOOP-END.                                        GAS3UPD 
02222      PERFORM 4700-000-UPDATE-CONTROL-RECORD.                      GAS3UPD 
02223                                                                   GAS3UPD 
02224  2400-400-DISPLAY-SCREEN.                                         GAS3UPD 
02225                                                                   GAS3UPD 
02226      IF COPY-IDX3  =  GAC-ENTRY-COUNT AND  =  1                   GAS3UPD 
02227         MOVE 'CHG/ADD'     TO  DELADDO                            GAS3UPD 
02228         MOVE SPACES        TO  DELOPTNO,   DELOLITO               GAS3UPD 
02229         MOVE DFHBMASD      TO  DLOPTLTA,   DELOPTNA               GAS3UPD 
02230         SET  WT-01-INDEX   TO  +19                                GAS3UPD 
02231         MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO         GAS3UPD 
02232         PERFORM 4100-000-DISPLAY-SKELETON.                        GAS3UPD 
02233                                                                   GAS3UPD 
02234      IF  COPY-IDX3  <  GAC-ENTRY-COUNT                            GAS3UPD 
02235      THEN                                                         GAS3UPD 
02236          SET GAC-INDEX  TO  COPY-IDX3                             GAS3UPD 
02237          MOVE SPACES    TO  ERRMSGO                               GAS3UPD 
02238          PERFORM 4400-000-BUILD-DISPLAY                           GAS3UPD 
02239      ELSE                                                         GAS3UPD 
02240          SET  GAC-INDEX     TO  1                                 GAS3UPD 
02241          SET  WT-01-INDEX   TO  +14                               GAS3UPD 
02242          MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO        GAS3UPD 
02243          PERFORM 4400-000-BUILD-DISPLAY.                          GAS3UPD 
02244                                                                   GAS3UPD 
02245      MOVE SPACES  TO  DELOPTNO.                                   GAS3UPD 
02246      PERFORM 9010-000-SEND-DATAONLY-RETURN.                       GAS3UPD 
02247                                                                   GAS3UPD 
02248  2400-900-EXIT. EXIT.                                             GAS3UPD 
02249                                                                   GAS3UPD 
02250 /*****************************************************************GAS3UPD 
02251 *     P R O C E S S   V A L   L I M I T                           GAS3UPD 
02252 ******************************************************************GAS3UPD 
02253  2600-000-PROCESS-VAL-LIMIT     SECTION.                          GAS3UPD 
02254  2600-010.                                                        GAS3UPD 
02255                                                                   GAS3UPD 
02256      IF (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'NEG' OR                   GAS3UPD 
02257          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'NEG') OR                 GAS3UPD 
02256         (ACWA-VAL-LIM-SCREEN-NEG1-3  = 'UNL' OR                   GAS3UPD 
02257          ACWA-VAL-LIM-SCREEN-NEG2-3  =  'UNL')                    GAS3UPD 
02258          GO TO 2600-900-EXIT.                                     GAS3UPD 
02259                                                                   GAS3UPD 
02260      IF  ACWA-BNMXVALI-N NUMERIC                                  GAS3UPD 
02261      THEN                                                         GAS3UPD 
02262          IF  BENVLQLI  =  '5'                                     GAS3UPD 
02263          THEN                                                     GAS3UPD 
02264              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-7         GAS3UPD 
02265              MOVE ZEROS            TO  ACWA-VALUE-LIMIT-2         GAS3UPD 
02266              GO TO 2600-900-EXIT                                  GAS3UPD 
02267          ELSE                                                     GAS3UPD 
02268              MOVE ACWA-BNMXVALI-N  TO  ACWA-VALUE-LIMIT-9-9       GAS3UPD 
02269              GO TO 2600-900-EXIT                                  GAS3UPD 
02270      ELSE                                                         GAS3UPD 
02271          NEXT SENTENCE.                                           GAS3UPD 
02272                                                                   GAS3UPD 
02273      IF  ACWA-VAL-LIM-SCREEN-1  =  '.'                            GAS3UPD 
02274          MOVE ACWA-VAL-LIM-SCREEN-7  TO  ACWA-VALUE-LIMIT-7       GAS3UPD 
02275          MOVE ACWA-VAL-LIM-SCREEN-2  TO  ACWA-VALUE-LIMIT-2       GAS3UPD 
02276          GO TO 2600-900-EXIT.                                     GAS3UPD 
02277                                                                   GAS3UPD 
02278  2600-900-EXIT. EXIT.                                             GAS3UPD 
02279                                                                   GAS3UPD 
02280 /*****************************************************************GAS3UPD 
02281 * 3000 UPDATE GAD RECORD                                         *GAS3UPD 
02282 *                                                                *GAS3UPD 
02283 *    THIS ROUTINE REWRITES THE UPDATED RECORD TO THE WORK FILE.  *GAS3UPD 
02284 ******************************************************************GAS3UPD 
02285  3000-000-UPDATE-GAC-RECORD     SECTION.                          GAS3UPD 
02286  3000-010.                                                        GAS3UPD 
02287                                                                   GAS3UPD 
02288      COMPUTE  GCIO-RECORD-LENGTH   =                              GAS3UPD 
02289          GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ADL-FIXED-LEN  +     GAS3UPD 
02290          (GC-GCTABULR-ADL-VARY-LEN * GAC-ENTRY-COUNT).            GAS3UPD 
02291                                                                   GAS3UPD 
02292 ** SET INDICATOR TO CAPTURE OPERATOR-ID.                          GAS3UPD 
02293      MOVE '1'                      TO  GCIO-OPER-ID-IND.          GAS3UPD 
02294      MOVE  GC-GCIO-ACCESS-CODE-WU  TO  GCIO-FILE-ACCESS-CODE.     GAS3UPD 
02295                                                                   GAS3UPD 
02296      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
02297                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS3UPD 
02298                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS3UPD 
02299                                                                   GAS3UPD 
02300      IF NOT GCIO-GOOD-RETURN                                      GAS3UPD 
02301         MOVE WS-ABCODE-1DF4       TO  WS-ABCODE                   GAS3UPD 
02302         MOVE WS-ABCODE-1DF4-MSG   TO  WS-ABCODE-MSG               GAS3UPD 
02303         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02304                                                                   GAS3UPD 
02305  3000-900-EXIT. EXIT.                                             GAS3UPD 
02306                                                                   GAS3UPD 
02307 ******************************************************************GAS3UPD 
02308 * 3100  UNLOCK THE GAB ACCUM TAB RECORD READ EARLIER FOR UPDATE   GAS3UPD 
02309 ******************************************************************GAS3UPD 
02310  3100-RLSE-RU-GAC-REC SECTION.                                    GAS3UPD 
02311                                                                   GAS3UPD 
02312      MOVE GC-GCIO-ACCESS-CODE-UNL  TO  GCIO-FILE-ACCESS-CODE.     GAS3UPD 
02313                                                                   GAS3UPD 
02314      EXEC CICS  LINK   PROGRAM('GCIOPGM')                         GAS3UPD 
02315                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS3UPD 
02316                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS3UPD 
02317                                                                   GAS3UPD 
02318      IF NOT GCIO-GOOD-RETURN                                      GAS3UPD 
02319         MOVE WS-ABCODE-1DF4        TO  WS-ABCODE                  GAS3UPD 
02320         MOVE WS-ABCODE-1DF4-MSG    TO  WS-ABCODE-MSG              GAS3UPD 
02321         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02322  3199-EXIT.     EXIT.                                             GAS3UPD 
02323                                                                   GAS3UPD 
02324 /*****************************************************************GAS3UPD 
02325 * 3200  READ REC FOR UPDATE                                      *GAS3UPD 
02326 *                                                                *GAS3UPD 
02327 *    THIS ROUTINE READS THE RECORD FOR UPDATE THAT CORRESPONDS   *GAS3UPD 
02328 *  TO THE KEY FIELDS FOUND ON THE SCREEN'S HEADING.              *GAS3UPD 
02329 ******************************************************************GAS3UPD 
02330  3200-000-READ-REC-FOR-UPDATE   SECTION.                          GAS3UPD 
02331  3300-010.                                                        GAS3UPD 
02332                                                                   GAS3UPD 
02333      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN   +  GAS3UPD 
02334            GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ADL-FIXED-LEN   +  GAS3UPD 
02335            (GC-GCTABULR-ADL-VARY-LEN   *                          GAS3UPD 
02336                                  GC-GCTABULR-ADL-VARY-MAX-OCUR).  GAS3UPD 
02337                                                                   GAS3UPD 
02338      IF ACWA-WF-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS3UPD 
02339         NEXT SENTENCE                                             GAS3UPD 
02340      ELSE                                                         GAS3UPD 
02341         EXEC CICS GETMAIN                                         GAS3UPD 
02342                SET(ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD)      GAS3UPD 
02343                INITIMG(WS-HEX-00)                                 GAS3UPD 
02344                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS3UPD 
02345                END-EXEC                                           GAS3UPD 
02346         SET ACWA-WF-ALL-LEVEL-TAB-PNTR     TO                     GAS3UPD 
02347                  ADDRESS OF WF-IO-PARM-ALL-LVL-TAB-RECORD.        GAS3UPD 
02348                                                                   GAS3UPD 
02349                                                                   GAS3UPD 
02350      IF FRMNUIDI  =  'GS3A'                                       GAS3UPD 
02351         PERFORM 6000-000-BUILD-GROUP-SPEC-KEY.                    GAS3UPD 
02352      IF FRMNUIDI  =  'GC4A'  OR 'GTM1'                            GAS3UPD 
02353         PERFORM 6100-000-BUILD-CONTRACT-KEY.                      GAS3UPD 
02354      IF FRMNUIDI  =  'GC8A'                                       GAS3UPD 
02355         PERFORM 6200-000-BUILD-BEN-PROV-KEY.                      GAS3UPD 
02356                                                                   GAS3UPD 
02357      MOVE GC-GCPSWORK-DDNAME      TO  GCIO-FILE-DDNAME.           GAS3UPD 
02358      MOVE GC-GCIO-AREA-1          TO  GCIO-IO-AREA-TO-USE.        GAS3UPD 
02359      MOVE GCIO-WORKFILE-KEY       TO  GCIO-FILE-KEY.              GAS3UPD 
02360      MOVE GC-GCIO-ACCESS-CODE-RU  TO  GCIO-FILE-ACCESS-CODE.      GAS3UPD 
02361                                                                   GAS3UPD 
02362      MOVE GC-GCTABULR-ADL-VARY-MAX-OCUR  TO  GAC-ENTRY-COUNT.     GAS3UPD 
02363                                                                   GAS3UPD 
02364      EXEC CICS  LINK   PROGRAM ('GCIOPGM')                        GAS3UPD 
02365                 COMMAREA(WF-IO-PARM-ALL-LVL-TAB-RECORD)           GAS3UPD 
02366                 LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)      END-EXEC. GAS3UPD 
02367                                                                   GAS3UPD 
02368  3200-900-EXIT. EXIT.                                             GAS3UPD 
02369                                                                   GAS3UPD 
02370 /*****************************************************************GAS3UPD 
02371 * 4100  DISPLAY SKELETON                                         *GAS3UPD 
02372 *                                                                *GAS3UPD 
02373 *    THIS ROUTINE REINITIALIZES THE SCREEN FOR THE OPERATOR      *GAS3UPD 
02374 *  AFTER THEY HAVE REVIEWED THE ENTRY THEY JUST ADDED AND        *GAS3UPD 
02375 *  INDICATED THAT THEY WANTED TO ADD MORE BY KEYING 'ENTER'.     *GAS3UPD 
02376 ******************************************************************GAS3UPD 
02377  4100-000-DISPLAY-SKELETON      SECTION.                          GAS3UPD 
02378  4100-010.                                                        GAS3UPD 
02379                                                                   GAS3UPD 
02380      MOVE SPACES    TO  ERRMSGO.                                  GAS3UPD 
02381                                                                   GAS3UPD 
02382      MOVE DFHBMFSE  TO  PERIODA.                                  GAS3UPD 
02383                                                                   GAS3UPD 
02384      MOVE DFHBMUNP  TO  BENVLQLA FAMINDIA  INTDESKA  LOBA         GAS3UPD 
02385                         IBGROPTA IPGNOPTA  IPGTOPTA  MFRMSLTA     GAS3UPD 
02386                         IDGDOPTA IPGPOPTA  IPGSOPTA.              GAS3UPD 
02387                                                                   GAS3UPD 
02388      MOVE ALL '_'  TO  PERIODO   BENVLQLO  LOBO                   GAS3UPD 
02389                        FAMINDIO  PLCTRMTO.                        GAS3UPD 
02390                                                                   GAS3UPD 
02391      MOVE LOW-VALUES  TO  INTDESKO  MFRMSLTO  IDGDOPTO IPGPOPTO   GAS3UPD 
02392                           IPGTOPTO  IBGROPTO  IPGNOPTO IPGSOPTO.  GAS3UPD 
02393                                                                   GAS3UPD 
02394      MOVE ZEROS  TO  COPAYINO  CSTCONTO  PERTQALO  CARYOVRO       GAS3UPD 
02395            DAYFACIO  SRVGRUPO  PRTIMEFO  MANAPLIO  BISNDINO       GAS3UPD 
02396                      CLMLVLIO  INTRVALO  INTTYPEO  BNMXVALO       GAS3UPD 
02397            FYIVALO   OVRDINDO  NEWVALUO  DEFINTNO                 GAS3UPD 
02398            CONDALLO  CONDEXCO  CONDICDO  CONDTABO  CONDMENO       GAS3UPD 
02399            CONDEMCO  CONDEACO  CONDSMIO  CONDNSMO                 GAS3UPD 
02400         CONDDRGO  CONDALCO  CONDOBNO  CONDOBCO  CONDMALO CONDTMJO GAS3UPD 
02401         CONDCARO  CONDOBSO  CONDKDYO  CONDACCO  CONDSUIO CONDINFO GAS3UPD 
02402            PRTIMEFO  INTRVALO  CONDPECO  CONDNEMO  NEWVALUO       GAS3UPD 
02403            OENTCTRO  IBGRSLTO  IPGNSLTO  IPGTSLTO  TOCURANO       GAS3UPD 
02404            AGEQLLO   AGEQLHO   CONDLIFO  IPGSSLTO                 GAS3UPD 
02405            RELPINDO  AGELIMLO  AGELIMHO IDGDSLTO  IPGPSLTO        GAS3UPD 
02406            FEAKINDO  ACCUMIDO  CAPINDO  SABDINDO                  GAS3UPD 
02406            BENTYPO   TIERCDO   TIERLVO.                           GAS3UPD 
02407                                                                   GAS3UPD 
02408      MOVE '01'    TO  COCURANO.                                   GAS3UPD 
02409      MOVE -1      TO  PERIODL.                                    GAS3UPD 
02410                                                                   GAS3UPD 
02411      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS3UPD 
02412                                                                   GAS3UPD 
02413  4100-900-EXIT. EXIT.                                             GAS3UPD 
02414                                                                   GAS3UPD 
02415 /*****************************************************************GAS3UPD 
02416 * 4400 BUILD DISPLAY                                             *GAS3UPD 
02417 *                                                                *GAS3UPD 
02418 *    THIS ROUTINE WILL MOVE ALL THE FIELDS FROM THE OCCURENCE    *GAS3UPD 
02419 *  SPECIFIED BY INDEX GAC-INDEX TO THE SCREEN.                   *GAS3UPD 
02420 ******************************************************************GAS3UPD 
02421  4400-000-BUILD-DISPLAY         SECTION.                          GAS3UPD 
02422  4400-010.                                                        GAS3UPD 
02423                                                                   GAS3UPD 
02424      IF  DELADDI  =  'CHG/DEL'                                    GAS3UPD 
02425      THEN                                                         GAS3UPD 
02426          MOVE 'D'  TO  DELOLITO                                   GAS3UPD 
02427      ELSE                                                         GAS3UPD 
02428          MOVE SPACE  TO  DELOLITO.                                GAS3UPD 
02429                                                                   GAS3UPD 
02430      MOVE SPACE                                       TO DELOPTNO.GAS3UPD 
02431      MOVE GAC-OCCURS-ENTRY-COUNTER     (GAC-INDEX)  TO            GAS3UPD 
02432                                                ACWA-DISPLAY-LEN-7.GAS3UPD 
02433      MOVE ACWA-DISPLAY-LEN-7                        TO  OENTCTRO. GAS3UPD 
02434      MOVE GAC-DEDL-DAY-FACTOR-IND      (GAC-INDEX)  TO  DAYFACIO. GAS3UPD 
02435      MOVE GAC-DEDL-CO-PAY-IND          (GAC-INDEX)  TO  COPAYINO. GAS3UPD 
02436      MOVE GAC-DEDL-DEFINITION          (GAC-INDEX)  TO  DEFINTNO. GAS3UPD 
02437      MOVE GAC-DEDL-MANDATORY-IND       (GAC-INDEX)  TO  MANAPLIO. GAS3UPD 
02438      MOVE GAC-CARRY-OVER-CREDIT-IND    (GAC-INDEX)  TO  CARYOVRO. GAS3UPD 
02439      MOVE GAC-DEDL-COST-CONTAIN-IND    (GAC-INDEX)  TO  CSTCONTO. GAS3UPD 
02440      MOVE GAC-DEDL-BENEFIT-PERIOD      (GAC-INDEX)  TO  PERIODO.  GAS3UPD 
02441      MOVE GAC-DEDL-BEN-PER-TIME-QUAL   (GAC-INDEX)  TO  PERTQALO. GAS3UPD 
02442      MOVE GAC-DEDL-FAM-OR-INDIV        (GAC-INDEX)  TO  FAMINDIO. GAS3UPD 
02443      MOVE GAC-DEDL-PLACE-OF-TREATMENT  (GAC-INDEX)  TO  PLCTRMTO. GAS3UPD 
02444      MOVE GAC-DEDL-SERVICE-GROUP       (GAC-INDEX)  TO  SRVGRUPO. GAS3UPD 
02445      MOVE GAC-DEDL-RELATIONSHIP-IND    (GAC-INDEX)  TO  RELPINDO. GAS3UPD 
02446      MOVE GAC-DEDL-AGE-QUAL-IND-FROM   (GAC-INDEX)  TO  AGEQLLO.  GAS3UPD 
02447      MOVE GAC-DEDL-AGE-QUAL-IND-TO     (GAC-INDEX)  TO  AGEQLHO.  GAS3UPD 
02448      MOVE GAC-DEDL-AGE-LIMIT-FROM      (GAC-INDEX)  TO            GAS3UPD 
02449                                                ACWA-DISPLAY-LEN-3.GAS3UPD 
02450      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMLO. GAS3UPD 
02451      MOVE GAC-DEDL-AGE-LIMIT-TO        (GAC-INDEX)  TO            GAS3UPD 
02452                                                ACWA-DISPLAY-LEN-3.GAS3UPD 
02453      MOVE ACWA-DISPLAY-LEN-3                        TO  AGELIMHO. GAS3UPD 
           MOVE GAC-DEDL-BISCENDING-IND-RSV  (GAC-INDEX)  TO  BISNDINO.         
           MOVE GAC-DEDL-ASCEND-DESCEND-IND  (GAC-INDEX)  TO  ASCDSCDO.         
      **P21595 CHANGES STARTS                                                   
           MOVE GAC-DEDL-BEN-TYPE            (GAC-INDEX)  TO  BENTYPO.          
           MOVE GAC-DEDL-TIER-CODE           (GAC-INDEX)  TO  TIERCDO.          
           MOVE GAC-DEDL-TIER-LVL            (GAC-INDEX)  TO  TIERLVO.          
      **P21595 CHANGES ENDS                                                     
02454      MOVE GAC-DEDL-FEAK-IND            (GAC-INDEX)  TO  FEAKINDO. GAS3UPD 
02455      MOVE GAC-DEDL-ACCUMID             (GAC-INDEX)  TO  ACCUMIDO. GAS3UPD 
02456      MOVE GAC-DEDL-COMB-APPLIED-IND    (GAC-INDEX)  TO  CAPINDO.  GAS3UPD 
02457      MOVE GAC-DEDL-SEL-ADDL-BEN-DET    (GAC-INDEX)  TO  SABDINDO. GAS3UPD 
02458      MOVE GAC-DEDL-BEN-PER-TIME-FCTR   (GAC-INDEX)  TO            GAS3UPD 
02459                                                ACWA-DISPLAY-LEN-3.GAS3UPD 
02460      MOVE ACWA-DISPLAY-LEN-3                        TO  PRTIMEFO. GAS3UPD 
02461      MOVE GAC-DEDL-CLAIM-LVL-ACCUM-IND (GAC-INDEX)  TO  CLMLVLIO. GAS3UPD 
02462      MOVE GAC-DEDL-INTERVAL-TIME-FCTR  (GAC-INDEX)  TO            GAS3UPD 
02463                                                ACWA-DISPLAY-LEN-3.GAS3UPD 
02464      MOVE ACWA-DISPLAY-LEN-3                        TO  INTRVALO. GAS3UPD 
02465      MOVE GAC-DEDL-INTERVAL-TYPE       (GAC-INDEX)  TO  INTTYPEO. GAS3UPD 
02466      MOVE GAC-DEDL-L-O-B               (GAC-INDEX)  TO  LOBO.     GAS3UPD 
02467      MOVE GAC-DEDL-VALUE-LIMIT         (GAC-INDEX)  TO            GAS3UPD 
02468                                                ACWA-VALUE-LIMIT-9.GAS3UPD 
02469      IF  ACWA-VALUE-LIMIT-9-9 = -1                                GAS3UPD 
02470      THEN                                                         GAS3UPD 
02471          MOVE 'NEG' TO BNMXVALO                                   GAS3UPD 
02472      ELSE                                                         GAS3UPD 
02469      IF  ACWA-VALUE-LIMIT-9-9 = -2                                GAS3UPD 
02470      THEN                                                         GAS3UPD 
02471          MOVE 'UNL' TO BNMXVALO                                   GAS3UPD 
02472      ELSE                                                         GAS3UPD 
02473          IF  GAC-DEDL-VALUE-QUALIFIER (GAC-INDEX) = '5'           GAS3UPD 
02474          THEN                                                     GAS3UPD 
02475              MOVE ACWA-VALUE-LIMIT-9     TO  ACWA-EDIT-VALUE-LIMITGAS3UPD 
02476              MOVE ACWA-EDIT-VALUE-LIMIT  TO  BNMXVALO             GAS3UPD 
02477          ELSE                                                     GAS3UPD 
02478              MOVE GAC-DEDL-VALUE-LIMIT(GAC-INDEX)  TO             GAS3UPD 
02479                                               ACWA-DISPLAY-LEN-9-2GAS3UPD 
02480              MOVE ACWA-DISPLAY-LEN-9-X   TO  ACWA-DISPLAY-9       GAS3UPD 
02481              MOVE SPACES                 TO  ACWA-DISPLAY-1       GAS3UPD 
02482              MOVE ACWA-DISPLAY-VALUE-LIMIT  TO  BNMXVALO.         GAS3UPD 
02483                                                                   GAS3UPD 
02484      MOVE GAC-DEDL-VALUE-QUALIFIER    (GAC-INDEX)  TO  BENVLQLO.  GAS3UPD 
02485      MOVE GAC-DEDL-INTERVAL-OVRD-VALUE(GAC-INDEX)  TO             GAS3UPD 
02486                                                ACWA-DISPLAY-LEN-5.GAS3UPD 
02487      MOVE ACWA-DISPLAY-LEN-5                       TO  NEWVALUO.  GAS3UPD 
02488      MOVE GAC-DEDL-INTERVAL-OVRD-IND  (GAC-INDEX)  TO  OVRDINDO.  GAS3UPD 
02489      MOVE GAC-DEDL-INTERNAL-DESCRIPTOR(GAC-INDEX)  TO  INTDESKO.  GAS3UPD 
02490      MOVE GAC-COND-ALL-BIT            (GAC-INDEX)  TO  CONDALLO.  GAS3UPD 
02491      MOVE GAC-COND-EXCLUSION-BIT      (GAC-INDEX)  TO  CONDEXCO.  GAS3UPD 
02492      MOVE GAC-COND-ICD-BIT            (GAC-INDEX)  TO  CONDICDO.  GAS3UPD 
02493      MOVE GAC-COND-TB-BIT             (GAC-INDEX)  TO  CONDTABO.  GAS3UPD 
02494      MOVE GAC-COND-MENTAL-BIT         (GAC-INDEX)  TO  CONDMENO.  GAS3UPD 
02495      MOVE GAC-COND-DRUG-BIT           (GAC-INDEX)  TO  CONDDRGO.  GAS3UPD 
02496      MOVE GAC-COND-ALCOHOL-BIT        (GAC-INDEX)  TO  CONDALCO.  GAS3UPD 
02497      MOVE GAC-COND-OB-COMP-BIT        (GAC-INDEX)  TO  CONDOBCO.  GAS3UPD 
02498      MOVE GAC-COND-OB-NORM-BIT        (GAC-INDEX)  TO  CONDOBNO.  GAS3UPD 
02499      MOVE GAC-COND-MALIGNANCY-BIT     (GAC-INDEX)  TO  CONDMALO.  GAS3UPD 
02500      MOVE GAC-COND-CARDIAC-DISEASE-BIT(GAC-INDEX)  TO  CONDCARO.  GAS3UPD 
02501      MOVE GAC-COND-OBESITY-BIT        (GAC-INDEX)  TO  CONDOBSO.  GAS3UPD 
02502      MOVE GAC-COND-KIDNEY-DISEASE-BIT (GAC-INDEX)  TO  CONDKDYO.  GAS3UPD 
02503      MOVE GAC-COND-ACCIDENT-BIT       (GAC-INDEX)  TO  CONDACCO.  GAS3UPD 
02504      MOVE GAC-COND-PRE-EXIST-BIT      (GAC-INDEX)  TO  CONDPECO.  GAS3UPD 
02505      MOVE GAC-COND-NON-EMER-BIT       (GAC-INDEX)  TO  CONDNEMO.  GAS3UPD 
02506      MOVE GAC-COND-SUICIDE-BIT        (GAC-INDEX)  TO  CONDSUIO.  GAS3UPD 
02507      MOVE GAC-COND-TMJ-BIT            (GAC-INDEX)  TO  CONDTMJO.  GAS3UPD 
02508      MOVE GAC-COND-INF-BIT            (GAC-INDEX)  TO  CONDINFO.  GAS3UPD 
02509      MOVE GAC-COND-LIFE-THREAT-BIT    (GAC-INDEX)  TO  CONDLIFO.  GAS3UPD 
02510      MOVE GAC-COND-EMER-MED-BIT       (GAC-INDEX)  TO  CONDEMCO.  GAS3UPD 
02511      MOVE GAC-COND-EMER-ACC-BIT       (GAC-INDEX)  TO  CONDEACO.  GAS3UPD 
02512      MOVE GAC-COND-SER-MEN-ILL-BIT    (GAC-INDEX)  TO  CONDSMIO.  GAS3UPD 
02513      MOVE GAC-COND-NON-SER-MEN-ILL-BIT (GAC-INDEX)  TO  CONDNSMO. GAS3UPD 
02514                                                                   GAS3UPD 
02515      MOVE -1  TO  PERIODL.                                        GAS3UPD 
02516                                                                   GAS3UPD 
02517      MOVE GAC-DEDL-FYI-VALUE(GAC-INDEX)  TO  FYIVALO.             GAS3UPD 
02518      SET  CURNT-OCURS-BIN        TO  GAC-INDEX.                   GAS3UPD 
02519      MOVE CURNT-OCURS-BIN        TO  CURNT-OCURS-PKD.             GAS3UPD 
02520      MOVE CURNT-OCCURS-OUT       TO  COCURANO.                    GAS3UPD 
02521                                                                   GAS3UPD 
02522      IF GAC-ENTRY-COUNT  >  1                                     GAS3UPD 
02523      THEN                                                         GAS3UPD 
02524         COMPUTE  TOTAL-OCURS-UNK  =  GAC-ENTRY-COUNT  - 1         GAS3UPD 
02525         MOVE  TOTAL-OCCURS-OUT    TO  TOCURANO                    GAS3UPD 
02526      ELSE                                                         GAS3UPD 
02527         MOVE  '01'                TO  TOCURANO.                   GAS3UPD 
02528                                                                   GAS3UPD 
02529                                                                   GAS3UPD 
02530      MOVE ZEROS   TO  IBGRSLTO,  IPGNSLTO,  IPGTSLTO              GAS3UPD 
02531                       IDGDSLTO,  IPGPSLTO   IPGSSLTO.             GAS3UPD 
02532                                                                   GAS3UPD 
02533      SET GAC-INT-INDEX  TO       1.                               GAS3UPD 
02534      SET GAC-INT-INDEX  DOWN BY  1.                               GAS3UPD 
02535                                                                   GAS3UPD 
02536  4400-300-DISPLAY-LOOP.                                           GAS3UPD 
02537                                                                   GAS3UPD 
02538      SET GAC-INT-INDEX  UP BY  1.                                 GAS3UPD 
02539      IF  GAC-INT-INDEX  >  5                                      GAS3UPD 
02540          GO TO 4400-800-SEND.                                     GAS3UPD 
02541                                                                   GAS3UPD 
02542      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  =  HIGH-VALUES      GAS3UPD 
02543          GO TO 4400-800-SEND.                                     GAS3UPD 
02544                                                                   GAS3UPD 
02545      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)   =   '#IBGR '       GAS3UPD 
02546          MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)               GAS3UPD 
02547                                   TO  ACWA-DISPLAY-LEN-7          GAS3UPD 
02548          MOVE ACWA-DISPLAY-LEN-7  TO  IBGRSLTO                    GAS3UPD 
02549          GO TO 4400-300-DISPLAY-LOOP.                             GAS3UPD 
02550                                                                   GAS3UPD 
02551      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)   =   '#IDGD '       GAS3UPD 
02552          MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)               GAS3UPD 
02553                                   TO  ACWA-DISPLAY-LEN-7          GAS3UPD 
02554          MOVE ACWA-DISPLAY-LEN-7  TO  IDGDSLTO                    GAS3UPD 
02555          GO TO 4400-300-DISPLAY-LOOP.                             GAS3UPD 
02556                                                                   GAS3UPD 
02557      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)   =   '#IPGN '       GAS3UPD 
02558          MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)               GAS3UPD 
02559                                   TO  ACWA-DISPLAY-LEN-7          GAS3UPD 
02560          MOVE ACWA-DISPLAY-LEN-7  TO  IPGNSLTO                    GAS3UPD 
02561          GO TO 4400-300-DISPLAY-LOOP.                             GAS3UPD 
02562                                                                   GAS3UPD 
02563      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)   =   '#IPGP '       GAS3UPD 
02564          MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)               GAS3UPD 
02565                                   TO  ACWA-DISPLAY-LEN-7          GAS3UPD 
02566          MOVE ACWA-DISPLAY-LEN-7  TO  IPGPSLTO                    GAS3UPD 
02567          GO TO 4400-300-DISPLAY-LOOP.                             GAS3UPD 
02568                                                                   GAS3UPD 
02569      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)   =   '#IPGT '       GAS3UPD 
02570          MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)               GAS3UPD 
02571                                   TO  ACWA-DISPLAY-LEN-7          GAS3UPD 
02572          MOVE ACWA-DISPLAY-LEN-7  TO  IPGTSLTO                    GAS3UPD 
02573          GO TO 4400-300-DISPLAY-LOOP.                             GAS3UPD 
02574                                                                   GAS3UPD 
02575      IF  GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)   =   '#IPGS '       GAS3UPD 
02576          MOVE GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)               GAS3UPD 
02577                                   TO  ACWA-DISPLAY-LEN-7          GAS3UPD 
02578          MOVE ACWA-DISPLAY-LEN-7  TO  IPGSSLTO                    GAS3UPD 
02579          GO TO 4400-300-DISPLAY-LOOP.                             GAS3UPD 
02580                                                                   GAS3UPD 
02581      MOVE WS-ABCODE-1DF3       TO  WS-ABCODE                      GAS3UPD 
02582      MOVE WS-ABCODE-1DF3-MSG   TO  WS-ABCODE-MSG                  GAS3UPD 
02583      PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                       GAS3UPD 
02584                                                                   GAS3UPD 
02585                                                                   GAS3UPD 
02586  4400-800-SEND.                                                   GAS3UPD 
02587                                                                   GAS3UPD 
02588      PERFORM 4500-000-PROTECT-CRIT-DATA-ELE.                      GAS3UPD 
02589                                                                   GAS3UPD 
02590      PERFORM 9000-000-SEND-ERASE-RETURN.                          GAS3UPD 
02591                                                                   GAS3UPD 
02592  4400-900-EXIT. EXIT.                                             GAS3UPD 
02593 /*****************************************************************GAS3UPD 
02594 *  4500  -  PROTECT CRITICAL DATA ELEMENTS                       *GAS3UPD 
02595 *                                                                *GAS3UPD 
02596 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS3UPD 
02597 *          1. DETERMINE IF GROUP IS CRITICAL (CALL GCTRSRT).     *GAS3UPD 
02598 *          2. IF GROUP IS CRITICAL:                              *GAS3UPD 
02599 *              - READ PRODUCTION CONTRACT, GROUP SPECIFIC, OR    *GAS3UPD 
02600 *                BENEFIT PROVISION.                              *GAS3UPD 
02601 *                - IF ON DATA BASE:                              *GAS3UPD 
02602 *                  - SCAN FOR #ADL TABULAR                       *GAS3UPD 
02603 *                    - IF TABULAR PRESENT AND ACTIVE, TABULAR IS *GAS3UPD 
02604 *                      CRITICAL, PROTECT CRITICAL DATA ELEMENTS  *GAS3UPD 
02605 *                      ON SCREEN AND ISSUE MESSAGE.              *GAS3UPD 
02606 ******************************************************************GAS3UPD 
02607  4500-000-PROTECT-CRIT-DATA-ELE SECTION.                          GAS3UPD 
02608  4500-010.                                                        GAS3UPD 
02609                                                                   GAS3UPD 
02610      IF DELADDI  =  'CHG/DEL'     OR                              GAS3UPD 
02611         DELOLITI =  SPACES                                        GAS3UPD 
02612      THEN                                                         GAS3UPD 
02613         NEXT SENTENCE                                             GAS3UPD 
02614      ELSE                                                         GAS3UPD 
02615         GO TO 4500-900-EXIT.                                      GAS3UPD 
02616                                                                   GAS3UPD 
02617                                                                   GAS3UPD 
02618      MOVE WS-REQUEST-4500-CDE-PROTECT  TO  ACWA-CDE-REQUEST-CODE. GAS3UPD 
02619                                                                   GAS3UPD 
02620      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS3UPD 
02621                 COMMAREA (DFHCOMMAREA)                            GAS3UPD 
02622                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS3UPD 
02623                                                                   GAS3UPD 
02624      GO TO 4500-900-EXIT.                                         GAS3UPD 
02625                                                                   GAS3UPD 
02626                                                                   GAS3UPD 
02627  4500-900-EXIT.   EXIT.                                           GAS3UPD 
02628                                                                   GAS3UPD 
02629 /*****************************************************************GAS3UPD 
02630 *  4600  -  UPDATE CRITICAL DATA ELEMENT STATUS                  *GAS3UPD 
02631 *                                                                *GAS3UPD 
02632 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS3UPD 
02633 *           1. READ ALL LEVEL TABULAR FROM PROVISION POOL        *GAS3UPD 
02634 *           2. COMPARE CDE ELEMENTS ON W/F ALL LVL TAB TO THOSE  *GAS3UPD 
02635 *               ON THE PROVISION POOL ALL LVL TABULAR RECORD.    *GAS3UPD 
02636 *           3. IF CDE ELEMENTS ON W/F ALL LVL TAB HAVE BEEN      *GAS3UPD 
02637 *               CHANGED, ISSUE MESSAGE AND POSITION CURSOR ON    *GAS3UPD 
02638 *               +CDE+ INDICATOR (POSITION=8).                    *GAS3UPD 
02639 *              IF MESSAGE HAS BEEN ISSUED AND OPERATOR HAS HIT   *GAS3UPD 
02640 *               ENTER, CONTINUE PROCESSING.                      *GAS3UPD 
02641 ******************************************************************GAS3UPD 
02642  4600-000-UPDATE-CDE-STATUS     SECTION.                          GAS3UPD 
02643  4600-010.                                                        GAS3UPD 
02644                                                                   GAS3UPD 
02645                                                                   GAS3UPD 
02646      SET  ACWA-INDEX-1    TO  GAC-INDEX.                          GAS3UPD 
02647      MOVE WS-REQUEST-4600-CDE-STATUS  TO  ACWA-CDE-REQUEST-CODE.  GAS3UPD 
02648                                                                   GAS3UPD 
02649      EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                      GAS3UPD 
02650                 COMMAREA (DFHCOMMAREA)                            GAS3UPD 
02651                 LENGTH(LENGTH OF DFHCOMMAREA)           END-EXEC. GAS3UPD 
02652                                                                   GAS3UPD 
02653      IF  ACWA-CDE-RETURN-DONT-SEND                                GAS3UPD 
02654          EXEC CICS  RETURN   END-EXEC.                            GAS3UPD 
02655                                                                   GAS3UPD 
02656  4600-900-EXIT.   EXIT.                                           GAS3UPD 
02657                                                                   GAS3UPD 
02658 /*****************************************************************GAS3UPD 
02659 *  4700  -  UPDATE W/F CONTROL RECORD                            *GAS3UPD 
02660 *                                                                *GAS3UPD 
02661 *        FUNCTIONS: (VIA CDE MODULE GACDEPGM)                    *GAS3UPD 
02662 *          1. READ W/F CONTROL RECORD, UPDATE CDE RECORD COUNTS  *GAS3UPD 
02663 *             WITH THE ACTION TAKEN ON THE ALL LEVEL TABULAR     *GAS3UPD 
02664 *             RECORD AND THE INTERNAL TABULAR RECORDS; IF THE    *GAS3UPD 
02665 *             CDE STATUS HAS CHANGED.                            *GAS3UPD 
02666 *          2. REWRITE W/F CONTROL RECORD                         *GAS3UPD 
02667 ******************************************************************GAS3UPD 
02668  4700-000-UPDATE-CONTROL-RECORD SECTION.                          GAS3UPD 
02669  4700-010.                                                        GAS3UPD 
02670                                                                   GAS3UPD 
02671      SET  ACWA-INDEX-1     TO  GAC-INDEX.                         GAS3UPD 
02672      MOVE WS-REQUEST-4700-CNTL-UPDATE  TO  ACWA-CDE-REQUEST-CODE. GAS3UPD 
02673                                                                   GAS3UPD 
02674      EXEC CICS  LINK   PROGRAM ('GACDEPGM')                       GAS3UPD 
02675                 COMMAREA (DFHCOMMAREA)                            GAS3UPD 
02676                 LENGTH(LENGTH OF DFHCOMMAREA)      END-EXEC.      GAS3UPD 
02677                                                                   GAS3UPD 
02678  4700-900-EXIT.  EXIT.                                            GAS3UPD 
02679                                                                   GAS3UPD 
02680 ******************************************************************GAS3UPD 
02681 *  4900  -  R E S E T   O T H E R   I N T E R N A L   T A B S     GAS3UPD 
02682 *                                                                 GAS3UPD 
02683 *    FUNCTION  (VIA CDE MODULE GACDEPGM)                          GAS3UPD 
02684 *          READ W/F ACCUM'S INTERNAL TABULAR RECORDS, THOSE ON    GAS3UPD 
02685 *          W/F ONLY.  RESET THE CDE STATUS INDICATOR ON THIS      GAS3UPD 
02686 *          INTERNAL TO EITHER 1U OR 2 BASED ON THE INTERNAL       GAS3UPD 
02687 *          DESCRIPTOR, THEN REWRITE THIS RECORD.                  GAS3UPD 
02688 ******************************************************************GAS3UPD 
02689  4900-RESET-OTHER-INT-TABS      SECTION.                          GAS3UPD 
02690                                                                   GAS3UPD 
02691      MOVE ZERO  TO  WS-INTRNL-TABS-TO-CHG-CNT.                    GAS3UPD 
02692      PERFORM 4900-COUNT-INT-TAB                                   GAS3UPD 
02693         VARYING GAC-INT-INDEX  FROM  1  BY  1                     GAS3UPD 
02694         UNTIL GAC-INT-INDEX  NOT <                                GAS3UPD 
02695                          GAC-INTERNAL-TABULAR-COUNT(GAC-INDEX)  ORGAS3UPD 
02696               GAC-INT-ID(GAC-INDEX GAC-INT-INDEX)  =  HIGH-VALUES.GAS3UPD 
02697                                                                   GAS3UPD 
02698      IF WS-INTRNL-TABS-TO-CHG-CNT  >  ZERO                        GAS3UPD 
02699         MOVE WS-REQUEST-4900-CNTL-UPDATE  TO                      GAS3UPD 
02700                                            ACWA-CDE-REQUEST-CODE  GAS3UPD 
02701         EXEC CICS  LINK   PROGRAM  ('GACDEPGM')                   GAS3UPD 
02702                    COMMAREA (DFHCOMMAREA)                         GAS3UPD 
02703                    LENGTH(LENGTH OF DFHCOMMAREA)        END-EXEC. GAS3UPD 
02704                                                                   GAS3UPD 
02705      GO TO 4999-EXIT.                                             GAS3UPD 
02706                                                                   GAS3UPD 
02707  4900-COUNT-INT-TAB.                                              GAS3UPD 
02708      IF GAC-INT-SLOT(GAC-INDEX GAC-INT-INDEX)  >  +8999999        GAS3UPD 
02709         ADD 1  TO  WS-INTRNL-TABS-TO-CHG-CNT.                     GAS3UPD 
02710                                                                   GAS3UPD 
02711  4999-EXIT.       EXIT.                                           GAS3UPD 
02712 /*****************************************************************GAS3UPD 
02713 *     READ ALL LEVEL TABULAR FROM PROVISION POOL                  GAS3UPD 
02714 *                                                                 GAS3UPD 
02715 ******************************************************************GAS3UPD 
02716  5000-000-READ-PROD-ALL-LVL-TAB  SECTION.                         GAS3UPD 
02717                                                                   GAS3UPD 
02718      COMPUTE  WS-IO-PARM-WRK-ALL-LVL-LEN  =  GC-GCIOPARM-LEN  +   GAS3UPD 
02719               GC-WORKFILE-KEY-LEN  +  GC-GCTABULR-ADL-FIXED-LEN  +GAS3UPD 
02720              (GC-GCTABULR-ADL-VARY-LEN  *                         GAS3UPD 
02721                                    GC-GCTABULR-ADL-VARY-MAX-OCUR).GAS3UPD 
02722                                                                   GAS3UPD 
02723      IF ACWA-PR-ALL-LEVEL-TAB-COMP  >  ZERO                       GAS3UPD 
02724         NEXT SENTENCE                                             GAS3UPD 
02725      ELSE                                                         GAS3UPD 
02726         EXEC CICS GETMAIN                                         GAS3UPD 
02727                SET(ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD)      GAS3UPD 
02728                INITIMG(WS-HEX-00)                                 GAS3UPD 
02729                LENGTH(WS-IO-PARM-WRK-ALL-LVL-LEN)                 GAS3UPD 
02730                END-EXEC                                           GAS3UPD 
02731         SET ACWA-PR-ALL-LEVEL-TAB-PNTR     TO                     GAS3UPD 
02732                  ADDRESS OF PR-IO-PARM-ALL-LVL-TAB-RECORD.        GAS3UPD 
02733                                                                   GAS3UPD 
02734      MOVE GCIO-WORKFILE-KEY       TO WS-SV-RESTO-KY.              GAS3UPD 
02735      MOVE TABIDI                  TO  GCIO-TAB-TABULAR-ID.        GAS3UPD 
02736      MOVE WRK-TAB-PROV-COPY-SLOT  TO  GCIO-TAB-SLOT-NO.           GAS3UPD 
02737      MOVE GCIO-WORKFILE-KEY       TO  GCIOA-FILE-KEY.             GAS3UPD 
02738      MOVE GC-GCIO-AREA-2          TO  GCIOA-IO-AREA-TO-USE.       GAS3UPD 
02739      MOVE GC-GCIO-ACCESS-CODE-RD  TO  GCIOA-FILE-ACCESS-CODE.     GAS3UPD 
02740      MOVE GC-GCTABULR-DDNAME      TO  GCIOA-FILE-DDNAME.          GAS3UPD 
02741      MOVE GC-GCTABULR-ADL-VARY-MAX-OCUR                           GAS3UPD 
02742           TO  GAC2-ENTRY-COUNT.                                   GAS3UPD 
02743                                                                   GAS3UPD 
02744      MOVE WS-SV-RESTO-KY    TO  GCIO-WORKFILE-KEY.                GAS3UPD 
02745                                                                   GAS3UPD 
02746      IF GCIO-TAB-TABULAR-ID  =  GAC2-PROVISION-ID AND             GAS3UPD 
02747         GAC2-PROVISION-SLOT-NO  NUMERIC AND                       GAS3UPD 
02748         GCIO-TAB-SLOT-NO     =  GAC2-PROVISION-SLOT-NO            GAS3UPD 
02749         GO TO 5000-900-EXIT.                                      GAS3UPD 
02750                                                                   GAS3UPD 
02751      EXEC CICS  LINK   PROGRAM  ('GCIOPGM')                       GAS3UPD 
02752                 COMMAREA (PR-IO-PARM-ALL-LVL-TAB-RECORD)          GAS3UPD 
02753                 LENGTH (WS-IO-PARM-WRK-ALL-LVL-LEN)    END-EXEC.  GAS3UPD 
02754                                                                   GAS3UPD 
02755      IF NOT GCIOA-GOOD-RETURN                                     GAS3UPD 
02756         MOVE WS-ABCODE-1DF7        TO  WS-ABCODE                  GAS3UPD 
02757         MOVE WS-ABCODE-1DF7-MSG    TO  WS-ABCODE-MSG              GAS3UPD 
02758         PERFORM 9800-000-ERROR-MSG-THEN-ABEND.                    GAS3UPD 
02759                                                                   GAS3UPD 
02760  5000-900-EXIT.     EXIT.                                         GAS3UPD 
02761                                                                   GAS3UPD 
02762                                                                   GAS3UPD 
02763 /*****************************************************************GAS3UPD 
02764 * 6000  BUILD GROUP SPEC KEY                                     *GAS3UPD 
02765 *                                                                *GAS3UPD 
02766 *    BUILD THE GROUP SPECIFIC KEY FOR WORKFILE READS             *GAS3UPD 
02767 ******************************************************************GAS3UPD 
02768  6000-000-BUILD-GROUP-SPEC-KEY  SECTION.                          GAS3UPD 
02769  6000-010.                                                        GAS3UPD 
02770                                                                   GAS3UPD 
02771      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS3UPD 
02772      MOVE  'G'                 TO  GCIO-WRK-STATUS-CODE.          GAS3UPD 
02773      MOVE  'G3'                TO  GCIO-WRK-RECORD-TYPE.          GAS3UPD 
02774      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS3UPD 
02775      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS3UPD 
02776      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS3UPD 
02777      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS3UPD 
02778      MOVE SPACES               TO  GCIO-WRK-LINE-OF-BUS,          GAS3UPD 
02779                                    GCIO-WRK-PROVIDER-CONTROL.     GAS3UPD 
02780      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS3UPD 
02781      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS3UPD 
02782      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS3UPD 
02783      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS3UPD 
02784      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS3UPD 
02785      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS3UPD 
02786      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS3UPD 
02787                                                                   GAS3UPD 
02788  6000-900-EXIT. EXIT.                                             GAS3UPD 
02789                                                                   GAS3UPD 
02790 ******************************************************************GAS3UPD 
02791 * 6100  BUILD CONTRACT KEY                                       *GAS3UPD 
02792 *                                                                *GAS3UPD 
02793 *    BUILD THE CONTRACT KEY FOR WORKFILE READS                   *GAS3UPD 
02794 ******************************************************************GAS3UPD 
02795  6100-000-BUILD-CONTRACT-KEY    SECTION.                          GAS3UPD 
02796  6100-010.                                                        GAS3UPD 
02797                                                                   GAS3UPD 
02798      MOVE SPACES               TO  GCIO-WORKFILE-KEY.             GAS3UPD 
02799      MOVE  'C'                 TO  GCIO-WRK-STATUS-CODE.          GAS3UPD 
02800      MOVE  'C3'                TO  GCIO-WRK-RECORD-TYPE.          GAS3UPD 
02801      MOVE GCA-PLAN-CODE        TO  GCIO-WRK-PLAN-CODE.            GAS3UPD 
02802      MOVE GCA-GROUP-NUM        TO  GCIO-WRK-GROUP-NUM.            GAS3UPD 
02803      MOVE GCA-SECTION-NUM      TO  GCIO-WRK-SECTION-NUM.          GAS3UPD 
02804      MOVE GCA-PKG-CODE         TO  GCIO-WRK-PKG-CODE.             GAS3UPD 
02805      MOVE GCA-L-O-B            TO  GCIO-WRK-LINE-OF-BUS.          GAS3UPD 
02806      MOVE GCA-PROV-CTL         TO  GCIO-WRK-PROVIDER-CONTROL.     GAS3UPD 
02807      MOVE GCA-FAM-REL-LVL      TO  GCIO-WRK-FAMILY-RELATION-LVL.  GAS3UPD 
02808      MOVE GCA-EFFDT-CEN        TO  GCIO-WRK-EFFDT-CEN.            GAS3UPD 
02809      MOVE TABIDI               TO  GCIO-WRK-PROVISION-ID.         GAS3UPD 
02810      MOVE TABSLTNI             TO  ACWA-DISPLAY-LEN-7.            GAS3UPD 
02811      MOVE ACWA-DISPLAY-LEN-7   TO  GCIO-WRK-PROVISION-SLOT-NO.    GAS3UPD 
02812      MOVE SPACES               TO  GCIO-WRK-TAB-PROVISION-ID.     GAS3UPD 
02813      MOVE ZEROS                TO  GCIO-WRK-TAB-PROV-SLOT-NO.     GAS3UPD 
02814                                                                   GAS3UPD 
02815  6100-900-EXIT. EXIT.                                             GAS3UPD 
02816                                                                   GAS3UPD 
02817 /*****************************************************************GAS3UPD 
02818 * 6200  BUILD BEN PROV KEY                                       *GAS3UPD 
02819 *                                                                *GAS3UPD 
02820 *    BUILD THE BEN PROV KEY FOR WORKFILE READS                   *GAS3UPD 
02821 ******************************************************************GAS3UPD 
02822  6200-000-BUILD-BEN-PROV-KEY    SECTION.                          GAS3UPD 
02823  6200-010.                                                        GAS3UPD 
02824                                                                   GAS3UPD 
02825      MOVE SPACES                TO  GCIO-WORKFILE-KEY.            GAS3UPD 
02826      MOVE  'C'                  TO  GCIO-WRK-STATUS-CODE.         GAS3UPD 
02827      MOVE  'C5'                 TO  GCIO-WRK-RECORD-TYPE.         GAS3UPD 
02828      MOVE GCA-PLAN-CODE         TO  GCIO-WRK-PLAN-CODE.           GAS3UPD 
02829      MOVE GCA-GROUP-NUM         TO  GCIO-WRK-GROUP-NUM.           GAS3UPD 
02830      MOVE GCA-SECTION-NUM       TO  GCIO-WRK-SECTION-NUM.         GAS3UPD 
02831      MOVE GCA-PKG-CODE          TO  GCIO-WRK-PKG-CODE.            GAS3UPD 
02832      MOVE GCA-L-O-B             TO  GCIO-WRK-LINE-OF-BUS.         GAS3UPD 
02833      MOVE GCA-PROV-CTL          TO  GCIO-WRK-PROVIDER-CONTROL.    GAS3UPD 
02834      MOVE GCA-FAM-REL-LVL       TO  GCIO-WRK-FAMILY-RELATION-LVL. GAS3UPD 
02835      MOVE GCA-EFFDT-CEN         TO  GCIO-WRK-EFFDT-CEN.           GAS3UPD 
02836      MOVE GCA-BEN-PROV-ID       TO  GCIO-WRK-PROVISION-ID.        GAS3UPD 
02837      MOVE +9999999              TO  GCIO-WRK-PROVISION-SLOT-NO.   GAS3UPD 
02838      MOVE TABIDI                TO  GCIO-WRK-TAB-PROVISION-ID.    GAS3UPD 
02839      MOVE TABSLTNI              TO  ACWA-DISPLAY-LEN-7.           GAS3UPD 
02840      MOVE ACWA-DISPLAY-LEN-7    TO  GCIO-WRK-TAB-PROV-SLOT-NO.    GAS3UPD 
02841                                                                   GAS3UPD 
02842  6200-900-EXIT. EXIT.                                             GAS3UPD 
02843                                                                   GAS3UPD 
02844 /*****************************************************************GAS3UPD 
02845 *  XCTL TO MAIN MENU                                             *GAS3UPD 
02846 *                                                                *GAS3UPD 
02847 *    THE OPERATOR HAS ENTERED OUR FUNCTION CODE, BUT FOR US TO   *GAS3UPD 
02848 *  OPERATE WE MUST BE PASSED THE ALL LEVEL TABULAR RECORD.  SO WE*GAS3UPD 
02849 *  XCTL TO THE MAIN MENU THEREBY CAUSING THEM TO GO THRU THE MENUSGAS3UPD 
02850 *  TO GET TO US; WE ARE A MODULE AT THE BOTTOM OF A PYRAMID TO GETGAS3UPD 
02851 *  HERE YOU MUST START AT THE TOP (THE MAIN MENU).               *GAS3UPD 
02852 ******************************************************************GAS3UPD 
02853  6400-000-XCTL-TO-MAIN-MENU     SECTION.                          GAS3UPD 
02854  6400-010.                                                        GAS3UPD 
02855                                                                   GAS3UPD 
02856      MOVE WS-ABCODE-1DP1       TO  WS-ABCODE.                     GAS3UPD 
02857      MOVE WS-ABCODE-1DP1-MSG   TO  WS-ABCODE-MSG.                 GAS3UPD 
02858                                                                   GAS3UPD 
02859      EXEC CICS  XCTL   PROGRAM('GCPSPGM')   END-EXEC.             GAS3UPD 
02860                                                                   GAS3UPD 
02861  6400-900-EXIT. EXIT.                                             GAS3UPD 
02862                                                                   GAS3UPD 
02863 **************************************************************    GAS3UPD 
02864 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS3UPD 
02865 * - DEAD CODE ELIMINATION.                                        GAS3UPD 
02866 * - REMOVED: 7000-000-PRINT-HARDCOPY        SECTION.              GAS3UPD 
02867 **************************************************************    GAS3UPD 
02868                                                                   GAS3UPD 
02869 /*****************************************************************GAS3UPD 
02870 * 7900  RESET ATTRIBUTES                                         *GAS3UPD 
02871 ******************************************************************GAS3UPD 
02872  7900-000-RESET-ATTRIBUTES      SECTION.                          GAS3UPD 
02873  7900-010.                                                        GAS3UPD 
02874                                                                   GAS3UPD 
02875      MOVE DFHBMUNF  TO  BENVLQLA  COPAYINA  CSTCONTA  FAMINDIA    GAS3UPD 
02876               LOBA      PERIODA   PLCTRMTA  SRVGRUPA  PRTIMEFA    GAS3UPD 
02877               MANAPLIA  CARYOVRA  INTRVALA  INTTYPEA  CLMLVLIA    GAS3UPD 
02878               BNMXVALA  DAYFACIA  OVRDINDA  NEWVALUA  INTDESKA    GAS3UPD 
02879               FYIVALA   CONDALLA  CONDEXCA  CONDICDA  CONDTABA    GAS3UPD 
02880               CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA  BISNDINA    GAS3UPD 
02881               CONDMENA  CONDDRGA  CONDALCA  CONDOBNA  CONDOBCA    GAS3UPD 
02882               CONDMALA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA    GAS3UPD 
02883         CONDPECA  CONDNEMA  DEFINTNA  CONDSUIA CONDTMJA CONDINFA  GAS3UPD 
02884               IBGROPTA  IPGNOPTA  IPGTOPTA  MFRMSLTA  PERTQALA    GAS3UPD 
02885               AGEQLLA   AGEQLHA   CONDLIFA  IPGSOPTA  ASCDSCDA    GAS3UPD 
02886               IDGDOPTA  IPGPOPTA  RELPINDA  AGELIMLA AGELIMHA     GAS3UPD 
02887               FEAKINDA  ACCUMIDA  CAPINDA   SABDINDA              GAS3UPD 
02887               BENTYPA   TIERCDA   TIERLVA.                        GAS3UPD 
02888                                                                   GAS3UPD 
02889                                                                   GAS3UPD 
02890      IF DELADDO  =  'CHG/DEL'                                     GAS3UPD 
02891         NEXT SENTENCE                                             GAS3UPD 
02892      ELSE                                                         GAS3UPD 
02893         GO TO 7900-900-EXIT.                                      GAS3UPD 
02894                                                                   GAS3UPD 
02895                                                                   GAS3UPD 
02896      IF CDEINDO  =  '+CDE+'                                       GAS3UPD 
02897 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS3UPD 
02898         MOVE DFHBMABF TO  DLOPTLTA  PERIOTA   BENVLQTA  LOTA      GAS3UPD 
02899                AGELIMA    PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  GAS3UPD 
02900                AGEQLTA    COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  GAS3UPD 
                     BISNDITA                                                   
02901      ELSE                                                         GAS3UPD 
02902         NEXT SENTENCE.                                            GAS3UPD 
02903                                                                   GAS3UPD 
02904                                                                   GAS3UPD 
02905      IF CDEINDO  =  '+CDE-'                                       GAS3UPD 
02906 *---------------- HIGH-LIGHT CRITICAL DATA ELEMENT LABELS         GAS3UPD 
02907         MOVE DFHBMABF TO  DLOPTLTA  PERIOTA   BENVLQTA  LOTA      GAS3UPD 
02908                AGELIMA    PLCTRTTA  FAMINDTA  SRVGRUTA  CSTCOTTA  GAS3UPD 
02909                AGEQLTA    COPAYITA  INTDESTA  CONDTG1A  CONDTG2A  GAS3UPD 
02910                           DEFINTTA  BISNDITA                      GAS3UPD 
02911 *---------------- AUTOSKIP AND FSET CRITICAL DATA ELEMENTS        GAS3UPD 
02912         MOVE DFHBMASF TO  DELOPTNA  PERIODA   BENVLQLA  LOBA      GAS3UPD 
02913         AGELIMLA AGELIMHA PLCTRMTA  FAMINDIA  SRVGRUPA  CSTCONTA  GAS3UPD 
02914         AGEQLLA  AGEQLHA  COPAYINA  INTDESKA  CONDALLA  CONDEXCA  GAS3UPD 
02915                           CONDICDA  CONDTABA  CONDMENA  CONDDRGA  GAS3UPD 
02916                           CONDEMCA  CONDEACA  CONDSMIA  CONDNSMA  GAS3UPD 
02917                 CONDLIFA  CONDALCA  CONDOBCA  CONDOBNA  CONDMALA  GAS3UPD 
02918                 CONDTMJA  CONDCARA  CONDOBSA  CONDKDYA  CONDACCA  GAS3UPD 
02919                 CONDINFA  CONDPECA  CONDNEMA  CONDSUIA  DEFINTNA  GAS3UPD 
                      BISNDINA                                                  
02920          IF  ERRMSGO  >  SPACES                                   GAS3UPD 
02921          THEN                                                     GAS3UPD 
02922              NEXT SENTENCE                                        GAS3UPD 
02923          ELSE                                                     GAS3UPD 
02924              SET  WT-01-INDEX  TO  +08                            GAS3UPD 
02925              MOVE WT-01-MESSAGE-TEXT(WT-01-INDEX)  TO  ERRMSGO    GAS3UPD 
02926      ELSE                                                         GAS3UPD 
02927          NEXT SENTENCE.                                           GAS3UPD 
02928                                                                   GAS3UPD 
02929  7900-900-EXIT. EXIT.                                             GAS3UPD 
02930                                                                   GAS3UPD 
02931 /*****************************************************************GAS3UPD 
02932 * 9000  SEND ERASE THEN RETURN                                   *GAS3UPD 
02933 ******************************************************************GAS3UPD 
02934  9000-000-SEND-ERASE-RETURN     SECTION.                          GAS3UPD 
02935  9000-010.                                                        GAS3UPD 
02936                                                                   GAS3UPD 
02937      MOVE DFHBMASD  TO                                            GAS3UPD 
02938                    PERLITTA REININTA MAXOVRTA FDLRCLTA            GAS3UPD 
02939                    PERLIMTA REININDA MAXOVRDA FDLRCLIA            GAS3UPD 
02940                    TIMEDLRA TIMEDOLA.                             GAS3UPD 
02941                                                                   GAS3UPD 
02942      MOVE -1  TO  ERRMSGL.                                        GAS3UPD 
02943                                                                   GAS3UPD 
02944      EXEC CICS  SEND   MAP ('GA1XI01')    ERASE  CURSOR           GAS3UPD 
02945                 MAPSET('GA1XSET')    END-EXEC.                    GAS3UPD 
02946                                                                   GAS3UPD 
02947      EXEC CICS  RETURN   END-EXEC.                                GAS3UPD 
02948                                                                   GAS3UPD 
02949  9000-900-EXIT.    EXIT.                                          GAS3UPD 
02950                                                                   GAS3UPD 
02951 /*****************************************************************GAS3UPD 
02952 * 9010  SEND DATAONLY AND RETURN                                 *GAS3UPD 
02953 ******************************************************************GAS3UPD 
02954  9010-000-SEND-DATAONLY-RETURN  SECTION.                          GAS3UPD 
02955  9010-010.                                                        GAS3UPD 
02956                                                                   GAS3UPD 
02957      MOVE -1  TO  ERRMSGL.                                        GAS3UPD 
02958                                                                   GAS3UPD 
02959      EXEC CICS  SEND   MAP('GA1XI01')  DATAONLY  CURSOR           GAS3UPD 
02960                 MAPSET('GA1XSET')       END-EXEC.                 GAS3UPD 
02961                                                                   GAS3UPD 
02962      EXEC CICS  RETURN   END-EXEC.                                GAS3UPD 
02963                                                                   GAS3UPD 
02964  9010-900-EXIT.     EXIT.                                         GAS3UPD 
02965                                                                   GAS3UPD 
02966 **************************************************************    GAS3UPD 
02967 * CCSP  IK  - FLAGSHIP RENOVATION - OCT 2004                      GAS3UPD 
02968 * - DEAD CODE ELIMINATION.                                        GAS3UPD 
02969 * - REMOVED: 9200-000-GREGORIAN-TO-JULIAN   SECTION.              GAS3UPD 
02970 *            9300-000-JULIAN-TO-GREGORIAN   SECTION.              GAS3UPD 
02971 **************************************************************    GAS3UPD 
02972                                                                   GAS3UPD 
02973 /*****************************************************************GAS3UPD 
02974 * 9800  E R R O R   T H E N   A B E N D                          *GAS3UPD 
02975 *                                                                *GAS3UPD 
02976 *    THIS ROUTINE DISPLAYS THE PREVIOUSLY BUILT ERROR MESSAGE    *GAS3UPD 
02977 *  AND THEN ABENDS USING THE ABEND CODE EARLIER MEFINED.         *GAS3UPD 
02978 ******************************************************************GAS3UPD 
02979  9800-000-ERROR-MSG-THEN-ABEND  SECTION.                          GAS3UPD 
02980  9800-010.                                                        GAS3UPD 
02981                                                                   GAS3UPD 
02982      MOVE -1              TO  MFRMSLTL.                           GAS3UPD 
02983      MOVE WS-ABCODE-MSG   TO  ERRMSGO.                            GAS3UPD 
02984                                                                   GAS3UPD 
02985      EXEC CICS  SEND   MAP ('GA1XI01') ERASE  CURSOR   WAIT       GAS3UPD 
02986                 MAPSET('GA1XSET')      END-EXEC.                  GAS3UPD 
02987                                                                   GAS3UPD 
02988      EXEC CICS  ABEND   ABCODE(WS-ABCODE)   END-EXEC.             GAS3UPD 
02989                                                                   GAS3UPD 
02990  9800-900-EXIT. EXIT.                                             GAS3UPD 
